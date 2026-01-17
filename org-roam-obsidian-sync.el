;;; org-roam-obsidian-sync.el --- Sync Org-Roam with Obsidian -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.3") (org-roam "2.0.0"))
;; Keywords: org, org-roam, obsidian, notes, productivity
;; URL: https://github.com/polhuang/org-roam-obsidian-sync.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 'org-roam)
(require 'org-id)
(require 'ox-md)

;;; Custom Variables

(defgroup org-roam-obsidian-sync nil
  "Bidirectional sync between Org-roam and Obsidian."
  :group 'org-roam
  :prefix "org-roam-obsidian-")

;;;; Directory Configuration

(defcustom org-roam-obsidian-vault-path
  (expand-file-name "~/Obsidian/Personal Vault 2")
  "Path to Obsidian vault directory."
  :type 'directory
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-roam-path
  (if (boundp 'org-roam-directory)
      (expand-file-name org-roam-directory)
    (expand-file-name "~/org/roam"))
  "Path to org-roam directory.
Defaults to `org-roam-directory' if available."
  :type 'directory
  :group 'org-roam-obsidian-sync)

;;;; Naming Patterns

(defcustom org-roam-obsidian-naming-pattern "%title.md"
  "Naming pattern for Obsidian files.
Supported placeholders:
  %title - Note title
  %slug  - Slugified title (lowercase, underscores)
  %timestamp - YYYYMMDDhhmmss format

Example patterns:
  \"%title.md\" - \"My Note.md\"
  \"%slug.md\" - \"my_note.md\"
  \"%timestamp-%slug.md\" - \"20240115093045-my_note.md\""
  :type 'string
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-roam-naming-pattern
  (if (boundp 'org-roam-extract-new-file-path)
      org-roam-extract-new-file-path
    "%<%Y%m%d%H%M%S>-${slug}.org")
  "Naming pattern for org-roam files.
Defaults to `org-roam-extract-new-file-path' if available."
  :type 'string
  :group 'org-roam-obsidian-sync)

;;;; Sync Behavior

(defcustom org-roam-obsidian-sync-on-change nil
  "If non-nil, automatically sync files when they change.
Uses file system notifications to detect changes in both org-roam
and Obsidian directories, including changes made by external applications
like the Obsidian app."
  :type 'boolean
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-sync-periodic nil
  "If non-nil, automatically sync at regular intervals.
See `org-roam-obsidian-sync-interval' to configure the interval."
  :type 'boolean
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-sync-interval 3600
  "Interval in seconds for periodic sync."
  :type 'integer
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-conflict-resolution 'prompt
  "How to handle sync conflicts.
Options:
  \\='prompt - Ask user which version to keep
  \\='newer - Always use the newer file
  \\='org-roam - Prefer org-roam version
  \\='obsidian - Prefer Obsidian version"
  :type '(choice (const :tag "Prompt user" prompt)
                 (const :tag "Use newer" newer)
                 (const :tag "Prefer org-roam" org-roam)
                 (const :tag "Prefer Obsidian" obsidian))
  :group 'org-roam-obsidian-sync)

;;;; Conversion Options

(defcustom org-roam-obsidian-preserve-yaml-frontmatter nil
  "If non-nil, preserve YAML frontmatter in markdown files as org properties."
  :type 'boolean
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-sync-attachments t
  "If non-nil, sync attachment references between formats."
  :type 'boolean
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-excluded-patterns nil
  "List of regex patterns for files to exclude from sync."
  :type '(repeat regexp)
  :group 'org-roam-obsidian-sync)

;;;; Dailies Options

(defcustom org-roam-obsidian-dailies-folder "daily"
  "Subdirectory name for daily notes in both org-roam and Obsidian.
Daily notes use special sync behavior:
- Filename format: YYYY-MM-DD.org / YYYY-MM-DD.md
- Headings are appended and sorted by time
- Heading format: * H:MM am/pm (org) or ## H:MM am/pm (markdown)"
  :type 'string
  :group 'org-roam-obsidian-sync)

;;; Internal Variables

(defvar org-roam-obsidian--file-map (make-hash-table :test 'equal)
  "Hash table mapping org-roam IDs to Obsidian file info.
Each value is an alist with keys:
  \\='md-file - Path to markdown file
  \\='org-file - Path to org file
  \\='title - Note title
  \\='last-sync - Last sync timestamp")

(defvar org-roam-obsidian--reverse-map (make-hash-table :test 'equal)
  "Hash table mapping Obsidian file paths to org-roam IDs.")

(defvar org-roam-obsidian--map-file
  (expand-file-name "org-roam-obsidian-map.el" user-emacs-directory)
  "File to persist mapping data.")

(defvar org-roam-obsidian--file-watchers nil
  "List of active file notification descriptors.")

(defvar org-roam-obsidian--pending-syncs (make-hash-table :test 'equal)
  "Hash table tracking files with pending sync operations.
Keys are file paths, values are timer objects.")

(defvar org-roam-obsidian--sync-ignore-list (make-hash-table :test 'equal)
  "Hash table of files to ignore for sync events.
Keys are file paths, values are timer objects for cleanup.")

(defvar org-roam-obsidian--timer nil
  "Timer object for periodic sync.")

;;; Sync Ignore List Management

(defun org-roam-obsidian--add-to-ignore-list (file)
  "Add FILE to sync ignore list temporarily (2 seconds).
This prevents infinite loops when our sync triggers file-notify events."
  (when-let ((existing-timer (gethash file org-roam-obsidian--sync-ignore-list)))
    (when (timerp existing-timer)
      (cancel-timer existing-timer)))
  (let ((cleanup-timer
         (run-with-timer 2.0 nil
                        (lambda ()
                          (remhash file org-roam-obsidian--sync-ignore-list)))))
    (puthash file cleanup-timer org-roam-obsidian--sync-ignore-list)))

(defun org-roam-obsidian--should-ignore-file (file)
  "Check if FILE is in the sync ignore list."
  (gethash file org-roam-obsidian--sync-ignore-list))

;;; Mapping Persistence

(defun org-roam-obsidian--save-mappings ()
  "Save file mappings to disk."
  (with-temp-file org-roam-obsidian--map-file
    (let ((forward-map (make-hash-table :test 'equal))
          (reverse-map (make-hash-table :test 'equal)))
      ;; Copy hash tables
      (maphash (lambda (k v) (puthash k v forward-map))
               org-roam-obsidian--file-map)
      (maphash (lambda (k v) (puthash k v reverse-map))
               org-roam-obsidian--reverse-map)
      ;; Write to file
      (prin1 (list :forward forward-map :reverse reverse-map) (current-buffer)))))

(defun org-roam-obsidian--load-mappings ()
  "Load file mappings from disk."
  (when (file-exists-p org-roam-obsidian--map-file)
    (with-temp-buffer
      (insert-file-contents org-roam-obsidian--map-file)
      (let* ((data (read (current-buffer)))
             (forward-map (plist-get data :forward))
             (reverse-map (plist-get data :reverse)))
        (when forward-map
          (setq org-roam-obsidian--file-map forward-map))
        (when reverse-map
          (setq org-roam-obsidian--reverse-map reverse-map))))))

(defun org-roam-obsidian--add-mapping (org-file md-file org-id title)
  "Add or update a file mapping.
ORG-FILE is the org-roam file path.
MD-FILE is the Obsidian markdown file path.
ORG-ID is the org-roam ID.
TITLE is the note title."
  (let ((mapping `((org-file . ,org-file)
                   (md-file . ,md-file)
                   (title . ,title)
                   (last-sync . ,(current-time)))))
    (puthash org-id mapping org-roam-obsidian--file-map)
    (puthash md-file org-id org-roam-obsidian--reverse-map)))

(defun org-roam-obsidian--get-mapping-by-id (org-id)
  "Get mapping info for ORG-ID."
  (gethash org-id org-roam-obsidian--file-map))

(defun org-roam-obsidian--get-mapping-by-md-file (md-file)
  "Get org-roam ID for MD-FILE."
  (gethash md-file org-roam-obsidian--reverse-map))

(defun org-roam-obsidian--remove-mapping (org-id)
  "Remove a file mapping for ORG-ID."
  (when-let* ((mapping (gethash org-id org-roam-obsidian--file-map))
              (md-file (alist-get 'md-file mapping)))
    (remhash md-file org-roam-obsidian--reverse-map)
    (remhash org-id org-roam-obsidian--file-map)))

(defun org-roam-obsidian--get-all-mappings ()
  "Get all mappings as a list of alists."
  (let ((mappings '()))
    (maphash (lambda (org-id mapping)
               (push (cons (cons 'org-id org-id) mapping) mappings))
             org-roam-obsidian--file-map)
    mappings))

(defun org-roam-obsidian--get-all-mapped-org-files ()
  "Get list of all mapped org files."
  (let ((files '()))
    (maphash (lambda (_ mapping)
               (push (alist-get 'org-file mapping) files))
             org-roam-obsidian--file-map)
    files))

(defun org-roam-obsidian--get-all-mapped-md-files ()
  "Get list of all mapped markdown files."
  (hash-table-keys org-roam-obsidian--reverse-map))

;;; Utility Functions

(defun org-roam-obsidian--list-org-files ()
  "List all org files in org-roam directory."
  (when (file-directory-p org-roam-obsidian-roam-path)
    (directory-files-recursively
     org-roam-obsidian-roam-path
     "\\.org$"
     nil
     (lambda (dir)
       (not (member (file-name-nondirectory dir) '(".git" ".obsidian")))))))

(defun org-roam-obsidian--list-md-files ()
  "List all markdown files in Obsidian vault."
  (when (file-directory-p org-roam-obsidian-vault-path)
    (directory-files-recursively
     org-roam-obsidian-vault-path
     "\\.md$"
     nil
     (lambda (dir)
       (not (member (file-name-nondirectory dir) '(".git" ".obsidian")))))))

(defun org-roam-obsidian--should-exclude-file (file)
  "Check if FILE matches any exclusion pattern."
  (cl-some (lambda (pattern)
             (string-match-p pattern file))
           org-roam-obsidian-excluded-patterns))

(defun org-roam-obsidian--get-last-sync-time (org-id)
  "Get last sync time for ORG-ID from mapping."
  (if-let ((mapping (org-roam-obsidian--get-mapping-by-id org-id)))
      (or (alist-get 'last-sync mapping)
          (time-subtract (current-time) (days-to-time 3650))) ; 10 years ago
    (time-subtract (current-time) (days-to-time 3650))))

(defun org-roam-obsidian--update-sync-time (org-id)
  "Update last sync time for ORG-ID in mapping."
  (when-let ((mapping (gethash org-id org-roam-obsidian--file-map)))
    (setf (alist-get 'last-sync mapping) (current-time))
    (puthash org-id mapping org-roam-obsidian--file-map)))

;;; File Name Generation

(defun org-roam-obsidian--get-relative-path (file base-dir)
  "Get relative directory path of FILE from BASE-DIR.
Returns the subdirectory path without the filename."
  (let* ((file-dir (file-name-directory file))
         (expanded-base (file-name-as-directory (expand-file-name base-dir)))
         (expanded-file-dir (file-name-as-directory (expand-file-name file-dir))))
    (if (string-prefix-p expanded-base expanded-file-dir)
        (substring expanded-file-dir (length expanded-base))
      "")))

(defun org-roam-obsidian--generate-md-filename (title)
  "Generate Obsidian filename based on naming pattern for TITLE."
  (let* ((slug (org-roam-node-slug (org-roam-node-create :title title)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         ;; Sanitize title for use in filenames - replace illegal characters
         (safe-title (replace-regexp-in-string
                      "[/\\\\:<>\"*?|]" "-"
                      (string-trim title)))
         (pattern org-roam-obsidian-naming-pattern))
    (replace-regexp-in-string
     "%title" safe-title
     (replace-regexp-in-string
      "%slug" slug
      (replace-regexp-in-string
       "%timestamp" timestamp
       pattern)))))

(defun org-roam-obsidian--generate-org-filename (title)
  "Generate org-roam filename based on naming pattern for TITLE."
  (let ((slug (org-roam-node-slug (org-roam-node-create :title title)))
        (timestamp (format-time-string "%Y%m%d%H%M%S")))
    (format "%s-%s.org" timestamp slug)))

(defun org-roam-obsidian--generate-md-path (org-file)
  "Generate full markdown path for ORG-FILE, preserving subdirectory structure."
  (let* ((title (org-roam-obsidian--extract-title-from-org org-file))
         (relative-dir (org-roam-obsidian--get-relative-path
                        org-file org-roam-obsidian-roam-path))
         (md-filename (org-roam-obsidian--generate-md-filename title))
         (md-dir (expand-file-name relative-dir org-roam-obsidian-vault-path)))
    ;; Ensure directory exists
    (unless (file-directory-p md-dir)
      (make-directory md-dir t))
    (expand-file-name md-filename md-dir)))

(defun org-roam-obsidian--generate-org-path (md-file)
  "Generate full org path for MD-FILE, preserving subdirectory structure."
  (let* ((title (org-roam-obsidian--extract-title-from-md md-file))
         (relative-dir (org-roam-obsidian--get-relative-path
                        md-file org-roam-obsidian-vault-path))
         (org-filename (org-roam-obsidian--generate-org-filename title))
         (org-dir (expand-file-name relative-dir org-roam-obsidian-roam-path)))
    ;; Ensure directory exists
    (unless (file-directory-p org-dir)
      (make-directory org-dir t))
    (expand-file-name org-filename org-dir)))

;;; Title Extraction

(defun org-roam-obsidian--extract-title-from-org (org-file)
  "Extract title from ORG-FILE (#+title: or first heading)."
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    (or (when (re-search-forward "^#\\+title: \\(.*\\)$" nil t)
          (match-string-no-properties 1))
        (when (re-search-forward "^\\* \\(.*\\)$" nil t)
          (match-string-no-properties 1))
        (file-name-base org-file))))

(defun org-roam-obsidian--extract-title-from-md (md-file)
  "Extract title from MD-FILE (# heading or YAML frontmatter)."
  (with-temp-buffer
    (insert-file-contents md-file)
    (goto-char (point-min))
    (or ;; Try YAML frontmatter
        (when (looking-at "^---$")
          (forward-line)
          (when (re-search-forward "^title: \\(.*\\)$" nil t)
            (match-string-no-properties 1)))
        ;; Try first markdown header
        (when (re-search-forward "^# \\(.*\\)$" nil t)
          (match-string-no-properties 1))
        (file-name-base md-file))))

;;; Format Conversion - Markdown to Org

(defun org-roam-obsidian--md-to-org (md-content)
  "Convert markdown string MD-CONTENT to org-mode string."
  (with-temp-buffer
    (insert md-content)
    (goto-char (point-min))

    ;; Convert headers (must be done first, line by line)
    (org-roam-obsidian--convert-md-headers)

    ;; Convert code blocks
    (goto-char (point-min))
    (org-roam-obsidian--convert-md-code-blocks)

    ;; Convert images ![alt](path) -> [[file:path]]
    (goto-char (point-min))
    (while (re-search-forward "!\\[\\([^]]*\\)\\](\\([^)]+\\))" nil t)
      (replace-match "[[file:\\2][\\1]]" nil nil))

    ;; Convert bold/italic formatting
    (goto-char (point-min))
    (org-roam-obsidian--convert-md-formatting)

    ;; Convert regular markdown links [text](url) -> [[url][text]]
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
      (let ((text (match-string 1))
            (url (match-string 2)))
        (replace-match (format "[[%s][%s]]" url text) nil nil)))

    (buffer-string)))

(defun org-roam-obsidian--convert-md-headers ()
  "Convert markdown headers (# ##) to org headers (* **) in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "^\\(#+\\) " nil t)
    (let ((level (length (match-string 1))))
      (replace-match (concat (make-string level ?*) " ") nil nil))))

(defun org-roam-obsidian--convert-md-code-blocks ()
  "Convert markdown code blocks to org source blocks in current buffer."
  (goto-char (point-min))
  (while (re-search-forward "^```\\([a-z]*\\)\n" nil t)
    (let ((lang (match-string 1)))
      (replace-match (format "#+begin_src %s\n" (if (string-empty-p lang) "" lang)) nil nil)
      (when (re-search-forward "^```$" nil t)
        (replace-match "#+end_src" nil nil)))))

(defun org-roam-obsidian--convert-md-formatting ()
  "Convert markdown bold/italic to org format in current buffer."
  ;; Convert bold: **text** or __text__ -> *text*
  ;; Need to be careful with nested formatting
  (goto-char (point-min))
  (while (re-search-forward "\\*\\*\\([^*]+?\\)\\*\\*" nil t)
    (replace-match "*\\1*" nil nil))
  (goto-char (point-min))
  (while (re-search-forward "__\\([^_]+?\\)__" nil t)
    (replace-match "*\\1*" nil nil))

  ;; Convert italic: *text* or _text_ -> /text/
  ;; This is tricky because org uses * for bold and * for headers
  ;; We need to be careful not to match headers or our just-converted bold
  (goto-char (point-min))
  (while (re-search-forward "\\(?:^\\|[^*]\\)\\(\\*\\)\\([^*\n]+?\\)\\(\\*\\)\\(?:[^*]\\|$\\)" nil t)
    (replace-match "/\\2/" nil nil nil 2)
    (replace-match "" nil nil nil 1)
    (replace-match "" nil nil nil 3))
  (goto-char (point-min))
  (while (re-search-forward "\\b_\\([^_\n]+?\\)_\\b" nil t)
    (replace-match "/\\1/" nil nil)))

;;; Format Conversion - Org to Markdown

(defun org-roam-obsidian--org-to-md (org-content)
  "Convert org-mode string ORG-CONTENT to markdown string."
  (with-temp-buffer
    (insert org-content)
    (org-mode)
    ;; Export to markdown using ox-md
    ;; Set org-export-with-broken-links to allow ID links that we'll convert later
    (let ((org-export-with-toc nil)
          (org-export-with-author nil)
          (org-export-with-date nil)
          (org-export-with-title nil)
          (org-export-with-properties nil)
          (org-export-with-broken-links t))  ; Allow broken ID links
      (org-export-as 'md nil nil t))))

;;; Link Conversion

(defun org-roam-obsidian--find-node-by-title (title)
  "Find org-roam node by TITLE or alias.
Returns (id . file) or nil if not found."
  (when (and (fboundp 'org-roam-db-query) title)
    (let ((results (org-roam-db-query
                    [:select [id file]
                     :from nodes
                     :where (= title $s1)]
                    title)))
      (when results
        (let ((result (car results)))
          (cons (car result) (cadr result)))))))

(defun org-roam-obsidian--get-node-title-by-id (id)
  "Get node title from org-roam database by ID."
  (when (and (fboundp 'org-roam-db-query) id)
    (let ((results (org-roam-db-query
                    [:select title
                     :from nodes
                     :where (= id $s1)]
                    id)))
      (when results
        (caar results)))))

(defun org-roam-obsidian--convert-wikilinks-to-id (text)
  "Convert Obsidian [[wikilinks]] to org-roam [[id:uuid][title]] links in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Match [[wikilink]] or [[wikilink|display]]
    (while (re-search-forward "\\[\\[\\([^]|]+\\)\\(|\\([^]]+\\)\\)?\\]\\]" nil t)
      (let* ((title (match-string 1))
             (display (or (match-string 3) title))
             (node-info (org-roam-obsidian--find-node-by-title title)))
        (if node-info
            (let ((id (car node-info)))
              (replace-match (format "[[id:%s][%s]]" id display) nil nil))
          ;; No node found, leave as plain text or org-style link without id
          (replace-match (format "[[%s][%s]]" title display) nil nil))))
    (buffer-string)))

(defun org-roam-obsidian--convert-id-links-to-wikilinks (text)
  "Convert [[id:uuid][title]] links to [[title]] wikilinks in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))

    ;; First handle org-style ID links: [[id:uuid][display]] or [[id:uuid]]
    (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]\\(\\[\\([^]]+\\)\\]\\)?\\]" nil t)
      (let* ((id (match-string 1))
             (display (match-string 3))
             (title (or (org-roam-obsidian--get-node-title-by-id id)
                       display
                       "unknown")))
        (if (and display (not (string= title display)))
            (replace-match (format "[[%s|%s]]" title display) nil nil)
          (replace-match (format "[[%s]]" title) nil nil))))

    ;; Also handle markdown-style links that might have been produced by export
    ;; [display](id:uuid) -> [[title|display]] or [[title]]
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](id:\\([^)]+\\))" nil t)
      (let* ((display (match-string 1))
             (id (match-string 2))
             (title (or (org-roam-obsidian--get-node-title-by-id id)
                       display
                       "unknown")))
        (if (string= title display)
            (replace-match (format "[[%s]]" title) nil nil)
          (replace-match (format "[[%s|%s]]" title display) nil nil))))

    (buffer-string)))

;;; Dailies Support

(defun org-roam-obsidian--is-daily-file (file)
  "Check if FILE is in the dailies folder."
  (let* ((dailies-folder (file-name-as-directory org-roam-obsidian-dailies-folder))
         (roam-dailies (expand-file-name dailies-folder org-roam-obsidian-roam-path))
         (obsidian-dailies (expand-file-name dailies-folder org-roam-obsidian-vault-path)))
    (or (string-prefix-p roam-dailies (expand-file-name file))
        (string-prefix-p obsidian-dailies (expand-file-name file)))))

(defun org-roam-obsidian--parse-time-heading (heading)
  "Parse time from HEADING in format 'H:MM am/pm' or 'HH:MM am/pm'.
Returns minutes since midnight for sorting, or nil if no valid time found."
  (when (string-match "\\([0-9]+\\):\\([0-9]+\\) *\\(am\\|pm\\)" heading)
    (let* ((hour (string-to-number (match-string 1 heading)))
           (minute (string-to-number (match-string 2 heading)))
           (ampm (match-string 3 heading))
           ;; Convert to 24-hour format
           (hour24 (cond
                    ((and (string= ampm "am") (= hour 12)) 0)
                    ((and (string= ampm "pm") (/= hour 12)) (+ hour 12))
                    (t hour))))
      (+ (* hour24 60) minute))))

(defun org-roam-obsidian--extract-org-headings (content)
  "Extract top-level headings from org CONTENT.
Returns list of (time-minutes . full-heading-text) pairs."
  (let ((headings '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\{1\\} \\(.*\\)$" nil t)
        (let* ((heading-start (match-beginning 0))
               (heading-title (match-string 1))
               (time-value (org-roam-obsidian--parse-time-heading heading-title))
               (next-heading (save-excursion
                              (if (re-search-forward "^\\*\\{1\\} " nil t)
                                  (match-beginning 0)
                                (point-max))))
               (heading-content (buffer-substring heading-start next-heading)))
          (push (cons time-value heading-content) headings))))
    (nreverse headings)))

(defun org-roam-obsidian--extract-md-headings (content)
  "Extract H2 headings from markdown CONTENT.
Returns list of (time-minutes . full-heading-text) pairs."
  (let ((headings '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "^## \\(.*\\)$" nil t)
        (let* ((heading-start (match-beginning 0))
               (heading-title (match-string 1))
               (time-value (org-roam-obsidian--parse-time-heading heading-title))
               (next-heading (save-excursion
                              (if (re-search-forward "^## " nil t)
                                  (match-beginning 0)
                                (point-max))))
               (heading-content (buffer-substring heading-start next-heading)))
          (push (cons time-value heading-content) headings))))
    (nreverse headings)))

(defun org-roam-obsidian--sort-headings-by-time (headings)
  "Sort HEADINGS list by time value.
HEADINGS is a list of (time-minutes . heading-text) pairs."
  (sort headings
        (lambda (a b)
          (let ((time-a (car a))
                (time-b (car b)))
            (cond
             ;; Both have times - sort by time
             ((and time-a time-b) (< time-a time-b))
             ;; Only a has time - a comes first
             (time-a t)
             ;; Only b has time - b comes first
             (time-b nil)
             ;; Neither has time - preserve order
             (t t))))))

(defun org-roam-obsidian--deduplicate-headings (existing new)
  "Remove headings from NEW that have same time as EXISTING.
Keeps the existing heading, skips duplicates from new.
EXISTING and NEW are lists of (time-minutes . heading-text) pairs."
  (let ((existing-times (mapcar #'car existing)))
    (cl-remove-if (lambda (heading)
                    (and (car heading) ; has time
                         (member (car heading) existing-times)))
                  new)))

(defun org-roam-obsidian--merge-daily-org-content (existing-content new-content)
  "Merge NEW-CONTENT into EXISTING-CONTENT for org daily notes.
Extracts headings, merges them, deduplicates by time, and sorts by time."
  (let* ((existing-headings (org-roam-obsidian--extract-org-headings existing-content))
         (new-headings (org-roam-obsidian--extract-org-headings new-content))
         ;; Get preamble (everything before first heading)
         (preamble (with-temp-buffer
                    (insert existing-content)
                    (goto-char (point-min))
                    (if (re-search-forward "^\\* " nil t)
                        (buffer-substring (point-min) (match-beginning 0))
                      existing-content)))
         ;; Deduplicate and merge headings
         (unique-new-headings (org-roam-obsidian--deduplicate-headings
                              existing-headings new-headings))
         ;; Sort all headings by time
         (all-headings (org-roam-obsidian--sort-headings-by-time
                       (append existing-headings unique-new-headings))))
    ;; Reconstruct content
    (concat preamble
            (mapconcat (lambda (heading) (cdr heading)) all-headings "\n"))))

(defun org-roam-obsidian--merge-daily-md-content (existing-content new-content)
  "Merge NEW-CONTENT into EXISTING-CONTENT for markdown daily notes.
Extracts headings, merges them, deduplicates by time, and sorts by time."
  (let* ((existing-headings (org-roam-obsidian--extract-md-headings existing-content))
         (new-headings (org-roam-obsidian--extract-md-headings new-content))
         ;; Get preamble (everything before first heading)
         (preamble (with-temp-buffer
                    (insert existing-content)
                    (goto-char (point-min))
                    (if (re-search-forward "^## " nil t)
                        (buffer-substring (point-min) (match-beginning 0))
                      existing-content)))
         ;; Deduplicate and merge headings
         (unique-new-headings (org-roam-obsidian--deduplicate-headings
                              existing-headings new-headings))
         ;; Sort all headings by time
         (all-headings (org-roam-obsidian--sort-headings-by-time
                       (append existing-headings unique-new-headings))))
    ;; Reconstruct content
    (concat preamble
            (mapconcat (lambda (heading) (cdr heading)) all-headings "\n"))))

(defun org-roam-obsidian--sync-daily-org-to-md (org-file md-file)
  "Sync daily note from ORG-FILE to MD-FILE with merge and sort."
  (let* ((org-content (with-temp-buffer
                        (insert-file-contents org-file)
                        (buffer-string)))
         (org-id (org-roam-obsidian--get-or-create-id org-file))
         (new-md-content (org-roam-obsidian--org-to-md org-content))
         (new-md-content-wikilinks (org-roam-obsidian--convert-id-links-to-wikilinks new-md-content)))

    ;; Add md-file to ignore list BEFORE writing to prevent loop
    (org-roam-obsidian--add-to-ignore-list md-file)

    (if (file-exists-p md-file)
        ;; File exists - merge content
        (let* ((existing-md-content (with-temp-buffer
                                      (insert-file-contents md-file)
                                      (buffer-string)))
               (merged-content (org-roam-obsidian--merge-daily-md-content
                               existing-md-content new-md-content-wikilinks)))
          (with-temp-file md-file
            (insert (string-trim-left merged-content))))
      ;; File doesn't exist - create new
      (with-temp-file md-file
        (insert (string-trim-left new-md-content-wikilinks))))

    (org-roam-obsidian--update-sync-time org-id)
    'synced))

(defun org-roam-obsidian--sync-daily-md-to-org (md-file org-file)
  "Sync daily note from MD-FILE to ORG-FILE with merge and sort."
  (let* ((md-content (with-temp-buffer
                       (insert-file-contents md-file)
                       (buffer-string)))
         (new-org-content-base (org-roam-obsidian--md-to-org md-content))
         (new-org-content (org-roam-obsidian--convert-wikilinks-to-id new-org-content-base))
         (org-id (if (file-exists-p org-file)
                     (org-roam-obsidian--get-or-create-id org-file)
                   (org-id-new)))
         (title (file-name-base md-file)))

    ;; Add org-file to ignore list BEFORE writing to prevent loop
    (org-roam-obsidian--add-to-ignore-list org-file)

    (if (file-exists-p org-file)
        ;; File exists - merge content
        (let* ((existing-org-content (with-temp-buffer
                                       (insert-file-contents org-file)
                                       (buffer-string)))
               (merged-content (org-roam-obsidian--merge-daily-org-content
                               existing-org-content new-org-content)))
          (with-temp-file org-file
            (insert merged-content)))
      ;; File doesn't exist - create new
      (with-temp-file org-file
        (insert ":PROPERTIES:\n")
        (insert (format ":ID:       %s\n" org-id))
        (insert ":END:\n")
        (insert (format "#+title: %s\n\n" title))
        (insert new-org-content)))

    (org-roam-obsidian--update-sync-time org-id)
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file org-file))
    'synced))

;;; ID Management

(defun org-roam-obsidian--get-or-create-id (org-file)
  "Get existing org ID from ORG-FILE, or create new one if missing."
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    (or (when (re-search-forward "^:ID: +\\(.+\\)$" nil t)
          (match-string-no-properties 1))
        (org-id-new))))

(defun org-roam-obsidian--ensure-id-in-org-file (org-file)
  "Ensure ORG-FILE has an ID property, add if missing.
Returns the ID."
  (let ((id nil))
    (with-current-buffer (find-file-noselect org-file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^:ID: +\\(.+\\)$" nil t)
            (setq id (match-string-no-properties 1))
          ;; Need to add ID
          (goto-char (point-min))
          (setq id (org-id-new))
          (if (re-search-forward "^:PROPERTIES:" nil t)
              ;; PROPERTIES drawer exists, add ID to it
              (progn
                (forward-line)
                (beginning-of-line)
                (insert (format ":ID:       %s\n" id)))
            ;; Create PROPERTIES drawer
            ;; Insert before #+title if it exists, otherwise at beginning
            (goto-char (point-min))
            (if (re-search-forward "^#\\+title:" nil t)
                (progn
                  (beginning-of-line)
                  (insert ":PROPERTIES:\n")
                  (insert (format ":ID:       %s\n" id))
                  (insert ":END:\n"))
              ;; No #+title, insert at beginning
              (goto-char (point-min))
              (insert ":PROPERTIES:\n")
              (insert (format ":ID:       %s\n" id))
              (insert ":END:\n")))
          (save-buffer)))
      (kill-buffer))
    id))

;;; Sync Engine - File Pair Synchronization

(defun org-roam-obsidian--sync-org-to-md (org-file md-file)
  "Sync ORG-FILE to MD-FILE.
Returns \\='synced on success."
  (let* ((org-content (with-temp-buffer
                        (insert-file-contents org-file)
                        (buffer-string)))
         (org-id (org-roam-obsidian--get-or-create-id org-file))
         (md-content (org-roam-obsidian--org-to-md org-content))
         ;; Convert ID links to wikilinks
         (md-content-final (org-roam-obsidian--convert-id-links-to-wikilinks md-content)))
    ;; Add md-file to ignore list BEFORE writing to prevent loop
    (org-roam-obsidian--add-to-ignore-list md-file)
    (with-temp-file md-file
      (insert (string-trim-left md-content-final)))
    (org-roam-obsidian--update-sync-time org-id)
    'synced))

(defun org-roam-obsidian--sync-md-to-org (md-file org-file)
  "Sync MD-FILE to ORG-FILE.
Returns \\='synced on success."
  (let* ((md-content (with-temp-buffer
                       (insert-file-contents md-file)
                       (buffer-string)))
         (org-content-base (org-roam-obsidian--md-to-org md-content))
         ;; Convert wikilinks to ID links
         (org-content (org-roam-obsidian--convert-wikilinks-to-id org-content-base))
         (org-id (if (file-exists-p org-file)
                     (org-roam-obsidian--get-or-create-id org-file)
                   (org-id-new)))
         (title (org-roam-obsidian--extract-title-from-md md-file)))
    ;; Add org-file to ignore list BEFORE writing to prevent loop
    (org-roam-obsidian--add-to-ignore-list org-file)
    ;; Ensure ID property in org content
    (with-temp-file org-file
      (insert ":PROPERTIES:\n")
      (insert (format ":ID:       %s\n" org-id))
      (insert ":END:\n")
      (insert (format "#+title: %s\n\n" title))
      (insert org-content))
    (org-roam-obsidian--update-sync-time org-id)
    ;; Update org-roam database
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file org-file))
    'synced))

(defun org-roam-obsidian--sync-file-pair (org-file md-file org-id)
  "Sync a single file pair ORG-FILE and MD-FILE identified by ORG-ID.
Returns \\='synced, \\='conflict, or \\='unchanged."
  (let* ((org-mtime (file-attribute-modification-time
                     (file-attributes org-file)))
         (md-mtime (file-attribute-modification-time
                    (file-attributes md-file)))
         (last-sync (org-roam-obsidian--get-last-sync-time org-id))
         (org-modified (time-less-p last-sync org-mtime))
         (md-modified (time-less-p last-sync md-mtime))
         (is-daily (org-roam-obsidian--is-daily-file org-file)))

    (cond
     ;; Daily files: always merge and sort (no conflict, just merge both)
     ((and is-daily (or org-modified md-modified))
      (message "Merging daily note: %s <-> %s"
               (file-name-nondirectory org-file)
               (file-name-nondirectory md-file))
      (when org-modified
        (org-roam-obsidian--sync-daily-org-to-md org-file md-file))
      (when md-modified
        (org-roam-obsidian--sync-daily-md-to-org md-file org-file))
      'synced)

     ;; Both modified - conflict!
     ((and org-modified md-modified)
      (org-roam-obsidian--handle-conflict org-file md-file org-mtime md-mtime))

     ;; Only org modified - sync org -> md
     (org-modified
      (message "Syncing %s -> %s" (file-name-nondirectory org-file)
               (file-name-nondirectory md-file))
      (if is-daily
          (org-roam-obsidian--sync-daily-org-to-md org-file md-file)
        (org-roam-obsidian--sync-org-to-md org-file md-file)))

     ;; Only md modified - sync md -> org
     (md-modified
      (message "Syncing %s -> %s" (file-name-nondirectory md-file)
               (file-name-nondirectory org-file))
      (if is-daily
          (org-roam-obsidian--sync-daily-md-to-org md-file org-file)
        (org-roam-obsidian--sync-md-to-org md-file org-file)))

     ;; Neither modified
     (t 'unchanged))))

;;; Conflict Handling

(defun org-roam-obsidian--handle-conflict (org-file md-file org-mtime md-mtime)
  "Handle conflict when both ORG-FILE and MD-FILE modified.
ORG-MTIME and MD-MTIME are the modification times.
Resolution based on `org-roam-obsidian-conflict-resolution'."
  (pcase org-roam-obsidian-conflict-resolution
    ('prompt
     (org-roam-obsidian--prompt-conflict-resolution
      org-file md-file org-mtime md-mtime))

    ('newer
     (if (time-less-p org-mtime md-mtime)
         (org-roam-obsidian--sync-md-to-org md-file org-file)
       (org-roam-obsidian--sync-org-to-md org-file md-file)))

    ('org-roam
     (org-roam-obsidian--sync-org-to-md org-file md-file))

    ('obsidian
     (org-roam-obsidian--sync-md-to-org md-file org-file))))

(defun org-roam-obsidian--prompt-conflict-resolution (org-file md-file org-mtime md-mtime)
  "Prompt user to resolve conflict between ORG-FILE and MD-FILE.
ORG-MTIME and MD-MTIME are the modification times."
  (let ((choice (read-multiple-choice
                 (format "Conflict: %s\nOrg: %s\nMD:  %s\nWhich to keep?"
                         (file-name-nondirectory org-file)
                         (format-time-string "%Y-%m-%d %H:%M:%S" org-mtime)
                         (format-time-string "%Y-%m-%d %H:%M:%S" md-mtime))
                 '((?o "org-roam" "Keep org-roam version")
                   (?m "markdown" "Keep Obsidian version")
                   (?s "skip" "Skip this file")))))
    (pcase (car choice)
      (?o (org-roam-obsidian--sync-org-to-md org-file md-file))
      (?m (org-roam-obsidian--sync-md-to-org md-file org-file))
      (?s 'skipped))))

;;; New Files Handling

(defun org-roam-obsidian--find-md-by-title (title md-files)
  "Find markdown file in MD-FILES matching TITLE (case-insensitive)."
  (cl-find-if
   (lambda (md-file)
     (string= (downcase title)
              (downcase (org-roam-obsidian--extract-title-from-md md-file))))
   md-files))

(defun org-roam-obsidian--handle-new-org-file (org-file md-files)
  "Handle new ORG-FILE - create corresponding markdown file.
MD-FILES is list of existing markdown files for matching."
  (let* ((title (org-roam-obsidian--extract-title-from-org org-file))
         (org-id (org-roam-obsidian--ensure-id-in-org-file org-file))
         (md-file (org-roam-obsidian--generate-md-path org-file)))

    ;; Check if markdown file with this title already exists
    (if-let ((existing-md (org-roam-obsidian--find-md-by-title title md-files)))
        ;; Match found - create mapping instead of new file
        (progn
          (message "Matched existing: %s <-> %s"
                   (file-name-nondirectory org-file)
                   (file-name-nondirectory existing-md))
          (org-roam-obsidian--add-mapping org-file existing-md org-id title))
      ;; No match - create new markdown file (preserving directory structure)
      (message "Creating new markdown: %s"
               (file-relative-name md-file org-roam-obsidian-vault-path))
      (org-roam-obsidian--sync-org-to-md org-file md-file)
      (org-roam-obsidian--add-mapping org-file md-file org-id title))))

(defun org-roam-obsidian--handle-new-md-file (md-file org-files)
  "Handle new MD-FILE - create corresponding org file.
ORG-FILES is list of existing org files for matching."
  (let* ((title (org-roam-obsidian--extract-title-from-md md-file))
         (org-file (org-roam-obsidian--generate-org-path md-file)))

    ;; Check if org file with this title already exists
    (if-let ((existing-org (cl-find-if
                           (lambda (f)
                             (string= (downcase title)
                                     (downcase (org-roam-obsidian--extract-title-from-org f))))
                           org-files)))
        ;; Match found - create mapping instead of new file
        (let ((org-id (org-roam-obsidian--ensure-id-in-org-file existing-org)))
          (message "Matched existing: %s <-> %s"
                   (file-name-nondirectory md-file)
                   (file-name-nondirectory existing-org))
          (org-roam-obsidian--add-mapping existing-org md-file org-id title))
      ;; No match - create new org file (preserving directory structure)
      (message "Creating new org: %s"
               (file-relative-name org-file org-roam-obsidian-roam-path))
      (org-roam-obsidian--sync-md-to-org md-file org-file)
      (let ((org-id (org-roam-obsidian--get-or-create-id org-file)))
        (org-roam-obsidian--add-mapping org-file md-file org-id title)))))

(defun org-roam-obsidian--sync-new-files (org-files md-files)
  "Process files in ORG-FILES and MD-FILES that don't have mappings yet."
  (let ((mapped-org (org-roam-obsidian--get-all-mapped-org-files))
        (mapped-md (org-roam-obsidian--get-all-mapped-md-files)))

    ;; Process unmapped org files
    (dolist (org-file org-files)
      (unless (or (member org-file mapped-org)
                  (org-roam-obsidian--should-exclude-file org-file))
        (org-roam-obsidian--handle-new-org-file org-file md-files)))

    ;; Process unmapped md files
    (dolist (md-file md-files)
      (unless (or (member md-file mapped-md)
                  (org-roam-obsidian--should-exclude-file md-file))
        (org-roam-obsidian--handle-new-md-file md-file org-files)))))

;;; Main Sync Function

;;;###autoload
(defun org-roam-obsidian-sync ()
  "Main sync function - bidirectional sync between org-roam and Obsidian."
  (interactive)
  (let ((org-files (org-roam-obsidian--list-org-files))
        (md-files (org-roam-obsidian--list-md-files))
        (sync-count 0)
        (conflict-count 0)
        (new-count 0)
        (unchanged-count 0))

    (message "Starting org-roam <-> Obsidian sync...")

    ;; Load existing mappings
    (org-roam-obsidian--load-mappings)

    ;; Process mapped files
    (dolist (mapping (org-roam-obsidian--get-all-mappings))
      (let* ((org-id (alist-get 'org-id mapping))
             (org-file (alist-get 'org-file mapping))
             (md-file (alist-get 'md-file mapping))
             (org-exists (file-exists-p org-file))
             (md-exists (file-exists-p md-file)))

        (cond
         ;; Both exist - check for updates
         ((and org-exists md-exists)
          (let ((result (org-roam-obsidian--sync-file-pair org-file md-file org-id)))
            (pcase result
              ('synced (cl-incf sync-count))
              ('conflict (cl-incf conflict-count))
              ('unchanged (cl-incf unchanged-count)))))

         ;; Only org exists - md was deleted
         (org-exists
          (when (yes-or-no-p
                 (format "Markdown deleted: %s\nDelete org file too? "
                         (file-name-nondirectory md-file)))
            (delete-file org-file)
            (org-roam-obsidian--remove-mapping org-id)))

         ;; Only md exists - org was deleted
         (md-exists
          (when (yes-or-no-p
                 (format "Org deleted: %s\nDelete markdown file too? "
                         (file-name-nondirectory org-file)))
            (delete-file md-file)
            (org-roam-obsidian--remove-mapping org-id)))

         ;; Both deleted - remove mapping
         (t
          (org-roam-obsidian--remove-mapping org-id)))))

    ;; Process unmapped files (new files)
    (org-roam-obsidian--sync-new-files org-files md-files)

    ;; Save updated mappings
    (org-roam-obsidian--save-mappings)

    (message "Sync complete: %d synced, %d conflicts, %d unchanged"
             sync-count conflict-count unchanged-count)))

;;; Interactive Commands

;;;###autoload
(defun org-roam-obsidian-sync-current-file ()
  "Sync only the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))

    (org-roam-obsidian--load-mappings)

    (cond
     ;; In org-roam directory
     ((string-prefix-p (expand-file-name org-roam-obsidian-roam-path) file)
      (let* ((org-id (org-roam-obsidian--ensure-id-in-org-file file))
             (mapping (org-roam-obsidian--get-mapping-by-id org-id)))
        (if mapping
            (let ((md-file (alist-get 'md-file mapping)))
              (org-roam-obsidian--sync-org-to-md file md-file)
              (org-roam-obsidian--save-mappings)
              (message "Synced to %s" (file-name-nondirectory md-file)))
          (message "No mapping found. Run full sync to create mapping."))))

     ;; In Obsidian directory
     ((string-prefix-p (expand-file-name org-roam-obsidian-vault-path) file)
      (let ((org-id (org-roam-obsidian--get-mapping-by-md-file file)))
        (if org-id
            (let* ((mapping (org-roam-obsidian--get-mapping-by-id org-id))
                   (org-file (alist-get 'org-file mapping)))
              (org-roam-obsidian--sync-md-to-org file org-file)
              (org-roam-obsidian--save-mappings)
              (message "Synced to %s" (file-name-nondirectory org-file)))
          (message "No mapping found. Run full sync to create mapping."))))

     (t (user-error "Current file not in org-roam or Obsidian directory")))))

;;;###autoload
(defun org-roam-obsidian-view-mappings ()
  "Display current file mappings in a buffer."
  (interactive)
  (org-roam-obsidian--load-mappings)
  (let ((buf (get-buffer-create "*Org-Roam Obsidian Mappings*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "* Org-Roam <-> Obsidian Mappings\n\n")
      (let ((mappings (org-roam-obsidian--get-all-mappings)))
        (if (null mappings)
            (insert "No mappings found. Run sync to create mappings.\n")
          (dolist (mapping mappings)
            (insert (format "** %s\n" (alist-get 'title mapping)))
            (insert (format "   - Org: %s\n" (alist-get 'org-file mapping)))
            (insert (format "   - MD:  %s\n" (alist-get 'md-file mapping)))
            (insert (format "   - ID:  %s\n\n" (alist-get 'org-id mapping)))))))
    (switch-to-buffer buf)))

;;;###autoload
(defun org-roam-obsidian-reset-mappings ()
  "Clear all mappings and rescan directories."
  (interactive)
  (when (yes-or-no-p "Really reset all mappings? ")
    (clrhash org-roam-obsidian--file-map)
    (clrhash org-roam-obsidian--reverse-map)
    (org-roam-obsidian--save-mappings)
    (message "Mappings reset. Run sync to rebuild.")))

;;; File Notification Sync Mode

(defun org-roam-obsidian--watch-directory-recursively (root-path flags)
  "Add file watchers to ROOT-PATH and all its subdirectories.
FLAGS are the file-notify flags to use. Returns number of watchers added."
  (let ((watch-count 0))
    ;; Watch the root directory
    (condition-case err
        (let ((descriptor (file-notify-add-watch
                          (expand-file-name root-path)
                          flags
                          #'org-roam-obsidian--file-change-callback)))
          (push descriptor org-roam-obsidian--file-watchers)
          (setq watch-count (1+ watch-count)))
      (error
       (message "Failed to watch %s: %s" root-path (error-message-string err))))

    ;; Recursively watch all subdirectories
    (when (file-directory-p root-path)
      (dolist (subdir (directory-files-recursively
                       root-path
                       ".*"
                       t  ; include-directories
                       (lambda (dir)
                         ;; Skip hidden directories and .obsidian
                         (not (or (string-match-p "/\\." dir)
                                  (string-match-p "\\.obsidian" dir))))))
        (when (file-directory-p subdir)
          (condition-case err
              (let ((descriptor (file-notify-add-watch
                                (expand-file-name subdir)
                                flags
                                #'org-roam-obsidian--file-change-callback)))
                (push descriptor org-roam-obsidian--file-watchers)
                (setq watch-count (1+ watch-count)))
            (error
             (message "Failed to watch subdirectory %s: %s"
                      subdir (error-message-string err)))))))
    watch-count))

(defun org-roam-obsidian--start-file-watching ()
  "Start watching both sync directories for file changes."
  (org-roam-obsidian--stop-file-watching)

  ;; Use 'change flag for all systems - Emacs abstracts the backend details
  (let ((flags '(change))
        (total-watchers 0))

    ;; Watch org-roam directory recursively
    (when (file-directory-p org-roam-obsidian-roam-path)
      (let ((count (org-roam-obsidian--watch-directory-recursively
                    org-roam-obsidian-roam-path flags)))
        (setq total-watchers (+ total-watchers count))
        (message "Watching org-roam directory (recursively): %s (%d watchers)"
                 org-roam-obsidian-roam-path count)))

    ;; Watch Obsidian vault directory recursively
    (when (file-directory-p org-roam-obsidian-vault-path)
      (let ((count (org-roam-obsidian--watch-directory-recursively
                    org-roam-obsidian-vault-path flags)))
        (setq total-watchers (+ total-watchers count))
        (message "Watching Obsidian directory (recursively): %s (%d watchers)"
                 org-roam-obsidian-vault-path count)))

    (message "Total file watchers active: %d" total-watchers)))

(defun org-roam-obsidian--stop-file-watching ()
  "Stop all file watchers and cancel pending syncs."
  (dolist (descriptor org-roam-obsidian--file-watchers)
    (file-notify-rm-watch descriptor))
  (setq org-roam-obsidian--file-watchers nil)

  ;; Cancel all pending sync timers
  (maphash (lambda (_file timer)
             (when (timerp timer)
               (cancel-timer timer)))
           org-roam-obsidian--pending-syncs)
  (clrhash org-roam-obsidian--pending-syncs))

(defun org-roam-obsidian--file-change-callback (event)
  "Handle file system change event.
EVENT format: (DESCRIPTOR ACTION FILE [FILE1])"
  (message "File-notify event received: %S" event)
  (let* ((action (nth 1 event))
         (file (nth 2 event)))

    (message "File-notify: action=%s file=%s" action file)
    (message "File-notify: file-exists=%s file-regular=%s"
             (file-exists-p file)
             (file-regular-p file))
    (message "File-notify: is-org=%s is-md=%s"
             (string-suffix-p ".org" file)
             (string-suffix-p ".md" file))

    ;; Check if file should be ignored (to prevent infinite loops)
    (if (org-roam-obsidian--should-ignore-file file)
        (message "File-notify: ignoring %s (recently synced)" (file-name-nondirectory file))

      ;; Filter: only process .org and .md files
      (when (and file
                 (file-exists-p file)
                 (or (string-suffix-p ".org" file)
                     (string-suffix-p ".md" file))
                 ;; Skip hidden files and Obsidian metadata
                 (not (string-match-p "/\\." file)))

        (message "File-notify: scheduling sync for %s" file)

        ;; Debounce: cancel existing timer for this file
        (when-let ((existing-timer (gethash file org-roam-obsidian--pending-syncs)))
          (when (timerp existing-timer)
            (cancel-timer existing-timer)))

        ;; Schedule sync after 1 second of idle time
        (let ((new-timer
               (run-with-idle-timer 1.0 nil
                 (lambda ()
                   (condition-case err
                       (progn
                         (org-roam-obsidian-sync-current-file-by-path file)
                         (remhash file org-roam-obsidian--pending-syncs))
                     (error
                      (message "File-notify sync error for %s: %s"
                               (file-name-nondirectory file)
                               (error-message-string err))
                      (remhash file org-roam-obsidian--pending-syncs)))))))
          (puthash file new-timer org-roam-obsidian--pending-syncs))))))

(defun org-roam-obsidian-sync-current-file-by-path (file)
  "Sync FILE to its corresponding pair.
FILE should be an absolute path to either an org or md file."
  (message "Sync-by-path called for: %s" file)
  (org-roam-obsidian--load-mappings)

  (cond
   ;; In org-roam directory
   ((string-prefix-p (expand-file-name org-roam-obsidian-roam-path) file)
    (message "File is in org-roam directory")
    (when (file-exists-p file)
      (let* ((org-id (org-roam-obsidian--ensure-id-in-org-file file))
             (mapping (org-roam-obsidian--get-mapping-by-id org-id)))
        (if mapping
            ;; Existing mapping - sync normally
            (let ((md-file (alist-get 'md-file mapping)))
              (message "Found mapping, syncing to: %s" md-file)
              (org-roam-obsidian--sync-org-to-md file md-file)
              (org-roam-obsidian--save-mappings)
              (message "Sync completed successfully"))
          ;; No mapping - treat as new file
          (message "New org file detected, creating corresponding markdown file...")
          (let* ((title (org-roam-obsidian--extract-title-from-org file))
                 (md-file (org-roam-obsidian--generate-md-path file)))
            (org-roam-obsidian--sync-org-to-md file md-file)
            (org-roam-obsidian--add-mapping file md-file org-id title)
            (org-roam-obsidian--save-mappings)
            (message "Created new markdown file: %s"
                     (file-relative-name md-file org-roam-obsidian-vault-path)))))))

   ;; In Obsidian directory
   ((string-prefix-p (expand-file-name org-roam-obsidian-vault-path) file)
    (message "File is in Obsidian directory")
    (when (file-exists-p file)
      (let ((org-id (org-roam-obsidian--get-mapping-by-md-file file)))
        (if org-id
            ;; Existing mapping - sync normally
            (let* ((mapping (org-roam-obsidian--get-mapping-by-id org-id))
                   (org-file (alist-get 'org-file mapping)))
              (message "Found mapping, syncing to: %s" org-file)
              (org-roam-obsidian--sync-md-to-org file org-file)
              (org-roam-obsidian--save-mappings)
              (message "Sync completed successfully"))
          ;; No mapping - treat as new file
          (message "New markdown file detected, creating corresponding org file...")
          (let* ((title (org-roam-obsidian--extract-title-from-md file))
                 (org-file (org-roam-obsidian--generate-org-path file))
                 (org-id (org-id-new)))
            (org-roam-obsidian--sync-md-to-org file org-file)
            (org-roam-obsidian--add-mapping org-file file org-id title)
            (org-roam-obsidian--save-mappings)
            (message "Created new org file: %s"
                     (file-relative-name org-file org-roam-obsidian-roam-path)))))))

   (t
    (message "WARNING: File is not in org-roam or Obsidian directory: %s" file))))

;;; Periodic Sync Mode

(defun org-roam-obsidian--start-timer ()
  "Start periodic sync timer."
  (org-roam-obsidian--stop-timer)
  (setq org-roam-obsidian--timer
        (run-with-timer org-roam-obsidian-sync-interval
                       org-roam-obsidian-sync-interval
                       #'org-roam-obsidian--timer-function)))

(defun org-roam-obsidian--stop-timer ()
  "Stop periodic sync timer."
  (when org-roam-obsidian--timer
    (cancel-timer org-roam-obsidian--timer)
    (setq org-roam-obsidian--timer nil)))

(defun org-roam-obsidian--timer-function ()
  "Function called by periodic sync timer."
  (condition-case err
      (org-roam-obsidian-sync)
    (error
     (message "Org-roam-Obsidian sync error: %s" (error-message-string err)))))

(defun org-roam-obsidian--update-sync-mode ()
  "Update sync mode based on current settings."
  ;; Handle on-change sync (file notifications)
  (if org-roam-obsidian-sync-on-change
      (org-roam-obsidian--start-file-watching)
    (org-roam-obsidian--stop-file-watching))

  ;; Handle periodic sync
  (if org-roam-obsidian-sync-periodic
      (org-roam-obsidian--start-timer)
    (org-roam-obsidian--stop-timer)))

;;;###autoload
(defun org-roam-obsidian-toggle-on-change-sync ()
  "Toggle on-change sync on/off."
  (interactive)
  (setq org-roam-obsidian-sync-on-change
        (not org-roam-obsidian-sync-on-change))
  (org-roam-obsidian--update-sync-mode)
  (message "On-change sync: %s"
           (if org-roam-obsidian-sync-on-change "enabled" "disabled")))

;;;###autoload
(defun org-roam-obsidian-toggle-periodic-sync ()
  "Toggle periodic sync on/off."
  (interactive)
  (setq org-roam-obsidian-sync-periodic
        (not org-roam-obsidian-sync-periodic))
  (org-roam-obsidian--update-sync-mode)
  (message "Periodic sync: %s"
           (if org-roam-obsidian-sync-periodic "enabled" "disabled")))

;;;###autoload
(defun org-roam-obsidian-diagnose ()
  "Diagnose file watching and sync configuration."
  (interactive)
  (let ((buf (get-buffer-create "*Org-Roam-Obsidian Diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Org-Roam Obsidian Sync Diagnostics ===\n\n")

      (insert "** Configuration **\n")
      (insert (format "Sync mode enabled: %s\n" org-roam-obsidian-sync-mode))
      (insert (format "Sync on change: %s\n" org-roam-obsidian-sync-on-change))
      (insert (format "Sync periodic: %s\n" org-roam-obsidian-sync-periodic))
      (insert (format "Org-roam path: %s\n" org-roam-obsidian-roam-path))
      (insert (format "Obsidian vault path: %s\n" org-roam-obsidian-vault-path))

      (insert "\n** Directory Status **\n")
      (insert (format "Org-roam directory exists: %s\n"
                      (file-directory-p org-roam-obsidian-roam-path)))
      (insert (format "Obsidian directory exists: %s\n"
                      (file-directory-p org-roam-obsidian-vault-path)))

      (insert "\n** File Watchers **\n")
      (insert (format "Number of active watchers: %d\n"
                      (length org-roam-obsidian--file-watchers)))
      (insert (format "Watcher descriptors: %S\n" org-roam-obsidian--file-watchers))

      (insert "\n** Pending Syncs **\n")
      (insert (format "Number of pending syncs: %d\n"
                      (hash-table-count org-roam-obsidian--pending-syncs)))

      (insert "\n** File Notify Support **\n")
      (insert (format "File notify supported: %s\n"
                      (if (fboundp 'file-notify-add-watch) "yes" "no")))
      (when (boundp 'file-notify--library)
        (insert (format "File notify library: %s\n" file-notify--library)))

      (insert "\n** Mappings **\n")
      (insert (format "Number of file mappings: %d\n"
                      (hash-table-count org-roam-obsidian--file-map))))
    (display-buffer buf)))

;;; Minor Mode

;;;###autoload
(define-minor-mode org-roam-obsidian-sync-mode
  "Minor mode for bidirectional sync between org-roam and Obsidian."
  :global t
  :lighter " OR-Sync"
  :group 'org-roam-obsidian-sync
  (if org-roam-obsidian-sync-mode
      (progn
        (org-roam-obsidian--load-mappings)
        (org-roam-obsidian--update-sync-mode)
        (message "Org-roam-Obsidian sync mode enabled (on-change: %s, periodic: %s)"
                 (if org-roam-obsidian-sync-on-change "yes" "no")
                 (if org-roam-obsidian-sync-periodic "yes" "no")))
    (org-roam-obsidian--stop-file-watching)
    (org-roam-obsidian--stop-timer)
    (message "Org-roam-Obsidian sync mode disabled")))

(provide 'org-roam-obsidian-sync)
;;; org-roam-obsidian-sync.el ends here
