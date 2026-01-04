;;; org-roam-obsidian-sync.el --- Sync Org Mode Roam with Obsidian -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org, org-roam, obsidian, notes
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
(require 'dash)

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

(defcustom org-roam-obsidian-sync-mode-type 'manual
  "Default sync mode.
Options:
  \\='manual - Sync only via explicit command
  \\='on-save - Sync whenever a file is saved
  \\='periodic - Background sync at regular intervals"
  :type '(choice (const :tag "Manual" manual)
                 (const :tag "On Save" on-save)
                 (const :tag "Periodic" periodic))
  :group 'org-roam-obsidian-sync)

(defcustom org-roam-obsidian-sync-interval 300
  "Interval in seconds for periodic sync (default: 5 minutes)."
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

(defvar org-roam-obsidian--save-hook-enabled nil
  "Whether save hook is currently active.")

(defvar org-roam-obsidian--timer nil
  "Timer object for periodic sync.")

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

(defun org-roam-obsidian--generate-md-filename (title)
  "Generate Obsidian filename based on naming pattern for TITLE."
  (let* ((slug (org-roam-node-slug (org-roam-node-create :title title)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (pattern org-roam-obsidian-naming-pattern))
    (replace-regexp-in-string
     "%title" title
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

(provide 'org-roam-obsidian-sync)
;;; org-roam-obsidian-sync.el ends here
