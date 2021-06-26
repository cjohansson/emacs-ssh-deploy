;;; ssh-deploy-diff-mode.el --- Mode for interactive directory differences  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Please see README.md from the same repository for extended documentation.

;;; Code:

(require 'ssh-deploy)

(defconst ssh-deploy-diff-mode--keywords
  '(
    "DIRECTORY A"
    "DIRECTORY B"
    "EXCLUDE-LIST"
    "FILES ONLY IN A"
    "FILES ONLY IN B"
    "FILES IN BOTH BUT DIFFERS"
    "HELP"
    )
  "Use list of keywords to build regular expression for syntax highlighting.")

(defconst ssh-deploy-diff-mode--font-lock-keywords
  (let ((regex (concat "\\<" (regexp-opt ssh-deploy-diff-mode--keywords t) "\\>")))
    (list
     `(,regex . font-lock-builtin-face)
     '("\\('\\w*'\\)" . font-lock-variable-name-face)))
  "Minimal highlighting expressions for SSH Deploy Diff major mode.")

(defvar ssh-deploy-diff-mode-map
  (let ((map (make-keymap)))
    (define-key map "C" 'ssh-deploy-diff-mode-copy-handler)
    (define-key map "a" 'ssh-deploy-diff-mode-copy-a-handler)
    (define-key map "b" 'ssh-deploy-diff-mode-copy-b-handler)
    (define-key map "D" 'ssh-deploy-diff-mode-delete-handler)
    (define-key map (kbd "<tab>") 'ssh-deploy-diff-mode-difference-handler)
    (define-key map "g" 'ssh-deploy-diff-mode-refresh-handler)
    (define-key map (kbd "<return>") 'ssh-deploy-diff-mode-open-handler)
    (define-key map (kbd "<RET>") 'ssh-deploy-diff-mode-open-handler)
    map)
  "Key-map for SSH Deploy Diff major mode.")

(defun ssh-deploy-diff-mode-copy-handler() "Start the copy action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--copy))
(defun ssh-deploy-diff-mode-copy-a-handler() "Start the copy A action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--copy-a))
(defun ssh-deploy-diff-mode-copy-b-handler() "Start the copy B action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--copy-b))
(defun ssh-deploy-diff-mode-delete-handler() "Start the delete action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--delete))
(defun ssh-deploy-diff-mode-difference-handler() "Start the difference action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--difference))
(defun ssh-deploy-diff-mode-refresh-handler() "Start the refresh action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--refresh))
(defun ssh-deploy-diff-mode-open-handler() "Start the open action." (interactive)(ssh-deploy-diff-mode--action-handler #'ssh-deploy-diff-mode--open))

(defun ssh-deploy-diff-mode--get-parts ()
  "Return current file and section if any."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((file nil))
      (when (looking-at "^- ")
        (let* ((start (+ 2 (line-beginning-position)))
               (end (line-end-position)))
          (setq file (buffer-substring-no-properties start end))))
      (while (and (> (line-number-at-pos) 1)
                  (not (looking-at "^[A-Z]+")))
        (forward-line -1))
      (when (looking-at "^[A-Z]")
        (let* ((start (line-beginning-position))
               (end (line-end-position))
               (section (buffer-substring-no-properties start end)))
          (setq section (replace-regexp-in-string ": ([0-9]+)\\'" "" section))
          (setq section
                (pcase section
                  ("DIRECTORY A" 'directory-a)
                  ("DIRECTORY B" 'directory-b)
                  ("EXCLUDE-LIST" 'exclude-list)
                  ("FILES ONLY IN A" 'only-in-a)
                  ("FILES ONLY IN B" 'only-in-b)
                  ("FILES IN BOTH BUT DIFFERS" 'in-both)
                  (_ (message "Could not find section %s" section)
                     section)))
          (while (and (> (line-number-at-pos) 1)
                      (not (looking-at "^DIRECTORY B:")))
            (forward-line -1))
          (when (looking-at "^DIRECTORY B:")
            (let* ((start (line-beginning-position))
                   (end (line-end-position))
                   (directory-b (buffer-substring-no-properties start end)))
              (setq directory-b (replace-regexp-in-string "DIRECTORY B: " "" directory-b))

              (while (and (> (line-number-at-pos) 1)
                          (not (looking-at "^DIRECTORY A:")))
                (forward-line -1))
              (when (looking-at "^DIRECTORY A:")
                (let* ((start (line-beginning-position))
                       (end (line-end-position))
                       (directory-a (buffer-substring-no-properties start end)))
                  (setq directory-a (replace-regexp-in-string "DIRECTORY A: " "" directory-a))
                  (list file section directory-a directory-b))))))))))

(defun ssh-deploy-diff-mode--action-handler (action)
  "Route valid ACTION to their functions."
  (interactive)
  (let ((parts (ssh-deploy-diff-mode--get-parts)))
    (unless (eq parts nil)
      (cond
       ((null parts) (message "Found nothing to do"))
       ((not (or (nth 0 parts)
                 ;; FIXME: Comparing equality of functions is bad karma!
                 (eq action #'ssh-deploy-diff-mode--refresh)))
        (message "Found nothing to do in the section for action %s"
                 (replace-regexp-in-string "ssh-deploy-diff-mode--" ""
                                           (format "%s" action))))
       (t (funcall action parts))))))

(defun ssh-deploy-diff-mode--refresh (parts)
  "Refresh current difference query based on PARTS."
  (interactive)
  (let ((root-local (nth 2 parts))
        (root-remote (nth 3 parts)))
    (kill-this-buffer)
    (ssh-deploy-diff-directories root-local root-remote)))

(defun ssh-deploy-diff-mode--copy (parts)
  "Perform an upload or download depending on section in PARTS."
  (let* ((file-name (nth 0 parts))
         (root-local (file-truename (nth 2 parts)))
         (root-remote (nth 3 parts))
         (path-local (file-truename (expand-file-name file-name root-local)))
         (path-remote (expand-file-name file-name root-remote))
         (section (nth 1 parts)))
    (pcase section
      ('only-in-a
       (ssh-deploy-upload path-local path-remote 1))
      ('only-in-b
       (ssh-deploy-download path-remote path-local))
      (_ (message "Copy is not available in this section")))))

(defun ssh-deploy-diff-mode--copy-a (parts)
  "Perform a upload of local-path to remote-path based on PARTS from section A or section BOTH."
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (file-truename (nth 2 parts)))
         (root-remote (nth 3 parts))
         (path-local (file-truename (expand-file-name file-name root-local)))
         (path-remote (expand-file-name file-name root-remote)))
    (cond ((memq section '(only-in-a in-both))
           (ssh-deploy-upload path-local path-remote 1))
          (t (message "Copy A is not available in this section")))))

(defun ssh-deploy-diff-mode--copy-b (parts)
  "Perform an download of remote-path to local-path based on PARTS from section B or section BOTH."
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (file-truename (nth 2 parts)))
         (root-remote (nth 3 parts))
         (path-local (file-truename (expand-file-name file-name root-local)))
         (path-remote (expand-file-name file-name root-remote)))
    (cond ((memq section '(only-in-b in-both))
           (ssh-deploy-download path-remote path-local))
          (t (message "Copy B is not available in this section")))))

(defun ssh-deploy-diff-mode--delete (parts)
  "Delete path in both, only in a or only in b based on PARTS from section A, B or BOTH."
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (nth 2 parts))
         (root-remote (nth 3 parts))
         (path-local (file-truename (expand-file-name file-name root-local)))
         (path-remote (expand-file-name file-name root-remote)))
    (pcase section
      ('in-both
       (let ((yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the file '%s': " file-name))))
         (when (string= yes-no-prompt "yes")
           (ssh-deploy-delete-both path-local))))
      ('only-in-a
       (ssh-deploy-delete path-local))
      ('only-in-b
       (ssh-deploy-delete path-remote))
      (_ (message "Delete is not available in this section")))))

(defun ssh-deploy-diff-mode--difference (parts)
  "If file exists in both start a difference session based on PARTS."
  (let ((section (nth 1 parts)))
    (if (eq section 'in-both)
        (let* ((file-name (nth 0 parts))
               (root-local (file-truename (nth 2 parts)))
               (root-remote (nth 3 parts))
               (path-local (file-truename (expand-file-name file-name root-local)))
               (path-remote (expand-file-name file-name root-remote)))
          (ssh-deploy-diff-files path-local path-remote))
      (message "File must exists in both roots to perform a difference action."))))

(defun ssh-deploy-diff-mode--open (parts)
  "Perform a open file action based on PARTS from section A or section B."
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (file-truename (nth 2 parts)))
         (root-remote (nth 3 parts))
         (path-local (file-truename (expand-file-name file-name root-local)))
         (path-remote (expand-file-name file-name root-remote)))
    (pcase section
      ('only-in-a
       (message "Opening file '%s'" path-local)
       (find-file path-local))
      ('only-in-b
       (message "Opening file '%s'" path-remote)
       (find-file path-remote))
      (_ (message "Open is not available in this section")))))

(define-derived-mode ssh-deploy-diff-mode special-mode "SSH-Deploy-Diff"
  "Major mode for SSH Deploy interactive directory differences."
  (set (make-local-variable 'font-lock-defaults)
       '(ssh-deploy-diff-mode--font-lock-keywords)))

(provide 'ssh-deploy-diff-mode)

;;; ssh-deploy-diff-mode.el ends here
