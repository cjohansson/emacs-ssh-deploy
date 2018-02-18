;;; ssh-deploy-diff-mode.el --- Mode for interactive directory differences

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 1 Feb 2018
;; Modified: 18 Feb 2018
;; Version: 1.11
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-ssh-deploy

;; Package-Requires: ((emacs "24"))

;; Copyright (C) 2017 Christian Johansson

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Spathoftware Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Please see README.md from the same repository for extended documentation.

;;; Code:

;; TODO: Must explicitly send global variables, seems like settings are lost sometimes?
;; TODO: Downloading and deletion of remote files that does not exist on local root does not work?

(defvar ssh-deploy-diff-mode nil)

(defconst ssh-deploy-diff-mode--section-directory-a 0 "Section for directory a.")
(defconst ssh-deploy-diff-mode--section-directory-b 1 "Section for directory b.")
(defconst ssh-deploy-diff-mode--section-exclude-list 2 "Section for exclude-list.")
(defconst ssh-deploy-diff-mode--section-only-in-a 3 "Section for only in a.")
(defconst ssh-deploy-diff-mode--section-only-in-b 4 "Section for only in b.")
(defconst ssh-deploy-diff-mode--section-in-both 5 "Section for in both.")

(defconst ssh-deploy-diff-mode--action-copy 0 "Action for copy.")
(defconst ssh-deploy-diff-mode--action-copy-a 1 "Action for copy A.")
(defconst ssh-deploy-diff-mode--action-copy-b 2 "Action for copy B.")
(defconst ssh-deploy-diff-mode--action-delete 3 "Action for delete.")
(defconst ssh-deploy-diff-mode--action-difference 4 "Action for difference.")
(defconst ssh-deploy-diff-mode--action-refresh 5 "Action for refreshing differences.")
(defconst ssh-deploy-diff-mode--action-open 6 "Action for open file.")

(defconst ssh-deploy-diff-mode--keywords
  (list
   "DIRECTORY A"
   "DIRECTORY B"
   "EXCLUDE-LIST"
   "FILES ONLY IN A"
   "FILES ONLY IN B"
   "FILES IN BOTH BUT DIFFERS"
   "HELP"
   )
  "Use list of keywords to build regular expression for syntax highlighting.")

(let ((regex (concat "\\<" (regexp-opt ssh-deploy-diff-mode--keywords t) "\\>")))
  (defconst ssh-deploy-diff-mode--font-lock-keywords
    (list
     `(,regex . font-lock-builtin-face)
     '("\\('\\w*'\\)" . font-lock-variable-name-face))
    "Minimal highlighting expressions for SSH Deploy Diff major mode."))

(defvar ssh-deploy-diff-mode--map
  (let ((map (make-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "c" 'ssh-deploy-diff-mode-copy-handler)
    (define-key map "a" 'ssh-deploy-diff-mode-copy-a-handler)
    (define-key map "b" 'ssh-deploy-diff-mode-copy-b-handler)
    (define-key map "d" 'ssh-deploy-diff-mode-delete-handler)
    (define-key map (kbd "<tab>") 'ssh-deploy-diff-mode-difference-handler)
    (define-key map "g" 'ssh-deploy-diff-mode-refresh-handler)
    (define-key map (kbd "<return>") 'ssh-deploy-diff-mode-open-handler)
    map)
  "Key-map for SSH Deploy Diff major mode.")

(defun ssh-deploy-diff-mode-copy-handler() "Start the copy action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-copy))
(defun ssh-deploy-diff-mode-copy-a-handler() "Start the copy A action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-copy-a))
(defun ssh-deploy-diff-mode-copy-b-handler() "Start the copy B action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-copy-b))
(defun ssh-deploy-diff-mode-delete-handler() "Start the delete action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-delete))
(defun ssh-deploy-diff-mode-difference-handler() "Start the difference action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-difference))
(defun ssh-deploy-diff-mode-refresh-handler() "Start the refresh action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-refresh))
(defun ssh-deploy-diff-mode-open-handler() "Start the open action." (interactive)(ssh-deploy-diff-mode--action-handler ssh-deploy-diff-mode--action-open))

(defun ssh-deploy-diff-mode--get-parts ()
  "Return current file and section if any."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((file nil))
      (if (looking-at "^- ")
          (let* ((start (+ 2 (line-beginning-position)))
                 (end (line-end-position)))
            (setq file (buffer-substring-no-properties start end))))
      (while (and (> (line-number-at-pos) 1)
                  (not (looking-at "^[A-Z]+")))
        (forward-line -1))
      (if (looking-at "^[A-Z]")
          (let* ((start (line-beginning-position))
                 (end (line-end-position))
                 (section (buffer-substring-no-properties start end)))
            (setq section (replace-regexp-in-string ": ([0-9]+)$" "" section))
            (cond ((string= section "DIRECTORY A") (setq section ssh-deploy-diff-mode--section-directory-a))
                  ((string= section "DIRECTORY B") (setq section ssh-deploy-diff-mode--section-directory-b))
                  ((string= section "EXCLUDE-LIST") (setq section ssh-deploy-diff-mode--section-exclude-list))
                  ((string= section "FILES ONLY IN A") (setq section ssh-deploy-diff-mode--section-only-in-a))
                  ((string= section "FILES ONLY IN B") (setq section ssh-deploy-diff-mode--section-only-in-b))
                  ((string= section "FILES IN BOTH BUT DIFFERS") (setq section ssh-deploy-diff-mode--section-in-both))
                  (t (message "Could not find section %s" section)))
            (while (and (> (line-number-at-pos) 1)
                        (not (looking-at "^DIRECTORY B:")))
              (forward-line -1))
            (if (looking-at "^DIRECTORY B:")
                (let* ((start (line-beginning-position))
                       (end (line-end-position))
                       (directory-b (buffer-substring-no-properties start end)))
                  (setq directory-b (replace-regexp-in-string "DIRECTORY B: " "" directory-b))

                  (while (and (> (line-number-at-pos) 1)
                              (not (looking-at "^DIRECTORY A:")))
                    (forward-line -1))
                  (if (looking-at "^DIRECTORY A:")
                      (let* ((start (line-beginning-position))
                             (end (line-end-position))
                             (directory-a (buffer-substring-no-properties start end)))
                        (setq directory-a (replace-regexp-in-string "DIRECTORY A: " "" directory-a))
                        (list file section directory-a directory-b))))))))))

(defun ssh-deploy-diff-mode--action-handler (action)
  "Route valid ACTION to their functions."
  (interactive)
  (let ((parts (ssh-deploy-diff-mode--get-parts)))
    (if (not (eq parts nil))
        (cond ((and (not (null (nth 0 parts))) (= action ssh-deploy-diff-mode--action-copy)) (ssh-deploy-diff-mode--copy parts))
              ((and (not (null (nth 0 parts))) (= action ssh-deploy-diff-mode--action-copy-a)) (ssh-deploy-diff-mode--copy-a parts))
              ((and (not (null (nth 0 parts))) (= action ssh-deploy-diff-mode--action-copy-b)) (ssh-deploy-diff-mode--copy-b parts))
              ((and (not (null (nth 0 parts))) (= action ssh-deploy-diff-mode--action-delete)) (ssh-deploy-diff-mode--delete parts))
              ((and (not (null (nth 0 parts))) (= action ssh-deploy-diff-mode--action-difference)) (ssh-deploy-diff-mode--difference parts))
              ((and (not (null (nth 0 parts))) (= action ssh-deploy-diff-mode--action-open)) (ssh-deploy-diff-mode--open parts))
              ((= action ssh-deploy-diff-mode--action-refresh) (ssh-deploy-diff-mode--refresh parts))
              (t (message "Found nothing to do in the section for action %s" action)))
      (message "Found nothing to do"))))

(defun ssh-deploy-diff-mode--refresh (parts)
  "Refresh current difference query based on PARTS."
  (interactive)
  (require 'ssh-deploy)
  (if (and (boundp 'ssh-deploy-root-local)
           (boundp 'ssh-deploy-root-remote)
           (fboundp 'ssh-deploy-diff-directories))
      (let ((root-local (nth 2 parts))
            (root-remote (nth 3 parts)))
        (progn
          (kill-this-buffer)
          (ssh-deploy-diff-directories root-local root-remote)))))

(defun ssh-deploy-diff-mode--copy (parts)
  "Perform an upload or download depending on section in PARTS."
  (require 'ssh-deploy)
  (let* ((file-name (nth 0 parts))
         (root-local (nth 2 parts))
         (root-remote (nth 3 parts))
         (path-local (concat root-local file-name))
         (path-remote (concat root-remote file-name))
         (section (nth 1 parts)))
    (let* ((path-local (file-truename path-local))
           (root-local (file-truename root-local)))
      (if (and (fboundp 'ssh-deploy-download)
               (fboundp 'ssh-deploy-upload))
          (cond ((= section ssh-deploy-diff-mode--section-only-in-a)
                 (ssh-deploy-upload path-local path-remote))
                ((= section ssh-deploy-diff-mode--section-only-in-b)
                 (ssh-deploy-download path-remote path-local))
                (t (message "Copy is not available in this section")))
        (display-warning "ssh-deploy" "Function ssh-deploy-download or ssh-deploy-upload is missing" :warning)))))

(defun ssh-deploy-diff-mode--copy-a (parts)
  "Perform a upload of local-path to remote-path based on PARTS from section A or section BOTH."
  (require 'ssh-deploy)
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (nth 2 parts))
         (root-remote (nth 3 parts))
         (path-local (concat root-local file-name))
         (path-remote (concat root-remote file-name)))
    (let* ((path-local (file-truename path-local))
           (root-local (file-truename root-local)))
      (if (fboundp 'ssh-deploy-upload)
          (cond ((or (= section ssh-deploy-diff-mode--section-only-in-a)
                     (= section ssh-deploy-diff-mode--section-in-both))
                 (ssh-deploy-upload path-local path-remote))
                (t "Copy A is not available in this section"))
        (display-warning "ssh-deploy" "Function ssh-deploy-upload is missing" :warning)))))

(defun ssh-deploy-diff-mode--copy-b (parts)
  "Perform an download of remote-path to local-path based on PARTS from section B or section BOTH."
  (require 'ssh-deploy)
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (nth 2 parts))
         (root-remote (nth 3 parts))
         (path-local (concat root-local file-name))
         (path-remote (concat root-remote file-name)))
    (let* ((path-local (file-truename path-local))
           (root-local (file-truename root-local)))
      (if (fboundp 'ssh-deploy-download)
          (cond ((or (= section ssh-deploy-diff-mode--section-only-in-b)
                     (= section ssh-deploy-diff-mode--section-in-both))
                 (ssh-deploy-download path-remote path-local))
                (t "Copy B is not available in this section"))
        (display-warning "ssh-deploy" "Function ssh-deploy-download is missing" :warning)))))

(defun ssh-deploy-diff-mode--delete (parts)
  "Delete path in both, only in a or only in b based on PARTS from section A, B or BOTH."
  (require 'ssh-deploy)
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (nth 2 parts))
         (root-remote (nth 3 parts))
         (path-local (concat root-local file-name))
         (path-remote (concat root-remote file-name)))
    (let* ((path-local (file-truename path-local))
           (root-local (file-truename root-local)))
      (if (fboundp 'ssh-deploy-delete)
          (cond ((= section ssh-deploy-diff-mode--section-in-both)
                 (let ((yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the file '%s': " file-name))))
                   (if (string= yes-no-prompt "yes")
                       (ssh-deploy-delete path-local root-local root-remote))))
                ((= section ssh-deploy-diff-mode--section-only-in-a) (ssh-deploy-delete path-local))
                ((= section ssh-deploy-diff-mode--section-only-in-b) (ssh-deploy-delete path-remote))
                ((= section ssh-deploy-diff-mode--section-in-both) (ssh-deploy-delete path-local root-local root-remote))
                (t (message "Delete is not available in this section")))
        (display-warning "ssh-deploy" "Function ssh-deploy-delete is missing" :warning)))))

(defun ssh-deploy-diff-mode--difference (parts)
  "If file exists in both start a difference session based on PARTS."
  (require 'ssh-deploy)
  (let ((section (nth 1 parts)))
    (if (= section ssh-deploy-diff-mode--section-in-both)
        (if (fboundp 'ssh-deploy-diff-files)
            (let* ((file-name (nth 0 parts))
                   (root-local (nth 2 parts))
                   (root-remote (nth 3 parts))
                   (path-local (concat root-local file-name))
                   (path-remote (concat root-remote file-name)))
              (let* ((path-local (file-truename path-local))
                     (root-local (file-truename root-local)))
                (ssh-deploy-diff-files path-local path-remote)))
          (display-warning "ssh-deploy" "Function ssh-deploy-diff-files is missing" :warning))
      (message "File must exists in both roots to perform a difference action."))))

(defun ssh-deploy-diff-mode--open (parts)
  "Perform a open file action based on PARTS from section A or section B."
  (require 'ssh-deploy)
  (let* ((section (nth 1 parts))
         (file-name (nth 0 parts))
         (root-local (nth 2 parts))
         (root-remote (nth 3 parts))
         (path-local (concat root-local file-name))
         (path-remote (concat root-remote file-name)))
    (let* ((path-local (file-truename path-local))
           (root-local (file-truename root-local)))
      (cond ((= section ssh-deploy-diff-mode--section-only-in-a)
             (progn
               (message "Opening file '%s'" path-local)
               (find-file path-local)))
            ((= section ssh-deploy-diff-mode--section-only-in-b)
             (progn
               (message "Opening file '%s'" path-remote)
               (find-file path-remote)))
            (t (message "Open is not available in this section"))))))

(defun ssh-deploy-diff-mode ()
  "Major mode for SSH Deploy interactive directory differences."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ssh-deploy-diff-mode--map)
  (set (make-local-variable 'font-lock-defaults) '(ssh-deploy-diff-mode--font-lock-keywords))
  (setq major-mode 'ssh-deploy-diff-mode)
  (setq mode-name "SSH-Deploy-Diff")
  (read-only-mode t)
  (run-hooks 'ssh-deploy-diff-mode-hook))

(provide 'ssh-deploy-diff-mode)

;;; ssh-deploy-diff-mode.el ends here
