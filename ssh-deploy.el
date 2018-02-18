;;; ssh-deploy.el --- Deployment via TRAMP, global or per directory.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 5 Jul 2016
;; Modified: 18 Feb 2018
;; Version: 1.76
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-ssh-deploy

;; Package-Requires: ((emacs "24"))

;; Copyright (C) 2017 - 2018 Christian Johansson

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

;; ssh-deploy enables automatic deploys on explicit-save actions, manual uploads, renaming,
;; deleting, downloads, file and directory differences, launching remote terminals,
;; detection of remote changes and remote directory browsing via TRAMP.
;;
;; For asynchrous operations it uses package async.el.
;;
;; By setting the variables (globally, per directory or per file):
;; ssh-deploy-root-local,ssh-deploy-root-remote, ssh-deploy-on-explicit-save
;; you can setup a directory for TRAMP deployment.
;;
;; For asynchronous transfers you need to setup ~/.netrc, ~/.authinfo or ~/.authinfo.gpg or key-based authorization or equivalent for automatic authentication.
;;
;; Example contents of ~/.netrc, ~/.authinfo or ~/.authinfo.gpg for password-based interaction-free authentication:
;; machine myserver.com login myuser port ftp password mypassword
;; machine myserver2.com login myuser2 port ssh password mypassword2
;; machine myserver3.com login myuser3 port sftp password mypassword3
;;
;; Set permissions to this file to 600 with your user as the owner.
;;
;; - To setup a upload hook on save do this:
;;     (add-hook 'after-save-hook (lambda() (if (and (boundp 'ssh-deploy-on-explicit-save) ssh-deploy-on-explicit-save) (ssh-deploy-upload-handler)) ))
;;
;; - To setup automatic storing of base revisions and detection of remote changes do this:
;;     (add-hook 'find-file-hook (lambda() (if (and (boundp 'ssh-deploy-automatically-detect-remote-changes) ssh-deploy-automatically-detect-remote-changes) (ssh-deploy-remote-changes-handler)) ))
;;
;; - To set key-bindings do something like this:
;;     (global-set-key (kbd "C-c C-z f") (lambda() (interactive)(ssh-deploy-upload-handler-forced) ))
;;     (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
;;     (global-set-key (kbd "C-c C-z D") (lambda() (interactive)(ssh-deploy-delete-handler) ))
;;     (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
;;     (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
;;     (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-eshell-base-handler) ))
;;     (global-set-key (kbd "C-c C-z T") (lambda() (interactive)(ssh-deploy-remote-terminal-eshell-handler) ))
;;     (global-set-key (kbd "C-c C-z R") (lambda() (interactive)(ssh-deploy-rename-handler) ))
;;     (global-set-key (kbd "C-c C-z e") (lambda() (interactive)(ssh-deploy-remote-changes-handler) ))
;;     (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-base-handler) ))
;;     (global-set-key (kbd "C-c C-z B") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))
;;     (global-set-key (kbd "C-c C-z o") (lambda() (interactive)(ssh-deploy-open-remote-file-handler) ))
;;
;; - To install and set-up using use-package and hydra do this:
;;   (use-package ssh-deploy
;;     :ensure t
;;     :demand
;;     :bind (("C-c C-z" . hydra-ssh-deploy/body))
;;     :hook ((after-save . (lambda() (if (and (boundp 'ssh-deploy-on-explicit-save) ssh-deploy-on-explicit-save) (ssh-deploy-upload-handler)) ))
;;            (find-file . (lambda() (if (and (boundp 'ssh-deploy-automatically-detect-remote-changes) ssh-deploy-automatically-detect-remote-changes) (ssh-deploy-remote-changes-handler)) )))
;;     :config
;;     (defhydra hydra-ssh-deploy (:color red :hint nil)
;;       "
;; _u_: Upload                              _f_: Force Upload
;; _d_: Download
;; _D_: Delete
;; _x_: Difference
;; _t_: Eshell Base Terminal                _T_: Eshell Relative Terminal
;; _e_: Detect Remote Changes
;; _R_: Rename
;; _b_: Browse Base                         _B_: Browse Relative
;; _o_: Open current file on remote
;; "
;;       ("f" ssh-deploy-upload-handler-forced)
;;       ("u" ssh-deploy-upload-handler)
;;       ("d" ssh-deploy-download-handler)
;;       ("D" ssh-deploy-delete-handler)
;;       ("x" ssh-deploy-diff-handler)
;;       ("t" ssh-deploy-remote-terminal-eshell-base-handler)
;;       ("T" ssh-deploy-remote-terminal-eshell-handler)
;;       ("e" ssh-deploy-remote-changes-handler)
;;       ("R" ssh-deploy-rename-handler)
;;       ("b" ssh-deploy-browse-remote-base-handler)
;;       ("B" ssh-deploy-browse-remote-handler)
;;       ("o" ssh-deploy-open-remote-file-handler)))
;;
;;
;; Here is an example for SSH deployment, /Users/Chris/Web/Site1/.dir-locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
;;   (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/site1/")
;;   (ssh-deploy-on-explicit-save . t)
;;   (ssh-deploy-async . t)
;; )))
;;
;; Here is an example for SFTP deployment, /Users/Chris/Web/Site2/.dir-locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
;;   (ssh-deploy-root-remote . "/sftp:myuser@myserver.com:/var/www/site2/")
;;   (ssh-deploy-on-explicit-save . nil)
;;   (ssh-deploy-async . nil)
;; )))
;;
;; Here is an example for FTP deployment, /Users/Chris/Web/Site3/.dir-locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site3/")
;;   (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/var/www/site3/")
;; )))
;;
;;
;; Now when you are in a directory which is configured for deployment.
;;
;; Here is a list of other variables you can set globally or per directory:

;; * ssh-deploy-root-local - The local root that should be under deployment *(string)*
;; * ssh-deploy-root-remote - The remote TRAMP root that is used for deployment *(string)*
;; * ssh-deploy-debug - Enables debugging messages *(boolean)*
;; * ssh-deploy-revision-folder - The folder used for storing local revisions *(string)*
;; * ssh-deploy-automatically-detect-remote-changes - Enables automatic detection of remote changes *(boolean)*
;; * ssh-deploy-on-explicit-save - Enabled automatic uploads on save *(boolean)*
;; * ssh-deploy-exclude-list - A list defining what paths to exclude from deployment *(list)*
;; * ssh-deploy-async - Enables asynchronous transfers (you need to have `async.el` installed as well) *(boolean)*
;;
;; Please see README.md from the same repository for extended documentation.

;;; Code:

(require 'ssh-deploy-diff-mode)

(defgroup ssh-deploy nil
  "Upload, download, difference, browse and terminal handler for files and directories on remote hosts via TRAMP."
  :group 'tools
  :group 'convenience)

(defcustom ssh-deploy-root-local nil
  "String variable of local root, nil by default."
  :type 'string
  :group 'ssh-deploy)
(put 'ssh-deploy-root-local 'permanent-local t)

(defcustom ssh-deploy-root-remote nil
  "String variable of remote root, nil by default."
  :type 'string
  :group 'ssh-deploy)
(put 'ssh-deploy-root-remote 'permanent-local t)

(defcustom ssh-deploy-on-explicit-save t
  "Boolean variable if deploy should be made on explicit save, t by default."
  :type 'boolean
  :group 'ssh-deploy)
(put 'ssh-deploy-on-explicit-save 'permanent-local t)

(defcustom ssh-deploy-debug nil
  "Boolean variable if debug messages should be shown, nil by default."
  :type 'boolean
  :group 'ssh-deploy)
(put 'ssh-deploy-debug 'permanent-local t)

(defcustom ssh-deploy-async t
  "Boolean variable if asynchrous method for transfers should be used, t by default."
  :type 'boolean
  :group 'ssh-deploy)
(put 'ssh-deploy-async 'permanent-local t)

(defcustom ssh-deploy-revision-folder "~/.ssh-deploy-revisions/"
  "String variable with path to revisions with trailing slash."
  :type 'string
  :group 'ssh-deploy)
(put 'ssh-deploy-revision-folder 'permanent-local t)

(defcustom ssh-deploy-automatically-detect-remote-changes t
  "Detect remote changes and store base revisions automatically, t by default."
  :type 'boolean
  :group 'ssh-deploy)
(put 'ssh-deploy-automatically-detect-remote-changes 'permanent-local t)

(defcustom ssh-deploy-exclude-list '(".git/" ".dir-locals.el")
  "List of strings that if found in paths will exclude paths from sync, '(\"/.git\"/' \".dir-locals.el\") by default."
  :type 'list
  :group 'ssh-deploy)
(put 'ssh-deploy-exclude-list 'permanent-local t)


;; PRIVATE FUNCTIONS
;;
;; these functions are only used internally and should be of no value to outside public and handler functions.
;; these functions MUST not use module variables.

(defun ssh-deploy--insert-keyword (text)
  "Insert TEXT as bold text."
  (put-text-property 0 (length text) 'face 'font-lock-keyword-face text)
  (insert text))

(defun ssh-deploy--get-revision-path (path root)
  "Generate revision-path for PATH in ROOT."
  (if (not (file-exists-p root))
      (make-directory root))
  (concat root (replace-regexp-in-string "\\(/\\|@\\|:\\)" "_" path)))

(defun ssh-deploy--file-is-in-path (file path)
  "Return non-nil if FILE is in the path PATH."
  (not (null (string-match path file))))

(defun ssh-deploy--file-is-included (path exclude-list)
  "Return non-nil if PATH is not in EXCLUDE-LIST."
  (let ((not-found t))
    (dolist (element exclude-list)
      (if (and (not (null element))
               (not (null (string-match element path))))
          (setq not-found nil)))
    not-found))

(defun ssh-deploy--get-relative-path (root path)
  "Return a string for the relative path based on ROOT and PATH."
  (replace-regexp-in-string root "" path))

(defun ssh-deploy--is-not-empty-string (string)
  "Return non-nil if the STRING is not empty and not nil.  Expects string."
  (and (not (null string))
       (not (zerop (length string)))))

(defun ssh-deploy--upload-via-tramp-async (path-local path-remote force revision-folder)
  "Upload PATH-LOCAL to PATH-REMOTE via TRAMP asynchronously and FORCE upload despite remote change, check for revisions in REVISION-FOLDER."
  (if (fboundp 'async-start)
      (let ((file-or-directory (not (file-directory-p path-local))))
        (if file-or-directory
            (let ((revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
              (message "Uploading file '%s' to '%s'.. (asynchronously)" path-local path-remote)
              (async-start
               `(lambda()
                  (require 'ediff-util)
                  (if (fboundp 'ediff-same-file-contents)
                      (if (or (eq t ,force) (not (file-exists-p ,path-remote)) (and (file-exists-p ,revision-path) (ediff-same-file-contents ,revision-path ,path-remote)))
                          (progn
                            (if (not (file-directory-p (file-name-directory ,path-remote)))
                                (make-directory (file-name-directory ,path-remote) t))
                            (copy-file ,path-local ,path-remote t t t t)
                            (copy-file ,path-local ,revision-path t t t t)
                            (list 0 (format "Upload of file '%s' completed. (asynchronously)" ,path-remote)))
                        (list 1 (format "Remote file '%s' has changed, please download or diff. (asynchronously)" ,path-remote)))
                    (list 1 "Function 'ediff-same-file-contents' is missing. (asynchronously)")))
               (lambda(return)
                 (if (= (nth 0 return) 0)
                     (message (nth 1 return))
                   (display-warning "ssh-deploy" (nth 1 return) :warning)))))
          (progn
            (message "Uploading directory '%s' to '%s'.. (asynchronously)" path-local path-remote)
            (async-start
             `(lambda()
                (copy-directory ,path-local ,path-remote t t t)
                ,path-local)
             (lambda(return-path)
               (message "Upload of directory '%s' finished. (asynchronously)" return-path))))))
    (message "async.el is not installed")))

(defun ssh-deploy--upload-via-tramp (path-local path-remote force revision-folder)
  "Upload PATH-LOCAL to PATH-REMOTE via TRAMP synchronously and FORCE despite remote change compared with copy in REVISION-FOLDER."
  (let ((file-or-directory (not (file-directory-p path-local)))
        (revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
    (if file-or-directory
        (progn
          (require 'ediff-util)
          (if (fboundp 'ediff-same-file-contents)
              (if (or (eq t force)
                      (not (file-exists-p path-remote))
                      (and (file-exists-p revision-path) (ediff-same-file-contents revision-path path-remote)))
                  (progn
                    (message "Uploading file '%s' to '%s'.. (synchronously)" path-local path-remote)
                    (if (not (file-directory-p (file-name-directory path-remote)))
                        (make-directory (file-name-directory path-remote) t))
                    (copy-file path-local path-remote t t t t)
                    (ssh-deploy-store-revision path-local revision-folder)
                    (message "Upload '%s' completed. (synchronously)" path-local))
                (display-warning "ssh-deploy" (format "Remote file '%s' has changed, please download or diff. (synchronously)" path-remote) :warning))
            (display-warning "ssh-deploy" "Function 'ediff-same-file-contents' is missing." :warning)))
      (progn
        (message "Uploading directory '%s' to '%s'.. (synchronously)" path-local path-remote)
        (copy-directory path-local path-remote t t t)
        (message "Upload '%s' finished. (synchronously)" path-local)))))

(defun ssh-deploy--download-via-tramp-async (path-remote path-local revision-folder)
  "Download PATH-REMOTE to PATH-LOCAL via TRAMP asynchronously and make a copy in REVISION-FOLDER."
  (if (fboundp 'async-start)
      (progn
        (message "Downloading '%s' to '%s'.. (asynchronously)" path-remote path-local)
        (async-start
         `(lambda()
            (let ((file-or-directory (not (file-directory-p ,path-remote)))
                  (revision-path (ssh-deploy--get-revision-path ,path-local ,revision-folder)))
              (if file-or-directory
                  (progn
                    (copy-file ,path-remote ,path-local t t t t)
                    (copy-file ,path-local ,revision-path t t t t))
                (copy-directory ,path-remote ,path-local t t t))
              ,path-local))
         (lambda(return-path)
           (message "Download of '%s' finished. (asynchronously)" return-path))))
    (display-warning "ssh-deploy" "async.el is not installed" :warning)))

(defun ssh-deploy--download-via-tramp (path-remote path-local revision-folder)
  "Download PATH-REMOTE to PATH-LOCAL via TRAMP synchronously and store a copy in REVISION-FOLDER."
  (let ((file-or-directory (not (file-directory-p path-remote))))
    (if file-or-directory
        (progn
          (message "Downloading file '%s' to '%s'.. (synchronously)" path-remote path-local)
          (copy-file path-remote path-local t t t t)
          (ssh-deploy-store-revision path-local revision-folder)
          (message "Download of file '%s' finished. (synchronously)" path-local))
      (progn
        (message "Downloading directory '%s' to '%s'.. (synchronously)" path-remote path-local)
        (copy-directory path-remote path-local t t t)
        (message "Download of directory '%s' finished. (synchronously)" path-local)))))

;; TODO Support cases where directory-a or directory-b does not exist
(defun ssh-deploy--diff-directories-data (directory-a directory-b exclude-list)
  "Find difference between DIRECTORY-A and DIRECTORY-B but exclude paths matching EXCLUDE-LIST."
  ;; (message "Comparing a: %s to b: %s" directory-a directory-b)
  (require 'subr-x)
  (if (fboundp 'string-remove-prefix)
      (let ((files-a (directory-files-recursively directory-a ""))
            (files-b (directory-files-recursively directory-b ""))
            (files-a-only (list))
            (files-b-only (list))
            (files-both (list))
            (files-both-equals (list))
            (files-both-differs (list))
            (files-a-relative-list (list))
            (files-b-relative-list (list))
            (files-a-relative-hash (make-hash-table :test 'equal))
            (files-b-relative-hash (make-hash-table :test 'equal)))

        ;; Collected included files in directory a with relative paths
        (mapc
         (lambda (file-a-tmp)
           (let ((file-a (file-truename file-a-tmp)))
             (let ((relative-path (string-remove-prefix directory-a file-a))
                   (included t))

               ;; Check if file is excluded
               (dolist (element exclude-list)
                 (if (and (not (null element))
                          (not (null (string-match element relative-path))))
                     (setq included nil)))

               (if included
                   (progn
                     (puthash relative-path file-a files-a-relative-hash)
                     (if (equal files-a-relative-list nil)
                         (setq files-a-relative-list (list relative-path))
                       (push relative-path files-a-relative-list)))))))
         files-a)

        ;; Collected included files in directory b with relative paths
        (mapc
         (lambda (file-b-tmp)
           ;; (message "file-b-tmp: %s %s" file-b-tmp (file-truename file-b-tmp))
           (let ((file-b (file-truename file-b-tmp)))
             (let ((relative-path (string-remove-prefix directory-b file-b))
                   (included t))

               ;; Check if file is excluded
               (dolist (element exclude-list)
                 (if (and (not (null element))
                          (not (null (string-match element relative-path))))
                     (setq included nil)))

               (if included
                   (progn
                     (puthash relative-path file-b files-b-relative-hash)
                     (if (equal files-b-relative-list nil)
                         (setq files-b-relative-list (list relative-path))
                       (push relative-path files-b-relative-list)))))))
         files-b)

        ;; Collect files that only exists in directory a and files that exist in both directory a and b
        (mapc
         (lambda (file-a)
           (if (not (equal (gethash file-a files-b-relative-hash) nil))
               (if (equal files-both nil)
                   (setq files-both (list file-a))
                 (push file-a files-both))
             (if (equal files-a-only nil)
                 (setq files-a-only (list file-a))
               (push file-a files-a-only))))
         files-a-relative-list)

        ;; Collect files that only exists in directory b
        (mapc
         (lambda (file-b)
           (if (equal (gethash file-b files-a-relative-hash) nil)
               (progn
                 ;; (message "%s did not exist in hash-a" file-b)
                 (if (equal files-b-only nil)
                     (setq files-b-only (list file-b))
                   (push file-b files-b-only)))))
         files-b-relative-list)

        ;; Collect files that differ in contents and have equal contents
        (require 'ediff-util)
        (if (fboundp 'ediff-same-file-contents)
            (mapc
             (lambda (file)
               (let ((file-a (gethash file files-a-relative-hash))
                     (file-b (gethash file files-b-relative-hash)))
                 (if (ediff-same-file-contents file-a file-b)
                     (if (equal files-both-equals nil)
                         (setq files-both-equals (list file))
                       (push file files-both-equals))
                   (if (equal files-both-differs nil)
                       (setq files-both-differs (list file))
                     (push file files-both-differs)))))
             files-both))

        (list directory-a directory-b exclude-list files-both files-a-only files-b-only files-both-equals files-both-differs))
    (display-warning "ssh-deploy" "Function 'string-remove-prefix' is missing.")))

(defun ssh-deploy--diff-directories-present (diff)
  "Present difference data for directories from DIFF."
  (require 'ssh-deploy-diff-mode)

  (let ((buffer (generate-new-buffer "ssh-deploy diff"))
        (old-ssh-deploy-root-local ssh-deploy-root-local)
        (old-ssh-deploy-root-remote ssh-deploy-root-remote)
        (old-ssh-deploy-on-explicit-save ssh-deploy-on-explicit-save)
        (old-ssh-deploy-debug ssh-deploy-debug)
        (old-ssh-deploy-async ssh-deploy-async)
        (old-ssh-deploy-revision-folder ssh-deploy-revision-folder)
        (old-ssh-deploy-automatically-detect-remote-changes ssh-deploy-automatically-detect-remote-changes)
        (old-ssh-deploy-exclude-list ssh-deploy-exclude-list))
    (switch-to-buffer buffer)

    (ssh-deploy--insert-keyword "DIRECTORY A: ")
    (insert (nth 0 diff) "\n")

    (ssh-deploy--insert-keyword "DIRECTORY B: ")
    (insert (nth 1 diff) "\n")

    (if (> (length (nth 2 diff)) 0)
        (progn
          (insert "\n")
          (ssh-deploy--insert-keyword (format "EXCLUDE-LIST: (%d)" (length (nth 2 diff))))
          (dolist (element (nth 2 diff))
            (insert "\n- " element))
          (insert "\n")))

    (insert "\n")

    (if (> (length (nth 4 diff)) 0)
        (progn
          (ssh-deploy--insert-keyword (format "FILES ONLY IN A: (%d)" (length (nth 4 diff))))
          (dolist (element (nth 4 diff))
            (insert "\n- " element))
          (insert "\n\n")))

    (if (> (length (nth 5 diff)) 0)
        (progn
          (ssh-deploy--insert-keyword (format "FILES ONLY IN B: (%d)" (length (nth 5 diff))))
          (dolist (element (nth 5 diff))
            (insert "\n- " element))
          (insert "\n\n")))

    (if (> (length (nth 7 diff)) 0)
        (progn
          (ssh-deploy--insert-keyword (format "FILES IN BOTH BUT DIFFERS: (%d)" (length (nth 7 diff))))
          (dolist (element (nth 7 diff))
            (insert "\n- " element))
          (insert "\n\n")))

    (insert "\nHELP: (q) quit, (c) copy, (a) copy A to B, (b) copy B to A, (d) delete, (TAB) difference, (g) refresh")

    (ssh-deploy-diff-mode)

    ;; Set local variables same as current directories
    (set (make-local-variable 'ssh-deploy-root-local) old-ssh-deploy-root-local)
    (set (make-local-variable 'ssh-deploy-root-remote) old-ssh-deploy-root-remote)
    (set (make-local-variable 'ssh-deploy-on-explicit-save) old-ssh-deploy-on-explicit-save)
    (set (make-local-variable 'ssh-deploy-debug) old-ssh-deploy-debug)
    (set (make-local-variable 'ssh-deploy-async) old-ssh-deploy-async)
    (set (make-local-variable 'ssh-deploy-revision-folder) old-ssh-deploy-revision-folder)
    (set (make-local-variable 'ssh-deploy-automatically-detect-remote-changes) old-ssh-deploy-automatically-detect-remote-changes)
    (set (make-local-variable 'ssh-deploy-exclude-list) old-ssh-deploy-exclude-list)))


;; PUBLIC functions
;;
;; handlers use these to do things and people SHOULD be able to use these as they please themselves
;; these functions MUST only use module variables as fall-backs for missing arguments.


;;;### autoload
(defun ssh-deploy-diff-files (file-a file-b)
  "Find difference between FILE-A and FILE-B."
  (require 'ediff-util)
  (if (fboundp 'ediff-same-file-contents)
      (progn
        (message "Comparing file '%s' to '%s'.." file-a file-b)
        (if (ediff-same-file-contents file-a file-b)
            (message "Files have identical contents.")
          (ediff file-a file-b)))
    (display-warning "ssh-deploy" "Function 'ediff-same-file-contents' is missing." :warning)))

;;;### autoload
(defun ssh-deploy-diff-directories (directory-a directory-b &optional exclude-list async)
  "Find difference between DIRECTORY-A and DIRECTORY-B but exclude paths matching EXCLUDE-LIST, do it asynchronously is ASYNC is true."
  (if (not (boundp 'async))
      (setq async ssh-deploy-async))
  (if (not (boundp 'exclude-list))
      (setq exclude-list ssh-deploy-exclude-list))
  (if (and async (fboundp 'async-start))
      (let ((script-filename (file-name-directory (symbol-file 'ssh-deploy-diff-directories))))
        (message "Generating differences between directory '%s' and '%s'.. (asynchronously)" directory-a directory-b)
        (async-start
         `(lambda()
            (add-to-list 'load-path ,script-filename)
            (require 'ssh-deploy)
            (ssh-deploy--diff-directories-data ,directory-a ,directory-b (list ,@exclude-list)))
         (lambda(diff)
           (message "Differences calculated between directory '%s' and '%s' -> %s only in A, %s only in B, %s differs. (asynchronously)" (nth 0 diff) (nth 1 diff) (length (nth 4 diff)) (length (nth 5 diff)) (length (nth 7 diff)))
           (if (or (> (length (nth 4 diff)) 0) (> (length (nth 5 diff)) 0) (> (length (nth 7 diff)) 0))
               (ssh-deploy--diff-directories-present diff)))))
    (progn
      (message "Generating differences between directory '%s' and '%s'.. (synchronously)" directory-a directory-b)
      (let ((diff (ssh-deploy--diff-directories-data directory-a directory-b exclude-list)))
        (message "Differences calculated between directory '%s' and '%s' -> %s only in A, %s only in B, %s differs. (synchronously)" (nth 0 diff) (nth 1 diff) (length (nth 4 diff)) (length (nth 5 diff)) (length (nth 7 diff)))
        (if (or (> (length (nth 4 diff)) 0) (> (length (nth 5 diff)) 0) (> (length (nth 7 diff)) 0))
            (ssh-deploy--diff-directories-present diff))))))

;;;### autoload
(defun ssh-deploy-remote-changes (path-local &optional root-local root-remote async revision-folder exclude-list)
  "Check if a local revision for PATH-LOCAL on ROOT-LOCAL and if remote file has changed on ROOT-REMOTE, do it optionally asynchronously if ASYNC is true, check for copies in REVISION-FOLDER and skip if path is in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (let* ((revision-folder (or revision-folder ssh-deploy-revision-folder))
               (exclude-list (or exclude-list ssh-deploy-exclude-list))
               (revision-path (ssh-deploy--get-revision-path path-local revision-folder))
               (path-remote (concat root-remote (ssh-deploy--get-relative-path root-local path-local))))
          (if (not (file-directory-p path-local))
              (if (file-exists-p revision-path)
                  (if (and async (fboundp 'async-start))
                      (async-start
                       `(lambda()
                          (if (file-exists-p ,path-remote)
                              (progn
                                (require 'ediff-util)
                                (if (fboundp 'ediff-same-file-contents)
                                    (if (ediff-same-file-contents ,revision-path ,path-remote)
                                        (list 0 (format "Remote file '%s' has not changed. (asynchronously)" ,path-remote))
                                      (if (ediff-same-file-contents ,path-local ,path-remote)
                                          (progn
                                            (copy-file ,path-local ,revision-path t t t t)
                                            (list 0 (format "Remote file '%s' is identical to local file '%s' but different to local revision. Updated local revision. (asynchronously)" ,path-remote ,path-local)))
                                        (list 1 (format "Remote file '%s' has changed, please download or diff. (asynchronously)" ,path-remote))))
                                  (list 1 "Function 'ediff-same-file-contents' is missing. (asynchronously)")))
                            (list 0 (format "Remote file '%s' doesn't exist. (asynchronously)" ,path-remote))))
                       (lambda(return)
                         (if (= (nth 0 return) 0)
                             (message (nth 1 return))
                           (display-warning "ssh-deploy" (nth 1 return) :warning))))
                    (if (file-exists-p path-remote)
                        (progn
                          (require 'ediff-util)
                          (if (fboundp 'ediff-same-file-contents)
                              (if (ediff-same-file-contents revision-path path-remote)
                                  (message "Remote file '%s' has not changed. (synchronously)" path-remote)
                                (display-warning "ssh-deploy" (format "Remote file '%s' has changed, please download or diff. (synchronously)" path-remote) :warning))
                            (display-warning "ssh-deploy" "Function 'ediff-same-file-contents' is missing. (synchronously)" :warning)))
                      (message "Remote file '%s' doesn't exist. (synchronously)" path-remote)))
                (if (and async (fboundp 'async-start))
                    (async-start
                     `(lambda()
                        (if (file-exists-p ,path-remote)
                            (progn
                              (require 'ediff-util)
                              (if (fboundp 'ediff-same-file-contents)
                                  (if (ediff-same-file-contents ,path-local ,path-remote)
                                      (progn
                                        (copy-file ,path-local ,revision-path t t t t)
                                        (list 0 (format "Remote file '%s' has not changed, created base revision. (asynchronously)" ,path-remote)))
                                    (list 1 (format "Remote file '%s' has changed, please download or diff. (asynchronously)" ,path-remote)))
                                (list 1 "Function ediff-file-same-contents is missing. (asynchronously)")))
                          (list 0 (format "Remote file '%s' doesn't exist. (asynchronously)" ,path-remote))))
                     (lambda(return)
                       (if (= (nth 0 return) 0)
                           (message (nth 1 return))
                         (display-warning "ssh-deploy" (nth 1 return) :warning))))
                  (if (file-exists-p path-remote)
                      (progn
                        (require 'ediff-util)
                        (if (fboundp 'ediff-same-file-contents)
                            (if (ediff-same-file-contents path-local path-remote)
                                (progn
                                  (copy-file path-local revision-path t t t t)
                                  (message "Remote file '%s' has not changed, created base revision. (synchronously)" path-remote))
                              (display-warning "ssh-deploy" (format "Remote file '%s' has changed, please download or diff. (synchronously)" path-remote) :warning))
                          (display-warning "ssh-deploy" "Function 'ediff-same-file-contents' is missing. (synchronously)" :warning)))
                    (message "Remote file '%s' does not exist. (synchronously)" path-remote)))))))))

(defun ssh-deploy-delete (path &optional async debug)
  "Delete PATH and use flags ASYNC and DEBUG."
  (if (and async (fboundp 'async-start))
      (async-start
       `(lambda()
          (if (file-exists-p ,path)
              (let ((file-or-directory (not (file-directory-p ,path))))
                (progn
                  (if file-or-directory
                      (delete-file ,path t)
                    (delete-directory ,path t t))
                  (list ,path 0)))
            (list ,path 1)))
       (lambda(response)
         (cond ((= 0 (nth 1 response)) (message "Deleted '%s'. (asynchronously)" (nth 0 response)))
               ((t (display-warning "ssh-deploy" (format "Did not find '%s'. (asynchronously)" (nth 0 response)) :warning))))))
    (if (file-exists-p path)
        (let ((file-or-directory (not (file-directory-p path))))
          (progn
            (if file-or-directory
                (delete-file path t)
              (delete-directory path t t))
            (message "Deleted '%s'. (synchronously)" path)))
      (display-warning "ssh-deploy" (format "Did not find '%s'. (synchronously)" path) :warning))))

;;;### autoload
(defun ssh-deploy-delete-both (path-local &optional root-local root-remote async debug exclude-list)
  "Delete PATH-LOCAL relative to ROOT-LOCAL as well as on ROOT-REMOTE, do it asynchronously if ASYNC is non-nil, debug if DEBUG is non-nil, check if path is excluded in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (let ((exclude-list (or exclude-list ssh-deploy-exclude-list))
              (file-or-directory (not (file-directory-p path-local)))
              (path-remote (concat root-remote (ssh-deploy--get-relative-path root-local path-local))))
          (ssh-deploy-delete path-local async debug)
          (kill-this-buffer)
          (ssh-deploy-delete path-remote async debug))
      (if debug
          (message "Path '%s' is not in the root '%s' or is excluded from it." path-local root-local)))))

;;;### autoload
(defun ssh-deploy-rename (old-path-local new-path-local &optional root-local root-remote async debug exclude-list)
  "Rename OLD-PATH-LOCAL to NEW-PATH-LOCAL under ROOT-LOCAL as well as on ROOT-REMOTE, do it asynchronously if ASYNC is non-nil, debug if DEBUG is non-nil but check if path is excluded in EXCLUDE-LIST first."
  (if (not (boundp 'debug))
      (setq debug ssh-deploy-debug))
  (if (not (boundp 'async))
      (setq async ssh-deploy-async))
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path old-path-local root-local)
             (ssh-deploy--file-is-in-path new-path-local root-local)
             (ssh-deploy--file-is-included old-path-local exclude-list)
             (ssh-deploy--file-is-included new-path-local exclude-list))
        (let ((exclude-list (or exclude-list ssh-deploy-exclude-list))
              (file-or-directory (not (file-directory-p old-path-local)))
              (old-path-remote (concat root-remote (ssh-deploy--get-relative-path root-local old-path-local)))
              (new-path-remote (concat root-remote (ssh-deploy--get-relative-path root-local new-path-local))))
          (rename-file old-path-local new-path-local t)
          (if (not (file-directory-p new-path-local))
              (progn
                (rename-buffer new-path-local)
                (set-buffer-modified-p nil)
                (set-visited-file-name new-path-local))
            (dired new-path-local))
          (message "Renamed '%s' to '%s'." old-path-local new-path-local)
          (if (and async (fboundp 'async-start))
              (async-start
               `(lambda()
                  (rename-file ,old-path-remote ,new-path-remote t)
                  (list ,old-path-remote ,new-path-remote))
               (lambda(files)
                 (message "Renamed '%s' to '%s'. (asynchronously)" (nth 0 files) (nth 1 files))))
            (progn
              (rename-file old-path-remote new-path-remote t)
              (message "Renamed '%s' to '%s'. (synchronously)" old-path-remote new-path-remote))))
      (if debug
          (message "Path '%s' or '%s' is not in the root '%s' or is excluded from it." old-path-local new-path-local root-local)))))

;;;### autoload
(defun ssh-deploy-browse-remote (path-local &optional root-local root-remote exclude-list)
  "Browse PATH-LOCAL in `dired-mode' on remote where it is inside ROOT-LOCAL and mirrored on ROOT-REMOTE and not in EXCLUDE-LIST."
  (let ((exclude-list (or exclude-list ssh-deploy-exclude-list))
        (root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (let ((path-remote (concat root-remote (ssh-deploy--get-relative-path root-local path-local))))
          (message "Opening '%s' for browsing on remote host.." path-remote)
          (dired path-remote)))))

;;;### autoload
(defun ssh-deploy-remote-terminal-eshell (path-local &optional root-local root-remote exclude-list)
  "Browse PATH-LOCAL inside ROOT-LOCAL on ROOT-REMOTE in `eshell-mode' if not in EXCLUDE-LIST."
  (let ((exclude-list (or exclude-list ssh-deploy-exclude-list))
        (root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (let ((path-remote (concat root-remote (ssh-deploy--get-relative-path root-local path-local))))
          (let ((old-directory default-directory))
            (require 'eshell)
            (if (and (fboundp 'eshell-kill-input)
                     (fboundp 'eshell-send-input))
                (progn
                  (message "Opening eshell on '%s'.." path-remote)
                  (defvar eshell-buffer-name)
                  (let ((old-eshell-buffer-name eshell-buffer-name))
                    (setq eshell-buffer-name path-remote)
                    (let ((eshell-buffer (eshell)))
                      (goto-char (point-max))
                      (eshell-kill-input)
                      (insert (concat "cd " path-remote))
                      (eshell-send-input)
                      (goto-char (point-max))
                      (setq eshell-buffer-name old-eshell-buffer-name))))
              (message "Missing required eshell functions")))))))

;;;### autoload
(defun ssh-deploy-store-revision (path &optional root)
  "Store PATH in revision-folder ROOT."
  (let ((root (or root ssh-deploy-revision-folder)))
    (let ((revision-path (ssh-deploy--get-revision-path path root)))
      (message "Storing revision of '%s' at '%s'.." path revision-path)
      (copy-file path revision-path t t t t))))

;;;### autoload
(defun ssh-deploy-diff (path-local path-remote &optional root-local debug exclude-list async)
  "Find differences between PATH-LOCAL and PATH-REMOTE, where PATH-LOCAL is inside ROOT-LOCAL.  DEBUG enables feedback message, check if PATH-LOCAL is not in EXCLUDE-LIST.   ASYNC make the process work asynchronously."
  (let ((file-or-directory (not (file-directory-p path-local)))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))
    (if (not (boundp 'root-local))
        (setq root-local ssh-deploy-root-local))
    (if (not (boundp 'debug))
        (setq debug ssh-deploy-debug))
    (if (not (boundp 'async))
        (setq async ssh-deploy-async))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (if file-or-directory
            (ssh-deploy-diff-files path-local path-remote)
          (ssh-deploy-diff-directories path-local path-remote exclude-list async))
      (if debug
          (message "Path '%s' is not in the root '%s' or is excluded from it." path-local root-local)))))

;;;### autoload
(defun ssh-deploy-upload (path-local path-remote &optional force async revision-folder)
  "Upload PATH-LOCAL to PATH-REMOTE and ROOT-LOCAL via TRAMP, FORCE uploads despite remote change, ASYNC determines if transfer should be asynchronously, check version in REVISION-FOLDER."
  (if (not (boundp 'async))
      (setq async ssh-deploy-async))
  (if (not (boundp 'force))
      (setq force nil))
  (let ((revision-folder (or revision-folder ssh-deploy-revision-folder)))
    (if (and async (fboundp 'async-start))
        (ssh-deploy--upload-via-tramp-async path-local path-remote force revision-folder)
      (ssh-deploy--upload-via-tramp path-local path-remote force revision-folder))))

;;;### autoload
(defun ssh-deploy-download (path-remote path-local &optional async revision-folder)
  "Download PATH-REMOTE to PATH-LOCAL via TRAMP, ASYNC determines if transfer should be asynchrous or not, check for revisions in REVISION-FOLDER."
  (if (not (boundp 'async))
      (setq async ssh-deploy-async))
  (let ((revision-folder (or revision-folder ssh-deploy-revision-folder)))
    (if (and async (fboundp 'async-start))
        (ssh-deploy--download-via-tramp-async path-remote path-local revision-folder)
      (ssh-deploy--download-via-tramp path-remote path-local revision-folder))))


;; HANDLERS
;;
;; these functions are suited to be bound to various Emacs commands.
;; these functions MUST depend on module variables.


;;;### autoload
(defun ssh-deploy-upload-handler (&optional force)
  "Upload current path to remote if it is configured for deployment and if remote version hasn't changed or FORCE is specified."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (let ((root-local (file-truename ssh-deploy-root-local))
            path-local)
        (if (not (boundp 'force))
            (setq force nil))
        (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
                 (file-exists-p buffer-file-name))
            (setq path-local (file-truename buffer-file-name))
          (if (and (ssh-deploy--is-not-empty-string default-directory)
                   (file-exists-p default-directory))
              (setq path-local (file-truename default-directory))))
        (if (and (ssh-deploy--is-not-empty-string path-local)
                 (ssh-deploy--file-is-in-path path-local root-local)
                 (ssh-deploy--file-is-included path-local ssh-deploy-exclude-list))
            (let ((path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
              (ssh-deploy-upload path-local path-remote force ssh-deploy-async ssh-deploy-revision-folder))
          (if ssh-deploy-debug
              (message "Ignoring upload, path '%s' is empty, not in the root '%s' or is excluded from it." path-local root-local))))))

;;;### autoload
(defun ssh-deploy-upload-handler-forced ()
  "Upload current path to remote host if it is configured for deployment."
  (interactive)
  (ssh-deploy-upload-handler t))

;;;### autoload
(defun ssh-deploy-remote-changes-handler()
  "Check if local revision exists or remote file has changed if path is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
           (ssh-deploy--is-not-empty-string buffer-file-name))
      (ssh-deploy-remote-changes (file-truename buffer-file-name) (file-truename ssh-deploy-root-local) ssh-deploy-root-remote ssh-deploy-async ssh-deploy-revision-folder ssh-deploy-exclude-list)))

;;;### autoload
(defun ssh-deploy-open-remote-file-handler()
  "Check if local revision exists or remote file has changed if path is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
           (ssh-deploy--is-not-empty-string buffer-file-name))
      (let* ((root-local (file-truename ssh-deploy-root-local))
             (path-local (file-truename buffer-file-name))
             (path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
        (message "Opening file on remote '%s'" path-remote)
        (find-file path-remote))))

;;;### autoload
(defun ssh-deploy-download-handler ()
  "Download current path from remote if it is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (let ((root-local (file-truename ssh-deploy-root-local))
            path-local)
        (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
                 (file-exists-p buffer-file-name))
            (setq path-local (file-truename buffer-file-name))
          (if (and (ssh-deploy--is-not-empty-string default-directory)
                   (file-exists-p default-directory))
              (setq path-local (file-truename default-directory))))
        (if (and (ssh-deploy--is-not-empty-string path-local)
                 (ssh-deploy--file-is-in-path path-local root-local)
                 (ssh-deploy--file-is-included path-local ssh-deploy-exclude-list))
            (let ((path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
              (ssh-deploy-download path-remote path-local ssh-deploy-async ssh-deploy-revision-folder))
          (if ssh-deploy-debug
              (message "Ignoring upload, path '%s' is empty, not in the root '%s' or is excluded from it." path-local root-local))))))

;;;### autoload
(defun ssh-deploy-diff-handler ()
  "Compare current path with remote host if it is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
               (file-exists-p buffer-file-name))
          (let* ((path-local (file-truename buffer-file-name))
                 (root-local (file-truename ssh-deploy-root-local))
                 (path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
            (ssh-deploy-diff path-local path-remote root-local ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let* ((path-local (file-truename default-directory))
                   (root-local (file-truename ssh-deploy-root-local))
                   (path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
              (ssh-deploy-diff path-local path-remote root-local ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async))))))

;;;### autoload
(defun ssh-deploy-delete-handler ()
  "Delete current file or directory."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
               (file-exists-p buffer-file-name))
          (let* ((path-local (file-truename buffer-file-name))
                 (root-local (file-truename ssh-deploy-root-local))
                 (yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the file '%s': " path-local))))
            (if (string= yes-no-prompt "yes")
                (ssh-deploy-delete-both path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug)))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let* ((path-local (file-truename default-directory))
                   (root-local (file-truename ssh-deploy-root-local))
                   (yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the directory '%s': " path-local))))
              (if (string= yes-no-prompt "yes")
                  (ssh-deploy-delete-both path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list)))))))

;;;### autoload
(defun ssh-deploy-rename-handler ()
  "Rename current file or directory."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
               (file-exists-p buffer-file-name))
          (let* ((old-path-local (file-truename buffer-file-name))
                 (root-local (file-truename ssh-deploy-root-local))
                 (basename (file-name-nondirectory old-path-local))
                 (new-path-local-tmp (read-file-name "New file name:" (file-name-directory old-path-local) basename nil basename))
                 (new-path-local (file-truename new-path-local-tmp)))
            (if (not (string= old-path-local new-path-local))
                (ssh-deploy-rename old-path-local new-path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug)))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let* ((old-path-local (file-truename default-directory))
                   (root-local (file-truename ssh-deploy-root-local))
                   (basename (file-name-nondirectory old-path-local))
                   (new-path-local-tmp (read-file-name "New directory name:" (file-name-directory old-path-local) basename nil basename))
                   (new-path-local (file-truename new-path-local-tmp)))
              (if (not (string= old-path-local new-path-local))
                  (ssh-deploy-rename old-path-local new-path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list)))))))

;;;### autoload
(defun ssh-deploy-remote-terminal-eshell-handler ()
  "Open current relative path on remote host in `eshell' but only if it's configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
           (ssh-deploy--is-not-empty-string default-directory))
      (let ((path-local (file-truename default-directory))
            (root-local (file-truename ssh-deploy-root-local)))
        (ssh-deploy-remote-terminal-eshell path-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;### autoload
(defun ssh-deploy-remote-terminal-eshell-base-handler ()
  "Open base path on remote host in `eshell' but only if it's configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (let ((root-local (file-truename ssh-deploy-root-local)))
        (ssh-deploy-remote-terminal-eshell root-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;### autoload
(defun ssh-deploy-browse-remote-handler ()
  "Open current relative path on remote host in `dired-mode' if it is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
           (ssh-deploy--is-not-empty-string default-directory))
      (let ((path-local (file-truename default-directory))
            (root-local (file-truename ssh-deploy-root-local)))
        (ssh-deploy-browse-remote path-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;### autoload
(defun ssh-deploy-browse-remote-base-handler ()
  "Open base path on remote host in `dired-mode' if it is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (let ((root-local (file-truename ssh-deploy-root-local)))
        (ssh-deploy-browse-remote root-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))


;; Mark variables as safe
(put 'ssh-deploy-root-local 'safe-local-variable 'stringp)
(put 'ssh-deploy-root-remote 'safe-local-variable 'stringp)
(put 'ssh-deploy-debug 'safe-local-variable 'booleanp)
(put 'ssh-deploy-revision-folder 'safe-local-variable 'stringp)
(put 'ssh-deploy-automatically-detect-remote-changes 'safe-local-variable 'booleanp)
(put 'ssh-deploy-on-explicit-save 'safe-local-variable 'booleanp)
(put 'ssh-deploy-exclude-list 'safe-local-variable 'listp)
(put 'ssh-deploy-async 'safe-local-variable 'booleanp)

(provide 'ssh-deploy)
;;; ssh-deploy.el ends here

