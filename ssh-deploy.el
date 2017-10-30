;;; ssh-deploy.el --- Deployment via SSH or FTP, global or per directory.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 5 Jul 2016
;; Modified: 30 Oct 2017
;; Version: 1.67
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-ssh-deploy

;; This file is not part of GNU Emacs.

;; Copyright (C) 2017 Christian Johansson

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

;; ssh-deploy enables automatic deploys on explicit-save, manual uploads, renaming,
;; deleting, downloads, file differences, remote terminals, detection of remote changes and remote directory browsing via TRAMP.
;;
;; For asynchrous operations it uses async.el,
;;
;; By setting the variables (globally, per directory or per file):
;; ssh-deploy-root-local,ssh-deploy-root-remote, ssh-deploy-on-explicit-save
;; you can setup a directory for SSH or FTP deployment.
;;
;; For asynchronous transfers you need to setup ~/.netrc or key-based authorization or equivalent for automatic authentication.
;;
;; Example contents of ~/.netrc for FTP:
;; machine myserver.com login myuser port ftp password mypassword
;;
;; Set permissions to this file to 700 with you as the owner.
;;
;; - To setup a upload hook on save do this:
;;     (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))
;;
;; - To setup automatic storing of base revisions and detection of remote changes do this:
;;     (add-hook 'find-file-hook (lambda() (if ssh-deploy-automatically-detect-remote-changes (ssh-deploy-remote-changes-handler)) ))
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
;;
;; Here is an example for SSH deployment, /Users/Chris/Web/Site1/.dir.locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
;;   (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/site1/")
;;   (ssh-deploy-on-explicity-save . t)
;; )))
;;
;; Here is an example for FTP deployment, /Users/Chris/Web/Site2/.dir.locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
;;   (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/var/www/site2/")
;;   (ssh-deploy-on-explicit-save . nil)
;; )))
;;
;; Now when you are in a directory which is deployed via SSH or FTP you can access these features.
;;
;; Here is a list of other variables you can set globally or per directory:

;; * ssh-deploy-root-local - The local root that should be under deployment *(string)*
;; * ssh-deploy-root-remote - The remote root that should be under deployment, should follow a `/protocol:user@host:path` format *(string)*
;; * ssh-deploy-debug - Enables debugging messages *(boolean)*
;; * ssh-deploy-revision-folder - The folder used for storing local revisions *(string)*
;; * ssh-deploy-automatically-detect-remote-changes - Enables automatic detection of remote changes *(boolean)*
;; * ssh-deploy-on-explicit-save - Enabled automatic uploads on save *(boolean)*
;; * ssh-deploy-exclude-list - A list defining what paths to exclude from deployment *(list)*
;; * ssh-deploy-async - Enables asynchronous transfers (you need to have `async.el` installed as well) *(boolean)*
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(defgroup ssh-deploy nil
  "Upload, download, difference, browse and terminal handler for files and directories on remote hosts via SSH and FTP."
  :group 'tools
  :group 'convenience)

(defcustom ssh-deploy-root-local nil
  "String variable of local root, nil by default."
  :type 'string
  :group 'ssh-deploy)

(defcustom ssh-deploy-root-remote nil
  "String variable of remote root, nil by default."
  :type 'string
  :group 'ssh-deploy)

(defcustom ssh-deploy-on-explicit-save t
  "Boolean variable if deploy should be made on explicit save, t by default."
  :type 'boolean
  :group 'ssh-deploy)

(defcustom ssh-deploy-debug nil
  "Boolean variable if debug messages should be shown, nil by default."
  :type 'boolean
  :group 'ssh-deploy)

(defcustom ssh-deploy-async t
  "Boolean variable if asynchrous method for transfers should be used, t by default."
  :type 'boolean
  :group 'ssh-deploy)

(defcustom ssh-deploy-revision-folder "~/.ssh-deploy-revisions/"
  "String variable with path to revisions with trailing slash."
  :type 'string
  :group 'ssh-deploy)

(defcustom ssh-deploy-automatically-detect-remote-changes t
  "Detect remote changes and store base revisions automatically, t by default."
  :type 'boolean
  :group 'ssh-deploy)

(defcustom ssh-deploy-exclude-list '("/.git/" ".dir-locals.el")
  "List of strings that if found in paths will exclude paths from sync, '(\"/.git\"/' \".dir-locals.el\") by default."
  :type 'list
  :group 'ssh-deploy)


;; PRIVATE FUNCTIONS
;;
;; these functions are only used internally and should be of no value to outside public and handler functions.
;; these functions MUST not use module variables.


(defun ssh-deploy--get-revision-path (path root)
  "Generate revision-path for PATH in ROOT."
  (if (not (file-exists-p root))
      (make-directory root))
  (concat root (replace-regexp-in-string "\\(/\\|@\\|:\\)" "_" path)))

(defun ssh-deploy--file-is-in-path (file path)
  "Return true if FILE is in the path PATH."
  (not (null (string-match path file))))

(defun ssh-deploy--file-is-included (path exclude-list)
  "Return true if PATH is not in EXCLUDE-LIST."
  (let ((not-found t))
    (dolist (element exclude-list)
      (if (and (not (null element))
               (not (null (string-match element path))))
          (progn
            (setq not-found nil))))
    not-found))

(defun ssh-deploy--get-relative-path (root path)
  "Return a string for the relative path based on ROOT and PATH."
  (replace-regexp-in-string root "" path))

(defun ssh-deploy--is-not-empty-string (string)
  "Return true if the STRING is not empty and not nil.  Expects string."
  (and (not (null string))
       (not (zerop (length string)))))

(defun ssh-deploy--upload-via-tramp-async (path-local path-remote force revision-folder)
  "Upload PATH-LOCAL to PATH-REMOTE via TRAMP asynchronously and FORCE upload despite remote change, check for revisions in REVISION-FOLDER."
  (if (fboundp 'async-start)
      (progn
        (let ((file-or-directory (file-regular-p path-local)))
          (if file-or-directory
              (progn
                (let ((revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
                  (message "Uploading file '%s' to '%s' via TRAMP asynchronously.." path-local path-remote)
                  (async-start
                   `(lambda()
                      (require 'ediff)
                      (if (fboundp 'ediff-same-file-contents)
                          (progn
                            (if (or (eq t ,force) (not (file-exists-p ,path-remote)) (and (file-exists-p ,revision-path) (ediff-same-file-contents ,revision-path ,path-remote)))
                                (progn
                                  (if (not (file-directory-p (file-name-directory ,path-remote)))
                                      (make-directory (file-name-directory ,path-remote) t))
                                  (copy-file ,path-local ,path-remote t t t t)
                                  (copy-file ,path-local ,revision-path t t t t)
                                  (list 0 (format "Upload '%s' completed." ,path-remote)))
                              (list 1 (format "Remote file '%s' has changed, please download or diff." ,path-remote))))
                        (list 1 "Function ediff-same-file-contents is missing.")))
                   (lambda(return)
                     (if (= (nth 0 return) 0)
                         (message (nth 1 return))
                       (display-warning "ssh-deploy" (nth 1 return) :warning))))))
            (progn
              (message "Uploading directory '%s' to '%s' via TRAMP asynchronously.." path-local path-remote)
              (async-start
               `(lambda()
                  (copy-directory ,path-local ,path-remote t t t)
                  ,path-local)
               (lambda(return-path)
                 (message "Upload '%s' finished." return-path)))))))
    (message "async.el is not installed")))

;; TODO Fix "bug" where this does not detect remote changes
(defun ssh-deploy--upload-via-tramp (path-local path-remote force revision-folder)
  "Upload PATH-LOCAL to PATH-REMOTE via TRAMP synchronously and FORCE despite remote change compared with copy in REVISION-FOLDER."
  (let ((file-or-directory (file-regular-p path-local)))
    (if file-or-directory
        (progn
          (if (or (boundp 'force) (not (ssh-deploy--remote-has-changed path-local path-remote revision-folder)))
              (progn
                (message "Uploading file '%s' to '%s' via TRAMP synchronously.." path-local path-remote)
                (if (not (file-directory-p (file-name-directory path-remote)))
                    (make-directory (file-name-directory path-remote) t))
                (copy-file path-local path-remote t t t t)
                (message "Upload '%s' finished" path-local)
                (ssh-deploy-store-revision path-local revision-folder))
            (display-warning "ssh-deploy" "Remote contents has changed or no base revision exists, please download or diff." :warning)))
      (progn
        (message "Uploading directory '%s' to '%s' via TRAMP synchronously.." path-local path-remote)
        (copy-directory path-local path-remote t t t)
        (message "Upload '%s' finished" path-local)))))

(defun ssh-deploy--download-via-tramp-async (path-remote path-local revision-folder)
  "Download PATH-REMOTE to PATH-LOCAL via TRAMP asynchronously and make a copy in REVISION-FOLDER."
  (if (fboundp 'async-start)
      (progn
        (let ((file-or-directory (file-regular-p path-local)))
          (if file-or-directory
              (progn
                (let ((revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
                  (message "Downloading file '%s' to '%s' via TRAMP asynchronously.." path-remote path-local)
                  (async-start
                   `(lambda()
                      (copy-file ,path-remote ,path-local t t t t)
                      (copy-file ,path-local ,revision-path t t t t)
                      ,path-local)
                   (lambda(return-path)
                     (message "Download '%s' finished." return-path)))))
            (progn
              (message "Downloading directory '%s' to '%s' via TRAMP synchronously.." path-remote path-local)
              (async-start
               `(lambda()
                  (copy-directory ,path-remote ,path-local t t t)
                  ,path-local)
               (lambda(return-path)
                 (message "Download '%s' finished." return-path)))))))
    (message "async.el is not installed")))

(defun ssh-deploy--download-via-tramp (path-remote path-local revision-folder)
  "Download PATH-REMOTE to PATH-LOCAL via TRAMP synchronously and store a copy in REVISION-FOLDER."
  (let ((file-or-directory (file-regular-p path-local)))
    (if file-or-directory
        (progn
          (message "Downloading file '%s' to '%s' via TRAMP synchronously.." path-remote path-local)
          (copy-file path-remote path-local t t t t)
          (message "Download '%s' finished." path-local)
          (ssh-deploy-store-revision path-local revision-folder))
      (progn
        (message "Downloading directory '%s' to '%s' via TRAMP synchronously.." path-remote path-local)
        (copy-directory path-remote path-local t t t)
        (message "Download '%s' finished." path-local)))))

(defun ssh-deploy--remote-has-changed (local remote revision-folder)
  "Synchronously check if last stored revision of LOCAL exists or has changed on REMOTE synchronously, stored in REVISION-FOLDER."
  (let ((revision-path (ssh-deploy--get-revision-path local revision-folder)))
    (if (file-exists-p remote)
        (progn
          (if (file-exists-p revision-path)
              (progn
                (require 'ediff)
                (if (fboundp 'ediff-same-file-contents)
                    (progn
                      (if (not (ediff-same-file-contents revision-path remote))
                          t
                        nil))
                  (progn
                    (message "Function ediff-same-file-contents is missing.")
                    nil)))
            t))
      nil)))


;; PUBLIC functions
;;
;; handlers use these to do things and people SHOULD be able to use these as they please themselves
;; these functions MUST only use module variables as fall-backs for missing arguments.


;;;### autoload
(defun ssh-deploy-remote-changes (path &optional root-local root-remote async revision-folder exclude-list)
  "Check if a local revision for PATH on ROOT-LOCAL and if remote file has changed on ROOT-REMOTE, do it optionally asynchronously if ASYNC is true, check for copies in REVISION-FOLDER and skip if path is in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path path root-local)
             (ssh-deploy--file-is-included path exclude-list))
        (let ((revision-folder (or revision-folder ssh-deploy-revision-folder))
              (exclude-list (or exclude-list ssh-deploy-exclude-list)))
          (progn
            (let ((revision-path (ssh-deploy--get-revision-path path revision-folder))
                  (path-remote (concat root-remote (ssh-deploy--get-relative-path root-local path))))
              (if (file-regular-p path)
                  (progn
                    (if (file-exists-p revision-path)
                        (progn
                          (if (and async (fboundp 'async-start))
                              (progn
                                (async-start
                                 `(lambda()
                                    (if (file-exists-p ,path-remote)
                                        (progn
                                          (require 'ediff)
                                          (if (fboundp 'ediff-same-file-contents)
                                              (progn
                                                (if (ediff-same-file-contents ,revision-path ,path-remote)
                                                    (list 0 (format "Remote file '%s' has not changed." ,path-remote))
                                                  (progn
                                                    (if (ediff-same-file-contents ,path ,path-remote)
                                                        (progn
                                                          (copy-file ,path ,revision-path t t t t)
                                                          (list 0 (format "Remote file '%s' is identical to local file '%s' but different to local revision. Updated local revision." ,path-remote ,path)))
                                                      (list 1 (format "Remote file '%s' has changed, please download or diff." ,path-remote))))))
                                            (list 1 "Function ediff-same-file-contents is missing.")))
                                      (list 0 (format "Remote file '%s' doesn't exist." ,path-remote))))
                                 (lambda(return)
                                   (if (= (nth 0 return) 0)
                                       (message (nth 1 return))
                                     (display-warning "ssh-deploy" (nth 1 return) :warning)))))
                            (progn
                              (if (file-exists-p path-remote)
                                  (progn
                                    (require 'ediff)
                                    (if (fboundp 'ediff-same-file-contents)
                                        (progn
                                          (if (ediff-same-file-contents revision-path path-remote)
                                              (message "Remote file '%s' has not changed." path-remote)
                                            (display-warning "ssh-deploy" (format "Remote file '%s' has changed, please download or diff." path-remote) :warning)))
                                      (display-warning "ssh-deploy" "Function ediff-same-file-contents is missing." :warning)))
                                (message "Remote file '%s' doesn't exist." path-remote)))))
                      (progn
                        (if (and async (fboundp 'async-start))
                            (progn
                              (async-start
                               `(lambda()
                                  (if (file-exists-p ,path-remote)
                                      (progn
                                        (require 'ediff)
                                        (if (fboundp 'ediff-same-file-contents)
                                            (progn
                                              (if (ediff-same-file-contents ,path ,path-remote)
                                                  (progn
                                                    (copy-file ,path ,revision-path t t t t)
                                                    (list 0 (format "Remote file '%s' has not changed, created base revision." ,path-remote)))
                                                (list 1 (format "Remote file '%s' has changed, please download or diff." ,path-remote))))
                                          (list 1 "Function ediff-file-same-contents is missing")))
                                    (list 0 (format "Remote file '%s' doesn't exist." ,path-remote))))
                               (lambda(return)
                                 (if (= (nth 0 return) 0)
                                     (message (nth 1 return))
                                   (display-warning "ssh-deploy" (nth 1 return) :warning)))))
                          (progn
                            (if (file-exists-p path-remote)
                                (progn
                                  (require 'ediff)
                                  (if (fboundp 'ediff-same-file-contents)
                                      (progn
                                        (if (ediff-same-file-contents path path-remote)
                                            (progn
                                              (copy-file path revision-path t t t t)
                                              (message "Remote file '%s' has not changed, created base revision." path-remote))
                                          (display-warning "ssh-deploy" (format "Remote file '%s' has changed, please download or diff." path-remote) :warning)))
                                    (display-warning "ssh-deploy" "Function ediff-same-file-contents is missing." :warning)))
                              (message "Remote file '%s' doesn't exist." path-remote))))))))))))))

;;;### autoload
(defun ssh-deploy-delete (path-local &optional root-local root-remote async debug exclude-list)
  "Delete PATH-LOCAL relative to ROOT-LOCAL as well as on ROOT-REMOTE, do it asynchronously if ASYNC is non-nil, debug if DEBUG is non-nil, check if path is excluded in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote)))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (let ((exclude-list (or exclude-list ssh-deploy-exclude-list))
              (file-or-directory (file-regular-p path-local))
              (path-remote (concat root-remote (ssh-deploy--get-relative-path root-local path-local))))
          (if file-or-directory
              (progn
                (delete-file path-local t)
                (message "Deleted file '%s'" path-local))
            (progn
              (delete-directory path-local t t)
              (message "Deleted directory '%s'" path-local)))
          (kill-this-buffer)
          (if (and async (fboundp 'async-start))
              (progn
                (async-start
                 `(lambda()
                    (if ,file-or-directory
                        (delete-file ,path-remote t)
                      (delete-directory ,path-remote t t))
                    (list ,path-remote))
                 (lambda(files)
                   (message "Asynchronously deleted '%s'." (nth 0 files)))))
            (progn
              (if file-or-directory
                  (delete-file path-remote t)
                (delete-directory path-remote t t))
              (message "Synchronously deleted '%s'." path-remote))))
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
        (let ((exclude-list (or exclude-list ssh-deploy-exclude-list)))
          (progn
            (let ((file-or-directory (file-regular-p old-path-local)))
              (let ((old-path-remote (concat root-remote (ssh-deploy--get-relative-path root-local old-path-local)))
                    (new-path-remote (concat root-remote (ssh-deploy--get-relative-path root-local new-path-local))))
                (rename-file old-path-local new-path-local t)
                (if (file-regular-p new-path-local)
                    (progn
                      (rename-buffer new-path-local)
                      (set-buffer-modified-p nil)
                      (set-visited-file-name new-path-local))
                  (dired new-path-local))
                (message "Renamed '%s' to '%s'." old-path-local new-path-local)
                (if (and async (fboundp 'async-start))
                    (progn
                      (async-start
                       `(lambda()
                          (rename-file ,old-path-remote ,new-path-remote t)
                          (list ,old-path-remote ,new-path-remote))
                       (lambda(files)
                         (message "Asynchronously renamed '%s' to '%s'." (nth 0 files) (nth 1 files)))))
                  (progn
                    (rename-file old-path-remote new-path-remote t)
                    (message "Synchronously renamed '%s' to '%s'." old-path-remote new-path-remote)))))))
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
(defun ssh-deploy-diff (path-local path-remote &optional root-local debug exclude-list)
  "Find differences between PATH-LOCAL and PATH-REMOTE, where PATH-LOCAL is inside ROOT-LOCAL.  DEBUG enables feedback message, check if PATH-LOCAL is not in EXCLUDE-LIST."
  (let ((file-or-directory (file-regular-p path-local))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))
    (if (not (boundp 'root-local))
        (setq root-local ssh-deploy-root-local))
    (if (not (boundp 'debug))
        (setq debug ssh-deploy-debug))
    (if (and (ssh-deploy--file-is-in-path path-local root-local)
             (ssh-deploy--file-is-included path-local exclude-list))
        (progn
          (if file-or-directory
              (progn
                (require 'ediff)
                (if (fboundp 'ediff-same-file-contents)
                    (progn
                      (message "Comparing file '%s' to '%s'.." path-local path-remote)
                      (if (ediff-same-file-contents path-local path-remote)
                          (message "Files have identical contents.")
                        (ediff path-local path-remote)))
                  (message "Function ediff-same-file-contents is missing.")))
            (progn
              (message "Unfortunately directory differences are not yet implemented."))))
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
  "Check if local revision exists or remote file has changed if path is configured for deployment"
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name))
          (ssh-deploy-remote-changes (file-truename buffer-file-name) (file-truename ssh-deploy-root-local) ssh-deploy-root-remote ssh-deploy-async ssh-deploy-revision-folder ssh-deploy-exclude-list))))

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
            (ssh-deploy-diff path-local path-remote root-local ssh-deploy-debug ssh-deploy-exclude-list))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let* ((path-local (file-truename default-directory))
                   (root-local (file-truename ssh-deploy-root-local))
                   (path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
              (ssh-deploy-diff path-local path-remote root-local ssh-deploy-debug ssh-deploy-exclude-list))))))

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
                (ssh-deploy-delete path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug)))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let* ((path-local (file-truename default-directory))
                   (root-local (file-truename ssh-deploy-root-local))
                   (yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the directory '%s': " path-local))))
              (if (string= yes-no-prompt "yes")
                  (ssh-deploy-delete path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list)))))))

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
        (ssh-deploy-remote-terminal-eshell root-local ssh-deploy-root-remote root-local ssh-deploy-exclude-list))))

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

