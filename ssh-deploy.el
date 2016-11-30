;;; `ssh-deploy.el --- Deployment via SSH or FTP, global or per directory.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 5 Jul 2016
;; Modified: 30 Nov 2016
;; Version: 1.43
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-ssh-deploy

;; This file is not part of GNU Emacs.

;; Copyright (C) 2016 Christian Johansson

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

;; `ssh-deploy' enables automatic deploys on explicit-save, manual uploads,
;; downloads, differences, remote terminals, detection of remote changes and remote directory browsing via TRAMP.
;; To do this it progressively uses `tramp', `tramp-term', `ediff', `async` and `ztree'.
;; By setting the variables (globally or per directory):
;; `ssh-deploy-root-local',`ssh-deploy-root-remote', `ssh-deploy-on-explicit-save'
;; you can setup a directory for SSH or FTP deployment.
;;
;; For asynchronous transfers you need to setup `~/.netrc' or equivalent for automatic authentication.
;;
;; Example contents of `~/.netrc':
;; machine myserver.com login myuser port ftp password mypassword
;;
;; Set permissions to this file to 700 with you as the owner.
;;
;; - To setup a upload hook on save do this:
;;     (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))
;;
;; - To setup automatic storing of base revisions and download of external changes do this:
;;     (add-hook 'find-file-hook (lambda() (if ssh-deploy-automatically-detect-remote-changes (ssh-deploy-remote-changes-handler)) ))
;;
;; - To set key-bindings do something like this:
;;     (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
;;     (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
;;     (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
;;     (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
;;     (global-set-key (kbd "C-c C-z r") (lambda() (interactive)(ssh-deploy-remote-changes-handler) ))
;;     (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))
;;
;; An illustrative example for `SSH' deployment, /Users/Chris/Web/Site1/.dir.locals.el
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
;;   (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/site1/")
;;   (ssh-deploy-on-explicity-save . t)
;; )))
;;
;; An example for `FTP' deployment, /Users/Chris/Web/Site2/.dir.locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
;;   (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/var/www/site2/")
;;   (ssh-deploy-on-explicit-save . nil)
;; )))
;;
;; Now when you are in a directory which is deployed via SSH or FTP you can access these features.
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

(defun ssh-deploy--get-revision-path (path)
  "Generate revision-path for PATH."
  (if (not (file-exists-p ssh-deploy-revision-folder))
      (make-directory ssh-deploy-revision-folder))
  (concat ssh-deploy-revision-folder (replace-regexp-in-string "\\(/\\|@\\|:\\)" "_" path)))

(defun ssh-deploy--file-is-in-path (file path)
  "Return true if FILE is in the path PATH."
  (not (null (string-match path file))))

(defun ssh-deploy--get-relative-path (root path)
  "Return a string for the relative path based on ROOT and PATH."
  (replace-regexp-in-string root "" path))

(defun ssh-deploy--parse-remote (string)
  "Return alist with connection attributes parsed from STRING."
  (let ((remote string))
    (let ((split (split-string remote "@")))
      (let ((left (nth 0 split))
            (right (nth 1 split)))
        (let ((server-path (split-string right ":")))
          (let ((server (nth 0 server-path))
                (path (nth 1 server-path)))
            (let ((protocol-user-password (split-string left ":")))
              (if (not (null (string-match "/" (nth 0 protocol-user-password))))
                  (let ((protocol (replace-regexp-in-string "/" "" (nth 0 protocol-user-password)))
                        (username (nth 1 protocol-user-password))
                        (password (nth 2 protocol-user-password)))
                    (let ((connection `((protocol . ,protocol) (username . ,username) (password . ,password) (server . ,server) (path . ,path) (string . ,remote))))
                      connection))
                (let ((username (nth 0 protocol-user-password))
                      (password (nth 1 protocol-user-password)))
                  (let ((connection `((protocol . "ssh") (username . ,username) (password . ,password) (server . ,server) (path . ,path) (string . ,remote))))
                    connection))))))))))

(defun ssh-deploy--is-not-empty-string (string)
  "Return true if the STRING is not empty and not nil.  Expects string."
  (and (not (null string))
       (not (zerop (length string)))))

(defun ssh-deploy--download (remote local local-root async)
  "Download REMOTE to LOCAL with the LOCAL-ROOT via tramp, ASYNC determines if transfer should be asynchrous or not."
  (if (and async (fboundp 'async-start))
      (ssh-deploy--download-via-tramp-async remote local local-root)
    (ssh-deploy--download-via-tramp remote local local-root)))

(defun ssh-deploy--upload-via-tramp-async (local remote local-root)
  "Upload LOCAL path to REMOTE and LOCAL-ROOT via tramp asynchrously."
  (if (fboundp 'async-start)
      (progn
        (let ((remote-path (concat "/" (alist-get 'protocol remote) ":" (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote))))
              (file-or-directory (file-regular-p local)))
          (if file-or-directory
              (progn
                (let ((revision-path (ssh-deploy--get-revision-path local)))
                  (message "Uploading file '%s' to '%s' via tramp asynchronously.." local remote-path)
                  (async-start
                   `(lambda()
                      (require 'ediff)
                      (if (fboundp 'ediff-same-contents)
                          (progn
                            (if (or (not (file-exists-p ,remote-path)) (and (file-exists-p ,revision-path) (ediff-same-contents ,revision-path ,remote-path)))
                                (progn
                                  (message "Uploading file '%s' to '%s' via tramp asynchronously.." ,local ,remote-path)
                                  (copy-file ,local ,remote-path t t t t)
                                  (copy-file ,local ,revision-path t t t t)
                                  0)
                              1))
                        2))
                   (lambda(return-message)
                     (if (= return-message 0) (message "Upload completed.")
                       (if (= return-message 1) (display-warning "ssh-deploy" "External file has changed, please download or diff." :warning)
                         (if (= return-message 2) (display-warning "ssh-deploy" "Function ediff-same-contents is missing" :warning))))))))
            (progn
              (message "Uploading directory '%s' to '%s' via tramp asynchronously.." local remote-path)
              (if (string= remote-path (alist-get 'string remote))
                  (progn
                    (async-start
                     `(lambda()
                        (copy-directory ,local ,remote-path t t t)
                        ,local)
                     (lambda(return-path)
                       (message "Upload '%s' finished" return-path))))
                (progn
                  (async-start
                   `(lambda()
                      (copy-directory ,local ,(file-name-directory (directory-file-name remote-path)) t t)
                      ,local)
                   (lambda(return-path)
                     (message "Upload '%s' finished" return-path)))))))))
    (message "async.el is not installed")))

(defun ssh-deploy--upload-via-tramp (local remote local-root)
  "Upload LOCAL path to REMOTE and LOCAL-ROOT via tramp synchrously."
  (let ((remote-path (concat "/" (alist-get 'protocol remote) ":" (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote))))
        (file-or-directory (file-regular-p local)))
    (if file-or-directory
        (progn
          (if (not (ssh-deploy--remote-has-changed local remote-path))
              (progn
                (message "Uploading file '%s' to '%s' via tramp synchronously.." local remote-path)
                (copy-file local remote-path t t t t)
                (message "Upload '%s' finished" local)
                (ssh-deploy-store-revision local))
            (display-warning "ssh-deploy" "Remote contents has changed or no base revision exists, please download or diff." :warning)))
      (progn
        (message "Uploading directory '%s' to '%s' via tramp synchronously.." local remote-path)
        (if (string= remote-path (alist-get 'string remote))
            (progn
              (copy-directory local remote-path t t t)
              (message "Upload '%s' finished" local))
          (progn
            (copy-directory local (file-name-directory (directory-file-name remote-path)) t t)
            (message "Upload '%s' finished" local)))))))

(defun ssh-deploy--download-via-tramp-async (remote local local-root)
  "Download REMOTE path to LOCAL and LOCAL-ROOT via tramp asynchronously."
  (if (fboundp 'async-start)
      (progn
        (let ((remote-path (concat "/" (alist-get 'protocol remote) ":" (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote))))
              (file-or-directory (file-regular-p local)))
          (if file-or-directory
              (progn
                (message "Downloading file '%s' to '%s' via tramp asynchronously.." remote-path local)
                (async-start
                 `(lambda()
                    (copy-file ,remote-path ,local t t t t)
                    ,local)
                 (lambda(return-path)
                   (message "Download '%s' finished" return-path)
                   (ssh-deploy-store-revision return-path))))
            (progn
              (message "Downloading directory '%s' to '%s' via tramp asynchronously.." remote-path local)
              (if (string= remote-path (alist-get 'string remote))
                  (progn
                    (async-start
                     `(lambda()
                        (copy-directory ,remote-path ,local t t t)
                        ,local)
                     (lambda(return-path)
                       (message "Download '%s' finished" return-path))))
                (progn
                  (async-start
                   `(lambda()
                      (copy-directory ,remote-path ,(file-name-directory (directory-file-name remote-path)) t t)
                      ,local)
                   (lambda(return-path)
                     (message "Download '%s' finished" return-path)))))))))
    (message "async.el is not installed")))

(defun ssh-deploy--download-via-tramp (remote local local-root)
  "Download REMOTE path to LOCAL and LOCAL-ROOT via tramp synchronously."
  (let ((remote-path (concat "/" (alist-get 'protocol remote) ":" (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote))))
        (file-or-directory (file-regular-p local)))
    (if file-or-directory
        (progn
          (message "Downloading file '%s' to '%s' via tramp synchronously.." remote-path local)
          (copy-file remote-path local t t t t)
          (message "Download '%s' finished" local)
          (ssh-deploy-store-revision local))
      (progn
        (message "Downloading directory '%s' to '%s' via tramp synchronously.." remote-path local)
        (if (string= remote-path (alist-get 'string remote))
            (progn
              (copy-directory remote-path local t t t)
              (message "Download '%s' finished" local))
          (progn
            (copy-directory remote-path (file-name-directory (directory-file-name remote-path)) t t)
            (message "Download '%s' finished" local))
          )))))

(defun ssh-deploy--upload (local remote local-root async)
  "Upload LOCAL to REMOTE and LOCAL-ROOT via tramp, ASYNC determines if transfer should be asynchronously or not."
  (if (and async (fboundp 'async-start))
      (ssh-deploy--upload-via-tramp-async local remote local-root)
    (ssh-deploy--upload-via-tramp local remote local-root)))

(defun ssh-deploy--remote-has-changed (local remote)
  "Check if last stored revision of LOCAL exists or has changed on REMOTE synchronously."
  (let ((revision-path (ssh-deploy--get-revision-path local)))
    (if (file-exists-p remote)
        (progn
          (if (file-exists-p revision-path)
              (progn
                (require 'ediff)
                (if (fboundp 'ediff-same-contents)
                    (progn
                      (if (not (ediff-same-contents revision-path remote))
                          t
                        nil))
                  (progn
                    (message "Function ediff-same-contents is missing.")
                    nil)))
            t))
      nil)))

;;;### autoload
(defun ssh-deploy (local-root remote-root upload-or-download path debug async)
  "Upload/Download file or directory relative to the roots LOCAL-ROOT with REMOTE-ROOT via ssh or ftp according to UPLOAD-OR-DOWNLOAD and the path PATH, DEBUG enables some feedback messages and ASYNC determines if transfers should be asynchrous or not."
  (if (ssh-deploy--file-is-in-path path local-root)
      (progn
        (let ((file-or-directory (file-regular-p path)))
          (let ((remote-path (concat remote-root (ssh-deploy--get-relative-path local-root path))))
            (let ((connection (ssh-deploy--parse-remote remote-path)))
              (if (not (null upload-or-download))
                  (ssh-deploy--upload path connection local-root async)
                (ssh-deploy--download connection path local-root async))))))
    (if debug
        (message "Path '%s' is not in the root '%s'" path local-root))))

;;;### autoload
(defun ssh-deploy-upload-handler ()
  "Upload current path to remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
               (file-exists-p buffer-file-name))
          (let ((local-path (file-truename buffer-file-name))
                (local-root (file-truename ssh-deploy-root-local)))
            (ssh-deploy local-root ssh-deploy-root-remote t local-path ssh-deploy-debug ssh-deploy-async))
        (if (ssh-deploy--is-not-empty-string default-directory)
            (let ((local-path (file-truename default-directory))
                  (local-root (file-truename ssh-deploy-root-local)))
              (ssh-deploy local-root ssh-deploy-root-remote t local-path ssh-deploy-debug ssh-deploy-async))))))

;;;### autoload
(defun ssh-deploy-remote-changes-handler()
  "Check if local revision exists or remote file has changed if path is configured for deployment"
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name))
          (let ((local-root (file-truename ssh-deploy-root-local))
                (remote-root (file-truename ssh-deploy-root-remote))
                (path (file-truename buffer-file-name)))
            (if (ssh-deploy--file-is-in-path path local-root)
                (progn
                  (let ((revision-path (ssh-deploy--get-revision-path path))
                        (remote-path (concat remote-root (ssh-deploy--get-relative-path local-root path))))
                    (if (file-regular-p path)
                        (progn
                          (if (file-exists-p revision-path)
                              (progn
                                (if (fboundp 'async-start)
                                    (progn
                                      (async-start
                                       `(lambda()
                                          (require 'ediff)
                                          (if (fboundp 'ediff-same-contents)
                                              (progn
                                                (if (ediff-same-contents ,revision-path ,remote-path)
                                                    0
                                                  1))
                                            2))
                                       (lambda(return-message)
                                         (if (= return-message 0) (message "Remote file has not changed.")
                                           (if (= return-message 1) (display-warning "ssh-deploy" "External file has changed, please download or diff." :warning)
                                             (if (= return-message 2) (display-warning "ssh-deploy" "Function ediff-same-contents is missing" :warning)))))))
                                  (progn
                                    (require 'ediff)
                                    (if (fboundp 'ediff-same-contents)
                                        (progn
                                          (if (ediff-same-contents revision-path remote-path)
                                              (message "Remote file has not changed.")
                                            (display-warning "ssh-deploy" "External file has changed, please download or diff." :warning)))
                                      (display-warning "ssh-deploy" "Function ediff-same-contents is missing" :warning)))))
                            (progn
                              (if (fboundp 'async-start)
                                  (progn
                                    (async-start
                                     `(lambda()
                                        (if (file-exists-p ,remote-path)
                                            (progn
                                              (require 'ediff)
                                              (if (fboundp 'ediff-same-contents)
                                                  (progn
                                                    (if (ediff-same-contents ,path ,remote-path)
                                                        (progn
                                                          (copy-file ,path ,revision-path t t t t)
                                                          (message "Remote file has not changed, created base revision."))
                                                      (display-warning "ssh-deploy" "External file has changed, please download or diff." :warning)))
                                                (display-warning "ssh-deploy" "Function ediff-same-contents is missing" :warning)))
                                          (progn
                                            (message "Remote file doesn't exist"))))
                                     (lambda(return-message)
                                       (message return-message))))
                                (progn
                                  (if (file-exists-p remote-path)
                                      (progn
                                        (require 'ediff)
                                        (if (fboundp 'ediff-same-contents)
                                            (progn
                                              (if (ediff-same-contents path remote-path)
                                                  (progn
                                                    (copy-file path revision-path t t t t)
                                                    (message "Remote file has not changed, created base revision."))
                                                (display-warning "ssh-deploy" "External file has changed, please download or diff." :warning)))
                                          (display-warning "ssh-deploy" "Function ediff-same-contents is missing" :warning)))
                                    (progn
                                      (message "Remote file doesn't exist"))))))))))))))))

;;;### autoload
(defun ssh-deploy-download-handler ()
  "Download current path from remote host if it is configured for deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
               (file-exists-p buffer-file-name))
          (let ((local-path (file-truename buffer-file-name))
                (local-root (file-truename ssh-deploy-root-local)))
            (ssh-deploy local-root ssh-deploy-root-remote nil local-path ssh-deploy-debug ssh-deploy-async))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let ((local-path (file-truename default-directory))
                  (local-root (file-truename ssh-deploy-root-local)))
              (ssh-deploy local-root ssh-deploy-root-remote nil local-path ssh-deploy-debug ssh-deploy-async))))))

;;;### autoload
(defun ssh-deploy-diff-handler ()
  "Compare current path with remote host if it is configured for deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local) (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (and (ssh-deploy--is-not-empty-string buffer-file-name)
               (file-exists-p buffer-file-name))
          (let ((local-path (file-truename buffer-file-name))
                (local-root (file-truename ssh-deploy-root-local)))
            (ssh-deploy-diff local-root ssh-deploy-root-remote local-path ssh-deploy-debug))
        (if (and (ssh-deploy--is-not-empty-string default-directory)
                 (file-exists-p default-directory))
            (let ((local-path (file-truename default-directory))
                  (local-root (file-truename ssh-deploy-root-local)))
              (ssh-deploy-diff local-root ssh-deploy-root-remote local-path ssh-deploy-debug))))))

;;;### autoload
(defun ssh-deploy-remote-terminal-handler ()
  "Open remote host in tramp terminal it is configured for deployment."
  (if (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
      (ssh-deploy-remote-terminal ssh-deploy-root-remote)))

;;;### autoload
(defun ssh-deploy-browse-remote-handler ()
  "Open current relative path on remote host in `dired-mode' if it is configured for deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
           (ssh-deploy--is-not-empty-string default-directory))
      (let ((local-path (file-truename default-directory))
            (local-root (file-truename ssh-deploy-root-local)))
        (ssh-deploy-browse-remote local-root ssh-deploy-root-remote local-path))))

;;;### autoload
(defun ssh-deploy-diff (local-root remote-root-string path &optional debug)
  "Find differences relative to the roots LOCAL-ROOT with REMOTE-ROOT-STRING and the path PATH, DEBUG enables feedback message."
  (let ((file-or-directory (file-regular-p path)))
    (if (ssh-deploy--file-is-in-path path local-root)
        (progn
          (let ((remote-path (concat remote-root-string (ssh-deploy--get-relative-path local-root path))))
            (let ((remote (ssh-deploy--parse-remote remote-path)))
              (let ((command (concat "/" (alist-get 'protocol remote) ":" (alist-get 'username remote) "@" (alist-get 'server remote) ":" (alist-get 'path remote))))
                (if file-or-directory
                    (progn
                      (require 'ediff)
                      (if (fboundp 'ediff-same-contents)
                          (progn
                            (message "Comparing file '%s' to '%s'.." path command)
                            (if (ediff-same-contents path command)
                                (message "Files have identical contents.")
                              (ediff path command)))
                        (message "Function ediff-same-contents is missing.")))
                  (progn
                    (if (fboundp 'ztree-diff)
                        (progn
                          (message "Comparing directory '%s' to '%s'.." path command)
                          (ztree-diff path command))
                      (message "ztree-diff is not installed."))))))))
      (if debug
          (message "Path '%s' is not in the root '%s'" path local-root)))))

;;;### autoload
(defun ssh-deploy-browse-remote (local-root remote-root-string path)
  "Browse relative to LOCAL-ROOT on REMOTE-ROOT-STRING the path PATH in `dired-mode`."
  (if (ssh-deploy--file-is-in-path path local-root)
      (let ((remote-path (concat remote-root-string (ssh-deploy--get-relative-path local-root path))))
        (let ((remote-root (ssh-deploy--parse-remote remote-path)))
          (let ((command (concat "/" (alist-get 'protocol remote-root) ":" (alist-get 'username remote-root) "@" (alist-get 'server remote-root) ":" (alist-get 'path remote-root))))
            (message "Opening '%s' for browsing on remote host.." command)
            (dired command))))))

;;;### autoload
(defun ssh-deploy-remote-terminal (remote-host-string)
  "Opens REMOTE-HOST-STRING in terminal."
  (let ((remote-root (ssh-deploy--parse-remote remote-host-string)))
    (if (string= (alist-get 'protocol remote-root) "ssh")
        (if (and (fboundp 'tramp-term)
                 (fboundp 'tramp-term--initialize)
                 (fboundp 'tramp-term--do-ssh-login))
            (progn
              (let ((hostname (concat (alist-get 'username remote-root) "@" (alist-get 'server remote-root))))
                (let ((host (split-string hostname "@")))
                  (message "Opening tramp-terminal for remote host '%s@%s' and '%s'.." (car host) (car (last host)) hostname)
                  (unless (eql (catch 'tramp-term--abort (tramp-term--do-ssh-login host)) 'tramp-term--abort)
                    (tramp-term--initialize hostname)
                    (run-hook-with-args 'tramp-term-after-initialized-hook hostname)
                    (message "tramp-term initialized")))))
          (message "tramp-term is not installed."))
      (message "Remote terminal is only available for ssh protocol"))))

;;;### autoload
(defun ssh-deploy-store-revision (path)
  "Store PATH in revision-folder."
  (let ((revision-path (ssh-deploy--get-revision-path path)))
    (message "Storing revision of '%s' at '%s'.." path revision-path)
    (copy-file path (ssh-deploy--get-revision-path path) t t t t)))

(provide 'ssh-deploy)
;;; ssh-deploy.el ends here
