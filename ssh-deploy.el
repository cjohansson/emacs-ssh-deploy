;;; ssh-deploy.el --- Deployment via SSH, global or per directory.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 5 Jul 2016
;; Modified: 1 Nov 2016
;; Version: 1.38
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

;; `ssh-deploy' enables automatic deploys on explicit-save, manual
;; uploads, downloads, differences, remote terminals and remote directory browsing
;; via key-pair password-less authorized SSH connections and password-based FTP connections.
;; To do this it uses `tramp',`tramp-term', `scp', `curl', `ediff' and `ztree'.
;; By setting the variables (globally or per directory):
;; `ssh-deploy-root-local',`ssh-deploy-root-remote',
;; `ssh-deploy-on-explicit-save' you can setup a directory for
;; SSH or FTP deployment.
;;
;; - To setup a hook on explicit save do this:
;;     (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicit-save (ssh-deploy-upload-handler)) ))
;;
;; - To set key-bindings do something like this:
;;     (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
;;     (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler) ))
;;     (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
;;     (global-set-key (kbd "C-c C-z t") (lambda() (interactive)(ssh-deploy-remote-terminal-handler) ))
;;     (global-set-key (kbd "C-c C-z b") (lambda() (interactive)(ssh-deploy-browse-remote-handler) ))
;;
;; An illustrative example for SSH, /Users/Chris/Web/Site1/.dir.locals.el
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
;;   (ssh-deploy-root-remote . "/ssh:web@myserver.com:/var/www/site1/")
;;   (ssh-deploy-on-explicity-save . t)
;; )))
;;
;; An example for FTP, /Users/Chris/Web/Site2/.dir.locals.el:
;; ((nil . (
;; (ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
;; (ssh-deploy-root-remote . "/ftp:myuser:mypassword@myserver.com:/site2/")
;; (ssh-deploy-on-explicit-save . nil)
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

(defcustom ssh-deploy-on-explicit-save nil
  "Boolean variable if deploy should be made on explicit save, nil by default."
  :type 'boolean
  :group 'ssh-deploy)

(defcustom ssh-deploy-debug nil
  "Boolean variable if debug messages should be shown, nil by default."
  :type 'boolean
  :group 'ssh-deploy)

(defun ssh-deploy--browse-remote (local-root remote-root-string path)
  "Browse relative to LOCAL-ROOT on REMOTE-ROOT-STRING the path PATH in `dired-mode`."
  (if (ssh-deploy--file-is-in-path path local-root)
      (let ((remote-path (concat remote-root-string (ssh-deploy--get-relative-path local-root path))))
        (let ((remote-root (ssh-deploy--parse-remote remote-path)))
          (let ((command (concat "/" (alist-get 'protocol remote-root) ":" (alist-get 'username remote-root) "@" (alist-get 'server remote-root) ":" (alist-get 'path remote-root))))
            (message "Opening '%s' for browsing on remote host.." command)
          (dired command))))))

(defun ssh-deploy--remote-terminal (remote-host-string)
  "Opens REMOTE-HOST-STRING in tramp terminal."
  (if (and (fboundp 'tramp-term)
           (fboundp 'tramp-term--initialize)
           (fboundp 'tramp-term--do-ssh-login))
      (progn
        (let ((remote-root (ssh-deploy--parse-remote remote-host-string)))
          (if (string= (alist-get 'protocol remote-root) "ssh")
              (progn
                (let ((hostname (concat (alist-get 'username remote-root) "@" (alist-get 'server remote-root))))
                  (let ((host (split-string hostname "@")))
                    (message "Opening tramp-terminal for remote host '%s@%s' and '%s'.." (car host) (car (last host)) hostname)
                    (unless (eql (catch 'tramp-term--abort (tramp-term--do-ssh-login host)) 'tramp-term--abort)
                      (tramp-term--initialize hostname)
                      (run-hook-with-args 'tramp-term-after-initialized-hook hostname)
                      (message "tramp-term initialized")))))
            (message "Terminal is only available for the SSH protocol."))))
    (message "tramp-term is not installed.")))

(defun ssh-deploy--file-is-in-path (file path)
  "Return true if FILE is in the path PATH."
  (not (null (string-match path file))))

(defun ssh-deploy--get-relative-path (root path)
  "Return a string for the relative path based on ROOT and PATH."
  (replace-regexp-in-string root "" path))

(defun ssh-deploy--diff (local-root remote-root-string path)
  "Find differences relative to the roots LOCAL-ROOT with REMOTE-ROOT-STRING via ssh and the path PATH."
  (let ((file-or-directory (file-regular-p path)))
    (if (ssh-deploy--file-is-in-path path local-root)
        (progn
          (let ((remote-path (concat remote-root-string (ssh-deploy--get-relative-path local-root path))))
            (let ((remote (ssh-deploy--parse-remote remote-path)))
              (let ((command (concat "/" (alist-get 'protocol remote) ":" (alist-get 'username remote) "@" (alist-get 'server remote) ":" (alist-get 'path remote))))
                (if file-or-directory
                    (progn
                      (message "Comparing file '%s' to '%s'.." path command)
                      (ediff path command))
                  (progn
                    (if (fboundp 'ztree-diff)
                        (progn
                          (message "Comparing directory '%s' to '%s'.." path command)
                          (ztree-diff path command))
                      (message "ztree-diff is not installed.")
                      )))))))
    (if ssh-deploy-debug
        (message "Path '%s' is not in the root '%s'" path local-root)))))

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

(defun ssh-deploy--run-shell-command (command)
  "Run COMMAND in asynchronous mode."
  (message "Shell command: '%s'" command)
  (let ((proc (start-process-shell-command "process" nil command)))
    (set-process-filter proc (lambda (proc output)(message "%s" (replace-regexp-in-string "\^M" "\n" output))))
    (set-process-sentinel proc (lambda (proc output)
                                 (if (string= (symbol-name (process-status proc)) "exit")
                                     (if (= (process-exit-status proc) 0)
                                         (message "Successfully ran shell command.")
                                       (message "Failed to run shell command.")))))))

(defun ssh-deploy--download (remote local local-root)
  "Download REMOTE to LOCAL with the LOCAL-ROOT via ssh or ftp."
  (if (or (string= (alist-get 'protocol remote) "ssh") (string= (alist-get 'protocol remote) "ftp"))
      (progn
        (let ((path (concat (alist-get 'server remote) ":" (alist-get 'path remote))))
          (message "Downloading path '%s' to '%s'.." path local)
          (let ((file-or-directory (file-regular-p local)))
            (if file-or-directory
                (if (string= (alist-get 'protocol remote) "ssh")
                    (ssh-deploy--download-file-via-ssh remote local)
                  (ssh-deploy--download-file-via-ftp remote local))
              (if (string= (alist-get 'protocol remote) "ssh")
                  (ssh-deploy--download-directory-via-ssh remote local local-root)
                (ssh-deploy--download-directory-via-ftp remote local local-root))))))
    (message "Unsupported protocol. Only SSH and FTP are supported at the moment.")))

;; TODO: Left for further research, is it possible to make this asynchrous?
(defun ssh-deploy--upload-via-tramp (local remote local-root)
  "Upload LOCAL path to REMOTE and LOCAL-ROOT via tramp."
  (let ((remote-path (concat "/" (alist-get 'protocol remote) ":" (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote))))
        (file-or-directory (file-regular-p local)))
    (if file-or-directory
        (progn
        (message "Uploading file '%s' to '%s'.." local remote-path)
        (copy-file local remote-path t t))
      (progn
        (message "Uploading directory '%s' to '%s' via TRAMP.." local remote-path)
        (copy-directory local remote-path t t)))))

;; TODO: Left for further research, is it possible to make this asynchrous?
(defun ssh-deploy--download-via-tramp (remote local local-root)
  "Download REMOTE path to LOCAL and LOCAL-ROOT via tramp."
  (let ((remote-path (concat "/" (alist-get 'protocol remote) ":" (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote))))
        (file-or-directory (file-regular-p local)))
    (if file-or-directory
        (progn
          (message "Downloading file '%s' to '%s' via TRAMP.." remote-path local)
          (copy-file remote-path local t t))
      (progn
        (message "Download directory '%s' to '%s' via TRAMP.." remote-path local)
        (copy-directory remote-path local t t)))))

(defun ssh-deploy--upload (local remote local-root)
  "Upload LOCAL to REMOTE and LOCAL-ROOT via ssh or ftp."
  (if (or (string= (alist-get 'protocol remote) "ssh") (string= (alist-get 'protocol remote) "ftp"))
      (progn
        (let ((path (concat (alist-get 'server remote) ":" (alist-get 'path remote))))
          (message "Uploading path '%s' to '%s'.." local path)
          (let ((file-or-directory (file-regular-p local)))
            (if file-or-directory
                (if (string= (alist-get 'protocol remote) "ssh")
                    (ssh-deploy--upload-file-via-ssh local remote)
                  (ssh-deploy--upload-file-via-ftp local remote))
              (if (string= (alist-get 'protocol remote) "ssh")
                  (ssh-deploy--upload-directory-via-ssh local remote local-root)
                (ssh-deploy--upload-directory-via-ftp local remote local-root))))))
        (message "Unsupported protocol. Only SSH and FTP are supported at the moment.")))

(defun ssh-deploy--upload-file-via-ssh (local remote)
  "Upload file LOCAL to REMOTE via ssh."
  (let ((command (concat (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote)))))
    (message "Uploading file '%s' to '%s' via SSH.." local command)
    (ssh-deploy--run-shell-command (concat "scp " (shell-quote-argument local) " " command))))

(defun ssh-deploy--download-file-via-ssh (remote local)
  "Download file REMOTE to LOCAL via ssh."
  (let ((command (concat (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote)))))
    (message "Downloading file '%s' to '%s' via SSH.." command local)
    (ssh-deploy--run-shell-command (concat "scp " command " " local))))

(defun ssh-deploy--upload-directory-via-ssh (local remote local-root)
  "Upload directory LOCAL to REMOTE and LOCAL-ROOT via ssh."
  (message "Uploading directory '%s' to '%s'.." local (alist-get 'string remote))
  (if (string= local local-root)
      (progn
        (let ((command (concat (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote)))))
          (ssh-deploy--run-shell-command (concat "scp -r " (shell-quote-argument local) "* " command))))
    (progn
      (let ((command (concat (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (file-name-directory (directory-file-name (alist-get 'path remote)))))))
        (ssh-deploy--run-shell-command (concat "scp -r " (shell-quote-argument local) " " command))))))

(defun ssh-deploy--download-directory-via-ssh (remote local local-root)
  "Download directory REMOTE to LOCAL with LOCAL-ROOT via ssh."
  (message "Downloading path '%s' to '%s'.." (alist-get 'string remote) local)
  (if (string= local local-root)
      (progn
        (let ((command (concat (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote)))))
          (ssh-deploy--run-shell-command (concat "scp -r " command "* " (shell-quote-argument local)))))
    (progn
      (let ((command (concat (shell-quote-argument (alist-get 'username remote)) "@" (shell-quote-argument (alist-get 'server remote)) ":" (shell-quote-argument (alist-get 'path remote)))))
        (ssh-deploy--run-shell-command (concat "scp -r " command " " (file-name-directory (directory-file-name local))))))))

(defun ssh-deploy--upload-file-via-ftp (local remote)
  "Upload file LOCAL to REMOTE via ftp."
  (message "Uploading file '%s' to '%s' via FTP.." local (alist-get 'string remote))
  (let ((command (concat "curl --ftp-create-dirs -T " (shell-quote-argument local) " ftp://" (shell-quote-argument (alist-get 'server remote)) (shell-quote-argument (alist-get 'path remote)) " --user " (shell-quote-argument (alist-get 'username remote)) ":" (shell-quote-argument (alist-get 'password remote)))))
    (ssh-deploy--run-shell-command command)))

(defun ssh-deploy--download-file-via-ftp (remote local)
  "Download file REMOTE to LOCAL via ftp."
  (message "Download file '%s' to '%s' via FTP.." (alist-get 'string remote) local)
  (let ((command (concat "curl ftp://" (shell-quote-argument (alist-get 'server remote)) (shell-quote-argument (alist-get 'path remote)) " --user " (shell-quote-argument (alist-get 'username remote)) ":" (shell-quote-argument (alist-get 'password remote)) " -o " (shell-quote-argument local))))
    (ssh-deploy--run-shell-command command)))

;; TODO Implement this
(defun ssh-deploy--upload-directory-via-ftp (local remote local-root)
  "Upload directory LOCAL to REMOTE with LOCAL-ROOT via ftp."
  (message "Upload directory '%s' to '%s' via FTP.." local (alist-get 'string remote))
  (message "Not implemented yet"))

;; (let ((host (split-string remote "@")))
;;   (let ((command (concat "find " local " -type f -exec curl --ftp-create-dirs -T {} ftp://" (shell-quote-argument (car (last host))) "{};")))
;;     (ssh-deploy--run-shell-command command))))

;; find mydir -type f -exec curl -u xxx:psw --ftp-create-dirs -T {} ftp://192.168.1.158/public/demon_test/{} \;

;; TODO Implement this
(defun ssh-deploy--download-directory-via-ftp (remote local local-root)
  "Download directory REMOTE to LOCAL with LOCAL-ROOT via ftp."
  (message "Download directory '%s' to '%s' via FTP.." local (alist-get 'string remote))
  (message "Not implemented yet"))

;;  (let ((host (split-string remote "@")))
;;    (let ((command (concat "curl -s ftp://" (shell-quote-argument (car (last host))) " --user " (car host) ":" ssh-deploy-password " | grep -e '^-' | awk '{ print $9 }' | while read f; do; curl -O ftp://" (shell-quote-argument (car (last host))) " --user" (car host) ":" ssh-deploy-password " -o " local "; done;")))
;;      (ssh-deploy--run-shell-command command))))

;; curl -s ftp://user:pass@IP/path/to/folder/ | \
;; grep -e '^-' | awk '{ print $9 }' | \
;; while read f; do \
;; curl -O ftp://user:pass@IP/path/to/folder/$f; \
;; done)

(defun ssh-deploy (local-root remote-root upload-or-download path)
  "Upload/Download file or directory relative to the roots LOCAL-ROOT with REMOTE-ROOT via ssh or ftp according to UPLOAD-OR-DOWNLOAD and the path PATH."
  (if (ssh-deploy--file-is-in-path path local-root)
      (progn
        (let ((file-or-directory (file-regular-p path)))
          (let ((remote-path (concat remote-root (ssh-deploy--get-relative-path local-root path))))
            (let ((connection (ssh-deploy--parse-remote remote-path)))
              (if (not (null upload-or-download))
                  (ssh-deploy--upload path connection local-root)
                (ssh-deploy--download connection path local-root))))))
        (if ssh-deploy-debug
            (message "Path '%s' is not in the root '%s'" path local-root))))

;;;### autoload
(defun ssh-deploy-upload-handler ()
  "Upload current path to remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local) (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (ssh-deploy--is-not-empty-string buffer-file-name)
          (let ((local-path (file-truename buffer-file-name))
                (local-root (file-truename ssh-deploy-root-local)))
            (ssh-deploy local-root ssh-deploy-root-remote t local-path))
        (if (ssh-deploy--is-not-empty-string default-directory)
            (let ((local-path (file-truename default-directory))
                  (local-root (file-truename ssh-deploy-root-local)))
              (ssh-deploy local-root ssh-deploy-root-remote t local-path))))))

;;;### autoload
(defun ssh-deploy-download-handler ()
  "Download current path from remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local) (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (ssh-deploy--is-not-empty-string buffer-file-name)
          (let ((local-path (file-truename buffer-file-name))
                (local-root (file-truename ssh-deploy-root-local)))
            (ssh-deploy local-root ssh-deploy-root-remote nil local-path))
        (if (ssh-deploy--is-not-empty-string default-directory)
            (let ((local-path (file-truename default-directory))
                  (local-root (file-truename ssh-deploy-root-local)))
              (ssh-deploy local-root ssh-deploy-root-remote nil local-path))))))

;;;### autoload
(defun ssh-deploy-diff-handler ()
  "Compare current path with remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local) (ssh-deploy--is-not-empty-string ssh-deploy-root-remote))
      (if (ssh-deploy--is-not-empty-string buffer-file-name)
          (let ((local-path (file-truename buffer-file-name))
                (local-root (file-truename ssh-deploy-root-local)))
            (ssh-deploy--diff local-root ssh-deploy-root-remote local-path))
        (if (ssh-deploy--is-not-empty-string default-directory)
            (let ((local-path (file-truename default-directory))
                  (local-root (file-truename ssh-deploy-root-local)))
              (ssh-deploy--diff local-root ssh-deploy-root-remote local-path))))))

;;;### autoload
(defun ssh-deploy-remote-terminal-handler ()
  "Open remote host in tramp terminal it is configured for SSH deployment."
  (if (ssh-deploy--is-not-empty-string ssh-deploy-root-remote)
      (ssh-deploy--remote-terminal ssh-deploy-root-remote)))

;;;### autoload
(defun ssh-deploy-browse-remote-handler ()
  "Open current relative path on remote host in `dired-mode' if it is configured for SSH deployment."
  (if (and (ssh-deploy--is-not-empty-string ssh-deploy-root-local) (ssh-deploy--is-not-empty-string ssh-deploy-root-remote) (ssh-deploy--is-not-empty-string default-directory))
      (let ((local-path (file-truename default-directory))
            (local-root (file-truename ssh-deploy-root-local)))
        (ssh-deploy--browse-remote local-root ssh-deploy-root-remote local-path))))

(provide 'ssh-deploy)
;;; ssh-deploy.el ends here
