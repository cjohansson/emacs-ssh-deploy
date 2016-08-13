;;; ssh-deploy.el --- Deployment via SSH, global or per directory.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 5 Jul 2016
;; Modified: 12 Aug 2016
;; Version: 1.35
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `ssh-deploy' enables automatic deploys on explicit-save, manual
;; uploads, downloads, differences, remote terminals and remote directory browsing
;; via key-pair password-less authorized SSH connections.  To do this it uses `tramp',
;; `tramp-term', `scp', `ediff' and `ztree'.
;; By setting the variables (globally or per directory):
;; `ssh-deploy-root-local`,`ssh-deploy-root-remote`,
;; `ssh-deploy-on-explicit-save` you can setup a directory for
;; SSH deploy.
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
;; Now when your in a directory which is deployed via SSH you can access these features.
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(defgroup ssh-deploy nil
  "Upload, download, difference, browse and terminal handler for files and directories on remote hosts via SSH."
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

(defun ssh-deploy-browse-remote (local-root remote-root path)
  "Browse relative to LOCAL-ROOT on REMOTE-ROOT the path PATH in `dired-mode`."
  (if (ssh-deploy-file-is-in-path path local-root)
      (let ((remote-path (concat remote-root (ssh-deploy-get-relative-path local-root path))))
        (message "Opening '%s' for browsing on remote host.." remote-path)
        (dired (concat "/ssh:" remote-path)))))

(defun ssh-deploy-remote-terminal (remote-host)
  "Opens REMOTE-HOST in tramp terminal."
  (if (and (fboundp 'tramp-term)
	   (fboundp 'tramp-term--initialize)
	   (fboundp 'tramp-term--do-ssh-login))
      (progn
        (let ((hostname (replace-regexp-in-string ":.*$" "" remote-host)))
          (let ((host (split-string hostname "@")))
            (message "Opening tramp-terminal for remote host '%s@%s' or '%s' translated from '%s'.." (car host) (car (last host)) hostname remote-host)
            (unless (eql (catch 'tramp-term--abort (tramp-term--do-ssh-login host)) 'tramp-term--abort)
              (tramp-term--initialize hostname)
              (run-hook-with-args 'tramp-term-after-initialized-hook hostname)
              (message "tramp-term initialized")))))
    (message "tramp-term is not installed.")))

(defun ssh-deploy-file-is-in-path (file path)
  "Return true if FILE is in the path PATH."
  (not (null (string-match path file))))

(defun ssh-deploy-get-relative-path (root path)
  "Return a string for the relative path based on ROOT and PATH."
  (replace-regexp-in-string root "" path))

(defun ssh-deploy-diff (local-root remote-root path)
  "Find differences relative to the roots LOCAL-ROOT with REMOTE-ROOT via ssh and the path PATH."
  (let ((file-or-directory (file-regular-p path)))
    (if (ssh-deploy-file-is-in-path path local-root)
        (progn
          (let ((remote-path (concat "/ssh:" remote-root (ssh-deploy-get-relative-path local-root path))))
            (if file-or-directory
                (progn
                  (message "Comparing file '%s' to '%s'.." path remote-path)
                  (ediff path remote-path))
              (progn
                (if (fboundp 'ztree-diff)
                    (progn
                      (message "Comparing directory '%s' to '%s'.." path remote-path)
                      (ztree-diff path remote-path))
                  (message "ztree-diff is not installed.")
                  )))))
      (if ssh-deploy-debug
          (message "Path '%s' is not in the root '%s'" path local-root)))))

(defun ssh-deploy-is-not-empty-string (string)
  "Return true if the STRING is not empty and not nil.  Expects string."
  (and (not (null string))
       (not (zerop (length string)))))

(defun ssh-deploy-run-shell-command (command)
  "Run COMMAND in asynchronous mode."
  (message "Shell command: '%s'" command)
  (let ((proc (start-process-shell-command "process" nil command)))
    (set-process-filter proc (lambda (proc output)(message "%s" (replace-regexp-in-string "\^M" "\n" output))))
    (set-process-sentinel proc (lambda (proc output)
				 (if (string= (symbol-name (process-status proc)) "exit")
				     (if (= (process-exit-status proc) 0)
					 (message "Successfully ran shell command.")
				       (message "Failed to run shell command.")))))))

(defun ssh-deploy (local-root remote-root upload-or-download path)
  "Upload/Download relative to the roots LOCAL-ROOT with REMOTE-ROOT via SSH according to UPLOAD-OR-DOWNLOAD and the path PATH."
  (let ((file-or-directory (file-regular-p path)))
    (let ((remote-path (concat remote-root (ssh-deploy-get-relative-path local-root path))))
      (if (ssh-deploy-file-is-in-path path local-root)
          (progn
            (if (not (null upload-or-download))
                (progn
                  (message "Uploading path '%s' to '%s'.." path remote-path)
                  (if file-or-directory
                      (progn
                        (let ((command (concat "scp " (shell-quote-argument path) " " (shell-quote-argument remote-path))))
			  (ssh-deploy-run-shell-command command)))
                    (progn
                      (let ((command (concat "scp -r " (shell-quote-argument path) " " (shell-quote-argument (file-name-directory (directory-file-name remote-path))))))
			(ssh-deploy-run-shell-command command)))))
              (progn
                (message "Downloading path '%s' to '%s'.." remote-path path)
                (if file-or-directory
                    (progn
                      (message "Downloading file '%s' to '%s'.." remote-path path)
                      (let ((command (concat "scp " (shell-quote-argument remote-path) " " (shell-quote-argument path))))
			(ssh-deploy-run-shell-command command)))
                  (progn
                    (message "Downloading directory '%s' to '%s'.." remote-path path)
                    (let ((command (concat "scp -r " (shell-quote-argument remote-path) " " (shell-quote-argument (file-name-directory (directory-file-name path))))))
		      (ssh-deploy-run-shell-command command)))))))
        (if ssh-deploy-debug
            (message "Path '%s' is not in the root '%s'" path local-root))))))

;;;### autoload
(defun ssh-deploy-upload-handler ()
  "Upload current path to remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote))
      (if (ssh-deploy-is-not-empty-string buffer-file-name)
	  (let ((local-path (file-truename buffer-file-name))
		(local-root (file-truename ssh-deploy-root-local)))
	    (ssh-deploy local-root ssh-deploy-root-remote t local-path))
        (if (ssh-deploy-is-not-empty-string default-directory)
	    (let ((local-path (file-truename default-directory))
		  (local-root (file-truename ssh-deploy-root-local)))
	      (ssh-deploy local-root ssh-deploy-root-remote t local-path))))))

;;;### autoload
(defun ssh-deploy-download-handler ()
  "Download current path from remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote))
      (if (ssh-deploy-is-not-empty-string buffer-file-name)
	  (let ((local-path (file-truename buffer-file-name))
		(local-root (file-truename ssh-deploy-root-local)))
	    (ssh-deploy local-root ssh-deploy-root-remote nil local-path))
        (if (ssh-deploy-is-not-empty-string default-directory)
	    (let ((local-path (file-truename default-directory))
		  (local-root (file-truename ssh-deploy-root-local)))
	      (ssh-deploy local-root ssh-deploy-root-remote nil local-path))))))

;;;### autoload
(defun ssh-deploy-diff-handler ()
  "Compare current path with remote host if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote))
      (if (ssh-deploy-is-not-empty-string buffer-file-name)
	  (let ((local-path (file-truename buffer-file-name))
		(local-root (file-truename ssh-deploy-root-local)))
	    (ssh-deploy-diff local-root ssh-deploy-root-remote local-path))
        (if (ssh-deploy-is-not-empty-string default-directory)
	    (let ((local-path (file-truename default-directory))
		  (local-root (file-truename ssh-deploy-root-local)))
	      (ssh-deploy-diff local-root ssh-deploy-root-remote local-path))))))

;;;### autoload
(defun ssh-deploy-remote-terminal-handler ()
  "Open remote host in tramp terminal it is configured for SSH deployment."
  (if (ssh-deploy-is-not-empty-string ssh-deploy-root-remote)
      (ssh-deploy-remote-terminal ssh-deploy-root-remote)))

;;;### autoload
(defun ssh-deploy-browse-remote-handler ()
  "Open current relative path on remote host in `dired-mode' if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string default-directory))
      (let ((local-path (file-truename default-directory))
	    (local-root (file-truename ssh-deploy-root-local)))
	(ssh-deploy-browse-remote local-root ssh-deploy-root-remote local-path))))

(provide 'ssh-deploy)
;;; ssh-deploy.el ends here
