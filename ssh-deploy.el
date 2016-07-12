;;; ssh-deploy.el --- Deployment via SSH, global or per directory.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 5 Jul 2016
;; Modified: 11 Jul 2016
;; Version: 1.0
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
;; uploads, manual downloads and manual diffs via key-pair
;; password-less authorized SSH connections.  To do this it uses tramp,
;; scp and ediff.  By setting the variables (globally or per directory):
;; `ssh-deploy-root-local`,`ssh-deploy-root-remote`,
;; `ssh-deploy-on-explicity-save` you can setup a directory for
;; SSH deploy.
;;
;; - To setup hook on explicit save do this:
;;     (add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicity-save (ssh-deploy-upload-handler)) ))
;;
;; - To set key-bindings do something like this:
;;     (global-set-key (kbd "C-c C-z u") (lambda() (interactive)(ssh-deploy-upload-handler) ))
;;     (global-set-key (kbd "C-c C-z d") (lambda() (interactive)(ssh-deploy-download-handler)(revert-buffer) ))
;;     (global-set-key (kbd "C-c C-z x") (lambda() (interactive)(ssh-deploy-diff-handler) ))
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(defgroup ssh-deploy nil
  "Upload, download and difference handler for files on remote hosts via SSH."
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

(defcustom ssh-deploy-on-explicity-save nil
  "Boolean variable if deploy should be made on explicit save, nil by default."
  :type 'boolean
  :group 'ssh-deploy)

(defun ssh-deploy-diff (local-root-raw remote-root)
  "Find differences between the path LOCAL-ROOT-RAW with REMOTE-ROOT via ssh."
  (let ((filename (shell-quote-argument buffer-file-name))
        (local-root (shell-quote-argument local-root-raw)))
    (let ((remote-path (concat "/" remote-root (replace-regexp-in-string local-root "" filename))))
      (if (string-match local-root filename)
          (progn
	    (message "Comparing file '%s' to '%s'.." filename remote-path)
	    (ediff filename remote-path))))))

(defun ssh-deploy-is-not-empty-string (string)
  "Return true if the STRING is not empty and not nil.  Expects string."
  (and (not (null string))
       (not (zerop (length string)))))

(defun ssh-deploy (local-root-raw remote-root-raw upload-or-download)
  "Upload/Download the path LOCAL-ROOT-RAW with REMOTE-ROOT-RAW via SSH according to UPLOAD-OR-DOWNLOAD."
  (let ((filename (shell-quote-argument buffer-file-name))
        (local-root (shell-quote-argument local-root-raw))
        (remote-root (shell-quote-argument remote-root-raw)))
    (let ((remote-path (concat remote-root (replace-regexp-in-string local-root "" filename))))
      (if (string-match local-root filename)
          (progn
            (if (not (null upload-or-download))
                (progn
                  (message "Uploading file '%s' to '%s'.." filename remote-path)
                  (let ((command (concat "scp " filename " " remote-path)))
                    (message "Upload command: '%s'" command)
		    (if (= (shell-command command) 0)
			(message "Successfully uploaded '%s' to '%s'" filename remote-path)
		      (message "Failed to upload '%s' to '%s'" filename remote-path))))
              (progn
		(message "Downloading file '%s' to '%s'.." remote-path filename)
		(let ((command (concat "scp " remote-path " " filename)))
		  (message "Upload command: '%s'" command)
		  (if (= (shell-command command) 0)
		      (message "Successfully downloaded '%s' to '%s'" remote-path filename)
		    (message "Failed to download '%s' to '%s'" remote-path filename)
		    )))))))))

;;;### autoload
(defun ssh-deploy-upload-handler ()
  "Upload current file if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string buffer-file-name))
      (ssh-deploy ssh-deploy-root-local ssh-deploy-root-remote t)
    ))

;;;### autoload
(defun ssh-deploy-download-handler ()
  "Download current file if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string buffer-file-name))
      (ssh-deploy ssh-deploy-root-local ssh-deploy-root-remote nil)
    ))

;;;### autoload
(defun ssh-deploy-diff-handler ()
  "Compare current file with remote if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string buffer-file-name))
      (ssh-deploy-diff ssh-deploy-root-local ssh-deploy-root-remote)
    ))

(provide 'ssh-deploy)
;;; ssh-deploy.el ends here
