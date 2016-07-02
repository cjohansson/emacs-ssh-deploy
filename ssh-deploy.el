;;; ssh-deploy.el --- Syncing of files using SSH
;;; Author: Christian Johansson <christian@cvj.se>
;;; TODO: Add options for checking if remote contents has changed contents
;;; Commentary:
;;; Code:

;; Variables
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
  :group 'ssh-deploy
  )
(defcustom ssh-deploy-key-binding-prefix "C-c C-z"
  "String variable of prefix 'key-binding', default is C-c C-z."
  :type 'string
  :group 'ssh-deploy
)

;; Functions
(defun ssh-deploy-diff (localRootRaw remoteRoot)
  "Find differences between the path LOCALROOTRAW with REMOTEROOT via ssh."
  (let ((filename (shell-quote-argument buffer-file-name))
        (localRoot (shell-quote-argument localRootRaw)))
    (let ((remotePath (concat "/" remoteRoot (replace-regexp-in-string localRoot "" filename))))
      (if (string-match localRoot filename)
          (progn
	    (message "Comparing file '%s' to '%s'.." filename remotePath)
	    (ediff filename remotePath))))))
(defun ssh-deploy-is-not-empty-string (string)
  "Return true if the STRING is not empty and not nil.  Expects string."
  (and (not (null string))
       (not (zerop (length string)))))
(defun ssh-deploy (localRootRaw remoteRootRaw uploadOrDownload)
  "Upload/Download the path LOCALROOTRAW with REMOTEROOTRAW via ssh according to UPLOADORDOWNLOAD."
  (let ((filename (shell-quote-argument buffer-file-name))
        (localRoot (shell-quote-argument localRootRaw))
        (remoteRoot (shell-quote-argument remoteRootRaw)))
    (let ((remotePath (concat remoteRoot (replace-regexp-in-string localRoot "" filename))))
      (if (string-match localRoot filename)
          (progn
            (if (not (null uploadOrDownload))
                (progn
                  (message "Uploading file '%s' to '%s'.." filename remotePath)
                  (let ((command (concat "scp " filename " " remotePath)))
                    (message "Upload command: '%s'" command)
		    (if (= (shell-command command) 0)
			(message "Successfully uploaded '%s' to '%s'" filename remotePath)
		      (message "Failed to upload '%s' to '%s'" filename remotePath))))
              (progn
		(message "Downloading file '%s' to '%s'.." remotePath filename)
		(let ((command (concat "scp " remotePath " " filename)))
		  (message "Upload command: '%s'" command)
		  (if (= (shell-command command) 0)
		      (message "Successfully downloaded '%s' to '%s'" remotePath filename)
		    (message "Failed to download '%s' to '%s'" remotePath filename)
		    )))))))))
(defun ssh-deploy-upload-handler ()
  "Upload current file if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string buffer-file-name))
      (ssh-deploy ssh-deploy-root-local ssh-deploy-root-remote t)
    ))
(defun ssh-deploy-download-handler ()
  "Download current file if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string buffer-file-name))
      (ssh-deploy ssh-deploy-root-local ssh-deploy-root-remote nil)
    ))
(defun ssh-deploy-diff-handler ()
  "Compare current file with remote if it is configured for SSH deployment."
  (if (and (ssh-deploy-is-not-empty-string ssh-deploy-root-local) (ssh-deploy-is-not-empty-string ssh-deploy-root-remote) (ssh-deploy-is-not-empty-string buffer-file-name))
      (ssh-deploy-diff ssh-deploy-root-local ssh-deploy-root-remote)
    ))

;; Hooks
(add-hook 'after-save-hook (lambda() (if ssh-deploy-on-explicity-save (ssh-deploy-upload-handler)) ))

;; Key-bindings
(global-set-key (kbd (concat ssh-deploy-key-binding-prefix " u")) (lambda() (interactive)(ssh-deploy-upload-handler) ))
(global-set-key (kbd (concat ssh-deploy-key-binding-prefix " d")) (lambda() (interactive)(ssh-deploy-download-handler)(revert-buffer) ))
(global-set-key (kbd (concat ssh-deploy-key-binding-prefix " x")) (lambda() (interactive)(ssh-deploy-diff-handler) ))

(provide 'ssh-deploy)
;;; ssh-deploy.el ends here
