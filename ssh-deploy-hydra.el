;;; ssh-deploy-hydra.el --- Deployment via Tramp, global or per directory.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

;; Package-Requires: ((emacs "25"))

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

;; This file only contains hydra shortcut

;; Please see README.md from the same repository for more extended documentation.

;;; Code:


(require 'ssh-deploy)

(defmacro ssh-deploy--fboundp-macro (name body)
  "Expand to BODY if NAME is bound.
Takes care of byte-compilation issues where the `byte-code' for the latter could signal an error if it has been compiled with Emacs 24.1 and is then later run by Emacs 24.5."
  (declare (indent 2) (debug (symbolp form &rest form)))
  (if (fboundp name)
      body
    `(when (fboundp ',name)
       (eval ',body))))

(ssh-deploy--fboundp-macro
 defhydra
 (defhydra ssh-deploy-hydra (:color red :hint nil)
   "
    SSH Deploy Menu
    
    _u_: Upload                              _f_: Force Upload
    _d_: Download
    _D_: Delete
    _x_: Difference
    _t_: Eshell Base Terminal                _T_: Eshell Relative Terminal
    _h_: Shell Base Terminal                 _H_: Shell Relative Terminal
    _e_: Detect Remote Changes
    _R_: Rename
    _b_: Browse Base                         _B_: Browse Relative
    _o_: Open current file on remote         _m_: Open sql-mysql on remote
    _s_: Run deploy script
    "
   ("f" #'ssh-deploy-upload-handler-forced)
   ("u" #'ssh-deploy-upload-handler)
   ("d" #'ssh-deploy-download-handler)
   ("D" #'ssh-deploy-delete-handler)
   ("x" #'ssh-deploy-diff-handler)
   ("t" #'ssh-deploy-remote-terminal-eshell-base-handler)
   ("T" #'ssh-deploy-remote-terminal-eshell-handler)
   ("h" #'ssh-deploy-remote-terminal-shell-base-handler)
   ("H" #'ssh-deploy-remote-terminal-shell-handler)
   ("e" #'ssh-deploy-remote-changes-handler)
   ("R" #'ssh-deploy-rename-handler)
   ("b" #'ssh-deploy-browse-remote-base-handler)
   ("B" #'ssh-deploy-browse-remote-handler)
   ("o" #'ssh-deploy-open-remote-file-handler)
   ("m" #'ssh-deploy-remote-sql-mysql-handler)
   ("s" #'ssh-deploy-run-deploy-script-handler)))

;;;###autoload
(defun ssh-deploy-hydra (shortcut)
  "Attach hydra at SHORTCUT."
  (when (fboundp 'ssh-deploy-hydra/body)
    (global-set-key (kbd shortcut) #'ssh-deploy-hydra/body)))

(provide 'ssh-deploy-hydra)
;;; ssh-deploy-hydra.el ends here
