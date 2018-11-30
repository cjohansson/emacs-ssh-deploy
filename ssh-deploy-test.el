;;; ssh-deploy-test.el --- Unit and integration tests for ssh-deploy.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2018  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 5 Jul 2016
;; Modified: 28 Nov 2018
;; Version: 3.0
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-ssh-deploy

;; Package-Requires: ((emacs "24"))

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


;; This file contains unit and integration tests for ssh-deploy plug-in for Emacs.


;;; Code:


(autoload 'ssh-deploy-diff-mode "ssh-deploy-diff-mode")
(autoload 'ssh-deploy "ssh-deploy")
(autoload 'ssh-deploy--file-is-in-path "ssh-deploy")
(autoload 'should "ert")

(defun ssh-deploy-test--file-is-in-path ()
  "Test this function."
  (should (equal t (ssh-deploy--file-is-in-path "/mydirectory/test.txt" "/mydirectory/")))
  (should (equal nil (ssh-deploy--file-is-in-path "/mydirectory/test.txt" "/mydirectory2/")))
  (should (equal nil (ssh-deploy--file-is-in-path "/mydirectory2/test.txt" "/mydirectory/")))
  )


(defun ssh-deploy-tests ()
  "Run test for plug-in."
  (ssh-deploy-test--file-is-in-path)
  )

(ssh-deploy-tests)


(provide 'ssh-deploy-test)
;;; ssh-deploy-test.el ends here
