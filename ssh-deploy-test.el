;;; ssh-deploy-test.el --- Unit and integration tests for ssh-deploy.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2018  Free Software Foundation, Inc.

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


(autoload 'should "ert")

(autoload 'ssh-deploy-diff-mode "ssh-deploy-diff-mode")

(autoload 'ssh-deploy "ssh-deploy")
(autoload 'ssh-deploy--get-revision-path "ssh-deploy")
(autoload 'ssh-deploy--file-is-in-path "ssh-deploy")
(autoload 'ssh-deploy--is-not-empty-string "ssh-deploy")

(defun ssh-deploy-test--get-revision-path ()
  "Test this function."
  (should (string= (expand-file-name "./_mydirectory_random-file.txt") (ssh-deploy--get-revision-path "/mydirectory/random-file.txt" (expand-file-name "."))))
  )

(defun ssh-deploy-test--file-is-in-path ()
  "Test this function."
  (should (equal t (ssh-deploy--file-is-in-path "/mydirectory/test.txt" "/mydirectory/")))
  (should (equal nil (ssh-deploy--file-is-in-path "/mydirectory/test.txt" "/mydirectory2/")))
  (should (equal nil (ssh-deploy--file-is-in-path "/mydirectory2/test.txt" "/mydirectory/")))
  )

(defun ssh-deploy-test--is-not-empty-string ()
  "Test this function."
  (should (equal t (ssh-deploy--is-not-empty-string "abc")))
  (should (equal nil (ssh-deploy--is-not-empty-string "")))
  (should (equal nil (ssh-deploy--is-not-empty-string nil)))
  )

(defun ssh-deploy-test ()
  "Run test for plug-in."
  (ssh-deploy-test--get-revision-path)
  (ssh-deploy-test--file-is-in-path)
  (ssh-deploy-test--is-not-empty-string)
  )

(ssh-deploy-test)


(provide 'ssh-deploy-test)
;;; ssh-deploy-test.el ends here
