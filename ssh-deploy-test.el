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
(autoload 'ssh-deploy--file-is-in-path-p "ssh-deploy")
(autoload 'ssh-deploy--is-not-empty-string-p "ssh-deploy")

(defun ssh-deploy-test--download ()
  "Test downloads."

  (require 'ediff-util)

  (let ((directory-a (expand-file-name "test-a/"))
        (directory-b (expand-file-name "test-b/")))

    ;; Delete directories if they already exists
    (when (file-directory-p directory-a)
      (delete-directory directory-a t))
    (when (file-directory-p directory-b)
      (delete-directory directory-b t))

    (make-directory-internal directory-a)
    (make-directory-internal directory-b)

    (let* ((file-a (expand-file-name "test-b" directory-a))
           (file-b (expand-file-name "test-b" directory-b))
           (file-b-contents "Random text")
           (ssh-deploy-root-local directory-a)
           (ssh-deploy-root-remote directory-b))

      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote)

        ;; Create a new file and add it's contents
        (find-file file-b)
        (insert file-b-contents)
        (save-buffer)

        ;; Visit local root
        (let ((default-directory directory-a))

          ;; Download file
          (ssh-deploy-download file-b file-a 0 nil 0)

          ;; Verify that both files have equal contents
          (should (equal t (ediff-same-file-contents file-a file-b)))

          (delete-file file-b)
          (delete-file file-a))))
    
    (delete-directory directory-a t)
    (delete-directory directory-b t)))

(defun ssh-deploy-test--get-revision-path ()
  "Test this function."
  (should (string= (expand-file-name "./_mydirectory_random-file.txt") (ssh-deploy--get-revision-path "/mydirectory/random-file.txt" (expand-file-name ".")))))

(defun ssh-deploy-test--file-is-in-path ()
  "Test this function."
  (should (equal t (ssh-deploy--file-is-in-path-p "/mydirectory/test.txt" "/mydirectory/")))
  (should (equal nil (ssh-deploy--file-is-in-path-p "/mydirectory/test.txt" "/mydirectory2/")))
  (should (equal nil (ssh-deploy--file-is-in-path-p "/mydirectory2/test.txt" "/mydirectory/"))))

(defun ssh-deploy-test--is-not-empty-string ()
  "Test this function."
  (should (equal t (ssh-deploy--is-not-empty-string-p "abc")))
  (should (equal nil (ssh-deploy--is-not-empty-string-p "")))
  (should (equal nil (ssh-deploy--is-not-empty-string-p nil))))

(defun ssh-deploy-test ()
  "Run test for plug-in."
  (ssh-deploy-test--get-revision-path)
  (ssh-deploy-test--file-is-in-path)
  (ssh-deploy-test--is-not-empty-string)
  (ssh-deploy-test--download))

(ssh-deploy-test)


(provide 'ssh-deploy-test)
;;; ssh-deploy-test.el ends here
