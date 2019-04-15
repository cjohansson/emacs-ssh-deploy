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

(autoload 'ediff-same-file-contents "ediff-util")

(autoload 'ssh-deploy-diff-mode "ssh-deploy-diff-mode")

(autoload 'ssh-deploy "ssh-deploy")
(autoload 'ssh-deploy--get-revision-path "ssh-deploy")
(autoload 'ssh-deploy--file-is-in-path-p "ssh-deploy")
(autoload 'ssh-deploy--is-not-empty-string-p "ssh-deploy")
(autoload 'ssh-deploy-download "ssh-deploy")
(autoload 'ssh-deploy-upload "ssh-deploy")
(autoload 'ssh-deploy-rename "ssh-deploy")
(autoload 'ssh-deploy-delete-both "ssh-deploy")
(autoload 'ssh-deploy-add-after-save-hook "ssh-deploy")
(autoload 'ssh-deploy-add-after-save-hook "ssh-deploy")
(autoload 'ssh-deploy-upload-handler "ssh-deploy")

(defun ssh-deploy-test--download (async async-with-threads)
  "Test downloads asynchronously if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."
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
           (ssh-deploy-root-remote directory-b)
           (ssh-deploy-on-explicit-save 0)
           (ssh-deploy-verbose 0)
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
                 ssh-deploy-verbose
                 ssh-deploy-debug
                 ssh-deploy-async
                 ssh-deploy-async-with-threads)

        ;; Create a new file and add it's contents
        (find-file file-b)
        (insert file-b-contents)
        (save-buffer)
        (kill-buffer)

        ;; Download file
        (ssh-deploy-download file-b file-a 0 nil 0)
        (when (> async 0)
          (sleep-for 1))

        ;; Verify that both files have equal contents
        (should (equal t (ediff-same-file-contents file-a file-b)))

        (delete-file file-b)
        (delete-file file-a)))

    (delete-directory directory-a t)
    (delete-directory directory-b t)))

(defun ssh-deploy-test--rename-and-delete (async async-with-threads)
  "Test downloads asynchronous if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."
  (let ((directory-a (expand-file-name "test-a/"))
        (directory-b (expand-file-name "test-b/"))
        (filename-old "testfile.txt")
        (filename-new "testfile-renamed.txt"))

    ;; Delete directories if they already exists
    (when (file-directory-p directory-a)
      (delete-directory directory-a t))
    (when (file-directory-p directory-b)
      (delete-directory directory-b t))

    (make-directory-internal directory-a)
    (make-directory-internal directory-b)

    (let* ((file-a-old (expand-file-name filename-old directory-a))
           (file-a-new (expand-file-name filename-new directory-a))
           (file-b-old (expand-file-name filename-old directory-b))
           (file-b-new (expand-file-name filename-new directory-b))
           (file-contents "Random text")
           (ssh-deploy-root-local directory-a)
           (ssh-deploy-root-remote directory-b)
           (ssh-deploy-on-explicit-save 0)
           (ssh-deploy-verbose 0)
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
                 ssh-deploy-verbose
                 ssh-deploy-debug
                 ssh-deploy-async
                 ssh-deploy-async-with-threads)

        ;; Create new files and add it's contents
        (find-file file-a-old)
        (insert file-contents)
        (save-buffer)
        (kill-buffer)

        ;; Create new files and add it's contents
        (find-file file-b-old)
        (insert file-contents)
        (save-buffer)
        (kill-buffer)

        ;; Both files should exist
        (should (equal t (file-exists-p file-a-old)))
        (should (equal t (file-exists-p file-b-old)))

        ;; Rename filename
        (find-file file-a-old)
        (ssh-deploy-rename file-a-old file-a-new)
        (when (> async 0)
          (sleep-for 1))

        ;; Both old files should not exist anymore
        (should (equal nil (file-exists-p file-a-old)))
        (should (equal nil (file-exists-p file-b-old)))

        ;; Both new files should exist anymore
        (should (equal t (file-exists-p file-a-new)))
        (should (equal t (file-exists-p file-b-new)))

        ;; Delete file
        (ssh-deploy-delete-both file-a-new)
        (when (> async 0)
          (sleep-for 1))
        (kill-buffer)

        ;; Both new files should not exist anymore
        (should (equal nil (file-exists-p file-a-new)))
        (should (equal nil (file-exists-p file-b-new)))))

    (delete-directory directory-a t)
    (delete-directory directory-b t)))

(defun ssh-deploy-test--upload (async async-with-threads)
  "Test uploads asynchronously if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."

  (let ((directory-a (expand-file-name "test-a/"))
        (directory-b (expand-file-name "test-b/")))

    ;; Delete directories if they already exists
    (when (file-directory-p directory-a)
      (delete-directory directory-a t))
    (when (file-directory-p directory-b)
      (delete-directory directory-b t))

    (make-directory-internal directory-a)
    (make-directory-internal directory-b)

    (let* ((file-a (expand-file-name "test.txt" directory-a))
           (file-b (expand-file-name "test.txt" directory-b))
           (file-a-contents "Random text")
           (ssh-deploy-root-local directory-a)
           (ssh-deploy-root-remote directory-b)
           (ssh-deploy-on-explicit-save 1)
           (ssh-deploy-verbose 0)
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
                 ssh-deploy-verbose
                 ssh-deploy-debug
                 ssh-deploy-async
                 ssh-deploy-async-with-threads)

        (ssh-deploy-add-after-save-hook)
        (find-file file-a)
        (insert file-a-contents)
        (save-buffer) ;; NOTE Should trigger upload action
        (when (> async 0)
          (sleep-for 1))

        ;; Verify that both files have equal contents
        (should (equal t (ediff-same-file-contents file-a file-b)))

        ;; Turn of automatic uploads
        (let ((ssh-deploy-on-explicit-save 0))
          ;; Bypass linter again
          (when ssh-deploy-on-explicit-save

            ;; Update should not trigger upload
            (insert file-a-contents)
            (save-buffer)
            (when (> async 0)
              (sleep-for 1))

            ;; Verify that both files have equal contents
            (should (equal nil (ediff-same-file-contents file-a file-b)))

            (ssh-deploy-upload-handler)
            (when (> async 0)
              (sleep-for 1))
            (kill-buffer)

            ;; Verify that both files have equal contents
            (should (equal t (ediff-same-file-contents file-a file-b)))

            ;; Delete both test files
            (delete-file file-b)
            (delete-file file-a)))))

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
  (if (fboundp 'async-start)
      (message "\nNOTE: Running tests for async.el as well since it's loaded\n")
    (message "\nNOTE: Skipping tests for async.el since it's not loaded\n"))
  (ssh-deploy-test--get-revision-path)
  (ssh-deploy-test--file-is-in-path)
  (ssh-deploy-test--is-not-empty-string)

  (ssh-deploy-test--upload 0 0)
  (when (fboundp 'async-start)
    (ssh-deploy-test--upload 1 0))
  (ssh-deploy-test--upload 1 1)

  (ssh-deploy-test--download 0 0)
  (when (fboundp 'async-start)
    (ssh-deploy-test--download 1 0))
  (ssh-deploy-test--download 1 1)

  (ssh-deploy-test--rename-and-delete 0 0)
  (when (fboundp 'async-start)
    (ssh-deploy-test--rename-and-delete 1 0))
  (ssh-deploy-test--rename-and-delete 1 1))

(ssh-deploy-test)


(provide 'ssh-deploy-test)
;;; ssh-deploy-test.el ends here
