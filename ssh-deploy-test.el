;;; ssh-deploy-test.el --- Unit and integration tests for ssh-deploy.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2023  Free Software Foundation, Inc.

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

(require 'ssh-deploy)
(require 'ssh-deploy-diff-mode)

(defun ssh-deploy-test--download (async async-with-threads)
  "Test downloads asynchronously if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."
  (message "\nTest Download\n")
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
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
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
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a file-b))))

        (delete-file file-b)
        (delete-file file-a)))

    (delete-directory directory-a t)
    (delete-directory directory-b t)))

(defun ssh-deploy-test--rename-and-delete (async async-with-threads)
  "Test downloads asynchronous if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."
  (message "\nTest Rename and delete\n")
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
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
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

        ;; Both old files should not exist any more
        (should (equal nil (file-exists-p file-a-old)))
        (should (equal nil (file-exists-p file-b-old)))

        ;; Both new files should exist any more
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
  (message "\nTest Upload\n")
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
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
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
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a file-b))))

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
            (should (equal nil (nth 0 (ssh-deploy--diff-files file-a file-b))))

            (ssh-deploy-upload-handler)
            (when (> async 0)
              (sleep-for 1))
            (kill-buffer)

            ;; Verify that both files have equal contents
            (should (equal t (nth 0 (ssh-deploy--diff-files file-a file-b))))

            ;; Delete both test files
            (delete-file file-b)
            (delete-file file-a)))))

    (delete-directory directory-a t)
    (delete-directory directory-b t)))

(defun ssh-deploy-test--detect-remote-changes (async async-with-threads)
  "Test uploads asynchronously if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."

  (message "\nTest Detect Remote Changes\n")
  (let ((directory-a (expand-file-name "test-a/"))
        (directory-b (expand-file-name "test-b/")))

    ;; Delete directories if they already exists
    (when (file-directory-p directory-a)
      (delete-directory directory-a t))
    (when (file-directory-p directory-b)
      (delete-directory directory-b t))

    (make-directory-internal directory-a)
    (make-directory-internal directory-b)

    (let* ((file-a (file-truename (expand-file-name "test.txt" directory-a)))
           (file-b (file-truename (expand-file-name "test.txt" directory-b)))
           (file-a-contents "Random text")
           (ssh-deploy-root-local (file-truename directory-a))
           (ssh-deploy-root-remote (file-truename directory-b))
           (ssh-deploy-on-explicit-save 1)
           (ssh-deploy-force-on-explicit-save 0)
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-async-with-threads async-with-threads)
           (revision-file (ssh-deploy--get-revision-path file-a ssh-deploy-revision-folder)))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
                 ssh-deploy-force-on-explicit-save
                 ssh-deploy-debug
                 ssh-deploy-async
                 ssh-deploy-async-with-threads)

        ;; Modify local file, remote file should be automatically uploaded
        (ssh-deploy-add-after-save-hook)
        (find-file file-a)
        (insert file-a-contents)
        (save-buffer) ;; NOTE Should trigger upload action
        (when (> async 0)
          (sleep-for 1))
        (kill-buffer)

        ;; Verify that both files have equal contents
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a revision-file))))
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a file-b))))

        ;; Make changes in file-b
        (find-file file-b)
        (insert "ABC")
        (save-buffer)
        (kill-buffer)

        ;; Verify that file-a and file-b differs
        (should (equal nil (nth 0 (ssh-deploy--diff-files file-a file-b))))

        ;; Make changes in file-a
        (find-file file-a)
        (insert "More")
        (save-buffer)

        (when (> async 0)
          (sleep-for 1))

        ;; Verify that file-a and file-b still differs
        (should (equal nil (nth 0 (ssh-deploy--diff-files file-a file-b))))

        ;; Make changes in file-a
        (find-file file-a)
        (setq ssh-deploy-force-on-explicit-save 1)
        (insert "More")
        (save-buffer)
        (kill-buffer)

        (when (> async 0)
          (sleep-for 1))

        ;; Verify that both files have equal contents again
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a file-b))))

        (setq ssh-deploy-force-on-explicit-save 0)

        ;; Modify only local revision
        (find-file revision-file)
        (insert "Random blob")
        (save-buffer)
        (kill-buffer)

        ;; Verify that both files don't have equal contents anymore
        (should (equal nil (nth 0 (ssh-deploy--diff-files file-a revision-file))))

        ;; Remote file should signal change now
        (if (> async 0)
            (progn
              (ssh-deploy--async-process
               (lambda() (ssh-deploy--remote-changes-data file-a))
               (lambda(response)
                 (should (equal 8 (nth 0 response))))
               async-with-threads)
              (sleep-for 1))
          (let ((response (ssh-deploy--remote-changes-data file-a)))
            (should (equal 8 (nth 0 response)))))

        ;; Run post-executor that should copy local-file to revision-file
        (ssh-deploy--remote-changes-post-executor (list 8 "" file-a revision-file) ssh-deploy-verbose)

        ;; Verify that both files have equal contents again
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a revision-file))))
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a file-b))))

        ;; Update should now trigger upload
        (find-file file-b)
        (insert "Random blob")
        (save-buffer)
        (kill-buffer)
        
        ;; Remote file should signal change now
        (if (> async 0)
            (progn
              (ssh-deploy--async-process
               (lambda() (ssh-deploy--remote-changes-data file-a))
               (lambda(response) (should (equal 5 (nth 0 response))))
               async-with-threads)
              (sleep-for 1))
          (should (equal 5 (nth 0 (ssh-deploy--remote-changes-data file-a)))))

        ;; Open file-a and download remote
        (find-file file-a)
        (ssh-deploy-download-handler)
        (when (> async 0)
          (sleep-for 1))
        (kill-buffer)

        ;; Remote file should not signal change now
        (if (> async 0)
            (progn
              (ssh-deploy--async-process
               (lambda() (ssh-deploy--remote-changes-data file-a))
               (lambda(response) (should (equal 4 (nth 0 response))))
               async-with-threads)
              (sleep-for 1))
          (should (equal 4 (nth 0 (ssh-deploy--remote-changes-data file-a)))))

        ;; Delete both test files
        (delete-file file-b)
        (delete-file file-a)))

    (delete-directory directory-a t)
    (delete-directory directory-b t)))

(defun ssh-deploy-test--directory-diff (async async-with-threads)
  "Test directory differences asynchronously if ASYNC is above zero, with threads if ASYNC-WITH-THREADS is above zero."

  (message "\nTest Directory Difference\n")
  (let ((directory-a (file-truename (expand-file-name "test-a/")))
        (directory-b (file-truename (expand-file-name "test-b/"))))

    ;; Delete directories if they already exists
    (when (file-directory-p directory-a)
      (delete-directory directory-a t))
    (when (file-directory-p directory-b)
      (delete-directory directory-b t))

    ;; Make directories for test
    (make-directory-internal directory-a)
    (make-directory-internal directory-b)

    (let* ((file-1-filename "test.txt")
           (file-2-filename "test2.txt")
           (file-a-1 (file-truename (expand-file-name file-1-filename directory-a)))
           (file-a-2 (file-truename (expand-file-name file-2-filename directory-a)))
           (file-b-1 (file-truename (expand-file-name file-1-filename directory-b)))
           (file-b-2 (file-truename (expand-file-name file-2-filename directory-b)))
           (file-a-1-contents "Random text")
           (file-a-2-contents "Randomized text")
           (ssh-deploy-root-local (file-truename directory-a))
           (ssh-deploy-root-remote (file-truename directory-b))
           (ssh-deploy-on-explicit-save 1)
           (ssh-deploy-debug 0)
           (ssh-deploy-async async)
           (ssh-deploy-exclude-list nil)
           (ssh-deploy-async-with-threads async-with-threads))

      ;; Just bypass the linter here
      (when (and ssh-deploy-root-local
                 ssh-deploy-root-remote
                 ssh-deploy-on-explicit-save
                 ssh-deploy-debug
                 ssh-deploy-async
                 ssh-deploy-async-with-threads)

        (ssh-deploy-add-after-save-hook)

        ;; Create file 1
        (find-file file-a-1)
        (insert file-a-1-contents)
        (save-buffer) ;; NOTE Should trigger upload action
        (when (> async 0)
          (sleep-for 1))
        (kill-buffer)

        ;; Verify that both files have equal contents
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a-1 file-b-1))))

        ;; Create file 2
        (find-file file-a-2)
        (insert file-a-2-contents)
        (save-buffer) ;; NOTE Should trigger upload action
        (when (> async 0)
          (sleep-for 1))
        (kill-buffer)

        ;; Verify that both files have equal contents
        (should (equal t (nth 0 (ssh-deploy--diff-files file-a-2 file-b-2))))

        ;; Both files should equal
        (should (equal
                 (ssh-deploy--diff-directories-data directory-a directory-b ssh-deploy-exclude-list)
                 (list directory-a directory-b ssh-deploy-exclude-list (list file-1-filename file-2-filename) nil nil (list file-1-filename file-2-filename) nil)))

        ;; Modify file B
        (find-file file-b-2)
        (insert file-a-1-contents)
        (save-buffer)
        (kill-buffer)

        ;; Verify that both files have equal contents
        (should (equal nil (nth 0 (ssh-deploy--diff-files file-a-2 file-b-2))))

        ;; Both files should equal
        (should (equal
                 (ssh-deploy--diff-directories-data directory-a directory-b ssh-deploy-exclude-list)
                 (list directory-a directory-b ssh-deploy-exclude-list (list file-1-filename file-2-filename) nil nil (list file-1-filename) (list file-2-filename))))

        ;; Delete test files
        (delete-file file-b-2)
        (delete-file file-b-1)
        (delete-file file-a-1)
        (delete-file file-a-2)))

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
  (require 'ssh-deploy)
  (setq make-backup-files nil)

  (let ((ssh-deploy-verbose 1)
        (ssh-deploy-debug 1)
        ;; (debug-on-error t)
        (async-threads (fboundp 'make-thread))
        (async-el (fboundp 'async-start))
        (ssh-deploy-revision-folder (file-truename (expand-file-name "revisions"))))
    (when (and ssh-deploy-verbose
               ssh-deploy-debug)

      (if async-threads
          (message "\nNOTE: Running tests for asynchronous threads as well since it's loaded\n")
        (message "\nNOTE: Skipping tests for asynchronous threads since it's not loaded\n"))
      
      (if async-el
          (message "\nNOTE: Running tests for async.el as well since it's loaded\n")
        (message "\nNOTE: Skipping tests for async.el since it's not loaded\n"))

      (ssh-deploy-test--get-revision-path)
      (ssh-deploy-test--file-is-in-path)
      (ssh-deploy-test--is-not-empty-string)

      ;; Upload
      (ssh-deploy-test--upload 0 0)
      (when async-el
        (ssh-deploy-test--upload 1 0))
      (when async-threads
        (ssh-deploy-test--upload 1 1))

      ;; Download
      (ssh-deploy-test--download 0 0)
      (when async-el
        (ssh-deploy-test--download 1 0))
      (when async-threads
        (ssh-deploy-test--download 1 1))

      ;; Rename And Delete
      (ssh-deploy-test--rename-and-delete 0 0)
      (when async-el
        (ssh-deploy-test--rename-and-delete 1 0))
      (when async-threads
        (ssh-deploy-test--rename-and-delete 1 1))

      ;; Directory Differences
      (ssh-deploy-test--directory-diff 0 0)
      (when async-el
        (ssh-deploy-test--directory-diff 1 0))
      (when async-threads
        (ssh-deploy-test--directory-diff 1 1))

      ;; Detect Remote Changes
      (ssh-deploy-test--detect-remote-changes 0 0)
      (when async-el
        (ssh-deploy-test--detect-remote-changes 1 0))
      (when async-threads
        (ssh-deploy-test--detect-remote-changes 1 1))

      (delete-directory ssh-deploy-revision-folder t)

      )))

(ssh-deploy-test)


(provide 'ssh-deploy-test)
;;; ssh-deploy-test.el ends here
