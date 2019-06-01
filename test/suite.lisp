(in-package :cl-feedparser/test)

(def-suite cl-feedparser)

(defun run-tests ()
  (run! 'cl-feedparser))

(defun debug-test (test &key (error t) (failure t))
  "Run TEST, breaking on error or failure."
  (let ((5am:*on-error* (and error :debug))
        (5am:*on-failure* (and failure :debug)))
    (run! test)))

(defparameter *test-data-dir*
  (asdf:system-relative-pathname :cl-feedparser "test/data/"))

(defun find-test-file (name)
  (uiop:merge-pathnames* name *test-data-dir*))

(defun load-test-file (name &key external-format)
  (read-file-into-string (find-test-file name) :external-format external-format))
