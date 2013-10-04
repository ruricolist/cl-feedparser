(in-package :cl-feedparser)

(eval-and-compile
  (defvar *base*
    #.(fad:pathname-directory-pathname (or *compile-file-truename* *load-truename*))))

(defparameter *feedparser-version*
  "5.1.3")

(defparameter *feedparse-script*
  (merge-pathnames #p"parse.py" *base*))

(defun name->keyword (name)
  (make-keyword (upcase (join (words (string-downcase name)) #\-))))

(defun parse-json (string)
  (ignoring end-of-file
    (yason:parse string :object-key-fn #'name->keyword)))

(defmethod feedparse.py ((string string))
  (let ((output
          (trivial-shell:shell-command
           *feedparse-script*
           :input string)))
    (parse-json output)))

(defmethod feedparse.py ((pathname pathname))
  (feedparse.py (read-file-into-string pathname)))
