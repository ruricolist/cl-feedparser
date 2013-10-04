(in-package #:cl-feedparser)

(defun parse-feed (input &key max-entries)
  (when (pathnamep input)
    (setf input (fad:file-exists-p input)))
  (check-type max-entries limit)
  (restart-case
      (parse-feed-aux input :max-entries max-entries)
    (punt ()
      :report "Punt to feedparser.py."
      (aprog1 (or (ignore-errors (feedparse.py input)) (dict))
        (setf (@ it :parser)
              (fmt "feedparser ~a" *feedparser-version*))))))

(defun parse-feed-safe (input &key max-entries)
  (handler-bind ((error
                   (lambda (c) (declare (ignore c))
                     (invoke-restart 'punt))))
    (parse-feed input :max-entries max-entries)))
