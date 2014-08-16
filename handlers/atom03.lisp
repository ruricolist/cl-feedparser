(in-package #:cl-feedparser)

(defmethod handle-tag ((ns (eql :atom03)) tag)
  "Fall back to Atom 1.0 parsing."
  (handle-tag :atom tag))

(defhandler :atom03 :tagline
  (handle-tag :atom :subtitle))

(defhandler :atom :info
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash* :info *feed*) text)))

(defhandler :atom03 :copyright
  (handle-tag :atom :rights))

(defhandler :atom03 :modified
  (handle-tag :atom :updated))

(defhandler :atom03 :created
  (awhen (get-timestring)
    (setf (gethash* :created *entry*) t)))
