(in-package #:cl-feedparser)

(defmethod handle-tag ((ns (eql :dcterms)) lname)
  (handle-tag :dc lname))

(defhandler :dc :title
  (handle-tag :atom :title))

(defhandler :dc :rights
  (handle-tag :atom :rights))

(defhandler :dc :creator
  (handle-tag nil :author))

(defhandler :dc :date
  (handle-tag :atom :updated))

(defhandler :dcterms :modified
  (handle-tag :atom :updated))

(defhandler :dcterms :contributor
  (push (dict :name (get-text))
        (gethash* :contributors *entry*)))

(defhandler :dc :created
  (handle-tag :atom03 :created))

(defhandler :dc :language
  (handle-tag nil :language))
