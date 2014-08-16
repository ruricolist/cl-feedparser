(in-package #:cl-feedparser)

(defhandler :itunes :subtitle
  (handle-tag :atom :subtitle))

(defhandler :itunes :author
  (handle-tag nil :author))
