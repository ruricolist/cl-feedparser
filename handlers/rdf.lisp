(in-package #:cl-feedparser)

(defhandler :rdf :title
  (handle-tag :atom :title))

(defhandler :rdf :description
  (handle-tag :dc :description))

(defhandler :rdf :item
  (when-let (id (klacks:get-attribute *source* "about"))
    (entry-context :id id)))
