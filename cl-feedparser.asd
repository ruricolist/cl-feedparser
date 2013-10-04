;;;; cl-feedparser.asd

(asdf:defsystem #:cl-feedparser
  :serial t
  :description "Describe cl-feedparser here"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:serapeum
               #:trivial-shell
               #:anaphora
               #:yason
               #:cl-libxml2
               #:net-telent-date
               #:local-time
               #:iterate
               #:sanitize
               #:cl-fad
               #:cl-textcat
               #:optima
               #:optima.ppcre)
  :components ((:file "package")
               (:file "cl-feedparser")))
