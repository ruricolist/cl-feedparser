;;;; cl-feedparser.asd

(asdf:defsystem #:cl-feedparser
  :serial t
  :description "Common Lisp universal feed parser"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "LLGPL"
  :depends-on (#:alexandria
               #:serapeum
               #:anaphora
               #:net-telent-date
               #:local-time
               #:cl-libxml2
               #:sanitize
               #:cl-fad
               #:babel
               #:markup-grinder)
  :components ((:file "package")
               (:file "cl-feedparser")))
