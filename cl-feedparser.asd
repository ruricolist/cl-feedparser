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
               #:cl-fad
               #:babel
               #:plump
               #:sax-sanitize
               #:html5-sax
               #:fset
               #:cxml
               #:closure-html
               #:cl-ppcre)
  :components ((:file "package")
               (:file "sanitizer")
               (:file "namespaces")
               (:file "cl-feedparser")
               (:module handlers
                :components ((:file "atom")
                             (:file "atom03")
                             (:file "rss")
                             (:file "rdf")
                             (:file "dc")
                             (:file "feedburner")
                             (:file "itunes")))))
