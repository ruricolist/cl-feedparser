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
               #:markup-grinder
               #:sax-sanitize
               #:html5-sax
               #:fset
               #:cxml
               #:closure-html
               #:cl-ppcre)
  :components ((:file "package")
               (:file "sanitizer")
               (:file "namespaces")
               (:file "cl-feedparser")))
