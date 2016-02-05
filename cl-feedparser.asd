;;;; cl-feedparser.asd

(asdf:defsystem #:cl-feedparser
  :serial t
  :description "Common Lisp universal feed parser"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "LLGPL"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:cl-feedparser/all))

(asdf:register-system-packages :cxml '(:cxml :klacks :cxml-dom :sax))
(asdf:register-system-packages :cxml-stp '(:stp :cxml-stp))

