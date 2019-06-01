;;;; cl-feedparser.asd

(defsystem "cl-feedparser"
  :serial t
  :description "Common Lisp universal feed parser"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "LLGPL"
  :version "1.2.0"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("cl-feedparser/all")
  :in-order-to ((test-op (test-op "cl-feedparser/test"))))

(defsystem "cl-feedparser/test"
  :serial t
  :description "Test suite for cl-feedparser."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :perform (test-op (o c) (symbol-call :cl-feedparser/test :run-tests))
  :depends-on ("cl-feedparser" "fiveam" "local-time" "fxml/html5")
  :pathname "test/"
  :components ((:file "package")
               (:file "suite")
               (:file "tests")))

(asdf:register-system-packages :cxml '(:cxml :klacks :cxml-dom :sax))
(asdf:register-system-packages :fxml '(:fxml :fxml-dom :fxml.klacks :fxml.sax))
(asdf:register-system-packages :cxml-stp '(:stp :cxml-stp))
(asdf:register-system-packages :fxml/stp '(:fxml.stp))
(asdf:register-system-packages :fxml/html5 '(:fxml.html5))
(asdf:register-system-packages :fxml/sanitize '(:fxml.sanitize))
(asdf:register-system-packages :cl-html5-parser '(:html5-parser))
(asdf:register-system-packages :net-telent-date '(:net.telent.date))

