(defsystem "cl-feedparser-tests"
  :description "Test suite for cl-feedparser."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("cl-feedparser" "fiveam" "local-time" "fxml/html5")
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "tests")))
