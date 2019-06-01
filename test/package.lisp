(defpackage :cl-feedparser/test
  (:use :cl :alexandria :serapeum :cl-feedparser :fiveam)
  (:import-from :html5-parser :parse-html5)
  (:shadowing-import-from :serapeum :string+)
  (:export :run-tests))
