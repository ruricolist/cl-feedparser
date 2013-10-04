;;;; package.lisp

(defpackage #:cl-feedparser
  (:use #:cl #:serapeum #:alexandria #:anaphora #:iterate)
  (:nicknames #:feedparser)
  (:shadowing-import-from
   #:serapeum
   #:collecting #:in #:summing #:reducing #:maximizing #:minimizing #:repeat)
  (:export #:parse-feed #:punt #:parse-feed-safe))
