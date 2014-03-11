;;;; package.lisp

(defpackage #:cl-feedparser
  (:use #:cl #:serapeum #:alexandria #:anaphora)
  (:nicknames #:feedparser)
  (:export #:parse-feed #:repair #:return-feed #:parse-feed-safe))
