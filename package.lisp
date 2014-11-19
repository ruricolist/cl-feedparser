;;;; package.lisp

(defpackage #:cl-feedparser
  (:use #:cl #:serapeum #:alexandria #:anaphora #:local-time)
  (:nicknames #:feedparser)
  (:export #:parse-feed
           #:repair #:return-feed
           #:feed-sanitizer
           #:unsanitized-string #:unsanitized-string-string #:string+))
