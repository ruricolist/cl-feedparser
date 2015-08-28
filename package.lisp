;;;; package.lisp

(defpackage #:cl-feedparser
  (:use #:cl #:serapeum #:alexandria #:anaphora #:local-time)
  (:nicknames #:feedparser)
  (:export #:parse-feed
           #:*keys* #:feedparser-key #:gethash*
           #:repair #:return-feed
           #:feed-sanitizer
           #:unsanitized-string #:unsanitized-string-string #:string+
           #:sanitize-title #:sanitize-content #:sanitize-text
           #:feed-string
           #:*base*
           #:parse-time
           #:masked?))
