;;;; package.lisp

(defpackage #:cl-feedparser
  (:use #:cl #:serapeum #:alexandria #:anaphora)
  (:nicknames #:feedparser)
  (:export #:parse-feed
           #:repair #:return-feed
           #:feed-sanitizer
           #:unsanitized-string #:unsanitized-string-string #:string+))
