(in-package #:cl-feedparser)

(defmethod handle-tag ((ns (eql :feedburner)) lname)
  "The feed is from Feedburner."
  (declare (ignore lname))
  (ensure2 (gethash* :proxy *feed*) "feedburner"))

(defhandler :feedburner :browser-friendly
  (handle-tag :atom :info))

(defhandler :feedburner :orig-link
  ;; Eg. 3QD.
  (when *entry*
    (setf (gethash* :link *entry*)
          (resolve-uri (get-text)))))
