(defpackage :cl-feedparser/time
  (:use :cl :alexandria :serapeum :local-time)
  (:import-from :cl-ppcre :regex-replace)
  (:import-from :net.telent.date)
  (:export :parse-time :time=))

(in-package :cl-feedparser/time)

(defun parse-time (string)
  "Parse STRING as a date.
As a second value, return the parser used: either :net.telent.date
or :local-time."
  (let ((string (regex-replace "UT$" string "GMT")))
    (if-let (date (net.telent.date:parse-time string))
      (values date :net.telent.date)
      (ignoring local-time::invalid-timestring ;XXX
        (handler-case
            (when-let (date
                       (timestamp-to-universal
                        (parse-timestring string)))
              (values date :local-time)))))))

(assert (= 3645907200 (parse-time "Wed, 15 Jul 2015 00:00:00 UT")))

(defmethod time= ((t1 integer) (t2 integer))
  (= t1 t2))

(defmethod time= ((t1 timestamp) (t2 timestamp))
  (timestamp= t1 t2))

(defmethod time= ((t1 timestamp) (t2 integer))
  (time= t2 t1))

(defmethod time= ((t1 integer) (t2 timestamp))
  (mvlet* ((ss1 mm1 hh1 day1 month1 year1 (decode-universal-time t1))
           (nsec2 ss2 mm2 hh2 day2 month2 year2 (decode-timestamp t2)))
    (declare (ignore nsec2))
    (and (= ss1 ss2)
         (= mm1 mm2)
         (= hh1 hh2)
         (= day1 day2)
         (= month1 month2)
         (= year1 year2))))

(defmethod time= ((t1 string) t2) (time= (parse-time t1) t2))
(defmethod time= (t1 (t2 string)) (time= t1 (parse-time t2)))

;; (assert
;;  (let* ((time (get-universal-time))
;;         (timestamp (local-time:universal-to-timestamp time)))
;;    (and (time= time timestamp)
;;         (not (time= (1+ time) timestamp)))))
