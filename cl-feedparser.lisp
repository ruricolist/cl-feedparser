;;;; cl-feedparser.lisp

(in-package #:cl-feedparser)

;;; "cl-feedparser" goes here. Hacks and glory await!

(defparameter *version*
  "0.3")

(defvar *base* nil
  "Default value for `current-xml-base'.")

(defun version-string ()
  (fmt "cl-feedparser ~a" *version*))

(deftype universal-time ()
  'integer)

(deftype time-designator ()
  '(or universal-time timestamp))

(deftype entry-mtime ()
  '(or time-designator null))

(deftype id ()
  '(or null string quri:uri))

(deftype mask-time ()
  '(or time-designator null string))

(deftype mask ()
  '(or string (cons string mask-time)))

(deftype max-entries ()
  '(or null wholenum))

(deftype uri-protocol ()
  '(or null string))



;;; TODO We should really switch from hash tables to objects.

;;; This is a hack to statically check that the wrong key can't be set
;;; due to a typo.

(eval-and-compile
  (defparameter *keys*
    '(:entries :bozo :title :info :rights :subtitle
      :proxy :link :rel :type :href :title
      :author :name :email :uri :email
      :name :author :language :icon
      :summary :value :summary-detail :subtitle
      :published :published-parsed
      :updated :updated-parsed
      :ttl :id :content :proxy
      :base :value :type
      :author-detail :links
      :enclosures :contributors
      :created :comments)))

(deftype feedparser-key ()
  `(member ,@*keys*))

(defmacro gethash* (key hash &optional default)
  "Like `gethash', but check (statically) that KEY is a member of
`*keys*'."
  (check-type key feedparser-key)
  `(gethash ,key ,hash ,@(unsplice default)))



(deftype sanitizer ()
  '(or null sax-sanitize::mode))

(deftype sanitizer-designator ()
  '(or sanitizer (eql t)))

(defstruct (unsanitized-string (:constructor unsanitized-string (string)))
  "Wrapper for an unsanitized string."
  (string "" :type string))

(deftype feed-string ()
  '(or string unsanitized-string))

(def empty-uri (quri:uri ""))

(defun empty-uri? (uri)
  (etypecase uri
    (string (emptyp uri))
    (quri:uri (quri:uri= uri empty-uri))))

(def http :http)
(def https :https)
(def relative nil)

(defun protocol-allowed? (protocol)
  (etypecase-of uri-protocol protocol
    (null t)
    (string
     (string-case protocol
       (("http" "https") t)))))

(defun string+ (&rest strings)
  "Concatenate STRINGS, ensuring that if any are unsanitized, the
result is an unsanitized string."
  (let* ((unsanitized nil)
         (s (with-output-to-string (s)
              (dolist (string strings)
                (etypecase-of feed-string string
                  (string (write-string string s))
                  (unsanitized-string
                   (setf unsanitized t)
                   (write-string (unsanitized-string-string string) s)))))))
    (if unsanitized
        (unsanitized-string s)
        s)))

(defun make-html-sink (&key base sanitizer)
  (~> (html5-sax:make-html5-sink)
      (make-absolute-uri-handler :base base)
      (sax-sanitize:wrap-sanitize sanitizer)))

(defun has-markup? (string)
  (ppcre:scan "[<>&]" string))

(defun sanitize-aux (x sanitizer)
  (cond ((typep x 'stp:node)
         (let ((sink (make-html-sink :sanitizer sanitizer)))
           (if (typep x 'stp:document)
               (stp:serialize x sink)
               (progn
                 (stp:serialize x sink)
                 (sax:end-document sink)))))
        ((not (stringp x)) x)
        ((emptyp x) x)
        ((not (has-markup? x)) x)
        (t (etypecase-of sanitizer sanitizer
             (null (unsanitized-string x))
             (sax-sanitize::mode
              (html5-sax:serialize-dom
               (html5-parser:parse-html5 x)
               (make-html-sink :sanitizer sanitizer)))))))

(defparameter *content-sanitizer* feed-sanitizer)

(defparameter *title-sanitizer* sax-sanitize:restricted)

(defun sanitize-content (x &optional (sanitizer t))
  (etypecase-of sanitizer-designator sanitizer
    ((eql t) (sanitize-aux x *content-sanitizer*))
    (sanitizer (sanitize-aux x sanitizer))))

(defun sanitize-title (x &optional (sanitizer t))
  (etypecase-of sanitizer-designator sanitizer
    ((eql t) (sanitize-aux x *title-sanitizer*))
    (sanitizer (sanitize-aux x sanitizer))))

(defun sanitize-text (x)
  (cond ((not (stringp x)) x)
        ((emptyp x) x)
        ((not (has-markup? x)) x)
        ;; If there are no entities, just strip all tags.
        ((not (find #\& x))
         ;; Tags that never close...
         (ppcre:regex-replace-all "<[^>]*>?" x " "))
        ;; If there are entities, we have to resort to a real parser.
        (t (sanitize-aux x sax-sanitize:default))))

;; Unclosed tags.
(assert (equal " " (sanitize-text "<video poster=javascript:alert(10)//")))



(defvar *parser*)
(defvar *source*)
(defvar *feed*)
(defvar *entry*)
(defvar *author*)
(defvar *content*)
(defvar *links*)
(defvar *author*)
(defvar *disabled*)

(defun ctx ()
  (or *entry* *feed*))

(defclass parser ()
  ((max-entries :initarg :max-entries :accessor parser-max-entries :type max-entries)
   (entries-count :initform 0 :accessor parser-entries-count :type wholenum)
   (guid-mask :initarg :guid-mask :accessor parser-guid-mask :type list))
  (:default-initargs :guid-mask nil
                     :max-entries nil))

(defun parser-context (input &key max-entries guid-mask)
  (let* ((*entry* nil)
         (*disabled* nil)
         (author (dict))
         (*author* author)
         (parser (make 'parser
                       :max-entries max-entries
                       :guid-mask guid-mask))
         (*parser* parser)
         (feed (dict :parser (version-string)
                     :author-detail *author*))
         (*feed* feed))

    (flet ((return-feed (&key bozo)
             (nreversef (gethash* :entries feed))
             (when bozo
               (setf (gethash* :bozo feed) t))
             (return-from parser-context
               (values feed parser))))
      (restart-case
          (catch 'parser-done
            (parser-loop (cxml:make-source input)))
        (return-feed ()
          :report "Return whatever we have so far."
          (return-feed :bozo t)))
      (return-feed))))

(defun parser-loop (source &key recursive)
  (let ((*source* source)
        (depth 0))
    (loop for event = (klacks:peek source) while event do
      (case event
        (:start-element
         (incf depth)
         (multiple-value-bind (ev uri lname)
             (klacks:consume source)
           (declare (ignore ev))
           (unless *disabled*
             (handle-tag (find-ns uri) lname))))
        (:end-element
         (decf depth)
         (klacks:consume source)
         (when (and recursive (minusp depth))
           (return)))
        (t (klacks:consume source))))))

(defun lispify (id)
  "Convert ID from camel-case to hyphenated form."
  ;; A little faster than a string stream.
  (declare (optimize speed) (string id))
  (let ((s (make-array 5
                       :element-type 'character
                       :adjustable t
                       :fill-pointer 0)))
    (loop for c across id
          if (upper-case-p c)
            do (vector-push-extend #\- s 2)
               (vector-push c s)
          else do (vector-push-extend c s)
          finally (return (nstring-upcase s)))))

(defgeneric handle-tag (ns lname)
  (:method (ns lname) (declare (ignore ns lname))
    nil)
  (:method (ns (lname string))
    (handle-tag ns (find-keyword (lispify lname)))))

(defmacro defhandler (ns lname &body body)
  (unless (member ns namespace-prefixes)
    (error "Unknown namespace: ~a" ns))
  (with-gensyms (gns glname)
    (let ((ns-spec
            (if (eql ns nil)
                `(,gns null)
                `(,gns (eql ,ns)))))
      `(defmethod handle-tag (,ns-spec (,glname (eql ,lname)))
         ,@body))))

(defun strip-parens (s)
  (when (stringp s)
    (string-trim " ()" s)))

(defun get-timestring ()
  (when-let (string (get-text/sanitized))
    (values string (parse-time string))))

(defun parse-time (string)
  (let ((string (ppcre:regex-replace "UT$" string "GMT")))
    (or (net.telent.date:parse-time string)
        (ignoring local-time::invalid-timestring ;XXX
          (timestamp-to-universal
           (parse-timestring string))))))

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

(assert
 (let* ((time (get-universal-time))
        (timestamp (local-time:universal-to-timestamp time)))
   (and (time= time timestamp)
        (not (time= (1+ time) timestamp)))))

(defun id-masked? (id mask)
  (etypecase-of id id
    (null nil)
    (string (equal id mask))
    (quri:uri (equal (quri:render-uri id nil) mask))))

(defun masked? (mask id &optional mtime)
  "Compare ID and MTIME against MASK and return T if the mask applies."
  ;; Note the strategy here: unless an explicit modification time is
  ;; provided by the feed, we assume the post has /not/ been modified.
  (etypecase-of mask mask
    (string (id-masked? id mask))
    ((cons string mask-time)
     (destructuring-bind (mask-id . mask-mtime) mask
       (and (id-masked? id mask-id)
            (etypecase-of entry-mtime mtime
              (null t)
              (time-designator
               (etypecase-of mask-time mask-mtime
                 (null nil)
                 ((or time-designator string)
                  (time= mtime mask-mtime))))))))))

(defun guid-masked? (entry)
  (let ((id (gethash* :id entry)))
    (some (lambda (mask)
            (let ((mtime (gethash* :updated-parsed entry)))
              (masked? mask id mtime)))
          (parser-guid-mask *parser*))))

(defun check-guid-mask (&optional (entry *entry*))
  (when (guid-masked? entry)
    (setf *disabled* t)))

(defun current-xml-base-aux ()
  "Hack to work around the fact that Feedburner wipes out the xml:base
attribute: if there is no current XML base, and this is a Feedburner
feed, use the :link property of the feed as the base."
  (when (boundp '*feed*)
    (let ((feed *feed*)
          (xml-base (klacks:current-xml-base *source*)))
      (if (not (emptyp xml-base)) xml-base
          (when (equal (gethash* :proxy feed) "feedburner")
            (gethash* :link feed))))))

(defun current-xml-base ()
  (or *base* (current-xml-base-aux) ""))

(defun get-content (&aux (source *source*))
  (let ((type (klacks:get-attribute source "type"))
        (content (dict :base (current-xml-base))))
    (if (equal type "xhtml")
        (xhtml-content content)
        (text-content content))))

(defun text-content (content)
  (let* ((attrs (klacks:list-attributes *source*))
         (string (sanitize-content (get-text))))
    (setf (gethash* :value content) string
          (gethash* :type content)  (guess-type string attrs))
    content))

(defclass absolute-uri-handler (cxml:broadcast-handler)
  ((base :initarg :base :accessor base-of)))

(defun make-absolute-uri-handler (handler &key (base (current-xml-base)))
  (make 'absolute-uri-handler :base base :handlers (list handler)))

(defun resolve-attr (attr)
  (with-accessors ((name sax:attribute-local-name)
                   (value sax:attribute-value))
      attr
    (when (or (equal name "href") (equal name "src"))
      (let ((abs-uri (quri:uri (resolve-uri value))))
        (if (empty-uri? abs-uri)
            (setf value "#")
            (setf value (quri:render-uri abs-uri nil)))))))

(defmethod sax:start-element ((handler absolute-uri-handler) ns lname qname attrs)
  (dolist (attr attrs)
    (resolve-attr attr))
  (call-next-method handler ns lname qname attrs))

(defun get-xhtml-content-value (&aux (source *source*))
  (let* ((handler (make-html-sink :base (current-xml-base)
                                  :sanitizer *content-sanitizer*))
         (string
           (klacks:serialize-element source handler :document-events t)))
    (etypecase-of sanitizer *content-sanitizer*
      (null (unsanitized-string string))
      (sax-sanitize::mode string))))

(defun xhtml-content (content &aux (source *source*))
  (klacks:find-element source "div")
  (let ((value (get-xhtml-content-value)))
    (setf (gethash* :value content) value ;Already sanitized.
          (gethash* :type content)  "text/html"
          (gethash* :base content)  (current-xml-base)))
  content)

(defun attrs-type (attrs)
  (when-let (attr (find "type" attrs :test 'equal :key #'sax:attribute-local-name))
    (sax:attribute-value attr)))

(defun guess-type (value attrs)
  (let ((type (attrs-type attrs)))
    (cond ((null type) nil)
          ((find #\/ type) type)
          ((string= type "html") "text/html")
          ((find-if (op (in _ #\< #\>)) value) "text/html")
          (t "text/plain"))))

(defun entry-count (&aux (parser *parser*))
  (parser-entries-count parser))

(defun (setf entry-count) (value &aux (parser *parser*))
  (check-type value wholenum)
  (setf (parser-entries-count parser) value)
  (let ((max-entries (parser-max-entries parser)))
    (etypecase-of max-entries max-entries
      (null value)
      (wholenum
       (when (> value max-entries)
         (throw 'parser-done nil))))))

(defun ensure-entry-id (entry &key id)
  "Substitute link for ID if there is none."
  (ensure2 (gethash* :id entry)
    (or id
        (when-let (id (gethash* :link entry))
          (check-guid-mask entry)
          (princ-to-string id)))))

(defun entry-loop (entry &key id)
  (let ((*disabled* *disabled*))
    (parser-loop *source* :recursive t)
    ;; Ensure an ID.
    (ensure-entry-id entry :id id)
    (unless *disabled*
      (push entry (gethash* :entries *feed*)))))

(defun entry-context (&key id)
  (incf (entry-count))
  (lret* ((author (dict))
          (entry (dict))
          (*author* author)
          (*entry* entry))
    (setf (gethash* :author-detail entry) author)
    (entry-loop entry :id id)
    (setf (gethash* :author entry) (gethash* :name author))))

(defun trim-uri (uri)
  (trim-whitespace (remove #\Newline uri)))

(defun resolve-uri/base (uri)
  (let ((base (current-xml-base)))
    (ignoring quri:uri-error
     (let* ((uri (merge-uris uri base))
            (protocol (or (quri:uri-scheme uri) relative)))
       (when (protocol-allowed? protocol)
         (quri:render-uri uri nil))))))

(defun merge-uris (uri base)
  (quri:merge-uris
   (quri:uri uri)
   (quri:uri base)))

(defun resolve-uri (uri)
  (when (stringp uri) (callf #'trim-uri uri))
  (or (resolve-uri/base uri) ""))

(defun get-text (&aux (source *source*))
  (trim-whitespace
   (if (not (eql (klacks:peek source) :characters))
       (get-text-from-elements)
       (with-output-to-string (s)
         (loop while (eql (klacks:peek source) :characters)
               do (write-string (nth-value 1 (klacks:consume source)) s))))))

(defun get-text-from-elements (&aux (source *source*))
  "Handle the case where RSS is used without CDATA."
  (let ((handler (cxml:make-string-sink)))
    (loop while (eql (klacks:peek source) :start-element) do
      (klacks:serialize-element source handler))
    (sax:end-document handler)))

(defun get-text/sanitized ()
  (sanitize-text (get-text)))

(defun urlish? (x)
  "Does X look like a URL?"
  (ppcre:scan "(?s)^\\s*https?://" x))



(defun repair ()
  "Invoke the REPAIR restart, if available."
  (maybe-invoke-restart 'repair))

(defun return-feed ()
  "Invoke the RETURN-FEED restart, if available."
  (maybe-invoke-restart 'return-feed))

(defun parse-feed (feed &key max-entries
                             (sanitize-content t)
                             (sanitize-titles t)
                             guid-mask
                             (safe t))
  "Try to parse FEED.
MAX-ENTRIES is the maximum number of entries to retrieve; GUID-MASK is
a list of GUIDs of entries that are already known to the caller and
thus not of interest.

Note that MAX-ENTRIES and GUID-MASK are, in effect, applied
successively, as though MAX-ENTRIES were first taken, then filtered by
GUID. (Actually, entries with masked GUIDs are not even parsed.)
Consider a feed with thousands of entries (they do exist): if the mask
were applied first, you would get another set of older entries each
time you called PARSE-FEED.

Sanitizing content can be turned off with SANITIZE-CONTENT (defaults
to T). SANITIZE-TITLES controls sanitizing titles. Sanitizing titles
uses the sanitizer `sax-sanitize:restricted'. Sanitizing content uses
the sanitizer `feedparser:feed-sanitizer'. There is no way to change
this, but you can turn sanitizing off. If you do so, then the hash
table returned will contain, instead of strings, objects of type
`unsanitized-string'. It is then your responsibility to extract the
underlying string (using `unsanitized-string-string') and sanitize it
yourself.

The idea is that if you see a string in the result, you can be sure it
is sanitized.

\(Note that if you do disable sanitizing, you may still get string
values in cases where the parser is able to determine that sanitizing
is not needed at all, or where it uses special sanitizing strategies,
or where the field is unconditionally sanitized, like dates and
email addresses.)"
  (when (pathnamep feed)
    (setf feed (fad:file-exists-p feed)))
  (let ((bozo nil)
        (*content-sanitizer* (if sanitize-content *content-sanitizer* nil))
        (*title-sanitizer* (if sanitize-titles *title-sanitizer* nil)))
    (handler-bind
        ((error
           (lambda (c) (declare (ignore c))
             (when safe
               ;; If CXML can't repair the damage,
               ;; fall back to markup-grinder.
               (repair)
               ;; Last resort: return whatever we've got so far.
               (return-feed)))))
      (handler-bind
          ((cxml:undefined-entity
             (lambda (c)
               (when safe
                 (when-let (exp (plump-dom:translate-entity
                                 (cxml:undefined-entity-name c)))
                   (use-value (string exp)))
                 (continue))))
           (cxml:undeclared-namespace
             (lambda (c)
               (when safe
                 (let* ((prefix (cxml:undeclared-namespace-prefix c))
                        (uri (prefix-uri prefix)))
                   (store-value uri)
                   (continue)))))
           (cxml:well-formedness-violation
             (lambda (c)
               (when safe
                 (continue c)))))
        (flet ((parse (feed)
                 ;; Always set the bozo bit before other handlers
                 ;; can take effect.
                 (handler-bind ((error (op (push _ bozo))))
                   (let ((parse
                           (parser-context feed
                                           :max-entries max-entries
                                           :guid-mask guid-mask)))
                     (when bozo
                       (setf (gethash :bozo parse) t
                             (gethash :bozo-exception parse) bozo))
                     (values parse bozo)))))
          (restart-case
              (parse feed)
            (repair ()
              :report "Try to repair the XML document and try again."
              (handler-bind (((or plump:invalid-xml-character
                                  plump:discouraged-xml-character)
                               #'abort))
                (~> feed
                    plump:parse
                    (plump:serialize nil)
                    parse)))))))))
