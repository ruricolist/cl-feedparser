(in-package :cl-user)
(defpackage :cl-feedparser/parser
  (:use
    :cl :alexandria :serapeum :anaphora
    :cl-feedparser/xml-namespaces
    :cl-feedparser/feed-sanitizer
    :cl-feedparser/time)
  (:import-from :local-time :timestamp)
  (:import-from :cl-ppcre :regex-replace-all)
  (:shadowing-import-from :cl-ppcre :scan)
  (:import-from :quri)
  (:import-from :fxml)
  (:import-from :fxml.sanitize)
  (:import-from :fxml.html5)
  (:import-from :fxml.stp)
  (:import-from :plump)
  (:import-from :html5-parser :parse-html5)
  (:import-from :uiop :file-exists-p)
  (:shadow :string+)
  (:export
   :parse-feed
   :*keys* :feedparser-key :feed-ref :gethash*
   :repair :return-feed
   :feed-sanitizer
   :unsanitized-string :unsanitized-string-string :string+
   :sanitize-title :sanitize-content :sanitize-text
   :feed-string
   :*base*
   :parse-time
   :masked?

   :gethash* :ensure-gethash*
   :defhandler :handle-tag
   :urlish?
   :get-content :get-text :get-text/sanitized
   :sanitize-title
   :*entry*
   :*feed*
   :*source*
   :*author*
   :resolve-uri
   :get-timestring
   :check-guid-mask
   :parser-loop
   :entry-context
   :strip-parens))

(in-package #:cl-feedparser/parser)

;;; "cl-feedparser" goes here. Hacks and glory await!

(defparameter *version*
  "0.3")

(defvar *base* nil
  "Default value for `current-xml-base'.")

(defvar *text-length-limit* nil
  "Limit for text length, if one is in effect.")

(defun version-string ()
  (fmt "cl-feedparser ~a" *version*))

(deftype universal-time ()
  'integer)

(deftype time-designator ()
  '(or universal-time timestamp))

(deftype entry-mtime ()
  '(or time-designator null))

(deftype uri ()
  '(or string quri:uri))

(deftype id ()
  '(or null uri))

(deftype mask-time ()
  '(or time-designator null string))

(deftype mask ()
  '(or string (cons string mask-time)))

(deftype max-entries ()
  '(or null wholenum))

(deftype uri-protocol ()
  '(or null string))

(deftype klacks-event ()
  '(member :start-document
    :characters
    :processing-instruction
    :comment
    :dtd
    :start-element
    :end-element
    :end-document
    nil))



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
      :created :comments
      ;; mrss
      :media-category
      :media-community
      :media-content
      :media-copyright
      :media-credit
      :media-description
      :media-group
      :media-hash
      :media-keywords
      :media-license
      :media-player
      :media-rating
      :media-restriction
      :media-star-rating
      :media-statistics
      :media-tags
      :media-title
      :media-thumbnail)))

(deftype feedparser-key ()
  `(member ,@*keys*))

(defmacro gethash* (key hash &optional default)
  "Like `gethash', but check (statically) that KEY is a member of
`*keys*'."
  (check-type key feedparser-key)
  `(gethash ,key ,hash ,@(unsplice default)))

(defmacro ensure-gethash* (key hash &optional default)
  "Like `ensure-gethash', but check (statically) that KEY is a member
of `*keys*'."
  (check-type key feedparser-key)
  `(ensure-gethash ,key ,hash ,@(unsplice default)))

(defsubst feed-ref (feed key &optional default)
  (gethash (assure feedparser-key key) feed default))

(define-compiler-macro feed-ref (&whole call feed key &optional default &environment env)
  (cond ((constantp key)
         `(gethash ,(assure feedparser-key key) ,feed ,default))
        ((constantp key env)
         `(gethash (load-time-value (assure feedparser-key ,key))
                   ,feed
                   ,default))
        (t call)))

(defsubst (setf feed-ref) (value feed key)
  (setf (gethash (assure feedparser-key key) feed) value))

(define-compiler-macro (setf feed-ref) (&whole call value feed key &environment env)
  (cond ((constantp key)
         `(setf (gethash ,(assure feedparser-key key)
                         ,feed)
                ,value))
        ((constantp key env)
         `(setf (gethash (load-time-value (assure feedparser-key ,key))
                         ,feed)
                ,value))
        (t call)))



(deftype sanitizer ()
  '(or null fxml.sanitize:mode))

(deftype sanitizer-designator ()
  '(or sanitizer (eql t)))

(defconstructor unsanitized-string
  "A string that has yet to be sanitized."
  (string string))

(deftype feed-string ()
  '(or string unsanitized-string))

(def empty-uri (quri:uri ""))

(defun empty-uri? (uri)
  (etypecase-of uri uri
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
  (~> (fxml.html5:make-html5-sink)
      (make-absolute-uri-handler :base base)
      (fxml.sanitize:wrap-sanitize sanitizer)))

(defun has-markup? (string)
  (scan "[<>&]" string))

(defun sanitize-aux (x sanitizer)
  (cond ((typep x 'fxml.stp:node)
         (let ((sink (make-html-sink :sanitizer sanitizer)))
           (if (typep x 'fxml.stp:document)
               (fxml.stp:serialize x sink)
               (progn
                 (fxml.stp:serialize x sink)
                 (fxml.sax:end-document sink)))))
        ((not (stringp x)) x)
        ((emptyp x) x)
        ((not (has-markup? x)) x)
        (t (etypecase-of sanitizer sanitizer
             (null (unsanitized-string x))
             (fxml.sanitize:mode
              (fxml.html5:serialize-dom
               (parse-html5 x)
               (make-html-sink :sanitizer sanitizer)))))))

(defparameter *content-sanitizer* feed-sanitizer)

(defparameter *title-sanitizer* fxml.sanitize:restricted)

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
         (regex-replace-all "<[^>]*>?" x " "))
        ;; If there are entities, we have to resort to a real parser.
        (t (sanitize-aux x fxml.sanitize:default))))

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
               (setf (feed-ref feed :bozo) t))
             (return-from parser-context
               (values feed parser))))
      (restart-case
          (catch 'parser-done
            (parser-loop (fxml:make-source input)))
        (return-feed ()
          :report "Return whatever we have so far."
          (return-feed :bozo t)))
      (return-feed))))

(defun parser-loop (source &key recursive)
  (let ((*source* source)
        (depth 0))
    (loop for event = (fxml.klacks:peek source) while event do
      (ecase-of klacks-event event
        (:start-element
         (incf depth)
         (multiple-value-bind (ev uri lname)
             (fxml.klacks:consume source)
           (declare (ignore ev))
           (unless *disabled*
             (handle-tag (find-ns uri) lname))))
        (:end-element
         (decf depth)
         (fxml.klacks:consume source)
         (when (and recursive (minusp depth))
           (return)))
        ((:start-document
          :end-document
          :characters
          :processing-instruction
          :comment :dtd)
         (fxml.klacks:consume source))
        ((nil)
         (error "Read past end of source."))))))

(defvar *lispified-ids* (dict))

(defun lispify (id)
  "Convert ID from camel-case to hyphenated form."
  (declare (optimize speed) (string id))
  ;; The theory is that most identifiers will be known.
  (or (gethash id *lispified-ids*)
      ;; A little faster than a string stream.
      (let ((s (make-array 5
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0)))
        (loop for c across id
              if (upper-case-p c)
                do (vector-push-extend #\- s 2)
                   (vector-push c s)
              else do (vector-push-extend c s)
              finally (return (nstring-upcase s))))))

(defun unlispify (id)
  "Convert hyphenated ID to camel-case."
  (let* ((id (string id))
         (id (split-sequence #\- id)))
    (fmt "~(~a~)~{~:(~a~)~}" (first id) (rest id))))

(defgeneric handle-tag (ns lname)
  (:method (ns lname) (declare (ignore ns lname))
    nil)
  (:method (ns (lname string))
    (handle-tag ns (find-keyword (lispify lname)))))

(defmacro defhandler (ns lname &body body)
  (unless (namespace? ns)
    (error "Unknown namespace: ~a" ns))
  (setf (gethash (unlispify lname) *lispified-ids*) lname)
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
          (xml-base (fxml.klacks:current-xml-base *source*)))
      (if (not (emptyp xml-base)) xml-base
          (if (equal (gethash* :proxy feed) "feedburner")
              (gethash* :link feed)
              (if-let (link (find "self"
                                  (gethash* :links feed)
                                  :test #'equal
                                  :key (op (href _ :rel))))
                (href link :href)
                xml-base))))))

(defun current-xml-base ()
  (or *base* (current-xml-base-aux) ""))

(defun get-content (&aux (source *source*))
  (let ((type (fxml.klacks:get-attribute source "type"))
        (content (dict :base (current-xml-base))))
    (if (equal type "xhtml")
        (xhtml-content content)
        (text-content content))))

(defun text-content (content)
  (let* ((attrs (fxml.klacks:list-attributes *source*))
         (string (sanitize-content (get-text))))
    (setf (gethash* :value content) string
          (gethash* :type content)  (guess-type string attrs))
    content))

(defclass absolute-uri-handler (fxml:broadcast-handler)
  ((base :initarg :base :accessor base-of)))

(defun make-absolute-uri-handler (handler &key (base (current-xml-base)))
  (make 'absolute-uri-handler :base base :handlers (list handler)))

(defun resolve-attr (attr)
  (with-accessors ((name fxml.sax:attribute-local-name)
                   (value fxml.sax:attribute-value))
      attr
    (when (or (equal name "href") (equal name "src"))
      (let ((abs-uri (quri:uri (resolve-uri value))))
        (if (empty-uri? abs-uri)
            (setf value "#")
            (setf value (quri:render-uri abs-uri nil)))))))

(defmethod fxml.sax:start-element ((handler absolute-uri-handler) ns lname qname attrs)
  (dolist (attr attrs)
    (resolve-attr attr))
  (call-next-method handler ns lname qname attrs))

(defun get-xhtml-content-value (&aux (source *source*))
  (let* ((handler (make-html-sink :base (current-xml-base)
                                  :sanitizer *content-sanitizer*))
         (string
           (fxml.klacks:serialize-element source handler :document-events t)))
    (etypecase-of sanitizer *content-sanitizer*
      (null (unsanitized-string string))
      (fxml.sanitize:mode string))))

(defun xhtml-content (content &aux (source *source*))
  (fxml.klacks:find-element source "div")
  (let ((value (get-xhtml-content-value)))
    (setf (gethash* :value content) value ;Already sanitized.
          (gethash* :type content)  "text/html"
          (gethash* :base content)  (current-xml-base)))
  content)

(defun attrs-type (attrs)
  (when-let (attr (find "type" attrs :test 'equal :key #'fxml.sax:attribute-local-name))
    (fxml.sax:attribute-value attr)))

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
  (etypecase-of uri uri
    (string (trim-whitespace (remove #\Newline uri)))
    (quri:uri uri)))

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
  (let ((uri (trim-uri uri)))
    (or (resolve-uri/base uri) "")))

(defun limit-text-length (string &aux (limit *text-length-limit*))
  (if (no limit) string
      (ellipsize string limit)))

(defun get-text (&aux (source *source*))
  (let ((text
          (if (not (eql :characters (fxml.klacks:peek source)))
              (get-text-from-elements source)
              (get-text-from-cdata source))))
    (~> text
        trim-whitespace
        limit-text-length)))

(defun get-text-from-cdata (source)
  (with-output-to-string (s)
    (loop while (eql (fxml.klacks:peek source) :characters)
          do (write-string (nth-value 1 (fxml.klacks:consume source)) s))))

(defun get-text-from-elements (source)
  "Handle the case where RSS is used without CDATA."
  (let ((handler (fxml:make-string-sink)))
    (loop while (eql (fxml.klacks:peek source) :start-element) do
      (fxml.klacks:serialize-element source handler))
    (fxml.sax:end-document handler)))

(defun get-text/sanitized ()
  (sanitize-text (get-text)))

(defun urlish? (x)
  "Does X look like a URL?"
  (scan "(?s)^\\s*https?://" x))



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
                             (safe t)
                             ((:text-length-limit *text-length-limit*) nil))
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

The argument TEXT-LENGTH-LIMIT restricts text to a maximum size
whenever text is extracted. Passing this make parsing more robust
against, say, some bozo who accidentally copy-pastes the complete text
of Moby Dick into a blog post.

Sanitizing content can be turned off with SANITIZE-CONTENT (defaults
to T). SANITIZE-TITLES controls sanitizing titles. Sanitizing titles
uses the sanitizer `fxml.sanitize:restricted'. Sanitizing content uses
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
    (setf feed (file-exists-p feed)))
  (let ((bozo nil)
        (*content-sanitizer* (if sanitize-content *content-sanitizer* nil))
        (*title-sanitizer* (if sanitize-titles *title-sanitizer* nil)))
    (handler-bind
        ((error
           (lambda (c) (declare (ignore c))
             (when safe
               ;; If FXML can't repair the damage,
               ;; fall back to plump.
               (repair)
               ;; Last resort: return whatever we've got so far.
               (return-feed)))))
      (handler-bind
          ((fxml:undefined-entity
             (lambda (c)
               (when safe
                 (when-let (exp (plump-dom:translate-entity
                                 (fxml:undefined-entity-name c)))
                   (use-value (string exp)))
                 (continue))))
           (fxml:undeclared-namespace
             (lambda (c)
               (when safe
                 (let* ((prefix (fxml:undeclared-namespace-prefix c))
                        (uri (prefix-uri prefix)))
                   (store-value uri)
                   (continue)))))
           (fxml:well-formedness-violation
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
