;;;; cl-feedparser.lisp

(in-package #:cl-feedparser)

;;; "cl-feedparser" goes here. Hacks and glory await!

(defparameter *version*
  "0.1")

(defun version-string ()
  (fmt "cl-feedparser ~a" *version*))



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
      :author-detail :links)))

(defmacro gethash* (key hash &optional default)
  "Like `gethash', but check (statically) that KEY is a member of
`*keys*'."
  (unless (member key *keys*)
    (error "Unknown key: ~a" key))
  `(gethash ,key ,hash ,@(unsplice default)))



(def empty-uri (puri:parse-uri ""))

(defparameter *allow-protocols*
  '(:http :https :relative)
  "List of the allowed protocols.")

#+ () (adt:defdata string
  (sanitized-string string)
  (unsanitized-string string))

(defstruct (unsanitized-string (:constructor make-unsanitized-string (string)))
  "Wrapper for an unsanitized string."
  (string "" :type string))

(defun string+ (&rest strings)
  "Concatenate STRINGS, ensuring that if any are unsanitized, the
result is an unsanitized string."
  (let* ((unsanitized nil)
         (s (with-output-to-string (s)
              (dolist (string strings)
                (etypecase string
                  (string (write-string string s))
                  (unsanitized-string
                   (setf unsanitized t)
                   (write-string (unsanitized-string-string string) s)))))))
    (if unsanitized
        (make-unsanitized-string s)
        s)))

(defun clean (x &optional (sanitizer feed-sanitizer))
  (if (ppcre:scan "[<>&]" x)
      (html5-sax:serialize-dom
       (html5-parser:parse-html5 x)
       (make-absolute-uri-handler
        (sax-sanitize:wrap-sanitize
         (html5-sax:make-html5-sink)
         sanitizer)))
      x))

(def default-sanitizer
  (lambda (x)
    (when x
      (clean x))))

(def default-title-sanitizer
  (lambda (x)
    (when x
      (clean x sax-sanitize:restricted))))

(declaim (type function *content-sanitizer* *title-sanitizer*))

(defparameter *content-sanitizer* default-sanitizer)

(defparameter *title-sanitizer* default-title-sanitizer)

(defun sanitize-content (x)
  (funcall *content-sanitizer* x))

(defun sanitize-title (x)
  (funcall *title-sanitizer* x))



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
  ((max-entries :initarg :max-entries :accessor parser-max-entries)
   (entries-count :initform 0 :accessor parser-entries-count)
   (guid-mask :initarg :guid-mask :accessor parser-guid-mask))
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
    (loop (let ((event (klacks:peek source)))
            (unless event
              (return))
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
              (t (klacks:consume source)))))))

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

(defhandler nil :title
  (handle-title))

(defhandler :atom :title
  (handle-title))

(defhandler :dc :title
  (handle-title))

(defhandler :rdf :title
  (handle-title))

(defun handle-title ()
  (when-let (text (get-text-safe))
    (let ((title (trim-whitespace text)))
      ;; Cf. Grantland.
      (ensure2 (gethash* :title (or *entry* *feed*))
        title))))

(defhandler :atom :tagline
  (handle-subtitle))

(defhandler :atom :subtitle
  (handle-subtitle))

(defhandler :itunes :subtitle
  (handle-subtitle))

(defhandler :atom :info
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash* :info *feed*) text)))

(defhandler :feedburner :browser-friendly
  (handle-tag :atom :info))

(defhandler :atom :rights
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash* :rights *feed*) text)))

(defhandler :atom :copyright
  (handle-tag :atom :rights))

(defhandler :dc :rights
  (handle-tag :atom :rights))

(defhandler nil :copyright
  (handle-tag :atom :rights))

(defun handle-subtitle ()
  (when-let (text (get-text-safe))
    (setf (gethash* :subtitle *feed*) text)))

(defmethod handle-tag ((ns (eql :feedburner)) lname)
  "The feed is from Feedburner."
  (declare (ignore lname))
  (ensure2 (gethash* :proxy *feed*) ns))

(defmethod handle-tag ((ns null) (lname (eql :link)))
  (when-let (string (get-text))
    (setf (gethash* :link (or *entry* *feed*))
          (resolve-uri string))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :link)))
  (let* ((source *source*)
         (rel (klacks:get-attribute source "rel"))
         (type (klacks:get-attribute source "type"))
         (href (klacks:get-attribute source "href"))
         (title (klacks:get-attribute source "title"))
         (link (make-hash-table)))

    ;; E.g. Quora.
    (when (or (equal href "None") (equal href "/None"))
      (return-from handle-tag))

    (when href
      (setf href (resolve-uri href)))

    (when (and href (or (not rel) (equal rel "alternate")))
      (setf (gethash* :link (or *entry* *feed*))
            href))

    (setf (gethash* :rel link) rel
          (gethash* :type link) type
          (gethash* :href link) href
          (gethash* :title link) title)

    (push link (gethash* :links (or *entry* *feed*)))))

(defhandler :atom :name
  (let ((name (get-text-safe sax-sanitize:default)))
    (setf (gethash* :author (or *entry* *feed*)) name
          (gethash* :name *author*) name)))

(defhandler :atom :email
  (setf (gethash* :email *author*)
        (get-text-safe sax-sanitize:default)))

(defhandler :atom :uri
  (setf (gethash* :uri *author*)
        (resolve-uri (get-text))))

(defhandler :dc :creator
  (get-author))

(defhandler :itunes :author
  (get-author))

(defhandler nil :author
  (get-author))

(defun get-author ()
  (let* ((author (get-text-safe sax-sanitize:default))
         (email? (find #\@ author))
         creator)

    (if email?
        (let ((space (position #\Space author)))
          (setf (gethash* :email *author*) (subseq author 0 space))
          (when space
            (ensure creator (strip-parens (subseq author space)))))
        (ensure creator author))

    (let ((name (strip-parens creator)))
      (setf (gethash* :name *author*) name
            (gethash* :author (or *entry* *feed*)) name))))

(defun strip-parens (s)
  (when (stringp s)
    (string-trim " ()" s)))

(defmethod handle-tag ((ns null) (lname (eql :language)))
  (setf (gethash* :language *feed*) (get-text)))

(defmethod handle-tag ((ns (eql :dc)) (lname (eql :language)))
  (setf (gethash* :language *feed*) (get-text)))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :feed)))
  (block nil
    (klacks:map-attributes
     (lambda (ns lname qname value dtdp)
       (declare (ignore ns lname dtdp))
       (when (equal qname "xml:lang")
         (setf (gethash* :language *feed*) value)
         (return)))
     *source*)))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :icon)))
  (setf (gethash* :icon *feed*)
        (resolve-uri (get-text))))

(defmethod handle-tag ((ns null) (lname (eql :description)))
  (get-summary))

(defun get-summary ()
  (if-let (entry *entry*)
    (when-let (content (get-content))
      (setf (gethash* :summary entry) (gethash* :value content)
            (gethash* :summary-detail entry) content))

    (setf (gethash* :subtitle *feed*)
          (sanitize-title (get-text)))))

(defmethod handle-tag ((ns null) (lname (eql :pub-date)))
  (read-pubdate))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :published)))
  (read-pubdate))

(defmethod handle-tag ((ns null) (lname (eql :last-build-date)))
  (unless *entry*
    (read-pubdate)))

(defmethod handle-tag ((ns (eql :dc)) (lname (eql :date)))
  (read-mtime))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :modified)))
  (read-mtime))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :updated)))
  (read-mtime))

(defun read-pubdate ()
  (let ((target (or *entry* *feed*)))
    (setf (values (gethash* :published target)
                  (gethash* :published-parsed target))
          (get-timestring))))

(defun read-mtime ()
  (let ((target (or *entry* *feed*)))
    (setf (values (gethash* :updated target)
                  (gethash* :updated-parsed target))
          (get-timestring))))

(defun get-timestring ()
  (when-let (string (get-text-safe))
    (values string (parse-timestring string))))

(defun parse-timestring (timestring)
  (or (net.telent.date:parse-time timestring)
      (ignoring local-time::invalid-timestring ;XXX
        (local-time:timestamp-to-universal
         (local-time:parse-timestring timestring)))))

(defmethod handle-tag ((ns null) (lname (eql :ttl)))
  (when-let (string (get-text))
    (setf (gethash* :ttl *feed*) string)))

(defun check-guid-mask (id)
  (when (and *entry* id (find id (parser-guid-mask *parser*) :test #'equal))
    (setf *disabled* t)))

(defmethod handle-tag ((ns null) (lname (eql :guid)))
  ;; todo rdf:about
  (when-let (entry *entry*)
    (let ((permalinkp
            (equal "true" (klacks:get-attribute *source* "isPermaLink")))
          (id (get-text)))
      (when id
        (check-guid-mask id)
        (setf (href entry :id) id)
        (when (or permalinkp
                  ;; Use GUID as a fallback link.
                  (and (urlish? id)
                       (null (gethash* :href entry))))
          (setf (gethash* :link entry) (resolve-uri id)))))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :id)))
  (let ((id (get-text)))
    (check-guid-mask id)
    (setf (gethash* :id (or *entry* *feed*)) id)))

(defhandler :dc :description
  (get-summary))

(defhandler :atom :summary
  (get-summary))

(defhandler :feedburner :orig-link
  ;; Eg. 3QD.
  (when *entry*
    (setf (gethash* :link *entry*)
          (resolve-uri (get-text)))))

(defhandler :content :encoded
  (get-entry-content))

(defhandler :atom :content
  (get-entry-content))

(defhandler nil :body
  (get-entry-content))

(defhandler nil :fullitem
  (get-entry-content))

(defhandler :xhtml :body
  (get-entry-content))

(defhandler :dcterms :modified
  (setf (values (gethash* :updated *entry*)
                (gethash* :updated-parsed *entry*))
        (get-timestring)))

(defun get-entry-content ()
  (push (get-content) (gethash* :content *entry*)))

(defun current-xml-base-aux (&aux (feed *feed*))
  "Hack to work around the fact that Feedburner wipes out the xml:base
attribute: if there is no current XML base, use the :link property of the feed."
  (let ((xml-base (klacks:current-xml-base *source*)))
    (if (not (emptyp xml-base))
        xml-base
        (let ((proxy (gethash* :proxy feed)))
          (when (eql proxy :feedburner)
            (gethash* :link feed))))))

(defun current-xml-base ()
  (or (current-xml-base-aux) ""))

(defun get-content (&aux (source *source*))
  (let ((type (klacks:get-attribute source "type"))
        (content (dict)))
    (setf (gethash* :base content) (current-xml-base))
    (if (equal type "xhtml")
        (get-xhtml-content content)
        (get-text-content content))
    content))

(defun get-text-content (content)
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
      (let ((abs-uri (resolve-uri value)))
        (if (eq abs-uri empty-uri)
            (setf value "#")
            (setf value (princ-to-string abs-uri)))))))

(defmethod sax:start-element ((handler absolute-uri-handler) ns lname qname attrs)
  (dolist (attr attrs)
    (resolve-attr attr))
  (call-next-method handler ns lname qname attrs))

(defun get-xhtml-content (content &aux (source *source*))
  (klacks:find-element source "div")
  (let* ((can-sanitize/sax (eql *content-sanitizer* default-sanitizer))
         (value
           (let ((handler (make-instance 'absolute-uri-handler
                                         :handlers (list (html5-sax:make-html5-sink))
                                         :base (current-xml-base))))
             (when can-sanitize/sax
               (setf handler (sax-sanitize:wrap-sanitize handler feed-sanitizer)))
             (klacks:serialize-element source handler :document-events t))))
    (setf (gethash* :value content)
          (if can-sanitize/sax
              value
              (sanitize-content value))
          (gethash* :type content)  "text/html"
          (gethash* :base content)  (current-xml-base))
    content))

(defun guess-type (value attrs)
  (when-let (attr (find "type" attrs :test 'equal :key #'sax:attribute-local-name))
    (let ((attr (sax:attribute-value attr)))
      (cond ((find #\/ attr) attr)
            ((string= attr "html") "text/html")
            ((find-if (lambda (c)
                        (or (eql c #\<)
                            (eql c #\>)))
                      value)
             "text/html")
            (t "text/plain")))))

(defmethod handle-tag ((ns null) (lname (eql :item)))
  (handle-entry))

(defmethod handle-tag ((ns (eql :rdf)) (lname (eql :item)))
  ;; todo rdf:about
  ;; TODO use guid-mask
  (let ((entry (handle-entry)))
    (when-let (id (klacks:get-attribute *source* "about"))
      (check-guid-mask id)
      (setf (gethash* :id entry) id))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :entry)))
  (handle-entry))

(defun ensure-entry-id (entry)
  "Substitute link for ID if there is none."
  (ensure2 (gethash* :id entry)
    (when-let (id (gethash* :link entry))
      (let ((id (princ-to-string id)))
        (check-guid-mask id)
        id))))

(defun handle-entry ()
  (let ((count (finc (parser-entries-count *parser*)))
        (max-entries (parser-max-entries *parser*)))
    (if (and max-entries (= max-entries count))
        (throw 'parser-done nil)
        (lret ((*author* (dict))
               (*entry* (dict)))

          (setf (gethash* :author-detail *entry*) *author*)

          (let ((*disabled* *disabled*))
            (parser-loop *source* :recursive t)
            ;; Ensure an ID.
            (ensure-entry-id *entry*)
            (unless *disabled*
              (push *entry* (gethash* :entries *feed*))))

          (setf (gethash* :author *entry*)
                (gethash* :name *author*))))))

(defun resolve-uri (uri)
  (when (stringp uri)
    (setf uri (trim-whitespace (remove #\Newline uri))))
  (let ((base (current-xml-base)))
    ;; Not that (ignoring puri:uri-parse-error ...) won't do it; e.g.
    ;; (parse-uri "1!USER@FTP.JONATHANKINLAY.COM" signals
    ;; `simple-error'.
    (or (ignore-errors
          (let* ((uri (puri:merge-uris uri base))
                 (protocol (or (puri:uri-scheme uri)
                               :relative)))
            (when (member protocol *allow-protocols*)
              uri)))
        empty-uri)))

(defun get-text (&aux (source *source*))
  (if (not (eql (klacks:peek source) :characters))
      ""
      (with-output-to-string (s)
        (loop while (eql (klacks:peek source) :characters)
              do (write-string (nth-value 1 (klacks:consume source)) s)))))

(defun get-text-safe (&optional (sanitizer sax-sanitize:restricted))
  (let ((text (get-text)))
    (if (emptyp text)
        text
        (clean text sanitizer))))

(defun urlish? (x)
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
successively, as though MAX-ENTRIES were taken and then filtered by
GUID. (Actually, entries with recognized GUIDs are never even parsed.)

Consider a feed with thousands of entries (they do exist): if the mask
were applied first, you would get another set of older entries each
time you called PARSE-FEED.

Sanitizing content can be turned off with SANITIZE-CONTENT (defaults
to T). SANITIZE-TITLES controls sanitizing titles."
  (when (pathnamep feed)
    (setf feed (fad:file-exists-p feed)))
  (let ((bozo nil)
        (*content-sanitizer*
          (if sanitize-content
              *content-sanitizer*
              #'make-unsanitized-string))
        (*title-sanitizer*
          (if sanitize-titles
              *title-sanitizer*
              #'make-unsanitized-string)))
    (handler-bind
        ((error
           (lambda (c) (declare (ignore c))
             (when safe
               ;; If CXML can't repair the damage,
               ;; fall back to markup-grinder.
               (repair)
               ;; Last resort: return whatever we've got so far. Bound
               ;; in `parse-feed-aux`.
               (return-feed)))))
      (handler-bind
          ((cxml:undefined-entity
             (lambda (c)
               (when safe
                 (when-let (match (markup-grinder:expand-entity
                                   (cxml:undefined-entity-name c)))
                   (use-value match))
                 (continue))))
           (cxml:undeclared-namespace
             (lambda (c)
               (when safe
                 (let* ((prefix (cxml:undeclared-namespace-prefix c))
                        (uri (fset:lookup namespace-map prefix)))
                   (store-value uri)
                   (continue)))))
           (cxml:well-formedness-violation
             (lambda (c)
               (when safe
                 (continue c)))))
        (flet ((parse (feed)
                 ;; Always set the bozo bit before other handlers
                 ;; can take effect.
                 (handler-bind ((error (lambda (c) (push c bozo))))
                   (values
                    (dict* (parser-context feed
                                           :max-entries max-entries
                                           :guid-mask guid-mask)
                           :bozo bozo)
                    bozo))))
          (restart-case
              (parse feed)
            (repair ()
              :report "Try to repair the XML document and try again."
              (parse
               (markup-grinder:grind
                feed
                (cxml:make-string-sink :indentation nil)
                :extra-namespaces namespace-map)))))))))
