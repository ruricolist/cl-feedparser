;;;; cl-feedparser.lisp

(in-package #:cl-feedparser)

;;; "cl-feedparser" goes here. Hacks and glory await!

(defparameter *version*
  "0.1")

(defun version-string ()
  (fmt "cl-feedparser ~a" *version*))



(def empty-uri (puri:parse-uri ""))

(defparameter *allow-protocols*
  '(:http :https :relative))

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

(sax-sanitize:define-sanitize-mode feed-sanitizer
  :elements ("a" "abbr" "acronym" "address" "area" "aside" "audio"
                 "b" "big" "bdo" "blockquote" "br"
                 "caption" "center" "cite" "code" "col" "colgroup"
                 "dd" "del" "details" "dfn" "dir" "div" "dl" "dt"
                 "em"
                 "figcaption" "figure"
                 "h1" "h2" "h3" "h4" "h5" "h6" "hgroup"
                 "i" "img" "ins"
                 "kbd"
                 "li"
                 "m" "map" "mark"
                 "ol"
                 "p" "pre"
                 "q"
                 "rp" "rt" "ruby"
                 "s" "samp" "section" "small" "span" "strike" "strong" "sub" "sup"
                 "table" "tbody" "td" "tfoot" "th" "thead" "time" "tr"
                 "u" "ul" "var"
                 "wbr")

  :remove-elements ("script" "style")

  :allow-data-attributes t

  :attributes ((:all         . ("dir" "lang" "title" "class"))
               ("a"          . ("href"))
               ("blockquote" . ("cite"))
               ("col"        . ("span" "width"))
               ("colgroup"   . ("span" "width"))
               ("del"        . ("cite" "datetime"))
               ("img"        . ("align" "alt" "height" "src" "width"))
               ("ins"        . ("cite" "datetime"))
               ("ol"         . ("start" "reversed" "type"))
               ("p"          . ("align")) ;XXX style?
               ("q"          . ("cite"))
               ("table"      . ("summary" "width"))
               ("td"         . ("abbr" "axis" "colspan" "rowspan" "width"))
               ("th"         . ("abbr" "axis" "colspan" "rowspan" "scope" "width"))
               ("time"       . ("datetime" "pubdate"))
               ("ul"         . ("type")))

  :protocols (("a"           . (("href" . (:ftp :http :https :mailto :relative))))
              ("blockquote"  . (("cite" . (:http :https :relative))))
              ("del"         . (("cite" . (:http :https :relative))))
              ("img"         . (("src"  . (:http :https :relative))))
              ("ins"         . (("cite" . (:http :https :relative))))
              ("q"           . (("cite" . (:http :https :relative))))))

(defun clean (x &optional (sanitizer feed-sanitizer))
  (if (ppcre:scan "[<>&]" x)
      (html5-sax:serialize-dom
       (html5-parser:parse-html5 x)
       (sax-sanitize:wrap-sanitize (html5-sax:make-html5-sink)
                                   sanitizer))
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

(defclass parser ()
  ((max-entries :initarg :max-entries :accessor parser-max-entries)
   (entries-count :initform 0 :accessor parser-entries-count)
   (guid-mask :initarg :guid-mask :accessor parser-guid-mask))
  (:default-initargs :guid-mask nil
                     :max-entries nil))

(def namespaces
  (dict "http://backend.userland.com/rss" nil
        "http://blogs.law.harvard.edu/tech/rss" nil
        "http://purl.org/rss/1.0/" nil
        "http://my.netscape.com/rdf/simple/0.9/" nil
        "http://example.com/newformat#" nil
        "http://example.com/necho" nil
        "http://purl.org/echo/" nil
        "uri/of/echo/namespace#" nil
        "http://purl.org/pie/" nil
        "http://purl.org/atom/ns#" :atom ;atom03
        "http://www.w3.org/2005/Atom" :atom ;atom10
        "http://purl.org/rss/1.0/modules/rss091#" nil
        "http://www.bloglines.com/about/specs/fac-1.0"             :access
        "http://webns.net/mvcb/"                                   :admin
        "http://purl.org/rss/1.0/modules/aggregation/"             :ag
        "http://purl.org/rss/1.0/modules/annotate/"                :annotate
        "http://www.w3.org/2007/app"                               :app
        "http://media.tangent.org/rss/1.0/"                        :audio
        "http://backend.userland.com/blogChannelModule"            :blog-channel
        "http://web.resource.org/cc/"                              :cc
        "http://www.microsoft.com/schemas/rss/core/2005"           :cf
        "http://backend.userland.com/creativeCommonsRssModule"     :creative-commons
        "http://purl.org/rss/1.0/modules/company"                  :co
        "http://purl.org/rss/1.0/modules/content/"                 :content
        "http://conversationsnetwork.org/rssNamespace-1.0/"        :conversations-network
        "http://my.theinfo.org/changed/1.0/rss/"                   :cp
        "http://purl.org/dc/elements/1.1/"                         :dc
        "http://purl.org/dc/terms/"                                :dcterms
        "http://purl.org/rss/1.0/modules/email/"                   :email
        "http://purl.org/rss/1.0/modules/event/"                   :ev
        "http://rssnamespace.org/feedburner/ext/1.0"               :feedburner
        "http://purl.org/syndication/history/1.0"                  :fh
        "http://freshmeat.net/rss/fm/"                             :fm
        "http://xmlns.com/foaf/0.1"                                :foaf
        "http://xmlns.com/foaf/0.1/"                               :foaf
        "http://www.w3.org/2003/01/geo/wgs84_pos#"                 :geo
        "http://www.georss.org/georss"                             :georss
        "http://geourl.org/rss/module/"                            :geourl
        "http://base.google.com/ns/1.0"                            :g
        "http://www.opengis.net/gml"                               :gml
        "http://postneo.com/icbm/"                                 :icbm
        "http://purl.org/rss/1.0/modules/image/"                   :image
        "urn:atom-extension:indexing"                              :indexing
        "http://www.itunes.com/DTDs/PodCast-1.0.dtd"               :itunes
        "http://example.com/DTDs/PodCast-1.0.dtd"                  :itunes
        "http://earth.google.com/kml/2.0"                          :kml20
        "http://earth.google.com/kml/2.1"                          :kml21
        "http://www.opengis.net/kml/2.2"                           :kml22
        "http://purl.org/rss/1.0/modules/link/"                    :l
        "http://www.w3.org/1998/Math/MathML"                       :mathml
        "http://search.yahoo.com/mrss"                             :media
        ;; Version 1.1.2 of the Media RSS spec added the trailing slash on
        ;; the namespace
        "http://search.yahoo.com/mrss/"                            :media
        "http://openid.net/xmlns/1.0"                              :openid
        "http://a9.com/-/spec/opensearchrss/1.0/"                  :opensearch10
        "http://a9.com/-/spec/opensearch/1.1/"                     :opensearch
        "http://www.opml.org/spec2"                                :opml
        "http://madskills.com/public/xml/rss/module/pingback/"     :pingback
        "http://prismstandard.org/namespaces/1.2/basic/"           :prism
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#"              :rdf
        "http://www.w3.org/2000/01/rdf-schema#"                    :rdfs
        "http://purl.org/rss/1.0/modules/reference/"               :ref
        "http://purl.org/rss/1.0/modules/richequiv/"               :reqv
        "http://purl.org/rss/1.0/modules/search/"                  :search
        "http://purl.org/rss/1.0/modules/slash/"                   :slash
        "http://schemas.xmlsoap.org/soap/envelope/"                :soap
        "http://purl.org/rss/1.0/modules/servicestatus/"           :ss
        "http://hacks.benhammersley.com/rss/streaming/"            :str
        "http://purl.org/rss/1.0/modules/subscription/"            :sub
        "http://www.w3.org/2000/svg"                               :svg
        "http://feedsync.org/2007/feedsync"                        :sx
        "http://purl.org/rss/1.0/modules/syndication/"             :sy
        "http://schemas.pocketsoap.com/rss/myDescModule/"          :szf
        "http://purl.org/rss/1.0/modules/taxonomy/"                :taxo
        "http://purl.org/rss/1.0/modules/threading/"               :thr
        "http://purl.org/syndication/thread/1.0"                   :thr
        "http://purl.org/rss/1.0/modules/textinput/"               :ti
        "http://madskills.com/public/xml/rss/module/trackback/"    :trackback
        "http://wellformedweb.org/commentAPI/"                     :wfw
        "http://purl.org/rss/1.0/modules/wiki/"                    :wiki
        "http://www.w3.org/1999/xhtml"                             :xhtml
        "http://www.w3.org/1999/xlink"                             :xlink
        "http://www.w3.org/XML/1998/namespace"                     :xml
        "xri://$xrd*($v*2.0)"                                      :xrd
        "xri://$xrds"                                              :xrds
        "http://podlove.org/simple-chapters"                       :psc)
  "Table from namespace to prefix.
Collected from the source of feedparser.py and the list at the W3C
feed validator.")

(defun symbol->camel-case (symbol)
  "Convert SYMBOL to a camel-cased string."
  (let ((string (copy-seq (string symbol))))
    ;; Don't worry about speed; this will be memoized.
    (with-output-to-string (s)
      (loop for c1 = #\Space then c2
            for c2 across string
            do (unless (eql c2 #\-)
                 (if (eql c1 #\-)
                     (write-char (char-upcase c2) s)
                     (write-char (char-downcase c2) s)))))))

(defun xmlify (id)
  "Convert ID to camel case."
  (ensure (get id 'camel-case)
    (symbol->camel-case id)))

(def namespace-prefixes
  (let ((map (fset:map)))
    (maphash (lambda (k v)
               (when k
                 (setf map (fset:with map (xmlify v) k))))
             namespaces)
    map)
  "Table from prefix to namespace.")

(defun find-ns (uri)
  (gethash uri namespaces))

(defun parse-feed-aux (input &key max-entries guid-mask)
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
             (nreversef (gethash :entries feed))
             (when bozo
               (setf (gethash :bozo feed) t))
             (return-from parse-feed-aux
               (values feed parser))))
      (restart-case
          (catch 'done
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

(defgeneric handle-tag (ns lname)
  (:method (ns lname) (declare (ignore ns lname))
    nil)
  (:method (ns (lname string))
    (handle-tag ns (find-keyword (lispify lname)))))

(defmacro defhandler (ns lname &body body)
  (with-gensyms (gns glname)
    `(defmethod handle-tag ((,gns (eql ,ns)) (,glname (eql ,lname)))
       ,@body)))

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
      (ensure2 (gethash :title (or *entry* *feed*))
        title))))

(defhandler :atom :tagline
  (handle-subtitle))

(defhandler :atom :subtitle
  (handle-subtitle))

(defhandler :itunes :subtitle
  (handle-subtitle))

(defhandler :atom :info
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash :info *feed*) text)))

(defhandler :feedburner :browser-friendly
  (handle-tag :atom :info))

(defhandler :atom :rights
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash :rights *feed*) text)))

(defhandler :atom :copyright
  (handle-tag :atom :rights))

(defhandler :dc :rights
  (handle-tag :atom :rights))

(defhandler nil :copyright
  (handle-tag :atom :rights))

(defun handle-subtitle ()
  (when-let (text (get-text-safe))
    (setf (gethash :subtitle *feed*) text)))

(defmethod handle-tag ((ns null) (lname (eql :link)))
  (when-let (string (get-text))
    (setf (gethash :link (or *entry* *feed*))
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
      (setf (gethash :link (or *entry* *feed*))
            href))

    (dict* link
           :rel rel
           :type type
           :href href
           :title title)

    (push link (gethash :links (or *entry* *feed*)))))

(defhandler :atom :name
  (let ((name (get-text-safe sax-sanitize:default)))
    (setf (gethash :author (or *entry* *feed*)) name
          (gethash :name *author*) name)))

(defhandler :atom :email
  (setf (gethash :email *author*)
        (get-text-safe sax-sanitize:default)))

(defhandler :atom :uri
  (setf (gethash :uri *author*)
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
          (setf (gethash :email *author*) (subseq author 0 space))
          (when space
            (ensure creator (strip-parens (subseq author space)))))
        (ensure creator author))

    (let ((name (strip-parens creator)))
      (setf (gethash :name *author*) name
            (gethash :author (or *entry* *feed*)) name))))

(defun strip-parens (s)
  (when (stringp s)
    (string-trim " ()" s)))

(defmethod handle-tag ((ns null) (lname (eql :language)))
  (setf (gethash :language *feed*) (get-text)))

(defmethod handle-tag ((ns (eql :dc)) (lname (eql :language)))
  (setf (gethash :language *feed*) (get-text)))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :feed)))
  (block nil
    (klacks:map-attributes
     (lambda (ns lname qname value dtdp)
       (declare (ignore ns lname dtdp))
       (when (equal qname "xml:lang")
         (setf (gethash :language *feed*) value)
         (return)))
     *source*)))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :icon)))
  (setf (gethash :icon *feed*)
        (resolve-uri (get-text))))

(defmethod handle-tag ((ns null) (lname (eql :description)))
  (get-summary))

(defun get-summary ()
  (if-let (entry *entry*)
    (when-let (content (get-content))
      (setf (gethash :summary entry) (gethash :value content)
            (gethash :summary-detail entry) content))

    (setf (gethash :subtitle *feed*)
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
    (setf (values (gethash :published target)
                  (gethash :published-parsed target))
          (get-timestring))))

(defun read-mtime ()
  (let ((target (or *entry* *feed*)))
    (setf (values (gethash :updated target)
                  (gethash :updated-parsed target))
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
    (setf (gethash :ttl *feed*) string)))

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
                       (null (@ entry :href))))
          (setf (href entry :link) (resolve-uri id)))))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :id)))
  (let ((id (get-text)))
    (check-guid-mask id)
    (setf (gethash :id (or *entry* *feed*)) id)))

(defmethod handle-tag ((ns (eql :dc)) (lname (eql :description)))
  (get-summary))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :summary)))
  (get-summary))

(defmethod handle-tag ((ns (eql :feedburner)) (lname (eql :orig-link)))
  ;; Eg. 3QD.
  (when *entry*
    (setf (gethash :link *entry*)
          (resolve-uri (get-text)))))

(defmethod handle-tag ((ns (eql :content)) (lname (eql :encoded)))
  (get-entry-content))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :content)))
  (get-entry-content))

(defmethod handle-tag ((ns null) (lname (eql :body)))
  (get-entry-content))

(defmethod handle-tag ((ns null) (lname (eql :fullitem)))
  (get-entry-content))

(defmethod handle-tag ((ns (eql :xhtml)) (lname (eql :body)))
  (get-entry-content))

(defmethod handle-tag ((ns (eql :dcterms)) (lname (eql :modified)))
  (setf (values (gethash :updated *entry*)
                (gethash :updated-parsed *entry*))
        (get-timestring)))

(defun get-entry-content ()
  (push (get-content) (gethash :content *entry*)))

(defun get-content (&aux (source *source*))
  (let ((type (klacks:get-attribute source "type"))
        (content (dict)))
    (setf (gethash :base content) (klacks:current-xml-base source))
    (if (equal type "xhtml")
        (get-xhtml-content content)
        (get-text-content content))
    content))

(defun get-text-content (content)
  (let* ((attrs (klacks:list-attributes *source*))
         (string (sanitize-content (get-text))))
    (setf (gethash :value content) string
          (gethash :type content)  (guess-type string attrs))
    content))

(defclass absolute-uri-handler (cxml:broadcast-handler)
  ((base :initarg :base :accessor base-of)))

(defmethod sax:start-element ((handler absolute-uri-handler) ns lname qname attrs)
  (flet ((make-absolute (attr)
           (let ((value (sax:attribute-value attr)))
             (when value
               (setf (sax:attribute-value attr)
                     (princ-to-string (puri:merge-uris value (base-of handler))))))))
    (let ((attrs
            (mapc (lambda (attr)
                    (let ((name (sax:attribute-local-name attr)))
                      (when (or (equal name "href") (equal name "src"))
                        (make-absolute attr))))
                  attrs)))
      (call-next-method handler ns lname qname attrs))))

(defun get-xhtml-content (content &aux (source *source*))
  (klacks:find-element source "div")
  (let* ((can-sanitize/sax (eql *content-sanitizer* default-sanitizer))
         (value
           (let ((handler (make-instance 'absolute-uri-handler
                                         :handlers (list (html5-sax:make-html5-sink))
                                         :base (klacks:current-xml-base source))))
             (when can-sanitize/sax
               (setf handler (sax-sanitize:wrap-sanitize handler feed-sanitizer)))
             (klacks:serialize-element source handler :document-events t))))
    (setf (gethash :value content)
          (if can-sanitize/sax
              value
              (sanitize-content value))
          (gethash :type content)  "text/html"
          (gethash :base content)  (klacks:current-xml-base source))
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
      (setf (gethash :id entry) id))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :entry)))
  (handle-entry))

(defun ensure-entry-id (entry)
  "Substitute link for ID if there is none."
  (ensure2 (gethash :id entry)
    (when-let (id (gethash :link entry))
      (let ((id (princ-to-string id)))
        (check-guid-mask id)
        id))))

(defun handle-entry ()
  (let ((count (finc (parser-entries-count *parser*)))
        (max-entries (parser-max-entries *parser*)))
    (if (and max-entries (= max-entries count))
        (throw 'done nil)
        (lret ((*author* (dict))
               (*entry* (dict)))

          (setf (gethash :author-detail *entry*) *author*)

          (let ((*disabled* *disabled*))
            (parser-loop *source* :recursive t)
            ;; Ensure an ID.
            (ensure-entry-id *entry*)
            (unless *disabled*
              (push *entry* (gethash :entries *feed*))))

          (setf (gethash :author *entry*)
                (gethash :name *author*))))))

(defun resolve-uri (uri)
  (when (stringp uri)
    (setf uri (trim-whitespace (remove #\Newline uri))))
  (let ((base (klacks:current-xml-base *source*)))
    (or (ignoring puri:uri-parse-error
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



(defun parse-feed (feed &rest args
                        &key max-entries
                             (sanitize-content t)
                             (sanitize-titles t)
                             guid-mask
                             repaired)
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
  (let ((feed
          (restart-case
              (let ((*content-sanitizer*
                      (if (not sanitize-content)
                          #'make-unsanitized-string
                          *content-sanitizer*))
                    (*title-sanitizer*
                      (if (not sanitize-titles)
                          #'make-unsanitized-string
                          *title-sanitizer*)))
                (dict* (parse-feed-aux feed
                                       :max-entries max-entries
                                       :guid-mask guid-mask)
                       :bozo nil))
            (repair ()
              :report "Try to repair the XML document and try again."
              :test (lambda (c) (declare (ignore c))
                      (not repaired))
              (let* ((repaired (markup-grinder:grind feed
                                                     (cxml:make-string-sink :indentation nil)
                                                     :extra-namespaces namespace-prefixes)))
                (apply #'parse-feed repaired :repaired t args))))))
    feed))

(defvar *parse-safe* t
  "For testing purposes, you can bind this to prevent
  `parse-feed-safe' from falling back to using markup-grinder.")

(defun parse-feed-safe (feed &rest args &key &allow-other-keys)
  "Try to parse FEED.
If FEED is invalid XML, try to repair it.
If FEED cannot be repaired, return a best-faith attempt."
  (let (e)
    (labels ((expand-html-entity (name)
               ;; XXX
               (sgml::find-named-entity chtml::*html-dtd* name))
             (parse-feed-safe ()
               (cond (*parse-safe*
                      ;; If CXML can't repair the damage, fall back to markup-grinder.
                      (handler-bind ((error
                                       (lambda (c) (declare (ignore c))
                                         (maybe-invoke-restart 'repair)
                                         (maybe-invoke-restart 'return-feed))))
                        ;; Try to repair the damage with our forked
                        ;; CXML.
                        (handler-bind ((cxml:undefined-entity
                                         (lambda (c)
                                           (when-let (match (expand-html-entity (cxml:undefined-entity-name c)))
                                             (use-value match))
                                           (continue)))
                                       (cxml:undeclared-namespace
                                         (lambda (c)
                                           (let* ((prefix (cxml:undeclared-namespace-prefix c))
                                                  (uri (fset:lookup namespace-prefixes prefix)))
                                             (store-value uri)
                                             (continue))))
                                       (cxml:well-formedness-violation #'continue))
                          ;; Set the bozo bit.
                          (handler-bind ((error (lambda (c) (setf e c))))
                            (apply #'parse-feed feed args)))))
                     (t (apply #'parse-feed feed args)))))
      (let ((feed (parse-feed-safe)))
        (if e
            (values (dict* feed :bozo t) e)
            (values feed nil))))))
