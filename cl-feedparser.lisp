;;;; cl-feedparser.lisp

(in-package #:cl-feedparser)

;;; "cl-feedparser" goes here. Hacks and glory await!

;; NB: Returned hash tables must always be EQUAL tables for use with
;; Funes.

(eval-and-compile
  (defvar *base*
    #.(fad:pathname-directory-pathname (or *compile-file-truename* *load-truename*))))

(defparameter *version*
  "0.1")

(defparameter *feedparser-version*
  "5.1.3")

(defparameter *feedparse-script*
  (merge-pathnames #p"parse.py" *base*))

(deftype limit ()
  '(or null array-index))

(defun name->keyword (name)
  (make-keyword (upcase (join (words (string-downcase name)) #\-))))

(defun parse-json (string)
  (ignoring end-of-file
    (yason:parse string :object-key-fn #'name->keyword)))

(defmethod feedparse.py ((string string))
  (with-input-from-string (in string)
    (feedparse.py in)))

(defmethod feedparse.py ((pathname pathname))
  (with-open-file (in pathname
                      :direction :input
                      :element-type 'character)
    (feedparse.py in)))

(defmethod feedparse.py ((in stream))
  ;; Parsing directly from the stream is slightly slower but conses a
  ;; third as much.
  (let* ((proc
           (sb-ext:run-program
            *feedparse-script*
            '()
            :wait nil
            :pty nil
            :input :stream
            :output :stream))
         (input (sb-ext:process-input proc))
         (output (sb-ext:process-output proc)))
    (copy-stream in input)
    (close input)
    (prog1 (parse-json output)
      (close output))))



(sanitize:define-sanitize-mode +feed+
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

  :attributes ((:all         . ("dir" "lang" "title" "class"))
               ("a"          . ("href"))
               ("blockquote" . ("cite"))
               ("col"        . ("span" "width"))
               ("colgroup"   . ("span" "width"))
               ("del"        . ("cite" "datetime"))
               ("img"        . ("align" "alt" "height" "src" "width"))
               ("ins"        . ("cite" "datetime"))
               ("ol"         . ("start" "reversed" "type"))
               ("p"          . ("align" "style")) ;XXX
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

(declaim (function *content-sanitizer* *title-sanitizer*))

(defvar *content-sanitizer*
  (lambda (x)
    (when x
      (sanitize:clean x +feed+))))

(defparameter *title-sanitizer* *content-sanitizer*)

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
(defvar *path*)
(defvar *disabled*)

(defclass parser ()
  ((max-entries :initarg :max-entries :accessor parser-max-entries)
   (guid-mask   :initarg :guid-mask   :accessor parser-guid-mask))
  (:default-initargs :guid-mask nil :max-entries nil))

(defparameter *namespaces*
  (alist-hash-table
   '(("http://backend.userland.com/rss" . nil)
     ("http://blogs.law.harvard.edu/tech/rss" . nil)
     ("http://purl.org/rss/1.0/" . nil)
     ("http://my.netscape.com/rdf/simple/0.9/" . nil)
     ("http://example.com/newformat#" . nil)
     ("http://example.com/necho" . nil)
     ("http://purl.org/echo/" . nil)
     ("uri/of/echo/namespace#" . nil)
     ("http://purl.org/pie/" . nil)
     ("http://purl.org/atom/ns#" . :atom)    ;atom03
     ("http://www.w3.org/2005/Atom" . :atom) ;atom10
     ("http://purl.org/rss/1.0/modules/rss091#" . nil)
     ("http://webns.net/mvcb/" .                                :admin)
     ("http://purl.org/rss/1.0/modules/aggregation/" .          :ag)
     ("http://purl.org/rss/1.0/modules/annotate/" .             :annotate)
     ("http://media.tangent.org/rss/1.0/" .                     :audio)
     ("http://backend.userland.com/blogChannelModule" .         :blog-channel)
     ("http://web.resource.org/cc/" .                           :cc)
     ("http://backend.userland.com/creativeCommonsRssModule" .  :creative-commons)
     ("http://purl.org/rss/1.0/modules/company" .               :co)
     ("http://purl.org/rss/1.0/modules/content/" .              :content)
     ("http://my.theinfo.org/changed/1.0/rss/" .                :cp)
     ("http://purl.org/dc/elements/1.1/" .                      :dc)
     ("http://purl.org/dc/terms/" .                             :dcterms)
     ("http://purl.org/rss/1.0/modules/email/" .                :email)
     ("http://purl.org/rss/1.0/modules/event/" .                :ev)
     ("http://rssnamespace.org/feedburner/ext/1.0" .            :feedburner)
     ("http://freshmeat.net/rss/fm/" .                          :fm)
     ("http://xmlns.com/foaf/0.1/" .                            :foaf)
     ("http://www.w3.org/2003/01/geo/wgs84_pos#" .              :geo)
     ("http://www.georss.org/georss" .                          :georss)
     ("http://www.opengis.net/gml" .                            :gml)
     ("http://postneo.com/icbm/" .                              :icbm)
     ("http://purl.org/rss/1.0/modules/image/" .                :image)
     ("http://www.itunes.com/DTDs/PodCast-1.0.dtd" .            :itunes)
     ("http://example.com/DTDs/PodCast-1.0.dtd" .               :itunes)
     ("http://purl.org/rss/1.0/modules/link/" .                 :l)
     ("http://search.yahoo.com/mrss" .                          :media)
     ;; Version 1.1.2 of the Media RSS spec added the trailing slash on
     ;; the namespace
     ("http://search.yahoo.com/mrss/" .                         :media)
     ("http://madskills.com/public/xml/rss/module/pingback/" .  :pingback)
     ("http://prismstandard.org/namespaces/1.2/basic/" .        :prism)
     ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" .           :rdf)
     ("http://www.w3.org/2000/01/rdf-schema#" .                 :rdfs)
     ("http://purl.org/rss/1.0/modules/reference/" .            :ref)
     ("http://purl.org/rss/1.0/modules/richequiv/" .            :reqv)
     ("http://purl.org/rss/1.0/modules/search/" .               :search)
     ("http://purl.org/rss/1.0/modules/slash/" .                :slash)
     ("http://schemas.xmlsoap.org/soap/envelope/" .             :soap)
     ("http://purl.org/rss/1.0/modules/servicestatus/" .        :ss)
     ("http://hacks.benhammersley.com/rss/streaming/" .         :str)
     ("http://purl.org/rss/1.0/modules/subscription/" .         :sub)
     ("http://purl.org/rss/1.0/modules/syndication/" .          :sy)
     ("http://schemas.pocketsoap.com/rss/myDescModule/" .       :szf)
     ("http://purl.org/rss/1.0/modules/taxonomy/" .             :taxo)
     ("http://purl.org/rss/1.0/modules/threading/" .            :thr)
     ("http://purl.org/rss/1.0/modules/textinput/" .            :ti)
     ("http://madskills.com/public/xml/rss/module/trackback/" . :trackback)
     ("http://wellformedweb.org/commentAPI/" .                  :wfw)
     ("http://purl.org/rss/1.0/modules/wiki/" .                 :wiki)
     ("http://www.w3.org/1999/xhtml" .                          :xhtml)
     ("http://www.w3.org/1999/xlink" .                          :xlink)
     ("http://www.w3.org/XML/1998/namespace" .                  :xml)
     ("http://podlove.org/simple-chapters" .                    :psc))
   :test 'equal))

(defun parse-feed-aux (input &key max-entries guid-mask)
  (let ((*feed* (dict))
        (*author* (dict))
        (*entry* nil)
        (*path* nil)
        (*disabled* nil)
        (*parser* (make 'parser
                        :max-entries max-entries
                        :guid-mask guid-mask)))

    (setf (gethash :parser *feed*) (fmt "cl-feedparser ~a" *version*))

    (setf (gethash :author-detail *feed*) *author*)

    ;; TODO Same for streams.
    (when (stringp input)
      (setf input (remove-if-not #'xml-character? input)))

    (catch 'done
      (parser-loop (cxml:make-source input)))

    (setf (gethash :entries *feed*)
          (nreverse (gethash :entries *feed*)))

    (setf (gethash :language *feed*)
          (guess-language *feed*))

    (values *feed* *parser*)))

(defun find-ns (uri)
  (gethash uri *namespaces*))

(defun parser-loop (*source* &key recursive)
  (let ((depth 0))
    (loop (let ((event (klacks:peek *source*)))
            (if (not event)
                (return)
                (case event
                  (:start-element
                   (incf depth)
                   (multiple-value-bind (ev uri lname)
                       (klacks:consume *source*)
                     (declare (ignore ev))
                     (push lname *path*)
                     (unless *disabled*
                       (handle-tag (find-ns uri) lname))))
                  (:end-element
                   (decf depth)
                   (pop *path*)
                   (klacks:consume *source*)

                   (when (and recursive (minusp depth))
                     (return)))
                  (t (klacks:consume *source*))))))))

(defgeneric handle-tag (ns lname)
  (:method (ns lname)
    nil)
  (:method (ns (lname string))
    (handle-tag ns (find-keyword lname))))

(defun find-keyword (string)
  (let ((id (with-output-to-string (s)
              (loop for c across string
                    if (upper-case-p c)
                      do (write-char #\- s)
                         (write-char c s)
                    else do (write-char (char-upcase c) s)))))
    (find-symbol id :keyword)))

(defmacro defhandler (ns lname &body body)
  (with-gensyms (gns glname)
    `(defmethod handle-tag ((,gns (eql ,ns)) (,glname (eql ,lname)))
       ,@body)))

(defmethod handle-tag ((ns null) (lname (eql :title)))
  (handle-title))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :title)))
  (handle-title))

(defmethod handle-tag ((ns (eql :dc)) (lname (eql :title)))
  (handle-title))

(defmethod handle-tag ((ns (eql :rdf)) (lname (eql :title)))
  (handle-title))

(defun handle-title ()
  (when-let (text (get-text))
    (let ((title (string-trim +whitespace+ (sanitize-title text))))
      (setf (gethash :title (or *entry* *feed*)) title))))

(defhandler :atom :tagline
  (handle-subtitle))

(defhandler :atom :subtitle
  (handle-subtitle))

(defhandler :itunes :subtitle
  (handle-subtitle))

(defhandler :atom :info
  (when-let (text (sanitize-content (get-text)))
    (setf (gethash :info *feed*) text)))

(defhandler :feedburner :browser-friendly
  (handle-tag :atom :info))

(defhandler :atom :rights
  (when-let (text (sanitize-content (get-text)))
    (setf (gethash :rights *feed*) text)))

(defhandler :atom :copyright
  (handle-tag :atom :rights))

(defhandler :dc :rights
  (handle-tag :atom :rights))

(defhandler nil :copyright
  (handle-tag :atom :rights))

(defhandler :dc :rights
  (handle-tag :atom :rights))

(defun handle-subtitle ()
  (when-let (text (sanitize-title (get-text)))
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

    (when href
      (setf href (resolve-uri href)))

    (when (and href (or (not rel) (equal rel "alternate")))
      (setf (gethash :link (or *entry* *feed*))
            (resolve-uri href)))

    (setf (gethash :rel link) rel
          (gethash :type link) type
          (gethash :href link) href
          (gethash :title link) title)

    (push link (gethash :links (or *entry* *feed*)))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :name)))
  (setf (gethash :name *author*) (get-text)))

(defhandler :atom :name
  (let ((name (get-text)))
    (setf (gethash :author (or *entry* *feed*)) name
          (gethash :name *author*) name)))

(defhandler :atom :email
  (setf (gethash :email *author*) (get-text)))

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
  (let* ((author (get-text))
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
  (when-let (content (get-content))
    (let ((entry *entry*))
      (if (not entry)
          (setf (gethash :subtitle *feed*) (@ content :value))
          (setf (@ entry :summary) (@ content :value)
                (@ entry :summary-detail) content)))))

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
  (when-let ((string (get-text)))
    (values string (parse-timestring string))))

(defun parse-timestring (timestring)
  (or (net.telent.date:parse-time timestring)
      (ignore-errors
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
  (when-let (id (get-text))
    (when *entry*
      (check-guid-mask id)
      (setf (gethash :id *entry*) id))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :id)))
  (let ((id (get-text)))
    (check-guid-mask id)
    (setf (gethash :id (or *entry* *feed*)) id)))

(defmethod handle-tag ((ns (eql :dc)) (lname (eql :description)))
  (get-summary))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :summary)))
  (get-summary))

(defmethod handle-tag ((ns (eql :feedburner)) (lname (eql :orig-link)))
  (setf (gethash :link *entry*)
        (get-text)))

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

;; TODO Handle XHTML.

(defun get-entry-content ()
  (push (get-content) (@ *entry* :content)))

(defun get-content ()
  (let ((type (klacks:get-attribute *source* "type"))
        (content (dict)))
    (setf (@ content :base) (klacks:current-xml-base *source*))
    (if (equal type "xhtml")
        (get-xhtml-content content)
        (get-text-content content))
    content))

(defun get-text-content (content)
  (let* ((attrs (klacks:list-attributes *source*))
         (string (sanitize-content (get-text))))
    (setf (@ content :value) string
          (@ content :type)  (guess-type string attrs))
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

(defun get-xhtml-content (content)
  ;; HACK: Unless we clear the namespace stack CXML tries to read the
  ;; rest of the document in as XHTML.
  (let ((ns (slot-value *source* 'cxml::namespace-stack)))
    (klacks:find-element *source* "div")
    (let ((value
            (flex:octets-to-string
             (let ((out (make-instance 'absolute-uri-handler
                                       :handlers (list (cxml:make-octet-vector-sink))
                                       :base (klacks:current-xml-base *source*))))
               (klacks:serialize-element *source* out :document-events t)))))
      (setf (@ content :value) (sanitize-content value)
            (@ content :type)  "text/html"
            (@ content :base)  (klacks:current-xml-base *source*))
      (setf (slot-value *source* 'cxml::namespace-stack) ns)
      content)))

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
      (setf (gethash :id entry) id))))

(defmethod handle-tag ((ns (eql :atom)) (lname (eql :entry)))
  (handle-entry))

(defun handle-entry ()
  (let ((count (length (gethash :entries *feed*)))
        (max-entries (parser-max-entries *parser*)))
    (if (and max-entries (= max-entries count))
        (throw 'done nil)
        (lret ((*author* (dict))
               (*entry* (dict)))

          (setf (gethash :author-detail *entry*) *author*)

          (let ((*disabled* *disabled*))
            (parser-loop *source* :recursive t)
            (unless *disabled*
              (push *entry* (gethash :entries *feed*))))

          (setf (gethash :author *entry*)
                (gethash :name *author*))))))

(defun resolve-uri (uri)
  (let ((base (klacks:current-xml-base *source*)))
    (ignoring puri:uri-parse-error
      (puri:merge-uris uri base))))

(defun xml-character? (c)
  (declare (optimize speed))
  (let ((code (char-code c)))
    (or (eql code 9)
        (eql code 10)
        (eql code 13)
        (<= 32 code #xd7ff)
        #+rune-is-utf-16 (<= #xD800 code #xDFFF)
        (<= #xe000 code #xfffd)
        #-rune-is-utf-16 (<= #x10000 code #x10ffff))))

(defun get-text ()
  (when (eql (klacks:peek *source*) :characters)
    (with-output-to-string (s)
      (loop while (eql (klacks:peek *source*) :characters)
            do (write-string (nth-value 1 (klacks:consume *source*)) s)))))

(defun guess-language (feed &key (min-length 100))
  (let ((lang (gethash :language feed)))
    ;; If it says it's not in English, it's probably right.
    (if (not (string^= "en" lang))
        lang
        (let ((langs (delq nil
                           (loop for entry in (gethash :entries feed)
                                 nconc (loop for content in (@ entry :content)
                                             for value = (join (ppcre:split "(?s)<.*?>"
                                                                            (@ content :value))
                                                               #\Space)
                                             when (> (length value) min-length)
                                               collect (textcat:classify value))))))
          ;; Do we have agreement?
          (if (and langs (every #'eql langs (rest langs)))
              (string-downcase (first langs))
              lang)))))



(defun parse-feed (feed &key max-entries punt (sanitize-content t) (sanitize-titles t)
                             guid-mask)
  "Try to parse FEED."
  (when (pathnamep feed)
    (setf feed (fad:file-exists-p feed)))
  (check-type max-entries limit)
  (let ((feed
          (if punt
              (lret ((feed (or (ignore-errors (feedparse.py feed))
                               (dict))))
                (setf (@ feed :parser)
                      (fmt "feedparser ~a" *feedparser-version*)))
              (restart-case
                  (let ((*content-sanitizer*
                          (if (not sanitize-content)
                              #'identity
                              *content-sanitizer*))
                        (*title-sanitizer*
                          (if (not sanitize-titles)
                              #'identity
                              *title-sanitizer*)))
                    (parse-feed-aux feed
                                    :max-entries max-entries
                                    :guid-mask guid-mask))
                (punt ()
                  :report "Punt to feedparser.py."
                  (lret ((feed (parse-feed feed :max-entries max-entries :punt t)))
                    (setf (@ feed :entries)
                          (remove-if (lambda (entry)
                                       (find (@ entry :id) guid-mask :test #'equal))
                                     (@ feed :entries)))))))))
    (setf (@ feed :language) (guess-language feed))
    feed))

(defun parse-feed-safe (feed &key max-entries punt
                                  (sanitize-content t)
                                  (sanitize-titles t)
                                  guid-mask)
  "Try to parse FEED, possibly punting to feedparser.py."
  (handler-bind ((error
                   (lambda (c) (declare (ignore c))
                     (invoke-restart 'punt))))
    (parse-feed feed :max-entries max-entries
                     :punt punt
                     :guid-mask guid-mask
                     :sanitize-content sanitize-content
                     :sanitize-titles sanitize-titles)))
