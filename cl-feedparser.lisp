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

(declaim (function *content-sanitizer* *title-sanitizer))

(defvar *content-sanitizer*
  (lambda (x)
    (when x
      (sanitize:clean x +feed+))))

(defparameter *title-sanitizer* *content-sanitizer*)

(defun sanitize-content (x)
  (funcall *content-sanitizer* x))

(defun sanitize-title (x)
  (funcall *title-sanitizer* x))



(defparameter *ns* '(("atom" "http://www.w3.org/2005/Atom")
                     ("feedburner" "http://rssnamespace.org/feedburner/ext/1.0")
                     ("dc" "http://purl.org/dc/elements/1.1/")
                     ("dcterms" "http://purl.org/dc/terms/")
                     ("itunes" "http://www.itunes.com/DTDs/PodCast-1.0.dtd")
                     ("xhtml" "http://www.w3.org/1999/xhtml")
                     ("content" "http://purl.org/rss/1.0/modules/content/")))

(defclass feed-parser ()
  ((rawfeed :initarg :rawfeed :reader rawfeed)
   (feed :initform (dict) :reader feed)
   (paths :initarg :paths :accessor paths)
   (max-entries :initarg :max-entries :type limit :accessor max-entries))
  (:default-initargs :max-entries nil))

(defparameter *rss-paths*
  (dictq :title "/rss/channel/title"
         :link "/rss/channel/link"
         :language '("/rss/channel/dc:language"
                     "/rss/channel/language")
         :description '("/rss/channel/description")
         :pubdate '("/rss/channel/lastBuildDate" "/rss/channel/dc:date"
                    "/rss/channel/pubDate")
         :entry-title "title"
         :entry-id '("guid" "feedburner:origLink" "link")
         :content '("content:encoded" "body" "fullitem" "xhtml:body")
         :summary '("description" "dc:description")
         :entry-link '("feedburner:origLink" "link")
         :entry-pubdate '("dc:date" "dcterms:modified" "pubDate")
         :ttl "/rss/channel/ttl"))

(defparameter *atom-paths*
  (dictq :title "/atom:feed/atom:title"
         :link "/atom:feed/atom:link[@rel=\"alternate\"]/@href"
         :language "/atom:feed/@xml:lang"
         :icon "/atom:feed/atom:icon"
         :pubdate '("/atom:feed/atom:modified" "/atom:feed/atom:updated")
         :entry-title "atom:title"
         :entry-id "atom:id"
         :summary "atom:summary"
         :content "atom:content"
         :entry-link '("feedburner:origLink" "atom:link[@rel=\"alternate\" or not(@rel)]/@href")
         :entry-pubdate '("atom:published" "atom:modified" "atom:updated")))

(defclass rss-parser (feed-parser)
  ((version :initform "rss20")
   (paths :initform *rss-paths*)))

(defclass atom-parser (feed-parser)
  ((version :initform "atom")
   (paths :initform *atom-paths*)))

(defun parse-atom-links (rawfeed xpath &aux (ns *ns*))
  (iter (for rawentry in-xpath-result xpath
             on rawfeed with-ns-map ns)
    (flet ((q (s)
             (libxml2.xpath:find-string rawentry s :ns-map ns)))
      (collect (aprog1 (dict)
                 (flet ((attr (key query)
                          (when-let (q (q query))
                            (setf (@ it key) q))))
                   (attr :rel "@rel")
                   (attr :type "@type")
                   (attr :href "@href")
                   (attr :title "@title")))))))

(defmethod parse-time ((parser rss-parser) string)
  (or (net.telent.date:parse-time string)
      ;; E.g. alistapart.
      (ignore-errors
       (local-time:timestamp-to-universal
        (local-time:parse-timestring string)))))

(defmethod parse-time ((parser atom-parser) string)
  (local-time:timestamp-to-universal
   (local-time:parse-timestring string)))

(defmethod parse-feed-links ((parser rss-parser))
  (parse-atom-links (rawfeed parser) "/rss/channel/atom:link"))

(defmethod parse-feed-links ((parser atom-parser))
  (parse-atom-links (rawfeed parser) "/atom:feed/atom:link"))

(defmethod map-entries (fun (parser atom-parser))
  (with-slots (max-entries) parser
    (let ((count 0))
      (iter (for rawentry in-xpath-result "/atom:feed/atom:entry"
                 on (rawfeed parser) with-ns-map *ns*)
        (when (and max-entries (> count max-entries))
          (return))
        (funcall fun rawentry)
        (incf count)))))

(defmethod map-entries (fun (parser rss-parser))
  (with-slots (max-entries) parser
    (let ((count 0))
      (iter (for rawentry in-xpath-result "/rss/channel/item"
                 on (rawfeed parser) with-ns-map *ns*)
        (when (and max-entries (> count max-entries))
          (return))
        (funcall fun rawentry)
        (incf count)))))

(defmethod author-details ((parser atom-parser) rawentry &aux (ns *ns*))
  (flet ((q (s)
           (libxml2.xpath:find-string rawentry s :ns-map ns)))
    (aprog1 (dict)
      (setf (@ it :name) (q "atom:author/atom:name")
            (@ it :email) (q "atom:author/atom:email")
            (@ it :href) (q "atom:author/atom:uri")))))

(defmethod author-details ((parser rss-parser) rawentry &aux (ns *ns*))
  (flet ((q (s)
           (libxml2.xpath:find-string rawentry s :ns-map ns))
         (strip-parens (s)
           (when s                      ;Lest we return "NIL"
             (string-trim " ()" s))))
    (aprog1 (dict)
      (let ((creator (or (q "dc:creator")
                         (q "itunes:author")))
            (author (q "author")))
        (let ((email? (find #\@ author)))
          (if email?
              (let ((space (position #\Space author)))
                (setf (@ it :email) (subseq author 0 space))
                (when space
                  (ensure creator (strip-parens (subseq author space)))))
              (ensure creator author)))
        (setf (@ it :name)
              (strip-parens creator))))))

(defmethod content (parser rawentry)
  nil)

(defmethod content ((parser atom-parser) rawentry &aux (ns *ns*))
  (with-slots (paths) parser
    (flet ((get-node (q)
             (q1 paths ns
                 #'libxml2.xpath:find-single-node
                 rawentry q)))
      (when-let ((type (libxml2.xpath:find-string rawentry "atom:content/@type")))
        (when (string*= "xhtml" type)
          (when-let ((div (libxml2.xpath:find-single-node
                           (get-node :content) "xhtml:div")))
            (sanitize-content (serialize div))))))))

(defun q1 (paths ns action raw expr)
  (when-let (key (@ paths expr))
    (or
     (if (listp key)
         (loop for k in key
               thereis (funcall action raw k :ns-map ns))
         (funcall action raw key :ns-map ns))
     "")))

(defun serialize (x)
  (xtree:serialize x :to-string))

(defmethod parse-feed! (parser &aux (ns *ns*))
  (let ((libxml2.xpath:*default-ns-map*
          (append libxml2.xpath:*default-ns-map* ns)))
    (with-slots (version paths rawfeed feed)
        parser
      (expand-relative-uris! rawfeed)
      (flet ((parse-time (s)
               (parse-time parser s)))
        (let ((qs (curry #'q1 paths ns #'libxml2.xpath:find-string)))
          (let-alias ((qs (curry qs rawfeed)))
            (setf (@ feed :version) version
                  (@ feed :title) (string-trim +whitespace+ (sanitize-title (qs :title)))
                  (@ feed :link) (qs :link)
                  (@ feed :language) (qs :language)
                  (@ feed :icon) (qs :icon)
                  (@ feed :ttl) (qs :ttl))
            (when-let (pubdate (qs :pubdate))
              (setf (@ feed :pubdate) (parse-time pubdate)))
            (setf (@ feed :links) (parse-feed-links parser))
            (let ((entries (serapeum:collecting
                             (map-entries
                              (lambda (rawentry)
                                (let-alias ((qs (curry qs rawentry)))
                                  (collect
                                      (aprog1 (dict)
                                        (setf (@ it :title)         (sanitize-title (qs :entry-title))
                                              (@ it :id)            (qs :entry-id)
                                              (@ it :author-detail) (author-details parser rawentry)
                                              (@ it :author)        (@ it :author-detail :name)
                                              (@ it :link)          (qs :entry-link))
                                        (let ((summary (sanitize-content (qs :summary))))
                                          (setf (@ it :summary) summary)
                                          ;; XXX Atom allows multiple content elements.
                                          (when-let ((content (or (content parser rawentry)
                                                                  (sanitize-content (qs :content)))))
                                            (unless (emptyp content)
                                              (setf (@ it :content)
                                                    (list (aprog1 (dict)
                                                            (setf (@ it :value)
                                                                  content
                                                                  (@ it :type) "text/html"
                                                                  ;; TODO
                                                                  (@ it :language)
                                                                  (@ feed :language))))))))
                                        (when-let (pubdate (qs :entry-pubdate))
                                          (setf (@ it :pubdate)
                                                (parse-time pubdate)))))))
                              parser))))
              (setf (@ feed :entries) entries
                    (@ feed :parser) (fmt "cl-feedparser ~a" *version*)))))))))

(defun guess-language (feed &key (min-length 100))
  (let ((lang (@ feed :language)))
    ;; If it says it's not in English, it's probably right.
    (if (not (string^= "en" lang))
        lang
        (let ((langs (delq nil
                           (loop for entry in (@ feed :entries)
                                 nconc (loop for content in (@ entry :content)
                                             for value = (join (ppcre:split "(?s)<.*?>" (@ content :value)) #\Space)
                                             when (> (length value) min-length)
                                               collect (textcat:classify value))))))
          ;; Do we have agreement?
          (if (and langs (every #'eql langs (rest langs)))
              (string-downcase (first langs))
              lang)))))

(defun expand-relative-uris! (tree &key base &aux (ns *ns*))
  (check-type tree libxml2.tree:document)

  (labels ((absolute? (uri)
             (string*= "://" uri))
           (expand! (node attribute)
             (let ((v (xtree:attribute-value node attribute)))
               (when (stringp v)
                 (unless (absolute? v)
                   (ignoring puri:uri-parse-error
                     (setf (xtree:attribute-value node attribute)
                           (princ-to-string
                            (puri:merge-uris v (or base (xtree:base-url node)))))))))))

    (iter (for node in-xpath-result "//*[@href]"
            on tree with-ns-map ns)
      (expand! node "href"))

    (iter (for node in-xpath-result "//*[@src]"
            on tree with-ns-map ns)
      (expand! node "src"))))

(defun parse-feed (feed &key max-entries punt (sanitize-content t) (sanitize-titles t))
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
                    (parse-feed-aux feed :max-entries max-entries)
                    #+ () (xtree:with-parse-document (rawfeed feed)
                            (unless rawfeed (error "No feed?"))
                            (let* ((root-feed (xtree:root rawfeed))
                                   (name (xtree:local-name root-feed))
                                   (namespace (xtree:namespace-uri root-feed)))
                              (cond
                                ((and (string= name "feed")
                                      (string= namespace "http://www.w3.org/2005/Atom"))
                                 (let ((feed
                                         (let ((parser (make 'atom-parser :rawfeed rawfeed :max-entries max-entries)))
                                           (parse-feed! parser)
                                           (feed parser))))
                                   feed))
                                ((and (not namespace)
                                      (string= name "rss")
                                      (string= "2.0"
                                               (xtree:attribute-value root-feed
                                                                      "version")))
                                 (let ((parser (make 'rss-parser :rawfeed rawfeed :max-entries max-entries)))
                                   (parse-feed! parser)
                                   (feed parser)))
                                (t (error "Unknown feed type"))))))
                (punt ()
                  :report "Punt to feedparser.py."
                  (parse-feed feed :max-entries max-entries :punt t))))))
    (setf (@ feed :language) (guess-language feed))
    feed))

(defun parse-feed-safe (feed &key max-entries punt
                                  (sanitize-content t)
                                  (sanitize-titles t))
  (handler-bind ((error
                   (lambda (c) (declare (ignore c))
                     (invoke-restart 'punt))))
    (parse-feed feed :max-entries max-entries
                     :punt punt
                     :sanitize-content sanitize-content
                     :sanitize-titles sanitize-titles)))
