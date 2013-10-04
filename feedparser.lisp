(in-package :cl-feedparser)

(defparameter *resolve-relative-uris* t)

(defparameter *html-sanitizer* #'identity)

(defparameter *namespaces*
  '(("" "")
    ("http://backend.userland.com/rss" "")
    ("http://blogs.law.harvard.edu/tech/rss" "")
    ("http://purl.org/rss/1.0/" "")
    ("http://my.netscape.com/rdf/simple/0.9/" "")
    ("http://example.com/newformat#" "")
    ("http://example.com/necho" "")
    ("http://purl.org/echo/" "")
    ("uri/of/echo/namespace#" "")
    ("http://purl.org/pie/" "")
    ("http://purl.org/atom/ns#" "")
    ("http://www.w3.org/2005/Atom" "")
    ("http://purl.org/rss/1.0/modules/rss091#" "")
    ("http://webns.net/mvcb/"                                "admin")
    ("http://purl.org/rss/1.0/modules/aggregation/"          "ag")
    ("http://purl.org/rss/1.0/modules/annotate/"             "annotate")
    ("http://media.tangent.org/rss/1.0/"                     "audio")
    ("http://backend.userland.com/blogChannelModule"         "blogChannel")
    ("http://web.resource.org/cc/"                           "cc")
    ("http://backend.userland.com/creativeCommonsRssModule"  "creativeCommons")
    ("http://purl.org/rss/1.0/modules/company"               "co")
    ("http://purl.org/rss/1.0/modules/content/"              "content")
    ("http://my.theinfo.org/changed/1.0/rss/"                "cp")
    ("http://purl.org/dc/elements/1.1/"                      "dc")
    ("http://purl.org/dc/terms/"                             "dcterms")
    ("http://purl.org/rss/1.0/modules/email/"                "email")
    ("http://purl.org/rss/1.0/modules/event/"                "ev")
    ("http://rssnamespace.org/feedburner/ext/1.0"            "feedburner")
    ("http://freshmeat.net/rss/fm/"                          "fm")
    ("http://xmlns.com/foaf/0.1/"                            "foaf")
    ("http://www.w3.org/2003/01/geo/wgs84_pos#"              "geo")
    ("http://www.georss.org/georss"                          "georss")
    ("http://www.opengis.net/gml"                            "gml")
    ("http://postneo.com/icbm/"                              "icbm")
    ("http://purl.org/rss/1.0/modules/image/"                "image")
    ("http://www.itunes.com/DTDs/PodCast-1.0.dtd"            "itunes")
    ("http://example.com/DTDs/PodCast-1.0.dtd"               "itunes")
    ("http://purl.org/rss/1.0/modules/link/"                 "l")
    ("http://search.yahoo.com/mrss"                          "media")
    ;; Version 1.1.2 of the Media RSS spec added the trailing slash on
    ;; the namespace
    ("http://search.yahoo.com/mrss/"                         "media")
    ("http://madskills.com/public/xml/rss/module/pingback/"  "pingback")
    ("http://prismstandard.org/namespaces/1.2/basic/"        "prism")
    ("http://www.w3.org/1999/02/22-rdf-syntax-ns#"           "rdf")
    ("http://www.w3.org/2000/01/rdf-schema#"                 "rdfs")
    ("http://purl.org/rss/1.0/modules/reference/"            "ref")
    ("http://purl.org/rss/1.0/modules/richequiv/"            "reqv")
    ("http://purl.org/rss/1.0/modules/search/"               "search")
    ("http://purl.org/rss/1.0/modules/slash/"                "slash")
    ("http://schemas.xmlsoap.org/soap/envelope/"             "soap")
    ("http://purl.org/rss/1.0/modules/servicestatus/"        "ss")
    ("http://hacks.benhammersley.com/rss/streaming/"         "str")
    ("http://purl.org/rss/1.0/modules/subscription/"         "sub")
    ("http://purl.org/rss/1.0/modules/syndication/"          "sy")
    ("http://schemas.pocketsoap.com/rss/myDescModule/"       "szf")
    ("http://purl.org/rss/1.0/modules/taxonomy/"             "taxo")
    ("http://purl.org/rss/1.0/modules/threading/"            "thr")
    ("http://purl.org/rss/1.0/modules/textinput/"            "ti")
    ("http://madskills.com/public/xml/rss/module/trackback/" "trackback")
    ("http://wellformedweb.org/commentAPI/"                  "wfw")
    ("http://purl.org/rss/1.0/modules/wiki/"                 "wiki")
    ("http://www.w3.org/1999/xhtml"                          "xhtml")
    ("http://www.w3.org/1999/xlink"                          "xlink")
    ("http://www.w3.org/XML/1998/namespace"                  "xml")
    ("http://podlove.org/simple-chapters"                    "psc")))

(defparameter *can-be-relative-uri*
  '(link id wfw_comment wfw_commentrss docs url
    href comments icon logo))

(defparameter *can-contain-relative-uris*
  '(content title summary info tagline subtitle
    copyright rights description))

(defparameter *can-contain-dangerous-markup*
  '(content title summary info tagline subtitle copyright rights description))

(defparameter *html-types*
  '(text/html application/xhtml+xml))

(defclass feed-parser (sax:default-handler)
  ((contexts :initform nil)))

(defgeneric handle-tag (self tag attrs)
  (:method ((self feed-parser) (tag string) attrs)
    (handle-tag self (find-keyword tag) attrs))
  (:method ((self feed-parser) (tag symbol) attrs)
    (if (not attrs)
        (push-tag self tag t)
        (progn
          (let ((ctx (get-ctx self)))
            (setf (gethash tag ctx) attrs))))
    nil))

(defun find-keyword (name)
  (find-symbol (string-upcase name) :keyword))

(defparameter *rss-versions*
  '(("0.91" . "rss091u")
    ("0.92" . "rss092")
    ("0.93" . "rss093")
    ("0.94" . "rss094")))

(defclass rss-handler (special-handler))

(defmethod handle-tag ((self feed-parser)
                         (tag (eql :rss))
                         attrs)
  (with-slots (version) self
    (when (or (not version)
              (not (string^= "rss" version)))
      (let* ((attr-version (or (get-attr "version") ""))
             (rss-version (assocdr attr-version *rss-versions* :test 'equal)))
        (cond (rss-version
               (setf version rss-version))
              ((string^= "2." attr-version)
               (setf version "rss20"))
              (t (setf version "rss")))))))

(defmethod handle-tag ((self feed-parser)
                         (tag (eql :channel))
                         attrs)
  (setf (in-feed-p self) t)
  (cdf-common self attrs))

(defun cdf-common (self attrs)
  (when-let (lastmod (gethash "lastmod" attrs))
    (start-modified self (dict))
    (setf (element-stack self)
          (rplaca (element-stack self) lastmod))
    (end-modified self)))

(defmethod sax:characters ((self feed-parser) data)
  (handle-data self data))

(defmethod sax:start-prefix-mapping ((self feed-parser) prefix uri)
  (when uri
    (track-namespace self uri)
    (let ((prefix (if (emptyp prefix) nil prefix)))
      (when (and prefix (puri:uri= "http://www.w3.org/1999/xlink" uri))
        (setf (gethash (fmt "xmlns:~a" prefix)
                       (decls self))
              uri)))))

(defmethod sax:start-element ((self feed-parser) ns local-name qname attrs)
  (with-slots (depth base-uri parent-lang
               base-stack lang-stack)
      self
    (incf depth)
    (let ((base (attrs-base-uri self attrs))
          (lang (attr-lang self attrs)))
      (push base-uri base-stack)
      (push lang lang-stack)
      (track-namespaces self attrs)
      (track-inline-content self local-name prefix attrs)
      (let ((prefix (match-ns self tag)))
        (empty-elements-hack self prefix tag)
        (call-special-handler self prefix suffix)))))

(defun get-attr (qname attrs)
  (when-let (attr (sax:find-attribute qname attrs))
    (sax:attribute-value attr)))

(defmethod sax:end-element ((self feed-parser) ns local-name qname)
  (with-slots (depth) self
    (match-ns self tag)
    (call-special-handler self prefix suffix)
    (track-inline-content self local-name prefix)
    (pop-stacks self)
    (decf depth)))

(defmethod pop-stacks ((self feed-parser))
  (with-slots (base-stack lang-stack base-uri lang)
      self
    (when base-stack
      (pop base-stack)
      (when (first base-stack)
        (setf base-uri (first base-stack))))

    (when lang-stack
      (pop lang-stack)
      (when lang-stack
        (setf lang (first lang-stack))))))

(defun feed-parse (input &key (entries-limit 10))
  (let ((parser (make 'feed-parser :entries-limit entries-limit)))
    (handler-case
        (progn
          (cxml:parse input parser)
          parser)
      (cxml:xml-parse-error (e)
        (setf (bozo parser) t
              (exception parser) e)))))
