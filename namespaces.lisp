(in-package #:cl-feedparser)

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
        "http://purl.org/atom/ns#" :atom03 ;atom03
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

(defun nstring-camel-case (string)
  "Destructively convert STRING to camelCase and return it."
  (prog1 string
    (loop for i from 0 below (length string)
          for c2 = (aref string i)
          for c1 = #\Space then c2
          do (unless (eql c2 #\-)
               (if (eql c1 #\-)
                   (setf (aref string i) (char-upcase c2))
                   (setf (aref string i) (char-downcase c2)))))))

(defun string-camel-case (string)
  "Return a copy of STRING in camelCase."
  (nstring-camel-case (copy-seq (string string))))

(def namespace-map
  (let ((map (fset:map)))
    (maphash (lambda (k v)
               (when k
                 (setf map (fset:with map (string-camel-case v) k))))
             namespaces)
    map)
  "Map from prefix to namespace.")

(def namespace-prefixes
  (cons nil (remove-duplicates (hash-table-values namespaces))))

(defun find-ns (uri)
  (gethash uri namespaces))
