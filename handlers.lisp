(defpackage :cl-feedparser/handlers
  (:use :cl :alexandria :serapeum :anaphora
        :cl-feedparser/parser)
  (:import-from :fxml.klacks
    :map-attributes :get-attribute))

(in-package :cl-feedparser/handlers)

;;; Atom 1.0.

(defhandler :atom :title
  (when-let (text (sanitize-title (get-text)))
    (let ((title (trim-whitespace text)))
      ;; Cf. Grantland.
      (ensure2 (gethash* :title (or *entry* *feed*))
        title))))

(defhandler :atom :subtitle
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash* :subtitle *feed*) text)))

(defhandler :atom :rights
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash* :rights *feed*) text)))

(defhandler :atom :link
  (let* ((source *source*)
         (rel (get-attribute source "rel"))
         (type (get-attribute source "type"))
         (href (get-attribute source "href"))
         (title (get-attribute source "title"))
         (link (make-hash-table)))

    ;; E.g. Quora.
    (when (or (equal href "None") (equal href "/None"))
      (return-from handle-tag))

    (when href
      (setf href (resolve-uri href)))

    ;; TODO Invalid for Atom 0.3?
    (when (and href (or (not rel) (equal rel "alternate")))
      (setf (gethash* :link (or *entry* *feed*))
            href))

    (setf (gethash* :rel link) rel
          (gethash* :type link) type
          (gethash* :href link) href
          (gethash* :title link) title)

    (when (and *entry* (equal rel "enclosures"))
      (push link (gethash* :enclosures *entry*)))

    (push link (gethash* :links (or *entry* *feed*)))))

(defhandler :atom :name
  (let ((name (sanitize-text (get-text))))
    (setf (gethash* :author (or *entry* *feed*)) name
          (gethash* :name *author*) name)))

(defhandler :atom :email
  (setf (gethash* :email *author*) (get-text/sanitized)))

(defhandler :atom :uri
  (setf (gethash* :href *author*)
        (resolve-uri (get-text))))

(defhandler :atom :feed
  (block nil
    (map-attributes
     (lambda (ns lname qname value dtdp)
       (declare (ignore ns lname dtdp))
       (when (equal qname "xml:lang")
         (setf (gethash* :language *feed*)
               (sanitize-text value))
         (return)))
     *source*)))

(defhandler :atom :icon
  (setf (gethash* :icon *feed*)
        (resolve-uri (get-text))))

(defhandler :atom :summary
  (if-let (entry *entry*)
    (when-let (content (get-content))
      (setf (gethash* :summary entry) (gethash* :value content)
            (gethash* :summary-detail entry) content))

    (setf (gethash* :subtitle *feed*)
          (sanitize-title (get-text)))))

(defhandler :atom :published
  (let ((target (or *entry* *feed*)))
    (setf (values (gethash* :published target)
                  (gethash* :published-parsed target))
          (get-timestring))))

;;; E.g. 3QD.
(defhandler :atom :issued
  (let ((target (or *entry* *feed*)))
    (setf (values (gethash* :published target)
                  (gethash* :published-parsed target))
          (get-timestring))))

(defhandler :atom :updated
  (let ((target (or *entry* *feed*)))
    (setf (values (gethash* :updated target)
                  (gethash* :updated-parsed target))
          (get-timestring)))

  (and *entry* (check-guid-mask)))

(defhandler :atom :id
  (let ((id (get-text)))
    (when-let (entry *entry*)
      (check-guid-mask entry))
    (setf (gethash* :id (or *entry* *feed*)) id)))

(defhandler :atom :content
  (push (get-content) (gethash* :content *entry*)))

(defhandler :atom :entry
  (entry-context))

(defhandler :atom :contributor
  (let ((*author* (dict)))
    (parser-loop *source* :recursive t)
    (push *author* (gethash* :contributors *entry*))))

;;; Atom 0.3.

(defmethod handle-tag ((ns (eql :atom03)) tag)
  "Fall back to Atom 1.0 parsing."
  (handle-tag :atom tag))

(defhandler :atom03 :tagline
  (handle-tag :atom :subtitle))

(defhandler :atom :info
  (when-let (text (sanitize-title (get-text)))
    (setf (gethash* :info *feed*) text)))

(defhandler :atom03 :copyright
  (handle-tag :atom :rights))

(defhandler :atom03 :modified
  (handle-tag :atom :updated))

(defhandler :atom03 :created
  (awhen (get-timestring)
    (setf (gethash* :created *entry*) t)))

;;; Dublin Core.

(defmethod handle-tag ((ns (eql :dcterms)) lname)
  (handle-tag :dc lname))

(defhandler :dc :title
  (handle-tag :atom :title))

(defhandler :dc :rights
  (handle-tag :atom :rights))

(defhandler :dc :creator
  (handle-tag nil :author))

(defhandler :dc :date
  (handle-tag :atom :updated))

(defhandler :dcterms :modified
  (handle-tag :atom :updated))

(defhandler :dcterms :contributor
  (push (dict :name (get-text))
        (gethash* :contributors *entry*)))

(defhandler :dc :created
  (handle-tag :atom03 :created))

(defhandler :dc :language
  (handle-tag nil :language))

;;; Feedburner.

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

;;; Itunes.

(defhandler :itunes :subtitle
  (handle-tag :atom :subtitle))

(defhandler :itunes :author
  (handle-tag nil :author))

;;; RDF.

(defhandler :rdf :title
  (handle-tag :atom :title))

(defhandler :rdf :description
  (handle-tag :dc :description))

(defhandler :rdf :item
  (when-let (id (get-attribute *source* "about"))
    (entry-context :id id)))

;;; RSS.

(defhandler nil :title
  (handle-tag :atom :title))

(defhandler nil :description
  (if *entry*
      (handle-tag :atom :summary)
      (handle-tag :atom :subtitle)))

(defhandler nil :copyright
  (handle-tag :atom :rights))

(defhandler nil :link
  (when-let (string (get-text))
    (setf (gethash* :link (or *entry* *feed*))
          (resolve-uri string))))

(defhandler nil :author
  (let* ((author (get-text/sanitized))
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

(defhandler nil :language
  (setf (gethash* :language *feed*) (get-text)))

(defhandler nil :pub-date
  (handle-tag :atom :published))

(defhandler nil :last-build-date
  (unless *entry*
    (handle-tag :atom :published)))

(defhandler nil :guid
  ;; todo rdf:about
  (when-let (entry *entry*)
    (let ((permalinkp
            (equal "true" (get-attribute *source* "isPermaLink")))
          (id (get-text)))
      (when id
        (check-guid-mask entry)
        (setf (href entry :id) id)
        (when (or permalinkp
                  ;; Use GUID as a fallback link.
                  (and (urlish? id)
                       (null (gethash* :href entry))))
          (setf (gethash* :link entry) (resolve-uri id)))))))

(defhandler nil :item
  (handle-tag :atom :entry))

(defhandler nil :comments
  (setf (gethash* :comments *entry*) (get-text)))

(defhandler nil :ttl
  (when-let (string (get-text))
    (setf (gethash* :ttl *feed*) string)))

(defhandler :content :encoded
  (handle-tag :atom :content))

(defhandler :xhtml :body
  (handle-tag :atom :content))

(defhandler nil :body
  (handle-tag :atom :content))

(defhandler nil :fullitem
  (handle-tag :atom :content))
