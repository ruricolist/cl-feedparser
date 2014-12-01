(in-package #:cl-feedparser)

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
  (setf (gethash* :uri *author*)
        (resolve-uri (get-text))))

(defhandler :atom :feed
  (block nil
    (klacks:map-attributes
     (lambda (ns lname qname value dtdp)
       (declare (ignore ns lname dtdp))
       (when (equal qname "xml:lang")
         (setf (gethash* :language *feed*) value)
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
