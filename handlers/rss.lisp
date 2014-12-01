(in-package #:cl-feedparser)

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
  (let* ((author (get-text/trivial-markup))
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
            (equal "true" (klacks:get-attribute *source* "isPermaLink")))
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
