(in-package :cl-feedparser/test)

(in-suite cl-feedparser)

(defun entry-content (entry)
  (let ((content
          (~> (href entry :content) first (href :value))))
    (is (stringp content))
    content))

(local*
  (defsubst control-char? (char)
    (let ((code (char-code char)))
      (or (<= 0 code #x001f)
          (<= #x007f code #x009f))))

  (defun print-bozo (bozo)
    "Print BOZO as with `princ-to-string', removing any control chars."
    (remove-if #'control-char? (princ-to-string bozo))))

(defun parse-date-safe (date)
  (and (stringp date)
       (ignoring local-time::invalid-timestring
         (local-time:parse-timestring date))))

(defun extract-pubdate (hash)
  (let* ((given
           (or (gethash :pubdate hash)
               (feed-ref hash :published-parsed)
               (feed-ref hash :updated-parsed)))
         (final
           (if (and given (< given (get-universal-time)))
               given
               (get-universal-time))))
    (values final (and given t))))

(def bad-mtime-feeds
  (let* ((dir (find-test-file "guid-mask/"))
         (files (uiop:directory-files dir)))
    (keep "xml" files
          :key #'pathname-type
          :test #'equal)))

(test bad-mtimes
  "Make sure trying to parse invalid modified dates in entries won't
error."
  (dolist (bmf bad-mtime-feeds)
    (let* ((file (load-test-file bmf))
           (feed (parse-feed file))
           (entries (href feed :entries))
           (mtimes (mapcar (op (href _ :updated)) entries)))
      (finishes (mapc #'parse-date-safe mtimes)))))

(test kinlay/ftp
  "Can we handle ftp links?"
  (is (member "A Study in Gold"
              (~> (load-test-file "kinlay.xml")
                  parse-feed
                  (href :entries))
              :key (op (href _ :title))
              :test #'equal)))

(test iframes
  (let ((parse (~> (load-test-file "slashdot-iframes.rss")
                   parse-feed)))
    (cl:loop for entry in (href parse :entries)
       for content = (href entry :summary)
       do (is (not (string*= "iframe" content))))))

(test xenomachina
  "Torture test for HTML sanitizers."
  (let ((testbed (load-test-file "testbed.xml")))
    (let ((hash (parse-feed testbed)))
      (is (hash-table-p hash))
      (is (= 1 (length (href hash :entries)))))))

(test no-articles
  "Check that article elements are sanitized."
  (let ((html (~> (parse-feed (load-test-file "what-if.atom"))
                  (href :entries)
                  first
                  (href :summary))))
    (is (not (string*= "<article" html)))))

(test empty-entries
  "How do we handle empty entries?"
  (flet ((try (file url)
           (let ((rss (load-test-file file)))
             (let (feed)
               (finishes (setf feed (parse-feed rss)))
               (is (typep feed 'hash-table))
               (setf (href feed :url) url)
               feed))))
    (try "2013-06-28HypocriteReader.rss"
         "http://feeds.feedburner.com/HypocriteReader")
    (try "foolnews_rss091.xml"
         "http://www.fool.com/xml/foolnews_rss091.xml")))

(test xml-characters
  "Check that we can handle invalid XML characters."
  (let ((feed (load-test-file "jqmobile.rss")))
    (let ((feed (parse-feed feed)))
      (is (hash-table-p feed))
      (is-true (href feed :entries))
      (cl:loop for entry in (href feed :entries)
         for content = (entry-content entry)
         do (is (stringp content))))))

(test recognize-rdf
  "Can we handle RDF?"
  (let ((texts (mapcar #'load-test-file
                       '("ideachampions.rdf"
                         "mit-newcourses-15"
                         "mit-newcourses"))))
    (dolist (text texts)
      (is (hash-table-p (parse-feed text))))))

(test rfc3339-in-rss
  "Can we handle RFC3339 (Atom) dates in RSS?"
  (let ((alistapart (parse-feed (load-test-file "alistapart.rss"))))
    (let ((entries (mapcar #'extract-pubdate (href alistapart :entries))))
      (is (not (every #'= entries (cdr entries)))))))

(test preserving-imgs
  "Are images preserved?"
  (let* ((hash (parse-feed (load-test-file "understandingsociety.xml")))
         (contents (~> hash (href :entries) first entry-content)))
    (is (stringp contents))
    (is (string*= "<img" contents))))

(test kill-styles
  "Are all styles stripped?"
  (let ((feed (parse-feed (load-test-file "tao.xml"))))
    (mapc (lambda (entry)
            (let ((content (entry-content entry)))
              (is (stringp content))
              (is (not (string*= "style=" content)))
              (is (not (string*= "inline-block" content)))))
          (href feed :entries))))

(test relative-uris
  "Can we handle relative URIs in feeds?"
  (labels ((absolute? (uri)
             (let ((uri (quri:uri uri)))
               (and (quri:uri-scheme uri)
                    (quri:uri-host uri))))
           (test-feed (file)
             (let ((feed (parse-feed (load-test-file file))))
               (is (string^= "cl-feedparser" (href feed :parser)))
               (dolist (entry (href feed :entries))
                 (is (absolute? (href entry :link)))
                 (let* ((doc (fxml.html5:parse (entry-content entry)
                                               (fxml-dom:make-dom-builder)))
                        (imgs (fxml.dom:get-elements-by-tag-name doc "img"))
                        (anchors (concatenate 'vector
                                              (fxml.dom:get-elements-by-tag-name doc "a")
                                              (fxml.dom:get-elements-by-tag-name doc "link"))))
                   (do-each (img imgs)
                     (let ((uri (fxml.dom:get-attribute img "src")))
                       (is (absolute? uri))))
                   (do-each (anchor anchors)
                     (let ((uri (fxml.dom:get-attribute anchor "href")))
                       (is (absolute? uri)))))))))
    ;; A feed with relative URIs.
    (test-feed "ongoing.atom")
    ;; A feed with relative URIs, proxied by Feedburner (no xml:base).
    (test-feed "elliotjaystocks.xml")))

(test relative-src
  "Can we handle relative URIs in image sources in feeds?"
  (let ((feed (parse-feed (load-test-file "what-if-imgs.atom"))))
    (is-false (string*= "src=\"/"
                        (~> feed (href :entries) first entry-content)))))

(test strip-style-element
  "Are the contents of style elements stripped?"
  (let ((feed (parse-feed (load-test-file "teche.xml"))))
    (dolist (entry (href feed :entries))
      (let ((content (entry-content entry)))
        (is (not (string*= "font-face" content)))))))

(test no-content-if-no-content
  (let ((files '("notes.unwieldy.rss" "yayitsrob.rss")))
    (dolist (file files)
      (let* ((text (load-test-file file))
             (feed (parse-feed text)))
        (dolist (entry (href feed :entries))
          (is-true (href entry :summary))
          (is-false (href entry :content)))))))

(test junk-before-declaration
  "Can we work around junk before the declaration?"
  (is (hash-table-p (parse-feed (load-test-file "binnsblog.rss"))))
  (is (hash-table-p (parse-feed (load-test-file "hydro74.rss")))))

(test hub-is-none
  "What happens if hub is None (Python)?"
  (let ((feed (~> "samuel-clay.rss"
                  load-test-file
                  parse-feed)))
    (is-false (gethash :hub feed))))

(test xhtml-content
  "Can we handle inline XHTML content?"
  (is (= 10 (length
             (href (parse-feed (load-test-file "hypercritical.xml")
                               :max-entries 10)
                   :entries)))))

(test statwing
  (is (= 10 (length (href (parse-feed (load-test-file "statwing.xml")
                                      :max-entries 10)
                          :entries))))
  (finishes (html5-parser:parse-html5 (load-test-file "statwing-poison.html"))))

(test mailto-href
  ;; The rub is "http://mailto:alex@iphonelife.com"
  (let ((feed (load-test-file "iphonelife")))
    (finishes (parse-feed feed))))

(test html5sec-rss
  (let ((feed (load-test-file "html5sec.rss")))
    ;; Looks like HTML.
    (let* ((parse (parse-feed feed)))
      (is (equal (href parse :link) ""))
      (dolist (entry (href parse :entries))
        (is (equal (href entry :link) ""))
        (macrolet ((test-key (key)
                     `(is-false (find #\< (href entry ,key))
                                "~a is: ~a" ,key (href entry ,key))))
          (test-key :author)
          (test-key :published)
          (test-key :description))))))

(test feedburner-namespaces
  "Regression for bug in Klacks."
  (is (not (emptyp (href (parse-feed (load-test-file "3q-atom.xml")) :entries)))))

(test locklin-etx
  (let ((parse (parse-feed (load-test-file "locklin-etx.xml"))))
    (is (null (find #\ETX (print-bozo (href parse :bozo-exception)))))))

(test xxe-exploit
  ;;  http://blog.detectify.com/post/82370846588/how-we-got-read-access-on-googles-production
  ;; http://mikeknoop.com/lxml-xxe-exploi/t
  (finishes (parse-feed (load-test-file "xxe-exploit.rss"))))

(test billion-laughs
  (signals fxml:entities-forbidden
    (fxml:parse (load-test-file "lol.xml") nil)))

(test undefined-entity
  "The expansion has to be supplied as a string, not a character."
  (let* ((file "arrdem.xml")
         (string (load-test-file file))
         (feed (parse-feed string))
         (entries (href feed :entries)))
    (is (= (length entries) 20))))

(test content-sans-markup
  (let* ((file (load-test-file "push-pub.xml"))
         (feed (parse-feed file)))
    (let ((entries (href feed :entries)))
      (is (every (op (href _ :content)) entries)))))

(test distrijob
  (finishes
    (parse-feed
     (load-test-file "distrijob.xml"))))

(test rss-no-cdata
  (let* ((file (load-test-file "mfarmer-no-cdata.rss"))
         (feed (parse-feed file)))
    (is (notany #'emptyp
                (mapcar (op (href _ :summary))
                        (href feed :entries))))))

(test guid-overrides-link
  "Test that the guid doesn't override the link."
  (let* ((feed (parse-feed (load-test-file "hpr_ogg_rss.php"))))
    (dolist (entry (href feed :entries))
      (is (string*= "eps.php?id=" (href entry :link))))))
