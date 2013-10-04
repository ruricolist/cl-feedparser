(in-package :feedparser)

(defcondition lenient-parser-error (error)
  ())

(defcondition unresolvable-entity (lenient-parser-error)
  ()
  )

(defun repair-source (string &key (repair-entities t)
                                  (strip-invalid))
  (when repair-entities
    (setq string (ppcre:regex-replace-all "&(?!#?\\w|;)" string "&amp;"))
    (string string (ppcre:regex-replace "&(\\w+);"
                                        ()
                                        )))
  (when strip-invalid
    (setq string (remove-if-not #'xml-character? string))))

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

(defun parse-leniently (string handler)
  (sax:start-document handler)
  (let ((tokens (ppcre:split "(<[^>]+>)|(&#?\\w+;)" string
                             :with-registers-p t)))
    (nlet parse ((tokens tokens))
      (optima:match tokens
        ;; Comment
        ((list* (optima.ppcre:ppcre "^<--") tokens)
         (parse (cdr (member-if (lambda (token) (ppcre:scan "--\\s*>$" token))
                                tokens))))
        ;; Start tag.
        ((list* (optima.ppcre:ppcre "^<([a-zA-Z][.a-zA-Z0-9]*)>$" tag)
                tokens)
         (sax:start-element handler nil tag tag nil)
         (parse tokens))
        ;; End tag.
        ((list* (optima.ppcre:ppcre "^</([a-zA-Z][.a-zA-Z0-9]*)>$" tag)
                tokens)
         (sax:end-element handler nil tag tag)
         (parse tokens))
        ;; Tag with attribute.
        ((list* (optima.ppcre:ppcre "^<([a-zA-Z][.a-zA-Z0-9]*) ([^>]+)$" tag attrs)
                tokens)
         (sax:start-element handler nil tag tag (parse-attrs attrs))
         (parse tokens))
        ;; Character reference.
        ((list* (optima.ppcre:ppcre "^&#(\\w+)" name)
                tokens)
         ())
        ;; Entity reference.
        ((list* (optima.ppcre:ppcre "^&(\\w+)" name)
                tokens))
        ((list* chars tokens)
         (sax:characters handler chars)))))
  (sax:end-document handler)
  )

(defun parse-leniently (string handler &key)
  (sax:start-document handler)
  (flet ((chars (string)
           (unless (emptyp string)
             (sax:characters handler string))))
    (let ((string (remove-if-not #'xml-character? string)))
      (nlet parse ((start 0))
        (let ((next (position-if (lambda (c)
                                   (or (eql c #\<)
                                       (eql c #\&)))
                                 string :start start)))
          (if (not next)
              (sax:characters handler (subseq string start))
              (progn
                (unless (= start next)
                  (sax:characters handler (subseq string start next)))
                (let ((char (aref string next)))
                  (ecase char
                    (#\<
                     (cond ((string^= "--" s2 :start2 (1+ next))
                            (parse (or (position #\> string)
                                       (length string))))
                           ((eql (aref string (1+ next)) #\/)
                            ()
                            )
                           (t (ppcre:register-groups-bind (name attrs)
                                  ("(\\w+)([^>]*)" string :start (1+ next))
                                (sax:start-element handler
                                                   nil
                                                   name
                                                   (parse-attrs attrs))
                                (parse (or (position #\> string :start (1+ next))
                                           (length string)))))))
                    (#\&
                     (ppcre:register-groups-bind (name)
                         ("(\\w+);" string :start (1+ next))
                       ()
                       ))
                    )
                  ))
              )
          ))))
  (sax:end-document handler))

(defun read-entity (source out)
  (let ((next (read-char source)))
    (or (case next
          (#\a
           ()
           )
          (#\g)
          (#\t)
          (#\q)

          )
        (error 'unresolvable-entity))))

(defmethod parse-char ((tag tag) (char (eql #\>)))
  (sax:end-element *handler*
                   (tag-ns tag)
                   (tag-qname tag)
                   (tag-lname tag)))

(defmethod parse-char ((tag tag) (char (eql #\>))))
