(defpackage :cl-feedparser/feed-sanitizer
  (:use :cl :alexandria :serapeum)
  (:import-from :fxml.sanitize :define-sanitize-mode)
  (:export feed-sanitizer))

(in-package :cl-feedparser/feed-sanitizer)

(define-sanitize-mode feed-sanitizer
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
                 "m" "map" "mark" "noscript"
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
