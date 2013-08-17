;;;; Last modified : 2013-08-17 17:17:36 tkych

;; cl-feed-parser/cl-feed-parser.asd

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under LLGPL.
;; For more details, see http://opensource.franz.com/preamble.html


;;====================================================================
;; CL-FEED-PARSER: Parse Atom and RSS feeds in Common Lisp
;;====================================================================
;; cl-feed-parser/
;;   cl-feed-parser.asd
;;   feed-parser.lisp
;;   README.md
;;   CHANGELOG


;;====================================================================
;; System for CL-FEED-PARSER
;;====================================================================

(asdf:defsystem #:cl-feed-parser
  :name        "Cl-Feed-Parser"
  :description "Parse Atom and RSS feeds in Common Lisp.
URL: https://github.com/tkych/cl-feed-parser"
  :version     "0.0.29"
  :licence     "LLGPL"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :depends-on  (#:iterate #:anaphora #:alexandria #:split-sequence
                #:cxml #:cl-ppcre
                #:drakma #:url-rewrite #:puri #:flexi-streams #:ironclad
                #:sanitize #:cl-date-time-parser)
  :components  ((:file "feed-parser"))
  )


;;====================================================================
