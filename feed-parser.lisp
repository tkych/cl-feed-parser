;;;; Last modified : 2013-08-17 21:32:46 tkych

;; cl-feed-parser/feed-parser.lisp

;; Copyright (c) 2013 Takaya OCHIAI <tkych.repl@gmail.com>
;; This software is released under LLGPL.
;; For more details, see http://opensource.franz.com/preamble.html


#|
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (ql:quickload '(:iterate :anaphora :cl-ppcre :alexandria :drakma
                   :cxml :url-rewrite :puri :flexi-streams
                   :split-sequence :sanitize :ironclad
                   :cl-date-time-parser
                   )))
|#

;; TODO:
;; -----
;; - !! Test: Authorization for password-protected feed !!
;; - Add: test & doc
;; - Add: microformats-parser like Beautiful Soup
;; - Add: relative link resolution
;; - Add: character encoding detection
;; - Change: namespace handling, Add: parse-option valid-namespaces
;; - Add: parsing CDF
;; - Add: documentation


;;====================================================================
;; CL-Feed-Parser: Parse Atom and RSS feeds in Common Lisp
;;====================================================================

(in-package :cl-user)

(defpackage #:cl-feed-parser
  (:nicknames #:feed-parser)
  (:use :cl :iterate)
  (:import-from #:anaphora #:it #:aif #:acond)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:parse-feed
           #:ref
           #:to-alist
           #:*user-agent*
           #:*date-time-parser*
           #:*sanitizer*
           #:be-lazy
           #:lazy-p
           #:force
           ))

(in-package #:cl-feed-parser)


;;--------------------------------------------------------------------
;; Utilities
;;--------------------------------------------------------------------
(declaim (inline lazy-p force md5))

(defun str (&rest strings)
  "Concatenates strings."
  (with-output-to-string (s)
    (dolist (string strings)
      (write-string string s))))

(defmacro be-lazy (&body body)
  "Make lazy object. For making a custum-sanitizer or
custum-date-time-parser."
  `(cons 'lazy (lambda () ,@body)))

(defun lazy-p (x)
  "Check `x' is a lazy object or not. For making a custum-accessor
function."
  (and (consp x) (eq 'lazy (car x))))

(defun force (lazy-object)
  "Force lazy object. For making a custum-accessor function."
  (check-type lazy-object (satisfies lazy-p))
  (funcall (cdr lazy-object)))

;;; Memo: 2013-08-10 by tkych
;;; `dispatch-string' is more specific util than `alexandria:switch'.
;;; `dispatch-string' is useful, if only the followings are satisfied:
;;;  1. matching-pattern is a string or list of strings (list will be
;;;     expanded),
;;;  2. keyform's :key option isn't need.
(defmacro dispatch-string (keyform &body clauses)
  "Same as CASE, except that the test function is #'STRING=."
  (let ((evaled-keyform (gensym "KEYFORM")))
    `(let ((,evaled-keyform ,keyform))
       (declare (ignorable ,evaled-keyform))
       (cond ,@(loop
                  :for (keys . form) :in clauses
                  :collect
                  (cond ((or (eq keys t) (eq keys 'otherwise))
                         `(t ,@form))
                        ((stringp keys)
                         `((string= ,keys ,evaled-keyform) ,@form))
                        ((listp keys)
                         (if (null (cdr keys))
                             `((string= ,(car keys) ,evaled-keyform)
                               ,@form)
                             `((or ,@(loop
                                        :for k :in keys
                                        :collect
                                        `(string= ,k ,evaled-keyform)))
                               ,@form)))))))))

(defun digest-string (digest-spec string)
  "Wrappar for ironclad:digest-sequence."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    digest-spec
    (flexi-streams:string-to-octets string :external-format :utf-8))))

(defun md5 (string) (digest-string :md5 string))

(defmacro unwind-protect-reserved-cleanup (cleanup-form
                                           &body long-protected-forms)
  "Same as unwind-protect, except for cleanup-form is the front of
protected-forms. For readablity purpose only, i.e. protected-form is
too long."
  `(unwind-protect
        (progn ,@long-protected-forms)
     ,cleanup-form))

;;; Extractable Format: <email>?\\s*( [ (<]<name>[ )>])?
;;; * (extract-email-name "geo@herald.com (George Matesky)")
;;;   => "geo@herald.com", "George Matesky"
;;; * (extract-email-name "geo@herald.com")
;;;   => "geo@herald.com", NIL
;;; * (extract-email-name "George Matesky")
;;;   => NIL, "George Matesky"
(defun extract-email-name (string)
  (if (find #\@ string)
      (aif (position #\Space string)
           (values
            (subseq string 0 it)
            (string-trim '(#\Space #\( #\) #\< #\>) (subseq string it)))
           (values string NIL))
      (values NIL
              (unless (string= "" string)
                (string-trim '(#\Space #\( #\) #\< #\>) string)))))


;;--------------------------------------------------------------------
;; Special Variables
;;--------------------------------------------------------------------

(defparameter *cl-feed-parser-version* "0.0.00")
(defparameter *cl-feed-parser-webpage*
  "https://github.com/tkych/cl-feed-parser")

(defparameter *user-agent*
  (format nil "CL-Feed-Parser/~A (~A ~A; ~A; ~A; ~A)"
          *cl-feed-parser-version*
          (lisp-implementation-type)
          (lisp-implementation-version)
          (software-type)
          (software-version)
          *cl-feed-parser-webpage*)
  "`*user-agent*' is strored the string which tells the server who
requests (i.e. User-Agent header value). If you are embedding
cl-feed-parser in a larger software, you should change the value of
`*user-agent*' to your software name and URL.")

(defparameter *304-report*
  "The feed has not changed since you last checked, so the server sent
no data. This is a feature, not a bug!")

(defvar *sanitizer*
  (lambda (html-string)
    (be-lazy (ignore-errors (sanitize:clean html-string))))
  "This variable is strored the function which sanitizes html-string.
The default value is the following:

      (lambda (html-string)
        (be-lazy (ignore-errors (sanitize:clean html-string))))

The above lazy function returns sanitized `html-string'.  If you want
to use the another sanitize-function or never to sanitize, you could
set this variable to the another sanitize-function or `#'identity`.  If
`html-string' is not html format, it returns NIL.")

(defvar *date-time-parser*
  (lambda (date-time-string)
    (be-lazy
     (ignore-errors
       (reduce #'+ (multiple-value-list
                    (cl-date-time-parser:parse-date-time
                     date-time-string))))))
  "This variable is strored the function which parses `date-time-string'
into universal time. The default value is the following:

      (lambda (date-time-string)
        (be-lazy
         (ignore-errors
           (reduce #'+ (multiple-value-list
                        (cl-date-time-parser:parse-date-time
                         date-time-string))))))

The above lazy function returns universal-time (plus fraction if exists).
 If you want to use the another date-time-parser or never to parse
date-time, you could set this variable to the another parser or
#'identity. If `date-time-string' is not date-time format, it returns
NIL.")


;; The following name-prefix data from Universal-Feed-Parser
(defparameter *namespaces*
  (alexandria:plist-hash-table
   '(;;"" ""
     "http://backend.userland.com/rss" ""
     "http://blogs.law.harvard.edu/tech/rss" ""
     "http://purl.org/rss/1.0/" ""
     "http://my.netscape.com/rdf/simple/0.9/" ""
     "http://example.com/newformat#" ""
     "http://example.com/necho" ""
     "http://purl.org/echo/" ""
     "uri/of/echo/namespace#" ""
     "http://purl.org/pie/" ""
     "http://purl.org/atom/ns#" ""
     "http://www.w3.org/2005/Atom" ""
     "http://purl.org/rss/1.0/modules/rss091#" ""

     "http://webns.net/mvcb/"                                "admin"
     "http://purl.org/rss/1.0/modules/aggregation/"          "ag"
     "http://purl.org/rss/1.0/modules/annotate/"             "annotate"
     "http://media.tangent.org/rss/1.0/"                     "audio"
     "http://backend.userland.com/blogChannelModule"         "blogChannel"
     "http://web.resource.org/cc/"                           "cc"
     "http://backend.userland.com/creativeCommonsRssModule"  "creativeCommons"
     "http://purl.org/rss/1.0/modules/company"               "co"
     "http://purl.org/rss/1.0/modules/content/"              "content"
     "http://my.theinfo.org/changed/1.0/rss/"                "cp"
     "http://purl.org/dc/elements/1.1/"                      "dc"
     "http://purl.org/dc/terms/"                             "dcterms"
     "http://purl.org/rss/1.0/modules/email/"                "email"
     "http://purl.org/rss/1.0/modules/event/"                "ev"
     "http://rssnamespace.org/feedburner/ext/1.0"            "feedburner"
     "http://freshmeat.net/rss/fm/"                          "fm"
     "http://xmlns.com/foaf/0.1/"                            "foaf"
     "http://www.w3.org/2003/01/geo/wgs84_pos#"              "geo"
     "http://postneo.com/icbm/"                              "icbm"
     "http://purl.org/rss/1.0/modules/image/"                "image"
     "http://www.itunes.com/DTDs/PodCast-1.0.dtd"            "itunes"
     "http://example.com/DTDs/PodCast-1.0.dtd"               "itunes"
     "http://purl.org/rss/1.0/modules/link/"                 "l"
     "http://search.yahoo.com/mrss"                          "media"
     ;; Version 1.1.2 of the Media RSS spec added the trailing slash on the namespace
     "http://search.yahoo.com/mrss/"                         "media"
     "http://madskills.com/public/xml/rss/module/pingback/"  "pingback"
     "http://prismstandard.org/namespaces/1.2/basic/"        "prism"
     "http://www.w3.org/1999/02/22-rdf-syntax-ns#"           "rdf"
     "http://www.w3.org/2000/01/rdf-schema#"                 "rdfs"
     "http://purl.org/rss/1.0/modules/reference/"            "ref"
     "http://purl.org/rss/1.0/modules/richequiv/"            "reqv"
     "http://purl.org/rss/1.0/modules/search/"               "search"
     "http://purl.org/rss/1.0/modules/slash/"                "slash"
     "http://schemas.xmlsoap.org/soap/envelope/"             "soap"
     "http://purl.org/rss/1.0/modules/servicestatus/"        "ss"
     "http://hacks.benhammersley.com/rss/streaming/"         "str"
     "http://purl.org/rss/1.0/modules/subscription/"         "sub"
     "http://purl.org/rss/1.0/modules/syndication/"          "sy"
     "http://schemas.pocketsoap.com/rss/myDescModule/"       "szf"
     "http://purl.org/rss/1.0/modules/taxonomy/"             "taxo"
     "http://purl.org/rss/1.0/modules/threading/"            "thr"
     "http://purl.org/rss/1.0/modules/textinput/"            "ti"
     "http://madskills.com/public/xml/rss/module/trackback/" "trackback"
     "http://wellformedweb.org/commentAPI/"                  "wfw"
     "http://purl.org/rss/1.0/modules/wiki/"                 "wiki"
     "http://www.w3.org/1999/xhtml"                          "xhtml"
     "http://www.w3.org/1999/xlink"                          "xlink"
     "http://www.w3.org/XML/1998/namespace"                  "xml"
     )
   :test #'equal))


;;--------------------------------------------------------------------
;; Conditions
;;--------------------------------------------------------------------

(define-condition feed-parser-condition (error)
  ())

(define-condition communication-failue (error)
  ((status-code   :reader status-code   :initarg :fail-status-code)
   (headers       :reader headers       :initarg :fail-headers)
   (reason-phrase :reader reason-phrase :initarg :fail-reason-phrase)))

(define-condition fatching-feed-faliure (feed-parser-condition
                                         communication-failue)
  ()
  (:report (lambda (condition stream)
             (with-slots (status-code reason-phrase) condition
               (format stream "Fail Fatching Feed: ~A ~A"
                       status-code reason-phrase)))))

(define-condition parse-failue (error)
  ())

(define-condition parsing-feed-failue (feed-parser-condition
                                       parse-failue)
  ())


;;--------------------------------------------------------------------
;; Containers
;;--------------------------------------------------------------------
(declaim (inline make-container %ref (setf %ref)))

(defun make-container () (make-hash-table :test #'equal))

(defun %ref (container key)
  (gethash key container nil))

(defun (setf %ref) (new-val container key)
  (setf (gethash key container) new-val))

(defun make-feed-data ()
  (let ((data (make-container)))
    (setf (%ref data "feed")    (make-container))
    (setf (%ref data "entries") (list))
    data))


;;--------------------------------------------------------------------
;; Parse-Feed: Main
;;--------------------------------------------------------------------

;; ?? response-headers
;; valid-namespaces

(defun parse-feed (url/pathname/xml-string/stream
                   &key etag
                     modified
                     (agent *user-agent*)
                     referer
                     authorization
                     request-headers
                     response-headers)
  "Parse the feed specified by `url/pathname/xml-string/stream', and
Return feed-data (hash-table) and result-report (T or condition).
If the argument is an url, first fetch the feed, then parse.

Keyword Options: (!! TODO !!)

 * etag -------------- 
 * modified ---------- 
 * agent ------------- 
 * referer ----------- 
 * authorization ----- 
 * request-headers --- 
 * response-headers -- 
"
  (check-type url/pathname/xml-string/stream (or string stream pathname))
  (check-type etag     (or null string))
  (check-type modified (or null string integer))
  (check-type agent    string)
  (check-type referer  (or null string))
  (check-type authorization    (or null (satisfies authorization-p)))
  (check-type request-headers  (satisfies request-header-p))
  ;; (check-type response-headers (or null ))

  (symbol-macrolet ((input url/pathname/xml-string/stream)) ;just abbrev
    (let ((data (make-feed-data)))
      (unwind-protect-reserved-cleanup
          ;; Cleanup-Form
          (when (typep input 'flexi-streams:flexi-stream)
            (close input))

        ;; Protected-Forms
        ;; 0. Fetch feed if input is url
        (when (and (stringp input)
                   (url-rewrite:starts-with-scheme-p input))
          (multiple-value-bind (stream last-status first-status header response-url)
              (handler-case
                  (fetch-feed input agent etag modified referer authorization
                              request-headers response-headers)
                (error (c)
                  ;; Stop processing if fatching feed is fail.
                  (format *error-output* "~&Fatch Fail: [~S] ~A~%"
                          (class-name (class-of c)) input)
                  (RETURN-FROM parse-feed (values NIL c))))
            (setf input stream)
            (setf (%ref data "header") header)
            (setf (%ref data "status") first-status)
            (setf (%ref data "href")   response-url)
            
            ;; Stop processing if the server sent status code 304 (Not Modified).
            (when (and (or etag modified)
                       (= 304 last-status))
              (setf (%ref data "debug-message") *304-report*)
              (RETURN-FROM parse-feed (values data T)))))
        
        ;; 1. Parse feed
        (handler-case
            (klacks:with-open-source (s (cxml:make-source input))
              (parse-encoding s data)
              (parse-version s data)
              (parse-contents s data)
              (setf (%ref data "bozo") 0))
          
          (cxml:xml-parse-error (c)
            (setf (%ref data "bozo") 1)
            (setf (%ref data "bozo-exception") c)
            (format *error-output* "~&Parse Fail: [~S] ~A~%"
                    (class-name (class-of c)) input)
            (RETURN-FROM parse-feed (values data c)))))
      
      (values data T))))


;;--------------------------------------------------------------------
;; Fatching Feeds
;;--------------------------------------------------------------------

(defvar *accept-header*
  (format nil "application/atom+xml,~
               application/rdf+xml,~
               application/rss+xml,~
               application/x-netcdf,~
               application/xml;q=0.9,~
               text/xml;q=0.2,~
               */*;q=0.1"))

(defun generate-rfc1123-timestamp (universal-time)
  (local-time:format-rfc1123-timestring
   nil (local-time:universal-to-timestamp universal-time)))

(defun request-header-p (x)
  (and (listp x)
       (every (lambda (y)
                (and (consp y)
                     (stringp (car y))
                     (stringp (cdr y))))
              x)))

(defun generate-additional-headers (etag modified referer request-headers)
  "Generate the value of drakma's option :additional-headers.
`etag' should be string escaped with double quote.
`modified' must be date-time-string with RFC1123 format or universal-time.
If universal time (integer) is supplied as `modified', it will convert into RFC1123 format.
`referer' should be uri-string."
  (let ((headers request-headers))
    (when etag
      (push (cons "If-None-Match" etag) headers))
    (when modified
      (push (cons "If-Modified-Since"
                  (etypecase modified
                    (string  modified)
                    (integer (generate-rfc1123-timestamp modified))))
            headers))
    (when referer
      (push (cons "Referer" referer) headers))
    (push (cons "A-IM" "feed") headers) ;c.f. rfc3229 10.5.3 A-IM
    
    (list :additional-headers headers)))


(defun fetch-feed (url agent etag modified referer authorization
                   request-headers response-headers)
  (declare (ignore response-headers))   ;for DEV
  (let* (;; Memo: default *drakma-default-external-format* is :latin-1
         (drakma:*drakma-default-external-format* :utf-8)
         (s (make-string-output-stream))
         (drakma:*header-stream* s))
    (unwind-protect-reserved-cleanup (close s)
      (let ((options (list :want-stream t :user-agent agent
                           :accept *accept-header*))
            (headers (generate-additional-headers
                      etag modified referer request-headers)))
        (multiple-value-bind (stream status-code header-alist response-url)
            (if authorization
                (authorization-request url authorization options headers)
                (normal-request url options headers))
          ;; Memo: default flexi-stream-external-format is :iso-8859-1
          (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
          (values stream
                  status-code
                  ;; Memo: 2013-07-21 by tkych
                  ;; Because the result of calling get-output-stream-string
                  ;; for closed stream is undefined, don't modify the following form
                  ;; to (be-lazy (get-output-stream-string s))
                  ;; c.f. CLHS, get-output-stream-string
                  (let ((header-string (get-output-stream-string s)))
                    (be-lazy (get-first-status-code header-string)))
                  (be-lazy (header-to-container header-alist))
                  (be-lazy (puri:render-uri response-url nil))))))))

(defun normal-request (url options headers &rest more-options)
  (apply #'drakma:http-request url (append options headers more-options)))

(defun get-first-status-code (header-string)
  (parse-integer
   (svref (nth-value
           1 (ppcre:scan-to-strings
              "HTTP/1.1 (\\d\\d\\d)" header-string))
          0)))

(defun header-to-container (header-alist)
  (let ((header (make-container)))
    (loop :for (key . val) :in header-alist :do
       (setf (%ref header (string-downcase key))
             val))
    header))


;;--------------------------------------------------------------------
;; Fatching Password Protected Feeds
;;--------------------------------------------------------------------

;; !!!!!!!! Not Tested !!!!!!!!!!!!

;; <authorization>      ::= (list <authorization-type> <user-name> <password>)
;; <authorization-type> ::= :basic | :digest | :proxy-basic
;; <user-name>          ::= <string>
;; <password>           ::= <string>

(defun authorization-p (x)
  (and (list x)
       (and (member  (first  x) '(:basic :proxy-basic :digest))
            (stringp (second x))
            (stringp (third  x)))))

(defun authorization-request (authorization url options headers)
  (destructuring-bind (auth-type user password) authorization
    (ecase auth-type
      (:basic
       (normal-request url options headers
                       :basic-authorization (list user password)))
      (:proxy-basic
       (normal-request url options headers
                       :proxy t
                       :proxy-basic-authorization (list user password)))
      (:digest
       (digest-auth-request url user password options headers))
      )))


;;--------------------------------------
;; Digest Authentication
;;--------------------------------------

(defun digest-auth-request (url user password options request-headers)
  "Digest Authentication HTTP Request, c.f. RFC 2617"
  (multiple-value-bind
        (_ status headers __ ___ ____ reason) (drakma:http-request url)
    (declare (ignore _ __ ___ ____))
    (if (and (/= status 401) (/= status 407))
        (error 'fatching-feed-faliure :fail-status-code   status
                                      :fail-headers       headers
                                      :fail-reason-phrase reason)
        (multiple-value-bind (realm domain nonce opaque algorithm qop)
            (parse-challenge (drakma:header-value :www-authenticate headers))
          (push (cons (ecase status
                        (401 "Authorization")
                        (407 "Proxy-Authorization"))
                      (generate-credentials user password realm domain
                                            nonce opaque algorithm qop))
                request-headers)
          (multiple-value-bind (auth-response auth-status auth-headers auth-url
                                ~ ~~ auth-reason)
              (normal-request url options request-headers)
            (declare (ignore ~ ~~))
            (if (= auth-status 400)
                (error 'fatching-feed-faliure :fail-status-code   auth-status
                                              :fail-headers       auth-headers
                                              :fail-reason-phrase auth-reason)
                (values auth-response auth-status auth-headers auth-url)))))))

(defun parse-challenge (challenge)
  (let* ((digest-challenge (subseq challenge
                                   (nth-value 1 (ppcre:scan "Digest " challenge))))
         (challenge-list   (ppcre:split ",\\s*" digest-challenge))
         realm domain nonce opaque algorithm qop)
    (iter (for directive-value :in challenge-list)
          (ppcre:register-groups-bind
              (dir val) ("(\\w+)=\"?([0-9a-zA-z= /.-_]+)\"?" directive-value) ;!! regex
            (dispatch-string dir
              ("realm"     (setf realm     val))
              ("domain"    (setf domain    val))
              ("nonce"     (setf nonce     val))
              ("opaque"    (setf opaque    val))
              ;; ("stale"     (setf stale     val)) omit stale for fatching feed
              ("algorithm" (setf algorithm val))
              ("qop"       (setf qop       val))
              (t nil))))  ;Any unrecognized directive MUST be ignored.
    (values realm domain nonce opaque algorithm qop)))

(defun generate-credentials (user password realm digit-uri
                             nonce opaque algorithm qop)
  (let* ((nc      (get-nonce-count))
         (cnonce  (generate-client-nonce))
         (a1      (dispatch-string algorithm
                    ("MD5"      (str user ":" realm ":" password))
                    ("MD5-sess" (str (md5 (str user ":" realm ":" password))
                                     ":" nonce ":" cnonce))
                    (t          (str user ":" realm ":" password))))
         (a2      (str "GET :" digit-uri)) ;suppose qop is only "auth"
         (request (if qop
                      (md5 (str (md5 a1) ":" nonce ":" nc ":" cnonce ":" qop ":" (md5 a2)))
                      (md5 (str (md5 a1) ":" nonce ":" (md5 a2)))))) ;for compatibility with rfc2069
    (str (format nil "Digest username=~S, realm=~S, nonce=~S, uri=~S, response=~S, nc=~A, cnonce=~S"
                 user realm nonce digit-uri request nc cnonce)
         (if qop       (format nil ", qop=~S" qop)       "")
         (if opaque    (format nil ", opaque=~S" opaque) "")
         (if algorithm (format nil ", algorithm=~S" algorithm) ""))))

(let ((count 0))
  (defun get-nonce-count ()
    (format nil "~(~8,'0X~)" (incf count))))

(defun generate-client-nonce ()
  (md5 (with-output-to-string (s)
         ;; Get date-time-string as unique string
         (let ((date-time-string
                (local-time:format-timestring
                 nil (local-time:now)
                 :format '(:year :month :day :hour :min :sec :nsec))))
           (write-string date-time-string s))
         ;; Add salt
         (loop
            ;; ?FIXME: ironclad:make-random-salt is not secure-random.
            ;; need? secure-random?, check!
            :for o :across (ironclad:make-random-salt)
            :do (write-char (code-char o) s)))))


;;--------------------------------------------------------------------
;; Encoding, Version
;;--------------------------------------------------------------------

(defun parse-encoding (source data)
  (setf (%ref data "encoding")
        (nth-value 2 (klacks:find-event source :start-document))))

(defun detect-feed-type-version (source)
  (iter (for (values event uri lname) := (klacks:find-event source :start-element))
        (dispatch-string lname
          ("feed" (return
                    (if (string= uri "http://www.w3.org/2005/Atom") ;!! version 1.0
                        "atom-1.0"
                        (dispatch-string (klacks:get-attribute source "version")
                          ("0.1" "atom-0.1")
                          ("0.2" "atom-0.2")
                          ("0.3" "atom-0.3")
                          (t     "atom")))))
          ("RDF" (return
                   (if (rassoc "http://purl.org/rss/1.0/"
                               (slot-value source 'cxml::current-namespace-declarations)
                               :test #'string=)
                       "rss-1.0"         ;!! :rss-0.90
                       "rss")))
          ("rss" (return
                   (dispatch-string (klacks:get-attribute source "version")
                     (("2.0" "2") "rss-2.0")
                     ("0.90"      "rss-0.90")
                     ("0.91"      "rss-0.91n") ;!!!
                     ;; ("0.91"      "rss091u") ;!!!
                     ("0.92"      "rss-0.92")
                     ("0.93"      "rss-0.93")
                     ("0.94"      "rss-0.93")
                     (t           "rss"))))
          ("channel" (return "cdf"))
          (t nil))
        (klacks:peek-next source)))

(defun parse-version (source data)
  (setf (%ref data "version") (detect-feed-type-version source)))


;;--------------------------------------------------------------------
;; Parse-Contents
;;--------------------------------------------------------------------

(defun parse-contents (source data)
  (dispatch-string (%ref data "version")
    ;; Major version
    ("atom-1.0"
     (parse-atom source data))
    ("rss-2.0"
     (parse-rss2.0-genus source data))
    ("rss-1.0"
     (parse-rss1.0-genus source data))
    ;; Minor version
    (("atom-0.3" "atom" "atom-0.2" "atom-0.1")
     (parse-atom source data))
    (("rss-0.91n" "rss-0.91u" "rss-0.92" "rss-0.93" "rss-0.94")
     (parse-rss2.0-genus source data))
    ("rss-0.90"
     (parse-rss1.0-genus source data))
    ("rss"            ;:rss -> 1.0 or 2.0?
     ;; (or (parse-rss1.0-entries source feed)
     ;;     (parse-entries-rss2.0-genus source feed))
     )
    ("cdf"
     (parse-cdf source data))
    (t
     (error "Unkown feed format."))))

(defun peek-all-chars (source)
  ;;; <foo>bar baz</foo> => "bar baz"
  (iter (for (values event data) := (klacks:peek-next source))
        (until (eq event :end-element))
        (when (eq event :characters)
          (collect data :into acc))
        (finally (return (apply #'str acc)))))

(defun parse-namespaces (source data except-for)
  ;;; Parse namespaces from the `source', and Store data into `container'.
  ;;; If the namespace is in the `except-for', it is not stored.
  (let ((namespaces (make-container)))
    (dolist (attr (klacks:list-attributes source))
      (unless (find (sax:attribute-qname attr) except-for :test #'string=)
        (setf (%ref namespaces (or (sax:attribute-local-name attr) ""))
              (sax:attribute-value attr))))
    (setf (%ref data "namespaces") namespaces)))

(defun parse-attributes (source container canonical-names tag-name)
  ;;; Parse attributes in the `tag-name' which is on the current parsing
  ;;; position for `source', and Store data into `container'. The local-name
  ;;; of attribute is converted into qualified-name by `canonical-names'.
  (dolist (attr (klacks:list-attributes source))
    (let* ((qname (sax:attribute-qname attr))
           (cname (cdr (assoc qname canonical-names :test #'string=))))
      (if cname
          (setf (%ref container cname) (sax:attribute-value attr))
          (progn
            (when tag-name
              (format t "~&Info: ~S is not predefined attribute for <~(~A~)>.~%"
                      (sax:attribute-qname attr) tag-name))
            (setf (%ref container qname) (sax:attribute-value attr)))))))

(defun get-all-attributes (source)
  (loop
     :for attr :in (klacks:list-attributes source)
     :collect (cons (sax:attribute-qname attr) (sax:attribute-value attr))))

(defun parse-element (source container qname dispatches feed-type)
  ;;; Parse `qname' element which is on the current parsing position
  ;;; for `source', and Store data into `container'.
  (aif (gethash qname dispatches nil)
       (funcall it container source)
       (multiple-value-bind (_ uri lname __) (klacks:peek source)
         (declare (ignore _ __))
         (aif (gethash uri *namespaces* nil) ;!!ADD: check consistency with current-feed ns
              (let ((keyname (format nil "~(~A-~A~)" it lname)))
                (setf (%ref container keyname)
                      (aif (get-all-attributes source)
                           (acons "value" (peek-all-chars source) it)
                           (peek-all-chars source))))
              (format t "~&INFO: <~A> is not parsed as ~A element.~%"
                      qname feed-type)))))

(defun parse-sub-elements (s container lname-keyname-alist paren-tag-name)
  ;;; Parse sub-elements in the `paren-tag-name' which is on the current parsing
  ;;; position for `source', and Store data into `container'. The lname-string
  ;;; of attribute is converted into keyword by `lname-keyname-alist'.
  (iter (for (values event data lname qname) := (klacks:peek-next s))
        (until (and (eq event :end-element) (equal lname paren-tag-name)))
        (when lname
          (let ((keyname (cdr (assoc lname lname-keyname-alist :test #'string=))))
            (if keyname
                (setf (%ref container keyname) (peek-all-chars s))
                (progn
                  (format t "~&Info: ~(~A~) is not predefined sub element for <~(~A~)>.~%"
                          qname paren-tag-name)
                  (setf (%ref container lname) (peek-all-chars s))))))))


;;--------------------------------------------------------------------
;; Parse Atom
;;--------------------------------------------------------------------

(defparameter *dispatch-for-atom-elements*
  ;; Dispatch hash-table for parsing atom-elements.
  ;; [keyword, value]    ::= [<local-tag-name>, <dispatch-function>]
  ;; <dispatch-function> ::= (lambda (container source) <parse-body>)
  (alexandria:plist-hash-table
   (list
    "author" (lambda (f/e s)
               (let ((new-author-detail (make-container)))
                 (parse-parson-element s new-author-detail "author")
                 (unless (%ref f/e "author")
                   (setf (%ref f/e "author")
                         (aif (%ref new-author-detail "email")
                              (format nil "~A (~A)"
                                      (%ref new-author-detail "name") it)
                              (%ref new-author-detail "name")))
                   (setf (%ref f/e "author-detail") new-author-detail))
                 (alexandria:appendf (%ref f/e "authors") (list new-author-detail))))

    "category" (lambda (f/e s)
                 (let ((new-tag (make-container)))
                   (parse-attributes s new-tag
                                     '(("term" . "term") ("scheme" . "scheme")
                                       ("label" . "label"))
                                     "category")
                   (alexandria:appendf (%ref f/e "tags") (list new-tag))))
    
    "content" (lambda (e s)
                (let ((new-content (make-container)))
                  (parse-atom-common-attributes s new-content "content")
                  ;; (parse-attributes s new-content
                  ;;                    '(("type" . :type) ("src" . :src))
                  ;;                    "content")
                  (setf (%ref new-content "value") (funcall *sanitizer* (peek-all-chars s)))
                  (alexandria:appendf (%ref e "content") (list new-content))))
    
    "contributor" (lambda (f/e s)
                    (let ((new-contributor (make-container)))
                      (parse-parson-element s new-contributor "contributor")
                      (alexandria:appendf (%ref f/e "contributors") (list new-contributor))))

    "copyright" (lambda (e s)
                  (let ((new-rights-detail (make-container)))
                    (parse-atom-common-attributes s new-rights-detail "rights")
                    (let ((value (peek-all-chars s)))
                      (setf (%ref e "rights") (funcall *sanitizer* value))
                      (setf (%ref new-rights-detail "value") value))
                    (setf (%ref e "rights-detail") new-rights-detail)))
    
    "generator" (lambda (f s)
                  (let ((new-generator-detail (make-container)))
                    (parse-attributes s new-generator-detail
                                      '(("name" . "name") ("uri" . "href") ("version" . "version"))
                                      "generator")
                    (let ((name (peek-all-chars s)))
                      (setf (%ref f "generator") name
                            (%ref new-generator-detail "name") name))
                    (setf (%ref f "generator-detail") new-generator-detail)))
    
    "icon" (lambda (f s) (setf (%ref f "icon") (peek-all-chars s)))

    "id"   (lambda (f/e s) (setf (%ref f/e "id") (peek-all-chars s)))

    "link" (lambda (f/e s)              ;!! + get home attr or uri
             (let ((new-link (make-container)))
               (parse-attributes s new-link
                                 '(("rel" . "rel") ("type" . "type") ("href" . "href")
                                   ("hreflang" . "hreflang") ("length" . "length") ("title" . "title"))
                                 "link")
               (alexandria:appendf (%ref f/e "links") (list new-link))))
    
    "logo" (lambda (f s) (setf (%ref f "logo") (peek-all-chars s)))

    "modified" (lambda (e s) (let ((date (peek-all-chars s)))
                               (setf (%ref e "updated") date)
                               (setf (%ref e "updated-parsed")
                                     (funcall *date-time-parser* date))))

    "published" (lambda (e s) (let ((date (peek-all-chars s)))
                                (setf (%ref e "published") date)
                                (setf (%ref e "published-parsed")
                                      (funcall *date-time-parser* date))))
    
    "rights" (lambda (e s)
               (let ((new-rights-detail (make-container)))
                 (parse-atom-common-attributes s new-rights-detail "rights")
                 (let ((value (peek-all-chars s)))
                   (setf (%ref e "rights") (funcall *sanitizer* value)
                         (%ref new-rights-detail "value") value))
                 (setf (%ref e "rights-detail") new-rights-detail)))

    "source" (lambda (e s)
               (let ((new-entry (make-container)))
                 (iter (for (values event uri/data lname) := (klacks:peek-next s))
                       (case event
                         (:start-element (parse-atom-element s new-entry lname))
                         (:end-element   (when (equal lname "source")
                                           (finish)))))
                 (setf (%ref e "source") new-entry)))
    
    "subtitle" (lambda (e s)
                 (let ((new-subtitle-detail (make-container)))
                   (parse-atom-common-attributes s new-subtitle-detail "subtitle")
                   (let ((value (peek-all-chars s)))
                     (setf (%ref e "subtitle") (funcall *sanitizer* value)
                           (%ref new-subtitle-detail "value") value))
                   (setf (%ref e "subtitle-detail") new-subtitle-detail)))

    "summary" (lambda (e s)
                (let ((new-summary-detail (make-container)))
                  (parse-atom-common-attributes s new-summary-detail "summary")
                  (let ((value (peek-all-chars s)))
                    (setf (%ref e "summary") (funcall *sanitizer* value)
                          (%ref new-summary-detail "value") value))
                  (setf (%ref e "summary-detail") new-summary-detail)))
    
    "title" (lambda (e s)
              (let ((new-title-detail (make-container)))
                (parse-atom-common-attributes s new-title-detail "title")
                (let ((value (peek-all-chars s)))
                  (setf (%ref e "title") (funcall *sanitizer* value)
                        (%ref new-title-detail "value") value))
                (setf (%ref e "title-detail") new-title-detail)))

    "updated" (lambda (f/e s)
                (let ((date (peek-all-chars s)))
                  (setf (%ref f/e "updated") date)
                  (setf (%ref f/e "updated-parsed")
                        (funcall *date-time-parser* date))))
    )
   :test #'equal))

(defun parse-atom (source data)
  (klacks:find-element source "feed")
  ;; parse <feed> common attributes
  (parse-namespaces source data '("xml:base" "xml:lang"))
  (dolist (attr (klacks:list-attributes source))
    (dispatch-string (sax:attribute-qname attr)
      ("xml:base" (setf (%ref data "base")     (sax:attribute-value attr)))
      ("xml:lang" (setf (%ref data "language") (sax:attribute-value attr)))))
  ;; parse elements in <feed> 
  (iter (for (values event uri/data lname) := (klacks:peek-next source))
        (cond
          ;; parse elements in <entry>
          ((equal lname "entry")     (parse-atom-entry source data))
          ;; parse elements in <feed> (except for <entry>)
          ((eq event :start-element) (parse-atom-element source (%ref data "feed") lname))
          ;; finish parse atom
          ((eq event :end-document)  (finish))
          (t                         nil))))

(defun parse-atom-element (source feed/entry qname)
  (parse-element source feed/entry qname
                 *dispatch-for-atom-elements* "Atom 1.0"))

;; !Add: parse common-attr
(defun parse-atom-entry (source data)
  (let ((new-entry (make-container)))
    (iter (for (values event uri/data lname) := (klacks:peek-next source))
          (case event
            ;; parse elements in <entry>
            (:start-element (parse-atom-element source new-entry lname))
            ;; finish parse <entry>
            (:end-element   (when (equal lname "entry")
                              (finish)))
            (t              nil)))
    (alexandria:appendf (%ref data "entries") (list new-entry))))

(defun parse-atom-common-attributes (source container tag-name)
  ;; xml:base, xml:lang, (+ type)
  (parse-attributes source container
                    '(("type" . "type") ("lang" . "language") ("base" . "base"))
                    tag-name))

(defun parse-parson-element (source container tag-name)
  (parse-sub-elements source container
                      '(("name" . "name") ("uri" . "href") ("email" . "email"))
                      tag-name))


;;--------------------------------------------------------------------
;; Parse RSS 2.0 Genus
;;--------------------------------------------------------------------

(defparameter *parsing-entry?* nil
  "T if currently parsing an entry, otherwise NIL.
Controls behaviar of parse-element-dispatch.")

(defparameter *dispatch-for-rss2.0-elements*
  ;; ! ADD: "rating"
  ;; Dispatch hash-table for parsing rss2.0-elements.
  ;; [keyword, value]    ::= [<local-tag-name>, <dispatch-function>]
  ;; <dispatch-function> ::= (lambda (container source) <parse-body>)
  (alexandria:plist-hash-table
   (list
    "author" (lambda (e s)
               (let ((new-author-detail (make-container))
                     (email-name (peek-all-chars s)))
                 (multiple-value-bind (email name) (extract-email-name email-name)
                   (when email (setf (%ref new-author-detail "email") email))
                   (when name  (setf (%ref new-author-detail "name") name))
                   (if (%ref e "author")
                       (alexandria:appendf (%ref e "authors") (list new-author-detail))
                       (setf (%ref e "author") (or name email)
                             (%ref e "author-detail") new-author-detail)))))

    "category" (lambda (f/e s)
                 (let ((new-tag (make-container)))
                   ;; !! Add: domain,   ??"scheme", "label"??
                   (setf (%ref new-tag "term") (peek-all-chars s))
                   (alexandria:appendf (%ref f/e "tags") (list new-tag))))
    
    "source" (lambda (e s)
               (let ((new-source (make-container)))
                 (parse-attributes s new-source '(("url" . "url")) "source")
                 (setf (%ref new-source "value") (peek-all-chars s))
                 (setf (%ref e "source") new-source)))

    "cloud" (lambda (f s)
              (let ((new-cloud (make-container)))
                (parse-attributes s new-cloud
                                  '(("domain" . "domain") ("port" . "port") ("path" . "path")
                                    ("registerProcedure" . "registerProcedure")
                                    ("protcol" . "protcol"))
                                  "cloud")
                (setf (%ref f "cloud") new-cloud)))

    "comments" (lambda (f s) (setf (%ref f "comments") (peek-all-chars s))) 

    "description" (lambda (f/e s)
                    ;; Memo: rss2.0 has two kinds of "description".
                    ;;  1. in <channel> -> :subtitle in :feed
                    ;;  2. in <item>    -> :summary  in :entry
                    (if *parsing-entry?*
                        (let ((new-summary-detail (make-container))
                              (value (peek-all-chars s)))
                          (setf (%ref f/e "summary") (funcall *sanitizer* value)
                                (%ref new-summary-detail "value") value
                                ;; !ADD!
                                ;; (%ref new-summary-detail :type) type
                                ;; (%ref new-summary-detail :base) base
                                )
                          (setf (%ref f/e "summary-detail") new-summary-detail))

                        (let ((new-subtitle-detail (make-container))
                              (value (peek-all-chars s)))
                          (setf (%ref f/e "subtitle") (funcall *sanitizer* value)
                                (%ref new-subtitle-detail "value") value
                                ;; !ADD!
                                ;; (%ref new-subtitle-detail :type) type
                                ;; (%ref new-subtitle-detail :base) base
                                )
                          (setf (%ref f/e "subtitle-detail") new-subtitle-detail))))
    
    "docs" (lambda (f s) (setf (%ref f "docs") (peek-all-chars s)))

    "enclosure" (lambda (e s)
                  (let ((new-enclosure (make-container)))
                    (parse-attributes s new-enclosure
                                      '(("url" . "href") ("length" . "length") ("type" . "type"))
                                      "enclosure")
                    (setf (%ref e "enclosure") new-enclosure)))

    "generator" (lambda (f s)
                  (let ((new-generator-detail (make-container)))
                    (parse-attributes s new-generator-detail
                                      '(("name" . "name") ("uri" . "href") ("version" . "version"))
                                      "generator")
                    (let ((name (peek-all-chars s)))
                      (setf (%ref f "generator") name
                            (%ref new-generator-detail "name") name))
                    (setf (%ref f "generator-detail") new-generator-detail)))
    
    "guid" (lambda (f/e s)
             (setf (%ref f/e "guid-is-link") t)
             (dolist (attr (klacks:list-attributes s))
               (when (and (equal "isPermaLink" (sax:attribute-local-name attr))
                          (equal "false" (sax:attribute-value attr)))
                 (setf (%ref f/e "guid-is-link") nil)))
             (setf (%ref f/e "id") (peek-all-chars s)))

    "language" (lambda (f s) (setf (%ref f "language") (peek-all-chars s)))

    "lastBuildDate" (lambda (f/e s)
                      (let ((date (peek-all-chars s)))
                        (setf (%ref f/e "updated") date)
                        (setf (%ref f/e "updated-parsed")
                              (funcall *date-time-parser* date))))

    "link" (lambda (f/e s)              ;!! + get home attr or uri
             (let ((new-link (make-container)))
               ;; !! add: detect by BS
               (setf (%ref f/e "link") (peek-all-chars s))
               (setf (%ref new-link "href") (%ref f/e "link"))
               (alexandria:appendf (%ref f/e "links") (list new-link))))

    "managingEditor" (lambda (f s)
                       (let ((new-author-detail (make-container))
                             (email-name (peek-all-chars s)))
                         (multiple-value-bind (email name) (extract-email-name email-name)
                           (setf (%ref new-author-detail "email") email)
                           (when name (setf (%ref new-author-detail "name") name))
                           (unless (%ref f "author")
                             (setf (%ref f "author") (or name email))
                             (setf (%ref f "author-detail") new-author-detail)))
                         (alexandria:appendf (%ref f "authors") (list new-author-detail))))

    "textInput" (lambda (f s)
                  (let ((new-text (make-container)))
                    (parse-sub-elements s new-text
                                        '(("title" . "title") ("description" . "description")
                                          ("name" . "name") ("link" . "link"))
                                        "textInput")
                    (setf (%ref f "textInput") new-text)))

    "image" (lambda (f s)
              (let ((new-image (make-container)))
                (parse-sub-elements s new-image
                                    '(("url" . "url") ("title" . "title") ("link" . "link")
                                      ("width" . "width") ("height" . "height")
                                      ("description" . "description"))
                                    "image")
                (setf (%ref f "image") new-image)))

    "pubDate" (lambda (e s) (let ((date (peek-all-chars s)))
                              (setf (%ref e "published") date)
                              (setf (%ref e "published-parsed")
                                    (funcall *date-time-parser* date))))

    "copyright" (lambda (e s)
                  (let ((new-rights-detail (make-container)))
                    (parse-atom-common-attributes s new-rights-detail "rights")
                    (let ((value (peek-all-chars s)))
                      (setf (%ref e "rights") (funcall *sanitizer* value)
                            (%ref new-rights-detail "value") value))
                    (setf (%ref e "rights-detail") new-rights-detail)))

    "skipHours" (lambda (f s)
                  (let ((hours '()))
                    (iter (for (values event data lname) := (klacks:peek-next s))
                          (until (and (eq event :end-element) (equal lname "skipHours")))
                          (when (and lname (equal lname "hour"))
                            (push (parse-integer (peek-all-chars s)) hours)))
                    (setf (%ref f "skipHours") (nreverse hours))))

    "skipDays" (lambda (f s)
                 (let ((days '()))
                   (iter (for (values event data lname) := (klacks:peek-next s))
                         (until (and (eq event :end-element) (equal lname "skipDays")))
                         (when (and lname (equal lname "day"))
                           (push (peek-all-chars s) days)))
                   (setf (%ref f "skipDays") (nreverse days))))

    "title" (lambda (e s)
              (let ((new-title-detail (make-container)))
                (let ((value (peek-all-chars s)))
                  (setf (%ref e "title") (funcall *sanitizer* value)
                        (%ref new-title-detail "value") value))
                (setf (%ref e "title-detail") new-title-detail)))

    "ttl" (lambda (f s) (let ((minutes (peek-all-chars s)))
                          (setf (%ref f "ttl") (be-lazy (parse-integer minutes)))))
    
    "webMaster" (lambda (f s)
                  (let ((new-publisher-detail (make-container))
                        (email-name (peek-all-chars s)))
                    (multiple-value-bind (email name) (extract-email-name email-name)
                      (setf (%ref new-publisher-detail "email") email)
                      (when name (setf (%ref new-publisher-detail "name") name))
                      (unless (%ref f "publisher")
                        (setf (%ref f "publisher") (or name email))
                        (setf (%ref f "publisher-detail") new-publisher-detail)))
                    (alexandria:appendf (%ref f "publishers") (list new-publisher-detail))))
    
    )
   :test #'equal))

(defun parse-rss2.0-genus (source data)
  (klacks:find-element source "rss")
  ;; parse <rss> common attributes
  (parse-namespaces source data '("version"))
  (klacks:find-element source "channel")
  ;; parse elements in <channel>
  (iter (for (values event uri/data lname) := (klacks:peek-next source))
        (cond
          ;; parse elements in <item>
          ((equal lname "item")      (let ((*parsing-entry?* t))
                                       (parse-rss2.0-entry source data)))
          ;; parse elements in <channel> (except for <item>)
          ((eq event :start-element) (parse-rss2.0-element
                                      source (%ref data "feed") lname))
          ;; finish parse atom
          ((eq event :end-document)  (finish))
          (t                         nil))))

(defun parse-rss2.0-element (source feed/entry qname)
  (parse-element source feed/entry qname
                 *dispatch-for-rss2.0-elements* "RSS 2.0"))

(defun parse-rss2.0-entry (source data)
  (let ((new-entry (make-container)))
    (iter (for (values event uri/data lname) := (klacks:peek-next source))
          (case event
            ;; parse elements in <item>
            (:start-element (parse-rss2.0-element source new-entry lname))
            ;; finish parse <item>
            (:end-element   (when (equal lname "item")
                              (finish)))
            (t              nil)))
    (alexandria:appendf (%ref data "entries") (list new-entry))))


;;--------------------------------------------------------------------
;; Parse RSS 1.0
;;--------------------------------------------------------------------

(defparameter *dispatch-for-rss1.0-elements*
  ;; Dispatch hash-table for parsing rss1.0-elements.
  ;; [keyword, value]    ::= [<local-tag-name>, <dispatch-function>]
  ;; <dispatch-function> ::= (lambda (container source) <parse-body>)
  
  ;; "items"
  ;; "textinput"
  ;; "name"
  ;; "url"
  ;; "image"

  ;; "dc:creator"  -> "author, "author-detail"
  ;; "dc:date"     -> "updated, "updated-parsed"
  ;; "dc:language" -> "language"
  ;; "dc:subject"  -> "tags"
  ;; "dc:contributor"  -> :contributor, :contributor
  ;; "dc:publisher"    -> 

  ;; "content:encoded" -> :content
  ;; "admin:generatorAgent" attr "rdf:resource" -> :generator, :generator-detail
  ;; "rdf:about" -> :link
  ;; "description" -> :summary-detail

  (alexandria:plist-hash-table
   (list
    "admin:generatorAgent" (lambda (f s)
                             (let ((new-generator-detail (make-container)))
                               (parse-attributes s new-generator-detail
                                                 '(("resource" . "href"))
                                                 "generator")
                               (setf (%ref f "generator") (%ref new-generator-detail "href"))
                               (setf (%ref f "generator-detail") new-generator-detail)))

    "content:encoded"  (lambda (f/e s)
                         (let ((new-summary-detail (make-container))
                               (value (peek-all-chars s)))
                           (setf (%ref f/e "summary") (funcall *sanitizer* value)
                                 (%ref new-summary-detail "value") value
                                 ;; !ADD!
                                 ;; (%ref new-summary-detail :type) type
                                 ;; (%ref new-summary-detail :base) base
                                 )
                           (setf (%ref f/e "summary-detail") new-summary-detail)))

    "dc:contributor" (lambda (f/e s)
                       (let ((new-contributor (make-container)))
                         (parse-parson-element s new-contributor "contributor")
                         (alexandria:appendf (%ref f/e "contributors") (list new-contributor))))

    "dc:creator" (lambda (e s)
                   (let ((new-author-detail (make-container))
                         (email-name (peek-all-chars s)))
                     (multiple-value-bind (email name) (extract-email-name email-name)
                       (when name (setf (%ref new-author-detail "name") name))
                       (when email (setf (%ref new-author-detail "email") email))
                       (if (%ref e "author")
                           (alexandria:appendf (%ref e "authors") (list new-author-detail))
                           (setf (%ref e "author") (or name email)
                                 (%ref e "author-detail") new-author-detail)))))

    "dc:date" (lambda (f/e s)
                (let ((date (peek-all-chars s)))
                  (setf (%ref f/e "updated") date)
                  (setf (%ref f/e "updated-parsed") (funcall *date-time-parser* date))))

    "dc:identifier" (lambda (f/e s) (setf (%ref f/e "id") (peek-all-chars s)))

    "dc:language" (lambda (f s) (setf (%ref f "language") (peek-all-chars s)))

    ;; "dc:publisher"

    "dc:rights" (lambda (e s)
                  (let ((new-rights-detail (make-container)))
                    (let ((value (peek-all-chars s)))
                      (setf (%ref e "rights") (funcall *sanitizer* value)
                            (%ref new-rights-detail "value") value))
                    (setf (%ref e "rights-detail") new-rights-detail)))
    
    "dc:subject" (lambda (f/e s)
                   (let ((new-tag (make-container)))
                     (setf (%ref new-tag "term") (peek-all-chars s))
                     (alexandria:appendf (%ref f/e "tags") (list new-tag))))

    "description" (lambda (f/e s)
                    (let ((new-subtitle-detail (make-container))
                          (value (peek-all-chars s)))
                      (setf (%ref f/e "subtitle") (funcall *sanitizer* value)
                            (%ref new-subtitle-detail "value") value
                            ;; !ADD!
                            ;; (%ref new-subtitle-detail "type") type
                            ;; (%ref new-subtitle-detail "base") base
                            )
                      (setf (%ref f/e "subtitle-detail") new-subtitle-detail)))

    "items" (lambda (f/e s) (declare (ignore f/e s)) nil)
    
    "link" (lambda (f/e s)
             (let ((new-link (make-container)))
               (setf (%ref f/e "link") (peek-all-chars s))
               (setf (%ref new-link "href") (%ref f/e "link"))
               (alexandria:appendf (%ref f/e "links") (list new-link))))
    
    "title" (lambda (f s)
              (let ((new-title-detail (make-container)))
                (let ((value (peek-all-chars s)))
                  (setf (%ref f "title") (funcall *sanitizer* value)
                        (%ref new-title-detail "value") value))
                (setf (%ref f "title-detail") new-title-detail)))
    )
   :test #'equal))

(defun parse-rss1.0-genus (source data)
  (klacks:find-element source "RDF")
  (parse-namespaces source data nil)
  (klacks:find-element source "channel")
  (iter (for (values event _ __ qname) := (klacks:peek-next source))
        (dispatch-string qname
          ("item"     (let ((*parsing-entry?* t))
                        (parse-rss1.0-entry source data "item" "entries")))
          ("image"    (let ((*parsing-entry?* t))
                        (parse-rss1.0-entry source data "image" "images")))
          ("textinput" (let ((*parsing-entry?* t))
                        (parse-rss1.0-entry source data "textinput" "textinput")))
          (t
           (case event
             (:start-element (parse-rss1.0-element
                              source (%ref data "feed") qname))
             (:end-document  (finish))
             (t              nil))))))

(defun parse-rss1.0-entry (source data entry-tag entry-key)
  (let ((new-entry (make-container)))
    (iter (for (values event _ __ qname) := (klacks:peek-next source))
          (case event
            ;; parse elements in <item>
            (:start-element (parse-rss1.0-element source new-entry qname))
            ;; finish parse <item>
            (:end-element   (when (equal qname entry-tag)
                              (finish)))
            (t              nil)))
    (alexandria:appendf (%ref data entry-key) (list new-entry))))

(defun parse-rss1.0-element (source feed/entry qname)
  (parse-element source feed/entry qname
                 *dispatch-for-rss1.0-elements* "RSS 1.0"))


;;--------------------------------------------------------------------
;; Parse CDF
;;--------------------------------------------------------------------

;; (defparameter *dispatch-for-cdf-elements*
;;   (alexandria:plist-hash-table
;;    (list
;;     )
;;    :test #'equal))

(defun parse-cdf (source feed)
  ;; !! STUB !!
  (declare (ignore source feed))
  (warn "CDF feed is under implementation."))

;; (defun parse-cdf-element (source feed/entry qname)
;;   (parse-element source feed/entry qname
;;                  *dispatch-for-cdf-elements* "CDF"))


;;--------------------------------------------------------------------
;; Ref
;;--------------------------------------------------------------------

(defparameter *key-map*
  (alexandria:plist-hash-table
   '("channel"            ("feed")
     "feed"               ("channel")
     "items"              ("entries")
     "entries"            ("items")
     "guid"               ("id")
     "id"                 ("guid")
     "date"               ("updated")
     "updated"            ("date")
     "date-parsed"        ("updated-parsed")
     "updated-parsed"     ("date-parsed")
     "description"        ("summary" "subtitle") ;! UGLY
     "summary"            ("description")
     "subtitle"           ("description")
     "description-detail" ("summary-detail" "subtitle-detail") ;! UGLY
     "summary-detail"     ("description-detail")
     "subtitle-detail"    ("description-detail")
     "url"                ("href")
     "href"               ("url")
     "modified"           ("updated")
     "updated"            ("modified")
     "modified-parsed"    ("updated-parsed")
     "updated-parsed"     ("modified-parsed")
     "issued"             ("published")
     "published"          ("issued")
     "issued-parsed"      ("published-parsed")
     "published-parsed"   ("issued-parsed")
     "copyright"          ("rights")
     "rights"             ("copyright")
     "copyright-detail"   ("rights-detail")
     "rights-detail"      ("copyright-detail")
     "tagline"            ("subtitle")
     "subtitle"           ("tagline")
     "tagline-detail"     ("subtitle-detail")
     "subtitle-detail"    ("tagline-detail"))
   :test #'equal))

(defun ref (feed-data &rest keys)
  "Get value from `feed-data' for `keys' like a method-chain.
`feed-data' is a hash-table stored feed data or a value of it.
`keys' are a string or a keyword which designates feed element.
If a `key' is keyword, then the `key' is automatically converted to a string.
If value is not exists, return NIL.

Examples: (suppose f is a feed-stored-hash-table)

    (feed:ref f :entries 0 :title)
    <=> (gethash \"title\" (nth 0 (gethash \"entries\" f)))

    (feed:ref f :entries most-positive-fixnum :title)
    => NIL ;probably

Note:

 * The element names of RSS and Atom are interchangable.
   i.e. you can get RSS value with Atom element name (or vice versa).
   e.g. (ref parsed-feed \"items\") <=> (ref parsed-feed \"entries\").
 * If element is lazy object, force it."
  (labels ((rec (feed-data keys)
             (if (or (null keys) (null feed-data))
                 feed-data
                 (destructuring-bind (first . rest) keys
                   (when (keywordp first)
                     (setf first (string-downcase first)))
                   (etypecase feed-data
                     (list
                      (aif (nth first feed-data)
                           (if (lazy-p it)
                               (let ((val (force it)))
                                 (setf (nth first feed-data) val)
                                 (rec val rest))
                               (rec it rest))))
                     (hash-table
                      (aif (gethash first feed-data nil)
                           (if (lazy-p it)
                               (let ((val (force it)))
                                 (setf (gethash first feed-data)
                                       val)
                                 (rec val rest))
                               (rec it rest))
                           (rec (some (lambda (k)
                                        (gethash k feed-data nil))
                                      (gethash first *key-map* nil))
                                (rest keys)))))))))
    (rec feed-data keys)))


;;--------------------------------------------------------------------
;; To-Alist
;;--------------------------------------------------------------------

(defun to-alist (feed-hash-table)
  "Convert the `feed-hash-table' into the alist."
  (check-type feed-hash-table hash-table)
  (let ((alist nil))
    (iter (for (k v) :in-hashtable feed-hash-table)
          (let ((val (typecase v
                       (hash-table
                        (to-alist v))
                       (list
                        (if (lazy-p v)
                            (let ((forced-val (force v)))
                              (if (hash-table-p forced-val)
                                  (progn
                                    (setf (gethash k feed-hash-table)
                                          forced-val)
                                    (to-alist forced-val))
                                  (setf (gethash k feed-hash-table)
                                        forced-val)))
                            (if (hash-table-p (car v))
                                (nreverse (mapcar #'to-alist v))
                                v)))
                       (t v))))
            (push (cons k val) alist)))
    (nreverse alist)))


;;====================================================================
