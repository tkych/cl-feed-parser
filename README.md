Last modified : 2013-08-17 21:41:58 tkych

version 0.0.29 (alpha)



TODO:
-----

 - Add: test! test! test!
 - Add: doc! doc! doc!
 - Change: namespace handling, Add: parse-option valid-namespaces
 - Add: parsing CDF
 - Add: microformats-parser like Beautiful Soup
 - Add: relative link resolution
 - Add: character encoding detection
 - !! TEST: Authorization for password-protected feed !!


CL-Feed-Parser
==============

CL-feed-parser is a feed parser library for Common Lisp.  The function
`parse-feed` takes a url, pathname, string or stream as an argument,
and returns hash-table stored feed data.  The goal of cl-feed-parser
is to treat the feed as the one format by hiding the difference of
various feed formats (atom1.0, rss1.0, rss2.0, etc.).

This project is inspired by python [feedparser][feedparser webpage]
(a.k.a. *Universal Feed Parser*).  The feedparser is created by
[Mark Pilgrim][Mark Pilgrim webpage], and bringing up by
[Kurt McKee][Kurt McKee webpage].

Though cl-feed-parser's implementation is different from feedparser's,
api is almost the same.


Difference:

cl-feed-parser is/has:

 * lazy eval,
 * accessor-function `ref`,
 * pased-date-time is universal time,


[feedparser webpage]: https://code.google.com/p/feedparser/
[Mark Pilgrim webpage]: http://en.wikipedia.org/wiki/Mark_Pilgrim_(software_developer)
[Kurt McKee webpage]: https://github.com/kurtmckee/feedparser


Depends-on
----------

 * [closure-xml](http://common-lisp.net/project/cxml/)
 * [drakma](http://weitz.de/drakma/)
 * [ironclad](http://method-combination.net/lisp/ironclad/)
 * [iterate](http://common-lisp.net/project/iterate/)
 * [alexandria](http://common-lisp.net/project/alexandria/)
 * [anaphora](http://common-lisp.net/project/anaphora/)
 * [cl-ppcre](http://weitz.de/cl-ppcre/)
 * [flexi-streams](http://weitz.de/flexi-streams/)
 * [url-rewrite](http://weitz.de/url-rewrite/)
 * [puri](http://www.cliki.net/PURI)
 * [cl-sanitize](https://github.com/archimag/cl-sanitize)
 * [cl-date-time-parser](https://github.com/tkych/cl-date-time-parser)


Installation
------------

 0. SHELL$   `git clone https://github.com/tkych/cl-feed-parser.git`
 1. CL-REPL> `(push #p"/path-to-cl-feed-parser/cl-feed-parser/" asdf:*central-registry*)`
 2. CL-REPL> `(ql:quickload :cl-feed-parser)` or `(asdf:load-system :cl-feed-parser)`


Examples
--------

The following example is available at https://gist.github.com/tkych/6255855

```lisp
;; Let's make a minimum feed reader!

CL-REPL> (let ((cache (make-hash-table :test #'equal)))
           (defun parse (feed-spec)
             (or (gethash feed-spec cache nil)
                 (setf (gethash feed-spec cache)
                       (feed-parser:parse-feed feed-spec)))))

CL-REPL> (defun show-entry-titles (feed-spec)
           (let ((f (parse feed-spec)))
             (loop
                :for i :from 0
                :for title := (feed-parser:ref f :entries i :title)
                :until (null title)
                :do (format t "~&[~D]: ~A" i title))))

CL-REPL> (defun read-nth-entry (nth feed-spec)
           (princ (or (feed-parser:ref (parse feed-spec)
                                       :entries nth :description)
                      (feed-parser:ref (parse feed-spec)
                                       :entries nth :summary)))
           nil)

CL-REPL> (defparameter f "http://www.whitehouse.gov/feed/press")

CL-REPL> (show-entry-titles f)

CL-REPL> (read-nth-entry 2 f)

CL-REPL> (setf f "http://planet.lisp.org/rss20.xml")

CL-REPL> (show-entry-titles f)

CL-REPL> (read-nth-entry 1 f)
```



Manual
------

#### [Function] PARSE-FEED _input_ _&key_ _etag_ _modified_ _agent_ _referer_ _handlers_ _request-heders_ _response-headers_ => _hash-table_

Parses feed, and returns _hash-table_ stored feed-data.
_input_ must be ether url-string, xml-string, pathname or stream.
If _input_ is url-string, first fetches the feed before parsing.


#### [Function] REF _feed-data_ _&rest_ _keys_ => _feed-value_

Get value from _feed-data_ for _keys_ like a method-chain.
_feed-data_ is a hash-table stored feed data or a value of it.
_keys_ are a string or a keyword which designates feed element.
If a _key_ is keyword, then the _key_ is automatically converted to a string.
If value is not exists, return NIL.


Examples: (suppose `f` is a feed-stored-hash-table)

    (feed:ref f :entries 0 :title)
    <=> (gethash "title" (nth 0 (gethash "entries" f)))
    
    (feed:ref f :entries most-positive-fixnum :title)
    => NIL ;probably


Note:

 * The element names of RSS and Atom are interchangable.
   * i.e. you can get RSS value with Atom element name (or vice versa).
   * e.g. (ref parsed-feed \"items\") <=> (ref parsed-feed \"entries\").

 * If element is lazy object, force it.
 

#### [Function] TO-ALIST _feed-hash-table_ => _NIL_

Convert the _feed-hash-table_ into the alist.


#### [Macro] BE-LAZY _form_  => _lazy-object_

Make lazy object. For making a custum-sanitizer or
custum-date-time-parser.


#### [Function] LAZY-P _x_ => _boolean_

Check whether _x_ is lazy-object, or not. For making a custum-accessor
function.


#### [Function] FORCE _lazy-object_ => _forced-value_

Force _lazy-object_. For making a custum-accessor function.


#### [Special Variable] \*USER-AGENT\*

\*USER-AGENT\* is strored the string which tells the server who
requests (i.e. User-Agent header value). If you are embedding
cl-feed-parser in a larger software, you should change the value of
\*USER-AGENT\* to your software name and URL.


#### [Special Variable] \*DATE-TIME-PARSER\*

\*DATE-TIME-PARSER\* is strored the function which parses _date-time-string_
into universal time. The default value is the following:

      (lambda (date-time-string)
        (be-lazy
         (ignore-errors
           (reduce #'+ (multiple-value-list
                        (cl-date-time-parser:parse-date-time
                         date-time-string))))))

The above lazy function returns universal-time (plus fraction if
exists). If you want to use the another date-time-parser or never to
parse date-time, you could set this variable to the another parser or
`#'identity`. If _date-time-string_ is not date-time format, it returns
NIL.


#### [Special Variable] \*SANITIZER\*

\*SANITIZER\* is strored the function which sanitizes _html-string_.
The default value is the following:

      (lambda (html-string)
        (be-lazy (ignore-errors (sanitize:clean html-string))))

The above lazy function returns sanitized _html-string_.  If you want
to use the another sanitize-function or never to sanitize, you could
set this variable to the another sanitize-function or `#'identity`.  If
_html-string_ is not html format, it returns NIL.


Author, License, Copyright
--------------------------

 - Takaya OCHIAI  <#.(reverse "moc.liamg@lper.hcykt")>

 - LLGPL

 - Copyright (C) 2013 Takaya OCHIAI
