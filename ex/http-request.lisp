; the body of the reply, either a string, when getting HTML or plain text, a binary array for images, audio, and JSON, or a file stream, if requested using a keyword parameter
; the HTTP status code
; an alist of the headers sent by the server
; the URI the reply comes from, which might be different than the request when redirects occur
; the stream the reply was read from
; a boolean indicating whether the stream should be closed
; the server's status text, e.g., "OK"

(ql:quickload "drakma")


(multiple-value-bind
  (body statuscode headers uri stream closestream statustxt)
  (drakma:http-request "http://www.google.com")
  (format t "status code: ~A" body))


; tells drakma that json is plain text, so interpret it as such!
(push (cons "application" "json") drakma:*text-content-types*)

(multiple-value-bind
  (body statuscode headers uri stream closestream statustxt)
  (drakma:http-request "https://docs-examples.firebaseio.com/rest/quickstart/users.json")
  (format t "body: ~A" body))

(multiple-value-bind
  (body statuscode headers uri stream closestream statustxt)
  (drakma:http-request "http://date.jsontest.com")
  (format t "body: ~A" body))

(multiple-value-bind
  (body statuscode headers uri stream closestream statustxt)
  (drakma:http-request "https://api.github.com/repos/youralien/ai-programming-course/commits/e9837a1816ec1646bf55de6e1fdd6378c64a7aa4")
  (format t "body: ~A" body))


(net.html.parser:parse-html (drakma:http-request "http://www.google.com"))

(ql:quickload "cl-json")
(let ((stream (drakma:http-request "https://docs-examples.firebaseio.com/rest/quickstart/users.json"
  :want-stream t)))
(setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
(cl-json:decode-json stream))

(let ((stream (drakma:http-request "http://date.jsontest.com"
  :want-stream t)))
(setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
(cl-json:decode-json stream))

(let ((stream (drakma:http-request "https://api.github.com/repos/youralien/ai-programming-course/commits/e9837a1816ec1646bf55de6e1fdd6378c64a7aa4"
  :want-stream t)))
(setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
(cl-json:decode-json stream))
 

; urls:

; www.google.com

; https://docs-examples.firebaseio.com/rest/quickstart/users.json

; https://api.github.com/repos/youralien/ai-programming-course/commits/e9837a1816ec1646bf55de6e1fdd6378c64a7aa4

; The raw HTML output looked like the plain-text of a HTML file.  Tags were displayed using the "<tag> content </tag>" format.  The output was also not nicely indented in a nested structure.

; The parsed HTML output was different:

; 1) The tags were represented as symbols, i.e. :TAG

; 2) The nested structure of the HTML was represented as lists
; 3) In particular, it took the basic form ( (:tag ) content ).

; Content could be a plain-text string i.e.

; ( (:P ) "This is my paragraph" )

; Content also was nested i.e.

; ((:P ) ((:B) "This is a bolded paragraph)))

; And tags could have their keyword arguments i.e.

; ( (:A :HREF "http://www.google.com" ) "google.com" )



; The raw JSON looked like JSON plain text.

; The parsed JSON had the following format

; 1) If JSON is simple {key: value}, then represented in a Lisp list
; (:KEY . VALUE)

; 2) If JSON is nested (parent: {child: value})

; (:PARENT (:CHILD . VALUE))



