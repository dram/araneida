(in-package :araneida)

;; that portion of the request path after base-url
(defmethod request-path-info ((r request))
  (let ((path (url-path (request-url r)))
        (ppath (url-path (request-base-url r))))
    (subseq path (length ppath) nil)))

(defmethod request-unhandled-part ((request request))
  (let* ((handled-by (request-handled-by request))
	 (offset (or (second (first handled-by)) 0))
	 (urlstring (request-urlstring request)))
    (subseq urlstring offset)))

(defgeneric request-header (request name)
  (:documentation "Returns a list containing the values of all header lines in REQUEST given by the keyword NAME"))

(defmethod request-header ((r request) name)
  (cdr (assoc name (request-headers r) :test #'string=)))

(defmethod request-if-modified-since ((request request)
                                      &optional (default nil))
  "Retrieve and parse the date in the If-Modified-Since header field.  Return DEFAULT if the header is absent or unparseable"
  (let ((if-mod-since (car (request-header request :if-modified-since))))
    (if if-mod-since
             (or (date:parse-time (car (split if-mod-since 2 '(#\;))))
                 default)
      default)))

(defmethod request-cookie ((request request) name)
  (labels ((same-name-p (st)
	     (loop
	      (unless (and (> (length st) 1)
			   (char= (elt st 0) #\Space)) (return))
	      (setf st (subseq st 1)))
	     (let ((l (length name)))
               (and (> (length st) l)
                    (string= st name :end1 l)
                    (eql (char st l) #\=)))))
  (let ((cookies (car (request-header request :cookie))))
    (loop for vv in (split cookies nil '(#\;))
          if (same-name-p vv)
          return (second (split vv 2 '(#\=)))))))


;; this takes an alist not a request, hence the name
(defun body-param (name alist)
  "Look in the request body ALIST for the value of the parameter NAME"
  (cadr (assoc name alist :test #'string=)))

(defun body-params (name alist &key (case-sensitive nil))
  "Look in the request body ALIST for the values of the parameters starting NAME, returning a list of KEY VALUE pairs"
  ;; find all parameters starting NAME
  (let ((equal (if case-sensitive #'string= #'string-equal))
        (len (length name)))
    (flet ((starts-with-name (string)
             (and (>= (length string) len)
                  (funcall equal string name :end1 len))))
      (remove-if-not #'starts-with-name alist :key #'car))))


(defmethod dispatch-request ((request request) handlers &optional discriminator)
  "Find the best match for REQUEST in the list HANDLERS"
  (unless discriminator (setf discriminator (request-url request)))
  (destructuring-bind
        (method match prefix func &optional needs-discriminator)
      (find-export (urlstring discriminator) handlers (request-method request))
    (unless method (return-from dispatch-request nil))
    (setf (request-base-url request) (parse-urlstring prefix))
    (let ((rest-of-url
           (subseq (urlstring discriminator)
                   (length (urlstring (request-base-url request)))
                   nil)))
      (cond ((and needs-discriminator (consp func))
             (apply (car func) request handlers discriminator
                    rest-of-url (cdr func)))
            ((consp func)
             (apply (car func) request rest-of-url (cdr func)))
            (needs-discriminator
             (funcall func request handlers discriminator rest-of-url))
            (t
             (funcall func request rest-of-url))))))

;;; rfc 1945 p26
(defvar *http-error-codes*
  '((400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")))

(defmethod request-send-headers ((request request) &key
                                 (content-type "text/html")
                                 content-length
                                 expires
				 cache-control
				 location
				 refresh
				 pragma
				 set-cookie
				 conditional
				 www-authenticate
                                 extra-http-headers
                                 (last-modified (get-universal-time))
                                 (response-text "OK")
                                 (response-code 200))
  "Send HTTP/1.0 headers in response to REQUEST.  If the request HTTP version
is less than 1.0, do nothing.  If CONDITIONAL is true, may signal RESPONSE-SENT
instead of returning normally."
  (when (< (request-http-version request) 1.0)
    (return-from request-send-headers response-code))
  (let ((stream (request-stream request)))
    (labels ((perhaps (if name &optional then)
	       (if if (princ (s. name ": " (or then if)  #.(format nil "~%"))
			     stream)))
	     (date (d) 
	       (if (numberp d) (date:universal-time-to-http-date d) d)))
      (when (and conditional
		 (<= last-modified
		     (request-if-modified-since request 0)))
	(setf response-code 304 response-text "Not modified"))
      (when (eql response-code 304)
	;;  "the response {SHOULD,MUST} NOT include other
	;;  entity-headers; this prevents inconsistencies between
	;;  cached entity-bodies and updated headers.
	(setf content-length nil content-type nil))
      (format stream "HTTP/1.0 ~D ~A
Date: ~A
Server: ~A
Connection: close~%"
	      response-code response-text 
	      (date:universal-time-to-http-date (get-universal-time))
	      *araneida-product-tokens*)
      (perhaps content-type "Content-Type")
      (perhaps last-modified "Last-Modified" (date last-modified))
      (perhaps content-length "Content-Length")
      (if set-cookie
	  (let ((cookies (if (listp set-cookie) set-cookie (list set-cookie))))
	    (dolist (cookie cookies) (perhaps cookie "Set-Cookie"))))
      (perhaps cache-control "Cache-Control" )
      (perhaps refresh "Refresh" )
      (perhaps location "Location" )
      (perhaps pragma "Pragma" )
      (perhaps expires "Expires"  (date expires))
      (perhaps www-authenticate "WWW-Authenticate")
      (mapc #'(lambda (header)
                (format stream "~A: ~A~%" (car header) (cdr header)))
            extra-http-headers)
      (terpri stream)
      (when (eql response-code 304)
	;; "The 304 response MUST NOT contain a message-body" (rfc2616)
	(signal 'response-sent)))
    response-code))

  
(defmethod request-send-error ((request request) error-code &rest extra-stuff)
  "Send the client HTTP headers and HTML body for an error message
with numeric code ERROR-CODE.  EXTRA-STUFF is an optional format
control and arguments for additional information which is written to
*log-stream*"
  (let ((stream (request-stream request))
        (extra (if (and extra-stuff (stringp (car extra-stuff)))
		   (apply #'format nil extra-stuff)))
        (error-text (cdr (assoc error-code *http-error-codes*))))
    (when *log-stream*
      (format *log-stream* "~&Logged error: ~A ~A ~A~%" 
	      error-code error-text extra))
    (request-send-headers request
                          :response-code error-code :response-text error-text)
    (format stream
            (html `(html (head (title ,(s. error-code) " " ,error-text))
                         (body
                          (h2 ,error-text)
                          (p "Was asked for URL "
                             (tt ,(urlstring (request-url request)))
                             ", but it didn't happen for us.  Sorry")
                          ,@(when (and extra (not *log-stream*))
				  `((H3 "Additional information: ")
				    (pre ,(html-escape extra))))))))
    
    (signal 'response-sent)))

(defmethod request-redirect ((request request) new-url &rest headers)  
  (let ((urlstring (urlstring
                    (if (typep new-url 'url)
			new-url
			(merge-url (request-url request) new-url)))))
    (apply #'request-send-headers
	   request
	   :location urlstring
	   :expires "Fri, 30 Oct 1998 14:19:41 GMT"
	   :pragma "no-cache"
	   :response-code 302 :response-text "Redirected"
	   headers)
    (format (request-stream request)
	    "~%<h1>Redirected</h1><p>Continue <a href=\"~A\">~A</a>"
	    urlstring urlstring)
    (signal 'response-sent)))

(defun copy-request (from)
  (let ((to (make-instance 'request)))
    (labels ((set-slot (name)
	       (if (slot-boundp from name)
		   (setf (slot-value to name)  (slot-value from name))
		   (slot-makunbound to name))))
      (dolist (i '(url urlstring http-version handled-by user 
		   method stream headers body condition))
	(set-slot i)))
    to))



