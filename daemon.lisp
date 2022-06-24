(in-package :araneida)

(defun parse-protocol-version (string)
  (let ((f (position #\/ string))
	(d (position #\. string))
	(l (length string)))
    (+ (parse-integer string :start (1+ f) :end d)
       (if d
	   (/ (parse-integer string :start (1+ d))
	      (expt 10 (- l d 1)))
	   0))))

(defun read-request-from-stream (listener stream)
  (destructuring-bind
	(method url-string &optional protocol)
      (split (read-line stream nil) 3 '(#\Space))
    (let* ((headers (and protocol (read-headers stream)))
	   (http-version
	    (or (and protocol (parse-protocol-version protocol)) 0.9))
	   (content-length (parse-integer
			    (or (header-value :content-length headers) "0")))
	   (body (and (> content-length 0)
		      (make-array content-length :element-type
				  (stream-element-type stream))))
	   (len (and (> content-length 0)
		     (read-sequence body stream)))
	   (parsed-body (if body (parse-body body '(#\&) len) nil))
	   (url (merge-url 
		 ;; it may be argued that we're going to hell for this, but
		 ;; (header-value :host) may in fact be host:port and I'm
		 ;; not about to pick it apart now just so we can use
		 ;; make-url instead 
		 (parse-urlstring
		  (format nil "http://~A/"
			  (or (header-value :host headers)
			      (http-listener-default-hostname listener))))
		 url-string)))
      (make-instance 'request 
		     :url url
		     :urlstring (urlstring url)
		     :method (intern (nstring-upcase method) 'keyword)
		     :http-version http-version
		     :body parsed-body
		     :stream stream :headers headers ))))

(defun parse-body (body-string &optional (delimiters '(#\&)) end)
  "Parse BODY-STRING returning list of (var val) pairs"
  (mapcar (lambda (x)
            (mapcar #'urlstring-unescape (split-sequence #\= x :count 2)))
          (split-sequence-if (lambda (x) (member x delimiters))
			     body-string :end end)))

(defun read-folded-line (stream &optional eof-error-p eof-value)
  "Read a complete logical header line, including folded continuation lines."
  (with-output-to-string (o)
    (loop
     (let* ((l (read-line stream eof-error-p eof-value))
	    (end (position-if (lambda (x) (or (eql x (code-char 10))
					      (eql x (code-char 13))))
			      l))
	    (next (and (if end (> end 0) t)
		       (> (length l) 0)
		       (peek-char nil stream nil nil))))
       (write-sequence l o :end end)
       (unless (or (eql next #\Space) (eql next #\Tab)) (return))))))

;;; Unless you're me (and probably not even then), you should be
;;; using REQUEST-HEADER not this:

(defun header-value (name header-list)
  "Get the value of the header named NAME in HEADER-LIST"
  (cadr (assoc name header-list)))

(defun read-headers (stream)
  (let ((headers nil))
    (do ((line (read-folded-line stream t) (read-folded-line stream t)))
        ((or (not line) (zerop (length line))) headers)
      
      ;; RFC 1945: "Each header field consists of a name followed
      ;; immediately by a colon (":"), a single space (SP) character,
      ;; and the field value. Field names are case- insensitive."
      (let* ((colon-pos (position #\: line))
	     (keyword (subseq line 0 colon-pos))
	     (value (subseq line (+ 2 colon-pos)))
	     (keyword-symbol (intern (string-upcase keyword) :keyword))
	     (keyword-value (assoc keyword-symbol  headers)))
	(if keyword-value
	    (setf (cdr (last keyword-value)) (list value))
            (setf headers (acons keyword-symbol (list value) headers)))))
    headers))
        
