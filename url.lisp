(in-package :araneida)

;;;; Handle URLs in a CLOS object way, after the pattern of PATHNAMEs
;;;; See also url-class.lisp

;;; TODO:
;;; - clean it up - it's a mess
;;; - print URLs in #u format
;;; - Tidy for non-HTTP schemes someday too

;;; This could usefully all be seriously revisited in the light of
;;; rfc2396, which I haven't read yet but looks at a glance to be
;;; impressively more sensible than 1738

(defvar *default-url-defaults* nil
  "Default URL to use as context when PARSE-URL is given a relative urlstring")

(defmethod make-load-form ((url url) &optional environment)
  (make-load-form-saving-slots url :environment environment))

;;; basic URL about which we know little

(defmethod url-p ((url url)) #+nil (declare (ignore url)) t)
(defmethod url-p ((anything t)) #+nil (declare (ignore anything)) nil)

(defmethod parse-url ((url url))
  (destructuring-bind (scheme rest) (split (url-unparsed url) 2 '(#\:))
    (setf (url-scheme url) scheme
          (url-unparsed url) rest)))

(defmethod parse-url ((url mailto-url))
  (let ((unparsed (second (split (url-unparsed url) 2 '(#\:)))))
    (destructuring-bind (user host) (split unparsed 2 '(#\@))
      (setf (url-username url) user
            (url-host url) host)
      url)))

(defmethod copy-url ((url url) &optional extra-slots)
  (let* ((class (class-of url))
         (new (make-instance class)))
    (loop for i in (append extra-slots '(string unparsed scheme))
          when (slot-boundp url i)
            do (setf (slot-value new  i) (slot-value url i)))
    new))

(defmethod copy-url ((url mailto-url) &optional extra-slots)
  (call-next-method url (append extra-slots '(username host))))

(defmethod copy-url ((url internet-url) &optional extra-slots)
  (call-next-method url (append extra-slots '(username password host port))))

(defmethod copy-url ((url httplike-url) &optional extra-slots)
  (call-next-method url (append extra-slots '(path query fragment))))

(defmethod url-equal-p ((url1 url) (url2 url))
  (and (eql (class-of url1) (class-of url2))
       (let ((class (class-of url1)))
         (loop for slot in  (class-slots class)
               for name = (slot-definition-name slot)
               always
               (or (eql name 'string) (eql name 'unparsed)
                   (equal (slot-value url1 (slot-definition-name slot))
                          (slot-value url2 (slot-definition-name slot))))))))

(defmethod url-equal-p ((u1 t) (u2 t))
  (and (eq u1 :wild) (eq u2 :wild)))

;;; internet-url

(defmethod parse-url ((url internet-url))
  (call-next-method url)                    ;parse bits that the parent knows
  (let* ((string (subseq (url-unparsed url) 2)) ; skip "//"
         (dir-s (position #\/ string))
         (at-s (position #\@ string :end dir-s))
         (colon1-s (position #\: string :end at-s))
         (colon2-s (if at-s (position #\: string :start (1+ at-s)) nil))
         (colon-s (if at-s colon2-s colon1-s)))
    (setf (url-unparsed url) (if dir-s (subseq string  dir-s) nil)
          (url-host url) (subseq string (1+ (or at-s -1)) (or colon-s dir-s))
          (url-port url)
            (if colon-s (parse-integer (subseq string (1+ colon-s) dir-s)))
          (url-username url) (if at-s (subseq string 0 (or colon1-s at-s)) nil)
          (url-password url) (if (and at-s colon1-s)
                                 (subseq string (1+ colon1-s) at-s) nil))))

(defmethod url-endpoint ((url internet-url))
  "Returns \`hostname:port\' for this URL.  The colon and port number are omitted if the port is the default for this URL class"
  ;; XXX the above is not true in practice for HTTPS
  (let ((default-port (url-port (make-instance (class-of url)))))
    (if (eql (url-port url) default-port)
        (url-host url)
      (s. (url-host url) ":" (princ-to-string (url-port url))))))

;;; httplike-url

(defun parse-http-path (url string)
  (let* ((frag-s (position #\# string :from-end t))
         (query-s (position #\? string :end frag-s :from-end t)))
    (setf (url-query url) (if query-s (subseq string (1+ query-s) frag-s) nil)
          (url-port url) (or (url-port url)
                             (url-port (make-instance (class-of url))))
          (url-fragment url) (if frag-s (subseq string (1+ frag-s)) nil)
          (url-path url)
          (or (subseq string 0 (or query-s frag-s)) "/"))))

  
(defmethod parse-url ((url httplike-url))
  (call-next-method url)                    ;parse bits that the parent knows
  (parse-http-path url (url-unparsed url))
  (setf (url-unparsed url) nil))        ;we've finished parsing

;;; watch us assemble a URL backwards ...
;;; XXX half of this should be in the internet-url method

(defmethod urlstring ((url httplike-url))
  (let ((out '()))
    (awhen (url-fragment url) (push it out) (push "#" out))
    (awhen (url-query url) (push it out) (push "?" out))
    (aif (url-path url) (push it out) (push "/" out))
    (unless (eql (url-port url) 80)
      (push (princ-to-string (url-port url)) out) (push ":" out))
    (awhen (url-host url) (push it out))
    (awhen (url-username url)
           (push "@" out)
           (awhen (url-password url)
                  (push it out)
                  (push ":" out))
           (push it out))
    (push "://" out)
    (push (url-scheme url) out)
    (apply #'concatenate 'string out)))

(defmethod urlstring ((url mailto-url))
  (format nil "mailto:~A@~A" (url-username url)
          (url-host url)))

(defmethod merge-url ((template httplike-url) string)
  (let ((url (copy-url template)))
    ;; Find the 'leftmost' bit present in string, and replace everything in
    ;; url to the right of that
    (cond ((zerop (length string)) url)
	  ((let ((colon (position #\: string))
		 (slash (position #\/ string)))
	     (and colon
		  (or (not slash)
		      (< colon slash))))
		 ;; XXX this is probably wrong if STRING is
		 ;; e.g. "foo.bar.com:80/" but I can't be bothered to
		 ;; figure it out now
	   (parse-urlstring string))
	  ((eql (elt string 0) #\/)
           (parse-http-path url string)
           url)
          ((eql (elt string 0) #\?)
           (let ((hash (position #\# string)))
             (setf (url-query url) (subseq string 1 hash))
             (setf (url-fragment url) (if hash (subseq string (1+ hash)) nil)))
	   url)
          ((eql (elt string 0) #\#)
           (setf (url-fragment url) (subseq string 1))
           url)
          (t ;; it's a partial path, then
           (let* ((p (url-path url))
                  (c (reverse (split p nil '(#\/)))))
             (rplaca c string)
             ;; we should really check for .. components too
             (merge-url template (join "/" (reverse c))))))))

(defmethod url-query-alist ((url httplike-url) &key prefix case-sensitive )
  "Return the URL query segment as a ( NAME VALUE ) alist.  NAME=VALUE pairs may be separated by ampersand or semicolons.  If PREFIX is supplied, select only the parameters that start with that text, and remove it from the keys before returning the list"
  (let ((s-eq (if case-sensitive #'string= #'string-equal)))
    (remove-if-not
     (if prefix
         (lambda (x)
           (destructuring-bind (k v) x
             (if (and (>= (length k) (length prefix))
                      (funcall s-eq prefix k :end2 (length prefix)))
                 (list (subseq k (length prefix)) v)
               nil)))
       #'identity)
     (mapcar (lambda (x)
               (destructuring-bind (k &optional (v "")) (split x 2 '(#\=) )
                 (list (urlstring-unescape k) (urlstring-unescape v ))))
             (split (url-query url) nil '(#\& #\;))))))
  
(defmethod url-query-param ((url httplike-url) name &key case-sensitive )
  "Return the values of the query parameter NAME, or NIL if not present"
  (mapcar #'cadr
          (remove-if-not
           (lambda (x) (funcall (if case-sensitive #'string= #'string-equal)
                                name (car x)))
           (url-query-alist url))))

;;; How to choose the right URL class: add its scheme here

(defparameter *url-schemes*
  '(("HTTP" http-url)
    ("FTP" ftp-url)
    ("HTTPS" https-url)
    ("MAILTO" mailto-url)))

(defun url-class-for-scheme (scheme)
  (aif (cadr (assoc scheme *url-schemes* :test #'string-equal))
       (find-class it)
       nil))

(defun make-url (&rest rest &key scheme &allow-other-keys)
  (apply #'make-instance (url-class-for-scheme scheme) rest))

(defun parse-urlstring (string &optional (error-if-unparseable-p t))
  (let* ((scheme (string-upcase (car (split string 2 '(#\:)))))
         (class (url-class-for-scheme scheme)))
    (if (and scheme class)
        (let ((url (make-instance (url-class-for-scheme scheme)
                                  :string string :unparsed string)))
          (parse-url url)
          url)
	;; no scheme.  maybe it's relative.  We can try and merge it
	;; onto our default *default-url-defaults*
	(if (and (boundp '*default-url-defaults*)
		 *default-url-defaults*
		 (url-p *default-url-defaults*))
	    (merge-url *default-url-defaults* string)
	    (if error-if-unparseable-p
		(error "Relative URL and no ~A" '*default-url-defaults*))))))

;; This can be used to set up #u as a reader macro to read in a URL
;; object.  Note that this only works for absolute URLs

(defun url-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (list 'parse-urlstring (read stream t nil t)))
(set-dispatch-macro-character #\# #\u #'url-reader)

(defun urlstring-unescape (url-string)
  (do* ((n 0 (+ n 1))
        (out '()))
      ((not (< n (length url-string))) (coerce (reverse out) 'string ))
    (let ((c (elt url-string n)))
      (setf out 
            (cond ((eql c #\%)
                   (progn (setf n (+ 2 n))
                          (cons (code-char
				 (or (parse-integer
				      url-string :start (- n 1)
				      :end (+ n 1)
				      :junk-allowed t
				      :radix 16) 32))
                                out)))
                  ((eql c #\+)
                   (cons #\Space out))
                  (t (cons c out)))))))


;;; This escapes URIs according to the generic URI syntax described in 
;;; rfc2396, or at least it does if the character set in use on this host
;;; is close enough to US-ASCII

;;; Allowed characters are
;;;         unreserved = alphanum | mark 
;;;         mark        = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
;;; All others must be escaped

(defun urlstring-reserved-p (c)
  (not (or (member c '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\) ))
           (alphanumericp c))))

(defun urlstring-escape (url-string)
  (apply #'concatenate 'string
   (loop for c across url-string
         if (urlstring-reserved-p c)
         collect (format nil "%~2,'0X" (char-code c))
         else
         collect (string c))))

;;; perhaps this should be a setf method on url-query-param
#+nil (defun update-query-param (name value query-string)
  "Return a new query string based on QUERY-STRING but with the additional or updated parameter NAME=VALUE"
  (let ((pairs (mapcar (lambda (x) (split x 2 '(#\=) ))
                       (split query-string nil '(#\& #\;)))))
    (aif (assoc name pairs :test #'string=)
         (rplacd it (list value))
         (setf pairs (acons name (list value) pairs)))
    (join "&" (mapcar (lambda (x) (s. (car x)  "=" (cadr x))) pairs))))

(defmethod print-object ((u url) stream)
  (print-unreadable-object (u stream :type t :identity t)
    (format stream "\"~A\"" (urlstring u))))

