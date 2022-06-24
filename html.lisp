(in-package :araneida)

;;; XXX fix this, it's not correct
(defun html-reserved-p (c)
  (member c '(#\< #\"  #\> #\&)))

(defun html-escape (html-string)
  (apply #'concatenate 'string
         (loop for c across html-string
               if (html-reserved-p c)
               collect (format nil "&#~A;" (char-code c))
               else if (eql c #\Newline) collect "<br>"
               else collect (string c))))

(defun html-escape-tag (tag attrs content)
  (declare (ignore tag attrs))
  (s. (mapcar #'html-escape content)))

(setf (get 'escape 'html-converter) #'html-escape-tag)
(setf (get 'null 'html-converter) #'princ-to-string)

#+nil
(defun html-attr (attr)
  (declare (optimize (speed 3) (debug 0)))
  (labels ((flatten (a) (if (consp a)
                            (apply #'s. (mapcar #'flatten a))
                          a)))
    ;; surely somewhere there's a function that takes a list of
    ;; strings to concatenate?  or maybe this isn't slow really
    (let ((r (apply #'s.
                    (loop for a on attr by #'cddr
                          append (list " " (symbol-name (car a))
                                       "=\"" (s. (flatten (cadr a))) "\"")))))
      (the simple-string r))))

(defun html-attr (attr)
  (with-output-to-string (o)
    (loop for (att val . rest) on attr by #'cddr do
	  (progn
	    (princ " " o)
	    (princ (symbol-name att) o)
	    (princ "=\"" o)
	    (princ val o)
	    (princ "\"" o)))))

(defun empty-element-p (tag)
  (member (intern (symbol-name tag) #.*package*) '(img br hr link input)))

;;; we had to comment out the fast version due to the large body of
;;; extant code that relied on implicit "let's convert everything to a
;;; string" in this old slow one.

(defun html (things &key (newline '(p option table tr h1 h2 h3 h4 h5 h6)))
  "Format supplied argument as HTML.  Argument may be a string (returned unchanged) or a list of (tag content) where tag may be (tagname attrs).  ((a :href \"/\") \"home\") is formatted as you'd expect it to be.  For special effects, set the HTML-CONVERTER property of a symbol for a tag to a function.  It will be called with arguments (TAG ATTRS CONTENT) and should return a string to be interpolated at that point"
  (cond ((and (consp things) (not (stringp (car things))))
         (let* ((tag (if (consp (car things)) (caar things) (car things)))
                (attrs (if (consp (car things)) (cdar things) ()))
                (content (cdr things)))
           (if (and (symbolp tag) (get tag :html-converter))
               (funcall (get tag :html-converter) tag attrs content)
             (if (not (empty-element-p tag))
                 ;; this isn't line noise.  Honest
                 (format nil "<~A~A>~{~A~}</~A>~:[~;
~]"
                         tag (html-attr attrs) (mapcar #'html content) tag
                         t #+nil (or 1 (member tag newline )))
               (format nil "<~A ~A>" tag (html-attr attrs))))))
        ((consp things)
         (apply #'concatenate 'string (mapcar #'princ-to-string things)))
        ((keywordp things)
         (format nil "<~A>" (symbol-name things)))
        (t
         things)))

(defun html-stream (stream things &optional inline-elements)
  "Format supplied argument as HTML.  Argument may be a string
(returned unchanged) or a list of (tag content) where tag may be
(tagname attrs).  ((a :href \"/ \") \"home\") is formatted as you'd
expect it to be.  INLINE-ELEMENTS is a list of elements not to print a
newline after.  Returns T unless broken, so can be the last form in a
handler"
  (declare (optimize (speed 3))
           (type stream stream))
  (cond ((and (consp things) (not (stringp (car things))))
         (let* ((tag (if (consp (car things)) (caar things) (car things)))
                (attrs (if (consp (car things)) (cdar things) ()))
                (content (cdr things)))
           (if (not (empty-element-p tag))
               (progn
                 (write-sequence "<" stream)
                 (write-sequence (symbol-name tag) stream)
                 (write-sequence (html-attr attrs) stream)
                 (write-sequence ">" stream)
                 (dolist (c content)
                   (html-stream stream c inline-elements))
                 (write-sequence "</" stream)
                 (write-sequence (symbol-name tag) stream)
                 (write-sequence ">" stream)
                 (unless (member tag inline-elements)
                   (terpri stream)))
             (format stream "<~A ~A>" tag (html-attr attrs)))))
        ((consp things)
         (dolist (thing things) (princ thing stream)))
        ((keywordp things)
         (write-sequence "<" stream)
         (write-sequence (symbol-name things) stream)
         (write-sequence ">" stream))
	((functionp things)
	 (funcall things stream))
        (t
         (princ things stream)))
  t)

#||
(search-html-tree '((string> ht :element) t (= p :element)) '((html) ((body) ((p) "foo") ((p) "bar") ((div :title "titl") "blah"))))
||#

(defun search-html-tree (search-terms tree)
  (labels ((node-matches (term tree)
	     (or (eql term t)
		 (destructuring-bind (op content name) term
		   (let ((r-op (if (eql op '=) 'equal op)))
		     (if (eql name :element)
			 (funcall r-op content (caar tree))
			 (funcall r-op content (getf (cdar tree) name))))))))
    (cond ((eql (cdr search-terms) nil)
	   (and (node-matches (car search-terms) tree) tree))
	  ((null tree) nil)
	  ((node-matches (car search-terms) tree)
	   (remove-if #'null
		      (mapcar (lambda (tr)
				(search-html-tree (cdr search-terms) tr))
			      (cdr tree)))))))
