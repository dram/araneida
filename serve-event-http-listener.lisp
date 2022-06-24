(in-package :araneida)

(defvar *in-other-handler* nil)

(defvar *pending-other-listeners* nil)

(defparameter *waiting-listener-limit* 3)

(defun serve-event-http-listener-accept-one-request (listener)
  (with-accept-flets
      (if *in-other-handler*
          (if (>= (length *pending-other-listeners*) 3)
              (forcibly-close-stream (accept listener))
              (setf *pending-other-listeners*
                    (nconc *pending-other-listeners*
                           (list (cons listener (accept listener))))))
          (let ((*in-other-handler* t))
            (let ((s (accept listener)))
              (unwind-protect
                   (unwind-protect
                        (do-it listener s)
                     (forcibly-close-stream s))
                (tagbody
                 top
                   (unless *pending-other-listeners*
                     (go end))
                   (let* ((pair (pop *pending-other-listeners*))
                          (listener (car pair))
                          (s (cdr pair)))
                     (unwind-protect
                          (unwind-protect
                               (do-it listener s)
                            (forcibly-close-stream s))
                       (go top)))
                 end)))))))

(defmethod start-listening ((listener serve-event-http-listener)
			    &key &allow-other-keys)
  (let ((socket
	 (host-make-listener-socket (http-listener-address listener)
                                    (http-listener-port listener))))
    (setf (http-listener-socket listener) socket)
    (setf (http-listener-serve-event listener)
          (host-add-fd-handler
           socket
           (lambda (fd)
	     (declare (ignore fd))
             (serve-event-http-listener-accept-one-request listener))
           ))))

(defmethod stop-listening ((listener serve-event-http-listener)
			   &key abort &allow-other-keys)
  (declare (ignore abort))		;FIXME we always abort.  not very nice
  (host-remove-fd-handler (http-listener-serve-event listener))
  (host-close-socket (http-listener-socket listener)))

