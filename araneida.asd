;;; -*- Lisp -*-
(defpackage :araneida-system (:use #:asdf #:cl))
(in-package :araneida-system)

;;; Define whether this lisp supports araneida threads

#+(or sb-thread allegro armedbear openmcl lispworks)
(pushnew :araneida-threads *features*)

#+(or sbcl cmu clisp)
(pushnew :araneida-serve-event *features*)

(defsystem araneida
  :depends-on (net-telent-date split-sequence
			       #+sbcl sb-bsd-sockets)
  :version "0.9"
  :components ((:file "defpackage")
               (:module "utility"
                        :components ((:file "aif")
                                     (:file "lists")
                                     (:file "split")
                                     (:file "seqlet")
                                     (:file "base64" :depends-on ("seqlet"))
                                     (:file "strings"))
			:depends-on ("defpackage"))
               (:module "compat"
                        :components
                        ((:file #+sbcl "compat-sbcl"
                                #+openmcl "compat-openmcl"
                                #+allegro "compat-allegro"
                                #+armedbear "compat-abcl"
                                #+cmu "compat-cmucl"
                                #+clisp "compat-clisp"
                                #+lispworks "compat-lispworks"))
                        :depends-on ("defpackage"))
	       (:file "araneida" :depends-on  ("defpackage"))
               (:file "html" :depends-on ("defpackage"))
               (:file "url-class" :depends-on ("defpackage"))
               (:file "url" :depends-on ("utility" "url-class"))
               (:file "exports"
                      :depends-on ("defpackage" "utility" "url" "handler"))
               (:file "request-class" :depends-on ("defpackage"))
               (:file "request" :depends-on ("url" "request-class" "araneida"))
               (:file "auth" :depends-on ("request"))
               (:file "define-page" :depends-on ("request"))
	       (:file "handler" :depends-on ("defpackage"))
	       (:file "redirect-handler" :depends-on ("handler" ))
               (:file "daemon"
                      :depends-on
                      ("url" "handler" "defpackage" "exports" "request"))
               (:file "memoization")
	       (:file "http-error" :depends-on ("defpackage"))
	       (:file "http-listener-class" :depends-on ("daemon"))
	       (:file "http-listener" :depends-on ("http-listener-class"))
               #+araneida-serve-event
               (:file "serve-event-http-listener"
                      :depends-on ("http-listener" "compat"))
               (:file "static-file-handler" :depends-on ("handler" "request" "compat"))
               (:file "file-request" :depends-on ("static-file-handler"))
               #+araneida-threads
               (:file "threaded-http-listener" :depends-on ("compat" "http-listener"))
               (:file "reverse-proxy-listener-class" 
		      :depends-on ("compat"))
	       (:file "https-listener-class" 
		      :depends-on ("reverse-proxy-listener-class"))
               ;; this file isn't used?
               #+nil (:file "client" :depends-on ("request" "url" "compat"))
               (:file "pattern-match" :depends-on ("defpackage" "memoization"))
	       (:static-file "NEWS")
	       (:module "doc"
			:components ((:html-file "examples")
				     (:static-file "example" 
						   :pathname "example.lisp")
				     (:html-file "faq")
				     (:html-file "handlers")
				     (:html-file "html")
				     (:html-file "index")
				     (:html-file "installation")
				     (:html-file "reference")
				     (:html-file "servers")
				     (:html-file "troubleshooting")
				     (:html-file "urls")
				     (:static-file "new-dispatch-model")
				     (:static-file "araneida"
						   :pathname "araneida.css")
				     (:static-file "PLAN")))
	       ))

#||
(defsystem araneida-examples
  :depends-on (araneida)
  :components ((:file "defpackage")
               (:file "session")
               (:file "variables" :depends-on ("defpackage"))               
	       (:file "per-host" :pathname #.*per-host-configuration*
		      :depends-on ("variables"))
               (:file "main" :depends-on ("defpackage" "per-host"))))
||#
