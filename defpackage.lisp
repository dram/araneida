(defpackage "ARANEIDA"
  (:export
   ;; client
   ;; with-url-stream
   ;; daemon
   :install-serve-event-handlers :remove-serve-event-handlers
   :define-page
   ;; exports (deprecated)
   :export-server :export-handler :output-apache-conf   
   ;; listeners
   :http-listener :threaded-http-listener :serve-event-http-listener
   :https-listener :threaded-https-listener :serve-event-https-listener
   :reverse-proxy-listener-mixin  :reverse-proxy-translations
   :serve-event-reverse-proxy-listener 
   :threaded-reverse-proxy-listener 
   :http-listener-handler :apache-conf-segment
   :start-listening :stop-listening :listening-p
   :*restart-on-handler-errors*
   ;; file-request
   :*content-types* :read-mime-types :file-request-handler :static-file-handler
   ;; handler
   :install-handler :uninstall-handler :find-handler
   :child-handlers :handler :dispatching-handler
   :response-sent :handle-request
   :handle-request-authorization :handle-request-response
   :handle-request-authentication
   ;; http-error
   :http-error :http-error-code :http-error-message :http-error-client-message
   :http-bad-request   :http-unauthorized   :http-payment-required
   :http-forbidden   :http-not-found   :http-method-not-allowed
   :http-not-acceptable   :http-proxy-authentication-required
   :http-request-time-out   :http-conflict   :http-gone
   :http-length-required   :http-precondition-failed
   :http-request-entity-too-large   :http-request-url-too-large
   :http-unsupported-media-type   :http-internal-server-error
   :http-not-implemented   :http-bad-gateway   :http-service-unavailable
   :http-gateway-time-out   :http-version-not-supported
   ;; redirect-handler
   :redirect-handler :redirect-location
   ;; html
   :html :html-escape :html-stream :search-html-tree
   ;; pattern-match
   :define-patterns :rewrite-tree
   ;; server
   :server-base-url :export-server :server
   ;; request
   :request :request-url :request-user :request-original-url :request-method
   #+nil :request-socket			;not actually used anywhere
   :request-unhandled-part
   :request-stream :request-session :request-headers
   :request-body :request-base-url :request-path-info :request-header
   :request-condition :request-cookie
   :request-send-headers :request-send-error :request-redirect   
   :body-param :body-params :request-if-modified-since
   :dispatch-request
   :request-handled-by
   ;; url
   :url :url-scheme :copy-url :internet-url :url-username :url-password
   :url-endpoint :url-host :url-port :url-path :url-query :url-fragment
   :urlstring :urlstring-unescape
   :http-url :https-url :httplike-url :urlstring-escape  :url-query-alist
   :parse-urlstring :make-url :merge-url :url-query-param
   ;; utilities
   :split				;give in to the dark side, luke
   ;; for CLISP
   :host-serve-events
   )
  (:use "SPLIT-SEQUENCE"
        #+sbcl "SB-MOP"
        #+openmcl "OPENMCL-MOP"
        #+allegro "ACL-MOP"
        "COMMON-LISP" )) ;; PAR: removed "SB-BSD-SOCKETS"