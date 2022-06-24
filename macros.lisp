(in-package :araneida)

#+sbcl
(defmacro with-ignored-signals (signals &body body)
  (let ((sighandlers
         (mapcar (lambda (sig) (list sig :ignore)) signals)))
    `(sb-sys:with-enabled-interrupts ,sighandlers ,@body)))

#+cmu
(defmacro with-ignored-signals (signals &body body)
  (let ((sighandlers
         (mapcar (lambda (sig) (list sig :ignore)) signals)))
    `(system:with-enabled-interrupts ,sighandlers ,@body)))
