(defpackage :cl-gss
  (:use :cl)
  (:documentation "Common Lisp interface to GSSAPI.")
  (:export #:gss-error
           #:gss-error-minor-messages
           #:gss-error-major-messages
           #:make-name
           #:name-to-string
           #:init-sec
           #:accept-sec
           #:wrap
           #:unwrap
           #:krb5-register-acceptor-identity))

(in-package :cl-gss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
