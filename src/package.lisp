(defpackage :cl-gss
  (:use :cl)
  (:documentation "Common Lisp interface to GSSAPI."))

(in-package :cl-gss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
