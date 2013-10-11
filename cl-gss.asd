(defpackage :cl-gss-system
  (:use :cl :asdf)
  (:documentation "private: ASDF system package for cl-gss"))

(in-package :cl-gss-system)

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(defsystem cl-gss
  :name "cl-gss"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Common Lisp interface to GSSAPI"
  :depends-on (:cffi
               :cffi-grovel
               :trivial-garbage)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (cffi-grovel:grovel-file "gss-grovel")
                                     (cffi-grovel:grovel-file "gss-mech-krb5-grovel")
                                     (:file "functions")
                                     (:file "functions-mech-krb5")
                                     (:file "cl-gss")))))
