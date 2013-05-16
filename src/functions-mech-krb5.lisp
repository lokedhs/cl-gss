(in-package :cl-gss)

(declaim #.*compile-decl*)

(cffi:defcvar ("gss_mech_krb5" gss-mech-krb5 :read-only t) (:pointer (:struct gss-oid-desc)))
