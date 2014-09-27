(in-package :cl-gss)

(cffi:defcvar ("gss_mech_krb5" gss-mech-krb5 :read-only t) (:pointer (:struct gss-oid-desc)))

(cffi:defcfun ("krb5_gss_register_acceptor_identity" %krb5-register-acceptor-identity) om-uint32
  (file :string))
