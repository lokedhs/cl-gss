(in-package :cl-gss)

(include "time.h")
(include "gssapi/gssapi.h")
(include "gssapi/gssapi_krb5.h")

(cstruct gss-krb5-lucid-key-t "gss_krb5_lucid_key_t"
         (type "type" :type om-uint32)
         (length "length" :type om-uint32)
         (data "data" :type :pointer))

(cstruct gss-krb5-rfc1964-keydata-t "gss_krb5_rfc1964_keydata_t"
         (sign-alg "sign_alg" :type om-uint32)
         (seal-alg "seal_alg" :type om-uint32)
         (ctx-key "ctx_key" :type gss-krb5-lucid-key-t))
