(in-package :cl-gss)

(include "gssapi/gssapi.h")

(constant (gss-c-gss-code "GSS_C_GSS_CODE"))
(constant (gss-c-no-name "GSS_C_NO_NAME"))
(constant (gss-c-no-buffer "GSS_C_NO_BUFFER"))
(constant (gss-c-no-oid "GSS_C_NO_OID"))
(constant (gss-c-no-oid-set "GSS_C_NO_OID_SET"))
(constant (gss-c-no-context "GSS_C_NO_CONTEXT"))
(constant (gss-c-no-credential "GSS_C_NO_CREDENTIAL"))
(constant (gss-c-no-channel-bindings "GSS_C_NO_CHANNEL_BINDINGS"))

(ctype om-uint32 "OM_uint32")

(cstruct gss-buffer-desc "gss_buffer_desc" (length "length" :type om-uint32) (value "value" :type :pointer))
(ctype gss-buffer-t "gss_buffer_t")

(ctype gss-name-t "gss_name_t")

(ctype gss-oid "gss_OID")
