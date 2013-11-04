(in-package :cl-gss)

(include "gssapi/gssapi.h")
#-darwin (include "gssapi/gssapi_ext.h")

(constant (gss-c-deleg-flag "GSS_C_DELEG_FLAG"))
(constant (gss-c-mutual-flag "GSS_C_MUTUAL_FLAG"))
(constant (gss-c-replay-flag "GSS_C_REPLAY_FLAG"))
(constant (gss-c-sequence-flag "GSS_C_SEQUENCE_FLAG"))
(constant (gss-c-conf-flag "GSS_C_CONF_FLAG"))
(constant (gss-c-integ-flag "GSS_C_INTEG_FLAG"))
(constant (gss-c-anon-flag "GSS_C_ANON_FLAG"))
(constant (gss-c-prot-ready-flag "GSS_C_PROT_READY_FLAG"))
(constant (gss-c-trans-flag "GSS_C_TRANS_FLAG"))
(constant (gss-c-deleg-policy-flag "GSS_C_DELEG_POLICY_FLAG"))

(constant (gss-c-both "GSS_C_BOTH"))
(constant (gss-c-initiate "GSS_C_INITIATE"))
(constant (gss-c-accept "GSS_C_ACCEPT"))

(constant (gss-c-gss-code "GSS_C_GSS_CODE"))
(constant (gss-c-mech-code "GSS_C_MECH_CODE"))

(constant (gss-c-no-name "GSS_C_NO_NAME"))
(constant (gss-c-no-buffer "GSS_C_NO_BUFFER"))
;(constant (gss-c-no-oid "GSS_C_NO_OID"))
(constant (gss-c-no-oid-set "GSS_C_NO_OID_SET"))
(constant (gss-c-no-context "GSS_C_NO_CONTEXT"))
(constant (gss-c-no-credential "GSS_C_NO_CREDENTIAL"))
(constant (gss-c-no-channel-bindings "GSS_C_NO_CHANNEL_BINDINGS"))

(constant (gss-c-calling-error-offset "GSS_C_CALLING_ERROR_OFFSET"))
(constant (gss-c-routine-error-offset "GSS_C_ROUTINE_ERROR_OFFSET"))
(constant (gss-c-supplementary-offset "GSS_C_SUPPLEMENTARY_OFFSET"))
(constant (gss-c-calling-error-mask "GSS_C_CALLING_ERROR_MASK"))
(constant (gss-c-routine-error-mask "GSS_C_ROUTINE_ERROR_MASK"))
(constant (gss-c-supplementary-mask "GSS_C_SUPPLEMENTARY_MASK"))

(constant (gss-s-call-inaccessible-read "GSS_S_CALL_INACCESSIBLE_READ"))
(constant (gss-s-call-inaccessible-write "GSS_S_CALL_INACCESSIBLE_WRITE"))
(constant (gss-s-call-bad-structure "GSS_S_CALL_BAD_STRUCTURE"))

(constant (gss-s-bad-mech "GSS_S_BAD_MECH"))
(constant (gss-s-bad-name "GSS_S_BAD_NAME"))
(constant (gss-s-bad-nametype "GSS_S_BAD_NAMETYPE"))
(constant (gss-s-bad-bindings "GSS_S_BAD_BINDINGS"))
(constant (gss-s-bad-status "GSS_S_BAD_STATUS"))
(constant (gss-s-bad-sig "GSS_S_BAD_SIG"))
(constant (gss-s-no-cred "GSS_S_NO_CRED"))
(constant (gss-s-no-context "GSS_S_NO_CONTEXT"))
(constant (gss-s-defective-token "GSS_S_DEFECTIVE_TOKEN"))
(constant (gss-s-defective-credential "GSS_S_DEFECTIVE_CREDENTIAL"))
(constant (gss-s-credentials-expired "GSS_S_CREDENTIALS_EXPIRED"))
(constant (gss-s-context-expired "GSS_S_CONTEXT_EXPIRED"))
(constant (gss-s-failure "GSS_S_FAILURE"))
(constant (gss-s-bad-qop "GSS_S_BAD_QOP"))
(constant (gss-s-unauthorized "GSS_S_UNAUTHORIZED"))
(constant (gss-s-unavailable "GSS_S_UNAVAILABLE"))
(constant (gss-s-duplicate-element "GSS_S_DUPLICATE_ELEMENT"))
(constant (gss-s-name-not-mn "GSS_S_NAME_NOT_MN"))
(constant (gss-s-bad-mech-attr "GSS_S_BAD_MECH_ATTR"))

(constant (gss-s-continue-needed "GSS_S_CONTINUE_NEEDED"))
(constant (gss-s-duplicate-token "GSS_S_DUPLICATE_TOKEN"))
(constant (gss-s-old-token "GSS_S_OLD_TOKEN"))
(constant (gss-s-unseq-token "GSS_S_UNSEQ_TOKEN"))
(constant (gss-s-gap-token "GSS_S_GAP_TOKEN"))

(constant (gss-c-qop-default "GSS_C_QOP_DEFAULT"))
(constant (gss-c-indefinite "GSS_C_INDEFINITE"))

(ctype om-uint32 "OM_uint32")

(ctype size-t "size_t")

(cstruct gss-buffer-desc "gss_buffer_desc"
         (length "length" :type size-t)
         (value "value" :type :pointer))

(cstruct gss-oid-desc "gss_OID_desc"
         (length "length" :type om-uint32)
         (elements "elements" :type :pointer))

(cstruct gss-oid-set-desc "gss_OID_set_desc"
         (count "count" :type size-t)
         (elements "elements" :type (:pointer (:struct gss-oid-desc))))

(ctype gss-name-t "gss_name_t")
(ctype gss-ctx-id-t "gss_ctx_id_t")
(ctype gss-channel-bindings-t "gss_channel_bindings_t")
(ctype gss-qop-t "gss_qop_t")
(ctype gss-cred-usage-t "gss_cred_usage_t")
