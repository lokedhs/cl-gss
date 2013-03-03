(in-package :cl-gss)

(declaim #.*compile-decl*)

(cffi:define-foreign-library libgssapi
  (:darwin "libgssapi_krb5.dylib")
  (:unix (:or "libgssapi_krb5.so" "libgss.so")))

(cffi:use-foreign-library libgssapi)

(cffi:defcfun ("gss_import_name" gss-import-name) om-uint32
  (minor-status (:pointer om-uint32))
  (input-name-buffer (:pointer gss-buffer-desc))
  (input-name-type gss-oid)
  (output-name (:pointer gss-name-t)))

(cffi:defcfun ("gss_display_name" gss-display-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name gss-name-t)
  (output-name (:pointer gss-buffer-desc))
  (output-type (:pointer gss-oid)))

(cffi:defcfun ("gss_display_status" gss-display-status) om-uint32
  (minor-status (:pointer om-uint32))
  (status-value-input om-uint32)
  (status-type :int)
  (mech-type gss-oid)
  (message-context (:pointer om-uint32))
  (status-string (:pointer gss-buffer-desc)))

(cffi:defcfun ("gss_release_buffer" gss-release-buffer) om-uint32
  (minor-status (:pointer om-uint32))
  (buffer (:pointer gss-buffer-desc)))

(cffi:defcfun ("gss_init_sec_context" gss-init-sec-context) om-uint32
  (minor-status (:pointer om-uint32))
  (initiator-cred-handle gss-cred-id-t)
  (context-handle (:pointer gss-ctx-id-t))
  (target-name gss-name-t)
  (mech-type gss-oid)
  (req-flags om-uint32)
  (time_req om-uint32)
  (input-chan-bindings gss-channel-bindings-t)
  (input-token (:pointer gss-buffer-desc))
  (actual-mech-type (:pointer gss-oid))
  (output-token (:pointer gss-buffer-desc))
  (ret-flags (:pointer om-uint32))
  (time-rec (:pointer om-uint32)))

(cffi:defcfun ("gss_accept_sec_context" gss-accept-sec-context) om-uint32
  (minor-status (:pointer om-uint32))
  (context-handle (:pointer gss-ctx-id-t))
  (acceptor-cred-handle gss-cred-id-t)
  (input-token-buffer (:pointer gss-buffer-desc))
  (input-chan-bindings gss-channel-bindings-t)
  (src-name gss-name-t)
  (mech-type (:pointer gss-oid))
  (output-token (:pointer gss-buffer-desc))
  (ret-flags (:pointer om-uint32))
  (time-rec (:pointer om-uint32))
  (delegated-cred-handle (:pointer gss-cred-id-t)))

(cffi:defcvar ("GSS_C_NT_HOSTBASED_SERVICE" *gss-c-nt-hostbased-service* :read-only t) gss-oid)
(cffi:defcvar ("GCC_C_NO_OID" *gss-c-no-oid* :read-only t) gss-oid)
