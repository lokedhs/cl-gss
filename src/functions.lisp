(in-package :cl-gss)

(declaim #.*compile-decl*)

(cffi:define-foreign-library libgssapi
  (:darwin "libgssapi_krb5.dylib")
  (:unix (:or "libgssapi_krb5.so" "libgss.so")))

(cffi:use-foreign-library libgssapi)

(cffi:defctype gss-oid :pointer)
(cffi:defctype gss-buffer-t :pointer)

(cffi:defcvar ("GSS_C_NT_HOSTBASED_SERVICE" *gss-c-nt-hostbased-service* :read-only t) gss-oid)

(defparameter *gss-c-no-oid* (cffi:null-pointer))

(cffi:defcfun ("gss_import_name" gss-import-name) om-uint32
  (minor-status (:pointer om-uint32))
  (input-name-buffer (:pointer (:struct gss-buffer-desc)))
  (input-name-type gss-oid)
  (output-name (:pointer gss-name-t)))

(cffi:defcfun ("gss_display_name" gss-display-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name gss-name-t)
  (output-name (:pointer (:struct gss-buffer-desc)))
  (output-type (:pointer gss-oid)))

(cffi:defcfun ("gss_display_status" gss-display-status) om-uint32
  (minor-status (:pointer om-uint32))
  (status-value-input om-uint32)
  (status-type :int)
  (mech-type gss-oid)
  (message-context (:pointer om-uint32))
  (status-string (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_release_buffer" gss-release-buffer) om-uint32
  (minor-status (:pointer om-uint32))
  (buffer (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_release_name" gss-release-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name (:pointer gss-name-t)))

(cffi:defcfun ("gss_delete_sec_context" gss-delete-sec-context) om-uint32
  (minor-status (:pointer om-uint32))
  (context-handle (:pointer gss-ctx-id-t))
  (output-token (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_init_sec_context" gss-init-sec-context) om-uint32
  (minor-status (:pointer om-uint32))
  (initiator-cred-handle gss-cred-id-t)
  (context-handle (:pointer gss-ctx-id-t))
  (target-name gss-name-t)
  (mech-type gss-oid)
  (req-flags om-uint32)
  (time_req om-uint32)
  (input-chan-bindings gss-channel-bindings-t)
  (input-token (:pointer (:struct gss-buffer-desc)))
  (actual-mech-type (:pointer gss-oid))
  (output-token (:pointer (:struct gss-buffer-desc)))
  (ret-flags (:pointer om-uint32))
  (time-rec (:pointer om-uint32)))

(cffi:defcfun ("gss_accept_sec_context" gss-accept-sec-context) om-uint32
  (minor-status (:pointer om-uint32))
  (context-handle (:pointer gss-ctx-id-t))
  (acceptor-cred-handle gss-cred-id-t)
  (input-token-buffer (:pointer (:struct gss-buffer-desc)))
  (input-chan-bindings gss-channel-bindings-t)
  (src-name (:pointer gss-name-t))
  (mech-type (:pointer gss-oid))
  (output-token (:pointer (:struct gss-buffer-desc)))
  (ret-flags (:pointer om-uint32))
  (time-rec (:pointer om-uint32))
  (delegated-cred-handle (:pointer gss-cred-id-t)))

(cffi:defcfun ("gss_wrap" gss-wrap) om-uint32
  (minor-status (:pointer om-uint32))
  (context-handle gss-ctx-id-t)
  (conf-req-flag :int)
  (qop-req gss-qop-t)
  (input-message-buffer (:pointer (:struct gss-buffer-desc)))
  (conf-state (:pointer :int))
  (output-message-buffer (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_unwrap" gss-unwrap) om-uint32
  (minor-status (:pointer om-uint32))
  (context-handle gss-ctx-id-t)
  (input-message-buffer (:pointer (:struct gss-buffer-desc)))
  (output-message-buffer (:pointer (:struct gss-buffer-desc)))
  (conf-state (:pointer :int))
  (qop-state (:pointer gss-qop-t)))

(cffi:defcfun ("gss_compare_name" gss-compare-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name1 gss-name-t)
  (name2 gss-name-t)
  (name-equal (:pointer :int)))

(defmacro buffer-desc-length (buf)
  `(cffi:foreign-slot-value ,buf '(:struct gss-buffer-desc) 'length))

(defmacro buffer-desc-value (buf)
  `(cffi:foreign-slot-value ,buf '(:struct gss-buffer-desc) 'value))
