(in-package :cl-gss)

(cffi:define-foreign-library libgssapi
  (:darwin "libgssapi_krb5.dylib")
  ((:and :sunos :x86-64) (:or "/usr/lib/64/gss/mech_krb5.so" "libgss.so"))
  (:unix (:or "libgssapi_krb5.so" "libgss.so")))

(cffi:use-foreign-library libgssapi)

(cffi:defctype gss-oid (:pointer (:struct gss-oid-desc)))
(cffi:defctype gss-buffer-t :pointer)
(cffi:defctype gss-cred-id-t :pointer)

(cffi:defcvar ("GSS_C_NT_USER_NAME" *gss-c-nt-user-name* :read-only t) gss-oid)
(cffi:defcvar ("GSS_C_NT_MACHINE_UID_NAME" *gss-c-nt-machine-uid-name* :read-only t) gss-oid)
(cffi:defcvar ("GSS_C_NT_STRING_UID_NAME" *gss-c-nt-string-uid-name* :read-only t) gss-oid)
(cffi:defcvar ("GSS_C_NT_HOSTBASED_SERVICE" *gss-c-nt-hostbased-service* :read-only t) gss-oid)

(defparameter *gss-c-no-oid* (cffi:null-pointer))

(cffi:defcfun ("gss_import_name" gss-import-name) om-uint32
  (minor-status (:pointer om-uint32))
  (input-name-buffer (:pointer (:struct gss-buffer-desc)))
  (input-name-type gss-oid)
  (output-name (:pointer gss-name-t)))

(cffi:defcfun ("gss_compare_name" gss-compare-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name1 gss-name-t)
  (name2 gss-name-t)
  (name-equal (:pointer :int)))

(cffi:defcfun ("gss_display_name" gss-display-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name gss-name-t)
  (output-name (:pointer (:struct gss-buffer-desc)))
  (output-type (:pointer gss-oid)))

(cffi:defcfun ("gss_export_name" gss-export-name) om-uint32
  (minor-status (:pointer om-uint32))
  (name gss-name-t)
  (exported (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_display_status" gss-display-status) om-uint32
  (minor-status (:pointer om-uint32))
  (status-value-input om-uint32)
  (status-type :int)
  (mech-type gss-oid)
  (message-context (:pointer om-uint32))
  (status-string (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_context_time" gss-context-time) om-uint32
  (minor-status (:pointer om-uint32))
  (context-handle gss-ctx-id-t)
  (time-rec (:pointer om-uint32)))

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

(defmacro buffer-desc-length (buf)
  `(cffi:foreign-slot-value ,buf '(:struct gss-buffer-desc) 'length))

(defmacro buffer-desc-value (buf)
  `(cffi:foreign-slot-value ,buf '(:struct gss-buffer-desc) 'value))

(cffi:defcfun ("gss_acquire_cred" gss-acquire-cred) om-uint32
  (minor-status (:pointer om-uint32))
  (desired-name gss-name-t)
  (time-req om-uint32)
  (desired-mechs (:pointer (:struct gss-oid-set-desc)))
  (cred-usage gss-cred-usage-t)
  (output-cred-handle (:pointer gss-cred-id-t))
  (actual-mechs (:pointer (:pointer (:struct gss-oid-set-desc))))
  (time-rec (:pointer om-uint32)))

(cffi:defcfun ("gss_acquire_cred_with_password" gss-acquire-cred-with-password) om-uint32
  (minor-status (:pointer om-uint32))
  (desired-name gss-name-t)
  (password (:pointer (:struct gss-buffer-desc)))
  (time-req om-uint32)
  (desired-mechs (:pointer (:struct gss-oid-set-desc)))
  (cred-usage gss-cred-usage-t)
  (output-cred-handle (:pointer gss-cred-id-t))
  (actual-mechs (:pointer (:pointer (:struct gss-oid-set-desc))))
  (time-rec (:pointer om-uint32)))

(cffi:defcfun ("gss_inquire_cred" gss-inquire-cred) om-uint32
  (minor-status (:pointer om-uint32))
  (cred-handle gss-cred-id-t)
  (name (:pointer gss-name-t))
  (lifetime (:pointer om-uint32))
  (cred-usage (:pointer gss-cred-usage-t))
  (mechs (:pointer (:pointer (:struct gss-oid-set-desc)))))

(cffi:defcfun ("gss_release_cred" gss-release-cred) om-uint32
  (minor-status (:pointer om-uint32))
  (cred-handle (:pointer gss-cred-id-t)))

(cffi:defcfun ("gss_indicate_mechs" gss-indicate-mechs) om-uint32
  (minor-status (:pointer om-uint32))
  (mech-set (:pointer (:pointer (:struct gss-oid-set-desc)))))

(cffi:defcfun ("gss_release_oid_set" gss-release-oid-set) om-uint32
  (minor-status (:pointer om-uint32))
  (oid-set (:pointer (:pointer (:struct gss-oid-set-desc)))))

(cffi:defcfun ("gss_release_oid" gss-release-oid) om-uint32
  (minor-status (:pointer om-uint32))
  (oid-set (:pointer (:pointer (:struct gss-oid-desc)))))

(cffi:defcfun ("gss_oid_to_str" gss-oid-to-str) om-uint32
  (minor-status (:pointer om-uint32))
  (oid (:pointer (:struct gss-oid-desc)))
  (output-string (:pointer (:struct gss-buffer-desc))))

(cffi:defcfun ("gss_str_to_oid" gss-str-to-oid) om-uint32
  (minor-status (:pointer om-uint32))
  (string (:pointer (:struct gss-buffer-desc)))
  (result-buffer (:pointer (:pointer (:struct gss-oid-desc)))))
