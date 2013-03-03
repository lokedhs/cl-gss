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

(cffi:defcvar ("GSS_C_NT_HOSTBASED_SERVICE" *gss-c-nt-hostbased-service* :read-only t) gss-oid)
(cffi:defcvar ("GCC_C_NO_OID" *gss-c-no-oid* :read-only t) gss-oid)

(defmacro gss-call (minor-sym form)
  (check-type minor-sym symbol)
  (let ((status-sym (gensym "STATUS-")))
    `(cffi:with-foreign-objects ((,minor-sym 'om-uint32))
       (let ((,status-sym ,form))
         (unless (zerop ,status-sym)
           (error "Call failed: ~s~%~s" ',form (errors-as-string ,status-sym ,minor-sym)))))))

#+nil(defun make-name (name-string)
  (let ((output-name (cffi:foreign-alloc 'gss-name-t)))
    (cffi:with-foreign-string (foreign-name-string name-string)
      (cffi:with-foreign-objects ((minor 'om-uint32)
                                  (buf 'gss-buffer-desc))
        (setf (cffi:foreign-slot-value buf 'gss-buffer-desc 'length) (1+ (length name-string)))
        (setf (cffi:foreign-slot-value buf 'gss-buffer-desc 'value) foreign-name-string)
        (let ((status (gss-import-name minor
                                       buf
                                       *gss-c-nt-hostbased-service*
                                       output-name)))
          (format t "status = ~s, minor = ~s~%" status (cffi:mem-ref minor 'om-uint32)))
        output-name))))

(defun make-name (name-string)
  (let ((output-name (cffi:foreign-alloc 'gss-name-t)))
    (cffi:with-foreign-string (foreign-name-string name-string)
      (cffi:with-foreign-objects ((buf 'gss-buffer-desc))
        (setf (cffi:foreign-slot-value buf 'gss-buffer-desc 'length) (1+ (length name-string)))
        (setf (cffi:foreign-slot-value buf 'gss-buffer-desc 'value) foreign-name-string)
        (gss-call minor (gss-import-name minor buf *gss-c-nt-hostbased-service* output-name))
        output-name))))

(defun name-to-string (name)
  (cffi:with-foreign-objects ((minor 'om-uint32)
                              (output-name 'gss-buffer-desc)
                              (output-type 'gss-oid))
    (let ((status (gss-display-name minor (cffi:mem-ref name 'gss-name-t) output-name output-type)))
      (unless (zerop status)
        (error "Error when calling gss-display-name: ~s" (errors-as-string status minor)))
      (cffi:convert-from-foreign (cffi:foreign-slot-value output-name 'gss-buffer-desc 'value) :string))))

(defun errors-as-string (major-status minor-status)
  (declare (ignore minor-status))
  (unless (zerop major-status)
    (cffi:with-foreign-objects ((message-context 'om-uint32))
      (loop
         collect (cffi:with-foreign-objects ((minor 'om-uint32)
                                             (status-output 'gss-buffer-desc))
                   (let ((status (gss-display-status minor major-status gss-c-gss-code gss-c-no-oid
                                                     message-context status-output)))
                     (unwind-protect
                          (progn
                            (unless (zerop status)
                              (error "call to gss-display-status failed with status=~s" status))
                            (cffi:convert-from-foreign (cffi:foreign-slot-value status-output
                                                                                'gss-buffer-desc
                                                                                'value) :string))
                       (gss-release-buffer minor status-output))))
         until (zerop (cffi:mem-ref message-context 'om-uint32))))))

(defun init-sec (target)
  (let ((name (make-name target)))
    (cffi:with-foreign-objects ((context 'gss-ctx-id-t)
                                (input-token 'gss-buffer-desc)
                                (actual-mech-type 'gss-oid)
                                (output-token 'gss-buffer-desc)
                                (ret-flags 'om-uint32)
                                (time-rec 'om-uint32))
      (setf (cffi:mem-ref context 'gss-ctx-id-t) gss-c-no-context)
      (setf (cffi:foreign-slot-value input-token 'gss-buffer-desc 'length) 0)
      (setf (cffi:foreign-slot-value input-token 'gss-buffer-desc 'value) (cffi:null-pointer))
      (gss-call m (gss-init-sec-context m
                                        gss-c-no-credential
                                        context
                                        (cffi:mem-ref name 'gss-name-t)
                                        gss-c-no-oid
                                        0
                                        0
                                        gss-c-no-channel-bindings
                                        input-token
                                        actual-mech-type
                                        output-token
                                        ret-flags
                                        time-rec)))))
