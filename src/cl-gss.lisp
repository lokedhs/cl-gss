(in-package :cl-gss)

(declaim #.*compile-decl*)

(defmacro gss-call (minor-sym form)
  (check-type minor-sym symbol)
  (let ((status-sym (gensym "STATUS-")))
    `(cffi:with-foreign-objects ((,minor-sym 'om-uint32))
       (let ((,status-sym ,form))
         (when (error-p ,status-sym)
           (raise-error ,status-sym (cffi:mem-ref ,minor-sym 'om-uint32) gss-mech-krb5))
         ,status-sym))))

(defmacro with-foreign-buffer-from-byte-array ((sym buffer) &body body)
  (let ((s (gensym "FOREIGN-BUFFER-")))
    `(let ((,s (array-to-foreign-char-array ,buffer)))
       (unwind-protect
            (let ((,sym ,s))
              (progn ,@body))
         (cffi:foreign-free ,s)))))

(defmacro with-buffer-desc ((sym buffer) &body body)
  (let ((array-copy (gensym "ARRAY-"))
        (mem-sym (gensym "MEM-"))
        (buffer-sym (gensym "EXTERNAL-BUFFER-")))
    `(let ((,array-copy ,buffer))
       (with-foreign-buffer-from-byte-array (,mem-sym ,array-copy)
         (cffi:with-foreign-objects ((,buffer-sym '(:struct gss-buffer-desc)))
           (setf (buffer-desc-length ,buffer-sym) (length ,array-copy))
           (setf (buffer-desc-value ,buffer-sym) ,mem-sym)
           (let ((,sym ,buffer-sym))
             ,@body))))))

(defclass gss-memory-mixin ()
  ((ptr :reader gss-memory-mixin-ptr
        :initarg :ptr
        :documentation "The wrapped pointer"))
  (:documentation "Mixin class for types that wraps a pointer that
needs to be released after the instance is no longer referenced. The
actual registration of the object is handled by the subclass."))

(defclass name (gss-memory-mixin)
  ()
  (:documentation "Wrapper class for instances of gss-name-t"))

(defmethod initialize-instance :after ((obj name) &key &allow-other-keys)
  (let ((ptr (gss-memory-mixin-ptr obj)))
    (trivial-garbage:finalize obj #'(lambda ()
                                      (cffi:with-foreign-objects ((n 'gss-name-t))
                                        (setf (cffi:mem-ref n 'gss-name-t) ptr)
                                        (gss-call m (gss-release-name m n)))))))

(defclass context (gss-memory-mixin)
  ()
  (:documentation "Wrapper class for instances of gss-ctx-id-t"))

(defmethod initialize-instance :after ((obj context) &key &allow-other-keys)
  (let ((ptr (gss-memory-mixin-ptr obj)))
    (trivial-garbage:finalize obj #'(lambda ()
                                      (cffi:with-foreign-objects ((h 'gss-ctx-id-t)
                                                                  (output '(:struct gss-buffer-desc)))
                                        (setf (cffi:mem-ref h 'gss-ctx-id-t) ptr)
                                        (gss-call m (gss-delete-sec-context m h output)))))))

(defclass cred (gss-memory-mixin)
  ()
  (:documentation "Wrapper class for instances of gss-cred-t"))

(defmethod initialize-instance :after ((obj cred) &key &allow-other-keys)
  (let ((ptr (gss-memory-mixin-ptr obj)))
    (trivial-garbage:finalize obj #'(lambda ()
                                      (cffi:with-foreign-objects ((n 'gss-cred-id-t))
                                        (setf (cffi:mem-ref n 'gss-cred-id-t) ptr)
                                        (gss-call m (gss-release-cred m n)))))))

(defun convert-to-bytes (array)
  (labels ((mk-byte8 (a)
             (let ((result (make-array (length a) :element-type '(unsigned-byte 8))))
               (map-into result #'(lambda (v)
                                    (unless (typep v '(unsigned-byte 8))
                                      (error "Value ~s in input array is not an (UNSIGNED-BYTE 8)" v))
                                    v)
                         array)
               result)))
    (typecase array
      ((simple-array (unsigned-byte 8) (*)) array)
      (t (mk-byte8 array)))))

(defun array-to-foreign-char-array (array)
  (let ((result (convert-to-bytes array)))
    ;; Due to a bug in ABCL, CFFI:CONVERT-TO-FOREIGN cannot be used.
    ;; Until this bug is fixed, let's just use a workaround.
    #-abcl (cffi:convert-to-foreign result (list :array :unsigned-char (length result)))
    #+abcl (let* ((length (length result))
                  (type (list :array :unsigned-char length))
                  (foreign-array (cffi:foreign-alloc type :count length)))
             (loop
                for v across result
                for i from 0
                do (setf (cffi:mem-aref foreign-array :unsigned-char i) v))
             foreign-array)))

(defun token->array (token)
  (if (cffi:null-pointer-p token)
      nil
      (convert-to-bytes (cffi:convert-from-foreign (buffer-desc-value token)
                                                   (list :array :unsigned-char
                                                         (cffi:convert-from-foreign (buffer-desc-length token) 'om-uint32))))))

(defun make-name (name-string &key (type :hostbased-service))
  "Create a new name object representing the given name.
The TYPE parameter indicates the requested usage type, its
value is one of :USER-NAME, :MACHINE-UID-NAME, :STRING-UID-NAME,
:HOSTBASE-SERVICE.
This function implements the functionality of the GSSAPI
function `gss_import_name'."
  (check-type name-string string)
  (let ((name-type (ecase type
                    (:user-name *gss-c-nt-user-name*)
                    (:machine-uid-name *gss-c-nt-machine-uid-name*)
                    (:string-uid-name *gss-c-nt-string-uid-name*)
                    (:hostbased-service *gss-c-nt-hostbased-service*))))
    (cffi:with-foreign-string ((foreign-name-string foreign-name-string-length) name-string :null-terminated-p nil)
      (cffi:with-foreign-objects ((buf '(:struct gss-buffer-desc))
                                  (output-name 'gss-name-t))
        (setf (buffer-desc-length buf) foreign-name-string-length)
        (setf (buffer-desc-value buf) foreign-name-string)
        (gss-call minor (gss-import-name minor buf name-type output-name))
        (make-instance 'name :ptr (cffi:mem-ref output-name 'gss-name-t))))))

(defun parse-identifier-to-name (obj)
  (etypecase obj
    (name obj)
    (string (make-name obj))))

(defun name-to-string (name)
  "Return the string representation of NAME."
  (check-type name name)
  (cffi:with-foreign-objects ((output-name '(:struct gss-buffer-desc))
                              (output-type 'gss-oid))
    (gss-call m (gss-display-name m (gss-memory-mixin-ptr name) output-name output-type))
    (unwind-protect
         (values (cffi:foreign-string-to-lisp (buffer-desc-value output-name)
                                              :count (buffer-desc-length output-name)))
      (gss-call m (gss-release-buffer m output-name)))))

(defun compare-name (name1 name2)
  "Compares two name objects. This function returns non-NIL if the two
name objects refers to the same entity. This function implements
the functionality of the GSSAPI function `gss_compare_name'."
  (check-type name1 name)
  (check-type name2 name)
  (cffi:with-foreign-objects ((result :int))
    (gss-call m (gss-compare-name m
                                  (gss-memory-mixin-ptr name1)
                                  (gss-memory-mixin-ptr name2)
                                  result))
    (not (zerop (cffi:mem-ref result :int)))))

(defun make-flags (flags)
  (loop
     with result = 0
     for flag in flags
     do (setf result (logior result (ecase flag
                                      (:deleg gss-c-deleg-flag)
                                      (:mutual gss-c-mutual-flag)
                                      (:replay gss-c-replay-flag)
                                      (:sequence gss-c-sequence-flag)
                                      (:conf gss-c-conf-flag)
                                      (:integ gss-c-integ-flag)
                                      (:anon gss-c-anon-flag))))
     finally (return result)))

(defun make-flags-list (value)
  (loop
     for (flag flag-value) in `((:deleg ,gss-c-deleg-flag)
                                (:mutual ,gss-c-mutual-flag)
                                (:replay ,gss-c-replay-flag)
                                (:sequence ,gss-c-sequence-flag)
                                (:conf ,gss-c-conf-flag)
                                (:integ ,gss-c-integ-flag)
                                (:anon ,gss-c-anon-flag)
                                (:prot-ready ,gss-c-prot-ready-flag)
                                (:trans ,gss-c-trans-flag))
     when (not (zerop (logand value flag-value)))
     collect flag))

(defun get-or-allocate-context (context)
  (if context
      (gss-memory-mixin-ptr context)
      gss-c-no-context))

;;;
;;;  Implementation of gss_init_sec_context
;;;
(defun init-sec (target &key flags (time-req 0) context input-token cred)
  "Initialise a GSS security context. This function implements the functionality of
the GSSAPI function `gss_init_sec_context'.

This function returns the following values:
  CONTINUE-NEEDED - non-NIL if the context needs a reply form the remote
                    service before it can be used
  CONTEXT - the context that is used to refer to this specific session
  BUFFER - the buffer that should be sent to the remote service
  FLAGS - a list of flags that describe various properties of the session.
          possible flags: :DELEG, :MUTUAL, :REPLAY, :SEQUENCE, :CONF, :INTEG, :ANON"

  (check-type target (or string name))
  (check-type flags list)
  (check-type time-req (integer 0))
  (check-type context (or null context))
  (check-type input-token (or null vector))
  (check-type cred (or null cred))

  (let ((name (parse-identifier-to-name target))
        (cred-ptr (if cred (gss-memory-mixin-ptr cred) (cffi-sys:null-pointer)))
        input-token-buffer)
    (cffi:with-foreign-objects ((input-token-content '(:struct gss-buffer-desc))
                                (context-handle 'gss-ctx-id-t)
                                (actual-mech-type 'gss-oid)
                                (output-token '(:struct gss-buffer-desc))
                                (ret-flags 'om-uint32)
                                (time-rec 'om-uint32))
      (setf (cffi:mem-ref context-handle 'gss-ctx-id-t) (get-or-allocate-context context))
      (if input-token
          ;; We have an input token, fill it in
          (progn
            (setq input-token-buffer (array-to-foreign-char-array input-token))
            (setf (buffer-desc-length input-token-content) (length input-token))
            (setf (buffer-desc-value input-token-content) input-token-buffer))
          ;; No input token, indicate this with size zero
          (progn
            (setf (buffer-desc-length input-token-content) 0)
            (setf (buffer-desc-value input-token-content) (cffi:null-pointer))))
      (unwind-protect
           (let ((result (gss-call m (gss-init-sec-context m
                                                           cred-ptr
                                                           context-handle
                                                           (gss-memory-mixin-ptr name)
                                                           *gss-c-no-oid*
                                                           (make-flags flags)
                                                           time-req
                                                           gss-c-no-channel-bindings
                                                           input-token-content
                                                           actual-mech-type
                                                           output-token
                                                           ret-flags
                                                           time-rec))))
             (unwind-protect
                  (values (continue-needed-p result)
                          (or context (make-instance 'context :ptr (cffi:mem-ref context-handle 'gss-ctx-id-t)))
                          (let ((array (token->array output-token)))
                            (if (zerop (length array))
                                nil
                                array))
                          (make-flags-list (cffi:mem-ref ret-flags 'om-uint32)))
               (gss-call m (gss-release-buffer m output-token))))
        (when input-token-buffer (cffi:foreign-free input-token-buffer))))))

;;;
;;;  Implements the functionality of gss_accept_sec_context
;;;
(defun accept-sec (buffer &key context cred)
  "Accept a security context from a remote client. This function implements the
functionality of the GSSAPI function `gss_accept_sec_context'.

Return values are:
  CONTINUE-NEEDED - if non-NIL, this value indicates that another message is expected
                    before the context is ready
  CONTEXT - the context that is used to refer to this specific session
  NAME - the name of the remote principal
  BUFFER - the buffer that should be sent to the remote service, or NIL if there is
           no need to send more messages
  FLAGS - a list of flags that describe various properties of the session.
          possible flags: :DELEG, :MUTUAL, :REPLAY, :SEQUENCE, :CONF, :INTEG, :ANON
  TIME-REC - The length of time that the context will be valid.
  DELEGATED-CRED-HANDLE - If the FLAGS value contains :DELEG, this value contains
                          the delegated credentials, an instance of the type CRED"

  (check-type buffer vector)
  (check-type context (or null context))
  (check-type cred (or null cred))
  (cffi:with-foreign-objects ((context-handle 'gss-ctx-id-t)
                              (src-name 'gss-name-t)
                              (output-token '(:struct gss-buffer-desc))
                              (ret-flags 'om-uint32)
                              (time-rec 'om-uint32)
                              (output-cred-handle :pointer))
    (setf (cffi:mem-ref context-handle 'gss-ctx-id-t) (get-or-allocate-context context))
    (with-buffer-desc (input-token-buffer buffer)
      (let* ((cred-ptr (if cred (gss-memory-mixin-ptr cred) (cffi:null-pointer)))
             (result (gss-call m (gss-accept-sec-context m ;minor
                                                         context-handle ;context-handle
                                                         cred-ptr ;acceptor-cred-handle
                                                         input-token-buffer ;input buffer
                                                         gss-c-no-channel-bindings ;chan bindings
                                                         src-name ;src name
                                                         (cffi:null-pointer) ;mech type
                                                         output-token ;output token
                                                         ret-flags ;ret flags
                                                         time-rec ;time rec
                                                         output-cred-handle ;delegated cred handle
                                                         ))))
        (unwind-protect
             (let ((flag-list (make-flags-list (cffi:mem-ref ret-flags 'om-uint32))))
               (values (continue-needed-p result)
                       (or context (make-instance 'context :ptr (cffi:mem-ref context-handle 'gss-ctx-id-t)))
                       (make-instance 'name :ptr (cffi:mem-ref src-name 'gss-name-t))
                       (token->array output-token)
                       flag-list
                       (cffi:mem-ref time-rec 'om-uint32)
                       (if (find :deleg flag-list)
                           (make-instance 'cred :ptr output-cred-handle)
                           nil)))
          (gss-call m (gss-release-buffer m output-token)))))))

;;;
;;;  Implements gss_wrap
;;;
(defun wrap (context buffer &key conf)
  "Wrap a the byte array in BUFFER in a cryptographic wrapper, using the specified CONTEXT.
The buffer will be encrypted if CONF is non-NIL. This function returns the encrypted
data as a byte array, and a second boolean return value that incidates whether the
message was encrypted or not."
  (check-type context context)
  (with-buffer-desc (input-foreign-desc buffer)
    (cffi:with-foreign-objects ((output-foreign-desc '(:struct gss-buffer-desc))
                                (conf-state :int))
      (gss-call m (gss-wrap m
                            (gss-memory-mixin-ptr context)
                            (if conf 1 0)
                            gss-c-qop-default
                            input-foreign-desc
                            conf-state
                            output-foreign-desc))
      (unwind-protect
           (values (token->array output-foreign-desc)
                   (not (zerop (cffi:mem-ref conf-state :int))))
        (gss-call m (gss-release-buffer m output-foreign-desc))))))

;;;
;;;  Implements gss_unwrap
;;;
(defun unwrap (context buffer)
  "Convert a wrapped buffer into usable form. CONTEXT is the security context to use,
BUFFER is the protected byte array. This function returns the unwrapped buffer, as well
as a boolean indicating whether the original message was encrypted."
  (check-type context context)
  (with-buffer-desc (input-message-buffer buffer)
    (cffi:with-foreign-objects ((output-message-buffer '(:struct gss-buffer-desc))
                                (conf-state :int)
                                (qop-state 'gss-qop-t))
      (gss-call m (gss-unwrap m
                              (gss-memory-mixin-ptr context)
                              input-message-buffer
                              output-message-buffer
                              conf-state
                              qop-state))
      (unwind-protect
           (values (token->array output-message-buffer)
                   (not (zerop (cffi:mem-ref conf-state :int))))
        (gss-call m (gss-release-buffer m output-message-buffer))))))

;;;
;;;  Credentials import
;;;
(defparameter *register-ffi-functions* '("krb5_gss_register_acceptor_identity"
                                         "gsskrb5_register_acceptor_identity"))

(defun krb5-register-acceptor-identity (file)
  "Register a server's identity. FILE is a keytab file containing the
credentials to be used."
  (check-type file (or string pathname))
  (let ((pathname (probe-file (pathname file))))
    (unless pathname
      (error "Keytab file does not exist: ~a" file))
    (let ((n (namestring pathname)))
      (loop
         for ffi-name in *register-ffi-functions*
         for ffi-fn = (cffi:foreign-symbol-pointer ffi-name)
         when ffi-fn
         do (let ((result (cffi:foreign-funcall-pointer ffi-fn () :string n om-uint32)))
              (when (error-p result)
                (raise-error result nil nil))
              (return nil))
         finally (error "Could not find acceptor function")))))
