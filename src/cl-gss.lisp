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
       (with-foreign-buffer-from-byte-array (,mem-sym buffer)
         (cffi:with-foreign-objects ((,buffer-sym '(:struct gss-buffer-desc)))
           (setf (buffer-desc-length ,buffer-sym) (length ,array-copy))
           (setf (buffer-desc-value ,buffer-sym) ,mem-sym)
           (let ((,sym ,buffer-sym))
             ,@body))))))

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
    (cffi:convert-to-foreign result (list :array :unsigned-char (length result)))))

(defun token->array (token)
  (if (cffi:null-pointer-p token)
      nil
      (convert-to-bytes (cffi:convert-from-foreign (buffer-desc-value token)
                                                    (list :array :unsigned-char
                                                          (cffi:convert-from-foreign (buffer-desc-length token) 'om-uint32))))))

(defclass gss-memory-mixin ()
  ((ptr :reader gss-memory-mixin-ptr
        :initarg :ptr)))

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
  (:documentation "Wrapper class for instances fo gss-ctx-id-t"))

(defmethod initialize-instance :after ((obj context) &key &allow-other-keys)
  (let ((ptr (gss-memory-mixin-ptr obj)))
    (trivial-garbage:finalize obj #'(lambda ()
                                      (cffi:with-foreign-objects ((h 'gss-ctx-id-t)
                                                                  (output '(:struct gss-buffer-desc)))
                                        (setf (cffi:mem-ref h 'gss-ctx-id-t) ptr)
                                        (gss-call m (gss-delete-sec-context m h output)))))))

(define-condition gss-error (error)
  ((major-errors :type list
                 :initarg :major-errors
                 :reader gss-error-major-messages
                 :documentation "List of major error messages")
   (minor-errors :type list
                 :initarg :minor-errors
                 :reader gss-error-minor-messages
                 :documentation "List of minor error messages"))
  (:report (lambda (condition out)
             (format out "GSS error:~{~%Major:~a~}~{~%Minor:~a~}"
                     (gss-error-major-messages condition)
                     (gss-error-minor-messages condition))))
  (:documentation "Error that is raised when a GSSAPI function returns an error"))

(defun raise-error (major minor minor-mech-oid)
  (destructuring-bind (major-messages minor-messages)
      (errors-as-string major minor minor-mech-oid)
    (error 'gss-error :major-errors major-messages :minor-errors minor-messages)))

(defun calling-error-p (code)
  (not (zerop (logand code (ash gss-c-calling-error-mask gss-c-calling-error-offset)))))

(defun routine-error-p (code)
  (not (zerop (logand code (ash gss-c-routine-error-mask gss-c-routine-error-offset)))))

(defun supplementary-info-p (code)
  (not (zerop (logand code (ash gss-c-supplementary-mask gss-c-supplementary-offset)))))

(defun error-p (code)
  (or (calling-error-p code) (routine-error-p code)))

(defun continue-needed-p (result)
  (/= (logand result gss-s-continue-needed) 0))

(defun make-name (name-string)
  (check-type name-string string)
  (cffi:with-foreign-string ((foreign-name-string foreign-name-string-length) name-string :null-terminated-p nil)
    (cffi:with-foreign-objects ((buf '(:struct gss-buffer-desc))
                                (output-name 'gss-name-t))
      (setf (buffer-desc-length buf) foreign-name-string-length)
      (setf (buffer-desc-value buf) foreign-name-string)
      (gss-call minor (gss-import-name minor buf *gss-c-nt-hostbased-service* output-name))
      (make-instance 'name :ptr (cffi:mem-ref output-name 'gss-name-t)))))

(defun name-to-string (name)
  (cffi:with-foreign-objects ((output-name '(:struct gss-buffer-desc))
                              (output-type 'gss-oid))
    (gss-call m (gss-display-name m (gss-memory-mixin-ptr name) output-name output-type))
    (values (cffi:foreign-string-to-lisp (buffer-desc-value output-name)
                                         :count (buffer-desc-length output-name)))))

(defun errors-as-string (major-status &optional minor-status minor-mech-oid)
  (labels ((extract-error (status status-code-type mech)
             (cffi:with-foreign-objects ((message-context 'om-uint32))
               (loop
                  repeat 8 ; Prevent heap overflow if the loop never exits
                  collect (cffi:with-foreign-objects ((minor 'om-uint32)
                                                      (status-output '(:struct gss-buffer-desc)))
                            (let ((display-result (gss-display-status minor status status-code-type mech
                                                                      message-context status-output)))
                              (unwind-protect
                                   (progn
                                     (when (error-p display-result)
                                       (error "call to gss-display-status failed with status=~s"
                                              display-result))
                                     (cffi:foreign-string-to-lisp (buffer-desc-value status-output)
                                                                  :count (buffer-desc-length status-output)))
                                (when (error-p (gss-release-buffer minor status-output))
                                  (error "failed to release memory from gss-display-status")))))
                  until (zerop (cffi:mem-ref message-context 'om-uint32))))))
    (when (error-p major-status)
      (list (extract-error major-status gss-c-gss-code *gss-c-no-oid*)
            (if (and minor-status minor-mech-oid (not (zerop minor-status)))
                (extract-error minor-status gss-c-mech-code minor-mech-oid)
                nil)))))

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
(defun init-sec (target &key flags (time-req 0) context input-token)
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

  (let ((name (if (stringp target) (make-name target) target))
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
                                                           gss-c-no-credential 
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
               (cffi:with-foreign-objects ((minor 'om-uint32))
                 (gss-release-buffer minor output-token))))
        (when input-token-buffer (cffi:foreign-free input-token-buffer))))))

;;;
;;;  Implements the functionality of gss_accept_sec_context
;;;
(defun accept-sec (buffer &key context)
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
          possible flags: :DELEG, :MUTUAL, :REPLAY, :SEQUENCE, :CONF, :INTEG, :ANON"

  (check-type buffer vector)
  (check-type context (or null context))
  (cffi:with-foreign-objects ((context-handle 'gss-ctx-id-t)
                              (input-token-buffer '(:struct gss-buffer-desc))
                              (src-name 'gss-name-t)
                              (output-token '(:struct gss-buffer-desc))
                              (ret-flags 'om-uint32)
                              (time-rec 'om-uint32))
    (setf (cffi:mem-ref context-handle 'gss-ctx-id-t) (get-or-allocate-context context))
    (let ((foreign-buffer (array-to-foreign-char-array buffer)))
      (unwind-protect
           (progn
             (setf (buffer-desc-length input-token-buffer) (length buffer))
             (setf (buffer-desc-value input-token-buffer) foreign-buffer)
             (let ((result (gss-call m (gss-accept-sec-context m ;minor
                                                               context-handle ;context-handle
                                                               gss-c-no-credential ;acceptor-cred-handle
                                                               input-token-buffer ;input buffer
                                                               gss-c-no-channel-bindings ;chan bindings
                                                               src-name ;src name
                                                               (cffi:null-pointer) ;mech type
                                                               output-token ;output token
                                                               ret-flags ;ret flags
                                                               time-rec ;time rec
                                                               (cffi-sys:null-pointer) ;delegated cred handle
                                                               ))))
               (unwind-protect
                    (values (continue-needed-p result)
                            (or context (make-instance 'context :ptr (cffi:mem-ref context-handle 'gss-ctx-id-t)))
                            (make-instance 'name :ptr (cffi:mem-ref src-name 'gss-name-t))
                            (token->array output-token)
                            (make-flags-list (cffi:mem-ref time-rec 'om-uint32)))
                 (cffi:with-foreign-objects ((minor 'om-uint32))
                   (gss-release-buffer minor output-token)))))
        (cffi:foreign-free foreign-buffer)))))

;;;
;;;  Implements gss_wrap
;;;
(defun wrap (context buffer &key conf)
  "Wrap a the byte array in BUFFER in an cryptographic wrapper, using the specified CONTEXT.
The buffer will be encrypted if CONF is non-NIL. This function returns the encrypted
data as a byte array, and a second boolean return value that incidates whether the
message was encrypted or not."
  (let ((foreign-buffer (array-to-foreign-char-array buffer)))
    (unwind-protect
         (cffi:with-foreign-objects ((input-foreign-desc '(:struct gss-buffer-desc))
                                     (output-foreign-desc '(:struct gss-buffer-desc))
                                     (conf-state :int))
           (setf (buffer-desc-length input-foreign-desc) (length buffer))
           (setf (buffer-desc-value input-foreign-desc) foreign-buffer)
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
             (gss-call m (gss-release-buffer m output-foreign-desc))))
      (cffi:foreign-free foreign-buffer))))

;;;
;;;  Implements gss_unwrap
;;;
(defun unwrap (context buffer)
  "Convert an wrapped buffer into usable form. CONTEXT is the security context to use,
BUFFER is the protected byte array. This function returns the unwrapped buffer, as well
as a boolean indicating whether the original message was encrypted."
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
(defun krb5-register-acceptor-identity (file)
  "Register a server's identity. FILE is a keytab file containing the
credentials to be used."
  (check-type file (or string pathname))
  (let ((pathname (pathname file)))
    (unless (probe-file pathname)
      (error "Keytab file does not exist: ~a" pathname))
    (krb5-register-acceptor-identity-ffi (namestring pathname))))
