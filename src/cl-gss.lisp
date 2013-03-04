(in-package :cl-gss)

(declaim #.*compile-decl*)

(defclass gss-memory-mixin ()
  ((ptr :reader gss-memory-mixin-ptr
        :initarg :ptr)))

(defclass name (gss-memory-mixin)
  ())

(defmethod initialize-instance :after ((obj name) &key &allow-other-keys)
  (let ((ptr (gss-memory-mixin-ptr obj)))
    (let ((out *standard-output*))
      (trivial-garbage:finalize obj #'(lambda ()
                                        (format out "releasing ~s~%" ptr)
                                        (gss-call m (gss-release-name m ptr)))))))

(defgeneric release-gss-object (value)
  (:method ((value name))
    (format *error-output* "explicit release is not currently supported~%")))

(defgeneric resolve-gss-ptr (value)
  (:method ((value gss-memory-mixin)) (gss-memory-mixin-ptr value))
  (:method ((value t)) value))

(defun calling-error-p (code)
  (not (zerop (logand code (ash gss-c-calling-error-mask gss-c-calling-error-offset)))))

(defun routine-error-p (code)
  (not (zerop (logand code (ash gss-c-routine-error-mask gss-c-routine-error-offset)))))

(defun supplementary-info-p (code)
  (not (zerop (logand code (ash gss-c-supplementary-mask gss-c-supplementary-offset)))))

(defun error-p (code)
  (or (calling-error-p code) (routine-error-p code)))

(defmacro gss-call (minor-sym form)
  (check-type minor-sym symbol)
  (let ((status-sym (gensym "STATUS-")))
    `(cffi:with-foreign-objects ((,minor-sym 'om-uint32))
       (let ((,status-sym ,form))
         (when (error-p ,status-sym)
           (error "Call failed: ~s~%~s" ',form (errors-as-string ,status-sym
                                                                 (cffi:mem-ref ,minor-sym 'om-uint32))))
         ,status-sym))))

(defun make-name (name-string)
  (check-type name-string string)
  (let ((output-name (cffi:foreign-alloc 'gss-name-t)))
    (cffi:with-foreign-string (foreign-name-string name-string)
      (cffi:with-foreign-objects ((buf 'gss-buffer-desc))
        (setf (buffer-desc-length buf) (1+ (length name-string)))
        (setf (buffer-desc-value buf) foreign-name-string)
        (gss-call minor (gss-import-name minor buf *gss-c-nt-hostbased-service* output-name))
        (make-instance 'name :ptr output-name)))))

(defun name-to-string (name)
  (cffi:with-foreign-objects ((minor 'om-uint32)
                              (output-name 'gss-buffer-desc)
                              (output-type 'gss-oid))
    (let ((status (gss-display-name minor (cffi:mem-ref (resolve-gss-ptr name) 'gss-name-t) output-name output-type)))
      (unless (zerop status)
        (error "Error when calling gss-display-name: ~s" (errors-as-string status minor)))
      (cffi:convert-from-foreign (buffer-desc-value output-name) :string))))

(defun errors-as-string (major-status minor-status)
  (declare (ignore minor-status))
  (unless (zerop major-status)
    (cffi:with-foreign-objects ((message-context 'om-uint32))
      (loop
         repeat 8 ; Prevent heap overflow if the loop never exits
         collect (cffi:with-foreign-objects ((minor 'om-uint32)
                                             (status-output 'gss-buffer-desc))
                   (let ((status (gss-display-status minor major-status gss-c-gss-code gss-c-no-oid
                                                     message-context status-output)))
                     (unwind-protect
                          (progn
                            (when (error-p status)
                              (error "call to gss-display-status failed with status=~s" status))
                            (cffi:convert-from-foreign (buffer-desc-value status-output) :string))
                       (when (error-p (gss-release-buffer minor status-output))
                         (error "failed to release memory from gss-display-status")))))
         until (zerop (cffi:mem-ref message-context 'om-uint32))))))

(defun init-sec (target)
  (let ((name (if (stringp target) (make-name target) target)))
    (cffi:with-foreign-objects ((context 'gss-ctx-id-t)
                                (input-token 'gss-buffer-desc)
                                (actual-mech-type 'gss-oid)
                                (output-token 'gss-buffer-desc)
                                (ret-flags 'om-uint32)
                                (time-rec 'om-uint32))
      (setf (cffi:mem-ref context 'gss-ctx-id-t) gss-c-no-context)
      (setf (buffer-desc-length input-token) 0)
      (setf (buffer-desc-value input-token) (cffi:null-pointer))
      (let ((result (gss-call m (gss-init-sec-context m
                                                      gss-c-no-credential
                                                      context
                                                      (cffi:mem-ref (resolve-gss-ptr name) 'gss-name-t)
                                                      gss-c-no-oid
                                                      0
                                                      0
                                                      gss-c-no-channel-bindings
                                                      input-token
                                                      actual-mech-type
                                                      output-token
                                                      ret-flags
                                                      time-rec))))
        (unwind-protect
             (values result (cffi:convert-from-foreign (buffer-desc-value output-token)
                                                       (list :array :unsigned-char (cffi:convert-from-foreign (buffer-desc-length output-token) 'om-uint32))))
          (cffi:with-foreign-objects ((minor 'om-uint32))
            (gss-release-buffer minor output-token)))))))

(defun accept-sec ()
  )
