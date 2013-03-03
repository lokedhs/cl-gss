(in-package :cl-gss)

(declaim #.*compile-decl*)

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
      (let ((result (gss-call m (gss-init-sec-context m
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
                                                      time-rec))))
        (unwind-protect
             (values result (cffi:convert-from-foreign (cffi:foreign-slot-value output-token 'gss-buffer-desc 'value)
                                                       (list :array :unsigned-char (cffi:convert-from-foreign (cffi:foreign-slot-value output-token 'gss-buffer-desc 'length) 'om-uint32))))
          (cffi:with-foreign-objects ((minor 'om-uint32))
            (gss-release-buffer minor output-token)))))))

(defun accept-sec ()
  )
