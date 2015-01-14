(in-package :cl-gss)

(define-condition gss-error (error)
  ((major        :type integer
                 :initarg :major
                 :reader gss-error-major
                 :documentation "Major status")
   (major-errors :type list
                 :initarg :major-errors
                 :reader gss-error-major-messages
                 :documentation "List of major error messages")
   (minor        :type integer
                 :initarg :minor
                 :reader gss-error-minor
                 :documentation "Minor status")
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
    (error 'gss-error
           :major major
           :major-errors major-messages
           :minor minor
           :minor-errors minor-messages)))

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

(defun extract-error-message (status status-code-type mech)
  (cffi:with-foreign-objects ((message-context 'om-uint32))
    (setf (cffi:mem-ref message-context 'om-uint32) 0)
    (loop
       repeat 8          ; Prevent heap overflow if the loop never exits
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
       until (zerop (cffi:mem-ref message-context 'om-uint32)))))

(defun errors-as-string (major-status &optional minor-status minor-mech-oid)
  (when (error-p major-status)
    (list (extract-error-message major-status gss-c-gss-code *gss-c-no-oid*)
          (if (and minor-status minor-mech-oid (error-p minor-status))
              (extract-error-message minor-status gss-c-mech-code minor-mech-oid)
              nil))))

