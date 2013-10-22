(in-package :cl-gss)

(declaim #.*compile-decl*)

(defun make-usage-flags (usage)
  (ecase usage
    (:initiate gss-c-initiate)
    (:accept gss-c-accept)
    (:both gss-c-both)))

(defun acquire-cred-password (desired-name password &key time-req (usage :initiate))
  (let ((name (parse-identifier-to-name desired-name)))
    (cffi:with-foreign-objects ((output-cred-handle :pointer)
                                (time-rec 'om-uint32))
      (with-buffer-desc (password-buffer (trivial-utf-8:string-to-utf-8-bytes password))
        (gss-call minor (gss-acquire-cred-with-password minor
                                                        (gss-memory-mixin-ptr name)    ; desired_name
                                                        password-buffer                ; password
                                                        (or time-req gss-c-indefinite) ; time_req
                                                        *gss-c-no-oid*                 ; desired_mechs
                                                        (make-usage-flags usage)       ; cred_usage
                                                        output-cred-handle             ; output_cred_handle
                                                        (cffi:null-pointer)            ; actual_mechs
                                                        time-rec                       ; time_rec
                                                        ))
        (values (make-instance 'cred :ptr (cffi:mem-ref output-cred-handle 'gss-cred-id-t))
                (cffi:mem-ref time-rec 'om-uint32))))))
