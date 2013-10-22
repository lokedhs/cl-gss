(in-package :cl-gss)

(declaim #.*compile-decl*)

(defun acquire-cred-password (desired-name password &key time-req)
  (let ((name (if (typep desired-name 'name) desired-name (make-name desired-name :type :user-name))))
    (cffi:with-foreign-objects ((output-cred-handle :pointer)
                                (actual-mechs '(:pointer (:struct gss-oid-set-desc)))
                                (time-rec 'om-uint32))
      (with-buffer-desc (password-buffer (trivial-utf-8:string-to-utf-8-bytes password))
        (gss-call minor (gss-acquire-cred-with-password minor
                                                        (gss-memory-mixin-ptr name)    ; desired_name
                                                        password-buffer                ; password
                                                        (or time-req gss-c-indefinite) ; time_req
                                                        *gss-c-no-oid*                 ; desired_mechs
                                                        gss-c-accept                   ; cred_usage
                                                        output-cred-handle             ; output_cred_handle
                                                        actual-mechs                   ; actual_mechs
                                                        time-rec                       ; time_rec
                                                        ))))))
