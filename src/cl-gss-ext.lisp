(in-package :cl-gss)

(declaim #.*compile-decl*)

(defun usage->foreign (usage)
  (ecase usage
    (:initiate gss-c-initiate)
    (:accept gss-c-accept)
    (:both gss-c-both)))

(defun foreign->usage (usage)
  (cond ((= usage gss-c-initiate)
         :initiate)
        ((= usage gss-c-accept)
         :accept)
        ((= usage gss-c-both)
         :both)))

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
                                                        (usage->foreign usage)         ; cred_usage
                                                        output-cred-handle             ; output_cred_handle
                                                        (cffi:null-pointer)            ; actual_mechs
                                                        time-rec                       ; time_rec
                                                        ))
        (values (make-instance 'cred :ptr (cffi:mem-ref output-cred-handle 'gss-cred-id-t))
                (cffi:mem-ref time-rec 'om-uint32))))))

(defun inquire-cred (cred)
  (cffi:with-foreign-objects ((name 'gss-name-t)
                              (lifetime 'om-uint32)
                              (usage 'gss-cred-usage-t))
    (gss-call minor (gss-inquire-cred minor
                                      (gss-memory-mixin-ptr cred)
                                      name
                                      lifetime
                                      usage
                                      (cffi-sys:null-pointer)))
    (values (make-instance 'name :ptr (cffi:mem-ref name 'gss-name-t))
            (cffi:mem-ref lifetime 'om-uint32)
            (foreign->usage (cffi:mem-ref usage 'gss-cred-usage-t)))))
