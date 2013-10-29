(in-package :cl-gss)

(declaim #.*compile-decl*)

(defun conv-usage-to-foreign (usage)
  (ecase usage
    (:initiate gss-c-initiate)
    (:accept gss-c-accept)
    (:both gss-c-both)))

(defun parse-usage-from-foreign (usage)
  (cond ((= usage gss-c-initiate)
         :initiate)
        ((= usage gss-c-accept)
         :accept)
        ((= usage gss-c-both)
         :both)))

(defun acquire-cred (desired-name &key time-req (usage :initiate))
  (let ((name (parse-identifier-to-name desired-name)))
    (cffi:with-foreign-objects ((output-cred-handle :pointer)
                                (time-rec 'om-uint32))
      (gss-call minor (gss-acquire-cred minor
                                        (gss-memory-mixin-ptr name) ; desired_name
                                        (or time-req gss-c-indefinite) ; time_req
                                        *gss-c-no-oid* ; desired_mechs
                                        (conv-usage-to-foreign usage) ; cred_usage
                                        output-cred-handle ; output_cred_handle
                                        (cffi:null-pointer) ; actual_mechs
                                        time-rec ; time_rec
                                        ))
      (values (make-instance 'cred :ptr (cffi:mem-ref output-cred-handle 'gss-cred-id-t))
              (cffi:mem-ref time-rec 'om-uint32)))))

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
                                                        (conv-usage-to-foreign usage)  ; cred_usage
                                                        output-cred-handle             ; output_cred_handle
                                                        (cffi:null-pointer)            ; actual_mechs
                                                        time-rec                       ; time_rec
                                                        ))
        (values (make-instance 'cred :ptr (cffi:mem-ref output-cred-handle 'gss-cred-id-t))
                (cffi:mem-ref time-rec 'om-uint32))))))

(defun make-mech (mech-ref)
  (let ((length (cffi:foreign-slot-value mech-ref '(:struct gss-oid-desc) 'length))
        (v (cffi:foreign-slot-value mech-ref '(:struct gss-oid-desc) 'elements)))
    (convert-to-bytes (cffi:convert-from-foreign v (list :array :unsigned-char length)))))

(defun generate-mech-list (mech-set)
  (let* ((mech-set (cffi:mem-ref mech-set '(:pointer (:struct gss-oid-set-desc))))
         (num-mechs (cffi:foreign-slot-value mech-set '(:struct gss-oid-set-desc) 'count))
         (elements-ref (cffi:foreign-slot-value mech-set '(:struct gss-oid-set-desc) 'elements)))
    (loop
       for i from 0 below num-mechs
       collect (make-mech (cffi:mem-aptr elements-ref '(:struct gss-oid-desc) i)))))

(defun inquire-cred (cred)
  "Inquire information about a credential. This function returns four values:
NAME - The name of the identity that is asserted by the credential
TIME - The number of seconds that the credential remains valid
USAGE - A value indicating how the credential is used, one of
        :INITIATE, :ACCEPT, :BOTH
MECHANISMS - A list of mech OID values describing the mechanisms that are
             supported."
  (cffi:with-foreign-objects ((name 'gss-name-t)
                              (lifetime 'om-uint32)
                              (usage 'gss-cred-usage-t)
                              (mech-list '(:pointer (:struct gss-oid-desc))))
    (gss-call minor (gss-inquire-cred minor
                                      (gss-memory-mixin-ptr cred)
                                      name
                                      lifetime
                                      usage
                                      mech-list))
    (unwind-protect
         (values (make-instance 'name :ptr (cffi:mem-ref name 'gss-name-t))
                 (cffi:mem-ref lifetime 'om-uint32)
                 (parse-usage-from-foreign (cffi:mem-ref usage 'gss-cred-usage-t))
                 (generate-mech-list mech-list))
      (gss-call m (gss-release-oid-set m mech-list)))))

(defun mech-list ()
  "Return a list of all suported mechnisms. Each entry is an OID describing
each mechanism."
  (cffi:with-foreign-objects ((mech-set-return '(:pointer (:struct gss-oid-set-desc))))
    (gss-call minor (gss-indicate-mechs minor mech-set-return))
    (unwind-protect
         (generate-mech-list mech-set-return)
      ;; Release the returned mech set
      (gss-call m (gss-release-oid-set m mech-set-return)))))

(defmacro with-oid-buffer ((sym oid) &body body)
  (let ((oid-copy (gensym "OID-"))
        (array-sym (gensym "ARRAY-"))
        (oid-struct (gensym "OID-VALUE-")))
    `(let ((,oid-copy ,oid))
       (cffi:with-foreign-objects ((,oid-struct '(:struct gss-oid-desc)))
         (with-foreign-buffer-from-byte-array (,array-sym ,oid-copy)
           (setf (cffi:foreign-slot-value ,oid-struct '(:struct gss-oid-desc) 'length) (length oid))
           (setf (cffi:foreign-slot-value ,oid-struct '(:struct gss-oid-desc) 'elements) ,array-sym)
           (let ((,sym ,oid-struct))
             ,@body))))))

(defun oid-to-string (oid)
  (check-type oid vector)
  (with-oid-buffer (oid-ref oid)
    (cffi:with-foreign-objects ((result '(:struct gss-buffer-desc)))
      (gss-call minor (gss-oid-to-str minor oid-ref result))
      (unwind-protect
           ;; The value returned contains the terminating 0 byte
           ;; which is somewhat surprising, since other functions
           ;; returning strings do not do this. Since there is very
           ;; little documentation available to explain exactly
           ;; how it's supposed to work, we'll simply strip the
           ;; trailing 0 if any can be found.
           (let ((length (buffer-desc-length result)))
             (loop
                while (and (plusp length)
                           (= (cffi:mem-aref (buffer-desc-value result) :unsigned-char (1- length)) 0))
                do (decf length))
             (cffi:foreign-string-to-lisp (buffer-desc-value result)
                                          :count length))
        (gss-call m (gss-release-buffer m result))))))

(defun string-to-oid (string)
  (check-type string string)
  (cffi:with-foreign-string ((foreign-name-string foreign-name-string-length) string)
    (cffi:with-foreign-objects ((buf '(:struct gss-buffer-desc))
                                (result '(:pointer (:pointer (:struct gss-oid-desc)))))
      (setf (buffer-desc-length buf) foreign-name-string-length)
      (setf (buffer-desc-value buf) foreign-name-string)
      (gss-call minor (gss-str-to-oid minor buf result))
      (unwind-protect
           (make-mech (cffi:mem-ref result '(:pointer (:struct gss-oid-desc))))
        (gss-call m (gss-release-oid m result))))))
