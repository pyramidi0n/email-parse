(in-package :cl-user)
(defpackage email-parse
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match
   :email-match)
  (:export
   :parse-octets
   :parse))
(in-package :email-parse)

;; ------------------------------------------------------------------------------

;; https://datatracker.ietf.org/doc/html/rfc5321#section-4.5.3.1.1
(defparameter *local-part-max-length* 64)

;; https://datatracker.ietf.org/doc/html/rfc5321#section-4.5.3.1.3
;;
;; By necessity, a maximum path of 256 octets compels a maximum length of 254
;; octets for a valid email address. This supersedes information in other RFCs
;; and errata, i.e. that the maximum length is 320 octets.
(defparameter *mailbox-max-length* 254)

;; ------------------------------------------------------------------------------

(declaim (inline plist))
#+:sbcl (declaim (sb-ext:maybe-inline parse-octets))

(defun plist (local-part domain-or-address-literal)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (or (simple-array (unsigned-byte 8) (*))
                     string
                     null)
                 local-part
                 domain-or-address-literal))
  (when (or local-part domain-or-address-literal)
    (list :local-part local-part
          :domain domain-or-address-literal)))

(defun parse-octets (octets lower upper &key (result-type 'string) plist)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (or (simple-array (unsigned-byte 8) (*))
                     null)
                 octets)
           (type fixnum lower)
           (type fixnum upper)
           (type (or symbol null) result-type)
           (type boolean plist))
  (let ((local-part-lower nil)
        (local-part-upper nil)
        (domain-or-address-literal-lower nil)
        (domain-or-address-literal-upper nil))
    (declare (type matched
                   local-part-lower
                   local-part-upper
                   domain-or-address-literal-lower
                   domain-or-address-literal-upper))
    (letrules ((r-parse-mailbox
                (concatenation (capture local-part-lower
                                        local-part-upper
                                        r-local-part)
                               (terminal +#\@+)
                               (capture domain-or-address-literal-lower
                                        domain-or-address-literal-upper
                                        (alternatives r-domain
                                                      r-address-literal)))))
              (declare (inline r-parse-mailbox))
              (when (<= (length octets) *mailbox-max-length*)
                (let ((parsed (r-parse-mailbox octets lower upper)))
                  (when (and parsed (= parsed (length octets)))
                    (let ((local-part (subseq octets
                                              local-part-lower
                                              local-part-upper))
                          (domain-or-address-literal (subseq octets
                                                             domain-or-address-literal-lower
                                                             domain-or-address-literal-upper)))
                      (when (<= (length local-part) *local-part-max-length*)
                        (case result-type
                          (string (let ((lp-str (ascii-code-string local-part))
                                        (doal-str (ascii-code-string domain-or-address-literal)))
                                    (if plist
                                        (plist lp-str doal-str)
                                        (values lp-str doal-str))))
                          (otherwise (if plist
                                         (plist local-part domain-or-address-literal)
                                         (values local-part domain-or-address-literal))))))))))))

(defun parse (str &key plist)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type string str)
           (type boolean plist))
  #+:sbcl (declare (inline parse-octets))
  (handler-case
      (let ((octets (ascii-string-code '(simple-array (unsigned-byte 8) (*)) str)))
        (declare (type (simple-array (unsigned-byte 8) (*)) octets))
        (multiple-value-bind (local-part domain-or-address-literal)
            (parse-octets octets 0 (length octets))
          (if plist
              (plist local-part domain-or-address-literal)
              (values local-part domain-or-address-literal))))
    (type-error ())))
