(in-package :cl-user)
(defpackage email-match
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match)
  (:export
   :r-atext
   :r-atom
   :r-dot-string
   :r-qtextsmtp
   :r-quoted-pairsmtp
   :r-qcontentsmtp
   :r-quoted-string
   :r-local-part
   :r-let-dig
   :r-ldh-str
   :r-sub-domain
   :r-domain
   :r-snum
   :r-ipv4-address-literal
   :r-ipv6-hex
   :r-ipv6-full
   :r-ipv6-comp
   :r-ipv6v4-full
   :r-ipv6v4-comp
   :r-ipv6-addr
   :r-ipv6-address-literal
   :r-standardized-tag
   :r-dcontent
   :r-general-address-literal
   :r-address-literal
   :r-mailbox))
(in-package :email-match)

;; ------------------------------------------------------------------------------
;;
;; RFC 5321
;; Simple Mail Transfer Protocol
;;
;; This implements matchers for the grammar necessary to describe a mailbox.
;; It is not a complete implementation of the contents of RFC 5321.
;;
;; ------------------------------------------------------------------------------

(declaim (inline r-atext))
(declaim (inline r-atom))
(declaim (inline r-dot-string))
(declaim (inline r-qtextsmtp))
(declaim (inline r-atext))
(declaim (inline r-quoted-pairsmtp))
(declaim (inline r-qcontentsmtp))
(declaim (inline r-quoted-string))
(declaim (inline r-local-part))
(declaim (inline r-let-dig))
(declaim (inline r-ldh-str))
(declaim (inline r-sub-domain))
(declaim (inline r-domain))
(declaim (inline r-snum))
(declaim (inline r-ipv4-address-literal))
(declaim (inline r-ipv6-hex))
(declaim (inline r-ipv6-full))
(declaim (inline r-ipv6-comp))
(declaim (inline r-ipv6v4-full))
(declaim (inline r-ipv6v4-comp))
(declaim (inline r-ipv6-addr))
(declaim (inline r-ipv6-address-literal))
(declaim (inline r-standardized-tag))
(declaim (inline r-dcontent))
(declaim (inline r-general-address-literal))
(declaim (inline r-address-literal))
(declaim (inline r-mailbox))

;; ---

(defrule r-atext
    ;; RFC 5122
    (alternatives r-alpha
                  r-digit
                  (terminal +#\!+)
                  (terminal +#\#+)
                  (terminal +#\$+)
                  (terminal +#\%+)
                  (terminal +#\&+)
                  (terminal +#\'+)
                  (terminal +#\*+)
                  (terminal +#\++)
                  (terminal +#\-+)
                  (terminal +#\/+)
                  (terminal +#\=+)
                  (terminal +#\?+)
                  (terminal +#\^+)
                  (terminal +#\_+)
                  (terminal +#\`+)
                  (terminal +#\{+)
                  (terminal +#\|+)
                  (terminal +#\}+)
                  (terminal +#\~+)))

(defrule r-atom
    (variable-repetition r-atext :minimum 1))

(defrule r-dot-string
    (concatenation r-atom
                   (variable-repetition (concatenation (terminal +#\.+)
                                                       r-atom))))

(defrule r-qtextsmtp
    (alternatives (value-range-alternatives 32 33)
                  (value-range-alternatives 35 91)
                  (value-range-alternatives 93 126)))

(defrule r-quoted-pairsmtp
    (concatenation (terminal 92)
                   (value-range-alternatives 32 126)))

(defrule r-qcontentsmtp
    (alternatives r-qtextsmtp
                  r-quoted-pairsmtp))

(defrule r-quoted-string
    (concatenation r-dquote
                   (variable-repetition r-qcontentsmtp)
                   r-dquote))

(defrule r-local-part
    (alternatives r-dot-string
                  r-quoted-string))

(defrule r-let-dig
    (alternatives r-alpha
                  r-digit))

(defrule r-ldh-str
    ;; r-let-dig is a strict subset of the variable-repetition, and consists
    ;; of one octet, so we pad the repetition by one octet.
    ;;
    ;; Otherwise, the repetition will match everything and potentially
    ;; create a scenario where we have a terminal "-", which violates
    ;; the RFC.
    (concatenation (variable-repetition (alternatives r-alpha
                                                      r-digit
                                                      (terminal +#\-+))
                                        :padding 1)
                   r-let-dig))

(defrule r-sub-domain
    ;; (concatenation r-let-dig
    ;;                (optional-sequence r-ldh-str))
    ;;
    ;; We're ignoring the ldh-str definition in RFC 5321 since it doesn't
    ;; correctly account for the maximum length of labels defined in
    ;; RFC 1035. Instead, we specify an explicit maximum here.
    ;;
    ;; See also:
    ;; https://datatracker.ietf.org/doc/html/rfc1035#section-2.3.4
    (concatenation r-let-dig
                   (optional-sequence
                    (concatenation
                     (variable-repetition (alternatives r-alpha
                                                        r-digit
                                                        (terminal +#\-+))
                                          :padding 1
                                          :maximum 62)
                     r-let-dig))))

(defrule r-domain
    (concatenation r-sub-domain
                   (variable-repetition (concatenation (terminal +#\.+)
                                                       r-sub-domain))))

(defrule r-snum
    (alternatives (concatenation (terminal +#\2+)
                                 (terminal +#\5+)
                                 (value-range-alternatives +#\0+ +#\5+))
                  (concatenation (terminal +#\2+)
                                 (value-range-alternatives +#\0+ +#\4+)
                                 r-digit)
                  (concatenation (value-range-alternatives +#\0+ +#\1+)
                                 r-digit
                                 r-digit)
                  (concatenation r-digit
                                 r-digit)
                  r-digit))

(defrule r-ipv4-address-literal
    (concatenation r-snum
                   (specific-repetition 3
                                        (concatenation (terminal +#\.+)
                                                       r-snum))))

(defrule r-ipv6-hex
    (variable-repetition r-hexdig :minimum 1 :maximum 4))

(defrule r-ipv6-full
    (concatenation r-ipv6-hex
                   (specific-repetition 7 (concatenation (terminal +#\:+)
                                                         r-ipv6-hex))))

(defrule r-ipv6-comp
    (alternatives
     (concatenation (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 5))))
     (concatenation r-ipv6-hex
                    (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 4))))
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 1)))
                    (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 3))))
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 2)))
                    (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 2))))
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 3)))
                    (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 1))))
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 4)))
                    (terminals +#\:+ +#\:+)
                    r-ipv6-hex)
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 5)))
                    (terminals +#\:+ +#\:+))))

(defrule r-ipv6v4-full
    (concatenation r-ipv6-hex
                   (specific-repetition 5 (concatenation (terminal +#\:+)
                                                         r-ipv6-hex))
                   (terminal +#\:+)
                   r-ipv4-address-literal))

(defrule r-ipv6v4-comp
    (alternatives
     (concatenation (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 3)
                                                      (terminal +#\:+)))
                    r-ipv4-address-literal)
     (concatenation r-ipv6-hex
                    (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 2)
                                                      (terminal +#\:+)))
                    r-ipv4-address-literal)
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 1)))
                    (terminals +#\:+ +#\:+)
                    (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 1)
                                                      (terminal +#\:+)))
                    r-ipv4-address-literal)
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 2)))
                    (terminals +#\:+ +#\:+)
                    r-ipv6-hex
                    r-ipv4-address-literal)
     (concatenation (optional-sequence (concatenation r-ipv6-hex
                                                      (variable-repetition (concatenation (terminal +#\:+)
                                                                                          r-ipv6-hex)
                                                                           :maximum 3)))
                    (terminals +#\:+ +#\:+)
                    r-ipv4-address-literal)))

(defrule r-ipv6-addr
    ;; These are ordered this way because r-ipv6-comp will sometimes partially
    ;; match against something that is fully matched by r-ipv6v4-comp, then
    ;; return prematurely.
    (alternatives r-ipv6-full
                  r-ipv6v4-full
                  r-ipv6v4-comp
                  r-ipv6-comp))

(defrule r-ipv6-address-literal
    (concatenation (terminals +#\I+ +\#P+ +#\v+ +#\6+ +#\:+)
                   r-ipv6-addr))

(defrule r-standardized-tag
  r-ldh-str)

(defrule r-dcontent
    (alternatives (value-range-alternatives 33 90)
                  (value-range-alternatives 94 126)))

(defrule r-general-address-literal
    (concatenation r-standardized-tag
                   (terminal +#\:+)
                   (variable-repetition r-dcontent :minimum 1)))

(defrule r-address-literal
    (concatenation (terminal +#\[+)
                   (alternatives r-ipv4-address-literal
                                 r-ipv6-address-literal
                                 ;; r-general-address-literal
                                 ;;
                                 ;; Note - general address literals are an
                                 ;; abstract grammar of which the only existing
                                 ;; implementation is IPv6. Therefore, we don't
                                 ;; match against this grammar so as to avoid
                                 ;; false positives.
                                 )
                   (terminal +#\]+)))

(defrule r-mailbox
    (concatenation r-local-part
                   (terminal +#\@+)
                   (alternatives r-domain
                                 r-address-literal)))
