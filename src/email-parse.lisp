(in-package :cl-user)
(defpackage email-parse
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match
   :email-match)
  (:export
   :parse-octets
   :parse
   :test))
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

(declaim (type matched *local-part-lower*))
(declaim (type matched *local-part-upper*))
(declaim (type matched *domain-or-address-literal-lower*))
(declaim (type matched *domain-or-address-literal-upper*))

(defparameter *local-part-lower* nil)
(defparameter *local-part-upper* nil)
(defparameter *domain-or-address-literal-lower* nil)
(defparameter *domain-or-address-literal-upper* nil)

(defun clear-captured ()
  (setf *local-part-lower* nil
        *local-part-upper* nil
        *domain-or-address-literal-lower* nil
        *domain-or-address-literal-upper* nil))

;; ------------------------------------------------------------------------------

(defrule r-parse-mailbox
    (concatenation (capture *local-part-lower*
                            *local-part-upper*
                            r-local-part)
                   (terminal +#\@+)
                   (capture *domain-or-address-literal-lower*
                            *domain-or-address-literal-upper*
                            (alternatives r-domain
                                          r-address-literal))))

(defun plist (local-part domain-or-address-literal)
  (when (or local-part domain-or-address-literal)
    (list :local-part local-part
          :domain domain-or-address-literal)))

(defun parse-octets (octets lower upper &key (result-type 'string) plist)
  (when (<= (length octets) *mailbox-max-length*)
    (let ((parsed (r-parse-mailbox octets lower upper)))
      (multiple-value-prog1
          (when (and parsed (= parsed (length octets)))
            (let ((local-part (subseq octets
                                      *local-part-lower*
                                      *local-part-upper*))
                  (domain-or-address-literal (subseq octets
                                                     *domain-or-address-literal-lower*
                                                     *domain-or-address-literal-upper*)))
              (when (<= (length local-part) *local-part-max-length*)
                (case result-type
                  (string (let ((lp-str (ascii-code-string local-part))
                                (doal-str (ascii-code-string domain-or-address-literal)))
                            (if plist
                                (plist lp-str doal-str)
                                (values lp-str doal-str))))
                  (otherwise (if plist
                                 (plist local-part domain-or-address-literal)
                                 (values local-part domain-or-address-literal)))))))
        (clear-captured)))))

(defun parse (str &key plist)
  (let ((octets (ascii-string-code '(simple-array (unsigned-byte 8) (*)) str)))
    (multiple-value-bind (local-part domain-or-address-literal)
        (parse-octets octets 0 (length octets))
      (if plist
          (plist local-part domain-or-address-literal)
          (values local-part domain-or-address-literal)))))

(defun test ()
  (and (every #'identity
              (mapcar #'parse
                      '(;; https://en.wikipedia.org/wiki/Email_address#Examples
                        "simple@example.com"
                        "very.common@example.com"
                        "disposable.style.email.with+symbol@example.com"
                        "other.email-with-hyphen@example.com"
                        "fully-qualified-domain@example.com"
                        "user.name+tag+sorting@example.com"
                        "x@example.com"
                        "example-indeed@strange-example.com"
                        "test/test@test.com"
                        "admin@mailserver1"
                        "example@s.example"
                        "\" \"@example.org"
                        "\"john..doe\"@example.org"
                        "mailhost!username@example.org"
                        "\"very.(),:;<>[]\\\".VERY.\\\"very@\\\\ \\\"very\\\".unusual\"@strange.example.com"
                        "user%example.com@example.org"
                        "user-@example.org"
                        "postmaster@[123.123.123.123]"
                        "postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]"

                        ;; https://github.com/dominicsayers/isemail/blob/cfeefc3f2f88cb195053f6a309fa4f640cd369b5/test/tests.xml
                        "test@io" ; valid
                        "test@iana.org" ; valid
                        "test@nominet.org.uk" ; valid
                        "test@about.museum" ; valid
                        "a@iana.org" ; valid
                        "test@e.com" ; valid
                        "test@iana.a" ; valid
                        "test.test@iana.org" ; valid
                        "!#$%&`*+/=?^`{|}~@iana.org" ; valid
                        "123@iana.org" ; valid
                        "test@123.com" ; valid
                        "test@iana.123" ; valid rfc5321
                        "test@255.255.255.255" ; valid rfc5321
                        "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklm@iana.org" ; valid
                        "test@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.com" ; valid
                        "test@mason-dixon.com" ; valid
                        "test@c--n.com" ; valid
                        "test@iana.co-uk" ; valid
                        "a@a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v" ; valid
                        "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklm@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghi" ; valid
                        "\"test\"@iana.org"; valid rfc 5321
                        "\"\"@iana.org" ; valid rfc 5321
                        "\"\\a\"@iana.org" ; valid rfc 5321
                        "\"\\\"\"@iana.org" ; valid rfc 5321
                        "\"\\\\\"@iana.org" ; valid rfc 5321
                        "\"test\\ test\"@iana.org" ; valid rfc 5321
                        "test@[255.255.255.255]" ; valid rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]" ; valid rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555::8888]" ; valid rfc 5321
                        "test@[IPv6:::3333:4444:5555:6666:7777:8888]" ; valid rfc 5321
                        "test@[IPv6:::]" ; valid rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555:6666:255.255.255.255]" ; valid rfc 5321
                        "test@[IPv6:1111:2222:3333:4444::255.255.255.255]" ; valid rfc 5321
                        "test@xn--hxajbheg2az3al.xn--jxalpdlp" ; valid
                        "xn--test@iana.org" ; valid
                        "test@org" ; valid
                        "test@test.com" ; valid
                        "test@nic.no" ; valid
                        )))
       (every #'null
              (mapcar #'parse
                      '(;; https://en.wikipedia.org/wiki/Email_address#Examples
                        "Abc.example.com"
                        "A@b@c@example.com"
                        "a\"b(c)d,e:f;g<h>i[j\\k]l@example.com"
                        "just\"not\"right@example.com"
                        "this is\"not\\allowed@example.com"
                        "this\\ still\\\"not\\\\allowed@example.com"
                        "1234567890123456789012345678901234567890123456789012345678901234+x@example.com"
                        "i_like_underscore@but_its_not_allowed_in_this_part.example.com"
                        "QA[icon]CHOCOLATE[icon]@test.com"

                        ;; https://github.com/dominicsayers/isemail/blob/cfeefc3f2f88cb195053f6a309fa4f640cd369b5/test/tests.xml
                        "" ; error
                        "test" ; error
                        "@" ; error
                        "test@" ; error
                        "@io" ; error
                        "@iana.org" ; error
                        ".test@iana.org" ; error
                        "test.@iana.org" ; error
                        "test..iana.org" ; error
                        "test_exa-ample.com" ; error
                        "test\\@test@iana.org" ; error
                        "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklmn@iana.org" ; error
                        "test@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklm.com" ;; label too long, rfc 5322
                        "test@-iana.org" ; error
                        "test@iana-.com" ; error
                        "test@.iana.org" ; error
                        "test@iana.org." ; error
                        "test@iana..com" ; error
                        "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklm@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij" ; error rfc 5322 too long
                        "a@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg.hij" ; error rfc 5322 too long
                        "a@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg.hijk" ; error rfc 5322 domain too long
                        "\"\"\"@iana.org" ; error
                        "\"\\\"@iana.org" ; error
                        "test\"@iana.org" ; error
                        "\"test@iana.org" ; error
                        "\"test\"test@iana.org" ; error
                        "test\"text\"@iana.org" ; error
                        "\"test\"\"test\"@iana.org" ; error
                        "\"test\".\"test\"@iana.org" ; error
                        "\"test\".test@iana.org" ; error
                        "\"abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghj\"@iana.org" ; error rfc 5322
                        "\"abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefg\\h\"@iana.org" ; error rfc 5322
                        "test@a[255.255.255.255]" ; error
                        "test@[255.255.255]" ;; test 63 - claims domain literal, unsure, rfc5322
                        "test@[255.255.255.255.255]" ;; test 64 - claims domain literal, unsure, rfc5322
                        "test@[255.255.255.256]" ; valid rfc 5322
                        "test@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555:6666::7777:8888]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6::3333:4444:5555:6666:7777:8888]" ; valid rfc 5322 not rfc 5321
                        "test@[1111:2222:3333:4444:5555:6666:7777:8888]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555:6666:7777]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6:1111::4444:5555::8888]" ;; test 76, claims is valid, rfc 5322
                        "test@[IPv6:1111:2222:3333:4444:5555:255.255.255.255]" ; valid rfc 5322
                        "test@[IPv6:1111:2222:3333:4444:5555:6666:7777:255.255.255.255]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555:6666::255.255.255.255]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:::255.255.255.255]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6::255.255.255.255]" ; valid rfc 5322 not rfc 5321
                        "test@[IPv6:1111:2222:3333:4444:5555:6666:7777:888G]" ; error
                        "test@[IPv6:1111:2222:3333:4444:5555:6666::8888]" ;; error, test 71
                        " test @iana.org" ; error
                        "test@ iana .com" ; error
                        "test . test@iana.org" ; error
                        "(comment)test@iana.org" ; valid within message but cannot be used for envelope
                        "((comment)test@iana.org" ; error
                        "(comment(comment))test@iana.org" ; valid within message but cannot be used for envelope
                        "test@(comment)iana.org" ; error
                        "test(comment)test@iana.org" ; error
                        "test@(comment)[255.255.255.255]" ; error
                        "(comment)abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghiklm@iana.org" ; valid within message but cannot be used for envelope
                        "test@(comment)abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghikl.com" ; error
                        "(comment)test@abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghik.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghik.abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk.abcdefghijklmnopqrstuvwxyzabcdefghijk.abcdefghijklmnopqrstu" ; valid within message but cannot be used for envelope
                        "test@iana.org-" ; error
                        "\"test@iana.org" ; error
                        "(test@iana.org" ; error
                        "test@(iana.org" ; error
                        "test@[1.2.3.4" ; error
                        "\"test\\\"@iana.org" ; error
                        "(comment\\)test@iana.org" ; error
                        "test@iana.org(comment\\)" ; error
                        "test@iana.org(comment\\" ; error
                        "test@[RFC-5322-domain-literal]" ; valid rfc 5322 not rfc 5321
                        "test@[RFC-5322]-domain-literal]" ; error
                        "test@[RFC-5322-[domain-literal]" ; error
                        "test@[RFC-5322-\\]-domain-literal]" ; valid rfc 5322 not rfc 5321
                        "test@[RFC-5322-domain-literal\\]" ; error
                        "test@[RFC-5322-domain-literal\\" ; error
                        "test@[RFC 5322 domain literal]" ; valid rfc 5322 not rfc 5321
                        "test@[RFC-5322-domain-literal] (comment)" ; valid rfc 5322 not rfc 5321
                        "@iana.org" ; error
                        "test@.org" ; error
                        "\"\"@iana.org" ; error
                        "\"\\\"@iana.org" ; error
                        "()test@iana.org" ; error
                        " test@iana.org" ; valid within message but cannot be used for envelope
                        "test@iana.org " ; valid within message but cannot be used for envelope
                        "test@[IPv6:1::2:]" ; valid rfc 5322 not rfc 5321
                        ;; "\"test\\©\"@iana.org" ; we don't support unicode
                        "test@iana/icann.org" ; valid rfc 5322 not rfc 5321
                        "test.(comment)test@iana.org" ; error
                        )))
       (every #'null
              (mapcar (lambda (addr)
                        (parse-octets addr 0 (length addr)))
                      (list
                       ;; https://github.com/dominicsayers/isemail/blob/cfeefc3f2f88cb195053f6a309fa4f640cd369b5/test/tests.xml
                       ;; 57
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"test")
                                    (list #x00)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"@iana.org"))
                       ;; 58
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"test\\")
                                    (list #x00)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"@iana.org"))
                       ;; 88
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " test@iana.org"))
                       ;; 89
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " test@iana.org"))
                       ;; 99
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org")
                                    (list #x0a))
                       ;; 115
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@[RFC-5322-\\")
                                    (list #x07)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "-domain-literal"))
                       ;; 116
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@[RFC-5322-\\")
                                    (list #x09)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "-domain-literal"))
                       ;; 127
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org")
                                    (list #x0d))
                       ;; 128
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0d)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 129
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"")
                                    (list #x0d)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test\"@iana.org"))
                       ;; 130
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "(")
                                    (list #x0d)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       ")test@iana.org"))
                       ;; 131
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org(")
                                    (list #x0d)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       ")"))
                       ;; 132
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 133
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"@iana.org"))
                       ;; 134
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\\")
                                    (list #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"@iana.org"))
                       ;; 135
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "(")
                                    (list #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       ")test@iana.org"))
                       ;; 136
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x07)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "@iana.org"))
                       ;; 137
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@")
                                    (list #x07)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       ".org"))
                       ;; 138
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"")
                                    (list #x07)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"@iana.org"))
                       ;; 139
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"\\")
                                    (list #x07)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "\"@iana.org"))
                       ;; 140
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "(")
                                    (list #x07)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       ")test@iana.org"))
                       ;; 141
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 142
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 143
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 144
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " test@iana.org"))
                       ;; 145
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 146
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org"))
                       ;; 147
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " test@iana.org"))
                       ;; 148
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " "))
                       ;; 149
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " "))
                       ;; 150
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org")
                                    (list #x0d #x0a))
                       ;; 151
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a))
                       ;; 152
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org ")
                                    (list #x0d #x0a))
                       ;; 153
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " "))
                       ;; 154
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org ")
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")
                                    (list #x0d #x0a))
                       ;; 155
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org ")
                                    (list #x0d #x0a)
                                    (list #x0d #x0a))
                       ;; 156
                       (concatenate '(simple-array (unsigned-byte 8) (*))
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       "test@iana.org ")
                                    (list #x0d #x0a)
                                    (list #x0d #x0a)
                                    (ascii-string-code '(simple-array (unsigned-byte 8) (*))
                                                       " ")))))))
