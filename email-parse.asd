(defsystem "email-parse"
  :version "1.0.0"
  :license "BSD-2"
  :description "A fast, RFC-compliant email address validator and parser."
  :author "Stephen Youts"
  :depends-on ("trivial-us-ascii"
               "abnf-match")
  :components
  ((:static-file "LICENSE")
   (:static-file "README.md")
   (:module "src"
    :components ((:file "email-match")
                 (:file "email-parse" :depends-on ("email-match")))))
  :in-order-to ((test-op (test-op "email-parse/tests"))))

(defsystem "email-parse/tests"
  :author "Stephen Youts"
  :license "BSD-2"
  :description "Tests for email-parse."
  :depends-on ("trivial-us-ascii"
               "email-parse")
  :components ((:module "tests"
                :components
                ((:file "email-parse-tests"))))
  :perform (test-op (op c) (symbol-call :email-parse.tests :test)))
