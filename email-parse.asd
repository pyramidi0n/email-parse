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
                 (:file "email-parse" :depends-on ("email-match"))))))
