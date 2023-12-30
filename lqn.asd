(asdf:defsystem #:lqn
  :description "Lisp Query Notation"
  :version "1.0.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:lqn/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:yason)
  :components ((:file "packages")
               (:file "init" :depends-on ("packages"))
               (:file "reader-macros" :depends-on ("init"))
               (:file "utils" :depends-on ("reader-macros"))
               (:file "docs" :depends-on ("utils"))
               (:file "io" :depends-on ("docs"))
               (:file "qry" :depends-on ("io" "utils" "docs"))
               (:file "sh" :depends-on ("qry"))))

(asdf:defsystem #:lqn/tests
  :depends-on (#:lqn #:prove #:uiop #:asdf)
  :version "1.0.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':lqn-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))
