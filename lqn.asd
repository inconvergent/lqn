(asdf:defsystem #:lqn
  :description "Lisp Query Notation"
  :version "1.12.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :in-order-to ((asdf:test-op (asdf:test-op #:lqn/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:yason #:uiop)
  :components ((:file "packages")
               (:file "basic-utils" :depends-on ("packages"))
               (:file "reader-macros" :depends-on ("basic-utils"))
               (:file "qry-utils" :depends-on ("reader-macros" "basic-utils"))
               (:file "docs" :depends-on ("qry-utils"))
               (:file "io" :depends-on ("docs"))
               (:file "pre-qry" :depends-on ("io" "qry-utils" "docs"))
               (:file "qry" :depends-on ("pre-qry"))
               (:file "sh" :depends-on ("qry"))
               (:file "qry-extra" :depends-on ("qry"))
               ))

(asdf:defsystem #:lqn/tests
  :depends-on (#:lqn #:prove #:uiop #:asdf)
  :version "1.12.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':lqn-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))
