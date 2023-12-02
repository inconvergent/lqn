(asdf:defsystem #:jqn
  :description "JSON query language"
  :version "0.0.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  ; :in-order-to ((asdf:test-op (asdf:test-op #:jqn/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:yason)
  :components ((:file "packages")
               (:file "init" :depends-on ("packages"))
               (:file "utils" :depends-on ("init"))
               (:file "docs" :depends-on ("utils"))
               (:file "jqn" :depends-on ("utils" "docs"))))

; (asdf:defsystem #:jqn/tests
;   :depends-on (#:veq #:jqn #:prove #+:jqn-parallel #:lparallel #:uiop #:asdf)
;   :version "1.0.0"
;   :perform (asdf:test-op (o s) (uiop:symbol-call ':jqn-tests
;                                  #+:jqn-parallel '#:p/run-tests
;                                  #-:jqn-parallel '#:run-tests))
;   :pathname "test/" :serial t
;   :components ((:file "run")))
