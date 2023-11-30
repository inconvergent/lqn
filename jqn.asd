(asdf:defsystem #:jqn
  :description "JSON query language"
  :version "0.0.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  ; :in-order-to ((asdf:test-op (asdf:test-op #:grph/tests)))
  :licence "MIT" :pathname "src/" :serial nil
  :depends-on (#:cl-json)
  :components ((:file "packages")
               (:file "init" :depends-on ("packages"))
               (:file "utils" :depends-on ("init"))

               (:file "docs" :depends-on ("utils"))

               (:file "jqn" :depends-on ("utils" "docs"))))

; (asdf:defsystem #:grph/tests
;   :depends-on (#:veq #:grph #:prove #+:grph-parallel #:lparallel #:uiop #:asdf)
;   :version "1.0.0"
;   :perform (asdf:test-op (o s) (uiop:symbol-call ':grph-tests
;                                  #+:grph-parallel '#:p/run-tests
;                                  #-:grph-parallel '#:run-tests))
;   :pathname "test/" :serial t
;   :components ((:file "run")))
