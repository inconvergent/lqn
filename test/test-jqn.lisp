(in-package #:jqn-tests)

(plan 1)


(subtest "jqn"
  (is (jqn:qryf (jqn::internal-path-string "test/sample.json")
        :q (*  _id (+@things (* name id))
                   (+@msg (string-downcase (jqn::@ :msg)))))
      #(((:_ID . "65679d23d38d711eaf999e89")
         (:THINGS . #(((:NAME . "Chris") (:ID . 0))))
         (:MSG . "this is a message"))
        ((:_ID . "65679d23fe33bc4c240675c0")
         (:THINGS
          . #(((:NAME . "Winters") (:ID . 10)) ((:NAME . "Haii") (:ID . 11))
              ((:NAME . "Klein") (:ID . 12))))
         (:MSG . "hello, undefined! you have 1 unread messages."))
        ((:_ID . "65679d235b4143d2932ea17a")
         (:THINGS
          . #(((:NAME . "Star") (:ID . 31)) ((:NAME . "Ball") (:ID . 32))))
         (:MSG . "hello, undefined! you have 5 unread messages.")))
         :test #'equalp))

(unless (finalize) (error "error in jqn"))

