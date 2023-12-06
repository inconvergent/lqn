(in-package #:jqn-tests)

(plan 1)

(defvar *test-data* (jqn::internal-path-string "test/sample.json"))


(subtest "jqn"
  (is (jqn:qryf *test-data*
        :q (*  _id (+@things (* name id))
                   (+@msg (string-downcase (@ :msg)))))
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
         :test #'equalp)

  (is (jqn::jsnout* (jqn:qryf *test-data* :q _))
      "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is (jqn::jsnout* (jqn:qryf *test-data* :q _))
      (jqn::jsnout* (jqn:qryf *test-data* :q (* _))))
  (is (jqn::jsnout* (jqn:qryf *test-data* :q _))
      (jqn::jsnout* (jqn:qryf *test-data* :q (& _))))
  (is (jqn::ldn-serialize
        (jqn:qryf *test-data* :q (* things)))
      #(((:THINGS . #(((:ID . 0) (:NAME . "Chris") (:EXTRA . "extra99")))))
        ((:THINGS
          . #(((:ID . 10) (:NAME . "Winters") (:EXTRA . "extra1"))
              ((:ID . 11) (:NAME . "Haii") (:EXTRA . "extra2"))
              ((:ID . 12) (:NAME . "Klein")))))
        ((:THINGS . #(((:ID . 31) (:NAME . "Star")) ((:ID . 32) (:NAME . "Ball"))))))
      :test #'equalp)
  )

(unless (finalize) (error "error in jqn"))

