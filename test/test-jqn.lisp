(in-package #:jqn-tests)

(plan 2)

(subtest "io"
  (is (jqn:ldnout *test-data-raw*) *test-data-raw* :test #'equalp)
  (is (jqn:ldnout (jqn:jsnloadf *test-data-fn*)) *test-data-raw* :test #'equalp)
  (is (jqn::jsnout* (jqn:jsnloadf *test-data-fn*))
      "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]
")
  (is (jqn:ldnout *test-data-2-raw*) *test-data-2-raw* :test #'equalp)
  (is (jqn:ldnout (jqn:jsnloadf *test-data-2-fn*)) *test-data-2-raw* :test #'equalp)
  (is (jqn::jsnout* (jqn:jsnloadf *test-data-2-fn*))
      "{\"credit\":\"Mega Corp.\",\"credit_URL\":\"http://fax.megacorp\",\"disclaimer_url\":null,\"copyright_url\":\"http://fax.megacorp/about/terms.asp\",\"image\":{\"url\":\"http://fax.megacorp/images/Logo.jpg\",\"title\":\"Mega Corp\",\"link\":\"http://fax.megacorp/yyyyyyyyy\"},\"suggested_pickup\":\"15 minutes after the hour\",\"suggested_pickup_period\":\"60\",\"dewpoint_c\":-22.2,\"dewpoint_f\":null,\"dewpoint_string\":\"-8.0 F (-22.2 C)\",\"heat_index_c\":-20.6,\"heat_index_f\":-5.0,\"heat_index_string\":\"-5.0 F (-20.6 C)\",\"observation_time\":\"Last Updated on Dec 5 2023, 9:37 pm CET\",\"current_observation\":{\"station_name\":\"Gulhuset\",\"observation_age\":42,\"dewpoint_day_high_f\":\"-7\",\"dewpoint_day_high_time\":\"8:47pm\",\"dewpoint_day_low_f\":-8.0,\"windchill_month_low_f\":-9,\"windchill_year_low_f\":-9},\"time_to_generate\":0.012046}
")
  (is (string-downcase (jqn::jsnout* (jqn:jsnloadf *test-data-2-fn*)))
      (string-downcase (jqn::jsnout* *test-data-2-raw*))))

(subtest "jqn qry"
  (is (jqn:ldnout (jqn:qryf *test-data-fn*
        :q (*$  _id (+@things (*$ name id))
                    (+@msg (string-downcase _)))))
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

  (is (jqn::jsnout* (jqn:qryf *test-data-fn*
        :q (*  _id (+@things (* name id))
                   (+@msg (string-downcase _)))))
      "[\"65679d23d38d711eaf999e89\",[\"Chris\",0],\"this is a message\",\"65679d23fe33bc4c240675c0\",[\"Winters\",10,\"Haii\",11,\"Klein\",12],\"hello, undefined! you have 1 unread messages.\",\"65679d235b4143d2932ea17a\",[\"Star\",31,\"Ball\",32],\"hello, undefined! you have 5 unread messages.\"]
"     :test #'equalp)

  (is (jqn:ldnout (jqn:qryf *test-data-fn* :q (*$ things)))
      #(((:THINGS . #(((:ID . 0) (:NAME . "Chris") (:EXTRA . "extra99")))))
        ((:THINGS
          . #(((:ID . 10) (:NAME . "Winters") (:EXTRA . "extra1"))
              ((:ID . 11) (:NAME . "Haii") (:EXTRA . "extra2"))
              ((:ID . 12) (:NAME . "Klein")))))
        ((:THINGS . #(((:ID . 31) (:NAME . "Star")) ((:ID . 32) (:NAME . "Ball"))))))
      :test #'equalp)

  (is (jqn::jsnout* (jqn:qryf *test-data-fn* :q _))
      "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]
")
  (is (jqn::jsnout* (jqn:qryf *test-data-fn* :q _))
      (jqn::jsnout* (jqn:qryf *test-data-fn* :q (*$ _))))
  ; (is (jqn::jsnout* (jqn:qryf *test-data-fn* :q _))
  ;     (jqn::jsnout* (jqn:qryf *test-data-fn* :q (& _))))
  )

(unless (finalize) (error "error in jqn"))

