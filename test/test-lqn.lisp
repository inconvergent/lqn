(in-package #:lqn-tests)

(plan 7)

(subtest "utils"
  (is (lqn:sub? "aabb" "ab") "aabb")
  (is (lqn:sub? "aabb" "abc") nil)
  (is (lqn:sub? "AABB" "ab") nil)
  (is (lqn:isub? "AABB" "ab") "AABB")
  (is (lqn:isub? "AABB" "abc") nil)
  (is (lqn:pref? "AABB" "AA") "AABB")
  (is (lqn:ipref? "AABB" "aa") "AABB")
  (is (lqn:suf? "AABB" "BB") "AABB")
  (is (lqn:suf? "AABB" "bb") nil)
  (is (lqn:isuf? "AABB" "bb") "AABB")
  (is (lqn::ct/kv/str "AABB") "AABB")
  (is (lqn::ct/kv/str :AABB) "aabb")
  (is (lqn::ct/kv/str 'AABB) 'AABB)
  (is (lqn::ct/kv/str (+ 1 2)) 3)
  (is (lqn::ct/kv/str (progn 'abc)) 'abc)
  (is (lqn:msym? 'aa 'aa) 'aa)
  (is (lqn:msym? 'aa :aa) nil)
  (is (lqn:msym? 'aabb "ab") 'aabb)
  (is (lqn:msym? 'aabb (progn "ab")) nil)
  (is (lqn:msym? 'aabb (progn 'aabb)) 'aabb)
  (is (lqn:msym? 'AABB (progn 'aabb)) 'aabb)
  (is (lqn:msym? :AABB (progn :aabb)) :aabb)
  (is (lqn::unpack-mode "?@fxfx")    '(:? "fxfx"))
  (is (lqn::unpack-mode '?@fxfx)     '(:? fxfx))
  (is (lqn::unpack-mode '(?@fxfx))   '(:? (fxfx)))
  (is (lqn::unpack-mode '(:? fxfx))  '(:? fxfx))
  (is (lqn::unpack-mode '(:?@fxfx))  '(:? (:fxfx)))
  (is (lqn::unpack-mode '(fxfx :ss)) '(:+ (fxfx :ss)))
  (is (lqn::unpack-mode "fxfx")      '(:+ "fxfx"))
  (is (lqn::unpack-mode 'fxfx :y)    '(:y fxfx))
  (is (lqn::unpack-mode 'fxfx :y)    '(:y fxfx)))

(subtest "io"
  (is (lqn:ldnout *test-data-raw*) *test-data-raw* :test #'equalp)
  (is (lqn:ldnout (lqn:jsnloadf *test-data-fn*)) *test-data-raw* :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnloadf *test-data-fn*))
          "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is (lqn:ldnout *test-data-2-raw*) *test-data-2-raw* :test #'equalp)
  (is (lqn:ldnout (lqn:jsnloadf *test-data-2-fn*)) *test-data-2-raw* :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnloadf *test-data-2-fn*))
          "{\"credit\":\"Mega Corp.\",\"credit_URL\":\"http://fax.megacorp\",\"disclaimer_url\":null,\"copyright_url\":\"http://fax.megacorp/about/terms.asp\",\"image\":{\"url\":\"http://fax.megacorp/images/Logo.jpg\",\"title\":\"Mega Corp\",\"link\":\"http://fax.megacorp/yyyyyyyyy\"},\"suggested_pickup\":\"15 minutes after the hour\",\"suggested_pickup_period\":\"60\",\"dewpoint_c\":-22.2,\"dewpoint_f\":null,\"dewpoint_string\":\"-8.0 F (-22.2 C)\",\"heat_index_c\":-20.6,\"heat_index_f\":-5.0,\"heat_index_string\":\"-5.0 F (-20.6 C)\",\"observation_time\":\"Last Updated on Dec 5 2023, 9:37 pm CET\",\"current_observation\":{\"station_name\":\"Gulhuset\",\"observation_age\":42,\"dewpoint_day_high_f\":\"-7\",\"dewpoint_day_high_time\":\"8:47pm\",\"dewpoint_day_low_f\":-8.0,\"windchill_month_low_f\":-9,\"windchill_year_low_f\":-9},\"time_to_generate\":0.012046}")
  (is (lqn:sdwn (lqn::jsnstr (lqn:jsnloadf *test-data-2-fn*)))
      (lqn:sdwn (lqn::jsnstr *test-data-2-raw*))))

(subtest "lqn qry preproc"
  (is (lqn::pre/$$ '(:ccc :ddd "IIUJ" "%@UU" :?@aa :?@bb ("cc" (progn _)) (:+ "ABC" (print _)) (:% "ABC" _) (:kkk "ABC" _)))
      '((:+ "ccc" :_) (:+ "ddd" :_) (:+ "IIUJ" :_) (:% "UU" :_) (:? "aa" :_) (:? "bb" :_)
        (:+ "cc" (PROGN _)) (:+ "ABC" (PRINT _)) (:% "ABC" :_) (:+ "kkk" "ABC")))
  (is (lqn::pre/** '(ccc :ddd "IIUJ" "%@UU" ?@aa ?@bb ("cc" (progn _)) (% "ABC" (print _)) (:% "ABC")))
     '((:? (WHEN (CCC :_) :_)) (:? (AND (LQN:STR? :_) (LQN:ISUB? :_ "ddd"))) (:? (AND (LQN:STR? :_) (LQN:SUB? :_ "IIUJ")))
       (:% (AND (LQN:STR? :_) (LQN:SUB? :_ "UU"))) (:? (WHEN (AA :_) :_)) (:? (WHEN (BB :_) :_)) (:? ("cc" (PROGN _))) (:? (% "ABC" (PRINT _)))
       (:% (AND (LQN:STR? :_) (LQN:SUB? :_ "ABC"))))))

(subtest "modifiers"
  (is (lqn:qry #("abc" "def") [(sub? _ :s@a)]) #("abc") :test #'equalp)
  (is (lqn:qry "abc x def x hij" nil) nil)
  (is (lqn:qry "aa" s@_) "aa")
  (is (lqn:qry 1 s@_) "1")
  (is (lqn:qry 1 (s@progn _)) "1")
  (is (lqn:qry "a" _@sup) "A")
  (is (lqn:qry "a" (progn _@sup)) "A")
  (is (lqn:qry 1 (progn (s@progn _))) "1")
  (is (lqn:qry "abc x def x hij" ∅) nil))

(subtest "lqn qry identities"
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* ($* _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*$ _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (** _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*map _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-2-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-2-fn* ($$ _)))))

(subtest "lqn qry 1"
  (is (lqn:ldnout (lqn:jsnqryf *test-data-fn* (|| #{:_id (:things #[:name :?@extra])})))
      #(((:_ID . "65679d23d38d711eaf999e89") (:THINGS . #("Chris" "extra99")))
        ((:_ID . "65679d23fe33bc4c240675c0") (:THINGS . #("Winters" "extra1" "Haii" "extra2" "Klein")))
        ((:_ID . "65679d235b4143d2932ea17a") (:THINGS . #("Star" "Ball")))) :test #'equalp)

  (is (lqn:ldnout (lqn:jsnqryf *test-data-fn* (|| #{:_id (:things #[:%@extra])})))
      #(((:_ID . "65679d23d38d711eaf999e89") (:THINGS . #("extra99")))
        ((:_ID . "65679d23fe33bc4c240675c0") (:THINGS . #("extra1" "extra2")))
        ((:_ID . "65679d235b4143d2932ea17a") (:THINGS . #()))) :test #'equalp)

  (is (lqn:ldnout (lqn:jsnqryf *test-data-fn* (*$  :_id (:+@things (*$ :name :id)) (:+@msg (sdwn _)))))
      #(((:_ID . "65679d23d38d711eaf999e89")
         (:THINGS . #(((:NAME . "Chris") (:ID . 0)))) (:MSG . "this is a message"))
        ((:_ID . "65679d23fe33bc4c240675c0")
         (:THINGS . #(((:NAME . "Winters") (:ID . 10)) ((:NAME . "Haii") (:ID . 11))
                      ((:NAME . "Klein") (:ID . 12))))
         (:MSG . "hello, undefined! you have 1 unread messages."))
        ((:_ID . "65679d235b4143d2932ea17a")
         (:THINGS . #(((:NAME . "Star") (:ID . 31)) ((:NAME . "Ball") (:ID . 32))))
         (:MSG . "hello, undefined! you have 5 unread messages."))) :test #'equalp)

  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                          ($*  :_id (:+@things ($* :name :id)) (:+@msg (sdwn _)))))
      "[\"65679d23d38d711eaf999e89\",[\"Chris\",0],\"this is a message\",\"65679d23fe33bc4c240675c0\",[\"Winters\",10,\"Haii\",11,\"Klein\",12],\"hello, undefined! you have 1 unread messages.\",\"65679d235b4143d2932ea17a\",[\"Star\",31,\"Ball\",32],\"hello, undefined! you have 5 unread messages.\"]")

  (is (lqn:ldnout (lqn:jsnqryf *test-data-fn* (*$ :things)))
      #(((:THINGS . #(((:ID . 0) (:NAME . "Chris") (:EXTRA . "extra99")))))
        ((:THINGS . #(((:ID . 10) (:NAME . "Winters") (:EXTRA . "extra1"))
                      ((:ID . 11) (:NAME . "Haii") (:EXTRA . "extra2"))
                      ((:ID . 12) (:NAME . "Klein")))))
        ((:THINGS . #(((:ID . 31) (:NAME . "Star")) ((:ID . 32) (:NAME . "Ball")))))) :test #'equalp)

  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _))
          "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]"))

(subtest "test data"
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*$ _ :-@_id :-@things)))
"[{\"index\":0,\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"index\":1,\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* ($* (:things (ind* (*$ _ :-@extra))))))
"[{\"id\":0,\"name\":\"Chris\"},{\"id\":10,\"name\":\"Winters\"},{\"id\":31,\"name\":\"Star\"}]")

  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* #{ _ :-@_id :-@things}))
"[{\"index\":0,\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"index\":1,\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                          #[:_id (:+@things #[:name :id]) (:+@msg (sdwn _))]))
      "[\"65679d23d38d711eaf999e89\",[\"Chris\",0],\"this is a message\",\"65679d23fe33bc4c240675c0\",[\"Winters\",10,\"Haii\",11,\"Klein\",12],\"hello, undefined! you have 1 unread messages.\",\"65679d235b4143d2932ea17a\",[\"Star\",31,\"Ball\",32],\"hello, undefined! you have 5 unread messages.\"]")

  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                         (compct #{(:%@things (compct #{(:%@extra (?? _ (sup _)))}))})))
"[{\"things\":[{\"extra\":\"EXTRA99\"}]},{\"things\":[{\"extra\":\"EXTRA1\"},{\"extra\":\"EXTRA2\"}]}]")
  (is (lqn:jsnqryf *test-data-fn* #[(:%@index (?? _ (= _ 0) _))]) #(0) :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (|| #[:things] (flatn* _) #[:id])))
          "[0,10,11,12,31,32]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (|| #(#[:things]) (flatn* _ 2) #[:id])))
          "[0,10,11,12,31,32]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                          (|| #(#[:things]) (flatn* _ 2) #[:id]
                              (*fld (list) acc (cons (1+ _) acc)) (reverse _))))
          "[1,11,12,13,32,33]"))

(unless (finalize) (error "error in test-lqn"))
