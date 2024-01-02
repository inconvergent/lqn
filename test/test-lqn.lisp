(in-package #:lqn-tests)

(plan 8)

(subtest "utils"
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
  (is (lqn:lqnout *test-data-raw*) *test-data-raw* :test #'equalp)
  (is (lqn:lqnout (lqn:jsnloadf *test-data-fn*)) *test-data-raw* :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnloadf *test-data-fn*))
          "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is (lqn:lqnout *test-data-2-raw*) *test-data-2-raw* :test #'equalp)
  (is (lqn:lqnout (lqn:jsnloadf *test-data-2-fn*)) *test-data-2-raw* :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnloadf *test-data-2-fn*))
          "{\"credit\":\"Mega Corp.\",\"credit_URL\":\"http://fax.megacorp\",\"disclaimer_url\":null,\"copyright_url\":\"http://fax.megacorp/about/terms.asp\",\"image\":{\"url\":\"http://fax.megacorp/images/Logo.jpg\",\"title\":\"Mega Corp\",\"link\":\"http://fax.megacorp/yyyyyyyyy\"},\"suggested_pickup\":\"15 minutes after the hour\",\"suggested_pickup_period\":\"60\",\"dewpoint_c\":-22.2,\"dewpoint_f\":null,\"dewpoint_string\":\"-8.0 F (-22.2 C)\",\"heat_index_c\":-20.6,\"heat_index_f\":-5.0,\"heat_index_string\":\"-5.0 F (-20.6 C)\",\"observation_time\":\"Last Updated on Dec 5 2023, 9:37 pm CET\",\"current_observation\":{\"station_name\":\"Gulhuset\",\"observation_age\":42,\"dewpoint_day_high_f\":\"-7\",\"dewpoint_day_high_time\":\"8:47pm\",\"dewpoint_day_low_f\":-8.0,\"windchill_month_low_f\":-9,\"windchill_year_low_f\":-9},\"time_to_generate\":0.012046}")
  (is (lqn:sdwn (lqn::jsnstr (lqn:jsnloadf *test-data-2-fn*)))
      (lqn:sdwn (lqn::jsnstr *test-data-2-raw*))))

(subtest "lqn qry identities"
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _))
      (lqn::jsnstr (lqn:jsnqryf *test-data-fn* ($* _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _))
      (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*$ _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _))
      (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (** _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _))
      (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*map _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-2-fn* _))
      (lqn::jsnstr (lqn:jsnqryf *test-data-2-fn* ($$ _)))))

(subtest "lqn qry 1"

  (is (lqn::preproc/$$
        '(ccc :ddd "IIUJ" "%@UU" ?@aa ?@bb ("cc" (progn _))
          (% "ABC" (print _)) (:% "ABC" _)))
      '((:+ "ccc" :_) (:+ "ddd" :_) (:+ "IIUJ" :_) (:% "UU" :_) (:? "aa" :_)
       (:? "bb" :_) (:+ "cc" (PROGN _)) (:+ "%" "ABC") (:% "ABC" _)))
  (is (lqn::preproc/**
        '(ccc :ddd "IIUJ" "%@UU" ?@aa ?@bb ("cc" (progn _))
          (% "ABC" (print _)) (:% "ABC")))
      '((:? (WHEN (CCC :_) :_)) (:? (WHEN (LQN:ISUB? :_ "ddd") :_))
       (:? (WHEN (LQN:SUB? :_ "IIUJ") :_)) (:% (WHEN (LQN:SUB? :_ "UU") :_))
       (:? (WHEN (AA :_) :_)) (:? (WHEN (BB :_) :_)) (:? ("cc" (PROGN _)))
       (:? (% "ABC" (PRINT _))) (:% (WHEN (LQN:SUB? :_ "ABC") :_))))

  (is (lqn:lqnout (lqn:jsnqryf *test-data-fn*
        (*$  _id (+@things (*$ name id))
                 (+@msg (sdwn _)))))
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

  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                          ($*  _id (+@things ($* name id)) (+@msg (sdwn _)))))
      "[\"65679d23d38d711eaf999e89\",[\"Chris\",0],\"this is a message\",\"65679d23fe33bc4c240675c0\",[\"Winters\",10,\"Haii\",11,\"Klein\",12],\"hello, undefined! you have 1 unread messages.\",\"65679d235b4143d2932ea17a\",[\"Star\",31,\"Ball\",32],\"hello, undefined! you have 5 unread messages.\"]")

  (is (lqn:lqnout (lqn:jsnqryf *test-data-fn* (*$ things)))
      #(((:THINGS . #(((:ID . 0) (:NAME . "Chris") (:EXTRA . "extra99")))))
        ((:THINGS
          . #(((:ID . 10) (:NAME . "Winters") (:EXTRA . "extra1"))
              ((:ID . 11) (:NAME . "Haii") (:EXTRA . "extra2"))
              ((:ID . 12) (:NAME . "Klein")))))
        ((:THINGS . #(((:ID . 31) (:NAME . "Star")) ((:ID . 32) (:NAME . "Ball"))))))
      :test #'equalp)

  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _))
          "[{\"_id\":\"65679d23d38d711eaf999e89\",\"index\":0,\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}],\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"_id\":\"65679d23fe33bc4c240675c0\",\"index\":1,\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}],\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"_id\":\"65679d235b4143d2932ea17a\",\"things\":[{\"id\":31,\"name\":\"Star\"},{\"id\":32,\"name\":\"Ball\"}],\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]"))

(subtest "lqn qry 2"
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*$ _ -@_id -@things)))
"[{\"index\":0,\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"index\":1,\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* ($* (things (*n (*$ _ -@extra) 0)))))
"[{\"id\":0,\"name\":\"Chris\"},{\"id\":10,\"name\":\"Winters\"},{\"id\":31,\"name\":\"Star\"}]"))

(subtest "lqn qry reader macros"
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* #{ _ -@_id -@things}))
"[{\"index\":0,\"msg\":\"this is a message\",\"fave\":\"strawberry\"},{\"index\":1,\"msg\":\"Hello, undefined! You have 1 unread messages.\",\"fave\":\"strawberry\"},{\"msg\":\"Hello, undefined! You have 5 unread messages.\",\"fave\":\"blueberry\"}]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                          #[_id (+@things #[name id]) (+@msg (sdwn _))]))
      "[\"65679d23d38d711eaf999e89\",[\"Chris\",0],\"this is a message\",\"65679d23fe33bc4c240675c0\",[\"Winters\",10,\"Haii\",11,\"Klein\",12],\"hello, undefined! you have 1 unread messages.\",\"65679d235b4143d2932ea17a\",[\"Star\",31,\"Ball\",32],\"hello, undefined! you have 5 unread messages.\"]"))

(subtest "lqn condense, >< <> || $_"
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                     (>< #{(%@things
                             (>< #{(%@extra (?? sup _))}))})))
"[{\"things\":[{\"extra\":\"EXTRA99\"}]},{\"things\":[{\"extra\":\"EXTRA1\"},{\"extra\":\"EXTRA2\"}]}]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                     #{(%@things
                            (>< #{(%@extra (?? sup _))}))}))
"[{\"things\":[{\"extra\":\"EXTRA99\"}]},{\"things\":[{\"extra\":\"EXTRA1\"},{\"extra\":\"EXTRA2\"}]},null]")
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (|| (*cat #[things]) #[id] _)))
          "[0,10,11,12,31,32]")
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                (|| ($_ "a") ($_ "b"))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                (|| ($_ "a") ($_ "b"))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                (|| ($_ :a) ($_ :b))) 3)

  (is-str (lqn::jsnstr (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                                  (|| ($_ "a") {_ (b (+ 10 _))})))
          "{\"b\":13,\"c\":7}")
  (is-str (lqn::jsnstr (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                                  {a/b}))
          "{\"b\":3}")
  (is-str (lqn::jsnstr (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                                  ($_ :a/b)))
          "3"))

(subtest "lqn qry 3"
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 +)) 109)
  (is (lqn:lqnout
        (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*map ($new :v _ :n (cnt)))))
      #(((:V . 1) (:N . 0)) ((:V . 1) (:N . 1))
        ((:V . 7) (:N . 2)) ((:V . 100) (:N . 3))) :test #'equalp)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 (+ _))) 109)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 1000 +)) 1109)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 acc (- acc _))) -109)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 3 acc (- _ acc))) 96)
  (is (lqn:qry #(1 2 3 4 5 6 7 8 9 0) (head _ 7) (tail _ 3)) #(5 6 7) :test #'equalp)
  (is (lqn:qry #(1 2 3 4 5 6 7 8 9 0) (head _ -6) (tail _ -6)) #(1 2 3 4) :test #'equalp)
  (is (lqn:qry "abk c x dkef x kkkk1 x uu" (splt _ :x) (*? (isubx? _ :k) (*new _ (trim (par)))))
      #(#(2 "abk c") #(2 "dkef") #(1 "kkkk1")) :test #'equalp)
  (is (lqn:qry "abk c x dkef x kkkk1 x uu" (splt _ :x) trim (*? (isubx? _ :k) (*new _ (par))))
      #(#(2 "abk c") #(1 "dkef") #(0 "kkkk1")) :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) trim (*map (xpr? :a :-@b sup sdwn)))
      #("AAAYYY" "abc" "def" "uuu" "sss" "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) trim (txpr? :a :-@b sup))
      #("AAAYYY" "abc" "def" "uuu" "sss" "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) trim (*map (xpr? :a :-@b sup nil)))
      #("AAAYYY" NIL NIL NIL NIL "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) trim (*map (hld :k _) (xpr? :a :-@b (str! (sup _) (ghv :k)) sdwn)))
      #("AAAYYYaaayyy" "abc" "def" "uuu" "sss" "AUIUUauiuu" "AAAAAaaaaa") :test #'equalp)
  (is (lqn:qry '#((a b xxx) (a b c) (a b (c xxx)))
               (txpr? (+@msym? _ xxx) (lqn::symb _ :-HIT---)))
      #((A B XXX-HIT---) (A B C) (A B (C XXX-HIT---))) :test #'equalp)
  (is (lqn:qry '#((a bbbxxx xxx) (a b c) (a b (c xxx)))
               (txpr? (msym? _ "xxx") (lqn::symb _ :-HIT---)))
      #((A BBBXXX-HIT--- XXX-HIT---) (A B C) (A B (C XXX-HIT---))) :test #'equalp)
  (is (lqn:qry '#((a bbbxxx xxx) (a b c) (a b (c xxx)))
               (txpr? (-@msym? _ "bbb") (msym? _ "xxx") (lqn::symb _ :-HIT---)))
      #((A BBBXXX XXX-HIT---) (A B C) (A B (C XXX-HIT---))) :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                          (|| #{ things } (txpr? "Star" "noooooooo!!"))))
          "[{\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}]},{\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}]},{\"things\":[{\"id\":31,\"name\":\"noooooooo!!\"},{\"id\":32,\"name\":\"Ball\"}]}]")
  )

(unless (finalize) (error "error in lqn"))

