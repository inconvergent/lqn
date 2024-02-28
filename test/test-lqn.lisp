(in-package #:lqn-tests)

(plan 6)

(subtest "lqn qry preproc"
  (is (lqn::pre/$$ '(:ccc :ddd "IIUJ" "%@UU" :?@aa :?@bb ("cc" (progn _)) (:+ "ABC" (print _)) (:% "ABC" _) (:kkk "ABC" _)))
      '((:+ "ccc" :_) (:+ "ddd" :_) (:+ "IIUJ" :_) (:% "UU" :_) (:? "aa" :_) (:? "bb" :_)
        (:+ "cc" (PROGN _)) (:+ "ABC" (PRINT _)) (:% "ABC" :_) (:+ "kkk" "ABC")))
  (is (lqn::pre/** '(ccc :ddd "IIUJ" "%@UU" ?@aa ?@bb ("cc" (progn _)) (% "ABC" (print _)) (:% "ABC")))
     '((:? (WHEN (CCC :_) :_)) (:? (AND (LQN:STR? :_) (LQN:ISUB? :_ "ddd"))) (:? (AND (LQN:STR? :_) (LQN:SUB? :_ "IIUJ")))
       (:% (AND (LQN:STR? :_) (LQN:SUB? :_ "UU"))) (:? (WHEN (AA :_) :_)) (:? (WHEN (BB :_) :_)) (:? ("cc" (PROGN _))) (:? (% "ABC" (PRINT _)))
       (:% (AND (LQN:STR? :_) (LQN:SUB? :_ "ABC"))))))

(subtest "lqn qry identities"
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* ($* _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (*$ _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (** _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (?map _))))
  (is (lqn::jsnstr (lqn:jsnqryf *test-data-2-fn* _)) (lqn::jsnstr (lqn:jsnqryf *test-data-2-fn* ($$ _)))))

(subtest "top-level modifiers"
  (is (lqn:qry #("abc" "def") [(sub? _ :s@a)]) #("abc") :test #'equalp)
  (is (lqn:qry "abc x def x hij" nil) nil)
  (is (lqn:qry "aa" s@_) "aa")
  (is (lqn:qry 1 s@_) "1")
  (is (lqn:qry 1 (s@progn _)) "1")
  (is (lqn:qry "a" _@sup) "A")
  (is (lqn:qry "a" (progn _@sup)) "A")
  (is (lqn:qry 1 (progn (s@progn _))) "1")
  (is (lqn:qry "abc x def x hij" ∅) nil))

(subtest "env fxns"
  (is (lqn:ldnout
        (lqn:qry (lqn:jsnloads "[{\"a\": 1, \"b\": 2}, {\"a\": 11, \"b\": 12}]")
                 #{:a (:b _) (:cnt (cnt)) (:key (key)) (:par (par))}))
      #(((:A . 1) (:B . 2) (:CNT . 0) (:KEY . "key")
         (:PAR . #(((:A . 1) (:B . 2)) ((:A . 11) (:B . 12)))))
        ((:A . 11) (:B . 12) (:CNT . 1) (:KEY . "key")
         (:PAR . #(((:A . 1) (:B . 2)) ((:A . 11) (:B . 12)))))) :test #'equalp)
  (is (lqn:ldnout
        (lqn:qry (lqn:jsnloads "[{\"a\": 1, \"b\": 2}, {\"a\": 11, \"b\": 12}]")
                 #[:a (:b _) (:cnt (cnt)) (:key (key)) (:par (par))]))
        #(1 2 0 "key"
            #(((:A . 1) (:B . 2)) ((:A . 11) (:B . 12))) 11 12 1 "key"
            #(((:A . 1) (:B . 2)) ((:A . 11) (:B . 12)))) :test #'equalp)
  (is (lqn:ldnout
        (lqn:qry (lqn:jsnloads "{\"a\": 1, \"b\": 2}")
                 {:a (:b _) (:cnt (cnt)) (:key (key)) (:par (par))}))
       '((:A . 1) (:B . 2) (:CNT . 0) (:KEY . "key")
         (:PAR (:A . 1) (:B . 2))) :test #'equalp)
  (is (lqn:ldnout
         (lqn:qry (lqn:jsnloads "[{\"a\": 1, \"b\": 23}, {\"a\": 11, \"b\": 123}, {\"a\": 11, \"b\": 123} ]")
                     (?grp (@ :a) (str! (key) (@ :b)))))
      '((1 . #("123")) (11 . #("11123" "11123"))) :test #'equalp)
  (is (lqn:ldnout
         (lqn:qry (lqn:jsnloads "[{\"a\": 1, \"b\": 23}, {\"a\": 11, \"b\": 123}, {\"a\": 11, \"b\": 123} ]")
                     (?grp :a (str! (key) (@ :b)))))
      '((1 . #("123")) (11 . #("11123" "11123"))) :test #'equalp)
  (is (lqn:ldnout
         (lqn:qry (lqn:jsnloads "[{\"a\": 1, \"b\": 23}, {\"a\": 11, \"b\": 123}, {\"a\": 11, \"b\": 123} ]")
                     (?grp :a :b)))
      '((1 . #(23)) (11 . #(123 123))) :test #'equalp))

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
                              (?fld (list) acc (cons (1+ _) acc)) (reverse _))))
          "[1,11,12,13,32,33]"))

(unless (finalize) (error "error in test-lqn"))
