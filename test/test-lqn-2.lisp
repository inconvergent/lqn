(in-package #:lqn-tests)

(plan 3)

(subtest "lqn vector"
  (is (lqn:empty? (lqn:new* 1 2 3)) nil)
  (is (lqn:empty? (lqn:new*)) t)
  (is (lqn:empty? (lqn:new$)) t)
  (is (lqn:empty? (lqn:new$) :empty) t)
  (is (lqn:empty? 1 :empty) :empty)
  (is (lqn:some? (lqn:new*) :empty) :empty)
  (is (lqn:some? (lqn:new* nil nil) :empty) nil)
  (is (lqn:some? (lqn:new* nil nil t) :empty) t)

  (is (lqn:none? (lqn:new*) :empty) :empty)
  (is (lqn:none? (lqn:new*)) t)
  (is (lqn:none? (lqn:new* nil nil) :empty) t)
  (is (lqn:none? (lqn:new* nil nil t) :empty) nil)

  (is (lqn:all? (lqn:new*) :empty) :empty)
  (is (lqn:all? (lqn:new*)) nil)
  (is (lqn:all? (lqn:new* nil nil) :empty) nil)
  (is (lqn:all? (lqn:new* nil nil t) :empty) nil)
  (is (lqn:all? (lqn:new* t t t) :empty) t)

  (is (lqn:uniq (lqn:new* 5 1 1 1 2 3 3 4 5)) #(1 2 3 4 5) :test #'equalp))

(subtest "lqn qry @@/@"
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ "a") (@ "b"))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ "a") (@ "b"))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ :a) (@ :b))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ :a) (@ "b"))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ :a) (@ :b))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ :a) (@ _ :b nil))) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (|| (@ :a) (@ _ :x :c))) :c)

  (is (lqn:qryd (lqn:jsnloads "{\"c\": 4, \"a\": {\"b\": 3, \"c\": 7}}") (|| (@* _ nil "c"))) #(4) :test #'equalp)
  (is (lqn:qryd (lqn:jsnloads "{\"c\": 4, \"a\": {\"b\": 3, \"c\": 7}}") (|| (@* _ :miss "a/b" "c" "a/u"))) #(3 4 :miss) :test #'equalp)
  (is (lqn:qryd (lqn:jsnloads "{\"c\": 4, \"a\": {\"b\": 3, \"c\": 7}}") (|| (@* _ :miss :a/b :c :a/u))) #(3 4 :miss) :test #'equalp)
  (is (lqn:qryd (lqn:jsnloads "{\"c\": 4, \"a\": {\"b\": 3, \"c\": 7}}") (|| (@* _ nil "c/uuu"))) #(nil) :test #'equalp)

  (is-str (lqn::jsnstr (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}")
                                  (|| (@ "a") {_ (:b (+ 10 _))})))
          "{\"b\":13,\"c\":7}")
  (is-str (lqn::jsnstr (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") {:a/b})) "{\"b\":3}")
  (is-str (lqn::jsnstr (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") {"a/b"})) "{\"b\":3}")
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (@ :a/b/c :miss)) :miss)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (@ :a/b)) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (@ :a/b :miss)) 3)
  (is (lqn:qryd (lqn:jsnloads "{\"a\": {\"b\": 3, \"c\": 7}}") (@ "a/b")) 3)

  (is (lqn:qry (lqn:new* 1 2 3 4 5) (@)) 1)
  (is (lqn:qry (lqn:new* 1 2 3 4 5) (@ -1)) 5)
  (is (lqn:qry (lqn:new* 1 2 3 4 5) (@ -5)) 1)
  (is (lqn:qry (lqn:new* 1 2 3 4 5) (@ -100)) nil)
  (let ((nest (lqn:new$ :a (lqn:new* (lqn:new$ :c 77 :x (lqn:new* 1 2 3))
                                     (lqn:new$ :c 99 :x (lqn:new* 5 8)))
                        :b nil)))
    (is (lqn:qry nest (@ "a/0/c")) 77)
    (is (lqn:qry nest (@ :a/0/c)) 77)
    (is (lqn:qry nest (@ "a/1/c")) 99)
    (is (lqn:qry nest (@ "a/100/x")) nil)
    (is (lqn:qry nest (@ "a/100/2")) nil)
    (is (lqn:qry nest (@ "0/100/2")) nil)
    (is (lqn:qry nest (@ :a/*/c)) #(77 99) :test #'equalp)
    (is (lqn:qry nest (@ "a/*/x/0")) #(1 5) :test #'equalp)
    (is (lqn:qry nest (@ "a/*/x/*")) #(#(1 2 3) #(5 8)) :test #'equalp)))

(subtest "lqn xpr"
  (is (lqn:qry #((a b xxx) (a b c) (a b (c xxx)))
               (?txpr (+@msym? _ xxx) (lqn::symb _ :-HIT---)))
      #((A B XXX-HIT---) (A B C) (A B (C XXX-HIT---))) :test #'equalp)
  (is (lqn:qry #((a bbbxxx xxx) (a b c) (a b (c xxx)))
               (?txpr (msym? _ "xxx") (lqn::symb _ :-HIT---)))
      #((A BBBXXX-HIT--- XXX-HIT---) (A B C) (A B (C XXX-HIT---))) :test #'equalp)
  (is (lqn:qry #((a bbbxxx xxx) (a b c) (a b (c xxx)))
               (?txpr (-@msym? _ "bbb") (msym? _ "xxx") (lqn::symb _ :-HIT---)))
      #((A BBBXXX XXX-HIT---) (A B C) (A B (C XXX-HIT---))) :test #'equalp)
  (is (lqn:qry #((a bbbxxx :xxx) (a b c) (a b (c xxx)))
               (?txpr (msym? _ :xxx) :hit))
      #((A BBBXXX :HIT) (A B C) (A B (C XXX))) :test #'equalp)
  (is (lqn:qry #((a bbbxxx :xxx) (a b c) (a b (c xxx)))
               (?txpr (msym? _ (progn :xxx)) :hit))
      #((A BBBXXX :HIT) (A B C) (A B (C XXX))) :test #'equalp)
  (is (lqn:qry #((a bbbxxx xxx) (a b c) (a b (c xxx)))
               (?txpr (msym? _ (progn 'xxx)) :hit))
      #((A BBBXXX :HIT) (A B C) (A B (C :HIT))) :test #'equalp)

  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) (*map (?xpr :a :-@b sup sdwn)))
      #("AAAYYY" "abc" "def" "uuu" "sss" "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) (*map _ (?xpr :a :-@b sup sdwn)))
      #("AAAYYY" "abc" "def" "uuu" "sss" "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) (*map (?xpr "a" "-@b" sup sdwn)))
      #("AAAYYY" "abc" "def" "uuu" "sss" "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) (?txpr :a :-@b sup))
      #("AAAYYY" "abc" "def" "uuu" "sss" "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) (*map (?xpr :a :-@b sup nil)))
      #("AAAYYY" NIL NIL NIL NIL "AUIUU" "AAAAA") :test #'equalp)
  (is (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
        (splt _ :x) (*map (hld :k _) (?xpr :a :-@b (str! (sup _) (ghv :k)) sdwn)))
      #("AAAYYYaaayyy" "abc" "def" "uuu" "sss" "AUIUUauiuu" "AAAAAaaaaa") :test #'equalp)

  (is (lqn:qry "a b c x def x 27" (splt _ :x) :-@de) #("a b c" "27") :test #'equalp)
  (is (lqn:qry "a b c x def x 27" (splt _ :x) "-@de") #("a b c" "27") :test #'equalp)
  (is (lqn:qry "a b c x def x 27" (splt _ :x) [:-@de] ) #("a b c" "27") :test #'equalp)
  (is (lqn:qry "a b c x def x 27" (splt _ :x) [_ :-@de] ) #("a b c" "27") :test #'equalp)
  (is (lqn:qry "a b c x def x 27" (splt _ :x) [-@int!?]) #("a b c" "def") :test #'equalp)
  (is (lqn:qry "a b c x def x 27" (splt _ :x) [_ -@int!?]) #("a b c" "def") :test #'equalp)
  (is (lqn:qry "a b c x def x 27" (splt _ :x) [-@int!?]) #("a b c" "def") :test #'equalp)

  (is (lqn:qry "1 xx x 2 3" (splt _ :x t))     #("1" "" "" "2 3") :test #'equalp)
  (is (lqn:qry "1 xx x 2 3" (splt _ :x t t))   #("1" "2 3") :test #'equalp)
  (is (lqn:qry "1 xx x 2 3" (splt _ :x nil))   #("1 " "" " " " 2 3") :test #'equalp)
  (is (lqn:qry "1 xx x 2 3" (splt _ :x t nil)) #("1" "" "" "2 3") :test #'equalp)
  (is (lqn:qry "1 xx x 2 3" (splt _ :x nil t)) #("1 " " " " 2 3") :test #'equalp)

  (is (lqn:qry "aaakyyy x akbc x def x ukuu x sssssk x auiuu x aaaaa"
        (splt _ :x) [(hld :v (isubx? _ "k")) (-@isubx? _ "a") (%@new* _ (ghv :v))])
      #(#("ukuu" 1) #("sssssk" 5)) :test #'equalp)

  (is (lqn:qry " aayy x abc x def x uuu x sss x auu x aa "
               (splt _ :x) (?txpr :a :-@b sup) #((progn #(_))))
      #(#(#\A #\A #\Y #\Y) #(#\a #\b #\c) #(#\d #\e #\f) #(#\u #\u #\u)
        #(#\s #\s #\s) #(#\A #\U #\U) #(#\A #\A)) :test #'equalp)
  (is (lqn:qry " aayy x abc x def x uuu x sss x auu x aa "
               (splt _ :x) (?txpr :a :-@b sup) #(#(_)))
      #(#(#\A #\A #\Y #\Y) #(#\a #\b #\c) #(#\d #\e #\f) #(#\u #\u #\u)
        #(#\s #\s #\s) #(#\A #\U #\U) #(#\A #\A)) :test #'equalp)
  (is (lqn:qry #((a bbbxxx xxx) (a b c) (a b (c xxx "ss")))
               (?mxpr ((-@msym? _ "bbb") (msym? _ "xxx")
                       (sym! _ :-HIT---))
                      ("ss" sup)))
      #((A BBBXXX XXX-HIT---) (A B C) (A B (C XXX-HIT--- "SS"))) :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn*
                        (|| #{ :things } (?txpr "Star" "noooooooo!!"))))
          "[{\"things\":[{\"id\":0,\"name\":\"Chris\",\"extra\":\"extra99\"}]},{\"things\":[{\"id\":10,\"name\":\"Winters\",\"extra\":\"extra1\"},{\"id\":11,\"name\":\"Haii\",\"extra\":\"extra2\"},{\"id\":12,\"name\":\"Klein\"}]},{\"things\":[{\"id\":31,\"name\":\"noooooooo!!\"},{\"id\":32,\"name\":\"Ball\"}]}]")
  (is (lqn:ldnout (lqn:jsnqryf *test-data-fn*
                    (|| #[:things] (flatn* _) #((cat$ {:id} {:extra})))))
      #(((:ID . 0) (:EXTRA . "extra99")) ((:ID . 10) (:EXTRA . "extra1"))
        ((:ID . 11) (:EXTRA . "extra2")) ((:ID . 12) (:EXTRA))
        ((:ID . 31) (:EXTRA)) ((:ID . 32) (:EXTRA))) :test #'equalp)
  (is (lqn:ldnout (lqn:jsnqryf *test-data-fn*
                    (|| #[:things] (flatn* _) #((cat$ {:id} {:?@extra})))))
      #(((:ID . 0) (:EXTRA . "extra99")) ((:ID . 10) (:EXTRA . "extra1"))
        ((:ID . 11) (:EXTRA . "extra2")) ((:ID . 12)) ((:ID . 31)) ((:ID . 32))) :test #'equalp)
  (is-str (lqn::jsnstr (lqn:jsnqryf *test-data-fn* (|| (@@ _ "*/things/*/extra"))))
           "[[\"extra99\"],[\"extra1\",\"extra2\"]]")

  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 +)) 109)
  (is (lqn:ldnout (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*map (new$ :v _ :n (cnt)))))
      #(((:V . 1) (:N . 0)) ((:V . 1) (:N . 1)) ((:V . 7) (:N . 2)) ((:V . 100) (:N . 3))) :test #'equalp)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 (+ _))) 109)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 1000 +)) 1109)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 acc (- acc _))) -109)
  (is (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 3 acc (- _ acc))) 96)
  (is (lqn:qry #(1 2 3 4 5 6 7 8 9 0) (head _ 7) (tail _ 3)) #(5 6 7) :test #'equalp)
  (is (lqn:qry #(1 2 3 4 5 6 7 8 9 0) (head _ -6) (tail _ -6)) #(1 2 3 4) :test #'equalp)
  (is (lqn:qry "abk c x dkef x kkkk1 x uu" (splt _ :x)
               (*? (isubx? _ "k") (new* _ (trim (itr)))))
      #(#(2 "abk c") #(1 "dkef") #(0 "kkkk1")) :test #'equalp)
  (is (lqn:qry "abk c x dkef x kkkk1 x uu" (splt _ :x nil)
               (*? (isubx? _ "k") (new* _ (trim (itr)))))
      #(#(2 "abk c") #(2 "dkef") #(1 "kkkk1")) :test #'equalp)
  (is (lqn:qry "abk c x dkef x kkkk1 x uu" (splt _ :x) trim
               (*? (isubx? _ "k") (new* _ (itr))))
      #(#(2 "abk c") #(1 "dkef") #(0 "kkkk1")) :test #'equalp)

  (is (lqn:qry #(0 1) (?rec (< (@ -1) 10000) (cat* _ (apply* + (tail _ 2))))
                     #((str! "- " (cnt) ": " _)) (join _ " "))
       "- 0: 0 - 1: 1 - 2: 1 - 3: 2 - 4: 3 - 5: 5 - 6: 8 - 7: 13 - 8: 21 - 9: 34 - 10: 55 - 11: 89 - 12: 144 - 13: 233 - 14: 377 - 15: 610 - 16: 987 - 17: 1597 - 18: 2584 - 19: 4181 - 20: 6765 - 21: 10946")
  )

(unless (finalize) (error "error in test-lqn-2"))

