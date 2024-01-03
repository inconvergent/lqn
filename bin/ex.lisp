#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

; (ql:quickload :auxin :silent t)
(ql:quickload :lqn :silent t)

(defmacro pretty-json (v) `(lqn:out (lqn:jsnstr ,v :indent t)))

(defun main ()

  (pretty-json (lqn:jsnqryf (lqn::internal-path-string "data/sample.json")
                            (|| #{_id (things #[name])})))

  (print (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 (+ _))))
  (print (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 +)))
  (print (lqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 acc (- _ acc))))

  (pretty-json (lqn:qry "1 x 1 x 7 x 100"
                        (splt _ :x) int!? ; split and parse as int
                        ($new :num (num)  ; new nested dict
                              :items #(($new :v _ :i (cnt))))))
  ; { "num": 4,
  ;   "items": [ { "v": 1, "i": 0 }, { "v": 1, "i": 1 },
  ;              { "v": 7, "i": 2 }, { "v": 100, "i": 3 } ] }

  (pretty-json (lqn:qry #("1 x 1 x 7 x 100" "3 x 8 x 30")
                        #((splt _ :x) int!? ; for each row, split and parse as int
                          ($new :num (num)  ; new nested dict for each row
                                :items #(($new :v _ :i (cnt)))))))
  ; [ { "num": 4,
  ;     "items": [ { "v": 1, "i": 0 }, { "v": 1, "i": 1 },
  ;                { "v": 7, "i": 2 }, { "v": 100, "i": 3 } ] },
  ;   { "num": 3,
  ;     "items": [ { "v": 3, "i": 0 }, { "v": 8, "i": 1 },
  ;                { "v": 30, "i": 2 } ] } ]

  (pretty-json (lqn:qry "1 x 1 x 7 x 100 $ 3 x 8 x 30"
                        (splt _ :$)
                        #((splt _ :x) int!? ; for each row, split and parse as int
                          ($new :num (num)  ; new nested dict for each row
                                :items #(($new :v _ :i (cnt)))))))
  ; same output as above

  ; (lqn:qry "abk c x dkef x kkkk1 x uu" (splt _ :x) trim (*? (isubx? _ :k) (*new _ (par))))

  (print (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
                  (splt _ :x) trim
                  #((?xpr :a :-@b sup nil))))


  (print (lqn:qry #((a b xxx) (a b c) (a b (c xxx)))
                  #((?txpr (msym? _ xxx)
                           (lqn:symb _ :-HIT---)))))
  ; #((A B XXX-HIT---) (A B C) (A B (C XXX-HIT---)))

  (print (lqn:qry #((a bbbxxx xxx) (a b c) (a b (c xxx)))
                  (?txpr (-@msym? _ "bbb") (+@msym? _ "xxx")
                         (sym! _ :-HIT---))))
  ; #((A BBBXXX XXX-HIT---) (A B C) (A B (C XXX-HIT---)))

  (print (lqn:qry "aaayyy x abc x def x uuu x sss x auiuu x aaaaa"
                  (splt _ :x) trim
                  (?txpr :a :-@b sup)))
   )

(main)

