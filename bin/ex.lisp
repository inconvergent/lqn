#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

; (ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)

(defmacro pretty-json (v) `(jqn:out (jqn::jsnout* ,v :indent t)))

(defun main ()

  (pretty-json
    (jqn:jsnqryf (jqn::internal-path-string "data/sample.json")
                 (|| #{_id (things #[name])})))

  (print (jqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 (+ _))))
  (print (jqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 +)))
  (print (jqn:qry "1 x 1 x 7 x 100" (splt _ :x) int!? (*fld 0 acc (- _ acc))))

  (pretty-json
    (jqn:qry "1 x 1 x 7 x 100"
            (splt _ :x) int!? ; split and parse as int
            ($new :num (num)  ; new nested dict
                  :items (*map ($new :v _ :i (cnt))))))
  ; { "num": 4,
  ;   "items": [ { "v": 1, "i": 0 }, { "v": 1, "i": 1 },
  ;              { "v": 7, "i": 2 }, { "v": 100, "i": 3 } ] }

  (pretty-json
    (jqn:qry #("1 x 1 x 7 x 100" "3 x 8 x 30")
            (*map (splt _ :x) int!? ; for each row, split and parse as int
                  ($new :num (num)  ; new nested dict for each row
                        :items (*map ($new :v _ :i (cnt)))))))
  ; [ { "num": 4,
  ;     "items": [ { "v": 1, "i": 0 }, { "v": 1, "i": 1 },
  ;                { "v": 7, "i": 2 }, { "v": 100, "i": 3 } ] },
  ;   { "num": 3,
  ;     "items": [ { "v": 3, "i": 0 }, { "v": 8, "i": 1 },
  ;                { "v": 30, "i": 2 } ] } ]

  (pretty-json
    (jqn:qry "1 x 1 x 7 x 100 $ 3 x 8 x 30"
            (splt _ :$)
            (*map (splt _ :x) int!? ; for each row, split and parse as int
                  ($new :num (num)  ; new nested dict for each row
                        :items (*map ($new :v _ :i (cnt)))))))
  )

(main)

