#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)

(in-package :jqn)


(defun main (args)
  ; (let ((jsn (loadf "./bin/sample.json")))
  ;   (veq:vp :jsn (type-of jsn))
  ;   (loop for o in jsn do
  ;    (loop for k being the hash-keys in o using (hash-value v)
  ;         do (veq:vp k v (type-of k) (type-of v))
  ;     ))
  ;   ; (dumps jsn :indent t)
  ;   )

  (loop for i from 0
        for x in (jqnf "./bin/sample.json" :db t
                       :q (* :_id
                             (:things (* :name :id))))
        do (print i) (print x)))

(main (auxin:cmd-args))

