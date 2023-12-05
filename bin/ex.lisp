#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
(in-package :jqn)

(defun main ()
  ; (loop for i from 0
  ;       for x across
  ;         (qryf "./sample.json" :db t
  ;               :q (*  _id
  ;                     (+@things (* name id))
  ;                     (+@force (print (@ :msg)))
  ;                     ))
  ;       do (print i) (print x))
  (print
    (qryf "./sample2.json" :db t
      :q _))

  )

(main)

