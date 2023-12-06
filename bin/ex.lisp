#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
(in-package :jqn)

; (print (jqn::internal-path-string "test/sample.json"))

(defun main ()
  (loop for i from 0
        for x across
          (qryf "./sample.json" :db t
                :q (*  _id
                      (+@things (* name id))
                      (+@msg (string-downcase (@ :msg)))
                      ))
        do (print i) (print x))
  ; (print
  ;   (qryf "./sample2.json" :db t
  ;     :q _))
  )

(main)
