#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
; (in-package :jqn)

; TODO crash on explicit nil

; TODO: dat type in compiler

(defun main ()
  (print
   (jqn:jsnout
     (jqn:qryf "./sample.json"
        :q (* (things )))
          )))

(main)
