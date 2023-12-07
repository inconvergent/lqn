#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
; (in-package :jqn)

; TODO crash on explicit nil

; TODO: dat type in compiler

(defun main ()
  (print
   (jqn:ldnout
     (jqn:qryf "./sample.json"
               :db t
        :q (*  _id
                (+@things (* name id))
                   ; (+@msg (string-downcase (*@ :msg)))
                   ))
          ))
  ; (print
  ;   (qryf "./sample2.json" :db t
  ;     :q _))
  )

(main)
