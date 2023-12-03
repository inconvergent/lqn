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
  ;   ; (wrtjsn jsn :indent t)
  ;   )

  (loop for i from 0
        for x across (qryf "./sample.json" :db t
                       :q (*  :+@_id
                             ("+@things" (* :name :id))
                             ("+@force" 333)
                             ))
        do (print i) (print x))
  ; (wrtjsn #(((3 . #(1 2 3))) 2 3))
  )

(main (auxin:cmd-args))

