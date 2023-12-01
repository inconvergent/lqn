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

  (format t "~s~%"
    (progn ;json:encode-json-to-string
      (jqnf "./bin/sample.json"
            :db t
            :q
            (* :_id
              ("things"
                (* "id" ("name" 1))))
            )))
  )

(main (auxin:cmd-args))

