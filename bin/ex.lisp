#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
; (in-package :jqn)

   ; (jqn:jsnout (jqn:jsnqryf "./sample.json" :db t (|| #{_ _id}) :db t) :indent t)

(defun main ()
  (print
    ; (jqn::preproc/$$itr
    ;     '(ccc :ddd "IIUJ" "%@UU" ?@aa ?@bb ("cc" (progn _))
    ;       (:% "xBC" (print _)) (:% "ABC" _)))
    ; (mapcar #'print
    ;  (jqn::preproc/**filter
    ;     '(ccc :ddd "IIUJ" "%@UU" ?@aa ?@bb ("cc" (progn _))
    ;       (% "ABC" (print _)) (:% "ABC" _))))

   (jqn:jsnout (jqn:jsnqryf "./sample.json" (|| (*map _)
                                              ; #{(:?@_id (print _))}
                                              ; #{(:? _id (print _))}
                                              ; #{(?@_id print _)}
                                              )
                            :db t
                            ) :indent t)

   )
  )

(main)

