#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
; (in-package :jqn)

; TODO crash on explicit nil

; TODO: dat type in compiler

; (*map  ; current dat is either each kv in array

;                  (*& ?@vv
;                      (+@vv (expr ...))
;                      (?key %@aref _ ) ; if dat is a vector
;                    )

;                  (*
;                    ?@vv get element vv from dat if dat is obj
;                    (%@aref _ 0) ; expression
;                    )

;                  (&
;                   selectors
;                   )

;                  ==> (* (& selectors))
;                  (&
;                   selectors
;                   )
;                  (*
;                   selectors

;                   )

;                  ; (key!@fx args) nope

;                  ; ?@kv  ;selectors
;                  ; (?@kv)
;                  ; (?@ "kv")
;                  ; (+@ "kv") kv or nil
;                  ; (%@ "kv" expr)
;                  ; (%@kv expr)
;                  ; %@kv .. makes no sense? behave as ?@?


;                    (kv ; just use & to mean kv?
;                       ?kk (subseq "Key" 0 3) ; slice ignore if empty
;                       +vv (subseq key 0 3) ; slice force
;                       !hh (subseq _ 0)   ; ind crash if empty
;                       ?vv _   ; select vv if present?
;                       xx (*map ... ) ; on _
;                    )
;                  (?%@aref 0)   ; ind
;                  (%@aref 0 3)   ; slice

;                  (%@kv _ _id) ; get id from dat

;                  (?@ _id) ; get k from dat?
;                  (?@ _id (tx _id )) ; get k from dat?
;                  (+@ _id) ; get k from dat?
;                  (+@ _id (string-upcase ))
;                  )

(defun main ()
  (print
   (jqn:jsnout
     (jqn:qryf "./sample.json" :db t
        :q (*$ ;_
              (?@_id :hi)
              ; (?@things ($ _))
              ))
          :indent t)))

(main)
