#!/usr/local/bin/sbcl --script

(let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file init) (load init)))

(ql:quickload :grph)
(ql:quickload :veq)
(in-package :lqn)
(load (internal-path-string "bin/cindex.lisp"))
(defvar *opt* '*opt*)
(defvar *dev* '*dev*)
(defvar *vv-a@* '*vv-a@*)
(defvar *vv-m@* '*vv-m@*)
(defvar *vv-f@* '*vv-f@*)
(defvar *vv-r@* '*vv-r@*)
(defvar *vv-x@* '*vv-x@*)
(defvar *vv-%@* '*vv-%@*)
(defvar *vv-!@* '*vv-!@*)
(defvar *vv-r@* '*vv-r@*)
(defvar *vv-_@* '*vv-_@*)
(defvar *vv-.@* '*vv-.@*)
(defvar *vv-r@* '*vv-r@*)
(defvar *vv-r@* '*vv-r@*)
(defvar *srndopt* '*srndopt*)

; todo handle #Paths in str!?
(defun main ()
  (let ((co (code))
        (path (internal-path-string "src/")) (ft ".lisp"))
    (print
      (qry (cat* (ls #P"/data/x/lqn/src/*.lisp")
                 (ls #P"/data/x/veq/src/*.lisp")
                 (ls #P"/data/x/grph/src/*.lisp")
                 (ls #P"/data/x/auxin/src/*.lisp")
                 (ls #P"/data/x/auxin/src/*/*.lisp"))
           str!
           #((code/index co _))

         ; [(suf? _ ft)] ;(tail* _ 1)
         ; #((strcat path _) (code/index co _))
         ))


    (print co)
    ; (code/write co "tmp")
    ))

(main)

; NOTE no forms can be the same. always create fresh

; >>>> (uniq: FILE)     :/is[filenum]            {:/ext/file}
; EX:  #((defun ..)..)  :/is[filenum]             :/ext/file

; >>>> (uniq: FILE)     :/file/has-name  -->     (ATOM: FILENAME)
; EX   #((defun ..)..)  :/file/has-name          "src.lisp"

; >>>> (uniq: FILE)     :/file/has-form[fnum]    (uniq: FORM)
; EX:  "src.lisp"       :/file/has-form[fnum]    (defun fx ...)

; >>>> (uniq: FORM)     :/form/has-name  -->     (ATOM: FORM-NAME)
; EX   (defun fx ..)    :/form/has-name          fx                | unknown

; >>>> (uniq: FORM)     :/is                     {:/ext/form}
; EX:  (defun fx..)     :/is                      :/ext/form


; >>>> (uniq: FORM)     :/form/has-atom          (ATOM)
; EX:  (defpackage ...) :/form/has-atom          abc


;                                                :/ft/set-macro-character        :/ft/defun
;                                                :/ft/defmacro   :/ft/defvar     :/ft/let
;                                                :/ft/labels     :/ft/in-package :/ft/declaim
;                                                :/ft/defpackage :/ft/unknown
; >>>> (uniq: FORM)     :/form/is-type   -->     {form-type}
; EX:  (defun fx ..)    :/form/is-type           :ft/defun

; >>>> (ATOM)           :/is                     {:/ext/atom}
; EX:  abc              :/is                      :/ext/atom


;                                                :/ty/string  :/ty/keyword :/ty/boolean :/ty/fixnum
;                                                :/ty/float   :/ty/number :/ty/cons    :/ty/character
;                                                :/ty/symbol  :/ty/comma :/ty/unknown
; >>>> (ATOM)           :/atom/is-type   -->     {atom-type}
; EX:  "ninja"          :/atom/is-type           :/ty/string



