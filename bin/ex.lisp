#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")

(ql:quickload :auxin :silent t)
(ql:quickload :jqn :silent t)
; (in-package :jqn)

; TODO: expand expressions in pre compiler.

(defun main ()
  (print
   (jqn:jsnout
     (jqn:qryf "./sample3.json" :db t
        :q ($ credit qq))
        :indent t)))

(main)


; ██ COMPILED ██████████████████████████
; ██ q:   (*$ THINGS (+@THINGS (*$ ?@ID)))
; ██ ---
;    (LABELS ((JQN::FN ()
;               NIL)
;             (JQN::CTX ()
;               NIL))
;      (LOOP JQN::WITH #:IRES1 = (JQN::MAV)
;            JQN::FOR #:DAT3 JQN::ACROSS (JQN::ENSURE-VECTOR #:DAT*0)
;            JQN::FOR #:KRES2 = (JQN::NEW-HT)
;            DO (PROGN
;                (SETF (GETHASH "things" #:KRES2) (JQN:@ #:DAT3 "things"))
;                (SETF (GETHASH "things" #:KRES2)
;                        (LOOP JQN::WITH #:IRES4 = (JQN::MAV)
;                              JQN::FOR #:DAT6 JQN::ACROSS (JQN::ENSURE-VECTOR
;                                                           (JQN:@ #:DAT3
;                                                                  "things"))
;                              JQN::FOR #:KRES5 = (JQN::NEW-HT)
;                              DO (PROGN
;                                  (SETF (GETHASH "id" #:KRES5)
;                                          (JQN:@ #:DAT6 "id"))
;                                  (JQN::VEXTEND #:KRES5 #:IRES4))
;                              JQN::FINALLY (RETURN #:IRES4)))
;                (JQN::VEXTEND #:KRES2 #:IRES1))
;            JQN::FINALLY (RETURN #:IRES1)))

