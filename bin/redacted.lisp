#!/usr/local/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(in-package :lqn)

(defun main ()
  (let ((chars (lst!? "abcdefghijklmnopqrstuvwxyz")))
    (qry (ls "../src/*.lisp") str!
         #((progn (out "~%~3,'0d: ** ~a **~%~%" (cnt) (seq* _ 8)) _)
           txt-read-file
           #((repl _ "defun" "DEFUN")
             (repl _ "defmacro" "DEFMACRO")
             #((if (member _ chars) #\― _))
             (out "~&~3,'0d: ~a~&" (cnt) (join _)))))))

(main)
