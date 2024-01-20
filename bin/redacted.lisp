#!/usr/local/bin/sbcl --script

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(in-package :lqn)

(defun main ()
  (let ((path (internal-path-string "src/")) (ft ".lisp")
        (chars (lst!? "abcdefghijklmnopqrstuvwxyz")))
    (qry (cmd :s@ls path) [(suf? _ ft)]
         #((strcat path _)
           (progn (out "~%~3,'0d: ** ~a **~%~%" (cnt) (seq* _ 8)) _)
           read-file-as-vector
           #((repl _ "defun" "DEFUN")
             (repl _ "defmacro" "DEFMACRO")
             #((if (member _ chars) #\― _))
             (out "~&~3,'0d: ~a~&" (cnt) (join _)))))))

(main)
