#!/usr/local/bin/sbcl --script

(let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file init) (load init)))

(ql:quickload :grph)
(in-package :lqn)
(load (internal-path-string "bin/cindex.lisp"))

(defun main ()
  (let ((co (code))
        (path (internal-path-string "src/")) (ft ".lisp"))
    (qry (cmd "ls" path) [(suf? _ ft)] ;(tail* _ 1)
         #((strcat path _) (code/index co _)))

    (mapc #'print
      (let ((?/fn (frag/fnd co :/f/filename)))
        (code/qry co :in ?/fn :select (?fn ?name ?t )
          :where (and (and (?/fn :file ?fn) (?fn :has ?o))
                      (and (?name :name ?o) (?t :obj ?o)))
          :collect (frag/mget co ?fn ?t ?name))))

    (print co)
    (code/write co "tmp")))

(main)

