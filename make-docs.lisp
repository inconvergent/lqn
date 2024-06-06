#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :lqn :silent t) (in-package :lqn)

(defun make-docs (&aux (*print-escape* nil))
  (with-open-file (fs (internal-path-string "docs/lqn.md")
                      :direction :output :if-exists :supersede)
    (format fs "# Lisp Query Notation Symbol Documentation (~a)~%~%" (v? t))
    (princ (with-output-to-string (*standard-output*)
             (ext-symbols? "LQN" :pretty)) fs)))
(make-docs)

