#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :lqn :silent t)

(in-package :lqn)

(defun import-all (fn)
  (with-open-file (f (mkstr fn ".lisp") :direction :input)
    (loop for o = (read f nil)
          while o collect o)))
(defun internal-path (path) (namestring (asdf:system-relative-pathname :lqn path)))

(defun make-docs ()
  (loop for (o . rest) in (import-all (internal-path "src/packages"))
        if (eq o 'defpackage)
        do (let* ((pkg (mkstr (car rest)))
                  (fn (string-downcase
                        (internal-path
                          (mkstr "docs/" (repl pkg "/" "-") ".md")))))
             (format t "~&~a~%" fn)
             (with-open-file (f fn :direction :output :if-exists :supersede)
               (format f (with-output-to-string (*standard-output*)
                           (ext-symbols? pkg :pretty)))))))
(make-docs)

