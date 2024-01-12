(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun lqn/read-from-file (f) (declare #.*opt*)
  (handler-case (read-file-as-data-vector f)
    (error (e) (sh/exit-msg 55 "LDN: failed to READ file: ~a~%~%~a~&" f e))))
(defun lqn/read-from-pipe () (declare #.*opt*)
  (handler-case (read-stream-as-data-vector *standard-input*)
    (error (e) (sh/exit-msg 55 "LDN: failed to READ from pipe:~%~%~a~&" e))))

(defun lqn/run-files (opts fx files)
  (declare (optimize speed) (function fx))
  (loop for fn in files for fi from 0 do
    (sh/out :ldn opts (sh/execute-qry fx (lqn/read-from-file fn) fn fi))))
(defun lqn/run-pipe (opts fx)
  (declare (optimize speed) (function fx))
  (sh/out :ldn opts (sh/execute-qry fx (sh/one? (lqn/read-from-pipe)) ":pipe:" 0)))

(sh/run-from-shell (format nil
"██ LQN - LISP QUERY NOTATION (~a)

Usage:
  lqn [options] <qry> [files ...]
  cat sample.csv | lqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -j output as JSON
  -l output to readable lisp data (LDN) [default]
  -t output as TXT
  -m minified JSON. indented is default.
  -z preserve empty lines in TXT. [compct is default]
  -h show this message.

██ options can be write as -i -v or -iv.
██
██ when outputing in TXT, internal vectors or kvs are printed in LDN
██ mode. use -tj and -tl to output to JSON or LDN respectively.
██ use -tjm to print a resulting vector as (minified) lines of json.
██
██ see docs at: https://github.com/inconvergent/lqn

Examples:

  # find small items, insert symbol, flatten
  echo '#(1 2 3 4 5 6 7 8)' |\
    lqn '#((?txpr (< _ 3) (new* :xx _))) (flatall* _ t)'

  # or search for defmacro symbol in several source code files:
  lqn -tl '#((?srch (msym? _ defmacro)
                    (new$ :fn (fn) :hit (head* (itr) 3))))
          [is?] (flatall* _)' src/*lisp
" (lqn:v?)) (cdr (cmd-args)) #'lqn/run-files #'lqn/run-pipe)

