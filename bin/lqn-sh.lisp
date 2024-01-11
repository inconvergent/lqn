(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun lqn/read-from-file (f) (declare #.*opt*)
  (handler-case (read-file-as-data-vector f)
    (error (e) (exit-with-msg 55 "lqn: failed to READ LISP file: ~a~%msg: ~a" f e))))
(defun lqn/read-from-pipe () (declare #.*opt*)
  (handler-case (read-stream-as-data-vector *standard-input*)
    (error (e) (exit-with-msg 55 "lqn: failed to READ CODE from pipe:~%~a" e))))

(defun lqn/run-files (opts fx files)
  (declare (optimize speed) (function fx))
  (loop for fn in files for fi from 0 do
    (sh/out :ldn opts (sh/execute-qry fx (lqn/read-from-file fn) fn fi))))
(defun lqn/run-pipe (opts fx)
  (declare (optimize speed) (function fx))
  (sh/out :ldn opts (sh/execute-qry fx (sh/one? (lqn/read-from-pipe)) ":pipe:" 0)))

(sh/run-from-shell (format nil "
LQN - LISP QUERY NOTATION (~a)

Usage:
  lqn [options] <qry> [files ...]
  cat sample.csv | lqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -t output as TXT [default]
  -j output as JSON
  -l output to readable lisp data (LDN)
  -m minified json. indented is default. ignored for -l/-t
  -h show this message.

  options can be write as -i -v or -iv.

Examples:
  TODO: lqn  (msym? _ in-package) (*new (fn) _) ...
" (lqn:v?)) (cdr (cmd-args)) #'lqn/run-files #'lqn/run-pipe)

