(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun xqn/run (opts fx &rest rest)
  (declare (optimize speed) (function fx))
  (sh/out :ldn opts (sh/execute-qry fx nil ":internal:" 0)))

(sh/run-from-shell (format nil "
XQN - EXECUTE LQN QUERIES (~a)

Usage:
  xqn [options] <qry>

Options:
  -c print compiled query, do nothing
  -v prints the full compiled qry to stdout before the result
  -l output to readable lisp data (LDN) [default]
  -j output as JSON
  -t output as TXT
  -m minified json. indented is default. ignored for -l/-t
  -h show this message.
" (lqn:v?)) (cdr (cmd-args)) #'xqn/run #'xqn/run)

