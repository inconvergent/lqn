(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun xqn/run (opts fx)
  (declare (optimize speed) (function fx))
  (sh/out :txt opts (sh/execute-qry fx nil ":pipe:" 0)))

(sh/run-from-shell (format nil
"██ XQN - EXECUTE LQN QUERIES/CL CODE WITH NO INPUT (~a)

Usage:
  xqn [options] <qry>

Example:

  xqn '(print (+ 1 3 4))'
" (lqn:v?)) (cdr (cmd-args)) #'xqn/run #'xqn/run t)

