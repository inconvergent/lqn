(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun xqn/run (opts fx &rest rest)
  (declare (optimize speed) (function fx))
  (sh/out :ldn opts (sh/execute-qry fx nil ":internal:" 0)))

(sh/run-from-shell (format nil
"~%██ XQN - EXECUTE LQN QUERIES (~a)

Usage:
  xqn [options] <qry>
                               TODO
" (lqn:v?)) (cdr (cmd-args)) #'xqn/run #'xqn/run)

