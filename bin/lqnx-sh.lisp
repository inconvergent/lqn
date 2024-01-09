(ql:quickload :lqn :silent t)
(in-package :lqn)

(defvar *ex* (format nil "
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
" (lqn:v?)))

(defun tqn/execute-query (opts dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 50 "xqn: failed to execute qry:~%~a" e))))

(defun tqn/parse-query (args)
  (handler-case `(|| ,@(read-all-str args))
    (error (e) (exit-with-msg 10 "xqn: failed to parse qry:~%~a" (mkstr e)))))

(defun tqn/runx (opts q &aux (conf `((:mode . :compile) (:entry . :xqn))))
  (when (help? opts) (exit-with-msg 0 *ex*))
  (unless q (exit-with-msg 1 "xqn: missing query.~%~a~&" *ex*))
  (cond ((compile? opts) 
           (qry/show q (proc-qry `((:dat . ,(gensym "DAT")) ,@conf) (tqn/parse-query q))))
        (t (sh/out :ldn opts (tqn/execute-query opts nil (tqn/parse-query q)
                                :conf conf :db (verbose? opts))))))

(defun xqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (tqn/runx opts (car args))))

(xqn/run-from-shell (cdr (cmd-args)))

