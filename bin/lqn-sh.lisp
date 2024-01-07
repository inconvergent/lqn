(ql:quickload :lqn :silent t)
(in-package :lqn)

(defvar *ex* (format nil "
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

" (lqn:v?)))

(defun lqn/execute-query (opts dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 50 "lqn: failed to execute qry:~%~a" e))))

(defun lqn/read-from-pipe ()
  (handler-case (read-stream-as-data-vector *standard-input*)
    (error (e) (exit-with-msg 30 "lqn: failed to read from pipe: ~a" e))))
(defun lqn/load-with-err (f)
  (handler-case (read-file-as-data-vector f)
    (error (e) (exit-with-msg 30 "lqn: failed to read txt file: ~a~%~a" f e))))

(defun lqn/parse-query (args)
  (handler-case `(|| ,@(read-all-str args))
    (error (e) (exit-with-msg 10 "lqn: failed to parse qry:~%~a" (mkstr e)))))

(defun lqn/run-files (opts q files)
  (when (help? opts) (exit-with-msg 0 *ex*))
  (unless q (exit-with-msg 1 "lqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "lqn: missing files.~%~a~&" *ex*))
  (loop for f in files for i from 0
        do (sh/out :ldn opts
             (lqn/execute-query opts (lqn/load-with-err f) (lqn/parse-query q)
               :conf `((:mode . :lqn) (:fn . ,f) (:fi . ,i) (:entry . :file))
               :db (verbose? opts)))))

(defun lqn/run-pipe (opts q)
  (when (help? opts) (exit-with-msg 0 *ex*))
  (unless q (exit-with-msg 1 "lqn: missing query.~%~a~&" *ex*))
  (labels ((one? (v) (if (> (length v) 1) v (aref v 0))))
    (sh/out :ldn opts
      (lqn/execute-query opts (one? (lqn/read-from-pipe)) (lqn/parse-query q)
        :conf `((:mode . :lqn) (:entry . :pipe))
        :db (verbose? opts)))))

(defun lqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (lqn/run-files opts (car args) (cdr args)))
          (t (lqn/run-pipe opts (car args))))))

(lqn/run-from-shell (cdr (cmd-args)))

