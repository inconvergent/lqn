(ql:quickload :jqn :silent t)
(in-package :jqn)

(defvar *ex* "
QUERY AND TRANSFORM TXT

Usage:
  jqn [options] <qry> [files ...]
  cat sample.csv | jqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -t output as TXT [default]
  -j output as JSON
  -l output to readable lisp data (LDN)
  -m minified json. indented is default. ignored for -l/-t
  -h show this message.

  options can be write as -i -v or -iv.

Examples:

")

(defun jqn/execute-query (opts dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 4 "jqn: failed to execute qry:~%~a" e))))

(defun jqn/load-with-err (f)
  (handler-case (read-file-as-data-vector f)
    (error (e) (exit-with-msg 2 "jqn: failed to read txt file: ~a~%~a" f e))))

(defun jqn/parse-query (args)
  (handler-case `(|| ,@(read-all-str args))
    (error (e) (exit-with-msg 3 "jqn: failed to parse qry:~%~a" (mkstr e)))))

(defun jqn/run-files (opts q files)
  (when (help? opts) (exit-with-msg 0 *ex*))
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "jqn: missing files.~%~a~&" *ex*))
  (loop for f in files for i from 0
        do (sh/out :ldn opts
             (jqn/execute-query opts (jqn/load-with-err f) (jqn/parse-query q)
               :conf `((:mode . :lqn) (:fn . ,f)
                       (:fi . ,i) (:ctx . :file))
               :db (verbose? opts)))))

; (defun jqn/run-pipe (opts q)
;   (when (help? opts) (exit-with-msg 0 *ex*))
;   (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
;   (labels ((one-line (v) (if (> (length v) 1) v (aref v 0))))
;    (sh/out :txt opts
;     (jqn/execute-query opts (one-line (read-stream-lines-as-vector)) (jqn/parse-query q)
;       :conf `((:mode . :lqn) (:ctx . :pipe))
;       :db (verbose? opts)))))

(defun jqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (jqn/run-files opts (car args) (cdr args)))
          (t (error "bad") (jqn/run-pipe opts (car args)))
          )))

(jqn/run-from-shell (cdr (cmd-args)))

