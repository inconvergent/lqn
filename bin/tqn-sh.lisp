(ql:quickload :jqn :silent t)
(in-package :jqn)

(defvar *ex* "

  tqn [options] <qry> [files ...]

or:
  cat sample.json | tqn [options] <qry>

options:
  -v prints the full compiled qry to stdout before the result
  -t output as TXT [default]
  -j output as JSON
  -l output to readable lisp data (ldn)
  -m minified json. indented is default. ignored for -l

options can be write as -i -v or -iv
")

(defun tqn/execute-query (dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 4 "tqn: failed to execute qry:~%~a" e))))

(defun tqn/load-with-err (f)
  (handler-case (read-file-as-vector f)
    (error (e) (exit-with-msg 2 "tqn: failed to read txt file: ~a~%~a" f e))))

(defun tqn/parse-query (args)
  (handler-case `(|| ,@(read-all-str args))
    (error (e) (exit-with-msg 3 "tqn: failed to parse qry:~%~a" (mkstr e)))))

(defun tqn/run-files (opts q files)
  (unless q (exit-with-msg 1 "tqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "tqn: missing files.~%~a~&" *ex*))
  (loop for f in files for i from 0
        do (sh/out :txt opts
             (tqn/execute-query (tqn/load-with-err f) (tqn/parse-query q)
               :conf `((:mode . :tqn) (:fn . ,f)
                       (:fi . ,i) (:ctx . :file))
               :db (verbose? opts)))))

(defun tqn/run-pipe (opts q)
  (unless q (exit-with-msg 1 "tqn: missing query.~%~a~&" *ex*))
  (sh/out :txt opts
    (tqn/execute-query (read-stream-lines-as-vector) (tqn/parse-query q)
      :conf `((:mode . :tqn) (:ctx . :pipe))
      :db (verbose? opts))))

(defun tqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (tqn/run-files opts (car args) (cdr args)))
          (t (tqn/run-pipe opts (car args))))))

(tqn/run-from-shell (cdr (cmd-args)))

