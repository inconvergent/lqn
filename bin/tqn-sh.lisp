(ql:quickload :lqn :silent t)
(in-package :lqn)

(defvar *ex* "
TQN - TXT QUERY NOTATION

Usage:
  tqn [options] <qry> [files ...]
  cat sample.csv | tqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -t output as TXT [default]
  -j output as JSON
  -l output to readable lisp data (LDN)
  -m minified json. indented is default. ignored for -l/-t
  -h show this message.

  options can be write as -i -v or -iv.

Examples:

  # split string and sum as integers:
  echo '1 x 1 x 7 x 100' | \
    tqn '(split _ :x) int!? (*fld 0 +)'

  # split string and make a new JSON structure:
  echo '1 x 1 x 7 x 100' | \
     tqn -j '(split _ :x) int!? (*map ($new :v _))'
")

(defun tqn/execute-query (opts dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 4 "tqn: failed to execute qry:~%~a" e))))

(defun tqn/load-with-err (f)
  (handler-case (read-file-as-vector f)
    (error (e) (exit-with-msg 2 "tqn: failed to read txt file: ~a~%~a" f e))))
(defun tqn/read-from-pipe ()
  (handler-case (read-stream-lines-as-vector)
    (error (e) (exit-with-msg 2 "tqn: failed to read from pipe:~%~a" e))))

(defun tqn/parse-query (args)
  (handler-case `(|| ,@(read-all-str args))
    (error (e) (exit-with-msg 3 "tqn: failed to parse qry:~%~a" (mkstr e)))))

(defun tqn/run-files (opts q files)
  (when (help? opts) (exit-with-msg 0 *ex*))
  (unless q (exit-with-msg 1 "tqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "tqn: missing files.~%~a~&" *ex*))
  (loop for f in files for i from 0
        do (sh/out :txt opts
             (tqn/execute-query opts (tqn/load-with-err f) (tqn/parse-query q)
               :conf `((:mode . :tqn) (:fn . ,f)
                       (:fi . ,i) (:ctx . :file))
               :db (verbose? opts)))))

(defun tqn/run-pipe (opts q)
  (when (help? opts) (exit-with-msg 0 *ex*))
  (unless q (exit-with-msg 1 "tqn: missing query.~%~a~&" *ex*))
  (labels ((one-line (v) (if (> (length v) 1) v (aref v 0))))
   (sh/out :txt opts
    (tqn/execute-query opts (one-line (tqn/read-from-pipe)) (tqn/parse-query q)
      :conf `((:mode . :tqn) (:ctx . :pipe))
      :db (verbose? opts)))))

(defun tqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (tqn/run-files opts (car args) (cdr args)))
          (t (tqn/run-pipe opts (car args))))))

(tqn/run-from-shell (cdr (cmd-args)))

