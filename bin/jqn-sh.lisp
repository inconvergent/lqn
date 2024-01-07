(ql:quickload :lqn :silent t)
(in-package :lqn)

(defvar *ex* (format nil "
JQN - JSON QUERY NOTATION (~a)

Usage:
  jqn [options] <qry> [files ...]
  cat sample.json | jqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -j output as JSON [default]
  -l output to readable lisp data (LDN)
  -t output as TXT
  -m minified json. indented is default. ignored for -l/-t
  -h show this message.

  options can be write as -i -v or -iv.

Examples:
  jqn _ sample.json                  # get everything in the file
  jqn '#{k1 k2}' sample.json         # get k1, k2 from list of objects
  jqn '{k1 k2}' sample.json          # get k1, k2 from object
  echo '{\"_id\": 1}' | jqn '{_id}'    # query data from pipe
" (lqn:v?)))

(defun jqn/execute-query (opts dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 50 "jqn: failed to execute qry:~%~a" e))))

(defun jqn/parse-pipe-json ()
  (handler-case (jsnloads *standard-input*)
    (error (e) (exit-with-msg 30 "jqn: failed to parse json from pipe:~%~a" e))))
(defun jqn/loadf-with-err (f)
  (handler-case (jsnloadf f)
    (error (e) (exit-with-msg 30 "jqn: failed to read json file: ~a~%~a" f e))))

(defun jqn/parse-query (args)
  (handler-case `(|| ,@(read-all-str args))
    (error (e) (exit-with-msg 10 "jqn: failed to parse qry:~%~a" (mkstr e)))))

(defun jqn/run-files (opts q files)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "jqn: missing files.~%~a~&" *ex*))
  (loop for f in files for i from 0
        do (sh/out :json opts
             (jqn/execute-query opts (jqn/loadf-with-err f) (jqn/parse-query q)
               :conf `((:mode . :jqn) (:fn . ,f) (:fi . ,i) (:entry . :file))
               :db (verbose? opts)))))

(defun jqn/run-pipe (opts q)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (sh/out :json opts
    (jqn/execute-query opts (jqn/parse-pipe-json) (jqn/parse-query q)
      :conf `((:mode . :jqn) (:entry . :pipe))
      :db (verbose? opts))))

(defun jqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (when (help? opts) (exit-with-msg 0 *ex*))
    (cond ((interactive-stream-p *standard-input*)
           (jqn/run-files opts (car args) (cdr args)))
          (t (jqn/run-pipe opts (car args))))))

(jqn/run-from-shell (cdr (cmd-args)))

