(ql:quickload :jqn :silent t)
(in-package :jqn)

(defvar *ex* "

  jqn [options] <qry> [files ...]

or:
  cat sample.json | jqn [options] <qry>

options:
  -v prints the full compiled qry to stdout before the result
  -j output as JSON [default]
  -l output to readable lisp data (ldn)
  -t output as TXT
  -m minified json. indented is default. ignored for -l

options can be write as -i -v or -iv

examples:

  # get everything in the file:
  jqn _ sample.json

  # get key1, key2 from list of objects:
  jqn '#{key1 key2}' sample.json

  # get key1, key2 from object:
  jqn '{key1 key2}' sample.json

  # query data from pipe:
  echo '{\"_id\": 1}' | jqn '{_id}'")

(defun jqn/execute-query (dat q &key conf db)
  (handler-case (qryl dat q :conf conf :db db)
    (error (e) (exit-with-msg 4 "jqn: failed to execute qry:~%~a" e))))

(defun jqn/loadf-with-err (f)
  (handler-case (jsnloadf f)
    (error (e) (exit-with-msg 2 "jqn: failed to read json file: ~a~%~a" f e))))

(defun jqn/parse-query (args)
  (handler-case (let ((all (read-all-str args)))
                  (if (= (length all) 1) (car all) `(|| ,@all)))
    (error (e) (exit-with-msg 3 "jqn: failed to parse qry:~%~a" (mkstr e)))))

(defun jqn/run-files (opts q files)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "jqn: missing files.~%~a~&" *ex*))
  (loop for f in files for i from 0
        do (sh/out :json opts
             (jqn/execute-query (jqn/loadf-with-err f) (jqn/parse-query q)
               :conf `((:mode . :jqn) (:fn . ,f)
                       (:fi . ,i) (:ctx . :file))
               :db (verbose? opts)))))

(defun jqn/run-pipe (opts q)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (sh/out :json opts
    (jqn/execute-query (jsnloads *standard-input*) (jqn/parse-query q)
      :conf `((:mode . :jqn) (:ctx . :pipe))
      :db (verbose? opts))))

(defun jqn/run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (jqn/run-files opts (car args) (cdr args)))
          (t (jqn/run-pipe opts (car args))))))

(jqn/run-from-shell (cdr (cmd-args)))

