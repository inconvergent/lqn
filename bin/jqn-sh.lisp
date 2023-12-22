(ql:quickload :jqn :silent t)
(in-package :jqn)

(defvar *ex* "

  jqn [options] <qry> [files ...]

or:

  cat sample.json | jqn [options] <qry>

options:

  -m minified json. indented is default. ignored for -l
  -v prints the full compiled qry to stdout before the result
  -l output to readable lisp data (ldn)

options can be write as -i -v or -iv

  TODO: not implemented yet:
  -r read ldn. eg. use -rl to read en return ldn

examples:

  # get everything in the file:
  jqn _ sample.json

  # get key1, key2 from list of objects:
  jqn '#{key1 key2}' sample.json

  # get key1, key2 from object:
  jqn '{key1 key2}' sample.json

  # query data from pipe:
  echo '{\"_id\": 1}' | jqn '{_id}'")

(defun jqn-execute-query (dat q &key conf db)
  (handler-case (qryl dat :q q :conf conf :db db)
    (error (e) (exit-with-msg 4 "jqn: failed to execute qry:~%~a" e))))

(defun jsnloadf-with-err (f)
  (handler-case (jsnloadf f)
    (error (e) (exit-with-msg 2 "jqn: failed to read json file: ~a~%~a" f e))))

(defun jqn-parse-query (args)
  (handler-case (read-str args)
    (error (e) (exit-with-msg 3 "jqn: failed to parse qry:~%~a" (mkstr e)))))

; TODO: input file/pipe read ldn
(defun jqn-out (res &optional (indent t) (format :json))
  (case format
    (:json (handler-case (jsnout res :indent indent)
              (error (e) (exit-with-msg 5
                           "jqn: failed to serialize json:~%~a" (mkstr e)))))
    (:ldn (handler-case (format t "~&~a~&" (ldnout res t))
              (error (e) (exit-with-msg 5
                           "jqn: failed to serialize ldn:~%~a" (mkstr e)))))))

(defun jqn-run-files (opts q files)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "jqn: missing files.~%~a~&" *ex*))
  (loop for f in files
        for i from 0
        do (jqn-out (jqn-execute-query (jsnloadf-with-err f) (jqn-parse-query q)
                  :conf `((:fn . ,f) (:fi . ,i) (:ctx . :file))
                  :db (verbose? opts))
                (indent? opts)
                (format? opts))))

(defun jqn-run-pipe (opts q)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (jqn-out (jqn-execute-query (jsnloads *standard-input*) (jqn-parse-query q)
         :conf `((:ctx . :pipe))
         :db (verbose? opts))
       (indent? opts)
       (format? opts)))

(defun jqn-run-from-shell (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (jqn-run-files opts (car args) (cdr args)))
          (t (jqn-run-pipe opts (car args))))))

(jqn-run-from-shell (cdr (cmd-args)))

