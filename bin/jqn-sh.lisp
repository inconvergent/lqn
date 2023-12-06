(ql:quickload :jqn :silent t)
(in-package :jqn)

(defvar *ex* "
examples:

  # get everything in the file:
  jqn _ sample.json

  # get key1, key2 from list of objects:
  jqn '(* key1 key2)' sample.json

  # get key1, key2 from object:
  jqn '(& key1 key2)' sample.json

  # query data from pipe:
  echo '{\"_id\": 1}' | jqn '(& _id)'")

(defmacro exit-with-msg (i &rest rest)
  (declare (fixnum i))
  `(progn (format *error-output* ,@rest)
          (auxin:terminate ,i t)))

(defun parse-query (args)
  (handler-case (read-str args)
    (error (e) (exit-with-msg 3 "jqn: error when parsing qry:~%~a" (mkstr e)))))

(defun execute-query (dat q)
  (handler-case (qryl dat :q q)
    (error (e) (exit-with-msg 4 "jqn: err when executing qry:~%~a" (mkstr e)))))

(defun out (res &optional (indent t))
  (handler-case (jsnout res :indent indent)
    (error (e) (exit-with-msg 5 "jqn: err when printing result:~%~a" (mkstr e)))))

(defun run-files (opts q files)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "jqn: missing files.~%~a~&" *ex*))
  (out (execute-query (jsnloadf (car files)) (parse-query q))))

(defun run-pipe (opts q)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (out (execute-query (jsnloads *standard-input*) (parse-query q))))

(defun run-from-cmd (args)
  (multiple-value-bind (opts args)
    (loop named opts for k in args for i from 0
          if (startswith? k "-") collect k into opts
          else do (return-from opts (values opts (subseq args i))))
    (cond ((interactive-stream-p *standard-input*)
           (run-files opts (car args) (cdr args)))
          (t (run-pipe opts (car args))))))

(defun main (args)
  (run-from-cmd (cdr args)))

(main (auxin:cmd-args))

