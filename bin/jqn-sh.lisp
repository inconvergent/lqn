(ql:quickload :jqn :silent t)

(in-package :jqn)

(defmacro exit-with-msg (i &rest rest)
  (declare (fixnum i))
  `(progn (format *error-output* ,@rest)
          (auxin:terminate ,i t)))

(defvar *ex* "examples:
  # get everything in the file
  jqn sample.json _
  # get key1, key2 from list of objects
  jqn sample.json '(* key1 key2)'
  # get key1, key2 from object
  jqn sample.json '(& key1 key2)'")

(defun parse-query (args)
  (handler-case (read-str args)
    (error (e) (exit-with-msg 3 "jqn: error when parsing qry:~%~a" (mkstr e)))))

(defun execute-query (args)
  (handler-case (qryl (second args) :q (parse-query (third args)))
    (error (e) (exit-with-msg 4 "jqn: err when executing qry:~%~a" (mkstr e)))))

(defun out (res)
  (handler-case (wrtjsn res :indent t)
    (error (e) (exit-with-msg 5 "jqn: err when printing result:~%~a" (mkstr e)))))

(defun main (args)
  (unless (< 1 (length args)) (exit-with-msg 1 "jqn: missing file name~%~a~&" *ex*))
  (unless (< 2 (length args)) (exit-with-msg 2 "jqn: missing query~%~a~&" *ex*))
  (out (execute-query args)))

(main (auxin:cmd-args))

