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

(defun verbose? (opts) (not (null (member :-v opts :test #'equal))))

(defun execute-query (dat q &key conf db)
  (handler-case (qryl dat :q q :conf conf :db db)
    (error (e) (exit-with-msg 4 "jqn: err when executing qry:~%~a" e))))

(defun out (res &optional (indent t))
  (handler-case (jsnout res :indent indent)
    (error (e) (exit-with-msg 5 "jqn: err when printing result:~%~a" (mkstr e)))))

(defun run-files (opts q files)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "jqn: missing files.~%~a~&" *ex*))
  (loop for f in files
        do (out (execute-query (jsnloadf f) (parse-query q)
                  :conf `((:fn . ,f) (:ctx . :file))
                  :db (verbose? opts)))))

(defun run-pipe (opts q)
  (unless q (exit-with-msg 1 "jqn: missing query.~%~a~&" *ex*))
  (out (execute-query (jsnloads *standard-input*) (parse-query q)
         :conf `((:ctx . :pipe))
         :db (verbose? opts))))

(defun split-opts-args (args)
  (labels ((explode-opts (opts)
                (loop for o in opts
                      if (= (length o) 2)
                      nconc `(,(kv o))
                      else nconc (loop for s across (subseq o 1)
                                       collect (kv (mkstr "-" s))))))
   (loop named opts for k in args for i from 0
        if (startswith? k "-") collect k into opts
        else do (return-from opts
                  (values (explode-opts opts) (subseq args i))))))

(defun run-from-cmd (args)
  (multiple-value-bind (opts args) (split-opts-args args)
    (cond ((interactive-stream-p *standard-input*)
           (run-files opts (car args) (cdr args)))
          (t (run-pipe opts (car args))))))

(defun main (args)
  (run-from-cmd (cdr args)))

(main (auxin:cmd-args))

