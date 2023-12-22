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
"


(defun out (res &optional (indent t) (format :json))
  (case format
    (:json (handler-case (jsnout res :indent indent)
              (error (e) (exit-with-msg 5
                           "tqn: failed to serialize json:~%~a" (mkstr e)))))
    (:ldn (handler-case (format t "~&~a~&" (ldnout res t))
              (error (e) (exit-with-msg 5
                           "tqn: failed to serialize ldn:~%~a" (mkstr e)))))))

(defun run-files (opts q files)
  (unless q (exit-with-msg 1 "tqn: missing query.~%~a~&" *ex*))
  (unless (< 0 (length files)) (exit-with-msg 2 "tqn: missing files.~%~a~&" *ex*))
  (loop for f in files
        for i from 0
        do (out (execute-query (jsnloadf-with-err f) (parse-query q)
                  :conf `((:fn . ,f) (:fi . ,i) (:ctx . :file))
                  :db (verbose? opts))
                (indent? opts)
                (format? opts))))

(defun run-pipe (opts q)
  (unless q (exit-with-msg 1 "tqn: missing query.~%~a~&" *ex*))
  (out (execute-query (jsnloads *standard-input*) (parse-query q)
         :conf `((:ctx . :pipe))
         :db (verbose? opts))
       (indent? opts)
       (format? opts)))

(defun split-opts-args (args)
  (labels ((do-explode (o)
             (loop for s across (subseq o 1) collect (kv (mkstr "-" s))))
           (explode-opts (opts)
             (loop for o in opts
                   if (= (length o) 2) nconc `(,(kv o))
                   else nconc (do-explode o))))
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

(main (cmd-args))

