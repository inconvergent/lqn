(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun jqn/read-from-file (f) (declare #.*opt*)
  (handler-case (jsnloadf f)
    (error (e) (sh/exit-msg 55 "JSON: failed to READ file: ~a~%~%~a~&" f e))))
(defun jqn/read-from-pipe (&optional all) (declare #.*opt*)
  (handler-case (jsnloads
                  *standard-input*
                  ; (read-all)
                  all)
    (end-of-file () nil)
    (error (e) (sh/exit-msg 55 "JSON: failed to PARSE from pipe:~%~%~a~&" e))))

; (defun read-all ()
;   (print
;    (join (loop with res = (mav)
;                for n = (read-from-string *standard-input* nil nil) while n do (vex res n)
;                 finally (return res)
;                ) #\Newline )))

(defun jqn/run-files (opts fx files) (declare #.*opt* (function fx))
  (loop for fn in files for fi from 0 do
    (sh/out :json opts (sh/execute-qry fx (jqn/read-from-file fn) fn fi))))
(defun jqn/run-pipe (opts fx) (declare #.*opt* (function fx))
  (loop for jsn = (jqn/read-from-pipe) for fi from 0 while jsn
        do (sh/out :json opts (sh/execute-qry fx jsn ":pipe:" fi))))

; (require :sb-sprof)
; (sb-sprof:with-profiling (:max-samples 50000 :mode :cpu #|:time|# :report :graph)
(sh/run-from-shell (format nil
"██ JQN - JSON - LISP QUERY NOTATION (~a)

Usage:
  jqn [options] <qry> [files ...]
  cat sample.json | jqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -j output as JSON [default]
  -l output to readable lisp data (LDN)
  -t output as TXT
  -m minified JSON. indented is default.
  -z preserve empty lines in TXT. [compct is default]
  -h show this message.

██ options can be write as -i -v or -iv.
██
██ when outputing in TXT, internal vectors or kvs are printed in LDN
██ mode. use -tj and -tl to output to JSON or LDN respectively.
██ use -tjm to print a resulting vector as (minified) lines of json.
██
██ see docs at: https://github.com/inconvergent/lqn

Examples:
  jqn _ sample.json                  # get everything in the file
  jqn '#{k1 k2}' sample.json         # get k1, k2 from list of objects
  jqn '{k1 k2}' sample.json          # get k1, k2 from object
  echo '{\"_id\": 1}' | jqn '{_id}'    # query data from pipe
" (lqn:v?)) (cdr (cmd-args)) #'jqn/run-files #'jqn/run-pipe)
; )

