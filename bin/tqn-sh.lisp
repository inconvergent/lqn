(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun tqn/read-from-file (f) (declare #.*opt*)
  (handler-case (read-file-as-vector f)
    (error (e) (exit-with-msg 55 "TXT: failed to READ file: ~a~%~a" f e))))
(defun tqn/read-from-pipe () (declare #.*opt*)
  (handler-case (read-stream-lines-as-vector)
    (error (e) (exit-with-msg 55 "TXT: failed to READ from pipe:~%~a" e))))

(defun tqn/run-files (opts fx files)
  (declare (optimize speed) (function fx))
  (loop for fn in files for fi from 0 do
    (sh/out :txt opts (sh/execute-qry fx (tqn/read-from-file fn) fn fi))))
(defun tqn/run-pipe (opts fx)
  (declare (optimize speed) (function fx))
  (sh/out :txt opts (sh/execute-qry fx (sh/one? (tqn/read-from-pipe)) ":pipe:" 0)))

; (require :sb-sprof)
; (sb-sprof:with-profiling (:max-samples 50000 :mode :cpu #|:time|# :report :graph)
(sh/run-from-shell (format nil
"~%██ TQN - TXT - LISP QUERY NOTATION (~a)

Usage:
  tqn [options] <qry> [files ...]
  cat sample.csv | tqn [options] <qry>

Options:
  -v prints the full compiled qry to stdout before the result
  -j output as JSON
  -l output to readable lisp data (LDN)
  -t output as TXT [default]
  -m minified JSON. indented is default.
  -z preserve empty lines in TXT. [compct is default]
  -h show this message.

██ options can be write as -i -v or -iv.
██
██ when outputing in TXT, internal vectors or kvs are printed in LDN
██ mode. use -tj and -tl to output to JSON or LDN respectively. use -tjm
██ to print a resulting vector as (minified) lines of json.
██
██ see docs at: https://github.com/inconvergent/lqn

Examples:

  # split string and sum as integers:
  echo '1 x 1 x 7 x 100' | \
    tqn '(splt _ :x) int!? (*fld 0 +)'

  # split string and make a new JSON structure:
  echo '1 x 1 x 7 x 100' | \
     tqn -j '(splt _ :x) int!? #(($new :v _))'
" (lqn:v?)) (cdr (cmd-args)) #'tqn/run-files #'tqn/run-pipe)
; )
