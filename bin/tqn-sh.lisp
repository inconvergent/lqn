(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun tqn/read-from-file (f) (declare #.*opt*)
  (handler-case (read-file-as-vector f)
    (error (e) (exit-with-msg 55 "tqn: failed to READ TXT file: ~a~%msg: ~a" f e))))
(defun tqn/read-from-pipe () (declare #.*opt*)
  (handler-case (read-stream-lines-as-vector)
    (error (e) (exit-with-msg 55 "tqn: failed to READ TXT from pipe:~%~a" e))))

(defun tqn/run-files (opts fx files)
  (declare (optimize speed) (function fx))
  (loop for fn in files for fi from 0 do
    (sh/out :txt opts (sh/execute-qry fx (tqn/read-from-file fn) fn fi))))
(defun tqn/run-pipe (opts fx)
  (declare (optimize speed) (function fx))
  (sh/out :txt opts (sh/execute-qry fx (sh/one? (tqn/read-from-pipe)) ":pipe:" 0)))

(sh/run-from-shell (format nil "
TQN - TXT QUERY NOTATION (~a)

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
    tqn '(splt _ :x) int!? (*fld 0 +)'

  # split string and make a new JSON structure:
  echo '1 x 1 x 7 x 100' | \
     tqn -j '(splt _ :x) int!? #(($new :v _))'
" (lqn:v?)) (cdr (cmd-args)) #'tqn/run-files #'tqn/run-pipe)

; (require :sb-sprof)
; (sb-sprof:with-profiling (:max-samples 50000 :mode :cpu :time :report :graph)
;   (tqn/run-from-shell (cdr (cmd-args))))
