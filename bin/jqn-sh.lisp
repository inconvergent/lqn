(ql:quickload :lqn :silent t)
(in-package :lqn)

(defun jqn/read-from-file (f) (declare #.*opt*)
  (handler-case (jsnloadf f)
    (error (e) (exit-with-msg 55 "jqn: failed to READ JSON file: ~a~%msg: ~a" f e))))
(defun jqn/read-from-pipe () (declare #.*opt*)
  (handler-case (jsnloads *standard-input*)
    (error (e) (exit-with-msg 55 "jqn: failed to PARSE JSON from pipe:~%~a" e))))

(defun jqn/run-files (opts fx files)
  (declare (optimize speed) (function fx))
  (loop for fn in files for fi from 0 do
    (sh/out :json opts (sh/execute-qry fx (jqn/read-from-file fn) fn fi))))
(defun jqn/run-pipe (opts fx)
  (declare (optimize speed) (function fx))
  (sh/out :json opts (sh/execute-qry fx (jqn/read-from-pipe) ":pipe:" 0)))

(sh/run-from-shell (format nil "
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
" (lqn:v?)) (cdr (cmd-args)) #'jqn/run-files #'jqn/run-pipe)

