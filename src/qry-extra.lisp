(in-package :lqn)

(defun dat-read-files (path-or-seq)
  (declare ((or string list vector) path-or-seq))
  "read lisp data from these paths (via lqn:ls) or this list of files as one large vector."
  (qry (etypecase path-or-seq (string (ls path-or-seq)) (sequence path-or-seq))
       #((dat-read-file _)) (flatn* _)))

