(in-package :lqn)

(defun dat-read-files (path-or-seq)
  (declare ((or string list vector) path-or-seq))
  "read lisp code from these paths (via lqn:ls) or read this list of files as one large vector."
  (etypecase path-or-seq
    (string (qry (ls path-or-seq) #((dat-read-file _)) (flatn* _)))
    (sequence (qry path-or-seq #((dat-read-file _)) (flatn* _)))))
