(in-package #:jqn)

(defun read-str (s) (read-from-string s nil nil))

(defun read-all-str (s &aux (l (length s)))
  (loop with pos = 0
        for (line new-pos) = (multiple-value-list
                               (read-from-string s nil nil :start pos))
        while (and line (<= new-pos l))
        do (setf pos new-pos)
        collect line))

(defun read-stream-lines-as-vector (&optional (s *standard-input*)
                                    &aux (res (make-adjustable-vector)))
  (loop for line = (read-line s nil nil)
        while line do (vextend line res))
  res)

(defun read-file-as-vector (fn &aux (res (make-adjustable-vector)))
  (with-open-file (in fn)
    (loop for line = (read-line in nil nil)
          while line
          do (vextend line res)))
  res)

(defun jsnloads (&optional (s *standard-input*))
  "parse json from stream; or *standard-input*"
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (yason:parse s)))

(defun jsnloadf (fn)
  (declare (string fn)) "parse json from file, fn"
  (with-open-file (f fn :direction :input)
    (handler-case (jsnloads f)
      (end-of-file (e) (declare (ignore e)) nil))))

(defun jsnout (o &key (s *standard-output*) indent)
  (declare (stream s) (boolean indent))
  "stream encoded json from o to s; or *standard-output*"
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*list-encoder* 'yason:encode-alist))
    (format s "~&")
    (yason:encode o (yason:make-json-output-stream s :indent indent))
    (format s "~&")
    (finish-output s)))

(defun jsnout* (o &key indent (s (make-string-output-stream)))
  (declare (boolean indent)) "serialize o as json string"
  (jsnout o :s s :indent indent)
  (get-output-stream-string s))

(defun ldnout (o &optional (kvkeys t))
  "serialize internal json representation to readable lisp data (ldn)."
  (etypecase o (string o)
               (hash-table (loop for k being the hash-keys of o using (hash-value v)
                                 collect `(,(kv k) . ,(ldnout v kvkeys))))
               (vector (loop with res = (make-adjustable-vector)
                             for v across o do (vextend (ldnout v kvkeys) res)
                             finally (return res)))
               (cons (loop for (k . v) in o
                           collect `(,(kv k) . ,(ldnout v kvkeys))))
               (atom o)))

