(in-package #:lqn)
; YASON DOCS https://phmarek.github.io/yason/

(defun read-all-str (s &aux (n (length s)) (pos 0))
  (loop for (l new-pos) = (mvl (read-from-string s nil nil :start pos))
        while (and l (<= new-pos n)) do (setf pos new-pos) collect l))

(defun read-stream-lines-as-vector (&optional (s *standard-input*)
                                    &aux (res (make-adjustable-vector)))
  (loop for l = (read-line s nil nil) while l do (vex res l))
  res)
(defun read-file-as-vector (fn &aux (res (make-adjustable-vector)))
  (with-open-file (in fn)
    (loop for l = (read-line in nil nil) while l do (vex res l)))
  res)
; (setf (readtable-case *readtable*) :preserve)
(defun read-file-as-data-vector (fn &aux (res (make-adjustable-vector)))
  (with-open-file (in fn)
    (loop for l = (read in nil nil) while l do (vex res l)))
  res)

(defun jsnloads (&optional (s *standard-input*))
  "parse json from stream; or *standard-input*"
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (yason:parse s)))
(defun jsnloadf (fn)
  (declare (string fn)) "parse json from file, fn"
  (with-open-file (f fn :direction :input)
    (handler-case (jsnloads f) (end-of-file () (warn "empty file: ~a" fn)))))

(defun jsnout (o &key (s *standard-output*) indent)
  (declare (stream s) (boolean indent))
  "stream serialized json from o to s; or *standard-output*"
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*list-encoder* 'yason:encode-alist))
    (format s "~&")
    (yason:encode o (yason:make-json-output-stream s :indent indent))
    (format s "~&")
    (finish-output s)))
(defun jsnstr (o &key indent (s (make-string-output-stream)))
  (declare (boolean indent)) "serialize o as json to string"
  (jsnout o :s s :indent indent)
  (get-output-stream-string s))

(defun lqnout (o)
  "serialize internal representation to readable lisp data."
  (typecase o (string o)
     (cons (cons (lqnout (car o)) (lqnout (cdr o))))
     (hash-table (loop for k being the hash-keys of o using (hash-value v)
                       collect `(,(kv k) . ,(lqnout v))))
     (vector (loop with res = (make-adjustable-vector)
                   for v across o do (vex res (lqnout v))
                   finally (return res)))
     (otherwise o)))

