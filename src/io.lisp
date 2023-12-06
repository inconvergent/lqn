(in-package #:jqn)

(defun read-str (s) (read-from-string s nil nil))

(defun jsnloads (&optional (s *standard-input*))
  "parse json from stream; or *standard-input*"
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (yason:parse s)))

(defun jsnloadf (fn)
  (declare (string fn)) "parse json from file, fn"
  (with-open-file (f fn :direction :input) (jsnloads f)))

(defun jsnout (o &key (s *standard-output*) indent)
  (declare (stream s) (boolean indent))
  "stream encoded json from o to s; or *standard-output*"
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*list-encoder* 'yason:encode-alist))
    (yason:encode o (yason:make-json-output-stream s :indent indent))))

(defun jsnout* (o &key indent (s (make-string-output-stream)))
  (declare (boolean indent)) "serialize o as json string"
  (jsnout o :s s :indent indent)
  (get-output-stream-string s))

(defun ldnout (o &optional (kvkeys t))
  "serialize internal json representation to readable lisp data."
  (etypecase o (string o)
               (hash-table (loop for k being the hash-keys of o using (hash-value v)
                                 collect `(,(kv k) . ,(ldnout v kvkeys))))
               (vector (loop with res = (make-adjustable-vector)
                             for v across o do (vextend (ldnout v kvkeys) res)
                             finally (return res)))
               (cons (loop for (k . v) in o
                           collect `(,(kv k) . ,(ldnout v kvkeys))))
               (atom o)))

