(in-package #:lqn)
; YASON DOCS https://phmarek.github.io/yason/
(defun read-all-str (s &aux (n (length s)) (pos 0))
  (declare #.*opt*)
  (loop for (l new-pos) = (mvl (read-from-string s nil nil :start pos))
        while (and l (<= new-pos n)) do (setf pos new-pos) collect l))


(defun read-stream-lines-as-vector (&optional (s *standard-input*)
                                    &aux (res (make-adjustable-vector)))
  (declare #.*opt*)
  (loop for l = (read-line s nil nil) while l do (vex res l))
  res)
(defun read-file-as-vector (fn &aux (res (make-adjustable-vector)))
  (declare #.*opt*)
  (with-open-file (in fn)
    (loop for l = (read-line in nil nil) while l do (vex res l)))
  res)
; (setf (readtable-case *readtable*) :preserve)
(defun read-file-as-data-vector (fn &aux (res (make-adjustable-vector)))
  (declare #.*opt*)
  (with-open-file (in fn)
    (loop for l = (read in nil nil) while l do (vex res l)))
  res)
(defun read-stream-as-data-vector (s &aux (res (make-adjustable-vector)))
  (declare #.*opt*)
  (loop for l = (read s nil nil) while l do (vex res l))
  res)

(defun jsnloads (&optional (s *standard-input*) all)
  (declare #.*opt* ) "parse json from stream; or *standard-input*"
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (if all (let ((res (mav)))
              (handler-case
                (loop for j = (yason:parse s) while j
                      do (vex res j) finally (return res))
                (end-of-file () res)))
            (yason:parse s))))
(defun jsnloadf (fn)
  (declare #.*opt* (string fn)) "parse json from file, fn"
  (with-open-file (f fn :direction :input)
    (handler-case (jsnloads f) (end-of-file () (warn "empty file: ~a" fn)))))

(defun jsnout (o &key (s *standard-output*) indent)
  (declare #.*opt* (stream s) (boolean indent))
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

(defun ldnout (o)
  "serialize internal representation to readable lisp data."
  (typecase o (string o)
     (cons (cons (ldnout (car o)) (ldnout (cdr o))))
     (hash-table (loop for k being the hash-keys of o using (hash-value v)
                       collect `(,(kv k) . ,(ldnout v))))
     (vector (loop with res = (make-adjustable-vector)
                   for v across o do (vex res (ldnout v))
                   finally (return res)))
     (otherwise o)))

(defun nstr (n &optional (c #\Space)) "str of length n, filled with c"
  (make-string n :initial-element c))
(defmacro out (s &rest rest) "print to standard out"
  (awg (s*) (if rest `(format *standard-output* ,s ,@rest)
                     `(let ((,s* ,s))
                        (when (and ,s* (or (not (stringp ,s*)) (> (length ,s*) 0)))
                          (format *standard-output* "~&~a~&" ,s*))))))
(defmacro fmt (s &rest rest) "format to string."
  (if rest `(format nil ,s ,@rest) `(format nil "~a" ,s)))
(defun lpad (s n &optional (c #\Space))
  (declare (string s) (fixnum n)) "left pad to length n. always of length n."
  (let ((d (- n (length s))))
    (cond ((< d 0) (subseq s (abs d))) ((> d 0) (strcat (nstr d c) s)) (t s))))
(defun rpad (s n &optional (c #\Space) &aux (l (length s)))
  (declare (string s) (fixnum n)) "right pad to length n. always of length n."
  (let ((d (- n l)))
    (cond ((< d 0) (subseq s 0 (+ l d))) ((> d 0) (strcat s (nstr d c))) (t s))))

(defun bar (size i &optional (pad #\Space) (bbb " ▏▎▍▌▋▊▉█") &aux (l (length bbb)))
  (declare (fixnum size l) (float i))
  (labels ((pad? (res) (if pad (rpad res size pad) res)))
    (when (<= i 0.0) (return-from bar (pad? "")))
    (let* ((i* (* (max 0.0 (clmp i)) size))
           (full (floor i*))
           (part (mod (floor (* l (rem i* 1))) l))
           (res (strcat (nstr full (char bbb (1- l))) (char bbb part))))
      (pad? res))))

