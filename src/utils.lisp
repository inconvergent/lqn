(in-package #:jqn)

; https://phmarek.github.io/yason/#install

; (defvar *opt* '(optimize (safety 1) (speed 3) debug space))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t) &aux (v (slot-value (asdf:find-system 'jqn) 'asdf:version)))
  "return/print jqn version."
  (unless silent (format t "~&JQN version: ~a~%." v))
  v)

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args) `(,',long ,@args)))

(defun mapqt (l) (declare (list l)) "new list with quoted items." (mapcar (lambda (s) `(quote ,s)) l))
(defun mkstr (&rest args) "coerce this to string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun reread (&rest args) "mkstr then read from string." (values (read-from-string (apply #'mkstr args))))
(defun kv (s) (declare (symbol s)) "mkstr, upcase, keyword." (intern (string-upcase (symbol-name s)) :keyword))
(defun last* (l) (declare (list l)) "last item in list." (first (last l)))
(defun close-path (l) (declare (list l)) "cons last of to l." (cons (last* l) l))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (apply #'mkstr args))))
(defun psymb (&optional (pkg 'grph) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))

(defun strcat (s)
  (declare (optimize speed) (list s))
  (apply #'concatenate 'string s))

(defun repl (s from to)
  (declare (string s to from))
  "replace from with to in s"
  (let ((s (strcat (mapcar (lambda (s) (mkstr s to))
                                (split-substr from s)))))
    (subseq s 0 (1- (length s)))))

(defun split-substr (x s &key prune &aux (lx (length x)))
  (declare (optimize speed) (string x s) (boolean prune))
  (labels
    ((lst (s) (typecase s (list s) (t (list s))))
     (splt (s &aux (i (match-substr x s)))
       (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))

(defun match-substr (sub s)
  (declare (optimize speed (safety 2)) (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from match-substr i)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

(abbrev mvc multiple-value-call)
(abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind)
(abbrev awg with-gensyms)

(abbrev vextend vector-push-extend)

(defun make-adjustable-vector (&key init (type t) (size 128))
  (if init (make-array (length init)
             :fill-pointer t :initial-contents init
             :element-type type :adjustable t)
           (make-array size
             :fill-pointer 0 :element-type type :adjustable t)))

(defun split-string (x s &key prune)
  (declare (character x) (string s) (boolean prune))
  "split s at all instances of character x."
  (labels ((splt (s) (loop for c across s for i from 0
                           if (equal c x)
                           do (return-from splt
                                (cons (subseq s 0 i) (splt (subseq s (1+ i))))))))
    (let ((res (splt (concatenate 'string s (string x)))))
      (if prune (remove-if (lambda (s) (= 0 (length s))) res)
                res))))

; (defun get-file (filename)
;   (with-open-file (stream filename)
;     (loop for line = (read-line stream nil) while line collect line)))
(defun read-str (s) (read-from-string s nil nil))

