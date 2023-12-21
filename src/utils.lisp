(in-package #:jqn)

; YASON DOCS https://phmarek.github.io/yason/

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'jqn) 'asdf:version)))
  "return/print jqn version."
  (unless silent (format t "~&JQN version: ~a~%." v))
  v)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args) `(,',long ,@args)))
(abbrev awg with-gensyms)
(abbrev dsb destructuring-bind)
(abbrev mvb multiple-value-bind)
(abbrev mvc multiple-value-call)
(abbrev mav make-adjustable-vector)
(abbrev vextend vector-push-extend)

(defun internal-path-string (path &optional (pkg :jqn))
  (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun gk (conf k &optional silent &aux (hit (cdr (assoc k conf))))
  (declare (list conf) (keyword k)) "get k from config"
  (if (or silent hit) hit (warn "JQN: missing conf key: ~a~%conf: ~s" k conf)))

(defun mkstr (&rest args) "coerce all arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun strcat (&rest rest)
  (declare (sequence s)) "concatenate all strings in sequences rest"
  (apply #'mkstr
    (mapcar (lambda (s) (etypecase s (string s)
                          (list (apply #'concatenate 'string s))
                          (vector (apply #'concatenate 'string (coerce s 'list)))))
            rest)))

(defun kv (s) "mkstr, upcase, keyword."
  (intern (string-upcase (etypecase s (string s) (symbol (symbol-name s)) (number (mkstr s))))
          :keyword))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (apply #'mkstr args))))
(defun psymb (&optional (pkg 'jqn) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))

(defun tree-replace (tree from to &optional (comparefx #'equal))
  "compares tree to from (with comparefx); replaces matches with to."
  (cond ((funcall comparefx tree from) to)
        ((null tree) nil) ((atom tree) tree)
        (t (mapcar (lambda (x) (tree-replace x from to)) tree))))
(defun tree-replace-fx (tree fxmatch fxtransform)
  "compares elements with (comparefx); repaces matches with (fxmatch hit)."
  (cond ((funcall fxmatch tree) (funcall fxtransform tree))
        ((null tree) nil) ((atom tree) tree)
        (t (mapcar (lambda (x) (tree-replace-fx x fxmatch fxtransform))
                   tree))))

(defun split-substr (s sub &key prune &aux (lx (length sub)))
  (declare (optimize speed) (string sub s) (boolean prune))
  "split string at substring. prune removes empty strings."
  (labels
    ((lst (s) (typecase s (list s) (t (list s))))
     (splt (s &aux (i (match-substr s sub)))
       (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))
(defun match-substr (s sub)
  (declare (optimize speed (safety 2)) (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from match-substr i)))
(defun repl (s from to)
  (declare (string s to from)) "replace from with to in string s"
  (let ((s (strcat (mapcar (lambda (s) (mkstr s to))
                           (split-substr s from)))))
    (subseq s 0 (1- (length s)))))

(defun make-adjustable-vector (&key init (type t) (size 128))
  (if init (make-array (length init) :fill-pointer t :initial-contents init
                                     :element-type type :adjustable t)
           (make-array size :fill-pointer 0 :element-type type :adjustable t)))

(defun startswith? (s pref &aux (s (mkstr s)))
  (declare (string s pref)) "t if s starts with pref"
  (and (<= (length pref) (length s))
       (string= pref s :end2 (length pref))))

(defun ensure-vector (v) (declare (sequence v)) "list to vector; or vector"
  (etypecase v (vector v) (list (coerce v 'vector))))
(defun ensure-key (s) "symbol to lowercase string; or string"
  (etypecase s (symbol (string-downcase (mkstr s))) (string s)))

(defun $itr? (s)  (and (symbolp s) (eq (kv s) :$$)))
(defun *$itr? (s) (and (symbolp s) (eq (kv s) :*$)))
(defun *itr? (s)  (and (symbolp s) (eq (kv s) :**)))
(defun all? (s)   (and (symbolp s) (eq (kv s) :_)))
(defun pipe? (s)  (and (symbolp s) (eq (kv s) :||)))

(defun $new? (s) (and (symbolp s) (eq (kv s) :$new)))
(defun *new? (s) (and (symbolp s) (eq (kv s) :*new)))

(defun car-$itr? (d)  (and (listp d) ($itr? (car d))))
(defun car-*$itr? (d) (and (listp d) (*$itr? (car d))))
(defun car-*itr? (d)  (and (listp d) (*itr? (car d))))
(defun car-all? (s)   (and (listp s) (all? (car s))))
(defun car-pipe? (s)  (and (listp s) (pipe? (car s))))

; convert known jqn functions to a symbol in jqn pkg
(defun car-jqnfx? (s)
  (and (listp s) (symbolp (car s)) (member (kv (car s)) *fxns*)))

(defun car-*new? (d) (and (listp d) (*new? (car d))))
(defun car-$new? (d) (and (listp d) ($new? (car d))))

(defun jqn/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---
   ~s
██ ██████████████████████████~%" q compiled))

(defun unpack-mode (sym &optional (modes *qmodes*) (default :+))
  (loop for mode in modes
        for ind = (match-substr (mkstr sym) (mkstr mode))
        if (and ind (= ind 0))
        do (return-from unpack-mode
              (list (kv (subseq (mkstr mode) 0 1))
                (typecase sym (string (subseq sym 2))
                              (symbol (kv (subseq (mkstr sym) 2)))))))
  (list default sym))

