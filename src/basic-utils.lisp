(in-package :lqn)

(declaim (inline symb mkstr subx? isubx?))

(defmacro noop (&rest rest) (declare (ignore rest)) "do nothing. return nil." nil)
(defmacro pretty-json (v) `(lqn:out (lqn:jsnstr ,v :indent t)))
(defmacro lit (a) `(progn ,a))
(defmacro qt (a) `(progn ',a))

(defun clmp (v &optional (a 0.0) (b 1.0))
  (declare (number v a b)) "clamp to range (a b)."
  (max a (min v b)))

(defun lpad-lst (n lst) "left pad list." (concatenate 'list (loop repeat (- n (length lst)) collect nil) lst))
(defun rpad-lst (n lst) "right pad list." (concatenate 'list lst (loop repeat (- n (length lst)) collect nil)))

(defun group (n l) (declare (list l) (fixnum n)) "group l into lists of n elements."
  (if (< n 1) (error "group: group size is smaller than 1"))
  (labels ((rec (l acc) (let ((rest (nthcdr n l)))
                          (if (consp rest) (rec rest (cons (subseq l 0 n) acc))
                                           (nreverse (cons l acc))))))
    (if l (rec l nil) nil)))
(defun mkstr (&rest args) "coerce all arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun kw (s) "mkstr, upcase, keyword."
  (intern (string-upcase (etypecase s (string s) (symbol (symbol-name s)) (number (mkstr s))))
          :keyword))
(defun ct/kw/str (a)
  (typecase a (string a) (keyword (string-downcase (mkstr a))) (otherwise a)))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (sup (apply #'mkstr args)))))
(defun psymb (&optional (pkg 'lqn) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (sup (apply #'mkstr args)) pkg)))
(defun lst (&rest rest) (apply #'list rest))

(defmacro car- (fx d) (declare (symbol d)) `(and (listp ,d) (,fx (car ,d))))
(defun sym-mode? (d &aux (mode-sym (unpack-mode d nil)))
  (if mode-sym (values-list (unpack-mode mode-sym d :?)) (values nil d)))
(defun qop? (s d &aux (d (and (listp d) (car d)))) (and d (ssym? d) (eq s (kw d))))
(defun dat?    (d) (and (symbolp d)    (eq (kw d) :_)))
(defun lqnfx?  (d) (and (ssym? d) (member (kw d) *fxns* :test #'eq)))
(defun custom-modifier? (m d)
  (and (symbolp d) (pref? (symbol-name d) m)
       (> (length (symbol-name d)) (length m))))

; IS TYPE?
(defun flt?  (f &optional d) "f if float; or d"       (if (floatp f) f d))
(defun int?  (i &optional d) "i if int; or d"         (if (integerp i) i d))
(defun kv?   (k &optional d) "k if kv; or d"          (if (hash-table-p k) k d))
(defun kw?   (k &optional d) "k if kw; or d"          (if (keywordp k) k d))
(defun sym?  (s &optional d) "s if sym; or d"         (if (symbolp s) s d))
(defun ssym? (s &optional d) "s if sym, not kw; or d" (if (and (sym? s) (not (kw? s))) s d))
(defun num?  (n &optional d) "n if number; or d"      (if (numberp n) n d))
(defun str?  (s &optional d) "s if string; or d"      (if (stringp s) s d))
(defun vec?  (v &optional d) "v if vector; or d"      (if (vectorp v) v d))
(defun lst?  (v &optional d) "v if list; or d"        (if (listp v) d d))
(defun seq?  (s &optional d) "s if sequence; or d"    (or (lst? s) (str? s) (vec? s) d))

; PARSE AS TYPE OR DEFAULT
(defun int!? (i &optional d) "i as int if it can be parsed; or d"
  (handler-case (or (int? i) (int? (read-from-string i nil nil)) d) (error () d)))
(defun flt!? (f &optional d) "f as float if it can be parsed; or d"
  (handler-case (or (flt? f) (flt? (read-from-string f nil nil)) d) (error () d)))
(defun num!? (n &optional d) "n as number if it can be parsed; or d"
  (handler-case (or (num? n) (num? (read-from-string n nil nil)) d) (error () d)))
(defun str!? (n &optional d) "s as str if it can be parsed; or d"
  (handler-case (or (str? n) (str? (read-from-string n nil nil)) d) (error () d)))
(defun vec!? (n &optional d) "v as vector if it can be parsed; or d"
  (handler-case (or (vec? n) (vec? (read-from-string n nil nil)) d) (error () d)))
(defun seq!? (n &optional d) "s as seq if it can be parsed; or d"
  (handler-case (or (seq? n) (seq? (read-from-string n nil nil)) d) (error () d)))
(defun lst!? (n &optional d) "v as list if it can be a list; or d"
  (labels ((cnv (a) (if (vec? a) (coerce a 'list) nil)))
    (handler-case (or (cnv n) (cnv (read-from-string n nil nil)) d) (error () d))))

; COERCE TO TYPE
(defun sym! (&rest rest) "stringify, make symbol" (apply #'symb rest))
(defun kw! (&rest rest) "stringify, make keyword" (apply #'psymb :keyword rest))
(defun str! (&rest rest) "coerce to string"
  (apply #'mkstr (loop for s in rest collect (typecase s (string s) (symbol (string-downcase s)) (t (mkstr s))))))
(defun vec! (v &optional (d `#(,v))) "coerce v to vector. if v is not a vector, list, string it returns d"
  (etypecase v (vector v) (list (coerce v 'vector)) (t d)))
(defun lst! (v &optional (d `(,v))) "coerce v to list if v; else d"
  (etypecase v (list v) (vector (coerce v 'list)) (t d)))

(defun size? (l &optional d) "length of sequence/number of keys in kv."
  (typecase l (sequence (length l)) (hash-table (hash-table-count l)) (otherwise d)))
(defun empty? (l &optional d &aux (n (size? l))) (if (int? n) (< n 1) d))
(defun uniq (s &optional (fx #'equal))
  (declare (function fx)) "remove duplicates from sequence"
  (remove-duplicates s :test fx))

; TODO: extend to check kvs?
; TODO: actually test this
(defun all? (v &optional empty) "check if all; or empty."
  (declare (sequence v))
  (if (not (empty? v)) (loop for k across (vec!? v) always (is? v)) empty))
(defun none? (v &optional (empty t)) "check if none; or empty."
  (declare (sequence v))
  (if (not (empty? v)) (loop for k across (vec!? v) never (is? k)) empty))
(defun some? (v &optional empty) "check if some; or empty."
  (declare (sequence v))
  (if (not (empty? v)) (not (none? v)) empty))

(defmacro smth? (v &body body) ; TODO: recursive strip with ext function
  (declare (symbol v)) "do body if v is not nil, empty sequence, or empty hash-table"
  (awg (v*)
  `(let ((,v* ,v))
     (typecase ,v* (sequence (when (> (length ,v) 0) (progn ,@body)))
                   (hash-table (when (> (hash-table-count ,v) 0) (progn ,@body)))
                   (otherwise (when ,v (progn ,@body)))))))
(defun is? (k &optional d) "k if k is not nil, empty sequence, or empty hash-table; or d"
  (if (smth? k t) k d))

(defun gk (conf k &optional silent &aux (hit (cdr (assoc k conf))))
  (declare (list conf) (keyword k)) "get k from config"
  (if (or silent hit) hit (warn "LQN: missing conf key: ~a~%conf: ~s" k conf)))

