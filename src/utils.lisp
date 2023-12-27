(in-package :jqn)

; YASON DOCS https://phmarek.github.io/yason/

(defun ct/kv/key (s)
  (typecase s (string s) (symbol (sdwn (mkstr s))) (t `(mkstr ,s))))

(defmacro noop (&rest rest) (declare (ignore rest)) "do nothing. return nil" nil)
(defmacro ?? (fx arg &rest args) ; ?!
  (declare (symbol fx)) "run (fx arg) only if arg is not nil."
  (awg (arg*) `(let ((,arg* ,arg)) (when ,arg* (,fx ,arg* ,@args)))))

(defun gk (conf k &optional silent &aux (hit (cdr (assoc k conf))))
  (declare (list conf) (keyword k)) "get k from config"
  (if (or silent hit) hit (warn "JQN: missing conf key: ~a~%conf: ~s" k conf)))

(defun mkstr (&rest args) "coerce all arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun strcat (&rest rest) "concatenate all strings in sequences rest"
  (apply #'mkstr
    (mapcar (lambda (s) (etypecase s (string s)
                          (list (apply #'concatenate 'string s))
                          (vector (apply #'concatenate 'string (coerce s 'list)))))
            rest)))

(defun kv (s) "mkstr, upcase, keyword."
  (intern (sup (etypecase s (string s) (symbol (symbol-name s)) (number (mkstr s))))
          :keyword))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (apply #'mkstr args))))
(defun psymb (&optional (pkg 'jqn) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))

(defun flt? (f &optional d) "f if float; or d" (if (floatp f) f d))
(defun int? (i &optional d) "i if int; or d" (if (integerp i) i d))
(defun kv?  (k &optional d) "k if hash-table; or d" (if (hash-table-p k) d))
(defun lst? (l &optional d) "l if list; or d" (if (listp l) l d))
(defun num? (n &optional d) "n if number; or d" (if (numberp n) n d))
(defun str? (s &optional d) "s if string; or d" (if (stringp s) s d))
(defun vec? (v &optional d) "v if vector; or d" (if (vectorp v) v d))
(defun seq? (s &optional d) "s if sequence; or d" (or (lst? s) (str? s) (vec? s) d))

(defun int!? (i &optional d) "i as int if it can be parsed as int; or d"
  (handler-case (or (int? i) (int? (read-from-string i nil nil)) d) (error () d)))
(defun flt!? (f &optional d) "f as float if it can be parsed as float; or d"
  (handler-case (or (flt? f) (flt? (read-from-string f nil nil)) d) (error () d)))
(defun num!? (n &optional d) "n as number if it can be parsed as number; or d"
  (handler-case (or (num? n) (num? (read-from-string n nil nil)) d) (error () d)))

(defmacro out (s &rest rest) "print to standard out"
  (awg (s*) (if rest `(format *standard-output* ,s ,@rest)
                     `(let ((,s* ,s))
                        (when (and ,s* (or (not (stringp ,s*)) (> (length ,s*) 0)))
                          (format *standard-output* "~&~a~&" ,s*))))))
(defmacro fmt (s &rest rest) "format to string."
  (if rest `(format nil ,s ,@rest) `(format nil "~a" ,s)))

(defun pref? (s pref &aux (s (mkstr s)))
  (declare (string s pref)) "t if s starts with pref"
  (and (<= (length pref) (length s))
       (string= pref s :end2 (length pref))))
(defun ipref? (s suf) "case insensitive pref?"
  (pref? (sup s) (sup suf)))

(defun suf? (s suf)
  (declare (string s suf)) "t if s ends with suf"
  (pref? (reverse s) (reverse suf)))
(defun isuf? (s suf) "case insensitive suf?"
  (suf? (sup s) (sup suf)))

(defun sub? (s sub)
  (declare (optimize speed (safety 2)) (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from sub? i)))
(defun isub? (s sub) "case insensitive check is sub is substring of s."
  (sub? (sup s) (sup sub)))

(defun split (s x &key prune &aux (lx (length x))) ; todo split to vector
  (declare (optimize speed) (string x s) (boolean prune))
  "split string at substring. prune removes empty strings."
  (labels ((lst (s) (typecase s (list s) (t (list s))))
           (splt (s &aux (i (sub? s x)))
             (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))
(defun repl (s from to)
  (declare (string s to from)) "replace from with to in s"
  (let ((s (strcat (mapcar (lambda (s) (mkstr s to))
                           (split s from)))))
    (subseq s 0 (- (length s) (length to)))))

(defun make-adjustable-vector (&key init (type t) (size 128))
  (if init (make-array (length init) :fill-pointer t :initial-contents init
                                     :element-type type :adjustable t)
           (make-array size :fill-pointer 0 :element-type type :adjustable t)))

(defun ensure-vector (v) (declare (sequence v)) "list to vector; or vector"
  (etypecase v (vector v) (list (coerce v 'vector))))

(defun head (s &optional (n 10)) ; TODO: negative indices, tests
  (declare (sequence s) (fixnum n)) "first n elements"
  (subseq s 0 (min n (length s))))
(defun tail (s &optional (n 10) &aux (l (length s)))
  (declare (sequence s) (fixnum n l)) "last n elements"
  (subseq s (max 0 (- l n)) l))

(defun sup (&rest rest) "mkstr and upcase" (string-upcase (apply #'mkstr rest)))
(defun sdwn (&rest rest) "mkstr and downcase" (string-downcase (apply #'mkstr rest)))

(defun *seq (v i &optional j) (declare (vector v) (fixnum i)) "(subseq v ,@rest)" (subseq v i j))
(defun *ind (v &optional (i 0)) (declare (vector v) (fixnum i)) "get this index from vector." (aref v i))

(defun *sel (v &rest seqs)
  (declare (vector v))
  "new vector with indices or ranges from v.
ranges are lists that behave like arguments to *seq"
  (apply #'concatenate 'vector
    (loop for s in seqs collect (etypecase s (list (apply #'*seq v s))
                                             (fixnum (list (*ind v s)))))))

(defun $make (&optional kv &aux (res (make-hash-table :test #'equal))) "new/soft copy kv"
  (when kv (loop for k being the hash-keys of kv using (hash-value v)
                 do (setf (gethash k res) (gethash k kv))))
  res)
(defun $nil (kv) "return nil for emtpy hash-tables. otherwise return kv"
  (typecase kv (hash-table (if (> (hash-table-count kv) 0) kv nil))
               (otherwise kv)))

(defun $cat (&rest rest &aux (res (make-hash-table :test #'equal)))
  "add all keys from all hash tables in rest. left to right."
  (loop for kv of-type hash-table in rest
            do (loop for k being the hash-keys of ($make kv)
                     using (hash-value v)
                     do (setf (gethash k res) (gethash k kv))))
  res)
(defun *cat (&rest rest &aux (res (make-adjustable-vector)))
  "concatenate all vectors in these vectors.
non-vectors are included in their position"
  (labels ((do-arg (aa) (loop for a across aa
                              do (loop for b across a do (vex b res)))))
    (loop for a in rest do (typecase a (vector (do-arg a))
                                       (otherwise (vex a res)))))
  res)
(defun *$cat (&rest rest &aux (res (make-hash-table :test #'equal)))
  "for all vectors in rest; for all hts in these vectors; copy all keys into new kv. left to right"
  (loop for v of-type vector in rest
        do (loop for kv of-type hash-table across v
                 do (loop for k being the hash-keys of ($make kv)
                          using (hash-value v)
                          do (setf (gethash k res) v))))
  res)

(defun $rget (o pp d) "recursively get p from some/path/thing."
  (labels ((rec (o pp)
             (unless pp (return-from rec (or o d)))
             (typecase o (hash-table (rec (gethash (car pp) o) (cdr pp)))
                         (otherwise (return-from rec (or o d))))))
    (rec o (split pp "/"))))
(defmacro $ (o k &optional d) "get key k from o" `($rget ,o (ct/kv/key ,k) ,d))

