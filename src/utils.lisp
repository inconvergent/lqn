(in-package :lqn)

(defun sup (&rest rest) "mkstr and upcase" (string-upcase (apply #'mkstr rest)))
(defun sdwn (&rest rest) "mkstr and downcase" (string-downcase (apply #'mkstr rest)))
(defun trim (s &optional default (chars '(#\Space #\Newline #\Backspace #\Tab
                                          #\Linefeed #\Page #\Return #\Rubout)))
  "trim string"
  (typecase s (string (string-trim chars s)) (otherwise (or s default))))

(defun subx? (s sub)
  (declare #.*opt* (string s sub))
  "returns index where substring matches s from left to right. otherwise nil"
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from subx? i)))

(defun isubx? (s sub) "ignore case subx?"
  (declare (string s sub)) (subx? (sup s) (sup sub)))
(defun sub? (s sub &optional d) "s if sub is substring of s; or d"
  (declare (string s sub)) (if (subx? s sub) s d))
(defun isub? (s sub &optional d) "ignore case sub?"
  (declare (string s sub)) (if (isubx? s sub) s d))


(defun unpack-mode (o &optional (default :+) merciful)
  (labels ((valid-mode (m) (member m *qmodes* :test #'eq))
           (repack- (s s*) (etypecase s (symbol (psymb (symbol-package s) (subseq s* 2)))
                                        (string (subseq s* 2))))
           (unpack-cons (cns) (if (valid-mode (car cns)) cns
                                  (dsb (m s) (unpack- (car cns)) `(,m (,s ,@(cdr cns))))))
           (unpack- (s &aux (s* (mkstr s)) (sx (subx? s* "@")))
             (if (and sx (= sx 1)) (let ((m (kv (subseq s* 0 1)))) ; nil -> :nil
                                     (if (or merciful (valid-mode m)) (list m (repack- s s*))
                                       (error "lqn: invalid mode in: ~a" s)))
                                   (list default s))))
    (typecase o (symbol (unpack- o)) (string (unpack- o))
                (cons (unpack-cons o)) (vector `(,default ,o))
      (otherwise (error "lqn: bad mode thing to have mode: ~a" o)))))

(defmacro msym? (a b &optional d)
  "compare symbol a to b. if b is a keword or symbol
a perfect match is required. if b is a string it performs a substring
match. If b is an expression, a is compared to the evaluated value of b."
  (awg (a* res)
  `(let* ((,a* ,a)
          (,res (and (symbolp ,a*)
                     ,(etypecase b (keyword `(eq ,a* ,b)) (symbol `(eq ,a* ',b)) ; direct match
                                   (string `(isub? (mkstr ,a*) ,b))
                                   (cons `(eq ,a* ,b))))))
     (if ,res ,a* ,d))))

(defun make$ (&optional kv
   &aux (res (make-hash-table :test (if kv (hash-table-test kv) #'equal))))
  "new/soft copy kv."
  (when kv (loop for k being the hash-keys of kv using (hash-value v)
             do (setf (gethash k res) (gethash k kv))))
  res)
(defun $nil (kv) "return nil for emtpy hash-tables. otherwise return kv." ; TODO: use kv?
  (typecase kv (hash-table (if (> (hash-table-count kv) 0) kv nil))
               (otherwise kv)))
(defmacro new* (&rest d) "new vector with these elements" `(vector ,@d))
(defmacro new$ (&rest d) "new kv/hash-table from these (k v) pairs"
  (awg (kv) `(let ((,kv (make$)))
               ,@(loop for (kk expr) in (group 2 d)
                       collect `(setf (gethash ,(ct/kv/str kk) ,kv) ,expr))
               ,kv)))

(defmacro ?? (a expr &optional res) (declare (symbol a)) ; todo: dont require sym?
  "evaluate expr only iff a is not nil. returns the result of expr or res; or nil."
  `(and ,a ,expr ,@(if res `(,res))))

(defun @@ (a path &optional d) (declare #.*opt*)
  "get nested key (e.g. aa/2/bb) from nested structure of kv/vec"
  (labels ((gkv (a* k) (and (kv? a*) (gethash k a*)))
           (ind (a* k) (if (< k 0) (+ (length a*) k) k))
           (gv (a* k) (when (vec? a*)
                        (let ((kk (ind a* k)))
                          (when (< -1 kk (length a*)) (aref a* kk)))))
           (good-key (k) (or (int? k) (and (str? k) (> (length k) 0))))
           (not-empty (a*) (remove-if-not #'good-key a*))
           (int-or-str (k) (cond ((int!? k)) (t k)))
           (pre (kk) (not-empty (mapcar #'int-or-str (str-split kk "/"))))
           (rec (a* kk) (unless kk (return-from rec a*))
             (let* ((k (pop kk))
                    (v (cond ((equal k "*")
                                (return-from rec
                                  (and (vec? a*) (compct (map 'vector (λ (b) (rec b kk)) a*)))))
                             ((str? k) (gkv a* k))
                             ((int? k) (gv a* k)))))
               (if (is? v) (rec v kk) (return-from rec d)))))
    (compct
      (rec a (etypecase path
             (string (pre path)) (keyword (pre (str! path))) (fixnum (list path)))))))
(defun compct (o) (declare #.*opt*)
  "remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`."
  (labels
    ((do-ht (o* &aux (ht (make$)))
       (loop for k being the hash-keys of o* using (hash-value v)
             for vv = (rec v) if (is? vv) do (setf (gethash k ht) vv))
       ht)
     (rec (o*) (typecase o* (string (if (empty? o*) nil o*)) (hash-table (do-ht o*))
                 (sequence (remove-if-not (λ (o*) (smth? o* t)) o*))
                 (otherwise o*))))
    (rec o)))

(defun @* (a d &rest rest &aux l) (declare #.*opt*)
  "pick these indices/keys from sequence/hash-table into new vector."
  (labels ((lt (l) (or (nth l a) d))
           (kv (k) (@@ a k d))
           (gt (i) (if (< i l) (aref a i) d)))
    (typecase a (vector (setf l (length a)) (map 'vector #'gt rest))
                (hash-table (map 'vector #'kv rest))
                (list (map 'vector #'lt rest)))))

(defun strcat (&rest rest) "concatenate all strings in sequences"
  (apply #'mkstr
    (mapcar (λ (s) (etypecase s (string s)
                     (list (apply #'concatenate 'string s))
                     (vector (apply #'concatenate 'string (coerce s 'list)))))
            rest)))

(defun pref? (s pref &optional d)
  (declare #.*opt* (string s pref))
  "s if s starts with pref; or d"
  (if (and (<= (length pref) (length s))
           (string= pref s :end2 (length pref)))
       s d))
(defun ipref? (s suf &optional d)
  (declare #.*opt* (string s suf)) "ignore case pref?"
  (pref? (sup s) (sup suf) d))

(defun suf? (s suf &optional d)
  (declare #.*opt* (string s suf)) "s if s ends with suf; or d"
  (if (pref? (reverse s) (reverse suf)) s d))
(defun isuf? (s suf &optional d)
  (declare #.*opt* (string s suf)) "ignore case suf?"
  (if (pref? (sup (reverse s)) (sup (reverse suf))) s d))

(defmacro join (v &rest s) "join sequence v with s into new string."
  (awg (o n i s* v*)
    `(let* ((,v* ,v) (,n (1- (length ,v))) (,s* ,(if s `(str! ,@s) "")))
       (with-output-to-string (*standard-output*)
         (loop for ,o across ,v* for ,i from 0
           do (format t "~a~a" ,o (if (< ,i ,n) ,s* "")))))))

(defun str-split (s x &key prune trim &aux (lx (length x))) ; inefficient
  (declare #.*opt* (string s x) (boolean prune trim) (fixnum lx))
  "split string at substring. trim removes whitespace. prune removes empty strings"
  (labels ((lst (s) (typecase s (list s) (otherwise (list s))))
           (trm (s) (if trim (trim s) s))
           (splt (s &aux (i (subx? s x)))
             (if i (cons (trm (subseq s 0 i))
                         (lst (splt (subseq s (+ lx i)))))
                   (trm s))))
    (let ((res (lst (splt s))))
      (if prune (remove-if (λ (s) (zerop (length s))) res)
                res))))

(defmacro splt (s x &optional (trim t) prune)
  "split s at substrings x to vector. trims whitespace by default. prune removes empty strings."
      `(vec! (str-split ,(ct/kv/str s) ,(ct/kv/str x)
                :prune ,prune :trim ,trim)))

(defun repl (s from to) (declare (string s from to)) "replace from with to in s"
  (let ((s (strcat (mapcar (λ (s) (mkstr s to)) (str-split s from)))))
    (subseq s 0 (- (length s) (length to)))))

(defun seq* (v i &optional j) ; TODO: negative indices, tests
  (declare (vector v) (fixnum i)) "(subseq v ,@rest)"
  (subseq v i j))
(defun ind* (v &optional (i 0))
  (declare (vector v) (fixnum i)) "get index."
  (@@ v i nil))

(defun head* (s &optional (n 10) &aux (l (length s)))
  (declare (sequence s) (fixnum n l)) "first ±n elements"
  (cond ((zerop n) #()) ((plusp n) (subseq s 0 (min n l)))
                        (t         (subseq s 0 (max 0 (+ l n))))))

(defun tail* (s &optional (n 10) &aux (l (length s)))
  (declare (sequence s) (fixnum n l)) "last ±n elements"
  (cond ((zerop n) #()) ((plusp n) (subseq s (max 0 (- l n)) l))
                        (t         (subseq s (max 0 (+ l n)) l) )))

(defun sel* (v &rest seqs) (declare (vector v))
  "new vector with indices or ranges from v.
ranges are lists that behave like arguments to seq*."
  (apply #'concatenate 'vector
    (loop for s in seqs collect
      (etypecase s (list (apply #'seq* v s)) (fixnum `(,(ind* v s)))))))

(defun cat$ (&rest rest &aux (res (make-hash-table :test #'equal)))
  "add all keys from all hash tables in rest. left to right."
  (loop for kv of-type hash-table in rest
    do (loop for k being the hash-keys of (make$ kv)
         using (hash-value v) do (setf (gethash k res) (gethash k kv))))
  res)
(defun cat* (&rest rest) "concatenate sequences in rest to vector"
  (apply #'concatenate 'vector (mapcar #'vec! rest))) ; inefficient

(defmacro apply* (fx v) `(apply #',fx (coerce ,v 'list)))

(defun flatall* (x &optional (str nil))
  "flatten all sequences into new vector. if str is t strings will become
  individual chars."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((and str (stringp x)) (cons x acc))
                   ((vectorp x) (if (> (length x) 0)
                                    (rec (aref x 0) (rec (subseq x 1) acc))
                                    acc))
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (coerce (rec x nil) 'vector)))
(defun flatn* (a &optional (n 1) (str nil))
  (declare (sequence a) (fixnum n)) "flatten n times" ; inefficient
  (loop repeat n do
    (setf a (apply #'concatenate 'vector ;
              (map 'list (λ (x) (typecase x (string (if str x `#(,x)))
                                            (sequence x) (atom `#(,x))))
                   a))))
  a)
(defun flatn$ (a) "flatten ht to vector: k0 v0 k1 v1 ..."
  (declare (hash-table a))
  (let ((res (make-array (size? a) :adjustable nil)))
    (loop for k being the hash-keys of a using (hash-value v)
          for i from 0 by 2 do (setf (aref res i) k (aref res (1+ i)) v))
    res))

(defmacro m/replfx ((d &optional (f* (gensym "F")) (safe t)) &body body)
  (unless d (error "m/replfx: missing args."))
  (awg (rec) `(locally (declare #.*opt*)
    (labels ((,rec (,f*)
      (cond ,@(loop for (fx tx) in (group 2 body) collect `(,fx ,(if safe tx `(,rec ,tx))))
            ((hash-table-p ,f*)
             (loop with kv = (make-hash-table :test (hash-table-test ,f*))
               for k being the hash-key of ,f* using (hash-value v)
               do (setf (gethash k kv) (,rec v))
               finally (return kv)))
            ((stringp ,f*) ,f*)
            ((vectorp ,f*) (map 'vector #',rec ,f*))
            ((consp ,f*) (cons (,rec (car ,f*)) (,rec (cdr ,f*))))
            (t ,f*))))
      (,rec ,d)))))

