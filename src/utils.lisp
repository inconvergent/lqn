(in-package :lqn)

(defun unpack-mode (o &optional (default :+) merciful)
  (labels ((valid-mode (m) (member m *qmodes* :test #'eq))
           (repack- (s s*) (etypecase s (symbol (psymb (symbol-package s) (subseq s* 2)))
                                        (string (subseq s* 2))))
           (unpack-cons (cns) (if (valid-mode (car cns)) cns
                                  (dsb (m s) (unpack- (car cns)) `(,m (,s ,@(cdr cns))))))
           (unpack- (s &aux (s* (mkstr s)) (splt (subx? s* "@")))
             (if (and splt (= splt 1)) (let ((m (kv (subseq s* 0 1)))) ; nil -> :nil
                                         (if (or merciful (valid-mode m)) (list m (repack- s s*))
                                             (error "lqn: invalid mode in: ~a" s)))
                                       (list default s))))
    (typecase o (symbol (unpack- o)) (string (unpack- o)) (cons (unpack-cons o))
      (otherwise (error "lqn: bad mode thing to have mode: ~a" o)))))

(defmacro ?? (fx arg &rest args) ; ?!
  (declare (symbol fx)) "run (fx arg ..) only if arg is not nil."
  (awg (arg*) `(let ((,arg* ,arg)) (when ,arg* (,fx ,arg* ,@args)))))

(defun gk (conf k &optional silent &aux (hit (cdr (assoc k conf))))
  (declare (list conf) (keyword k)) "get k from config"
  (if (or silent hit) hit (warn "LQN: missing conf key: ~a~%conf: ~s" k conf)))

(defun group (n l) (declare (list l) (fixnum n)) "group l into lists of n elements."
  (if (< n 1) (error "group: group size is smaller than 1"))
  (labels ((rec (l acc)
             (let ((rest (nthcdr n l)))
               (if (consp rest) (rec rest (cons (subseq l 0 n) acc))
                                (nreverse (cons l acc))))))
    (if l (rec l nil) nil)))

(defun strcat (&rest rest) "concatenate all strings in sequences"
  (apply #'mkstr
    (mapcar (lambda (s) (etypecase s (string s)
                          (list (apply #'concatenate 'string s))
                          (vector (apply #'concatenate 'string (coerce s 'list)))))
            rest)))
(defun trim (s &optional (chars '(#\Space #\Newline #\Backspace #\Tab #\Linefeed
                                  #\Page #\Return #\Rubout)))
  (declare (string s)) "trim string" (string-trim chars s))

(defmacro out (s &rest rest) "print to standard out"
  (awg (s*) (if rest `(format *standard-output* ,s ,@rest)
                     `(let ((,s* ,s))
                        (when (and ,s* (or (not (stringp ,s*)) (> (length ,s*) 0)))
                          (format *standard-output* "~&~a~&" ,s*))))))
(defmacro fmt (s &rest rest) "format to string."
  (if rest `(format nil ,s ,@rest) `(format nil "~a" ,s)))

(defun pref? (s pref &optional d &aux (s (str! s)) (pref (str! pref)))
  (declare (string s pref)) "s if s starts with pref; or d"
  (if (and (<= (length pref) (length s))
           (string= pref s :end2 (length pref)))
       s d))
(defun ipref? (s suf &optional d) "ignore case pref?" (pref? (sup s) (sup suf) d))

(defun suf? (s suf &optional d &aux (s (str! s)) (suf (str! suf)))
  "s if s ends with suf; or d" (pref? (reverse s) (reverse suf) d))
(defun isuf? (s suf &optional d) "ignore case suf?" (suf? (sup s) (sup suf) d))

(defun subx? (s sub &aux (s (str! s)) (sub (str! sub)))
  (declare (optimize speed (safety 2)))
  "returns index where substring matches s from left to right. otherwise nil"
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from subx? i)))
(defun isubx? (s sub) "ignore case subx?" (subx? (sup s) (sup sub)))
(defun sub? (s sub &optional d) "s if sub is substring of s; ord" (if (subx? s sub) s d))
(defun isub? (s sub &optional d) "ignore case sub?" (if (isubx? s sub) s d))

(defmacro msym? (s q &optional d)
  "compare symbol `a` to `b`. if `b` is a keword or symbol
a perfect match is required. if `b` is a string it performs a substring
match. If `b` is an expression, `a` is compared to the evaluated value of `b`."
  (awg (s* res)
   `(let* ((,s* ,s)
           (,res (and (symbolp ,s*)
                      ,(etypecase q
                                (keyword `(eq ,s* q))
                                (symbol `(eq ,s* ',q)) ; direct match
                                (string `(isub? (ct/kv/key ,s*) ,q))
                                (cons   `(equalp (ct/kv/key ,s*) ,q))))))
      (if ,res ,s* ,d))))

(defun split (s x &key prune &aux(lx (length x)))
  (declare (optimize speed) (boolean prune))
  "split string at substring. prune removes empty strings"
  (labels ((lst (s) (typecase s (list s) (t (list s))))
           (splt (s &aux (i (subx? s x)))
             (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))
(defun splt (s x &optional prune &aux (s (str! s)) (x (str! x)))
  "split s at substring x. returns vector."
  (vec! (split s x :prune prune)))

(defun repl (s from to &aux (s (str! s)) (from (str! from)) (to (str! to)))
  "replace from with to in s"
  (let ((s (strcat (mapcar (lambda (s) (mkstr s to))
                           (split s from)))))
    (subseq s 0 (- (length s) (length to)))))

(defun make-adjustable-vector (&key init (type t) (size 128))
  (if init (make-array (length init) :fill-pointer t :initial-contents init
                                     :element-type type :adjustable t)
           (make-array size :fill-pointer 0 :element-type type :adjustable t)))

; (defun *rng (a &optional b)) TODO:

(defun *seq (v i &optional j) ; TODO: negative indices, tests
  (declare (vector v) (fixnum i)) "(subseq v ,@rest)"
  (subseq v i j))
(defun *n (v &optional (i 0)) (declare (vector v) (fixnum i)) "get index." (aref v i))

(defun head (s &optional (n 10) &aux (l (length s)))
  (declare (sequence s) (fixnum n l)) "first ±n elements"
  (cond ((zerop n) #()) ((plusp n) (subseq s 0 (min n l)))
                        (t         (subseq s 0 (max 0 (+ l n))))))

(defun tail (s &optional (n 10) &aux (l (length s)))
  (declare (sequence s) (fixnum n l)) "last ±n elements"
  (cond ((zerop n) #()) ((plusp n) (subseq s (max 0 (- l n)) l))
                        (t         (subseq s (max 0 (+ l n)) l) )))

(defun size (l) "length of sequence l or number of keys in kv l."
  (etypecase l (sequence (length l)) (hash-table (hash-table-count l))))
(defun size? (l &optional d) "length of sequence/number of keys in kv."
  (typecase l (sequence (length l)) (hash-table (hash-table-count l)) (otherwise d)))

(defun sup (&rest rest) "mkstr and upcase" (string-upcase (apply #'mkstr rest)))
(defun sdwn (&rest rest) "mkstr and downcase" (string-downcase (apply #'mkstr rest)))

(defun *sel (v &rest seqs)
  (declare (vector v))
  "new vector with indices or ranges from v.
ranges are lists that behave like arguments to *seq."
  (apply #'concatenate 'vector
    (loop for s in seqs collect
      (etypecase s (list (apply #'*seq v s)) (fixnum `(,(*n v s)))))))

(defun $make (&optional kv &aux (res (make-hash-table :test #'equal))) "new/soft copy kv."
  (when kv (loop for k being the hash-keys of kv using (hash-value v)
                 do (setf (gethash k res) (gethash k kv))))
  res)
(defun $nil (kv) "return nil for emtpy hash-tables. otherwise return kv." ; TODO: use kv?
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
  "concatenate all vectors in these vectors."
  (labels ((do-arg (aa) (loop for a across aa
                              do (loop for b across a do (vex res b)))))
    (loop for a in rest do (typecase a (vector (do-arg a))
                                       (otherwise (vex res a)))))
  res)
(defun *$cat (&rest rest &aux (res (make-hash-table :test #'equal)))
  "for all vectors in rest; for all kvs in these vectors;
copy all keys into new kv. left to right."
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

(defun >< (o) ; TODO: recursive?
  "remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`."
  (typecase o (sequence (remove-if-not (lambda (o*) (something? o* t)) o))
              (hash-table (loop with keys = (list)
                                for k being the hash-keys of o using (hash-value v)
                                do (unless (something? v t) (push k keys))
                                finally (loop for k in keys do (remhash k o)))
                          o)
              (otherwise o)))

(defun t/fndfx (d fx &optional (res (list))) ; TODO: vector / ht
  (declare (optimize speed) (function fx) (list res)) "find all elements where fx is t."
  (cond ((funcall fx d) (return-from t/fndfx (cons d res)))
        ((atom d) nil)
        (t (let ((l (t/fndfx (car d) fx res))
                 (r (t/fndfx (cdr d) fx res)))
             (when l (setf res `(,@l ,@res)))
             (when r (setf res `(,@r ,@res))))
           res)))

(defmacro m/replfx ((d &optional (safe t)) &body body)
  (unless d (error "m/replfx: missing args."))
  (awg (rec d*)
    `(labels ((,rec (,d*)
       (cond ,@(loop for (fx tx) in body
                     collect `((funcall ,fx ,d*)
                              ,(if safe `(funcall ,tx ,d*)
                                        `(,rec (funcall ,tx ,d*)))))
             ((hash-table-p ,d*)
              (loop with kv = (make-hash-table :test (hash-table-test ,d*))
                for k being the hash-key of ,d* using (hash-value v)
                do (setf (gethash k kv) (,rec v))
                finally (return kv)))
             ((stringp ,d*) ,d*) ((vectorp ,d*) (map 'vector #',rec ,d*))
             ((atom ,d*) ,d*) (t (cons (,rec (car ,d*)) (,rec (cdr ,d*)))))))
       (,rec ,d))))

