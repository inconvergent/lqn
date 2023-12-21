(in-package :jqn)

(defmacro noop (&rest rest) (declare (ignore rest)) "do nothing. return nil" nil)
(defmacro something? (v &body body) ; ??
  (declare (symbol v))
  `(typecase ,v (vector (when (> (length ,v) 0) (progn ,@body)))
                (hash-table (when (> (hash-table-count ,v) 0) (progn ,@body)))
                (otherwise (when ,v (progn ,@body)))))

; QRY RUNTIME

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

(defun $make (&optional kv &aux (res (make-hash-table :test #'equal)))
  "new/soft copy kv"
  (when kv (loop for k being the hash-keys of kv using (hash-value v)
                 do (setf (gethash k res) (gethash k kv))))
  res)
(defun $nil (kv)
  "return nil for emtpy hash-tables. otherwise return kv"
  (typecase kv (hash-table (if (> (hash-table-count kv) 0) kv nil))
               (otherwise kv)))

(defun $cat (&rest rest &aux (res (make-hash-table :test #'equal)))
  "add all keys from all hash tables in rest. left to right."
  (loop for kv of-type hash-table in rest
            do (loop for k being the hash-keys of ($make kv)
                     using (hash-value v)
                     do (setf (gethash k res) (gethash k kv))))
  res)
(defun *cat (&rest rest &aux (res (make-adjustable-vector))) ; TODO: handle non vectors
  "concatenate all vectors in these vectors.
non-vectors are included in their position"
  (labels ((do-arg (aa) (loop for a across aa
                              do (loop for b across a do (vextend b res)))))
    (loop for a in rest do (typecase a (vector (do-arg a))
                                       (otherwise (vextend a res)))))
  res)
(defun *$cat (&rest rest &aux (res (make-hash-table :test #'equal)))
  "for all vectors in rest; for all hts in these vectors; copy all keys into new kv. left to right"
  (loop for v of-type vector in rest
        do (loop for kv of-type hash-table across v
                 do (loop for k being the hash-keys of ($make kv)
                          using (hash-value v)
                          do (setf (gethash k res) v))))
  res)

(defun $rget (o pp d)
  (labels ((rec (o pp)
             (unless pp (return-from rec (or o d)))
             (typecase o (hash-table (rec (gethash (car pp) o) (cdr pp)))
                         (otherwise (return-from rec (or o d))))))
    (rec o (split-substr pp "/"))))
(defmacro @ (o k &optional d) "get key k from o" `($rget ,o (ensure-key ,k) ,d))

(defmacro ?? (fx arg &rest args) ; ?!
  (declare (symbol fx)) "run (fx arg) only if arg is not nil."
  (awg (arg*) `(let ((,arg* ,arg)) (when ,arg* (,fx ,arg* ,@args)))))

(defun path-to-key (pp) (first (last (split-substr pp "/"))))

(defmacro $add+ (lft k v &optional d)
  (declare (symbol lft)) "do (setf lft (or v default))"
  `(setf (gethash ,(path-to-key k) ,lft) (or ,v ,d)))
(defmacro $add? (lft k v)
  (declare (symbol lft)) "do (setf lft v) if (@_ k) is not nil"
  `(when (@_ ,k) (setf (gethash ,(path-to-key k) ,lft) ,v)))
(defmacro $add% (lft k v)
  (declare (symbol lft)) "do (setf lft v) if v is not nil"
  (awg (v*) `(let ((,v* ,v))
               (something? ,v* (setf (gethash (path-to-key ,k) ,lft) ,v*)))))
(defmacro $del (lft k v)
  (declare (ignore v) (symbol lft)) "delete key"
  `(remhash ,k ,lft))

(defmacro *add+ (lft k v &optional d)
  (declare (ignore dat k) (symbol lft)) "do (vextend (or v default) lft)"
  `(vextend (or ,v ,d) ,lft))
(defmacro *add? (lft k v)
  (declare (symbol lft)) "do (vextend v lft) if (gethash k dat) is not nil"
  `(when (@_ ,k) (vextend ,v ,lft)))
(defmacro *add% (lft k v)
  (declare (ignore k) (symbol lft)) "do (vextend v lft) if v is not nil or empty"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (vextend ,v* ,lft)))))

; list/vector: remove if not someting
; hash-table: remove key if value not something
(defun >< (o)
  "remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`."
  (typecase o
    (sequence (remove-if-not (lambda (o*) (something? o* t)) o))
    (hash-table (loop with keys = (list)
                      for k being the hash-keys of o using (hash-value v)
                      do (unless (something? v t) (push k keys))
                      finally (loop for k in keys do (remhash k o)))
                o)
    (otherwise (warn ">< works on sequence/vector or hash-table.
got: ~a" o))))

; COMPILER

(defun strip-all (d) (declare (list d)) (if (car-all? d) (cdr d) d))
(defun new-conf (conf kk) `((:dat . (@_ ,kk)) ,@conf))
(defun $add (m) (declare (keyword m)) (ecase m (:+ '$add+) (:? '$add?) (:% '$add%) (:- '$del)))
(defun *add (m) (declare (keyword m)) (ecase m (:+ '*add+) (:? '*add?) (:% '*add%) (:- 'noop)))

(defun compile/itr/preproc (q)
  (labels
    ((stringify (a)
      (handler-case (ensure-key a)
        (error (e) (error "failed to stringify key: ~a.~%err: ~a" a e))))
     (stringify-key (v) (dsb (a b c) v `(,a ,(stringify b) ,c)))
     (unpack-cons (k &aux (ck (car k)))
       (declare (list k))
       (case (length k) (0 (warn "empty selector"))
         (1 `(,@(unpack-mode ck *qmodes*) :_))          ; ?/m [m]@key _
         (2 `(,@(unpack-mode ck *qmodes*) ,(second k))) ; ?/m [m]@key expr
         (3 `(,ck ,(stringify (second k)) ,(third k)))  ; m       key expr
         (otherwise (warn "bad # items in selector: ~a" k))))
     (unpack (k)
       (typecase k (symbol `(,@(unpack-mode k *qmodes*) :_))
                   (string `(,@(unpack-mode k *qmodes*) :_))
                   (cons   (unpack-cons k))
                   (otherwise (error "selector should be symbol, string, list.
got: ~a" k)))))
    (let* ((q* (remove-if #'all? q))
           (res (mapcar #'stringify-key (mapcar #'unpack q*))))
      (if (not (= (length q) (length q*)))
          (cons :_ res) res))))

; TODO: interpret expr => empty dict/vec as nil and drop in %mode

(defun proc-qry (conf* q) "compile jqn query"
  (labels
    ((labels/@_ (dat) `((@_ (k &optional d) (@ ,dat k d))))
     (*itr/labels (vv dat i)
       `((cnt (&optional (k 0)) (+ ,i k)) (num () (length ,vv))  (par () ,vv)
         ,@(labels/@_ dat)))
     (compile/*new (conf d) `(vector ,@(loop for o in d collect (rec conf o))))
     (compile/$new (conf d)
       (awg (kv dat) `(let ((,kv ($make)))
                        ,@(loop for (kk expr) in (strip-all d)
                                collect `($add+ ,kv ,(ensure-key kk)
                                           ,(rec conf expr)))
                        ($nil ,kv))))
     (compile/$itr (conf d)
       (awg (kv dat)
         `(let* ((,dat ,(gk conf :dat))
                 (,kv ,(if (car-all? d) `($make ,dat) `($make))))
            (labels ((@_ (k &optional d) (@ ,dat k d)))
             ,@(loop for (mode kk expr) in (strip-all d)
                     collect `(,($add mode) ,kv ,kk
                               ,(rec (new-conf conf kk) expr))))
            ($nil ,kv))))
     (compile/*itr (conf d)
       (awg (ires dat i vv)
         `(loop with ,ires of-type vector = (mav)
                with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
                for ,dat across ,vv for ,i from 0
                do (labels (,@(*itr/labels vv dat i))
                     ,(when (car-all? d) `(*add+ ,ires nil ,dat))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             collect `(,(*add mode) ,ires ,kk
                                       ,(rec (new-conf conf kk) expr))))
                finally (return ,ires))))
     (compile/*$itr (conf d)
       (awg (ires kv dat i vv)
         `(loop with ,ires of-type vector = (mav)
                with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
                for ,i from 0 for ,dat of-type hash-table across ,vv
                for ,kv of-type hash-table = ,(if (car-all? d) `($make ,dat) `($make))
                do (labels (,@(*itr/labels vv dat i))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             for comp-expr = (rec (new-conf conf kk) expr)
                             collect `(,($add mode) ,kv ,kk ,comp-expr))
                     (vextend ($nil ,kv) ,ires))
                finally (return ,ires))))
     (compile/pipe (conf d)
       (awg (pipe)
         `(let ((,pipe ,(gk conf :dat)))
           ,@(loop for op in d for i from 0
                   collect `(labels (,@(labels/@_ pipe))
                             (setf ,pipe ,(rec `((:dat . ,pipe) ,@conf) op))))
           ,pipe)))
     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-*$itr? d) (compile/*$itr conf (compile/itr/preproc (cdr d))))
             ((car-$itr? d)  (compile/$itr conf (compile/itr/preproc (cdr d))))
             ((car-*itr? d)  (compile/*itr conf (compile/itr/preproc (cdr d))))
             ((car-*new? d)  (compile/*new conf (cdr d)))
             ((car-$new? d)  (compile/$new conf (cdr d)))
             ((car-pipe? d)  (compile/pipe conf (cdr d)))
             ((car-jqnfx? d) `(,(psymb 'jqn (car d))
                               ,@(rec conf (cdr d))))
             ((consp d) (cons (rec conf (car d))
                              (rec conf (cdr d))))
             (t (error "jqn compile error for: ~a" d)))))
    `(labels ((ctx () ,(gk conf* :ctx t))
              (fn () ,(gk conf* :fn t))
              (fi (&optional (k 0)) (+ k ,(or (gk conf* :fi t) 0)))
              ,@(labels/@_ (gk conf* :dat)))
       ,(rec conf* q))))

(defmacro qryd (dat &key (q :_) conf db) "run jqn query on dat"
  (awg (dat*) (let ((compiled (proc-qry `((:dat . ,dat*) ,@conf) q)))
                (when db (jqn/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))

(defmacro qryf (fn &key (q :_) db) "run jqn query on file, fn"
  `(qryd (jsnloadf ,fn) :q ,q :db ,db))

(defun qryl (dat &key (q :_) conf db) "compile jqn query and run on dat"
  (eval `(qryd ,dat :q ,q :db ,db :conf ,conf)))

