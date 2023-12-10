(in-package :jqn)

; QRY RUNTIME

(defmacro maybe (fx arg)
  (declare (symbol fx)) "run (fx arg) only if arg is not nil"
  (awg (arg*) `(let ((,arg* ,arg))
                 (if (null ,arg*) nil (,fx ,arg*)))))
(defun copy-ht (ht)
  (declare (hash-table ht))
  (loop with res = (make-hash-table :test #'equal)
        for k being the hash-keys of ht using (hash-value v)
        do (setf (gethash k res) (gethash k ht))
        finally (return res)))
(defun new-ht () (make-hash-table :test #'equal))
(defun kvnil (kv)
  (typecase kv (hash-table (if (> (hash-table-count kv) 0) kv nil))
               (otherwise kv)))
(defun itradd (itr v) (vextend (kvnil v) itr))

(defmacro @ (o k &optional default)
  "get k from dict o; or default"
  (if default `(gethash ,k (nil-as-empty-ht ,o) ,default)
              `(gethash ,k (nil-as-empty-ht ,o))))
(defmacro ind (o sel) ; rename
  "get index or range from json array (vector).
if sel is an atom: (aref o ,sel)
if sel is cons: (subseq o ,@sel)"
  (typecase sel (cons `(subseq o ,@sel))
                (atom `(aref ,o ,sel))
                (otherwise (error "ind: wanted atom or (atom atom). got: ~a" sel))))

(defmacro nilop (&rest rest) (declare (ignore rest)) "do nothing" nil)

(defmacro kvadd+ (dat lft k v &optional default)
  (declare (ignore dat) (symbol lft)) "do (setf lft (or v default))"
  `(setf (gethash ,k ,lft) (or ,v ,default)))
(defmacro kvadd? (dat lft k v)
  (declare (symbol lft)) "do (setf lft v) if (gethash k dat) is not nil"
  `(when (gethash ,k ,dat) (setf (gethash ,k ,lft) ,v)))
(defmacro kvadd% (dat lft k v)
  (declare (ignore dat) (symbol lft)) "do (setf lft v) if v is not nil"
  (awg (v*) `(let ((,v* ,v)) (when ,v* (setf (gethash ,k ,lft) ,v*)))))

(defmacro kvdel (dat lft k v)
  (declare (ignore dat v) (symbol lft)) "delete key"
  `(remhash ,k ,lft))

(defmacro vvadd+ (dat lft k v &optional default)
  (declare (ignore dat) (symbol lft)) "do (vextend (or v default) lft)"
  `(vextend (or ,v ,default) ,lft))
(defmacro vvadd? (dat lft k v)
  (declare (symbol lft)) "do (vextend v lft) if (gethash k dat) is not nil"
  `(when (gethash ,k ,dat) (vextend ,v ,lft)))
(defmacro vvadd% (dat lft k v)
  (declare (ignore dat) (symbol lft)) "do (vextend v lft) if v is not nil"
  (awg (v*) `(let ((,v* ,v)) (when ,v* (vextend ,v* ,lft)))))


; COMPILER

(defun kvadd (mode)
  (ecase mode (:+ 'kvadd+) (:? 'kvadd?) (:% 'kvadd%) (:- 'kvdel)))
(defun vvadd (mode)
  (ecase mode (:+ 'vvadd+) (:? 'vvadd?) (:% 'vvadd%) (:- 'nilop)))

(defun new-conf (conf dat kk) `((:dat . (@ ,dat ,kk)) ,@conf))
(defun strip-all (d) (declare (list d)) (if (car-all? d) (cdr d) d))

(defun compile/itr/preproc (q)
  (labels
    ((stringify (a)
      (handler-case
        (ensure-string a)
        (error (e) (error "failed to stringify key: ~a. try \"a\"" a))
        )
                )
     (stringify-key (v) (dsb (a b c) v `(,a ,(stringify b) ,c)))
     (unpack-cons (k &aux (ck (car k)))
       (declare (list k))
       (case (length k)
         (0 (warn "empty selector"))
         (1 `(,@(unpack-mode ck *qmodes*) :_))             ; ?/m [m]@key _
         (2 `(,@(unpack-mode ck *qmodes*) ,(second k)))    ; ?/m [m]@key expr
         (3 `(,ck ,(stringify (second k)) ,(third k))) ; m       key expr
         (otherwise (warn "bad # items in selector: ~a" k))))
     (unpack (k)
       (typecase k
         (symbol `(,@(unpack-mode k *qmodes*) :_))
         (string `(,@(unpack-mode k *qmodes*) :_))
         (cons   (unpack-cons k))
         (otherwise (error "selector should be symbol, string or list. got: ~a" k)))))
    (let* ((q* (remove-if #'all? q))
           (res (mapcar #'stringify-key (mapcar #'unpack q*))))
      (if (not (= (length q) (length q*)))
          (cons :_ res) res))))

; TODO: interpret expr => empty dict/vec as nil and drop in %mode

(defun proc-qry (conf* q)
  "compile jqn query"
  (labels
    ((*itr/labels (vv dat i)
       `((i (&optional (k 0)) (+ ,i k))
         (num () (length ,vv))
         (dat () ,dat)
         (@dat (k &optional default) (@ (dat) k default))
         (par () ,vv)))
     (compile/$itr (conf d)
       (awg (kres dat)
         `(let* ((,dat ,(gk conf :dat))
                 (,kres ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))))
            (labels ((dat () ,dat)
                     (@dat (k &optional default) (@ (dat) k default)))
             ,@(loop for (mode kk expr) in (strip-all d)
                     collect `(,(kvadd mode) ,dat ,kres ,kk
                               ,(rec (new-conf conf dat kk) expr))))
            (kvnil ,kres))))
     (compile/*itr (conf d)
       (awg (ires dat i vv)
         `(loop with ,ires = (mav)
                with ,vv = (ensure-vector ,(gk conf :dat))
                for ,dat across ,vv
                for ,i from 0
                do (labels (,@(*itr/labels vv dat i))
                     ,(when (car-all? d) `(vvadd+ nil ,ires nil ,dat))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             collect `(,(vvadd mode) ,dat ,ires ,kk
                                       ,(rec (new-conf conf dat kk) expr))))
                finally (return ,ires))))
     (compile/*$itr (conf d)
       (awg (ires kres dat i vv)
         `(loop with ,ires = (mav)
                with ,vv = (ensure-vector ,(gk conf :dat))
                for ,i from 0
                for ,dat across ,vv
                for ,kres = ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))
                do (labels (,@(*itr/labels vv dat i))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             for comp-expr = (rec (new-conf conf dat kk) expr)
                             collect `(,(kvadd mode) ,dat ,kres ,kk ,comp-expr))
                     (itradd ,ires ,kres))
                finally (return ,ires))))

     ; TODO: incomplete
     ; TODO: what happens with selector inside *new/$new?
     (compile/*new (conf d) ; ignores _, simpler expr pre processor
       (awg (ires)
         `(let ((,ires (mav))) ;
            ,@(loop for (mode kk expr) in (strip-all d)
                    collect `(,(vvadd mode) nil ,ires
                              ,(rec conf expr)))
            ,ires)))

     (compile/$new (conf d) ; ignores _
       (awg (kres dat)
         `(let* ((,dat ,(gk conf :dat))
                 (,kres (new-ht)))
            ,@(loop for (mode kk expr) in (strip-all d)
                    collect `(,(kvadd mode) ,dat ,kres ,kk
                              ,(rec conf expr)))
            (kvnil ,kres))))

     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-$itr? d) (compile/$itr conf (compile/itr/preproc (cdr d))))
             ((car-*itr? d) (compile/*itr conf (compile/itr/preproc (cdr d))))
             ((car-*$itr? d) (compile/*$itr conf (compile/itr/preproc (cdr d))))
             ((car-*new? d) (compile/*new conf (compile/itr/preproc (cdr d))))
             ((car-$new? d) (compile/$new conf (compile/itr/preproc (cdr d))))
             ((consp d) (cons (rec conf (car d))
                              (rec conf (cdr d))))
             (t (error "jqn compile error for: ~a" d)))))
    `(labels ((fn () ,(gk conf* :fn t))
              (fi (&optional (k 0)) (+ k ,(or (gk conf* :fi t))))
              (ctx () ,(gk conf* :ctx t)))
     ,(rec conf* q))))

(defmacro qryd (dat &key (q :_) conf db)
  (declare (boolean db)) "run jqn query on dat"
  (awg (dat*) (let ((compiled (proc-qry `((:dat . ,dat*) (:dattype) ,@conf) q)))
                (when db (jqn/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))

(defmacro qryf (fn &key (q :_) db)
  (declare (boolean db)) "run jqn query on file, fn"
  `(qryd (jsnloadf ,fn) :q ,q :db ,db))

(defun qryl (dat &key (q :_) conf db)
  "compile jqn query and run on dat"
  (eval `(qryd ,dat :q ,q :db ,db :conf ,conf)))

