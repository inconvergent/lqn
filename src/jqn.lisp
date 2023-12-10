(in-package :jqn)

(defun compile/itr/preproc (q)
  (labels
    ((stringify-key (v) (dsb (a b c) v `(,a ,(ensure-string b) ,c)))
     (unpack-cons (k &aux (ck (car k)))
       (declare (list k))
       (case (length k)
         (0 (warn "empty selector"))
         (1 `(,@(unpack-mode ck *qmodes*) :_))             ; ?/m [m]@key _
         (2 `(,@(unpack-mode ck *qmodes*) ,(second k)))    ; ?/m [m]@key expr
         (3 `(,ck ,(ensure-string (second k)) ,(third k))) ; m       key expr
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

(defun copy-ht (ht)
  (loop with res = (make-hash-table :test #'equal)
        for k being the hash-keys of ht using (hash-value v)
        do (setf (gethash k res) (gethash k ht))
        finally (return res)))
(defun new-ht () (make-hash-table :test #'equal))
(defun strip-all (d) (if (car-all? d) (cdr d) d))
(defun new-conf (conf dat kk) `((:dat . (@ ,dat ,kk)) ,@conf))
(defun kvnil (kv)
  (typecase kv (hash-table (if (> (hash-table-count kv) 0) kv nil))
               (otherwise kv)))
(defun itradd (itr v) (vextend (kvnil v) itr))

(defun proc-qry (conf* q)
  "compile jqn query"
  (labels
    ((compile/$itr (conf d)
       (awg (kres dat)
         `(let* ((,dat ,(gk conf :dat))
                 (,kres ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))))
            ,@(loop for (mode kk expr) in (strip-all d)
                    collect `(,(kvadd mode) ,kres ,kk
                              ,(rec (new-conf conf dat kk) expr)))
            (kvnil ,kres))))
     (compile/*itr (conf d)
       (awg (ires dat)
         `(loop with ,ires = (mav)
                for ,dat across (ensure-vector ,(gk conf :dat))
                do (progn
                     ,(when (car-all? d) `(vvadd+ ,ires ,dat))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             collect `(,(vvadd mode) ,ires
                                       ,(rec (new-conf conf dat kk) expr))))
                finally (return ,ires))))
     (compile/*$itr (conf d)
       (awg (ires kres dat)
         `(loop with ,ires = (mav)
                for ,dat across (ensure-vector ,(gk conf :dat))
                for ,kres = ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))
                do (progn
                     ,@(loop for (mode kk expr) in (strip-all d)
                             for comp-expr = (rec (new-conf conf dat kk) expr)
                             collect `(,(kvadd mode) ,kres ,kk ,comp-expr))
                     (itradd ,ires ,kres))
                finally (return ,ires))))
     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-$itr? d) (compile/$itr conf (compile/itr/preproc (cdr d))))
             ((car-*itr? d) (compile/*itr conf (compile/itr/preproc (cdr d))))
             ((car-*$itr? d) (compile/*$itr conf (compile/itr/preproc (cdr d))))
             ((consp d) (cons (rec conf (car d))
                              (rec conf (cdr d))))
             (t (error "jqn compile error for: ~a" d)))))

    `(labels ((fn () ,(gk conf* :fn t))
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

