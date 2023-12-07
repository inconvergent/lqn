(in-package :jqn)

(defun compile/itr/preproc (q)
  (labels
    ((unpack-cons (k &aux (ck (car k)))
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
    (mapcar #'unpack q)))

 ; this is hacky, and will break if we add negation
(defmacro expr-all-shortcut (d dat body)
  `(if (all? (cadr ,d)) ,dat ,body))

(defun proc-qry (conf* q)
  "compile jqn query"
  (labels
    ((compile/expr/rec (kk conf expr &aux (dat (gk conf :dat)))
       (cond ((all? expr) `(*@ ,dat ,kk))
             ((atom expr) expr)
             ((car-itr? expr)
                (rec `((:dat *@ ,dat ,kk) ,@conf) expr))
             ((car-kvget? expr)
                `(*@ ,dat ,(ensure-string (second expr)) ,@(cddr expr)))
             ((consp expr) (cons (compile/expr/rec kk conf (car expr))
                                 (compile/expr/rec kk conf (cdr expr))))
             (t (warn "unexpected expr in selector: ~a~%expr: ~a" k expr))))
     (psh (mode kk expr)
       (case mode (:? 'apsh?) (:+ 'apsh+)
         (otherwise (error "unexpected mode in selector: ~a ~a~%expr: ~a"
                           mode kk expr))))
     (do-op-body (kvres conf d)
       (loop for (mode kk expr) in (reverse d)
             collect `(,(psh mode kk expr) ,kvres ,kk
                       ,(compile/expr/rec
                          (ensure-string kk) conf expr))))
     (compile/kv (conf d)
       (awg (kvres) `(let ((,kvres (list))) ,@(do-op-body kvres conf d) ,kvres)))
     (compile/itr (conf d)
       (awg (kvres itrlst dat)
         `(loop with ,itrlst = (mav) for ,kvres = (list)
                for ,dat across (ensure-vector ,(gk conf :dat))
                do (progn
                     ,@(do-op-body kvres `((:dat . ,dat) ,@conf) d)
                     (vextend ,kvres ,itrlst))
                finally (return ,itrlst))))
     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-kv? d)  (expr-all-shortcut d dat
                             (compile/kv  conf (compile/itr/preproc (cdr d)))))
             ((car-itr? d) (expr-all-shortcut d dat
                             (compile/itr conf (compile/itr/preproc (cdr d)))))
             (t (error "compile error for: ~a" d)))))

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

