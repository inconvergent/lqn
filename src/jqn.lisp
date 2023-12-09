(in-package :jqn)

(defun compile/itr/preproc (q)
  (labels
    ((expand-exprs (sel)
      (loop for (mode kk expr) in sel
            for gh = `(@ :_ ,(ensure-string kk))
            collect `(,mode ,kk
                       ,(if (all? expr) gh
                          (tree-replace-fx expr #'all? (lambda (o) gh))))))
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
           (res (expand-exprs (mapcar #'unpack q*))))
      (if (not (= (length q) (length q*)))
          (cons :_ res) res))))

(defun copy-ht (ht)
  (loop with res = (make-hash-table :test #'equal)
        for k being the hash-keys of ht using (hash-value v)
        do (setf (gethash k res) (gethash k ht))
        finally (return res)))
(defun new-ht () (make-hash-table :test #'equal))
(defun strip-all (d) (if (car-all? d) (cdr d) d))

(defun itr-select-dat (conf dat expr kk)
 (if (or (car-*$itr? expr)
         (car-$itr? expr)
         (car-*itr? expr)) ; this is probably incorrect
  `((:dat . (@ ,dat ,kk)) ,@conf)
  `((:dat . ,dat) ,@conf)
  ))

(defun proc-qry (conf* q)
  "compile jqn query"
  (labels
    (
     (compile/$itr (conf d)
       (error "$itr not implemented "))

     (compile/*itr (conf d)
       (veq:vp :* conf d)
       (awg (ires kres dat) ; incomplete
         `(loop with ,ires = (mav)
                for ,dat across (ensure-vector ,(gk conf :dat))
                do (progn
                     ,@(loop for (mode kk expr) in (strip-all d)
                             for kk* = (ensure-string kk)
                             for com-expr = (rec (itr-select-dat conf dat expr kk*)
                                                 expr)
                             collect `(vextend ,comp-expr ,ires)))
                finally (return ,ires))))
     (compile/*$itr (conf d)
       (veq:vp :*$ conf d)
       (awg (ires kres dat)
         `(loop with ,ires = (mav)
                for ,dat across (ensure-vector ,(gk conf :dat))
                for ,kres = ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))
                do (progn ; TODO: this fails when expr has nested iterator
                     ,@(loop for (mode kk expr) in (strip-all d)
                             for kk* = (ensure-string kk)
                             for comp-expr = (rec (itr-select-dat conf dat expr kk*)
                                                  expr)
                             collect `(setf (gethash ,kk* ,kres) ,comp-expr))
                     (vextend ,kres ,ires))
                finally (return ,ires))))
     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-*itr? d) (compile/*itr conf (compile/itr/preproc (cdr d))))
             ((car-*$itr? d) (compile/*$itr conf (compile/itr/preproc (cdr d))))
             ((car-$itr? d) (compile/$itr conf (compile/itr/preproc (cdr d))))
             ((consp d)
              (cons (rec conf (car d))
                    (rec conf (cdr d))))
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

