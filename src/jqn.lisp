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
    (let* ((q* (remove-if #'all? q))
           (res (mapcar #'unpack q*)))
      (if (not (= (length q) (length q*)))
          (cons :_ res) res))))

(defun copy-ht (ht)
  (loop with res = (make-hash-table :test #'equal)
        for k being the hash-keys of ht using (hash-value v)
        do (setf (gethash k res) (gethash k ht))
        finally (return res)))
(defun strip-all (d) (if (car-all? d) (cdr d) d))

(defun proc-qry (conf* q)
  "compile jqn query"
  (labels
    (

     (compile/$itr (conf d)
      (error "$itr not implemented "))

     (compile/*itr (conf d)
      (veq:vp conf d)
       (awg (ires kres dat) ; incomplete
         `(loop with ,ires = (mav)
                for ,dat across (ensure-vector ,(gk conf :dat))
                ; for ,kres = (list)
                do (progn
                     ,@(loop for (mode kk expr) in (reverse (strip-all d))
                             for kk* = (ensure-string kk)
                             collect `(vextend
                                   ; ,(rec `((:dat . ())) expr)
                                   (gethash ,kk* ,dat)
                                   ,ires)
                     ))

                    ; (progn ,(if (car-all? d)
                    ;            `(push ,dat ,kres))
                    ;  (vextend ,kres ,ires))
                finally (return ,ires))
         ))
     (compile/*$itr (conf d)
       (awg (ires kres dat)
         `(loop with ,ires = (mav)
                for ,dat across (ensure-vector ,(gk conf :dat))
                for ,kres = ,(if (car-all? d) `(copy-ht ,dat)
                                              `(make-hash-table :test #'equal))
                do (progn ,@(loop for (mode kk expr) in (reverse (strip-all d))
                                  for kk* = (ensure-string kk)
                                  collect `(setf (gethash ,kk* ,kres)
                                                 ,(rec `((:dat . (gethash ,kk* ,dat)) ,@conf)
                                                       expr)))
                          (vextend ,kres ,ires))
                finally (return ,ires))))
     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-*itr? d) (compile/*itr conf (compile/itr/preproc (cdr d))))
             ((car-*$itr? d) (compile/*$itr conf (compile/itr/preproc (cdr d))))
             ((car-$itr? d) (compile/$itr conf (compile/itr/preproc (cdr d))))
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

