(in-package :jqn)

(defmacro something? (v &body body) ; TODO: recursive with ext function
  (declare (symbol v))
  `(typecase ,v (vector (when (> (length ,v) 0) (progn ,@body)))
                (hash-table (when (> (hash-table-count ,v) 0) (progn ,@body)))
                (otherwise (when ,v (progn ,@body)))))

; jsn -t use tqn output

(defun path-to-key (pp) (first (last (split pp "/"))))
(defmacro $add+ (lft k v &optional d)
  (declare (symbol lft)) "do (setf lft (or v default))"
  `(setf (gethash ,(path-to-key k) ,lft) (or ,v ,d)))
(defmacro $add? (lft k v)
  (declare (symbol lft)) "do (setf lft v) if ($_ k) is not nil"
  `(when ($_ ,k) (setf (gethash ,(path-to-key k) ,lft) ,v)))
(defmacro $add% (lft k v)
  (declare (symbol lft)) "do (setf lft v) if v is not nil"
  (awg (v*) `(let ((,v* ,v))
               (something? ,v* (setf (gethash (path-to-key ,k) ,lft) ,v*)))))
(defmacro $del (lft k v)
  (declare (ignore v) (symbol lft)) "delete key"
  `(remhash ,k ,lft))

(defmacro *add+ (lft k v &optional d)
  (declare (ignore k) (symbol lft)) "do (vex (or v default) lft)"
  `(vex (or ,v ,d) ,lft))
(defmacro *add? (lft k v)
  (declare (symbol lft)) "do (vex v lft) if (gethash k dat) is not nil"
  `(when ($_ ,k) (vex ,v ,lft)))
(defmacro *add% (lft k v)
  (declare (ignore k) (symbol lft)) "do (vex v lft) if v is not nil or empty"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (vex ,v* ,lft)))))

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

(defmacro car- (fx d) (declare (symbol fx d)) `(and (listp ,d) (,fx (car ,d))))

(defun sym-mode? (d &aux (mode-sym (unpack-mode d *qmodes* nil)) )
  (if mode-sym (values-list (unpack-mode mode-sym d *qmodes* :?))
               (values nil d)))
(defun all?   (d) (and (symbolp d) (eq (kv d) :_)))
(defun $new?  (d) (and (symbolp d) (eq (kv d) :$new)))
(defun *new?  (d) (and (symbolp d) (eq (kv d) :*new)))
(defun $$itr? (d) (and (symbolp d) (eq (kv d) :$$)))
(defun *$itr? (d) (and (symbolp d) (eq (kv d) :*$)))
(defun **itr? (d) (and (symbolp d) (eq (kv d) :**)))
(defun *>itr? (d) (and (symbolp d) (eq (kv d) :*>)))
(defun *map?  (d) (and (symbolp d) (eq (kv d) :*map)))
(defun *fld?  (d) (and (symbolp d) (eq (kv d) :*fld)))
(defun pipe?  (d) (and (symbolp d) (eq (kv d) :||)))
(defun jqnfx? (d) (and (symbolp d) (member (kv d) *fxns* :test #'eq)))

(defun car-sym-mode? (d)
  (typecase d (cons (if (or (str? (car d)) (symbolp (car d)))
                        (values-list (sym-mode? (car d)))
                        (values nil d)))
              (otherwise (values nil d))))

(defun qry/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---
   ~s
██ ██████████████████████████~%" q compiled))

(defun unpack-mode (sym &optional (modes *qmodes*) (default :+))
  (loop for m in modes
        for ind = (sub? (mkstr sym) (mkstr m))
        if (and ind (= ind 0))
        do (return-from unpack-mode
              (list (kv (subseq (mkstr m) 0 1))
                    (typecase sym (string (subseq sym 2))
                                  (symbol (psymb (symbol-package sym)
                                                 (subseq (mkstr sym) 2)))))))
  (list default sym))

(defun strip-all (d) (declare (list d)) (if (car- all? d) (cdr d) d))
(defun $add (m) (declare (keyword m)) (ecase m (:+ '$add+) (:? '$add?) (:% '$add%) (:- '$del)))
(defun *add (m) (declare (keyword m)) (ecase m (:+ '*add+) (:? '*add?) (:% '*add%) (:- 'noop)))

(defun preproc/$$itr (q)
  (labels
    ((unpack-s (k) `(,@(unpack-mode k *qmodes*) :_))
     (stringify (a)
      (handler-case (ct/kv/key a)
        (error (e) (error "$$itr: bad key: ~a.~%err: ~a" a e))))
     (stringify-key (v) (dsb (a b c) v `(,a ,(stringify b) ,c)))
     (unpack-cons (k &aux (ck (car k)))
       (case (length k) (0 (warn "$$itr: empty selector"))
         (1 `(,@(unpack-mode ck *qmodes*) :_))          ; ?/m [m]@key _
         (2 `(,@(unpack-mode ck *qmodes*) ,(second k))) ; ?/m [m]@key expr
         (3 `(,ck ,(stringify (second k)) ,(third k)))  ; m       key expr
         (otherwise (warn "$$itr: bad # items in selector: ~a" k))))
     (unpack (k)
       (typecase k (symbol (unpack-s k)) (string (unpack-s k))
                   (cons   (unpack-cons k))
                   (otherwise (error "$$itr: selector should be either: symbol, string, cons
got: ~a" k)))))
    (let* ((q* (remove-if #'all? q))
           (res (mapcar #'stringify-key (mapcar #'unpack q*))))
      (if (not (= (length q) (length q*)))
          (cons :_ res) res))))

(defun preproc/**itr (q)
  (labels
    ((unpack-s (k) (unpack-mode k *qmodes* :?))
     (unpack-expr (k &aux (ck (car k)))
       (dsb (mode sym) (unpack-mode ck)
         (cond ((and mode (zerop (length (mkstr sym)))) k)
               (mode `(,mode (,sym ,@(cdr k)))))))
     (unpack (k)
       (typecase k (symbol (unpack-s k))
                   (string (unpack-s k))
                   (cons   (unpack-expr k))
                   (otherwise (error "**itr: bad selector, expected either symbol, string, cons
got: ~a" k))))
     (stringify (a) (handler-case (ct/kv/key a)
                     (error (e) (error "**itr: bad key: ~a.~%err: ~a" a e))))
     (handle-strs (k)
       (typecase (second k)
         (string `(,(car k) (isub? _ ,(second k))))
         (symbol `(,(car k) (isub? _ ,(stringify (second k)))))
         (otherwise k))))
    (mapcar #'handle-strs (mapcar #'unpack (remove-if #'all? q)))))

; TODO: interpret expr => empty dict/vec as nil and drop in %mode


(defun compile/*new (d) `(vector ,@(loop for expr in d collect expr)))
(defun compile/$new (d)
  (awg (kv) `(let ((,kv ($make)))
               ,@(loop for (kk expr) in (strip-all d)
                       collect `(setf (gethash ,(ct/kv/key kk) ,kv) ,expr))
               ($nil ,kv))))

(defun labels/$_ (dat) `(($_ (k &optional d) ($ ,dat k d))))
(defun *itr/labels (vv dat i)
  `((cnt (&optional (k 0)) (+ ,i k)) (num () (length ,vv))  (par () ,vv)
    ,@(labels/$_ dat)))

(defun compile/*map (rec conf d &aux (dat* (gk conf :dat)))
  (awg (i ires dat)
    (labels ((do-map (vv curr expr)
               `(loop with ,ires = (mav)
                      for ,curr across ,vv for ,i from 0
                      do (labels (,@(*itr/labels vv curr i))
                           (*add+ ,ires nil
                             ,(funcall rec `((:dat . ,curr) ,@conf) expr)))
                      finally (return ,ires))))
      (case (length d)
        (1 (typecase (car d) (symbol (do-map dat* dat `(,(car d) ,dat)))
                             (cons (do-map dat* dat (car d)))))
        (2 (apply #'do-map dat* d))
        (3 (apply #'do-map (funcall rec `((:dat . ,dat*) ,@conf) d))) ; TODO: almost redundant
        (otherwise (error "*map: bad args: ~a" d))))))

(defun compile/*fld (rec conf d &aux (dat* (gk conf :dat)))
  (awg (i ires dat)
    (labels ((do-map (vv curr expr)
               `(loop with ,ires = (mav)
                      for ,curr across ,vv for ,i from 0
                      do (labels (,@(*itr/labels vv curr i))
                           (*add+ ,ires nil
                             ,(funcall rec `((:dat . ,curr) ,@conf) expr)))
                      finally (return ,ires))))
      (case (length d)
        (1 (typecase (car d) (symbol (do-map dat* dat `(,(car d) ,dat)))
                             (cons (do-map dat* dat (car d)))))
        (2 (apply #'do-map dat* d))
        (3 (apply #'do-map (funcall rec `((:dat . ,dat*) ,@conf) d))) ; TODO: almost redundant
        (otherwise (error "*fld: bad args: ~a" d))))))

(defun new-conf (conf kk) `((:dat . ($_ ,kk)) ,@conf))

(defun compile/$$ (rec conf d) ; {...}
  (awg (kv dat)
    `(let* ((,dat ,(gk conf :dat))
            (,kv ,(if (car- all? d) `($make ,dat) `($make))))
       (labels (($_ (k &optional d) ($ ,dat k d)))
        ,@(loop for (m kk expr) in (strip-all d)
                collect `(,($add m) ,kv ,kk
                          ,(funcall rec (new-conf conf kk) expr))))
       ($nil ,kv))))

(defun compile/*> (rec conf d) ; #[...]
  (awg (i ires dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
           for ,dat across ,vv for ,i from 0
           do (labels (,@(*itr/labels vv dat i))
                ,(when (car- all? d) `(*add+ ,ires nil ,dat))
                ,@(loop for (m kk expr) in (strip-all d)
                        collect `(,(*add m) ,ires ,kk
                                  ,(funcall rec (new-conf conf kk) expr))))
           finally (return ,ires))))

(defun compile/*$ (rec conf d) ; #{...}
  (awg (i ires kv dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
           for ,i from 0 for ,dat of-type hash-table across ,vv
           for ,kv of-type hash-table = ,(if (car- all? d) `($make ,dat) `($make))
           do (labels (,@(*itr/labels vv dat i))
                ,@(loop for (m kk expr) in (strip-all d)
                        for comp-expr = (funcall rec (new-conf conf kk) expr)
                        collect `(,($add m) ,kv ,kk ,comp-expr))
                (vex ($nil ,kv) ,ires))
           finally (return ,ires))))

(defun compile/** (rec conf d &aux (d* (strip-all d))) ; [...]
  (awg (i ires dat vv)
    (labels ((get-modes (mm)
               (let ((res (loop for (m expr) in d*
                                if (member m mm :test #'eq)
                                collect (funcall rec `((:dat . ,dat) ,@conf) expr))))
                 (if res res '(nil)))))
      `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
           for ,dat across ,vv for ,i from 0
           do (labels (,@(*itr/labels vv dat i))
                (when (or ,@(get-modes `(:? :%)) ; TODO: :-@, :_
                          (and ,@(get-modes `(:+))))
                (*add+ ,ires nil ,dat)))
         finally (return ,ires)))))

(defun compile/pipe (rec conf d) ; (|| ...)
  (awg (pipe)
    `(let ((,pipe ,(gk conf :dat)))
      ,@(loop for op in d for i from 0
              collect `(labels (,@(labels/$_ pipe))
                         (setf ,pipe ,(funcall rec `((:dat . ,pipe) ,@conf) op))))
      ,pipe)))

(defun proc-qry (conf* q) "compile jqn query"
  (labels
    ((rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car- pipe?  d) (compile/pipe #'rec conf (cdr d)))
             ((car- **itr? d) (compile/**   #'rec conf (preproc/**itr (cdr d))))
             ((car- *$itr? d) (compile/*$   #'rec conf (preproc/$$itr (cdr d))))
             ((car- $$itr? d) (compile/$$   #'rec conf (preproc/$$itr (cdr d))))
             ((car- *>itr? d) (compile/*>   #'rec conf (preproc/$$itr (cdr d))))
             ((car- *map?  d) (compile/*map #'rec conf (cdr d)))
             ((car- *fld?  d) (compile/*fld #'rec conf (cdr d)))
             ((car- *new?  d) (rec conf (compile/*new (cdr d))))
             ((car- $new?  d) (rec conf (compile/$new (cdr d))))
             ((car- jqnfx? d) `(,(psymb 'jqn (car d)) ,@(rec conf (cdr d))))
             ((consp d) (cons (rec conf (car d)) (rec conf (cdr d))))
             (t (error "jqn compile error for: ~a" d)))))
    `(labels ((ctx () ,(gk conf* :ctx t))
              (fn () ,(gk conf* :fn t))
              (fi (&optional (k 0)) (+ k ,(or (gk conf* :fi t) 0)))
              ,@(labels/$_ (gk conf* :dat)))
       ,(rec conf* q))))

; WRAPPERS ------------

(defmacro qryd (dat q &key conf db) "run jqn query on dat"
  (awg (dat*) (let ((compiled (proc-qry `((:dat . ,dat*) ,@conf) q)))
                (when db (qry/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))

(defun qryl (dat q &key conf db) "compile jqn query and run on dat"
  (eval `(qryd ,dat ,q :db ,db :conf ,conf)))

(defmacro jsnqryf (fn q &key db) "run jqn query on json file, fn"
  `(qryd (jsnloadf ,fn) ,q :db ,db))

