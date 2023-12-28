(in-package :jqn)

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

(defmacro car- (fx d) (declare (symbol fx d)) `(and (listp ,d) (,fx (car ,d))))

(defun sym-mode? (d &aux (mode-sym (unpack-mode d nil)))
  (if mode-sym (values-list (unpack-mode mode-sym d :?))
               (values nil d)))
(defun all?   (d) (and (symbolp d) (eq (kv d) :_)))
(defun $new?  (d) (and (symbolp d) (eq (kv d) :$new)))
(defun *new?  (d) (and (symbolp d) (eq (kv d) :*new)))
(defun $$sel? (d) (and (symbolp d) (eq (kv d) :$$)))
(defun *$sel? (d) (and (symbolp d) (eq (kv d) :*$)))
(defun **sel? (d) (and (symbolp d) (eq (kv d) :**)))
(defun $*sel? (d) (and (symbolp d) (eq (kv d) :$*)))
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

(defun strip-all (d) (declare (list d)) (if (car- all? d) (cdr d) d))
(defun $add (m) (declare (keyword m)) (ecase m (:+ '$add+) (:? '$add?) (:% '$add%) (:- '$del)))
(defun *add (m) (declare (keyword m)) (ecase m (:+ '*add+) (:? '*add?) (:% '*add%) (:- 'noop)))

(defun preproc/pipe (qq)
  (loop for q in qq collect
    (if (all? q) (kv q)
      (typecase q (keyword `(** ,(ct/kv/key q))) (string `(** ,q)) (number `(** ,(ct/kv/key q)))
                  (symbol `(*map ,q)) (cons q)
                  (otherwise (error "||jqn: invalid clause: ~a" q))))))

(defun preproc/$$ (q &optional (m :+))
  (labels
    ((str- (a b c) `(,a ,(etypecase b (symbol (ct/kv/key b)) (string b)) ,c))
     (repack- (o) (subseq `(,@o :_) 0 3))
     (repack-cons (ck k) (ecase (length k) (3 k) (2 `(,ck ,(caadr k) ,(cadadr k)))))
     (unpack- (o &aux (k (unpack-mode o m)) (ck (car k)))
       (apply #'str- (etypecase (second k)
                       (symbol (repack- k)) (string (repack- k))
                       (cons (repack-cons ck k))))))
    (let* ((q* (remove-if #'all? q)) (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

(defun preproc/** (q &optional (m :?))
  (labels
    ((unpack- (o &aux (k (unpack-mode o m)) (ck (car k)) (sk (second k)))
       (etypecase (second k) (keyword `(,ck (isub? _ ,(ct/kv/key sk))))
                             (string `(,ck (isub? _ ,sk)))
                             (cons `(,ck ,@(cdr k))) (symbol `(,ck (,sk _))))))
    (let* ((q* (remove-if #'all? q)) (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

; TODO: interpret expr => empty dict/vec as nil and drop in %mode
(defun compile/*new (d) `(vector ,@(loop for expr in d collect expr)))
(defun compile/$new (d)
  (awg (kv) `(let ((,kv ($make)))
               ,@(loop for (kk expr) in (strip-all d)
                       collect `(setf (gethash ,(ct/kv/key kk) ,kv) ,expr))
               ($nil ,kv))))

(defun labels/$_ (dat) `(($_ (k &optional d) ($ ,dat k d))))
(defun *sel/labels (par dat i)
  `((par () ,par) (cnt (&optional (k 0)) (+ ,i k))
    (num (&optional (d 0)) (typecase ,par (sequence (length ,par))
                                          (hash-table (hash-table-count ,par))
                                          (otherwise d)))
     ,@(labels/$_ dat)))

(defun compile/*map (rec conf d &aux (dat* (gk conf :dat)))
  (awg (i ires dat)
    (labels ((do-map (vv curr expr)
               `(loop with ,ires = (mav)
                      for ,curr across ,vv for ,i from 0
                      do (labels (,@(*sel/labels vv curr i))
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
  (awg (i res dat) ; 0 + ; 0 rht (+ old rht) ; 0 lft rht (+ lft rht)
    (labels ((do-fld (init l r expr)
               (unless (and (symbolp l) (symbolp r))
                       (error "*fld: unexpected lhs/rhs: ~a/~a" l r))
               (unless (consp expr) (error "*fld: unexpected expr: ~a" expr))
               `(loop with ,l = ,init
                      for ,r across ,dat* for ,i from 0
                      do (labels (,@(*sel/labels dat* r i))
                           (setf ,l ,(funcall rec `((:dat . ,r) ,@conf) expr)))
                      finally (return ,l))))
      (case (length d)
        (2 (do-fld (car d) res dat `(,(second d) ,res ,dat)))
        (3 (let ((d3 (third d)))
             (do-fld (car d) res (second d) `(,(car d3) ,res ,@(cdr d3)))))
        (4 (apply #'do-fld d))
        (otherwise (error "*fld: bad args: ~a" d))))))

(defun new-conf (conf kk) `((:dat . ($_ ,kk)) ,@conf))
(defun compile/$$ (rec conf d) ; {...} ; sel
  (awg (kv dat)
    `(let* ((,dat ,(gk conf :dat))
            (,kv ,(if (car- all? d) `($make ,dat) `($make))))
       (labels (($_ (k &optional d) ($ ,dat k d)))
        ,@(loop for (m kk expr) in (strip-all d)
                collect `(,($add m) ,kv ,kk
                          ,(funcall rec (new-conf conf kk) expr))))
       ($nil ,kv))))

(defun compile/$* (rec conf d) ; #[...] ; sel
  (awg (i ires dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
           for ,dat across ,vv for ,i from 0
           do (labels (,@(*sel/labels vv dat i))
                ,(when (car- all? d) `(*add+ ,ires nil ,dat))
                ,@(loop for (m kk expr) in (strip-all d)
                        collect `(,(*add m) ,ires ,kk
                                  ,(funcall rec (new-conf conf kk) expr))))
           finally (return ,ires))))

(defun compile/*$ (rec conf d) ; #{...} ; sel
  (awg (i ires kv dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
           for ,i from 0 for ,dat of-type hash-table across ,vv
           for ,kv of-type hash-table = ,(if (car- all? d) `($make ,dat) `($make))
           do (labels (,@(*sel/labels vv dat i))
                ,@(loop for (m kk expr) in (strip-all d)
                        for comp-expr = (funcall rec (new-conf conf kk) expr)
                        collect `(,($add m) ,kv ,kk ,comp-expr))
                (vex ($nil ,kv) ,ires))
           finally (return ,ires))))

(defun compile/** (rec conf d &aux (d* (strip-all d))) ; [...] ; filter
  (awg (i ires dat vv)
    (labels ((get-modes (&rest mm)
               (let ((res (loop for (m expr) in d*
                                if (member m mm :test #'eq)
                                collect (funcall rec `((:dat . ,dat) ,@conf) expr))))
                 (if res res '(nil)))))
      `(loop with ,ires of-type vector = (mav)
             with ,vv of-type vector = (ensure-vector ,(gk conf :dat))
             for ,dat across ,vv for ,i from 0
             do (labels (,@(*sel/labels vv dat i))
                  (when (and (or ,(car- all? d) ,@(get-modes :? :%)
                                 (and ,@(get-modes :+)))
                             (not (or ,@(get-modes :-))))
                        (vex ,dat ,ires)))
             finally (return ,ires)))))

(defun compile/pipe (rec conf d) ; (|| ...)
  (awg (pipe)
    `(let ((,pipe ,(gk conf :dat)))
      ,@(loop for op in d for i from 0
              collect `(labels (,@(*sel/labels pipe pipe 0))
                         (setf ,pipe ,(funcall rec `((:dat . ,pipe) ,@conf) op))))
      ,pipe)))

(defun proc-qry (conf* q) "compile jqn query"
  (labels
    ((rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car- pipe?  d) (compile/pipe #'rec conf (preproc/pipe  (cdr d))))
             ((car- **sel? d) (compile/**   #'rec conf (preproc/** (cdr d))))
             ((car- *$sel? d) (compile/*$   #'rec conf (preproc/$$ (cdr d))))
             ((car- $$sel? d) (compile/$$   #'rec conf (preproc/$$ (cdr d))))
             ((car- $*sel? d) (compile/$*   #'rec conf (preproc/$$ (cdr d))))
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

