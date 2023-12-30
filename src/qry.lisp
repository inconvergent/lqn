(in-package :jqn)

(defmacro make-ind-getters (n)
  `(progn ,@(loop for i from 0 to n collect
              `(defun ,(symb :* i) (v &optional (k 0))
                 (declare (sequence v) (fixnum k))
                 (*n v (+ k ,i))))))
(make-ind-getters 9)

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
  (declare (ignore k) (symbol lft)) "do (vex lft (or v d))"
  `(vex ,lft (or ,v ,d)))
(defmacro *add? (lft k v)
  (declare (symbol lft)) "do (vex lft v) if (gethash k dat) is not nil"
  `(when ($_ ,k) (vex ,lft ,v)))
(defmacro *add% (lft k v)
  (declare (ignore k) (symbol lft)) "do (vex lft v) if v is not nil or empty"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (vex ,lft ,v*)))))

(defun strip-all (d) (declare (list d)) (if (car- all? d) (cdr d) d))
(defun $add (m) (declare (keyword m)) (ecase m (:+ '$add+) (:? '$add?) (:% '$add%) (:- '$del)))
(defun *add (m) (declare (keyword m)) (ecase m (:+ '*add+) (:? '*add?) (:% '*add%) (:- 'noop)))

(defun preproc/pipe (qq)
  (loop for q in qq collect
    (if (all? q) (kv q)
      (typecase q (cons q) (keyword `(** ,q)) (symbol `(*map ,q))
                  (string `(** ,q)) (number `(** ,(ct/kv/key q)))
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
                             (string `(,ck (sub? _ ,sk)))
                             (number `(,ck (sub? _ ,(ct/kv/key sk))))
                             (symbol `(,ck (,sk _)))
                             (cons `(,ck ,@(cdr k))))))
    (let* ((q* (remove-if #'all? q)) (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

; TODO: interpret expr => empty dict/vec as nil and drop in %mode
(defun compile/*new (d) `(vector ,@(loop for expr in d collect expr)))
(defun compile/$new (d)
  (awg (kv) `(let ((,kv ($make)))
               ,@(loop for (kk expr) in (group 2 d)
                       collect `(setf (gethash ,(ct/kv/key kk) ,kv) ,expr))
               ($nil ,kv))))

(defun labels/$_ (dat) `(($_ (k &optional d) ($ ,dat k d))))
(defmacro op/fxs ((par dat i) &body body)
  `(labels ((par () ,par) (cnt (&optional (k 0)) (+ ,i k))
            (num (&optional (d 0)) (typecase ,par (sequence (length ,par))
                                                  (hash-table (hash-table-count ,par))
                                                  (otherwise d)))
            ,@(labels/$_ dat))
     ,@body))

(defun dat/key (conf kk) `((:dat . ($_ ,kk)) ,@conf))
(defun dat/new (conf dat) `((:dat . ,dat) ,@conf))

(defun compile/*map (rec conf d &aux (dat* (gk conf :dat)))
  (awg (i ires dat)
    (when (car- all? d) (return-from compile/*map dat*))
    (labels ((do-map (curr expr)
               (unless (and (symbolp curr) (consp expr))
                       (error "*map: bad args: ~a ~a" curr expr))
               `(loop with ,ires = (mav)
                      for ,curr across (vec! ,dat*) for ,i from 0
                      do (op/fxs (,dat* ,curr ,i)
                           (vex ,ires ,(funcall rec (dat/new conf curr) expr)))
                      finally (return ,ires))))
      (case (length d) ; todo: handle if k is not defined?
        (0 (error "*map: missing args."))
        (1 (typecase (car d) (symbol (do-map dat `(,(car d) ,dat)))
                             (cons (do-map dat (car d)))))
        (otherwise (compile/*map rec conf `((|| ,@d))))))))

(defun compile/*fld (rec conf d &aux (dat* (gk conf :dat)))
  (awg (i res dat) ; 0 + ; 0 acc (+ acc _)
    (labels ((do-fld (init acc r expr)
               (unless (and (symbolp acc) (symbolp r))
                       (error "*fld: unexpected lhs/rhs: ~a/~a" acc r))
               (unless (consp expr) (error "*fld: unexpected expr: ~a" expr))
               `(loop with ,acc = ,init
                      for ,r across (vec! ,dat*) for ,i from 0
                      do (op/fxs  (,dat* ,r ,i)
                           (setf ,acc ,(funcall rec (dat/new conf r) expr)))
                      finally (return ,acc))))
      (case (length d)
        ((0 1) (error "*fld: missing args."))
        (2 (etypecase (second d)
            (symbol (do-fld (car d) res dat `(,(second d) ,res ,dat)))
            (cons (do-fld (car d) res dat `(,(first (second d)) ,res ,@(cdr (second d)))))))
        (3 (let ((d3 (third d))) (do-fld (car d) (second d) dat d3)))
        (otherwise (error "*fld: bad args: ~a" d))))))

(defun compile/$$ (rec conf d) ; {...} ; sel
  (awg (kv dat)
    `(let* ((,dat ,(gk conf :dat))
            (,kv ,(if (car- all? d) `($make ,dat) `($make))))
       (labels (($_ (k &optional d) ($ ,dat k d)))
        ,@(loop for (m kk expr) in (strip-all d) collect
            `(,($add m) ,kv ,kk ,(funcall rec (dat/key conf kk) expr))))
       ($nil ,kv))))

(defun compile/$* (rec conf d) ; #[...] ; sel
  (awg (i ires dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (vec! ,(gk conf :dat))
           for ,dat across (vec! ,vv) for ,i from 0
           do (op/fxs (,vv ,dat ,i)
                ,(when (car- all? d) `(*add+ ,ires nil ,dat))
                ,@(loop for (m kk expr) in (strip-all d) collect
                    `(,(*add m) ,ires ,kk ,(funcall rec (dat/key conf kk) expr))))
           finally (return ,ires))))

(defun compile/*$ (rec conf d) ; #{...} ; sel
  (awg (i ires kv dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (vec! ,(gk conf :dat))
           for ,i from 0 for ,dat of-type hash-table across (vec! ,vv)
           for ,kv of-type hash-table = ,(if (car- all? d) `($make ,dat) `($make))
           do (op/fxs (,vv ,dat ,i)
                ,@(loop for (m kk expr) in (strip-all d)
                        for comp-expr = (funcall rec (dat/key conf kk) expr)
                        collect `(,($add m) ,kv ,kk ,comp-expr))
                (vex ,ires ($nil ,kv)))
           finally (return ,ires))))

(defun compile/** (rec conf d &aux (d* (strip-all d))) ; [...] ; filter
  (awg (i ires dat vv)
    (labels ((get-modes (&rest mm)
               (let ((res (loop for (m expr) in d*
                                if (member m mm :test #'eq)
                                collect (funcall rec (dat/new conf dat) expr))))
                 (if res res '(nil)))))
      `(loop with ,ires of-type vector = (mav)
             with ,vv of-type vector = (vec! ,(gk conf :dat))
             for ,dat across (vec! ,vv) for ,i from 0
             do (op/fxs (,vv ,dat ,i)
                  (when (and (or ,(car- all? d) ,@(get-modes :? :%)
                                 (and ,@(get-modes :+)))
                             (not (or ,@(get-modes :-))))
                        (vex ,ires ,dat)))
             finally (return ,ires)))))

(defun compile/pipe (rec conf d) ; (|| ...)
  (awg (pipe)
    (if (< (length d) 2)
        `(op/fxs (,(gk conf :dat) ,(gk conf :dat) 0)
           ,(funcall rec conf (car d)))
        `(let ((,pipe ,(gk conf :dat)))
           ,@(loop for op in d for i from 0 collect
               `(op/fxs (,pipe ,pipe ,i)
                  (setf ,pipe ,(funcall rec (dat/new conf pipe) op))))
           ,pipe))))

(defmacro qry/fxs (conf &body body)
  `(labels ((ctx () ,(gk conf :ctx t)) (fn () ,(gk conf :fn t))
            (fi (&optional (k 0)) (+ k ,(or (gk conf :fi t) 0)))
            ,@(labels/$_ (gk conf :dat)))
     ,@body))

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
    `(qry/fxs ,conf* ,(rec conf* q))))

(defun qry/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---
   ~s
██ ██████████████████████████~%" q compiled))

(defmacro qryd (dat q &key conf db) "run jqn query on dat"
  (awg (dat*) (let ((compiled (proc-qry `((:dat . ,dat*) ,@conf) q)))
                (when db (qry/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))
(defmacro qry (dat &rest rest)
  "query data.
ex: (jqn:qry \"1 x 1 x 7 x 100 $
              3 x 8 x 30\"
            (splt _ :$)
            (*map k (splt k :x) int!? ; for each row, split and parse as int
                    ($new :num (num)  ; new nested dict for each row
                          :items (*map ($new :v _ :i (cnt))))))"
  `(qryd ,dat (|| ,@rest)))

(defun qryl (dat q &key conf db) "compile jqn query and run on dat"
  (eval `(qryd ,dat ,q :db ,db :conf ,conf)))

(defmacro jsnqryf (fn q &key db) "run jqn query on json file, fn"
  `(qryd (jsnloadf ,fn) ,q :db ,db))

