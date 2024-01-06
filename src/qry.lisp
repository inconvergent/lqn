(in-package :lqn)

(defmacro make-ind-getters (n)
  `(progn ,@(loop for i from 0 to n collect
              `(defun ,(symb :* i) (v &optional (k 0))
                 (declare (sequence v) (fixnum k))
                 (*n v (+ k ,i))))))
(make-ind-getters 9)

(defmacro *new (&rest d) "new vector with these elements" `(vector ,@d))
(defmacro $new (&rest d) "new kv/hash-table from these (k v) pairs"
  (awg (kv) `(let ((,kv ($make)))
               ,@(loop for (kk expr) in (group 2 d)
                       collect `(setf (gethash ,(ct/kv/str kk) ,kv) ,expr))
               ,kv)))

(defun ct/path/key (pp) (first (last (str-split pp "/"))))
(defmacro $add+ (lft k v &optional d) (declare (symbol lft)) "do (setf lft (or v default))"
  `(setf (gethash ,(ct/path/key k) ,lft) (or ,v ,d)))

(defmacro $add? (lft k v) (declare (symbol lft)) "do (setf lft v) if value of key is not nil"
  `(when ($rget (dat) ,k) (setf (gethash ,(ct/path/key k) ,lft) ,v)))

(defmacro $add% (lft k v) (declare (symbol lft)) "do (setf lft v) if v is not nil"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (setf (gethash (ct/path/key ,k) ,lft) ,v*)))))

(defmacro $del (lft k v) (declare (ignore v) (symbol lft)) "delete key" `(remhash ,k ,lft))

(defmacro *add+ (lft k v &optional d) (declare (ignore k) (symbol lft)) "do (vex lft (or v d))"
  `(vex ,lft (or ,v ,d)))

(defmacro *add? (lft k v) (declare (symbol lft)) "do (vex lft v) if (gethash k dat) is not nil"
  `(when ($rget (dat) ,k) (vex ,lft ,v)))

(defmacro *add% (lft k v) (declare (ignore k) (symbol lft)) "do (vex lft v) if v is not nil or empty"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (vex ,lft ,v*)))))

(defun dat/new (conf dat) `((:dat . ,dat) ,@conf))
(defun $add (m) (declare (keyword m)) (ecase m (:+ '$add+) (:? '$add?) (:% '$add%) (:- '$del)))
(defun *add (m) (declare (keyword m)) (ecase m (:+ '*add+) (:? '*add?) (:% '*add%) (:- 'noop)))
(defun strip-all (d) (declare (list d)) (if (car- all? d) (cdr d) d))

(defmacro //fxs/op/ ((par &optional (dat par) (i 0)) &body body)
  (declare (symbol par))
  `(labels ((par () ,par) (dat () ,dat) (cnt (&optional (k 0)) (+ ,i k))
            (num (&optional (d 0)) (size? ,par d)))
     ,@body))

(defun pre/or-all (d)
  (etypecase d (boolean d) (cons d) (keyword d) (sequence d)
    (symbol (if (all? d) :_ `(,d :_)))))

(defun when-equal (a b) (when (equal a b) a))
(defun pre/xpr-sel (ty k) (declare (symbol k))
  (etypecase ty (number `(when-equal ,k ,ty))
                (keyword `(and (str? ,k) (isub? ,k ,(ct/kv/str ty))))
                (string `(and (str? ,k) (sub? ,k ,ty)))
                (symbol `(when (,ty ,k) ,k))
                (cons ty) (boolean `(when-equal ,ty ,k))))

(defun prescan (qq)
  (loop for q in qq do (when (equal :@ (kv (sym-not-kv q))) (warn "unexpected @ in: ~a" qq)))
  qq)
(defun pre/|| (qq) (unless qq (warn "||: missing args."))
  (loop for q in (prescan qq) collect
    (if (all? q) (kv q)
        (typecase q (cons q) (keyword `(** ,q)) (symbol `(*map ,q))
                    (string `(** ,q)) (number `(** ,(ct/kv/str q)))
                    (vector `(*map ,@(coerce q 'list)))
                    (otherwise (error "||: expected cons/symbol/vector/number. got: ~a" q))))))

(defun pre/$$ (q &optional (m :+)) (unless q (warn "$$: missing args."))
  (labels
    ((str- (a b c) `(,a ,(typecase b (keyword (sdwn (mkstr b))) (string b)
                           (otherwise (error "$$: expected string/:keyword. got: ~a" b)))
                        ,c))
     (repack- (o) (subseq `(,@o :_) 0 3))
     (repack-cons (ck k) (ecase (length k) (3 k) (2 `(,ck ,(caadr k) ,(cadadr k)))))
     (unpack- (o &aux (k (unpack-mode o m)) (ck (car k)))
       (apply #'str- (etypecase (second k)
                       (symbol (repack- k)) (string (repack- k))
                       (cons (repack-cons ck k))))))
    (let* ((q* (remove-if #'all? q)) (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

(defun pre/** (q &optional (mm :?)) (unless q (warn "**: missing args."))
  (labels ((unpack- (o) (dsb (m sk) (unpack-mode o mm) `(,m ,(pre/xpr-sel sk :_)))))
    (let* ((q* (remove-if #'all? q)) (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

(defun compile/|| (rec conf d &aux (dat* (gk conf :dat))) ; (|| ...)
  (awg (pipe-)
    (if (< (length d) 2)
        `(//fxs/op/ (,dat*) ,(funcall rec conf (car d)))
        `(let ((,pipe- ,dat*))
           ,@(loop for op in d for i from 0 collect
               `(//fxs/op/ (,pipe- ,pipe- ,i)
                  (setf ,pipe- ,(funcall rec (dat/new conf pipe-) op))))
           ,pipe-))))

(defun compile/*map (rec conf d &aux (dat* (gk conf :dat))) ; (*map ...)
  (awg (i ires dat)
    (labels ((do-map (expr)
               `(loop with ,ires = (mav)
                  for ,dat across (vec! ,dat*) for ,i from 0
                  do (//fxs/op/ (,dat* ,dat ,i)
                       (vex ,ires ,(funcall rec (dat/new conf dat) expr)))
                  finally (return ,ires))))
      (case (length d) (0 (warn "*map: missing args."))
        (1 (let ((cd (car d)))
             (etypecase cd (string cd) (number cd) (keyword cd) (boolean cd)
                           (vector (funcall rec conf cd))
                           (symbol (do-map (if (all? cd) :_ `(,cd ,dat))))
                           (cons (do-map cd)))))
        (otherwise (compile/*map rec conf `((|| ,@d))))))))

(defun compile/*fld (rec conf d &aux (dat* (gk conf :dat))) ; (*fld ...)
  (awg (i res dat) ; 0 + ; 0 acc (+ acc _)
    (labels ((do-fld (init acc r expr)
               (unless (and (symbolp acc) (symbolp r))
                       (error "*fld: expected symbols, got: ~a/~a" acc r))
               (unless (consp expr) (error "*fld: expected cons, got: ~a" expr))
               `(loop with ,acc = ,init
                      for ,r across (vec! ,dat*) for ,i from 0
                      do (//fxs/op/ (,dat* ,r ,i)
                           (setf ,acc ,(funcall rec (dat/new conf r) expr)))
                      finally (return ,acc))))
      (case (length d) (0 (error "*fld: missing args."))
        (2 (etypecase (second d)
             (symbol (do-fld (car d) res dat `(,(second d) ,res ,dat)))
             (cons (do-fld (car d) res dat `(,(first (second d)) ,res ,@(cdr (second d)))))))
        (3 (let ((d3 (third d))) (do-fld (car d) (second d) dat d3)))
        (otherwise (error "*fld: bad args: ~a" d))))))

(defun compile/$$ (rec conf d) ; {...} ; sel
  (awg (kres dat dat*)
    `(let* ((,dat ,(gk conf :dat))
            (,kres ,(if (car- all? d) `($make ,dat) `($make))))
       (//fxs/op/ (,dat)
         ,@(loop for (m kk expr) in (strip-all d) collect
             `(let ((,dat* ($rget (dat) ,kk)))
                (declare (ignorable ,dat*))
               (,($add m) ,kres ,kk ,(funcall rec (dat/new conf dat*) expr)))))
       ($nil ,kres))))

(defun compile/$* (rec conf d) ; #[...] ; sel
  (awg (i ires dat dat* vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (vec! ,(gk conf :dat))
           for ,dat across (vec! ,vv) for ,i from 0
           do (//fxs/op/ (,vv ,dat ,i)
                ,(when (car- all? d) `(*add+ ,ires nil ,dat))
                ,@(loop for (m kk expr) in (strip-all d) collect
                    `(let ((,dat* ($rget (dat) ,kk) ))
                      (declare (ignorable ,dat*))
                      (,(*add m) ,ires ,kk ,(funcall rec (dat/new conf dat*) expr)))))

           finally (return ,ires))))

(defun compile/*$ (rec conf d) ; #{...} ; sel
  (awg (i ires kv dat dat* vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (vec! ,(gk conf :dat))
           for ,i from 0 for ,dat of-type hash-table across (vec! ,vv)
           for ,kv of-type hash-table = ,(if (car- all? d) `($make ,dat) `($make))
           do (//fxs/op/ (,vv ,dat ,i)
                ,@(loop for (m kk expr) in (strip-all d)
                        collect `(let ((,dat* ($rget (dat) ,kk)))
                                   (declare (ignorable ,dat*))
                                  (,($add m) ,kv ,kk ,(funcall rec (dat/new conf dat*) expr))))
                (vex ,ires ($nil ,kv)))
           finally (return ,ires))))

(defun compile/** (rec conf d) ; [...] ; filter
  (awg (i ires dat vv)
    `(loop with ,ires of-type vector = (mav)
           with ,vv of-type vector = (vec! ,(gk conf :dat))
           for ,dat across (vec! ,vv) for ,i from 0
           do ,(compile/xpr rec `((:par . ,vv) (:dat . ,dat) (:i . ,i))
                                `(,@d (vex ,ires ,dat) nil))
           finally (return ,ires))))

(defun compile/*? (rec conf d &aux (cd (car d)) (sd (second d))) ; (*? test expr) ; filter, map
  (unless (< 0 (length d) 3) (error "*?: bad args: ~a" d))
  (awg (i ires dat dat* vv)
    `(loop with ,ires of-type vector = (mav) with ,vv of-type vector = (vec! ,(gk conf :dat))
       for ,dat across (vec! ,vv) for ,i from 0
       for ,dat* = ,(funcall rec (dat/new conf dat) (pre/xpr-sel cd dat))
       do (//fxs/op/ (,dat ,dat* ,i)
            (when ,dat*
              (vex ,ires ,(if (= (length d) 1) dat*
                              (funcall rec (dat/new conf dat*) (pre/or-all sd))))))
       finally (return ,ires))))

(defun compile/xpr (rec conf d &aux (dat (gk conf :dat)) (cd (butlast d 2))) ; (xpr sel .. hit miss)
  (unless (> (length d) 2) (error "xpr: missing args: ~a" d))
  (labels ((get-modes (&rest mm) (loop for (m expr) in (strip-all cd)
                                   if (member m mm :test #'eq) collect (funcall rec conf expr))))
     (let ((ors (get-modes :? :%)) (nots (get-modes :-)) (ands (get-modes :+)))
      `(//fxs/op/ (,(gk conf :par t) ,dat ,(gk conf :i t))
        (if (and (or ,(car- all? cd) ,@(when ors ors) ,@(when ands `((and ,@ands))))
                 ,@(when nots `((not (or ,@nots)))))
          ,@(mapcar (lambda (d) (funcall rec conf (pre/or-all d)))
                    (last d 2)))))))
(defun compile/mxpr (rec conf d)
  (awg (f dat)
    `(let ((,dat ,(gk conf :dat)))
       (m/replfx (,dat ,f)
        ,@(loop with conf* = `((:dat . ,f) (:par . ,dat) (:i . 0) ,@conf)
            for d* in d nconc `(,(funcall rec conf* `(?xpr ,@(butlast d*) t nil))
                                ,(funcall rec conf* (pre/or-all (car (last d*))))))))))
(defun compile/txpr (rec conf d) (funcall rec conf `(?mxpr ,d)))

(defun compile/@ (rec conf d)
  (case (length d)
    (1 `(@@ (dat) ,(funcall rec conf (car d))))
    (2 `(@@ (dat) ,@(funcall rec conf d)))
    (3 `(@@ ,@(funcall rec conf d)))))

(defmacro //fxs/qry (conf &body body &aux (meta (gensym "META")))
  `(let ((,meta (make-hash-table :test #'eq)))
     (labels ((ctx () ,(gk conf :ctx t)) (fn () ,(gk conf :fn t)) (dat () ,(gk conf :dat))
              (fi (&optional (k 0)) (+ k ,(or (gk conf :fi t) 0)))
              (ghv (k &optional d) (declare (symbol k)) (gethash k ,meta d))
              (hld (k &optional v) (declare (symbol k))
                (if v (setf (gethash k ,meta) v) (remhash k ,meta)) v))
       ,@body)))

(defun proc-qry (conf* q) "compile lqn query"
  (labels
    ((rec (conf d)
       (cond ((all? d) (gk conf :dat)) ((stringp d) d)
         ((vectorp d) (compile/*map #'rec conf (coerce d 'list))) ((atom d) d)
         ((optrig? :$$    d) (compile/$$   #'rec conf (pre/$$ (cdr d))))
         ((optrig? :$*    d) (compile/$*   #'rec conf (pre/$$ (cdr d))))
         ((optrig? :*$    d) (compile/*$   #'rec conf (pre/$$ (cdr d))))
         ((optrig? :**    d) (compile/**   #'rec conf (pre/** (cdr d))))
         ((optrig? :||    d) (compile/||   #'rec conf (pre/|| (cdr d))))
         ((optrig? :@     d) (compile/@    #'rec conf (cdr d)))
         ((optrig? :*?    d) (compile/*?   #'rec conf (cdr d)))
         ((optrig? :*fld  d) (compile/*fld #'rec conf (cdr d)))
         ((optrig? :*map  d) (compile/*map #'rec conf (cdr d)))
         ((optrig? :?mxpr d) (compile/mxpr #'rec conf (cdr d)))
         ((optrig? :?txpr d) (compile/txpr #'rec conf (cdr d)))
         ((optrig? :?xpr  d) (compile/xpr  #'rec conf `(,@(pre/** (cdr (butlast d 2))) ,@(last d 2))))
         ((car- lqnfx? d) `(,(psymb 'lqn (car d)) ,@(rec conf (cdr d))))
         ((consp d) (cons (rec conf (car d)) (rec conf (cdr d))))
         (t (error "lqn: unexpected clause: ~a" d)))))
    `(//fxs/qry ,conf* ,(rec conf* q))))

(defun qry/show (q compiled)
  (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---------~%   ~s
██ END      ██████████████████████████~%" q compiled))

(defmacro qryd (dat q &key conf db) "run lqn query on dat"
  (awg (dat*) (let ((compiled (proc-qry `((:dat . ,dat*) ,@conf) q)))
                (when db (qry/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))
(defmacro qry (dat &rest rest) "query data. rest is wrapped in the pipe operator."
  `(qryd ,dat (|| ,@rest)))
(defun qryl (dat q &key conf db) "compile lqn query and run on dat"
  (eval `(qryd ,dat ,q :db ,db :conf ,conf)))
(defmacro jsnqryf (fn q &key db) "run lqn query on json file, fn"
  `(qryd (jsnloadf ,fn) ,q :db ,db))

