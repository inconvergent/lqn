(in-package :lqn)

(defun ct/path/key (pp) (first (last (str-split pp "/"))))
(defun dat/new (conf dat) `((:dat . ,dat) ,@conf))
(defun strip-all (d) (declare (list d)) (if (car- all? d) (cdr d) d))

; CONTEXTS

(defmacro //fxs/qry ((dat fn fi) &body body)
  (declare (symbol dat fn fi))
  (awg (nope meta)
    `(let ((,meta (make-hash-table :test #'eq)))
     (block ,nope
       (labels ((fn () ,fn)
                (fi (&optional (k 0)) (+ k ,fi))
                (ghv (k &optional d) (declare (keyword k)) (gethash k ,meta d))
                (hld (k &optional v) (declare (keyword k))
                  (if v (setf (gethash k ,meta) v) (remhash k ,meta)) v)
                (cnt (&optional (k 0)) (+ k ,fi))
                (nope (&optional a) (return-from ,nope a))
                (err (&optional a) (error "qry err: ~a" a))
                (wrn (&optional a) (warn "qry wrn: ~a" a))
                (par () ,dat)
                (pnum (&optional d) (size? (par) d))
                (itr () (wrn "no (itr) in qry scope."))
                (inum () (wrn "no (inum) in qry scope.")))
         ,@body)))))

(defmacro ∈ ((par &optional i itr) &body body)
  (declare (symbol par itr))
  `(labels (,@(when par `((par () ,par) (pnum () (size? ,par))))
            ,@(when itr `((itr () ,itr) (inum () (size? ,itr))))
            ,@(when i `((cnt (&optional (k 0)) (+ ,i k)))))
     ,@body))

(defun compile/$add (rec conf mode lft k v)
  (labels ((rec (x) (funcall rec conf x)))
    (case mode
      (:? `(when ,(gk conf :dat) (setf (gethash ,(ct/path/key k) ,lft) ,(rec v))))
      (:% (awg (v*) `(let ((,v* ,(rec v)))
                       (smth? ,v* (setf (gethash ,(ct/path/key k) ,lft) ,v*)))))
      (:+ `(setf (gethash ,(ct/path/key k) ,lft) ,(rec v)))
      (:- `(remhash ,k ,lft))
      (otherwise (error "$: expected :?, :%, :+, :- mode, got: ~a" mode)))))

(defun compile/*add (rec conf mode lft v)
  (labels ((rec (x) (funcall rec conf x)))
    (case mode
      (:? `(when ,(gk conf :dat) (vex ,lft ,(rec v))))
      (:% (awg (v*) `(let ((,v* ,(rec v))) (smth? ,v* (vex ,lft ,v*)))))
      (:+ `(vex ,lft ,(rec v)))
      (otherwise (error "*: expected :?, :%, :+ mode, got: ~a" mode)))))

; PRE PROCESSORS

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

(defun prescan (qq &aux (isect (intersection (mapcar (λ (k) (kv (sym-not-kv k))) qq)
                                             *operators* :test #'equal)))
  (when isect (error "unexpected operator ~a~%in: ~a" isect qq))
  qq)
(defun pre/|| (qq) (unless qq (warn "||: missing args."))
  (loop for q in (prescan qq) collect
    (if (all? q) (kv q)
      (typecase q (cons q) (keyword `(** ,q)) (symbol `(*map ,q))
                  (string `(** ,q)) (number `(** ,(ct/kv/str q)))
                  (vector `(*map ,@(coerce q 'list)))
                  (otherwise (error "||: expected cons/symbol/vec/number. got: ~a" q))))))

(defun pre/$$ (q &optional (m :+)) (unless q (warn "$$: missing args."))
  (labels ; TODO: how to handle selecting only keys with -@?
    ((str- (a b c) `(,a ,(typecase b (keyword (sdwn (mkstr b))) (string b)
                           (otherwise (error "$$: expected string/:keyword. got: ~a" b)))
                        ,c))
     (repack- (o) (subseq `(,@o :_) 0 3))
     (repack-cons (ck k) (ecase (length k) (3 k) (2 `(,ck ,(caadr k) ,(cadadr k)))))
     (unpack- (o &aux (k (unpack-mode o m)) (ck (car k)))
       (apply #'str- (etypecase (second k)
                       (symbol (repack- k)) (string (repack- k))
                       (cons (repack-cons ck k))))))
    (let* ((q* (remove-if #'all? (prescan q)))
           (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

; OPERATOR COMPILERS

(defun compile/|| (rec conf d) ; (|| ...)
  (if (< (length d) 2) (funcall rec conf (car d))
      `(let ((∇ ,(gk conf :dat)))
         ,@(loop for op in d collect `(setf ∇ ,(funcall rec (dat/new conf '∇) op)))
        ∇)))

(defun compile/@ (rec conf d &aux (dat (gk conf :dat)))
  (case (length d) (0 `(@@ ,dat 0 nil))
                   (1 `(@@ ,(gk conf :dat) ,(funcall rec conf (car d))))
                   (2 `(@@ ,(gk conf :dat) ,@(funcall rec conf d)))
                   (3 `(@@ ,@(funcall rec conf d)))
                   (otherwise (error "@: expected 0-3 arguments. got: ~a" d))))

(defun pre/*map (q &optional (mm :+)) (unless q (warn "*map: missing args."))
  (labels ((unpack- (o) ; NOTE: can we use modes here?
             (dsb (m sk) (unpack-mode o mm)
               (unless (eq m :+) (error "*map: expected mode :+, got: ~a." m))
               (etypecase sk (sequence sk) (keyword sk) (symbol `(,sk :_))))))
    (let* ((q* (remove-if #'all? (prescan q)))
           (res (mapcar #'unpack- q*))
           (allres (if (= (length q) (length q*)) res (cons `(lit :_) res))))
      (if (< (length allres) 2) allres `((|| ,@allres))))))

(defun compile/*map (rec conf d) ; (*map ...)
  (when (zerop (length d)) (error "*map: missing args."))
  (awg (i ires itr par)
    (labels ((do-map (expr)
               `(loop with ,ires = (mav)
                  with ,par = (vec! ,(gk conf :dat))
                  for ,itr across ,par for ,i from 0
                  do (∈ (,par ,i ,itr)
                       (vex ,ires ,(funcall rec (dat/new conf itr) expr)))
                  finally (return ,ires))))
      (let ((cd (car d)))
        (etypecase cd (cons (do-map cd)) (vector (do-map cd)))))))

(defun compile/*fld (rec conf d) ; (*fld ...)
  (awg (i res itr par)           ; 0 + ; 0 acc (+ acc _)
    (labels ((do-fld (init acc itr expr)
               (unless (and (symbolp acc) (symbolp itr))
                       (error "*fld: expected symbols, got: ~a/~a" acc itr))
               (unless (consp expr) (error "*fld: expected cons, got: ~a" expr))
               `(loop with ,acc = ,init
                      with ,par = (vec! ,(gk conf :dat))
                      for ,itr across ,par for ,i from 0
                      do (∈ (,par ,i ,itr)
                           (setf ,acc ,(funcall rec (dat/new conf itr) expr)))
                      finally (return ,acc))))
      (case (length d) (0 (error "*fld: missing args."))
        (2 (etypecase (second d)
             (symbol (do-fld (car d) res itr `(,(second d) ,res ,itr)))
             (cons (do-fld (car d) res itr `(,(first (second d)) ,res ,@(cdr (second d)))))))
        (3 (let ((d3 (third d))) (do-fld (car d) (second d) itr d3)))
        (otherwise (error "*fld: bad args: ~a" d))))))

(defun compile/$$ (rec conf d) ; {...} ; sel
  (awg (kres par dat)
    `(let* ((,par ,(gk conf :dat))
            (,kres ,(if (car- all? d) `(make$ ,par) `(make$))))
       (∈ (,par) ; MOVE??
         ,@(loop for (m kk expr) in (strip-all d) collect
             `(let ((,dat (@@ ,par ,kk)))
                 (declare (ignorable ,dat))
                ,(compile/$add rec (dat/new conf dat) m kres kk expr))))
       ($nil ,kres))))

(defun compile/$* (rec conf d) ; #[...] ; sel
  (awg (i ires itr dat par)
    `(loop with ,ires of-type vector = (mav)
           with ,par of-type vector = (vec! ,(gk conf :dat))
           for ,itr across ,par for ,i from 0
           do (∈ (,par ,i ,itr)
                ,(when (car- all? d) (compile/*add rec conf :+ ires itr))
                ,@(loop for (m kk expr) in (strip-all d) collect
                    `(let ((,dat (@@ ,itr ,kk)))
                        (declare (ignorable ,dat))
                       ,(compile/*add rec (dat/new conf dat) m ires expr))))
           finally (return ,ires))))

(defun compile/*$ (rec conf d) ; #{...} ; sel
  (awg (i ires kvres itr dat par)
    `(loop with ,ires of-type vector = (mav)
           with ,par of-type vector = (vec! ,(gk conf :dat))
           for ,itr of-type hash-table across ,par for ,i from 0
           for ,kvres of-type hash-table = ,(if (car- all? d) `(make$ ,itr) `(make$))
           do (∈ (,par ,i ,itr)
                ,@(loop for (m kk expr) in (strip-all d)
                    collect `(let ((,dat (@@ ,itr ,kk)))
                                (declare (ignorable ,dat))
                               ,(compile/$add rec (dat/new conf dat) m kvres kk expr)))
                (vex ,ires ($nil ,kvres)))
           finally (return ,ires))))


(defun compile/*? (rec conf d &aux (cd (car d)) (sd (second d))) ; (*? test expr) ; filter, map
  (unless (< 0 (length d) 3) (error "*?: bad args: ~a" d))       ; REWRITE WITH XPR OR **?
  (awg (i ires itr dat par)
    `(loop with ,ires of-type vector = (mav)
       with ,par of-type vector = (vec! ,(gk conf :dat))
       for ,itr across ,par for ,i from 0
       for ,dat = ,(funcall rec (dat/new conf itr) (pre/xpr-sel cd itr)) ; HERE
       if ,dat do (∈ (,par ,i ,itr)
                    (vex ,ires ,(if (= (length d) 1) dat
                                    (funcall rec (dat/new conf dat) (pre/or-all sd)))))
       finally (return ,ires))))

(defun pre/** (q &optional (mm :?)) (unless q (warn "**: missing args."))
  (labels ((unpack- (o) (dsb (m sk) (unpack-mode o mm) `(,m ,(pre/xpr-sel sk :_)))))
    (let* ((q* (remove-if #'all? (prescan q)))
           (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

(defun get-modes (cd &rest mm)
  (loop for (m expr) in (strip-all cd) if (member m mm :test #'eq) collect expr))

(defun compile/** (rec conf d) ; [...] ; filter
  (awg (i ires itr par)
    `(loop with ,ires of-type vector = (mav)
           with ,par of-type vector = (vec! ,(gk conf :dat))
           for ,itr across ,par for ,i from 0
           do ,(compile/?xpr rec `((:par . ,par) (:dat . ,itr) (:i . ,i) (:itr . ,itr))
                 `(,@d (vex ,ires ,@(or (get-modes d :%) `(,itr))) nil))
           finally (return ,ires))))

(defun compile/?xpr/bool (rec conf cd)
  (labels ((rec (expr) (funcall rec conf expr)))
    (let ((ands (get-modes cd :+)) (nots (get-modes cd :-)) (ors (get-modes cd :?)))
      (cond ((and nots (null ands) (null ors)) (rec `(not (or ,@nots))))
            (t (rec `(and (or ,(car- all? cd) ,@ors ,@(and ands `((and ,@ands))))
                          ,@(when nots `((not (or ,@nots)))))))))))
(defun compile/?xpr (rec conf d) ; (xpr sel .. hit miss)
  (labels ((do-last (d n) (mapcar (λ (d) (funcall rec conf (pre/or-all d))) (last d n))))
    `(∈ (,(gk conf :par t) ,(gk conf :i t) ,(gk conf :itr t))
       ,(case (length d) ((0 (error "xpr: missing args")))
          (1 (compile/?xpr/bool rec conf (pre/** d)))
          (2 `(if ,(compile/?xpr/bool rec conf (pre/** (butlast d 1))) ,@(do-last d 1)))
          (otherwise `(if ,(compile/?xpr/bool rec conf
                             (pre/** (butlast d 2))) ,@(do-last d 2)))))))

(defun compile/?txpr (rec conf d) (funcall rec conf `(?mxpr ,d)))
(defun compile/?mxpr (rec conf d)
  (awg (f dat)
    `(let ((,dat ,(gk conf :dat)))
       (m/replfx (,dat ,f)
        ,@(loop with conf* = `((:dat . ,f) (:itr . ,dat) (:par . ,dat) ,@conf)
            for d* in d nconc `(,(funcall rec conf* `(?xpr ,@(butlast d*) t nil))
                                ,(funcall rec conf* (pre/or-all (car (last d*))))))))))

(defun compile/?srch (rec conf d)
  (unless d (error "?srch: missing args."))
  (awg (res) `(let ((,res (mav)))
                ,(if (> (length d) 1) (funcall rec conf
                                        `(?txpr ,@(butlast d) (vex ,res ,(first (last d)))))
                                      (funcall rec conf `(?txpr ,@d (vex ,res _))))
                ,res)))

(defun compile/?rec (rec conf d) ; (?rec (< (inum) 10) (+ _ 1))
  (unless (< 1 (length d) 4) (error "?rec: expected 2-3 arguments. got: ~a" d))
  (awg (i)
    (let ((d (lpad 3 d)))
      `(let ((∇ ,(gk conf :dat)) (,i 0))
         (∈ (,(gk conf :dat) ,i)
           (loop while ,(funcall rec (dat/new conf '∇) (second d))
                 ,@(loop for (s start then) in (car d)
                         nconc `(for ,s = ,start then ,then))
                 do (setf ∇ ,(funcall rec (dat/new conf '∇) (third d))
                          ,i (+ ,i 1))))
         (values ∇ ,i)))))

(defun proc-qry (q &optional conf*) "compile lqn query"
  (awg (dat fn fi)
  (labels
    ((rec (conf d)
       (cond
         ((all? d) (gk conf :dat))
         ((stringp d) d) ; this order is important
         ((vectorp d) (rec conf `(*map ,@(coerce d 'list))))
         ((atom d) d)
         ((qop? :||    d) (compile/||    #'rec conf (pre/|| (cdr d))))
         ((qop? :**    d) (compile/**    #'rec conf (pre/** (cdr d))))
         ((qop? :$*    d) (compile/$*    #'rec conf (pre/$$ (cdr d))))
         ((qop? :*$    d) (compile/*$    #'rec conf (pre/$$ (cdr d))))
         ((qop? :$$    d) (compile/$$    #'rec conf (pre/$$ (cdr d))))
         ((qop? :*map  d) (compile/*map  #'rec conf (pre/*map (cdr d))))
         ((qop? :?xpr  d) (compile/?xpr  #'rec conf (cdr d)))
         ((qop? :@     d) (compile/@     #'rec conf (cdr d)))
         ((qop? :*?    d) (compile/*?    #'rec conf (cdr d)))
         ((qop? :*fld  d) (compile/*fld  #'rec conf (cdr d)))
         ((qop? :?mxpr d) (compile/?mxpr #'rec conf (cdr d)))
         ((qop? :?txpr d) (compile/?txpr #'rec conf (cdr d)))
         ((qop? :?srch d) (compile/?srch #'rec conf (cdr d)))
         ((qop? :?rec  d) (compile/?rec  #'rec conf (cdr d)))
         ((car- lqnfx? d)    `(,(psymb 'lqn (car d)) ,@(rec conf (cdr d))))
         ((consp d) (cons (rec conf (car d)) (rec conf (cdr d))))
         (t (error "lqn: unexpected clause: ~a~%in: ~a" d q)))))
      `(λ (,dat ,fn ,fi) (//fxs/qry (,dat ,fn ,fi)
                               ,(rec `((:dat . ,dat) ,@conf*) q))))))

(defun qry/show (q cq)
  (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---------~%   ~s
██ END      ██████████████████████████~%" q cq))

(defmacro qryd (dat q &key db) "run lqn query on dat"
  (let ((cq (proc-qry q)))
    (when db (qry/show q cq))
    `(funcall ,cq ,dat ":internal:" 0)))
(defmacro qry (dat &rest rest) "query data. rest is wrapped in the pipe operator."
  `(qryd ,dat (|| ,@rest)))
(defmacro qrydb (dat &rest rest) "query data. rest is wrapped in the pipe operator."
  `(qryd ,dat (|| ,@rest) :db t))
(defun qryl (dat q &key db) "compile lqn query and run on dat"
  (eval `(qryd ,dat ,q :db ,db)))
(defmacro jsnqryf (fn q &key db) "run lqn query on json file, fn"
  `(qryd (jsnloadf ,fn) ,q :db ,db))

