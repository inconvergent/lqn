(in-package :lqn)

(defun ct/path/key (pp) (first (last (str-split pp "/"))))
(defun dat/new (conf dat) `((:dat . ,dat) ,@conf))
(defun strip-all (d) (declare (list d)) (if (car- all? d) (cdr d) d))

; CONTEXTS

(defmacro //fxs/qry (conf &body body &aux (meta (gensym "META")))
  (awg (nope)
    `(let ((,meta (make-hash-table :test #'eq)))
     (block ,nope
       (labels ((entry () ,(gk conf :entry t))
                (fn () ,(gk conf :fn t))
                (fi (&optional (k 0)) (+ k ,(or (gk conf :fi t) 0)))
                (ghv (k &optional d) (declare (keyword k)) (gethash k ,meta d))
                (hld (k &optional v) (declare (keyword k))
                  (if v (setf (gethash k ,meta) v) (remhash k ,meta)) v)
                (cnt (&optional (k 0)) (+ k ,(or (gk conf :fi t) 0)))
                (nope (&optional a) (return-from ,nope a))
                (err (&optional a) (error "qry err: ~a" a))
                (wrn (&optional a) (warn "qry wrn: ~a" a))
                (par () ,(gk conf :dat))
                (pnum (&optional d) (size? (par) d))
                (itr () (wrn "no (itr) in qry scope."))
                (inum () (wrn "no (inum) in qry scope."))
                )
         ,@body)))))

(defmacro //fxs/op/ ((par &optional i itr) &body body)
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

; OPERATOR COMPILERS

(defun compile/|| (rec conf d) ; (|| ...)
  (if (< (length d) 2) (funcall rec conf (car d))
      `(let ((<> ,(gk conf :dat)))
         ,@(loop for op in d collect `(setf <> ,(funcall rec (dat/new conf '<>) op)))
         <>)))

(defun compile/@ (rec conf d)
  (case (length d) (1 `(@@ ,(gk conf :dat) ,(funcall rec conf (car d))))
                   (2 `(@@ ,(gk conf :dat) ,@(funcall rec conf d)))
                   (3 `(@@ ,@(funcall rec conf d)))))

(defun compile/*map (rec conf d) ; (*map ...)
  (awg (i ires itr par)
    (labels ((do-map (expr)
               `(loop with ,ires = (mav)
                  with ,par = (vec! ,(gk conf :dat))
                  for ,itr across ,par for ,i from 0
                  do (//fxs/op/ (,par ,i ,itr)
                       (vex ,ires ,(funcall rec (dat/new conf itr) expr)))
                  finally (return ,ires))))
      (case (length d) (0 (warn "*map: missing args."))
        (1 (let ((cd (car d)))
             (etypecase cd (string cd) (number cd) (keyword cd) (boolean cd)
                           (vector (funcall rec conf cd))
                           (symbol (do-map (if (all? cd) :_ `(,cd ,itr))))
                           (cons (do-map cd)))))
        (otherwise (compile/*map rec conf `((|| ,@d))))))))

(defun compile/*fld (rec conf d) ; (*fld ...)
  (awg (i res itr par) ; 0 + ; 0 acc (+ acc _)
    (labels ((do-fld (init acc itr expr)
               (unless (and (symbolp acc) (symbolp itr))
                       (error "*fld: expected symbols, got: ~a/~a" acc itr))
               (unless (consp expr) (error "*fld: expected cons, got: ~a" expr))
               `(loop with ,acc = ,init
                      with ,par = (vec! ,(gk conf :dat))
                      for ,itr across ,par for ,i from 0
                      do (//fxs/op/ (,par ,i ,itr)
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
            (,kres ,(if (car- all? d) `($make ,par) `($make))))
       (//fxs/op/ (,par) ; MOVE??
         ,@(loop for (m kk expr) in (strip-all d) collect
             `(let ((,dat ($rget ,par ,kk)))
                 (declare (ignorable ,dat))
                ,(compile/$add rec (dat/new conf dat) m kres kk expr))))
       ($nil ,kres))))

(defun compile/$* (rec conf d) ; #[...] ; sel
  (awg (i ires itr dat par)
    `(loop with ,ires of-type vector = (mav)
           with ,par of-type vector = (vec! ,(gk conf :dat))
           for ,itr across ,par for ,i from 0
           do (//fxs/op/ (,par ,i ,itr)
                ,(when (car- all? d) (compile/*add rec conf :+ ires itr))
                ,@(loop for (m kk expr) in (strip-all d) collect
                    `(let ((,dat ($rget ,itr ,kk)))
                        (declare (ignorable ,dat))
                       ,(compile/*add rec (dat/new conf dat) m ires expr))))
           finally (return ,ires))))

(defun compile/*$ (rec conf d) ; #{...} ; sel
  (awg (i ires kvres itr dat par)
    `(loop with ,ires of-type vector = (mav)
           with ,par of-type vector = (vec! ,(gk conf :dat))
           for ,itr of-type hash-table across ,par for ,i from 0
           for ,kvres of-type hash-table = ,(if (car- all? d) `($make ,itr) `($make))
           do (//fxs/op/ (,par ,i ,itr)
                ,@(loop for (m kk expr) in (strip-all d)
                    collect `(let ((,dat ($rget ,itr ,kk)))
                                (declare (ignorable ,dat))
                               ,(compile/$add rec (dat/new conf dat) m kvres kk expr)))
                (vex ,ires ($nil ,kvres)))
           finally (return ,ires))))

; REWRITE WITH XPR OR **?
(defun compile/*? (rec conf d &aux (cd (car d)) (sd (second d))) ; (*? test expr) ; filter, map
  (unless (< 0 (length d) 3) (error "*?: bad args: ~a" d))
  (awg (i ires itr dat par)
    `(loop with ,ires of-type vector = (mav)
       with ,par of-type vector = (vec! ,(gk conf :dat))
       for ,itr across ,par for ,i from 0
       for ,dat = ,(funcall rec (dat/new conf itr) (pre/xpr-sel cd itr)) ; HERE
       if ,dat do (//fxs/op/ (,par ,i ,itr)
                    (vex ,ires ,(if (= (length d) 1) dat
                                    (funcall rec (dat/new conf dat) (pre/or-all sd)))))
       finally (return ,ires))))

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

(defun compile/?xpr (rec conf d) ; (xpr sel .. hit miss)
  (labels ((do-last (d n) (mapcar (lambda (d) (funcall rec conf (pre/or-all d))) (last d n)))
           (build-bool (cd &aux (ands (get-modes cd :+)) (nots (get-modes cd :-)))
             (funcall rec conf
               `(and (or ,(car- all? cd) ,@(get-modes cd :?) ,@(and ands `((and ,@ands))))
                     ,@(when nots `((not (or ,@nots))))))))
    `(//fxs/op/ (,(gk conf :par t) ,(gk conf :i t) ,(gk conf :itr t))
       ,(case (length d) ((0 (error "xpr: missing args")))
          (1 (build-bool (pre/** d)))
          (2 `(if ,(build-bool (pre/** (butlast d 1))) ,@(do-last d 1)))
          (otherwise `(if ,(build-bool (pre/** (butlast d 2))) ,@(do-last d 2)))))))
(defun compile/?mxpr (rec conf d)
  (awg (f dat)
    `(let ((,dat ,(gk conf :dat)))
       (m/replfx (,dat ,f)
        ,@(loop with conf* = `((:dat . ,f) (:itr . ,dat) (:par . ,dat) ,@conf)
            for d* in d nconc `(,(funcall rec conf* `(?xpr ,@(butlast d*) t nil))
                                ,(funcall rec conf* (pre/or-all (car (last d*))))))))))
(defun compile/?txpr (rec conf d) (funcall rec conf `(?mxpr ,d)))
(defun compile/?tsrch (rec conf d)
  (unless d (error "?tsrch: missing args."))
  (awg (res) `(let ((,res (mav)))
                ,(if (> (length d) 1) (funcall rec conf
                                        `(?txpr ,@(butlast d) (vex ,res ,(first (last d)))))
                                      (funcall rec conf `(?txpr ,@d (vex ,res _))))
                ,res)))

(defun proc-qry (conf* q) "compile lqn query"
  (labels
    ((rec (conf d)
       (cond ((all? d) (gk conf :dat)) ((stringp d) d)
         ((vectorp d) (compile/*map #'rec conf (coerce d 'list))) ((atom d) d)
         ((optrig? :$$    d) (compile/$$    #'rec conf (pre/$$ (cdr d))))
         ((optrig? :$*    d) (compile/$*    #'rec conf (pre/$$ (cdr d))))
         ((optrig? :*$    d) (compile/*$    #'rec conf (pre/$$ (cdr d))))
         ((optrig? :**    d) (compile/**    #'rec conf (pre/** (cdr d))))
         ((optrig? :||    d) (compile/||    #'rec conf (pre/|| (cdr d))))
         ((optrig? :@     d) (compile/@     #'rec conf (cdr d)))
         ((optrig? :*?    d) (compile/*?    #'rec conf (cdr d)))
         ((optrig? :*fld  d) (compile/*fld  #'rec conf (cdr d)))
         ((optrig? :*map  d) (compile/*map  #'rec conf (cdr d)))
         ((optrig? :?mxpr d) (compile/?mxpr #'rec conf (cdr d)))
         ((optrig? :?txpr d) (compile/?txpr #'rec conf (cdr d)))
         ((optrig? :?xpr  d) (compile/?xpr  #'rec conf (cdr d)))
         ((optrig? :?tsrch d) (compile/?tsrch #'rec conf (cdr d)))
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

