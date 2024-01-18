(in-package :lqn)

(defun unpack-mode (o &optional (default :+) merciful)
  (labels ((valid-mode (m) (member m *qmodes* :test #'eq))
           (repack- (s s*) (etypecase s (symbol (psymb (symbol-package s) (subseq s* 2)))
                                        (string (subseq s* 2))))
           (unpack-cons (cns) (if (valid-mode (car cns)) cns
                                  (dsb (m s) (unpack- (car cns)) `(,m (,s ,@(cdr cns))))))
           (unpack- (s &aux (s* (mkstr s)) (sx (subx? s* "@")))
             (if (and sx (= sx 1)) (let ((m (kv (subseq s* 0 1)))) ; nil -> :nil
                                     (if (or merciful (valid-mode m)) (list m (repack- s s*))
                                       (error "lqn: invalid mode in: ~a" s)))
                                   (list default s))))
    (typecase o (symbol (unpack- o)) (string (unpack- o))
                (cons (unpack-cons o)) (vector `(,default ,o))
      (otherwise (error "lqn: bad mode thing to have mode: ~a" o)))))


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
                (err (&optional a) (error "qry err: ~a." a))
                (wrn (&optional a) (warn "qry wrn: ~a." a))
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
      (:+ `(setf (gethash ,(ct/path/key k) ,lft)
                 ,(rec v)
                 ))
      (:- `(remhash ,k ,lft))
      (otherwise (error "$: expected :?, :%, :+, :- mode, got: ~a." mode)))))

(defun compile/*add (rec conf mode lft v)
  (labels ((rec (x) (funcall rec conf x)))
    (case mode
      (:? `(when ,(gk conf :dat) (vex ,lft ,(rec v))))
      (:% (awg (v*) `(let ((,v* ,(rec v))) (smth? ,v* (vex ,lft ,v*)))))
      (:+ `(vex ,lft ,(rec v)))
      (otherwise (error "*: expected :?, :%, :+ mode, got: ~a." mode)))))

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
  (when isect (error "unexpected operator ~a~%in: ~a." isect qq))
  qq)
(defun pre/|| (qq) (unless qq (warn "||: missing args."))
  (loop for q in (prescan qq) collect
    (if (all? q) (kv q)
      (typecase q (cons q) (keyword `(** ,q))
                ; (boolean q)
                (symbol `(*map ,q))
                  (string `(** ,q)) (number `(** ,(ct/kv/str q)))
                  (vector `(*map ,@(coerce q 'list)))
                  (otherwise (error "||: expected cons/symbol/vec/number. got: ~a." q))))))

(defun pre/*map (q &optional (mm :+)) (unless q (warn "*map: missing args."))
  (labels ((unpack- (o) ; NOTE: can we use modes here?
             (dsb (m sk) (unpack-mode o mm)
               (unless (eq m :+) (error "*map: expected mode :+, got: ~a." m))
               (etypecase sk (sequence sk) (keyword sk) (symbol `(,sk :_))))))
    (let* ((q* (remove-if #'all? (prescan q)))
           (res (mapcar #'unpack- q*))
           (allres (if (= (length q) (length q*)) res (cons `(lit :_) res))))
      (if (< (length allres) 2) allres `((|| ,@allres))))))

(defun pre/$$ (q &optional (m :+)) (unless q (warn "$$: missing args."))
  (labels ; TODO: how to handle selecting only keys with -@?
    ((tx- (a b c)
      `(,a ,(typecase b (keyword (sdwn (mkstr b))) (string b)
              (symbol (when (symbol-package b)
                            (error "$$: got intered symbol. use #:~a or :~a instead" b b))
                      (sdwn (mkstr b)))
              (otherwise (error "$$: expected string/:keyword/uninterned symbol. got: ~a." b)))
           ,(typecase c (keyword c) (boolean c)
                        (symbol (if (all? c) :_ `(,c :_))) ; TODO: fix: #{(:aa #:aa)}
                        (otherwise c))))
     (repack- (o) (subseq `(,@o :_) 0 3))
     (repack-cons (ck k) (ecase (length k) (3 k) (2 `(,ck ,(caadr k) ,(cadadr k)))))
     (unpack- (o &aux (k (unpack-mode o m)) (ck (car k)))
       (apply #'tx- (etypecase (second k)
                      (symbol (repack- k)) (string (repack- k))
                      (cons (repack-cons ck k))))))
    (let* ((q* (remove-if #'all? (prescan q)))
           (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

; OPERATOR COMPILERS
