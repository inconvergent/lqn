(in-package :lqn)

(defun opstr (op &optional (fx #'identity))
  (prtcomp (format nil "██ op: ~s ██" (funcall fx op))))
(defun unpack-mode (o &optional (default :+) merciful)
  (labels ((valid-mode (m) (member m *qmodes* :test #'eq))
           (repack- (s s*) (etypecase s (symbol (psymb (symbol-package s) (subseq s* 2)))
                                        (string (subseq s* 2))))
           (unpack-cons (cns) (if (valid-mode (car cns)) cns
                                  (dsb (m s) (unpack- (car cns)) `(,m (,s ,@(cdr cns))))))
           (unpack- (s &aux (s* (mkstr s)) (sx (subx? s* "@")))
             (if (and sx (= sx 1)) (let ((m (kw (subseq s* 0 1)))) ; nil -> :nil
                                     (if (or merciful (valid-mode m)) (list m (repack- s s*))
                                       (error "lqn: invalid mode in: ~a" s)))
                                   (list default s))))
    (typecase o (symbol (unpack- o))
                (string (unpack- o))
                (cons (unpack-cons o))
                (vector `(,default ,o))
                (atom `(default ,o))
      (otherwise (error "lqn: bad thing to have mode: ~a" o)))))

(defun ct/path/key (pp) (first (last (str-split pp "/"))))
(defun dat/new (conf dat) `((:dat . ,dat) ,@conf))
(defun strip-all (d) (declare (list d)) (if (car- dat? d) (cdr d) d))

; CONTEXTS ; envs

(defmacro ██q∈ ((dat fn fi) &body body)
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
                (itr () (wrn "no (itr) in this scope."))
                (key () (wrn "no (key) in this scope."))
                (inum () (wrn "no (inum) in this scope.")))
         ,@body)))))

(defmacro ██∈ ((&key par cnt itr key) &body body)
  (declare (symbol par cnt itr))
  `(labels (,@(when par `((par () ,par) (pnum () (size? ,par))))
            ,@(when itr `((itr () ,itr) (inum () (size? ,itr))))
            ,@(when cnt `((cnt (&optional (k 0)) (+ ,cnt k))))
            ,@(when key `((key () ,key))))
     ,@body))

; PRE PROCESSORS

(defun when-equal (a b) (when (equal a b) a))
(defun re-sym (s &optional (n 2)) (psymb (symbol-package s) (subseq (str! s) n)))
(defun compile/_@ (s) (etypecase s (symbol `(,(re-sym s) :_))))
(defun compile/s@ (s)
  (etypecase s (keyword (subseq (sdwn (str! s)) 2))
               (symbol `(str! ,(re-sym s)))
               (cons `(str! (,(re-sym (car s)) ,@(cdr s))))))

(defun pre/or-all (d) (etypecase d (boolean d) (cons d) (keyword d) (sequence d)
                        (symbol (if (dat? d) :_ `(,d :_)))))
(defun pre/xpr-sel (ty k) (declare (symbol k))
  (etypecase ty (number `(when-equal ,k ,ty))
                (keyword `(and (str? ,k) (isub? ,k ,(ct/kw/str ty))))
                (string `(and (str? ,k) (sub? ,k ,ty)))
                (symbol `(when (,ty ,k) ,k)) ; TODO: attempt to parse as float, int?
                (cons ty) (boolean `(when-equal ,ty ,k))))

(defun pre/scan-clause (q &optional (full t))
  ; use full=nil to buypass checks that will trip up in proc-qry recursion
  (labels ((s@? (s) (custom-modifier? "S@" s))
           (_@? (s) (custom-modifier? "_@" s))
           (do-sym (s) (cond ((s@? s) (compile/s@ s)) ((_@? s) (compile/_@ s))
                             ((eq (kw s) :∅) nil)     (t s)))
           (do-cons (s) (cond ((not full) s) ((car- s@? s)  (compile/s@ s)) (t s))))
   (typecase q (cons (do-cons q)) (symbol (do-sym q)) (otherwise q))))

(defun pre/scan-clauses (qq &optional (ctx "pre-compile") ign)
  (declare (list qq))
  (let ((isect (intersection (mapcar (λ (k) (kw (ssym? k))) qq)
                             (set-difference *operators* ign)
                             :test #'equal)))
    (when isect (error "~a: unexpected bare operator(s) ~a~%in: ~a."
                       ctx isect qq)))
  (loop for q in qq collect (pre/scan-clause q)))

(defun pre/?pipe (qq) (unless qq (warn "?pipe: missing args.")) ; pipe
  (loop for q in (pre/scan-clauses qq '#:?pipe '(:@)) collect
    (if (dat? q) (kw q)
      (typecase q (cons q) (boolean q)
                  (keyword `(?filter ,q)) (string `(?filter ,q))
                  (symbol `(?map ,q)) (vector `(?map ,@(coerce q 'list)))
                  (otherwise q)))))

(defun pre/?map (q &optional (mm :+)) (unless q (warn "?map: missing args."))
  (labels ((do-symbol (sk) (case sk ('@ '(@ 0)) (otherwise `(,sk :_))))
           (unpack- (o) ; NOTE: can we use modes here?
             (dsb (m sk) (unpack-mode o mm)
               (unless (eq m :+) (error "?map: expected mode :+, got: ~a. in: ~a" m q))
               (etypecase sk (sequence sk)
                             (keyword sk)
                             (symbol (do-symbol sk))))))
    (let* ((q* (remove-if #'dat? (pre/scan-clauses q '#:?map '(:@))))
           (res (mapcar #'unpack- q*))
           (allres (if (= (length q) (length q*)) res (cons `(lit :_) res))))
      (if (< (length allres) 2) allres `((?pipe ,@allres))))))

(defun pre/?select (q &optional (m :+)) (unless q (warn "?select: missing args."))
  (labels ; TODO: how to handle selecting only keys with -@?
    ((tx- (a b c)
      `(,a ,(typecase b (keyword (sdwn (mkstr b))) (string b)
              (symbol (when (symbol-package b)
                            (error "?select: got intered symbol. use #:~a or :~a instead" b b))
                      (sdwn (mkstr b)))
              (otherwise (error "?select: expected string/:keyword/uninterned symbol. got: ~a." b)))
           ,(typecase c (keyword c) (boolean c)
                        (symbol (if (dat? c) :_ `(,c :_))) ; TODO: fix: #{(:aa #:aa)}
                        (otherwise c))))
     (repack- (o) (subseq `(,@o :_) 0 3))
     (repack-cons (ck k) (ecase (length k) (3 k) (2 `(,ck ,(caadr k) ,(cadadr k)))))
     (unpack- (o &aux (k (unpack-mode o m)) (ck (car k)))
       (apply #'tx- (etypecase (second k)
                      (symbol (repack- k)) (string (repack- k))
                      (cons (repack-cons ck k))))))
    (let* ((q* (remove-if #'dat? (pre/scan-clauses q '#:?select)))
           (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

; SELECTOR ADDERS

(defun compile/$add (rec conf mode lft k v)
  (labels ((rec (x) (funcall rec conf x)))
    (case mode
      (:? `(when ,(gk conf :dat) (setf (gethash ,(ct/path/key k) ,lft) ,(rec v))))
      ; NOTE: remember that there was a bug here for _ -@ with %@
      (:% (awg (v*) `(let ((,v* ,(rec v)))
                       (if (is? ,v*) (setf (gethash ,(ct/path/key k) ,lft) ,v*)
                                     (remhash ,(ct/path/key k) ,lft)))))
      (:+ `(setf (gethash ,(ct/path/key k) ,lft) ,(rec v)))
      (:- `(remhash ,k ,lft))
      (otherwise (error "$: expected :?, :%, :+, :- mode, got: ~a." mode)))))

(defun compile/*add (rec conf mode lft v)
  (labels ((rec (x) (funcall rec conf x)))
    (case mode
      (:? `(when ,(gk conf :dat) (vex ,lft ,(rec v))))
      (:% (awg (v*) `(let ((,v* ,(rec v))) (smth? ,v* (vex ,lft ,v*)))))
      (:+ `(vex ,lft ,(rec v)))
      (otherwise (error "*: expected :?, :%, :+ mode, got: ~a." mode)))))

