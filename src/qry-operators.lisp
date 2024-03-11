(in-package :lqn)

(defun compile/|| (rec conf d) ; (|| ...) pipe
  (awg (∇-)
  (if (< (length d) 2) (funcall rec conf (car d))
     `(let ((,∇- ,(gk conf :dat)))
        ,@(loop for op in d collect `(setf ,∇- ,(funcall rec (dat/new conf ∇-) op)))
       ,∇-))))

(defun compile/@ (rec conf d &aux (dat (gk conf :dat)))
  (case (length d) (0 `(@@ ,dat 0 nil))
                   (1 `(@@ ,dat ,(funcall rec conf (car d))))
                   (2 `(@@ ,dat ,@(funcall rec conf d)))
                   (3 `(@@ ,@(funcall rec conf d)))
                   (otherwise (error "@: expected 0-3 arguments. got: ~a." d))))

(defun compile/?map (rec conf d) ; #(...) ; do this sequence of expressions on each item
  (when (zerop (length d)) (error "?map: missing args."))
  (awg (k i kres vres itr par)
  (let ((expr (funcall rec (dat/new conf itr) (car d))))
    (labels ((do-map ()
             `(let ((,par ,(gk conf :dat)))
                (labels
                  ((do-ht ()
                     (loop with ,kres = (new$) for ,i from 0
                       for ,itr being the hash-values of ,par using (hash-key ,k)
                       do (∈ (:par ,par :cnt ,i :itr ,itr :key ,k)
                             (setf (gethash ,k ,kres)
                                   ,expr))
                       finally (return ,kres)))
                   (do-vec ()
                     (loop with ,vres = (mav) with ,par = (vec! ,par) for ,i from 0
                       for ,itr across ,par
                       do (∈ (:par ,par :cnt ,i :itr ,itr :key ,i)
                             (vex ,vres ,expr))
                       finally (return ,vres))))
                  (typecase ,par
                    (null nil) (hash-table (do-ht)) (list (do-vec))
                    (vector (do-vec)) (simple-vector (do-vec))
                    (otherwise (error "RT: ?map: bad type. expected hash-table or vector:~%got: ~a." ,par)))))))
      (typecase expr
        (list (do-map)) (vector (do-map))
        (otherwise (error "?map: expected vector or cons. got: ~a." expr)))))))

(defun compile/?fld (rec conf d) ; (?fld ...)
  (awg (k i res itr par)         ; 0 + ; 0 acc (+ acc _)
  (labels ((do-fld (init acc itr expr)
             (unless (and (symbolp acc) (symbolp itr))
                     (error "?fld: expected symbols, got: ~a/~a." acc itr))
             (unless (consp expr) (error "?fld: expected cons or got: ~a." expr))
             `(let ((,par ,(gk conf :dat)) (,acc ,init))
                (labels ((do-ht ()
                           (loop for ,i from 0
                             for ,itr being the hash-values of ,par using (hash-key ,k)
                             do (∈ (:par ,par :cnt ,i :key ,k :itr ,itr)
                                   (setf ,acc ,(funcall rec (dat/new conf itr) expr)))))
                         (do-vec ()
                           (loop with ,par = (vec! ,par)
                             for ,itr across ,par for ,i from 0
                             do (∈ (:par ,par :cnt ,i :key ,i :itr ,itr)
                                   (setf ,acc ,(funcall rec (dat/new conf itr) expr))))))
                  (typecase ,par
                    (null ,acc) (hash-table (do-ht)) (list (do-vec))
                    (vector (do-vec)) (simple-vector (do-vec))
                    (otherwise (error "RT: ?fld: bad type. expected hash-table or vector:~%got: ~a." ,par))))
                ,acc)))
    (case (length d)
      (2 (etypecase (second d)
           (symbol (do-fld (car d) res itr `(,(second d) ,res ,itr)))
           (cons (do-fld (car d) res itr `(,(first (second d)) ,res ,@(cdr (second d)))))))
      (3 (do-fld (car d) (second d) itr (third d)))
      (otherwise (error "?fld: expected 2-3 args. got: ~a." d))))))


; TODO: is (key) env fx consistent?
(defun compile/?grp (rec conf d)
  (unless (< 0 (length d) 3) (error "?grp: expected 1 or 2 args. got: ~a." d))
  (awg (i k kvres key itr par acc)
  (labels ((do-dat () (case (length d) (1 itr) (2 (do-key (second d)))))
           (do-key (cd) (funcall rec (dat/new conf itr)
                          (typecase cd (keyword `(@ ,cd)) (string `(@ ,cd)) (otherwise cd)))))
    `(let ((,par ,(gk conf :dat)) (,kvres (make$)))
       (labels ((do-vec ()
                  (loop with ,par = (vec! ,par)
                        for ,i from 0 for ,itr across ,par
                        for ,key = (∈ (:par ,par :key ,i :cnt ,i) ,(do-key (car d)))
                        for ,acc = (gethash ,key ,kvres (new*))
                        do (∈ (:par ,par :cnt ,i :itr ,itr :key ,key)
                              (setf (gethash ,key ,kvres) (psh* ,acc ,(do-dat))))))
                (do-ht ()
                  (loop for ,i from 0
                        for ,itr being the hash-values of ,par using (hash-key ,k)
                        for ,key = (∈ (:par ,par :key ,k :cnt ,i) ,(do-key (car d)))
                        for ,acc = (gethash ,key ,kvres (new*))
                        do (∈ (:par ,par :cnt ,i :itr ,itr :key ,key)
                              (setf (gethash ,key ,kvres) (psh* ,acc ,(do-dat)))))))
         (typecase ,par
           (null nil) (hash-table (do-ht)) (list (do-vec))
           (vector (do-vec)) (simple-vector (do-vec))
           (otherwise (error "RT: ?grp bad type. expected hash-table or vector:~%got: ~a." ,par))))
       ,kvres))))

(defun compile/$$ (rec conf d) ; {...} ; sel ; select keys/exprs from ht to new ht
  (awg (kres par dat)
  `(let* ((,par ,(gk conf :dat)))
     (labels ((do-ht (&aux (,kres ,(if (car- dat? d) `(make$ ,par) `(make$))))
                (∈ (:par ,par)
                   ,@(loop for (m kk expr) in (strip-all d)
                       collect `(let ((,dat (@@ ,par ,kk)))
                                  (declare (ignorable ,dat))
                                  (∈ (:key ,kk)
                                     ,(compile/$add rec
                                        (dat/new conf dat) m kres kk expr))))
                   ($nil ,kres)))
              (do-vec () ; TODO: not implemented
                (error "vector not implemented")))
       (typecase ,par
         (null nil) (hash-table (do-ht)) (list (do-vec))
         (vector (do-vec)) (simple-vector (do-vec))
         (otherwise (error "RT: {..} bad type. expected hash-table or vector.~%got: ~a." ,par)))))))

; TODO: vec! does the wrong thing for non-sequence

; rename this to (?@ ) or something?
; change to select from vec of objects? it kinda is alread??
(defun compile/$* (rec conf d) ; #[...] ; sel ; select from vec of hts to vec
  (awg (i vres itr dat par)
  `(let ((,par ,(gk conf :dat)))
     (labels ((do-vec (&aux (,vres (mav)))
                (loop with ,par of-type vector = (vec! ,par)
                   for ,itr across ,par for ,i from 0
                   do (∈ (:par ,par :cnt ,i :itr ,itr)
                         ,(when (car- dat? d) (compile/*add rec conf :+ vres itr))
                         ,@(loop for (m kk expr) in (strip-all d) collect
                             `(let ((,dat (@@ ,itr ,kk)))
                                (declare (ignorable ,dat))
                                (∈ (:key ,kk) ,(compile/*add rec
                                                 (dat/new conf dat) m vres expr))))))
                ,vres))
       (typecase ,par ; TODO: support hts
        (null nil) (vector (do-vec)) (simple-vector (do-vec)) (list (do-vec))
        (otherwise (error "RT: #[..] bad type. expected vector, got: ~a." ,par)))))))

(defun compile/*$ (rec conf d) ; #{...} ; sel ; select from vec of hts to vec of hts
  (awg (i vres kvres itr dat par)
  `(let ((,par ,(gk conf :dat)))
     (labels ((do-vec (&aux (,vres (mav)))
                (loop with ,par of-type vector = (vec! ,par)
                   for ,itr of-type hash-table across ,par for ,i from 0
                   for ,kvres of-type hash-table = ,(if (car- dat? d) `(make$ ,itr) `(make$))
                   do (∈ (:par ,par :cnt ,i :itr ,itr)
                         ,@(loop for (m kk expr) in (strip-all d)
                             collect `(let ((,dat (@@ ,itr ,kk)))
                                        (declare (ignorable ,dat))
                                        (∈ (:key ,kk) ,(compile/$add rec
                                                         (dat/new conf dat) m kvres kk expr))))
                         (vex ,vres ($nil ,kvres))))
                ,vres))
       (typecase ,par (null nil)
         (vector (do-vec)) (simple-vector (do-vec)) (list (do-vec))
         (otherwise (error "RT: #{..} bad type. expected vector, got: ~a." ,par)))))))

(defun pre/?filter (q &optional (mm :?)) (unless q (warn "?filter: missing args."))
  (labels ((unpack- (o) (dsb (m sk) (unpack-mode o mm) `(,m ,(pre/xpr-sel sk :_)))))
    (let* ((q* (remove-if #'dat? (pre/scan-clauses q '#:?filter)))
           (res (mapcar #'unpack- q*)))
      (if (= (length q) (length q*)) res (cons :_ res)))))

(defun xpr/get-modes (cd &rest mm)
  (loop for (m expr) in (strip-all cd) if (member m mm :test #'eq) collect expr))

(defun compile/?filter (rec conf d) ; [..] ; sel ; filter object by these expressions
  (awg (k i kres vres itr par)
  `(let ((,par ,(gk conf :dat)))
     (labels
       ((do-vec () (loop with ,par = (vec! ,par) with ,vres of-type vector = (mav) for ,i from 0
                     for ,itr across ,par
                     do ,(compile/?xpr rec
                           `((:par . ,par) (:dat . ,itr) (:cnt . ,i) (:itr . ,itr) (:key . ,i))
                           `(,@d (vex ,vres ,@(or (xpr/get-modes d :%) `(,itr)))
                                 nil))
                     finally (return ,vres)))
        (do-ht () (loop with ,kres of-type hash-table = (new$) for ,i from 0
                    for ,itr being the hash-values of ,par using (hash-key ,k)
                    do ,(compile/?xpr rec
                          `((:par . ,par) (:dat . ,itr) (:cnt . ,i) (:itr . ,itr) (:key . ,k))
                          `(,@d (setf (gethash ,k ,kres)
                                      ,@(or (xpr/get-modes d :%) `(,itr)))
                                nil))
                    finally (return ,kres))))
       (typecase ,par
         (null nil) (hash-table (do-ht)) (list (do-vec))
         (vector (do-vec)) (simple-vector (do-vec))
         (otherwise (error "RT: ?filter bad type. expected hash-table or vector:~%got: ~a." ,par)))))))

(defun compile/?xpr/bool (rec conf cd)
  (labels ((rec (expr) (funcall rec conf expr)))
    (let ((ands (xpr/get-modes cd :+))
          (nots (xpr/get-modes cd :-))
          (ors (xpr/get-modes cd :?)))
      (cond ((and nots (null ands) (null ors)) (rec `(not (or ,@nots))))
            (t (rec `(and (or ,(car- dat? cd) ,@ors ,@(and ands `((and ,@ands))))
                          ,@(when nots `((not (or ,@nots)))))))))))
(defun compile/?xpr (rec conf d) ; (xpr sel .. hit miss)
  (labels ((do-last (d n) (mapcar (λ (d) (funcall rec conf (pre/or-all d))) (last d n))))
    `(∈ (:par ,(gk conf :par t) :cnt ,(gk conf :cnt t)
         :itr ,(gk conf :itr t) :key ,(gk conf :key t))
       ,(case (length d) ((0 (error "xpr: missing args.")))
          (1 (compile/?xpr/bool rec conf (pre/?filter d)))
          (2 `(if ,(compile/?xpr/bool rec conf (pre/?filter (butlast d 1))) ,@(do-last d 1)))
          (otherwise `(if ,(compile/?xpr/bool rec conf
                             (pre/?filter (butlast d 2))) ,@(do-last d 2)))))))

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
  (awg (res)
  `(let ((,res (mav)))
     ,(if (> (length d) 1) (funcall rec conf
                             `(?txpr ,@(butlast d) (vex ,res ,(first (last d)))))
        (funcall rec conf `(?txpr ,@d (vex ,res _))))
     ,res)))
(defun compile/?rec/vars (rec conf d)
  (labels ((rec (x) (funcall rec conf x)))
    (loop for o in d
      nconc (case (length o)
              (1 `(with ,(car o)))
              (2 `(for ,(car o) = ,(rec (second o)) then ,(rec (second o))))
              (3 `(for ,(car o) = ,(rec (second o)) then ,(rec (third o))))
              (otherwise (error "?rec: wanted 1-3 args, got: ~a." o))))))
(defun compile/?rec (rec conf d) ; (?rec (< (inum) 10) (+ _ 1))
  (unless (< 1 (length d) 4) (error "?rec: expected 2-3 arguments. got: ~a." d))
  (awg (i ∇-)
  (let ((d (lpad-lst 3 d)))
    `(let ((,∇- ,(gk conf :dat)) (,i 0))
       (∈ (:par ,(gk conf :dat) :cnt ,i :key ,i)
          (loop ,@(compile/?rec/vars rec conf (car d))
                while ,(funcall rec (dat/new conf ∇-) (second d))
                do (setf ,∇- ,(funcall rec (dat/new conf ∇-)
                                       (third d)) ,i (+ ,i 1))))
       (values ,∇- ,i)))))

(defun proc-qry (q &optional conf*) "compile lqn query"
  (awg (dat fn fi)
  (labels
    ((rec (conf d* &aux (d (pre/scan-clause d* nil)))
       (cond ((dat? d) (gk conf :dat))
             ((stringp d)    d) ; remember that this order is important
             ((vectorp d)    (rec conf `(?map ,@(coerce d 'list))))
             ((atom d)       d)
             ((qop? :||      d) (compile/||      #'rec conf (pre/||      (cdr d))))
             ((qop? :$*      d) (compile/$*      #'rec conf (pre/$$      (cdr d))))
             ((qop? :*$      d) (compile/*$      #'rec conf (pre/$$      (cdr d))))
             ((qop? :$$      d) (compile/$$      #'rec conf (pre/$$      (cdr d))))
             ((qop? :?map    d) (compile/?map    #'rec conf (pre/?map    (cdr d))))
             ((qop? :?filter d) (compile/?filter #'rec conf (pre/?filter (cdr d))))
             ((qop? :?xpr    d) (compile/?xpr    #'rec conf (cdr d)))
             ((qop? :@       d) (compile/@       #'rec conf (cdr d)))
             ((qop? :?fld    d) (compile/?fld    #'rec conf (cdr d)))
             ((qop? :?mxpr   d) (compile/?mxpr   #'rec conf (cdr d)))
             ((qop? :?txpr   d) (compile/?txpr   #'rec conf (cdr d)))
             ((qop? :?srch   d) (compile/?srch   #'rec conf (cdr d)))
             ((qop? :?rec    d) (compile/?rec    #'rec conf (cdr d)))
             ((qop? :?grp    d) (compile/?grp    #'rec conf (cdr d)))
             ((car- lqnfx?   d) `(,(psymb 'lqn (car d)) ,@(rec conf (cdr d))))
             ((consp d) (cons (rec conf (pre/scan-clause (car d))) (rec conf (cdr d))))
             (t (error "lqn: unexpected clause: ~a~%in: ~a." d q)))))
    `(λ (,dat ,fn ,fi) (q∈ (,dat ,fn ,fi)
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

; TODO: fix arguments/names in qryd, jsnqryf etc.
(defmacro qrydb (dat &rest rest) "query data. rest is wrapped in the pipe operator."
  `(qryd ,dat (|| ,@rest) :db t))
(defun qryl (dat q &key db) "compile lqn query and run on dat"
  (eval `(qryd ,dat ,q :db ,db)))
(defmacro jsnqryf (fn q &key db) "run lqn query on json file, fn"
  `(qryd (jsnloadf ,fn) ,q :db ,db))

