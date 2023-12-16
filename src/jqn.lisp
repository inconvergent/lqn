(in-package :jqn)

; QRY RUNTIME

(defun sup (&rest rest) "mkstr and upcase" (string-upcase (apply #'mkstr rest)))
(defun sdwn (&rest rest) "mkstr and downcase" (string-downcase (apply #'mkstr rest)))

(defmacro @ (o k &optional default)
  "get k from dict o; or default"
  (if default `(gethash ,k (nil-as-empty-ht ,o) ,default)
              `(gethash ,k (nil-as-empty-ht ,o))))
(defmacro ind (o sel) ; rename
  "get index or range from json array (vector).
if sel is an atom: (aref o ,sel)
if sel is cons: (subseq o ,@sel)"
  (typecase sel (cons `(subseq o ,@sel))
                (atom `(aref ,o ,sel))
                (otherwise (error "ind: wanted atom or (atom atom). got: ~a" sel))))

(defmacro something? (v &body body) ; ??
  (declare (symbol v))
  `(typecase ,v (vector (when (> (length ,v) 0) (progn ,@body)))
                (hash-table (when (> (hash-table-count ,v) 0) (progn ,@body)))
                (otherwise (when ,v (progn ,@body)))))
(defmacro maybe (fx arg &rest args) ; ?!
  (declare (symbol fx)) "run (fx arg) only if arg is not nil.
qry abbrev ??."
  (awg (arg*) `(let ((,arg* ,arg))
                 (if (null ,arg*) nil (,fx ,arg* ,@args)))))
(abbrev ?? maybe)

(defun squeeze (&rest rest &aux (res (make-adjustable-vector)))
  (labels ((do-arg (aa) (loop for a across aa
                            do (loop for b across a do (vextend b res)))))
    (loop for a in rest do (do-arg a)))
  res)
(abbrev <> squeeze)

; (defmacro pipe (&body body))
; (abbrev || pipe)

(defmacro nilop (&rest rest) (declare (ignore rest)) "do nothing. return nil" nil)

(defmacro kvadd+ (dat lft k v &optional default)
  (declare (ignore dat) (symbol lft)) "do (setf lft (or v default))"
  `(setf (gethash ,k ,lft) (or ,v ,default)))
(defmacro kvadd? (dat lft k v)
  (declare (symbol lft)) "do (setf lft v) if (gethash k dat) is not nil"
  `(when (gethash ,k ,dat) (setf (gethash ,k ,lft) ,v)))
(defmacro kvadd% (dat lft k v)
  (declare (ignore dat) (symbol lft)) "do (setf lft v) if v is not nil"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (setf (gethash ,k ,lft) ,v*)))))
(defmacro kvdel (dat lft k v)
  (declare (ignore dat v) (symbol lft)) "delete key"
  `(remhash ,k ,lft))

(defmacro vvadd+ (dat lft k v &optional default)
  (declare (ignore dat k) (symbol lft)) "do (vextend (or v default) lft)"
  `(vextend (or ,v ,default) ,lft))
(defmacro vvadd? (dat lft k v)
  (declare (symbol lft)) "do (vextend v lft) if (gethash k dat) is not nil"
  `(when (gethash ,k ,dat) (vextend ,v ,lft)))
(defmacro vvadd% (dat lft k v)
  (declare (ignore dat k) (symbol lft)) "do (vextend v lft) if v is not nil or empty"
  (awg (v*) `(let ((,v* ,v)) (something? ,v* (vextend ,v* ,lft)))))

; list/vector: remove if not someting
; hash-table: remove key if value not something
(defun condense (o)
  "remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`.
qry abbrev ><. "
  (typecase o
    (sequence (remove-if-not (lambda (o*) (something? o* t)) o))
    (hash-table (loop with keys = (list)
                      for k being the hash-keys of o using (hash-value v)
                      do (unless (progn
                                   (print (list k v))
                                   (something? v t)) (push k keys))
                      finally (loop for k in keys do (remhash k o)))
                o)
    (otherwise (warn "condense/>< works on sequence (json array) or hash-table (json object).
got: ~a.
did nothing." o))))
(abbrev >< condense)

(defun nil-as-empty-ht (v)
  (if (not v) (make-hash-table :test #'equal) v))
(defun copy-ht (ht &aux (res (make-hash-table :test #'equal)))
  (declare (hash-table ht)) "soft copy ht"
  (loop for k being the hash-keys of ht using (hash-value v)
        do (setf (gethash k res) (gethash k ht))
        finally (return res)))
(defun new-ht () "new hash table" (make-hash-table :test #'equal))
(defun kvnil (kv)
  "return nil for emtpy hash-tables. otherwise return kv"
  (typecase kv (hash-table (if (> (hash-table-count kv) 0) kv nil))
               (otherwise kv)))

; COMPILER

(defun kvadd (mode)
  (ecase mode (:+ 'kvadd+) (:? 'kvadd?) (:% 'kvadd%) (:- 'kvdel)))
(defun vvadd (mode)
  (ecase mode (:+ 'vvadd+) (:? 'vvadd?) (:% 'vvadd%) (:- 'nilop)))

(defun new-conf (conf dat kk) `((:dat . (@ ,dat ,kk)) ,@conf))
(defun strip-all (d) (declare (list d)) (if (car-all? d) (cdr d) d))

(defun compile/itr/preproc (q)
  (labels
    ((stringify (a)
      (handler-case
        (ensure-string a)
        (error (e) (error "failed to stringify key: ~a.~%err: ~a" a e))))
     (stringify-key (v) (dsb (a b c) v `(,a ,(stringify b) ,c)))
     (unpack-cons (k &aux (ck (car k)))
       (declare (list k))
       (case (length k)
         (0 (warn "empty selector"))
         (1 `(,@(unpack-mode ck *qmodes*) :_))          ; ?/m [m]@key _
         (2 `(,@(unpack-mode ck *qmodes*) ,(second k))) ; ?/m [m]@key expr
         (3 `(,ck ,(stringify (second k)) ,(third k)))  ; m       key expr
         (otherwise (warn "bad # items in selector: ~a" k))))
     (unpack (k)
       (typecase k
         (symbol `(,@(unpack-mode k *qmodes*) :_))
         (string `(,@(unpack-mode k *qmodes*) :_))
         (cons   (unpack-cons k))
         (otherwise (error "selector should be symbol, string or list. got: ~a" k)))))
    (let* ((q* (remove-if #'all? q))
           (res (mapcar #'stringify-key (mapcar #'unpack q*))))
      (if (not (= (length q) (length q*)))
          (cons :_ res) res))))

; TODO: interpret expr => empty dict/vec as nil and drop in %mode

(defun proc-qry (conf* q)
  "compile jqn query"
  (labels
    ((*itr/labels (vv dat i)
       `((i (&optional (k 0)) (+ ,i k))
         (num () (length ,vv)) (dat () ,dat) (par () ,vv)
         (@dat (k &optional default) (@ (dat) k default))))
     (compile/$itr (conf d)
       (awg (kres dat)
         `(let* ((,dat ,(gk conf :dat))
                 (,kres ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))))
            (labels ((dat () ,dat)
                     (@dat (k &optional default) (@ (dat) k default)))
             ,@(loop for (mode kk expr) in (strip-all d)
                     collect `(,(kvadd mode) ,dat ,kres ,kk
                               ,(rec (new-conf conf dat kk) expr))))
            (kvnil ,kres))))
     (compile/*itr (conf d)
       (awg (ires dat i vv)
         `(loop with ,ires = (mav)
                with ,vv = (ensure-vector ,(gk conf :dat))
                for ,dat across ,vv for ,i from 0
                do (labels (,@(*itr/labels vv dat i))
                     ,(when (car-all? d) `(vvadd+ nil ,ires nil ,dat))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             collect `(,(vvadd mode) ,dat ,ires ,kk
                                       ,(rec (new-conf conf dat kk) expr))))
                finally (return ,ires))))
     (compile/*$itr (conf d)
       (awg (ires kres dat i vv)
         `(loop with ,ires = (mav)
                with ,vv = (ensure-vector ,(gk conf :dat))
                for ,i from 0 for ,dat across ,vv
                for ,kres = ,(if (car-all? d) `(copy-ht ,dat) `(new-ht))
                do (labels (,@(*itr/labels vv dat i))
                     ,@(loop for (mode kk expr) in (strip-all d)
                             for comp-expr = (rec (new-conf conf dat kk) expr)
                             collect `(,(kvadd mode) ,dat ,kres ,kk ,comp-expr))
                     (vextend (kvnil ,kres) ,ires))
                finally (return ,ires))))
     (compile/pipe (conf d &aux (pipe (gensym "PIPE")))
      `(let ((,pipe ,(gk conf :dat)))
         ,@(loop for op in d
                 collect `(setf ,pipe ,(rec `((:dat . ,pipe) ,@conf) op)))
         ,pipe))

     ; TODO: incomplete
     ; TODO: what happens with selector inside *new/$new?
     (compile/*new (conf d) ; ignores _, simpler expr pre processor
       (awg (ires)
         `(let ((,ires (mav))) ;
            ,@(loop for (mode kk expr) in (strip-all d)
                    collect `(,(vvadd mode) nil ,ires ,kk
                              ,(rec conf expr)))
            ,ires)))
     (compile/$new (conf d) ; ignores _
       (awg (kres dat)
         `(let* ((,dat ,(gk conf :dat))
                 (,kres (new-ht)))
            ,@(loop for (mode kk expr) in (strip-all d)
                    collect `(,(kvadd mode) ,dat ,kres ,kk
                              ,(rec conf expr)))
            (kvnil ,kres))))

     (rec (conf d &aux (dat (gk conf :dat)))
       (cond ((all? d) dat) ((atom d) d)
             ((car-*$itr? d) (compile/*$itr conf (compile/itr/preproc (cdr d))))
             ((car-$itr? d)  (compile/$itr conf (compile/itr/preproc (cdr d))))
             ((car-*itr? d)  (compile/*itr conf (compile/itr/preproc (cdr d))))
             ((car-*new? d)  (compile/*new conf (compile/itr/preproc (cdr d))))
             ((car-$new? d)  (compile/$new conf (compile/itr/preproc (cdr d))))
             ((car-pipe? d)  (compile/pipe conf (cdr d)))
             ((car-jqnfx? d) `(,(psymb 'jqn (car d))
                               ,@(rec conf (cdr d))))
             ((consp d) (cons (rec conf (car d))
                              (rec conf (cdr d))))
             (t (error "jqn compile error for: ~a" d)))))
    `(labels ((fn () ,(gk conf* :fn t))
              (fi (&optional (k 0)) (+ k ,(or (gk conf* :fi t))))
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

