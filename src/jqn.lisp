(in-package :jqn)

(defun jqn/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---
   ~s
██ ██████████████████████████~%" q compiled))

(defun jsnloads (&optional (s *standard-input*))
  "parse json from stream; or *standard-input*"
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (yason:parse s)))

(defun jsnloadf (fn)
  (declare (string fn)) "parse json from file, fn"
  (with-open-file (f fn :direction :input) (jsnloads f)))

(defun jsnout (o &key (s *standard-output*) indent)
  (declare (stream s) (boolean indent))
  "stream encoded json from o to s; or *standard-output*"
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*list-encoder* 'yason:encode-alist))
    (yason:encode o (yason:make-json-output-stream s :indent indent))))

(defun jsnout* (o &key indent (s (make-string-output-stream)))
  (declare (boolean indent)) "serialize o as json string"
  (jsnout o :s s :indent indent)
  (get-output-stream-string s))

(defun ldn-serialize (o &optional (kvkeys t))
  (etypecase o (string o)
               (hash-table (loop for k being the hash-keys of o using (hash-value v)
                                 collect `(,(kv k) . ,(ldn-serialize v))))
               (vector (loop with res = (make-adjustable-vector)
                             for v across o do (vextend (ldn-serialize v) res)
                             finally (return res)))
               (cons (loop for (k . v) in o
                           collect `(,(kv k) . ,(ldn-serialize v))))
               (atom o)))



(defun compile/itr/preproc (q)
  (labels
    ((unpack-cons (k &aux (ck (car k)))
       (declare (list k))
       (case (length k)
         (0 (warn "empty selector"))
         (1 `(,@(unpack-mode ck *qmodes*) :_))             ; ?/m [m]@key _
         (2 `(,@(unpack-mode ck *qmodes*) ,(second k)))    ; ?/m [m]@key expr
         (3 `(,ck ,(ensure-string (second k)) ,(third k))) ; m       key expr
         (otherwise (warn "bad # items in selector: ~a" k))))
     (unpack (k)
       (typecase k
         (symbol `(,@(unpack-mode k *qmodes*) :_))
         (string `(,@(unpack-mode k *qmodes*) :_))
         (cons   (unpack-cons k))
         (otherwise (error "selector should be symbol, string or list. got: ~a" k)))))
    (mapcar #'unpack q)))

; TODO: handle (*)/(&) as _
; TODO: default to "(& key)" for "key"

 ; this is hacky, and will break if we add negation
(defmacro expr-all-shortcut (d dat body)
  `(if (all? (cadr ,d)) ,dat ,body))

(defun proc-qry (dat q)
  "compile jqn query"
  (labels
    ((compile/expr/rec (kk o expr)
       (cond ((all? expr) `(jqn:@ ,o ,kk))
             ((atom expr) expr)
             ((car-itr? expr) (rec `(jqn:@ ,o ,kk) expr))
             ((car-get? expr)
              `(jqn:@ ,o ,(ensure-string (second expr)) ,@(cddr expr)))
             ((consp expr) (cons (compile/expr/rec kk o (car expr))
                                 (compile/expr/rec kk o (cdr expr))))
             (t (warn "unexpected expr in selector: ~a~%expr: ~a" k expr))))
     (psh (mode kk expr)
       (case mode (:? 'apsh?) (:+ 'apsh+)
         (otherwise (error "unexpected mode in selector: ~a ~a~%expr: ~a"
                           mode kk expr))))
     (do-op-body (kvres dat d)
       (loop for (mode kk expr) in (reverse d)
             collect `(,(psh mode kk expr) ,kvres ,kk
                       ,(compile/expr/rec (ensure-string kk) dat expr))))
     (compile/kv (dat d)
       (awg (kvres) `(let ((,kvres (list))) ,@(do-op-body kvres dat d) ,kvres)))
     (compile/itr (dat d)
       (awg (kvres itrlst o)
         `(loop with ,itrlst = (mav) for ,kvres = (list)
                for ,o across (ensure-vector ,dat)
                do (progn ,@(do-op-body kvres o d) (vextend ,kvres ,itrlst))
                finally (return ,itrlst))))
     (rec (dat d)
       (cond ((all? d) dat) ((atom d) d)
             ((car-kv? d)  (expr-all-shortcut d dat
                             (compile/kv  dat (compile/itr/preproc (cdr d)))))
             ((car-itr? d) (expr-all-shortcut d dat
                             (compile/itr dat (compile/itr/preproc (cdr d)))))
             (t (error "compile error for: ~a ~a" dat d)))))

    (rec dat q)))

(defmacro qryd (dat &key (q :_) db)
  (declare (boolean db)) "run jqn query on dat"
  (awg (dat*) (let ((compiled (proc-qry dat* q)))
                (when db (jqn/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))

(defmacro qryf (fn &key (q :_) db)
  (declare (boolean db)) "run jqn query on file, fn"
  `(qryd (jsnloadf ,fn) :q ,q :db ,db))

(defun qryl (dat &key (q :_) db)
  "compile jqn query and run on dat"
  (eval `(qryd ,dat :q ,q :db ,db)))

