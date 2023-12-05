(in-package :jqn)

(defun jqn/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---
   ~s
██ ██████████████████████████~%" q compiled))

(defun loadjsn (fn)
  (declare (string fn)) "load json from file fn"
  (with-open-file (f fn :direction :input)
    (let ((yason:*parse-json-arrays-as-vectors* t))
      (yason:parse f))))

(defun wrtjsn (o &key (s *standard-output*) indent)
  (declare (stream s) (boolean indent)) "encode o as json to stream, s"
  (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase)
        (yason:*list-encoder* 'yason:encode-alist))
    (yason:encode o (yason:make-json-output-stream s :indent indent))))

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
(defun ensure-string (s)
  (etypecase s (symbol (string-downcase (mkstr s)))
               (string s)))

(defmacro @ (o k &optional default)
  (if default `(gethash ,k ,o ,default) `(gethash ,k ,o)))



(defun proc-qry (dat q)
  "compile jqn query"
  (labels
    ((compile/expr/rec (kk o expr)
       (cond ((all? expr) `(@ ,o ,kk))
             ((atom expr) expr)
             ; ((car-itr? expr) (rec o expr))
             ((car-itr? expr) (rec `(@ ,o ,kk) expr))
             ((car-get? expr)
              `(@ ,o ,(ensure-string (second expr)) ,@(cddr expr)))
             ((consp expr) (cons (compile/expr/rec kk o (car expr))
                                 (compile/expr/rec kk o (cdr expr))))
             (t (warn "unexpected expr in selector: ~a~%expr: ~a" (list kk o expr)))))

     (psh (mode kk expr)
       (case mode (:? 'apsh?) (:+ 'apsh+)
         (otherwise (error "unexpected mode in selector: ~a ~a~%expr: ~a"
                           mode kk expr))))
     (compile/itr (dat d)
       (awg (kvres itrlst o)
         `(loop with ,itrlst = (mav)
                for ,o across (ensure-vector ,dat)
                for ,kvres = (list)
                do (progn ,@(loop for (mode kk expr) in (reverse d)
                                  collect `(,(psh mode kk expr) ,kvres ,kk
                                            ,(compile/expr/rec (ensure-string kk)
                                               o expr)))
                          (vextend ,kvres ,itrlst))
                finally (return ,itrlst))))
     (rec (dat d)
       (cond ((all? d) dat) ((atom d) d)
             ((car-itr? d) (compile/itr dat
                             (compile/itr/preproc (cdr d))))
             (t (error "compile error for: ~a ~a" dat d)))))

    (rec dat q)))

(defmacro qryd (dat &key q db)
  (declare (boolean db) (cons q)) "query dat"
  (awg (dat*) (let ((compiled (proc-qry dat* q)))
                (when db (jqn/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))
(defmacro qryf (fn &key q db)
  (declare (boolean db) (cons q)) "query file fn"
  `(qryd (loadjsn ,fn) :q ,q :db ,db))

(defun qryl (dat &key q)
  (declare (string dat)) "compile query and run it on dat"
  (awg (dat*) (eval `(let ((,dat* ,dat))
                       (qryf ,dat* :q ,q)))))

