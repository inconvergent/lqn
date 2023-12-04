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
  (let ((yason:*list-encoder* 'yason:encode-alist))
    (yason:encode o (yason:make-json-output-stream s :indent indent))))

(defun compile/itr/preproc (q)
  (labels ((unpack-cons (k &aux (ck (car k)))
             (declare (list k))
             (case (length k)
               (1 `(,@(unpack-mode ck *qmodes*) :_))              ; ?/m [m]@key _
               (2 `(,@(unpack-mode ck *qmodes*) ,(second k)))     ; ?/m [m]@key expr
               (3  `(,ck ,(ensure-string (second k)) ,(third k))) ; m       key expr
               (otherwise (warn "unexpected # items in selector: ~a" k)))))
    (loop for k in q
          collect (etypecase k (symbol `(,@(unpack-mode k *qmodes*) :_))
                               (string `(,(unpack-mode k *qmodes*) :_))
                               (cons `(,@(unpack-cons k)))))))
(defun ensure-string (s)
  (etypecase s (symbol (string-downcase (mkstr s)))
               (string s)))

(defun proc-qry (dat q)
  "compile jqn query"
  (labels
    ((psh (mode kk vv)
       (case mode (:? 'apsh?) (:+ 'apsh+)
         (otherwise (error "unexpected mode in selector for (~a ~a ~a)"
                           mode kk vv))))
     (make-val (kk o vv) (rec `(gethash ,kk ,o) vv))
     (compile/itr (dat d)
       (awg (kvres itrlst o)
         (let ((loop-body
                 (loop for (mode kk vv) in (reverse d)
                       collect `(,(psh mode kk vv) ,kvres ,kk
                                 ,(make-val (ensure-string kk) o vv)))))
           `(loop with ,itrlst = (mav)
                  for ,o across (ensure-vector ,dat)
                  for ,kvres = (list)
                  do (progn ,@loop-body
                            (vextend ,kvres ,itrlst))
                  finally (return ,itrlst)))))
     (rec (dat d) (cond ((all? d) dat)
                        ((car-itr? d) (compile/itr dat
                                        (compile/itr/preproc (cdr d))))
                        ((atom d) d)
                        (t (error "compile error for: ~a" d)))))
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

