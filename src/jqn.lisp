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
               (1 `(,@(unpack-mode ck *qmodes*) :_))
               (2 `(,@(unpack-mode ck *qmodes*) ,@(cdr k)))
               (3  `(,ck ,(ensure-string (second k)) ,(third k)))
               (otherwise (warn "unexpected # items in selector: ~a" k)))))
    (loop for k in q
          collect (etypecase k (keyword `(,@(unpack-mode k *qmodes*) :_))
                               (string `(,(unpack-mode k *qmodes*) :_))
                               (cons `(,@(unpack-cons k)))))))
(defun ensure-string (s)
  (etypecase s (keyword (string-downcase (mkstr s)))
               (string s)))

(defun proc-qry (dat q)
  "compile jqn query"
  (labels
    ((select-psh (mode kk vv)
       (case mode (:? 'apsh?) (:+ 'apsh+)
         (otherwise (error "unexpected mode in selector for (~a ~a ~a)"
                           mode kk vv))))
     (compile/itr (dat d)
       (awg (kvres itrlst o)
         (let ((loop-body
                 (loop for (mode kk vv) in (reverse d)
                       collect `(,(select-psh mode kk vv) ,kvres ,kk
                                  ,(rec `(gethash ,(ensure-string kk) ,o) vv)))))
           `(loop with ,itrlst = (mav)
                  for ,o across (ensure-vector ,dat)
                  for ,kvres = (list)
                  do (progn ,@loop-body
                            (vextend ,kvres ,itrlst))
                  finally (return ,itrlst)))))
     (rec (dat d) (cond ((all? d) dat)
                        ((car-itr? d) (compile/itr dat
                                        (compile/itr/preproc (cdr d))))
                        ((car-kv? d) d)
                        (t (error "not implemented: ~a" d)))))
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

