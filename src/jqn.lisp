(in-package :jqn)

(defun loadf (fn)
  "load json file"
  (with-open-file (f fn :direction :input)
    (yason:parse f)))

(defun dumps (o &key (s *standard-output*) indent)
  (yason:encode o
    (yason:make-json-output-stream s :indent indent)))

(defun car-kv? (d) (and (listp d) (keywordp (car d))))
(defun car-itr? (d) (and (listp d) (eq '* (car d))))
(defun all? (d) (eq d '_))

(defun compile/itr/preproc (q)
  (loop for k in q
        collect (typecase k (keyword `(,(string-downcase (mkstr k)) _))
                            (string `(,k _))
                            (cons k))))

(defun ser (o)
  (etypecase o
            (hash-table
              (loop for k being the hash-key of o using (hash-value v)
                    collect `(,k . ,(ser v))))
            (cons (mapcar (lambda (v) (ser v)) o))
            (t o)
                     ))

(defun jqn (src q)
  "compile jqn query"
  (labels ((compile/itr (src d)
             (let ((list-body ; HERE TODO
                     (loop for (kk vv) in d
                           collect `(cons ,kk (ser (gethash ,kk o))))))
               `(loop for o in ,src
                      collect `(,,@list-body ))))
           (rec (src d)
             (cond ((all? d) src)
                   ((atom d) d)
                   ((car-itr? d) (compile/itr src
                                   (compile/itr/preproc (cdr d))))
                   ((car-kv? d) d)
                   (t (error "not implemented: ~a" d)))))
    (rec src q)))

(defun jqn/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~a
██ ---
   ~a
██ ██████████████████████████~%" q compiled))

(defmacro jqnd (dat &key q db)
  "query dat"
  (awg (dat*)
    (let ((compiled (jqn dat* q)))
      (when db (jqn/show q compiled))
        `(let ((,dat* ,dat))
                 ,compiled))))
(defmacro jqnf (fn &key q db)
  "query file fn"
  `(jqnd (loadf ,fn) :q ,q :db ,db))

(defun jqnfl (dat &key q)
  (declare (string dat))
  (awg (dat*) (eval `(let ((,dat* ,dat))
                       (jqnf ,dat* :q ,q)))))

; (defun jqn (src q)
;   "compile jqn query"
;   (labels ((compile/itr (src d)
;              (let ((case-body ; HERE TODO
;                      (loop for (kk vv) in d
;                            for i from 0
;                            nconc `(,@(if (= 0 i) '(if) '(else if)) (eq k ,kk)
;                                    collect `(,,kk . ,,(rec 'v vv))))))
;                `(loop for o in ,src
;                       collect (loop for (k . v) in o ,@case-body))))
;            (rec (src d)
;              (cond ((all? d) src)
;                    ((atom d) d)
;                    ((car-itr? d) (compile/itr src
;                                    (compile/itr/preproc (cdr d))))
;                    ((car-kv? d) d)
;                    (t (error "not implemented: ~a" d)))))
;     (rec src q)))

