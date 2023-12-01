(in-package :jqn)

(defun car-kv? (d) (and (listp d) (keywordp (car d))))
(defun car-itr? (d) (and (listp d) (eq '* (car d))))
(defun all? (d) (eq d '_))

(defun compile/itr/preproc (q)
  (loop for k in q
        collect (typecase k (keyword `(,k _))
                            (cons k))))

(defun jqn (src q)
  "compile jqn query"
  (labels ((compile/itr (src d)
             (let ((case-body
                     (loop for (kk vv) in d
                           for i from 0
                           nconc `(,@(if (= 0 i) '(if) '(else if)) (eq k ,kk)
                                   collect `(,,kk . ,,(rec 'v vv))))))
               `(loop for o in ,src
                      collect (loop for (k . v) in o ,@case-body))))
           (rec (src d)
             (cond ((all? d) src)
                   ((atom d) d)
                   ((car-itr? d) (compile/itr src
                                   (compile/itr/preproc (cdr d))))
                   ((car-kv? d) d)
                   (t (error "not implemented: ~a" d)))))
    (rec src q)))

(defmacro jqnd (dat &key q)
  "query dat"
  (awg (dat*) `(let ((,dat* ,dat))
                 ,(jqn dat* q))))
(defmacro jqnf (fn &key q)
  "query file fn"
  `(jqnd (loadf ,fn) :q ,q))

(defun jqnfl (dat &key q)
  (declare (string dat))
  (awg (dat*) (eval `(let ((,dat* ,dat))
                       (jqnf ,dat* :q ,q)))))

