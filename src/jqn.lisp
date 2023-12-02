(in-package :jqn)

(defun jqn/show (q compiled)
 (format t "
██ COMPILED ██████████████████████████
██ q:   ~s
██ ---
   ~s
██ ██████████████████████████~%" q compiled))

(defmacro push? (res k v)
  (declare (symbol res)) "push (k . v) to res if v"
  (awg (v*) `(let ((,v* ,v)) (when ,v* (push `(,,k . ,,v*) ,res)))))
(defmacro push! (res k v &optional default)
  (declare (symbol res)) "push (k . v) to res if v; otherwise push (k . default)"
  (awg (v*) `(let ((,v* ,v))
               (if ,v* (push `(,,k . ,,v*) ,res)
                       (push `(,,k . ,,default) ,res)))))

(defun loadf (fn)
  (declare (string fn)) "load json from file fn"
  (with-open-file (f fn :direction :input) (yason:parse f)))
(defun dumps (o &key (s *standard-output*) indent)
  (declare (stream s) (boolean indent)) "encode o as json to stream, s"
  (yason:encode o (yason:make-json-output-stream s :indent indent)))

(defun car-kv? (d) (and (listp d) (keywordp (car d))))
(defun car-itr? (d) (and (listp d) (eq '* (car d))))
(defun all? (d) (eq d '_))

(defun compile/itr/preproc (q)
  (loop for k in q ; todo case cons should check if (car keyword)
        collect (etypecase k
                  (keyword `(,(string-downcase (mkstr k)) _))
                  (cons `(,(string-downcase (mkstr (car k))) ,@(cdr k)))
                  (string `(,k _)))))

(defun jqn (src q)
  "compile jqn query"
  (labels ((compile/itr (src d)
             (awg (res o)
               (let ((loop-body
                       (loop for (kk vv) in (reverse d)
                             collect `(push? ,res ,kk
                                       ,(rec `(gethash ,kk ,o) vv)))))
                 `(loop for ,o in ,src
                        collect (let ((,res (list))) ,@loop-body ,res)))))
           (rec (src d)
             (cond ((all? d) src)
                   ((atom d) d)
                   ((car-itr? d) (compile/itr src
                                   (compile/itr/preproc (cdr d))))
                   ((car-kv? d) d)
                   (t (error "not implemented: ~a" d)))))
    (rec src q)))

(defmacro jqnd (dat &key q db)
  (declare (boolean db) (cons q)) "query dat"
  (awg (dat*) (let ((compiled (jqn dat* q)))
                (when db (jqn/show q compiled))
                `(let ((,dat* ,dat)) ,compiled))))
(defmacro jqnf (fn &key q db)
  (declare (boolean db) (cons q)) "query file fn"
  `(jqnd (loadf ,fn) :q ,q :db ,db))

(defun jqnfl (dat &key q)
  (declare (string dat)) "compile query and run it on dat"
  (awg (dat*) (eval `(let ((,dat* ,dat))
                       (jqnf ,dat* :q ,q)))))

