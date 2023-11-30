(in-package :jqn)

(defun dat (fn)
  (with-open-file (f fn :direction :input)
    (json:decode-json f)))

(defun car-kv? (d) (and (listp d) (keywordp (car d))))
(defun car-itr? (d) (and (listp d) (eq '* (car d))))
(defun all? (d) (eq d '_))

(defun compile/itr/preproc (q)
  (loop for k in q
        collect (typecase k (keyword `(,k _))
                            (cons k))))

(defun jqn (src q)
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
  (let ((dat* (gensym "DAT")))
    `(let ((,dat* ,dat))
      ,(jqn dat* q))))
(defmacro jqnf (fn &key q) `(jqnd (dat ,fn) :q ,q))

(defun jqnfl (dat &key q)
  ; (declare (grph g) (boolean db))
  (awg (dat*)
    (eval `(let ((,dat* ,dat))
             (jqnf ,dat* :q ,q)))))

; ensure is list

; any parent pattern,
; exact parent pattern

;   (:friends
;        ; _ :id (:name "Lola Nixon"))

; prologue is bidirectionality: you can write `X = 5*2` to assign 10 to X, but
; you can also write `10 = X*2` to assign 5 to X, or even `10 = X*Y`

; ( :_id  ; keyword, assume selector
;   (:friends
;    _ :id (:name "Lola Nixon")
;     )  ;
;    )

