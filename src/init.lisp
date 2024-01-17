(in-package :lqn)

(declaim (inline symb mkstr subx? isubx?))

(defvar *qmodes* '(:+ :? :- :%))
(defvar *operators*
  `(:*map :@ :|| ?rec :*$ :$$ :$* :** :*fld :?* :?xpr :?txpr :?mxpr :?srch))
(defvar *opt* '(optimize (speed 3) (safety 1)))
(defvar *fxns* '(:err :wrn :nope :noop :lst :lit :qt :hld :ghv :pnum :inum :cnt :λ
                 :fmt :out :jsnstr
                 :fn :fi :ctx  :par :itr :compct :?? :@@ :@* :smth? :ind* :sel* :seq* :apply* :join
                 :new* :new$ :cat* :cat$ :head* :tail* :size?
                 :flatn* :flatall* :flatn$
                 :pref? :suf? :sub? :subx? :ipref? :isuf? :isub? :isubx? :lpad :rpad :nstr
                 :sup :sdwn :mkstr :repl :strcat :splt
                 :msym? :is? :kv? :sym? :sym! :trim
                 :num!? :num? :flt!? :flt? :int!? :int?
                 :lst? :seq? :seq!? :str! :str? :str!? :vec! :vec? :vec!?))
(defun cmd-args ()
  (or #+SBCL sb-ext:*posix-argv* #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words* nil))

(defun terminate (status &optional (silent t)) ;https://www.rosettacode.org/wiki/Program_termination#Common_Lisp
  (unless silent (format t "~%terminated with status: ~a~%" status))
  #+sbcl (sb-ext:quit :unix-status status) #+ccl (ccl:quit status)
  #+clisp (ext:quit status) #+cmu (unix:unix-exit status)
  #+abcl (ext:quit:status status) #+allegro (excl:exit status :quiet t)
  #+gcl (common-lisp-user::bye status) #+ecl (ext:quit status))

(defmacro pretty-json (v) `(lqn:out (lqn:jsnstr ,v :indent t)))
(defmacro noop (&rest rest) (declare (ignore rest)) "do nothing. return nil." nil)
(defmacro lit (a) `(progn ,a))
(defmacro qt (a) `(progn ',a))
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

(defmacro abbrev (short long) `(defmacro ,short (&rest args) `(,',long ,@args)))
(abbrev awg with-gensyms)        (abbrev mav make-adjustable-vector)
(abbrev dsb destructuring-bind)  (abbrev mvb multiple-value-bind)
(abbrev mvc multiple-value-call) (abbrev mvl multiple-value-list)
(abbrev vpe vector-push-extend)  (defmacro vex (v o) `(vpe ,o ,v))

(defmacro λ (&rest rest) `(lambda ,@rest))
(defun internal-path-string (path &optional (pkg :lqn)) (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'lqn) 'asdf:version)))
  "return/print lqn version." (unless silent (format t "~&LQN version: ~a~%." v)) v)

(defun make-adjustable-vector (&key init (type t) (size 128))
  (if init (make-array (length init) :fill-pointer t :initial-contents init
                                     :element-type type :adjustable t)
           (make-array size :fill-pointer 0 :element-type type :adjustable t)))

(defun lpad-lst (n lst)
  (concatenate 'list (loop repeat (- n (length lst)) collect nil) lst))
(defun rpad-lst (n lst)
  (concatenate 'list lst (loop repeat (- n (length lst)) collect nil)))

(defun group (n l) (declare (list l) (fixnum n)) "group l into lists of n elements."
  (if (< n 1) (error "group: group size is smaller than 1"))
  (labels ((rec (l acc)
             (let ((rest (nthcdr n l)))
               (if (consp rest) (rec rest (cons (subseq l 0 n) acc))
                                (nreverse (cons l acc))))))
    (if l (rec l nil) nil)))
(defun mkstr (&rest args) "coerce all arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun kv (s) "mkstr, upcase, keyword."
  (intern (string-upcase (etypecase s (string s) (symbol (symbol-name s)) (number (mkstr s))))
          :keyword))
(defun ct/kv/str (a)
  (typecase a (string a) (keyword (string-downcase (mkstr a))) (otherwise a)))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (apply #'mkstr args))))
(defun psymb (&optional (pkg 'lqn) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))
(defun lst (&rest rest) (apply #'list rest))

(defun clmp (v &optional (a 0.0) (b 1.0)) (declare (number v a b)) (max a (min v b)))

(defmacro car- (fx d) (declare (symbol fx d)) `(and (listp ,d) (,fx (car ,d))))
(defun sym-not-kv (d) (when (and (symbolp d) (not (keywordp d))) d))
(defun sym-mode? (d &aux (mode-sym (unpack-mode d nil)))
  (if mode-sym (values-list (unpack-mode mode-sym d :?)) (values nil d)))
(defun qop? (s d &aux (d (and (listp d) (car d)))) (and d (sym-not-kv d) (eq s (kv d))))
(defun all?    (d) (and (symbolp d)    (eq (kv d) :_)))
(defun lqnfx?  (d) (and (sym-not-kv d) (member (kv d) *fxns* :test #'eq)))

; IS TYPE?
(defun flt? (f &optional d) "f if float; or d"    (if (floatp f) f d))
(defun int? (i &optional d) "i if int; or d"      (if (integerp i) i d))
(defun kv?  (k &optional d) "k if kv; or d"       (if (hash-table-p k) k d))
(defun sym? (s &optional d) "s if sym; or d"      (if (symbolp s) s d))
(defun lst? (l &optional d) "l if list; or d"     (if (listp l) l d))
(defun num? (n &optional d) "n if number; or d"   (if (numberp n) n d))
(defun str? (s &optional d) "s if string; or d"   (if (stringp s) s d))
(defun vec? (v &optional d) "v if vector; or d"   (if (vectorp v) v d))
(defun seq? (s &optional d) "s if sequence; or d" (or (lst? s) (str? s) (vec? s) d))

; PARSE AS TYPE OR DEFAULT
(defun int!? (i &optional d) "i as int if it can be parsed; or d"
  (handler-case (or (int? i) (int? (read-from-string i nil nil)) d) (error () d)))
(defun flt!? (f &optional d) "f as float if it can be parsed; or d"
  (handler-case (or (flt? f) (flt? (read-from-string f nil nil)) d) (error () d)))
(defun num!? (n &optional d) "n as number if it can be parsed; or d"
  (handler-case (or (num? n) (num? (read-from-string n nil nil)) d) (error () d)))
(defun str!? (n &optional d) "s as str if it can be parsed; or d"
  (handler-case (or (str? n) (str? (read-from-string n nil nil)) d) (error () d)))
(defun vec!? (n &optional d) "v as vector if it can be parsed; or d"
  (handler-case (or (vec? n) (vec? (read-from-string n nil nil)) d) (error () d)))
(defun seq!? (n &optional d) "s as seq if it can be parsed; or d"
  (handler-case (or (seq? n) (seq? (read-from-string n nil nil)) d) (error () d)))

; COERCE TO TYPE
(defun sym! (&rest rest) "stringify, make symbol" (apply #'symb rest))
(defun str! (&rest rest) "coerce to string"
  (apply #'mkstr (loop for s in rest collect (typecase s (string s) (symbol (string-downcase s)) (t (mkstr s))))))
(defun vec! (v &optional (d `#(,v))) "coerce v to vector. if v is not a vector, list, string it returns d"
  (etypecase v (vector v) (list (coerce v 'vector)) (t d)))

(defun size? (l &optional d) "length of sequence/number of keys in kv."
  (typecase l (sequence (length l)) (hash-table (hash-table-count l)) (otherwise d)))
(defun empty? (l &optional d &aux (n (size? l))) (if (int? n) (< n 1) d))

(defmacro smth? (v &body body) ; TODO: recursive strip with ext function
  (declare (symbol v)) "do body if v is not nil, empty sequence, or empty hash-table"
  (awg (v*)
  `(let ((,v* ,v))
     (typecase ,v* (sequence (when (> (length ,v) 0) (progn ,@body)))
                   (hash-table (when (> (hash-table-count ,v) 0) (progn ,@body)))
                   (otherwise (when ,v (progn ,@body)))))))
(defun is? (k &optional d) "k if k is not nil, empty sequence, or empty hash-table; or d"
  (if (smth? k t) k d))

(defun gk (conf k &optional silent &aux (hit (cdr (assoc k conf))))
  (declare (list conf) (keyword k)) "get k from config"
  (if (or silent hit) hit (warn "LQN: missing conf key: ~a~%conf: ~s" k conf)))

