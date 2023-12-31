(in-package :lqn)

(defvar *qmodes* '(:+ :? :- :%))
(defvar *operators* `(:*map :@ :|| :*$ :$$ :$* :** :*fld :?* :?xpr :?txpr :?mxpr :?srch))
(defvar *fxns* '(:err :wrn :nope :noop :lst :lit :qt :hld :ghv
                 :fmt :out :jsnstr
                 :fn :fi :ctx  :par :itr :compct :?? :@@ :@* :smth?
                 :*0 :*1 :*2 :*3 :*4 :*5 :*6 :*7 :*8 :*9 :ind* :sel* :seq*
                 :new* :new$ :cat* :cat$ :head* :tail* :size :size?
                 :flatn* :flatall* :flatn$
                 :pnum :inum :cnt
                 :pref? :suf? :sub? :subx? :ipref? :isuf? :isub? :isubx?
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

(defun internal-path-string (path &optional (pkg :lqn)) (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'lqn) 'asdf:version)))
  "return/print lqn version." (unless silent (format t "~&LQN version: ~a~%." v)) v)

(defun mkstr (&rest args) "coerce all arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun kv (s) "mkstr, upcase, keyword."
  (intern (sup (etypecase s (string s) (symbol (symbol-name s)) (number (mkstr s))))
          :keyword))
(defun ct/kv/str (a)
  (typecase a (string a) (keyword (sdwn (mkstr a))) (otherwise a)))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (apply #'mkstr args))))
(defun psymb (&optional (pkg 'lqn) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))
(defun lst (&rest rest) (apply #'list rest))

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
  (apply #'mkstr (loop for s in rest collect (typecase s (string s) (symbol (sdwn s)) (t (mkstr s))))))
(defun vec! (v &optional (d `#(,v))) "coerce v to vector. if v is not a vector, list, string it returns d"
  (etypecase v (vector v) (list (coerce v 'vector)) (t d)))

(defmacro smth? (v &body body) ; TODO: recursive strip with ext function
  (declare (symbol v)) "do body if v is not nil, empty sequence, or empty hash-table"
  (awg (v*)
  `(let ((,v* ,v))
     (typecase ,v* (sequence (when (> (length ,v) 0) (progn ,@body)))
                   (hash-table (when (> (hash-table-count ,v) 0) (progn ,@body)))
                   (otherwise (when ,v (progn ,@body)))))))
(defun is? (k &optional d) "k if k is not nil, empty sequence, or empty hash-table; or d"
  (if (smth? k t) k d))

