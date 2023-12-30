(in-package :lqn)

(defvar *qmodes* '(:+ :? :- :%))
(defvar *fxns* '(:fmt :out :jsnstr
                 :fn :fi :ctx :num :cnt :par :$ :$_ :>< :??
                 :*0 :*1 :*2 :*3 :*4 :*5 :*6 :*7 :*8 :*9 :*n :*sel :*seq
                 :*new :$new :*cat :$cat :head :tail :size
                 :sup :sdwn :mkstr :repl :strcat :splt
                 :tfnd?  :is? :kv?
                 :pref? :suf? :sub? :subx? :ipref? :isuf? :isub? :isubx?
                 :num!? :num? :flt!? :flt? :int!? :int?
                 :lst? :seq? :str! :str? :vec! :vec?))

(defun cmd-args ()
  (or #+SBCL sb-ext:*posix-argv* #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words* nil))

(defun terminate (status &optional (silent t)) ;https://www.rosettacode.org/wiki/Program_termination#Common_Lisp
  (unless silent (format t "~%terminated with status: ~a~%" status))
  #+sbcl (sb-ext:quit :unix-status status) #+ccl (ccl:quit status)
  #+clisp (ext:quit status) #+cmu (unix:unix-exit status)
  #+abcl (ext:quit:status status) #+allegro (excl:exit status :quiet t)
  #+gcl (common-lisp-user::bye status) #+ecl (ext:quit status))

(defmacro noop (&rest rest) (declare (ignore rest)) "do nothing. return nil." nil)
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

(defmacro abbrev (short long) `(defmacro ,short (&rest args) `(,',long ,@args)))
(abbrev awg with-gensyms) (abbrev mav make-adjustable-vector)
(abbrev dsb destructuring-bind) (abbrev mvb multiple-value-bind)
(abbrev mvc multiple-value-call) (abbrev mvl multiple-value-list)
(abbrev vpe vector-push-extend) (defmacro vex (v o) `(vpe ,o ,v))

(defun internal-path-string (path &optional (pkg :lqn))
  (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'lqn) 'asdf:version)))
  "return/print lqn version."
  (unless silent (format t "~&LQN version: ~a~%." v))
  v)

(defun mkstr (&rest args) "coerce all arguments to a string."
  (with-output-to-string (s) (dolist (a args) (princ a s))))
(defun kv (s) "mkstr, upcase, keyword."
  (intern (sup (etypecase s (string s) (symbol (symbol-name s)) (number (mkstr s))))
          :keyword))
(defun symb (&rest args) "mkstr, make symbol." (values (intern (apply #'mkstr args))))
(defun psymb (&optional (pkg 'lqn) &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  "mkstr, make symbol in pkg."
  (values (intern (apply #'mkstr args) pkg)))

(defmacro car- (fx d) (declare (symbol fx d)) `(and (listp ,d) (,fx (car ,d))))
(defun sym-mode? (d &aux (mode-sym (unpack-mode d nil)))
  (if mode-sym (values-list (unpack-mode mode-sym d :?))
               (values nil d)))
(defun symbol-not-kv (d) (and (symbolp d) (not (keywordp d))))
(defun all?   (d) (and (symbolp d) (eq (kv d) :_)))
(defun $$sel? (d) (and (symbol-not-kv d) (eq (kv d) :$$)))
(defun *$sel? (d) (and (symbol-not-kv d) (eq (kv d) :*$)))
(defun **sel? (d) (and (symbol-not-kv d) (eq (kv d) :**)))
(defun $*sel? (d) (and (symbol-not-kv d) (eq (kv d) :$*)))
(defun *map?  (d) (and (symbol-not-kv d) (eq (kv d) :*map)))
(defun *fld?  (d) (and (symbol-not-kv d) (eq (kv d) :*fld)))
(defun pipe?  (d) (and (symbol-not-kv d) (eq (kv d) :||)))
(defun lqnfx? (d) (and (symbol-not-kv d) (member (kv d) *fxns* :test #'eq)))

