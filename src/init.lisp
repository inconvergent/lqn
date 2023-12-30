(in-package :jqn)

(defvar *qmodes* '(:+ :? :- :%))
(defvar *fxns* '(:fn :fi :ctx :num :cnt :par :$ :$_ :>< :??
                 :*0 :*1 :*2 :*3 :*4 :*5 :*6 :*7 :*8 :*9 :*n :*sel :*seq
                 :*cat :$cat :head :tail :size
                 :sup :sdwn :mkstr :repl :strcat :splt
                 :tfnd?
                 :is? :kv?
                 :pref? :suf? :sub? :subx? :ipref? :isuf? :isub? :isubx?
                 :num!? :num?  :flt!? :flt?  :int!? :int?
                 :lst? :seq?  :str! :str?  :vec! :vec?
                 :fmt :out))

(defun cmd-args ()
  (or #+SBCL sb-ext:*posix-argv* #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words* nil))

(defun terminate (status &optional (silent t)) ;https://www.rosettacode.org/wiki/Program_termination#Common_Lisp
  (unless silent (format t "~%terminated with status: ~a~%" status))
  #+sbcl (sb-ext:quit :unix-status status) #+ccl (ccl:quit status)
  #+clisp (ext:quit status) #+cmu (unix:unix-exit status)
  #+abcl (ext:quit:status status) #+allegro (excl:exit status :quiet t)
  #+gcl (common-lisp-user::bye status) #+ecl (ext:quit status))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s)))) syms) ,@body))

(defmacro abbrev (short long) `(defmacro ,short (&rest args) `(,',long ,@args)))
(abbrev awg with-gensyms) (abbrev mav make-adjustable-vector)
(abbrev dsb destructuring-bind) (abbrev mvb multiple-value-bind)
(abbrev mvc multiple-value-call) (abbrev mvl multiple-value-list)
(abbrev vpe vector-push-extend) (defmacro vex (v o) `(vpe ,o ,v))

(defun internal-path-string (path &optional (pkg :jqn))
  (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'jqn) 'asdf:version)))
  "return/print jqn version."
  (unless silent (format t "~&JQN version: ~a~%." v))
  v)

(defmacro car- (fx d) (declare (symbol fx d)) `(and (listp ,d) (,fx (car ,d))))

(defun sym-mode? (d &aux (mode-sym (unpack-mode d nil)))
  (if mode-sym (values-list (unpack-mode mode-sym d :?))
               (values nil d)))
(defun all?   (d) (and (symbolp d) (eq (kv d) :_)))
(defun $new?  (d) (and (symbolp d) (eq (kv d) :$new)))
(defun *new?  (d) (and (symbolp d) (eq (kv d) :*new)))
(defun $$sel? (d) (and (symbolp d) (eq (kv d) :$$)))
(defun *$sel? (d) (and (symbolp d) (eq (kv d) :*$)))
(defun **sel? (d) (and (symbolp d) (eq (kv d) :**)))
(defun $*sel? (d) (and (symbolp d) (eq (kv d) :$*)))
(defun *map?  (d) (and (symbolp d) (eq (kv d) :*map)))
(defun *fld?  (d) (and (symbolp d) (eq (kv d) :*fld)))
(defun pipe?  (d) (and (symbolp d) (eq (kv d) :||)))
(defun jqnfx? (d) (and (symbolp d) ; only symbols, not kv
                       (not (keywordp d))
                       (member (kv d) *fxns* :test #'eq)))

(defun car-sym-mode? (d)
  (typecase d (cons (if (or (str? (car d)) (symbolp (car d)))
                        (values-list (sym-mode? (car d)))
                        (values nil d)))
              (otherwise (values nil d))))
