(in-package :jqn)

(defvar *qmodes* '(:+@ :?@ :-@ :%@))
(defvar *fxns* '(:fn :fi :ctx :num :cnt :par :$ :$_ :>< :??
                 :*cat :$cat *ind *sel *seq :head :tail
                 :sup :sdwn :mkstr :repl :strcat
                 :pref? :suf? :sub? :ipref? :isuf? :isub?
                 :flt? :int? :kv? :lst? :num? :seq? :str? :vec?
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
(abbrev awg with-gensyms)
(abbrev dsb destructuring-bind)
(abbrev mvb multiple-value-bind)
(abbrev mvc multiple-value-call)
(abbrev mav make-adjustable-vector)
(abbrev vex vector-push-extend)

(defun internal-path-string (path &optional (pkg :jqn))
  (declare (string path))
  (namestring (asdf:system-relative-pathname pkg path)))

(defun d? (s) "describe symbol." (describe s)) (defun i? (s) "inspect s" (inspect s))
(defun v? (&optional (silent t)
           &aux (v (slot-value (asdf:find-system 'jqn) 'asdf:version)))
  "return/print jqn version."
  (unless silent (format t "~&JQN version: ~a~%." v))
  v)
