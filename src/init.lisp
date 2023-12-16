(in-package :jqn)

(defvar *qmodes* '(:+@ :?@ :-@ :%@))
(defvar *fxns* '(:<> :>< :?? :sup :sdwn :@ :@dat))
; (defvar *itrs* '(:** :$$ :*$))

(defun cmd-args ()
  (or #+SBCL sb-ext:*posix-argv* #+LISPWORKS system:*line-arguments-list*
      #+CMU extensions:*command-line-words* nil))

(defun terminate (status &optional (silent t)) ;https://www.rosettacode.org/wiki/Program_termination#Common_Lisp
  (unless silent (format t "~%terminated with status: ~a~%" status))
  #+sbcl (sb-ext:quit :unix-status status) #+ccl (ccl:quit status)
  #+clisp (ext:quit status) #+cmu (unix:unix-exit status)
  #+abcl (ext:quit:status status) #+allegro (excl:exit status :quiet t)
  #+gcl (common-lisp-user::bye status) #+ecl (ext:quit status))
