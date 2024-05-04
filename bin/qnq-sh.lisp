(ql:quickload :lqn :silent t)
(in-package :lqn)

; TODO: this is incomplete

(lqn:qry (cdr (find :export (lqn:dat-read-one (lqn::internal-path-string "src/packages.lisp"))
                    :key (lambda (k) (and (listp k) (car k)))))
         [(lqn:isub? (str! _) (lqn:sdwn (second (cmd-args))))]
         #((lqn:out (lqn::desc (lqn::psymb :lqn _)))))

; (defun xqn/run (opts fx)
;   (declare (optimize speed) (function fx))
;   (sh/out :txt opts (sh/execute-qry fx nil ":pipe:" 0)))

; (sh/run-from-shell (format nil
; "██ QNQ - QUERY LQN SYMBOLS

; Usage:
;   qnq <qry>

; Example:

;   qnq 'msym'
; " (lqn:v?)) (cdr (cmd-args)) #'xqn/run #'xqn/run t)

