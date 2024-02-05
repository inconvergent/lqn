#!/usr/local/bin/sbcl --script

(let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file init) (load init)))

(setf lparallel:*kernel* (lparallel:make-kernel 8))

(ql:quickload :grph)
(ql:quickload :veq)
(load (lqn::internal-path-string "bin/cindex.lisp"))

(defvar *places* '(
                   (:veq ("/home/anders/x/veq/src/*.lisp"))
                   (:grph ("/home/anders/x/grph/src/*.lisp"))
                   (:lqn ("/home/anders/x/lqn/src/*.lisp"))
                   (:auxin ("/home/anders/x/auxin/src/*.lisp"
                            "/home/anders/x/auxin/src/*/*.lisp"))))

#|(cl:*read-eval* nil)|#
(defun main ()
  (let ((co (lqn/code:code)))
    (loop for (sys pth) in *places*
      do (let ((cl:*package* (find-package sys)))
           (lqn:qry pth ls (flatn* _)
                    #(str! (lqn/code:index co _ sys)))))
    ; (mapc #'print
    ;  (lqn::code/cqry co :select (?a (grp ?file)) :db t
    ;    :where (and (?file :/file/form ?form)
    ;                (?form :/form/atom ?a)
    ;                (?form :/form/name ?a))
    ;    :collect (list (lqn::frag/mget co ?a)
    ;                   (loop for f across (lqn:flatall* (grp ?file))
    ;                         collect (lqn::frag/get co f)))))
    ; (mapc #'print
    ;   (lqn/code:cqry co :select (?file ?name) :db t
    ;     :where (and (?file :/file/form ?form)
    ;                 (//ft/defun :/form-type ?form)
    ;                 (?form :/form/name ?name))
    ;     :collect (lqn/code:mget co ?name ?file )))

    ; (print (grph::mid (lqn/code::code-grp co)))

    ; (mapc #'print
    ;   (lqn/code:cqry co :select (?atom) :db :full
    ;     :where (and
    ;                 ; (//ty/fixnum :/type ?atom)
    ;                 (:/ext _ ?atom)
    ;                 )
    ;     :collect (lqn/code:mget co ?atom)
    ;     ))

    ; (mapc #'print
    ;   (lqn/code:cqry co :select (?file ?sys ?pkg (grp ?atom)) :db :full
    ;     :where (and (?sys :/sys/file ?file)
    ;                 (?file :/file/form ?form)
    ;                 (?pkg :/pkg ?form)
    ;                 (?form :/form/atom ?atom)
    ;                 ; (//ty/fixnum :/type ?atom)
    ;                 (:/ty/fixnum :/type ?atom)
    ;                 )
    ;     :collect (lqn:qry (lqn:cat* ?file ?sys ?pkg ;!?ty/fixnum
    ;                                 (lqn:uniq (lqn:flatall* (grp ?atom))))
    ;                       #((lqn/code:fget co _)))))

    (print co)
    (lqn/code:gwrite co "tmp")))

(main)

