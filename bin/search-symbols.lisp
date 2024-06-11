(in-package :lqn)

(let ((q (second (cmd-args))) (pkg (third (cmd-args))))
 (qry (ls "~/common-lisp/*/src/packages.lisp")
      dat-read-file _@flatn* second                        ; package names
      [(or (not pkg) (isub? s@_ pkg))] extsym? _@flatn*    ; filter packages
      [(or (string= s@q "_") (isub? s@_ q))]               ; filter symbol
      #((join (head (splt (stdstr _@describe)
                          #.(str! #\Newline) nil) -1)      ; some formatting
               #\Newline " ; ")
        (out "~&████████████████████████████████████████~& ; ~a~&~%" _))))

