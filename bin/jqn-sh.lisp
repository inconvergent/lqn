(ql:quickload :jqn :silent t)

(in-package :jqn)

(defun main (args)
  (
   ; print
   wrtjsn
    (qryl (second args) :db t :q (read-str (third args)))))

(main (auxin:cmd-args))

