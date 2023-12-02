(ql:quickload :jqn :silent t)

(in-package :jqn)

(defun main (args)
  (print ;dumps
    (jqnfl (second args) :q (read-str (third args)))
    ))

(main (auxin:cmd-args))

