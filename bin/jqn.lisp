(ql:quickload :jqn :silent t)

(in-package :jqn)

(defun main (args)
  (format t "~a~%"
    (json:encode-json-to-string
      (jqnfl (second args) :q (read-str (third args))))))

(main (auxin:cmd-args))

