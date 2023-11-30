(ql:quickload :jqn :silent t)

(in-package :jqn)

(defun main (args)
  (format t "~a~%"
    (json:encode-json-to-string
      (jqnfl (second args)
        :q (read-from-string (third args) nil nil)))))

(main (auxin:cmd-args))

