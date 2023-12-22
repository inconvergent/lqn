(in-package :jqn)

(defun verbose? (opts) (not (null (member :-v opts :test #'equal))))
(defun indent? (opts) (null (member :-m opts :test #'equal)))
(defun format? (opts) (if (member :-l opts :test #'equal) :ldn :json))

(defmacro exit-with-msg (i &rest rest)
  (declare (fixnum i))
  `(progn (format *error-output* ,@rest)
          (terminate ,i t)))

(defun split-opts-args (args)
  (labels ((do-explode (o)
             (loop for s across (subseq o 1) collect (kv (mkstr "-" s))))
           (explode-opts (opts)
             (loop for o in opts
                   if (= (length o) 2) nconc `(,(kv o))
                   else nconc (do-explode o))))
   (loop named opts for k in args for i from 0
        if (startswith? k "-") collect k into opts
        else do (return-from opts
                  (values (explode-opts opts) (subseq args i))))))

