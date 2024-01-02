(in-package :lqn)

(defun verbose? (opts) (not (null (member :-v opts :test #'equal))))
(defun help? (opts)    (member :-h opts :test #'equal))
(defun indent? (opts)  (null (member :-m opts :test #'equal)))
(defun format? (opts &optional (d :json))
  (cond ((member :-l opts :test #'eq) :ldn)
        ((member :-t opts :test #'eq) :txt)
        ((member :-j opts :test #'eq) :json)
        (t d)))

(defmacro exit-with-msg (i &rest rest) (declare (fixnum i))
  (if (< i 1) `(progn (format *standard-output* ,@rest) (terminate ,i t))
              `(progn (format *error-output* ,@rest) (terminate ,i t))))

(defun split-opts-args (args)
  (labels ((expl- (o) (loop for s across (subseq o 1) collect (kv (mkstr "-" s))))
           (explode- (opts) (loop for o in opts if (= (length o) 2) nconc `(,(kv o))
                                                else nconc (expl- o))))
   (loop named opts for k in args for i from 0
         if (pref? k "-") collect k into opts
         else do (return-from opts (values (explode- opts) (subseq args i)))
         finally (return-from opts (values (explode- opts) nil)))))

(defun sh/out (d opts res)
  (ecase (format? opts d)
    (:json (handler-case (jsnout res :indent (indent? opts))
             (error (e) (exit-with-msg 70 "failed to serialize JSON.~%~a" (mkstr e)))))
    (:ldn (handler-case (format t "~&~s~&" (lqnout res))
            (error (e) (exit-with-msg 70 "failed to serialize LQN.~%~a" (mkstr e)))))
    (:txt (handler-case
            (labels ((prln (s) (format t "~&~a~%" s)))
              (etypecase res (vector (loop for s across res if s do (prln s)))
                             (list (loop for s in res if s do (prln s)))
                             (hash-table (prln res)) ; coerce to jsn line?
                             (string (prln res)) (number (prln res))))
            (error (e) (exit-with-msg 70 "failed to serialize TXT.~%~a" (mkstr e)))))))

