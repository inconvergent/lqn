(in-package :lqn)

(defun sh/one? (v) (if (> (length v) 1) v (aref v 0)))
(defun verbose? (opts) (not (null (member :-v opts :test #'equal))))
(defun compile? (opts) (not (null (member :-c opts :test #'equal))))
(defun help? (opts)    (member :-h opts :test #'equal))
(defun indent? (opts)  (null (member :-m opts :test #'equal)))
(defun format? (opts &optional (d :json))
  (cond ((member :-l opts :test #'eq) :ldn)
        ((member :-t opts :test #'eq) :txt)
        ((member :-j opts :test #'eq) :json)
        (t d)))

(defmacro exit-with-msg (i &rest rest) (declare (fixnum i))
  (declare (optimize speed))
  (if (< i 1) `(progn (format *standard-output* ,@rest) (terminate ,i t))
              `(progn (format *error-output* ,@rest) (terminate ,i t))))

(defun split-opts-args (args)
  (declare (optimize speed (safety 3)))
  (labels ((expl- (o) (loop for s across (subseq o 1) collect (kv (mkstr "-" s))))
           (explode- (opts) (loop for o in opts if (= (length o) 2) nconc `(,(kv o))
                                                else nconc (expl- o))))
   (loop named opts for k in args for i from 0
         if (pref? k "-") collect k into opts
         else do (return-from opts (values (explode- opts) (subseq args i)))
         finally (return-from opts (values (explode- opts) nil)))))

(defun sh/is-query (q) (unless q (exit-with-msg 5 "lqn: MISSING query")) q)
(defun sh/parse-query (q) (declare #.*opt*)
  (handler-case `(|| ,@(read-all-str q))
    (error (e) (exit-with-msg 10 "lqn: failed to PARSE qry:~%~a" e))))
(defun sh/compile-query (qq) (declare #.*opt*)
  (handler-case (proc-qry qq)
    (error (e) (exit-with-msg 20 "lqn: failed to COMPILE qry:~%~a" e))))
(defun sh/eval-query (cq) (declare #.*opt*)
  (handler-case (eval cq)
    (error (e) (exit-with-msg 30 "lqn: failed to BUILD QUERY FX qry:~%~a" e))))

(defun sh/execute-qry (fx &rest rest) (declare #.*opt* (function fx))
  (handler-case (apply fx rest)
    (error (e) (exit-with-msg 80 "lqn: failed to EXECUTE qry: ~a" e))))

(defun sh/run-from-shell (ex args filefx pipefx)
  (declare (optimize speed) (simple-string ex) (function filefx pipefx))
  (multiple-value-bind (opts args) (split-opts-args args)
    (when (help? opts) (exit-with-msg 0 ex))
    (let* ((q  (sh/is-query (car args))) (qq (sh/parse-query q))
           (cq (sh/compile-query qq))    (fx (sh/eval-query cq))
           (files (cdr args)))
      (when (verbose? opts) (qry/show qq cq))
      (cond ((interactive-stream-p *standard-input*)
             (unless (< 0 (length files)) (exit-with-msg 50 "lqn: MISSING files.~%~a~&" ex))
             (funcall filefx opts fx files))
            (t (funcall pipefx opts fx))))))

(defun sh/out (d opts res) (declare (optimize speed (safety 2)))
  (ecase (format? opts d)
    (:json (handler-case (jsnout res :indent (indent? opts))
             (error (e) (exit-with-msg 101 "ldn: failed to SERIALIZE JSON.~%~a" (mkstr e)))))
    (:ldn (handler-case (format t "~&~s~&" (ldnout res))
            (error (e) (exit-with-msg 102 "ldn: failed to SERIALIZE LDN.~%~a" (mkstr e)))))
    (:txt (handler-case
            (labels ((prldn (s) (format t "~&~s~&" (ldnout s)))
                     (prln (s) (format t "~&~a~%" s))
                     (doln (s) (typecase s (null nil) ; silent nils, use smth??
                                           (string (prln s))
                                           (vector (prldn s)) (hash-table (prldn s))
                                           (list (prldn s))
                                           (number (prln s))
                                           (otherwise (prln s)))))
              (typecase res (string (doln res))
                            (vector (loop for s across (compct res) if s do (doln s)))
                            (otherwise (doln res))))
            (error (e) (exit-with-msg 103 "failed to SERIALIZE TXT.~%~a" (mkstr e)))))))

