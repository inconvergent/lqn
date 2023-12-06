
(setf prove:*enable-colors* nil)
(defpackage #:jqn-tests (:use #:cl #:prove) (:export #:run-tests))
(in-package #:jqn-tests)

(defun -run-tests (files)
  (labels ((rel (f) (mapcar (lambda (p) (asdf:system-relative-pathname
                                          "jqn/tests" p))
                            f)))
    (loop with fails = 0
          for f in (rel files)
          do (format t "~&~%starting tests in: ~a~%" (jqn::mkstr f))
             (unless (prove:run f :reporter :fiveam)
                     (incf fails))
             (format t "~&done: ~a~%" (jqn::mkstr f))
          finally (return (unless (< fails 1) (uiop:quit 7))))))

(defun run-tests ()
  (-run-tests '(#P"test/test-jqn.lisp")))

(defun lsort* (l &aux (l (copy-list l)))
  (declare (optimize speed) (list l))
  "radix sort list of lists (of numbers or symbols).
inefficient. use for tests only."
  (loop for i of-type fixnum from (1- (length (the list (first l)))) downto 0
        do (labels ((srt (a b)
                      (funcall
                        (etypecase a (symbol #'string<) (number #'<)
                                     (cons (lambda (a* b*) ; hacky ...
                                             (string< (veq::mkstr a*) (veq::mkstr b*)))))
                        a b))
                    (p (a b) (srt (nth i a) (nth i b))))
                   (setf l (stable-sort (the list l) #'p))))
  l)
(defun ls (l) (lsort* l))
(defun mapls (&rest rest) (mapcar #'lsort* rest))
(defun rs (l) (sort l #'<))

