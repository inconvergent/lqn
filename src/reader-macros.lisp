(in-package :lqn)

; (setf *readtable* (copy-readtable nil))
; (defvar lqnrt (copy-readtable *readtable*) )

(set-macro-character #\{ '-read-left-curly-brace)
(set-macro-character #\[ '-read-left-bracket)
(set-macro-character #\} '-read-delimiter)
(set-macro-character #\] '-read-delimiter)
; (make-dispatch-macro-character #\§)
; (make-dispatch-macro-character #\∈)

(defun -read-delimiter (stream char)
  (let ((*readtable* (copy-readtable)))
    (error "lqn: unexpected delimiter ~s
next symb: ~a" char (peek-char t stream t nil t))))

(defun read-next-object (sep del &optional (stream *standard-input*))
  (let ((*readtable* (copy-readtable)))
    (flet ((peek- () (peek-char t stream t nil t))
          (discard- () (read-char stream t nil t)))
     (if (and del (char= (peek-) del))
         (progn (discard-) nil)
         (let ((o (read stream t nil t))
               (nxt (peek-)))
           (cond ((char= nxt sep) (discard-))
                 ((and del (char= nxt del)) nil)
                 (t nxt)) ; (t (error "Unexpected next char: ~S" nxt)) ; why was this here?
           o)))))

(defun -read-left-curly-brace (stream char) ; {} ; sel
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (loop for o = (read-next-object #\Space #\} stream)
          while o collect o into objects
          finally (return `(?select ,@objects)))))

(defun -read-left-bracket (stream char) ; [] ; filter
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (loop for o = (read-next-object #\Space #\] stream)
          while o collect o into objects
          finally (return `(?filter ,@objects)))))

(set-dispatch-macro-character #\# #\{ ; #{} ; sel
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    (let ((*readtable* (copy-readtable)))
      (loop for o = (read-next-object #\Space #\} stream)
            while o collect o into objects
            finally (return `(*$ ,@objects))))))

(set-dispatch-macro-character #\# #\[ ; #[] ; sel
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    (let ((*readtable*  (copy-readtable)))
      (loop for o = (read-next-object #\Space #\] stream)
            while o collect o into objects
            finally (return `($* ,@objects))))))

