(in-package :jqn)

(set-macro-character #\{ '-read-left-curly-brace)
(set-macro-character #\[ '-read-left-bracket)

(set-macro-character #\} '-read-delimiter)
(set-macro-character #\] '-read-delimiter)

(defun -read-delimiter (stream char)
  (error "jqn: unexpected delimiter ~s
next symb: ~a" char (peek-char t stream t nil t)))

(defun read-next-object (sep del &optional (stream *standard-input*))
  (flet ((peek- () (peek-char t stream t nil t))
         (discard- () (read-char stream t nil t)))
    (if (and del (char= (peek-) del))
        (progn (discard-) nil)
        (let ((o (read stream t nil t))
              (nxt (peek-)))
          (cond ((char= nxt sep) (discard-))
                ((and del (char= nxt del)) nil)
                (t nxt)
                ; (t (error "Unexpected next char: ~S" nxt)) ; why was this here?
                )
          o))))

(defun -read-left-curly-brace (stream char) ; {} ; sel
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (loop for o = (read-next-object #\Space #\} stream)
          while o collect o into objects
          finally (return `($$ ,@objects)))))

(defun -read-left-bracket (stream char) ; [] ; filter
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (loop for o = (read-next-object #\Space #\] stream)
          while o collect o into objects
          finally (return `(** ,@objects)))))

(set-dispatch-macro-character #\# #\{ ; #{} ; sel
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    (let ((*readtable* (copy-readtable)))
      (loop for o = (read-next-object #\Space #\} stream)
            while o collect o into objects
            finally (return `(*$ ,@objects))))))

; (make-dispatch-macro-character #\o)

(set-dispatch-macro-character #\# #\[ ; #[] ; sel
  (lambda (stream subchar arg)
    (declare (ignorable subchar arg))
    (let ((*readtable* (copy-readtable)))
      (loop for o = (read-next-object #\Space #\] stream)
            while o collect o into objects
            finally (return `($* ,@objects))))))
