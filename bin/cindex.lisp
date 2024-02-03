(defpackage lqn/code
  (:use #:cl #:lqn)
  (:export #:gwrite #:index #:gwrite #:cqry #:mget #:fget #:code))

(in-package :lqn/code)

; comma: http://christophe.rhodes.io/notes/blog/posts/2014/naive_vs_proper_code-walking/
(defvar *lower* 50)
(defvar *ext-nodes*
  '(:/ext :/ext/frag :/ext/atom :/ext/form :/ext/form-type :/ext/type :/ext/file
          :/ext/sys/none :/ext/sys :/ext/pkg/none :/ext/pkg :/ext/unknown))
(defvar *ft-nodes*
  '(:/ext/form-type :/ft/in-package :/ft/defpackage :/ft/defun :/ft/defmacro :/ft/defvar
                    :/ft/declaim :/ft/let :/ft/labels :/ft/set-macro-character :/ft/unknown))
(defvar *type-nodes*
  '(:/ext/type :/ty/keyword :/ty/symbol :/ty/string :/ty/character :/ty/cons :/ty/boolean
               :/ty/fixnum :/ty/float :/ty/number :/ty/comma :/ty/unknown))

(defun get-node-type (co o)
    (typecase o (cons :/ext/form) (sb-impl::comma :/ext/comma) (otherwise :/ext/atom)))

(defun get-form-type (co o)
  (fnd co (lqn::psymb :keyword :/ft/ (car o)) :/ft/unknown)) ; make new ft dynamically

(defun get-type (co s)
  (fnd co
    (typecase s (string :/ty/string) (keyword :/ty/keyword) (boolean   :/ty/boolean)
                (cons   :/ty/cons)   (symbol  :/ty/symbol)  (fixnum    :/ty/fixnum)
                (float  :/ty/float)  (number  :/ty/number)  (character :/ty/character)
                (sb-impl::comma :/ty/comma)
                (otherwise (warn "unexpected type: ~a" s) :/ty/unknown))))

;                                   :/ext/file :/ext/form
;                                   :/ext/form-type :/ext/type
;                                   :/ext/atom :/ext/unknown :/ext/pkg
; :/ext            :/ext            (EXT)
; :/ext/form-type  :/ext/form-type  (FORM-TYPE)
; :/ext/type       :/ext/type       (TYPE)
; :/ext/file       :/file           (atom: FILE)
; :/ext/atom       :/atom           (ATOM)
; :/ext/form       :/form           (FORM)
; :/ext/frag       :/frag           (atom: FRAG)
; :/ext/sys        :/sys            (atom: SYS)
; :/ext/pkg        :/pkg            (atom: PKG)

; :/ty/string
; :/ty/keyword
; :/ty/symbol
; (TYPE)           :/type           (ATOM)

; :/ft/defun
; :/ft/defmacro
; :/ft/defvar
; (FORM-TYPE)      :/form-type      (FORM)

; (atom: SYS)      :/sys/file       (atom: FILE)
; (atom: FILE)     :/file/form      (FORM)
; (FORM)           :/form/name      (atom: FORM-NAME)
; (FORM)           :/form/atom      (atom)

(defun no-pck (s) ; hacky.. need to actually handle symbol packages across the board
  (typecase s (boolean s) (number s) (keyword s)
              (sb-impl::comma (str! s))
              (symbol (lqn::psymb :cl-user (str! s))) (otherwise s)))

(defun proc-cqry (co q)
  (labels ((is?/ (s) (and (lqn:ssym? s) (pref? (str! s) "//")))
           (var (s) (sym! "?" (sup (seq* (str! s) 2))))
           (expr (s) `(fnd ,co ,(lqn::kw (seq* (str! s) 1))))
           (mklet (vars) (lqn:qry vars #((lst (var _) (expr _))))))
    (let ((in (uniq (lqn:qry q (?srch is?/)))))
      (lqn::awg (g) `(let ((,g (code-grp ,co)))
                       (grph:qry ,g ,@(when (some? in) `(:in ,(lst! (mklet in))))
                                    ,@(lqn:qry q (?txpr is?/ var))))))))

(defmacro cqry (co &rest rest) (proc-cqry co rest))

(defun allsym (o) (uniq (flatall* o 2)) )
(defun prt (o &optional s)
  (lqn::with-struct (code- i grp) o
    (format s "<@code (frags: ~a~%  ~a)>" i grp)))

; TODO: handle form mappings ambiguity
(defstruct (code (:constructor -make-code ()) (:print-object prt))
  (frags (make-array 500000 :initial-element nil))
  (frag->ind (make-hash-table :test #'equalp))
  (grp (grph:make)) (i 0))

(defun code (&aux (co (-make-code)))
  (loop for oo in `(,*ext-nodes* ,*ft-nodes* ,*type-nodes*)
        for root = (reg co (car oo) :lnk nil)
        do (setf (code-grp co) (grph:prop (code-grp co) root (car oo)))
        do (loop for o in (cdr oo) for i = (reg co o :lnk nil)
                 do (setf (code-grp co) (grph:prop (code-grp co) i o)) ; HEREHEREHER HERE
                    (lnk co root i `(,(car oo))))) ; use root as edge prop? /is
  (setf (code-i co) *lower*) ; fix this
  co)

(defun mget (co &rest rest) (apply #'sel* (code-frags co) rest))
(defun fget (co i)
  (declare (fixnum i))
  (lqn::with-struct (code- frags) co (aref frags i)))
(defun fnd (co frag &optional (d :/ext/unknown))
  (values (or (gethash (no-pck frag) (code-frag->ind co))
              (gethash d (code-frag->ind co)))))

(defun reg (co frag &key (lnk t) force)
  (labels ((reg (co frag frag-no)
             (lqn::with-struct (code- frag->ind i) co
               (setf (aref (code-frags co) i) frag-no)
               (incf (code-i co))
               (setf (gethash frag-no frag->ind) i)
               (when lnk (lnk co (get-type co frag) i '(:/type))
                              (lnk co (fnd co :/ext/frag) i '(:/frag)))
               i)))
    (let ((frag-no (no-pck frag)))
      (if force (reg co frag frag-no)
                (or (fnd co frag-no nil) (reg co frag frag-no))))))

(defun lnk (co parent child prop)
  (declare (fixnum parent child))
  (lqn::with-struct (code- grp) co
    (grph:add*! grp parent child -> prop)
    (setf (code-grp co) grp)
    child))
(defun attach (co parent frag prop &key force)
  (declare (fixnum parent))
  (lnk co parent (reg co frag :force force) prop))

(defun form-reg-name (co o)
  (if (and (> (length o) 1) (symbolp (second o))) (reg co (second o))
                                                  (fnd co :/ext/unknown)))
; TODO: form/atom has stringified name
(defun index (co fn &optional (sys :/ext/sys/none))
  (lqn::with-struct (code- frag->ind) co
    (labels
      ((-reg-file (sysi)
         (let ((fi (attach co (fnd co :/ext/file) fn '(:/file))))
           (lnk co (fnd co :/ext/sys) sysi '(:/ext/sys))
           (lnk co sysi fi '(:/sys/file))
           fi))
       (-reg-with-ext-type (co frag ty prop)
         (lnk co (fnd co ty) (reg co frag) prop))
       (-do-file-obj (fi pkgi o)
         (let ((oi (attach co (fnd co :/ext/form) o '(:/form) :force t)))
           (lnk co fi oi '(:/file/form))
           (lnk co pkgi oi '(:/pkg))

           (loop for atm across (allsym o)
                 for atmi = (-reg-with-ext-type co atm
                              (get-node-type co atm) '(:/atom))
                 do (lnk co oi atmi '(:/form/atom)))

           (typecase o
             (cons (lnk co (get-form-type co o) oi '(:/form-type))
                   (lnk co oi (form-reg-name co o) '(:/form/name)))))))

      (let ((sysi (reg co sys)))
        (loop with fi = (-reg-file sysi)
              with pkgi = (fnd co :/ext/pkg/none)
              for o across (lqn::read-file-as-data-vector fn)
              if (and (consp o) (ssym? (car o))
                      (eq :in-package (lqn::kw (car o))))
              do (setf pkgi (attach co (fnd co :/ext/pkg) (second o) '(:/ext/pkg)))
              do (-do-file-obj fi pkgi
                   (lqn:qry o (?mxpr (ssym? no-pck)))))))))

(defun gwrite (co fn)
  (lqn::with-struct (code- grp i frag->ind frags) co
    (grph/io:gwrite fn grp :meta `((:frags . ,(head* frags i))
                                   (:frag->ind . ,(flatn$ frag->ind))))))

