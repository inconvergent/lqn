#### JQN:

```
:missing:todo:

 ; JQN:||
 ;   [symbol]
```

#### JQN:$

```
 ; JQN:$
 ;   [symbol]
 ; 
 ; $ names a macro:
 ;   Lambda-list: (O K &OPTIONAL D)
 ;   Documentation:
 ;     get key k from o
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:$_

```
:missing:todo:

 ; JQN:$_
 ;   [symbol]
```

#### JQN:$ADD%

```
 ; JQN:$ADD%
 ;   [symbol]
 ; 
 ; $ADD% names a macro:
 ;   Lambda-list: (LFT K V)
 ;   Documentation:
 ;     do (setf lft v) if v is not nil
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:$ADD+

```
 ; JQN:$ADD+
 ;   [symbol]
 ; 
 ; $ADD+ names a macro:
 ;   Lambda-list: (LFT K V &OPTIONAL D)
 ;   Documentation:
 ;     do (setf lft (or v default))
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:$ADD?

```
 ; JQN:$ADD?
 ;   [symbol]
 ; 
 ; $ADD? names a macro:
 ;   Lambda-list: (LFT K V)
 ;   Documentation:
 ;     do (setf lft v) if ($_ k) is not nil
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:$CAT

```
 ; JQN:$CAT
 ;   [symbol]
 ; 
 ; $CAT names a compiled function:
 ;   Lambda-list: (&REST REST &AUX
 ;                 (RES (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION * (VALUES HASH-TABLE &OPTIONAL))
 ;   Documentation:
 ;     add all keys from all hash tables in rest. left to right.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:$DEL

```
 ; JQN:$DEL
 ;   [symbol]
 ; 
 ; $DEL names a macro:
 ;   Lambda-list: (LFT K V)
 ;   Documentation:
 ;     delete key
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:$NEW

```
:missing:todo:

 ; JQN:$NEW
 ;   [symbol]
```

#### JQN:\*ADD%

```
 ; JQN:*ADD%
 ;   [symbol]
 ; 
 ; *ADD% names a macro:
 ;   Lambda-list: (LFT K V)
 ;   Documentation:
 ;     do (vex v lft) if v is not nil or empty
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:\*ADD+

```
 ; JQN:*ADD+
 ;   [symbol]
 ; 
 ; *ADD+ names a macro:
 ;   Lambda-list: (LFT K V &OPTIONAL D)
 ;   Documentation:
 ;     do (vex (or v default) lft)
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:\*ADD?

```
 ; JQN:*ADD?
 ;   [symbol]
 ; 
 ; *ADD? names a macro:
 ;   Lambda-list: (LFT K V)
 ;   Documentation:
 ;     do (vex v lft) if (gethash k dat) is not nil
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:\*CAT

```
 ; JQN:*CAT
 ;   [symbol]
 ; 
 ; *CAT names a compiled function:
 ;   Lambda-list: (&REST REST &AUX (RES (MAKE-ADJUSTABLE-VECTOR)))
 ;   Derived type: (FUNCTION *
 ;                  (VALUES (AND ARRAY (NOT SIMPLE-ARRAY)) &OPTIONAL))
 ;   Documentation:
 ;     concatenate all vectors in these vectors.
 ;     non-vectors are included in their position
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:\*IND

```
 ; JQN:*IND
 ;   [symbol]
 ; 
 ; *IND names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (VECTOR &OPTIONAL FIXNUM)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get this index from vector.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:\*NEW

```
:missing:todo:

 ; JQN:*NEW
 ;   [symbol]
```

#### JQN:\*SEL

```
 ; JQN:*SEL
 ;   [symbol]
 ; 
 ; *SEL names a compiled function:
 ;   Lambda-list: (V &REST SEQS)
 ;   Derived type: (FUNCTION (VECTOR &REST T)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     new vector with indices or ranges from v.
 ;     ranges are lists that behave like arguments to *seq
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:\*SEQ

```
 ; JQN:*SEQ
 ;   [symbol]
 ; 
 ; *SEQ names a compiled function:
 ;   Lambda-list: (V I &OPTIONAL J)
 ;   Derived type: (FUNCTION (VECTOR FIXNUM &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     (subseq v ,@rest)
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:<>

```
:missing:todo:

 ; JQN:<>
 ;   [symbol]
```

#### JQN:><

```
 ; JQN:><
 ;   [symbol]
 ; 
 ; >< names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:??

```
 ; JQN:??
 ;   [symbol]
 ; 
 ; ?? names a macro:
 ;   Lambda-list: (FX ARG &REST ARGS)
 ;   Documentation:
 ;     run (fx arg) only if arg is not nil.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:CNT

```
:missing:todo:

 ; JQN:CNT
 ;   [symbol]
```

#### JQN:CTX

```
:missing:todo:

 ; JQN:CTX
 ;   [symbol]
```

#### JQN:D?

```
 ; JQN:D?
 ;   [symbol]
 ; 
 ; D? names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     describe symbol.
 ;   Source file: /data/x/jqn/src/init.lisp
```

#### JQN:FI

```
:missing:todo:

 ; JQN:FI
 ;   [symbol]
```

#### JQN:FLT!?

```
 ; JQN:FLT!?
 ;   [symbol]
 ; 
 ; FLT!? names a compiled function:
 ;   Lambda-list: (F &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     f as float if it can be parsed as float; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:FLT?

```
 ; JQN:FLT?
 ;   [symbol]
 ; 
 ; FLT? names a compiled function:
 ;   Lambda-list: (F &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     f if float; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:FMT

```
 ; JQN:FMT
 ;   [symbol]
 ; 
 ; FMT names a macro:
 ;   Lambda-list: (S &REST REST)
 ;   Documentation:
 ;     format to string.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:FN

```
:missing:todo:

 ; JQN:FN
 ;   [symbol]
```

#### JQN:HEAD

```
 ; JQN:HEAD
 ;   [symbol]
 ; 
 ; HEAD names a compiled function:
 ;   Lambda-list: (S &OPTIONAL (N 10))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL FIXNUM)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     first n elements
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:INT!?

```
 ; JQN:INT!?
 ;   [symbol]
 ; 
 ; INT!? names a compiled function:
 ;   Lambda-list: (I &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     i as int if it can be parsed as int; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:INT?

```
 ; JQN:INT?
 ;   [symbol]
 ; 
 ; INT? names a compiled function:
 ;   Lambda-list: (I &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     i if int; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:IPREF?

```
 ; JQN:IPREF?
 ;   [symbol]
 ; 
 ; IPREF? names a compiled function:
 ;   Lambda-list: (S SUF &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) *)
 ;   Documentation:
 ;     case insensitive pref?
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:IS?

```
 ; JQN:IS?
 ;   [symbol]
 ; 
 ; IS? names a compiled function:
 ;   Lambda-list: (K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     k if k is not nil ,not an empty sequence, and not an empty hash-table; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:ISUB?

```
 ; JQN:ISUB?
 ;   [symbol]
 ; 
 ; ISUB? names a compiled function:
 ;   Lambda-list: (S SUB &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     case insensitive sub?
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:ISUBX?

```
 ; JQN:ISUBX?
 ;   [symbol]
 ; 
 ; ISUBX? names a compiled function:
 ;   Lambda-list: (S SUB)
 ;   Derived type: (FUNCTION (T T) *)
 ;   Documentation:
 ;     case insensitive subx?
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:ISUF?

```
 ; JQN:ISUF?
 ;   [symbol]
 ; 
 ; ISUF? names a compiled function:
 ;   Lambda-list: (S SUF &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) *)
 ;   Documentation:
 ;     case insensitive suf?
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:JSNLOADF

```
 ; JQN:JSNLOADF
 ;   [symbol]
 ; 
 ; JSNLOADF names a compiled function:
 ;   Lambda-list: (FN)
 ;   Derived type: (FUNCTION (STRING) *)
 ;   Documentation:
 ;     parse json from file, fn
 ;   Source file: /data/x/jqn/src/io.lisp
```

#### JQN:JSNLOADS

```
 ; JQN:JSNLOADS
 ;   [symbol]
 ; 
 ; JSNLOADS names a compiled function:
 ;   Lambda-list: (&OPTIONAL (S *STANDARD-INPUT*))
 ;   Derived type: (FUNCTION (&OPTIONAL T) *)
 ;   Documentation:
 ;     parse json from stream; or *standard-input*
 ;   Source file: /data/x/jqn/src/io.lisp
```

#### JQN:JSNOUT

```
 ; JQN:JSNOUT
 ;   [symbol]
 ; 
 ; JSNOUT names a compiled function:
 ;   Lambda-list: (O &KEY (S *STANDARD-OUTPUT*) INDENT)
 ;   Derived type: (FUNCTION (T &KEY (:S STREAM) (:INDENT BOOLEAN))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     stream encoded json from o to s; or *standard-output*
 ;   Source file: /data/x/jqn/src/io.lisp
```

#### JQN:JSNQRYF

```
 ; JQN:JSNQRYF
 ;   [symbol]
 ; 
 ; JSNQRYF names a macro:
 ;   Lambda-list: (FN Q &KEY DB)
 ;   Documentation:
 ;     run jqn query on json file, fn
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:KV?

```
 ; JQN:KV?
 ;   [symbol]
 ; 
 ; KV? names a compiled function:
 ;   Lambda-list: (K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     k if hash-table; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:LDNOUT

```
 ; JQN:LDNOUT
 ;   [symbol]
 ; 
 ; LDNOUT names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     serialize internal json representation to readable lisp data (ldn).
 ;   Source file: /data/x/jqn/src/io.lisp
```

#### JQN:LST?

```
 ; JQN:LST?
 ;   [symbol]
 ; 
 ; LST? names a compiled function:
 ;   Lambda-list: (L &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     l if list; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:MKSTR

```
 ; JQN:MKSTR
 ;   [symbol]
 ; 
 ; MKSTR names a compiled function:
 ;   Lambda-list: (&REST ARGS)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     coerce all arguments to a string.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:NOOP

```
 ; JQN:NOOP
 ;   [symbol]
 ; 
 ; NOOP names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     do nothing. return nil
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:NUM

```
:missing:todo:

 ; JQN:NUM
 ;   [symbol]
```

#### JQN:NUM!?

```
 ; JQN:NUM!?
 ;   [symbol]
 ; 
 ; NUM!? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     n as number if it can be parsed as number; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:NUM?

```
 ; JQN:NUM?
 ;   [symbol]
 ; 
 ; NUM? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     n if number; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:OUT

```
 ; JQN:OUT
 ;   [symbol]
 ; 
 ; OUT names a macro:
 ;   Lambda-list: (S &REST REST)
 ;   Documentation:
 ;     print to standard out
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:PAR

```
:missing:todo:

 ; JQN:PAR
 ;   [symbol]
```

#### JQN:PREF?

```
 ; JQN:PREF?
 ;   [symbol]
 ; 
 ; PREF? names a compiled function:
 ;   Lambda-list: (S PREF &OPTIONAL D &AUX (S (MKSTR S)))
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if s starts with pref; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:PROC-QRY

```
 ; JQN:PROC-QRY
 ;   [symbol]
 ; 
 ; PROC-QRY names a compiled function:
 ;   Lambda-list: (CONF* Q)
 ;   Derived type: (FUNCTION (T T) (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     compile jqn query
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:QRYD

```
 ; JQN:QRYD
 ;   [symbol]
 ; 
 ; QRYD names a macro:
 ;   Lambda-list: (DAT Q &KEY CONF DB)
 ;   Documentation:
 ;     run jqn query on dat
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:QRYL

```
 ; JQN:QRYL
 ;   [symbol]
 ; 
 ; QRYL names a compiled function:
 ;   Lambda-list: (DAT Q &KEY CONF DB)
 ;   Derived type: (FUNCTION (T T &KEY (:CONF T) (:DB T)) *)
 ;   Documentation:
 ;     compile jqn query and run on dat
 ;   Source file: /data/x/jqn/src/qry.lisp
```

#### JQN:REPL

```
 ; JQN:REPL
 ;   [symbol]
 ; 
 ; REPL names a compiled function:
 ;   Lambda-list: (S FROM TO)
 ;   Derived type: (FUNCTION (STRING STRING STRING)
 ;                  (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     replace from with to in s
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SDWN

```
 ; JQN:SDWN
 ;   [symbol]
 ; 
 ; SDWN names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     mkstr and downcase
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SEQ?

```
 ; JQN:SEQ?
 ;   [symbol]
 ; 
 ; SEQ? names a compiled function:
 ;   Lambda-list: (S &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if sequence; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SIZE

```
 ; JQN:SIZE
 ;   [symbol]
 ; 
 ; SIZE names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (MOD 4611686018427387901) &OPTIONAL))
 ;   Documentation:
 ;     length of sequence l or number of keys in kv l
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SPLIT

```
 ; JQN:SPLIT
 ;   [symbol]
 ; 
 ; SPLIT names a compiled function:
 ;   Lambda-list: (S X &KEY PRUNE &AUX (LX (LENGTH X)))
 ;   Derived type: (FUNCTION (STRING STRING &KEY (:PRUNE BOOLEAN))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     split string at substring. prune removes empty strings.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:STR?

```
 ; JQN:STR?
 ;   [symbol]
 ; 
 ; STR? names a compiled function:
 ;   Lambda-list: (S &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if string; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:STRCAT

```
 ; JQN:STRCAT
 ;   [symbol]
 ; 
 ; STRCAT names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     concatenate all strings in sequences rest
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SUB?

```
 ; JQN:SUB?
 ;   [symbol]
 ; 
 ; SUB? names a compiled function:
 ;   Lambda-list: (S SUB &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if sub is substring of s; ord
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SUBX?

```
 ; JQN:SUBX?
 ;   [symbol]
 ; 
 ; SUBX? names a compiled function:
 ;   Lambda-list: (S SUB)
 ;   Derived type: (FUNCTION (STRING STRING)
 ;                  (VALUES (OR NULL (MOD 4611686018427387901)) &OPTIONAL))
 ;   Documentation:
 ;     returns index where substring matches s from left to right. otherwise nil.
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SUF?

```
 ; JQN:SUF?
 ;   [symbol]
 ; 
 ; SUF? names a compiled function:
 ;   Lambda-list: (S SUF &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T) *)
 ;   Documentation:
 ;     s if s ends with suf; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:SUP

```
 ; JQN:SUP
 ;   [symbol]
 ; 
 ; SUP names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     mkstr and upcase
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:TAIL

```
 ; JQN:TAIL
 ;   [symbol]
 ; 
 ; TAIL names a compiled function:
 ;   Lambda-list: (S &OPTIONAL (N 10) &AUX (L (LENGTH S)))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL FIXNUM)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     last n elements
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:V?

```
 ; JQN:V?
 ;   [symbol]
 ; 
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T) &AUX
 ;                 (V
 ;                  (SLOT-VALUE (FIND-SYSTEM (QUOTE JQN))
 ;                              (QUOTE VERSION))))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     return/print jqn version.
 ;   Source file: /data/x/jqn/src/init.lisp
```

#### JQN:VEC?

```
 ; JQN:VEC?
 ;   [symbol]
 ; 
 ; VEC? names a compiled function:
 ;   Lambda-list: (V &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     v if vector; or d
 ;   Source file: /data/x/jqn/src/utils.lisp
```

