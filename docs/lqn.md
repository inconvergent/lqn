#### LQN:??

```
 ; LQN:??
 ;   [symbol]
 ; 
 ; ?? names a macro:
 ;   Lambda-list: (A EXPR &OPTIONAL RES)
 ;   Documentation:
 ;     evaluate expr only iff a is not nil. returns the result of expr or res; or nil.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:@\*

```
 ; LQN:@*
 ;   [symbol]
 ; 
 ; @* names a compiled function:
 ;   Lambda-list: (A D &REST REST &AUX L)
 ;   Derived type: (FUNCTION (T T &REST T)
 ;                  (VALUES (OR SIMPLE-VECTOR NULL) &OPTIONAL))
 ;   Documentation:
 ;     pick these indices/keys from sequence/hash-table into new vector.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:@@

```
 ; LQN:@@
 ;   [symbol]
 ; 
 ; @@ names a compiled function:
 ;   Lambda-list: (A PATH &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) *)
 ;   Documentation:
 ;     get nested key (e.g. aa/2/bb) from nested structure of kv/vec
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:ALL?

```
 ; LQN:ALL?
 ;   [symbol]
 ; 
 ; ALL? names a compiled function:
 ;   Lambda-list: (V &OPTIONAL EMPTY)
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     check if all; or empty.
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:APPLY\*

```
 ; LQN:APPLY*
 ;   [symbol]
 ; 
 ; APPLY* names a macro:
 ;   Lambda-list: (FX V)
 ;   Documentation:
 ;     apply, but for sequences.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:ASCII

```
 ; LQN:ASCII
 ;   [symbol]
 ; 
 ; ASCII names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (FLOAT) *)
 ;   Documentation:
 ;     ascii char with this density.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:BAR

```
 ; LQN:BAR
 ;   [symbol]
 ; 
 ; BAR names a compiled function:
 ;   Lambda-list: (SIZE S &OPTIONAL (PAD  ) (BBB  ▏▎▍▌▋▊▉█) &AUX
 ;                 (L (LENGTH BBB)))
 ;   Derived type: (FUNCTION (FIXNUM FLOAT &OPTIONAL T T) *)
 ;   Documentation:
 ;     draw progress bar
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:CAT$

```
 ; LQN:CAT$
 ;   [symbol]
 ; 
 ; CAT$ names a compiled function:
 ;   Lambda-list: (&REST REST &AUX
 ;                 (RES (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION * (VALUES HASH-TABLE &OPTIONAL))
 ;   Documentation:
 ;     add all keys from all hash tables in rest. left to right.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:CAT\*

```
 ; LQN:CAT*
 ;   [symbol]
 ; 
 ; CAT* names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION *
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     concatenate sequences in rest to vector
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:CD

```
 ; LQN:CD
 ;   [symbol]
 ; 
 ; CD names a compiled function:
 ;   Lambda-list: (PATH)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     change dir.
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:CLMP

```
 ; LQN:CLMP
 ;   [symbol]
 ; 
 ; CLMP names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (A 0.0) (B 1.0))
 ;   Derived type: (FUNCTION (NUMBER &OPTIONAL NUMBER NUMBER)
 ;                  (VALUES REAL &OPTIONAL))
 ;   Documentation:
 ;     clamp to range (a b).
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:CMD

```
 ; LQN:CMD
 ;   [symbol]
 ; 
 ; CMD names a compiled function:
 ;   Lambda-list: (FX &REST ARGS)
 ;   Derived type: (FUNCTION (STRING &REST T) (VALUES T T T &OPTIONAL))
 ;   Documentation:
 ;     run terminal command
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:CNT

```
:missing:todo:

 ; LQN:CNT
 ;   [symbol]
```

#### LQN:COMPCT

```
 ; LQN:COMPCT
 ;   [symbol]
 ; 
 ; COMPCT names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:CWD

```
 ; LQN:CWD
 ;   [symbol]
 ; 
 ; CWD names a compiled function:
 ;   Lambda-list: ()
 ;   Derived type: (FUNCTION NIL *)
 ;   Documentation:
 ;     current working dir.
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:D?

```
 ; LQN:D?
 ;   [symbol]
 ; 
 ; D? names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     describe symbol.
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:DAT-EXPORT

```
 ; LQN:DAT-EXPORT
 ;   [symbol]
 ; 
 ; DAT-EXPORT names a compiled function:
 ;   Lambda-list: (FN O &OPTIONAL (PFX .dat))
 ;   Derived type: (FUNCTION (STRING T &OPTIONAL STRING)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     write o to file. see dat-read-file
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:DAT-READ-FILE

```
 ; LQN:DAT-READ-FILE
 ;   [symbol]
 ; 
 ; DAT-READ-FILE names a compiled function:
 ;   Lambda-list: (FN &AUX (RES (MAV)))
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read lisp data from file into vector. see dat-export.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:DAT-READ-FILES

```
 ; LQN:DAT-READ-FILES
 ;   [symbol]
 ; 
 ; DAT-READ-FILES names a compiled function:
 ;   Lambda-list: (PATH-OR-SEQ)
 ;   Derived type: (FUNCTION ((OR LIST VECTOR)) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read lisp data from these paths (via lqn:ls) or this list of files as one large vector.
 ;   Source file: /data/x/lqn/src/qry-extra.lisp
```

#### LQN:DAT-READ-ONE

```
 ; LQN:DAT-READ-ONE
 ;   [symbol]
 ; 
 ; DAT-READ-ONE names a compiled function:
 ;   Lambda-list: (FN)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read lisp one object from file. see dat-export.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:DAT-READ-STREAM

```
 ; LQN:DAT-READ-STREAM
 ;   [symbol]
 ; 
 ; DAT-READ-STREAM names a compiled function:
 ;   Lambda-list: (S &AUX (RES (MAV)))
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read lisp data from stream into vector.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:DIR?

```
 ; LQN:DIR?
 ;   [symbol]
 ; 
 ; DIR? names a compiled function:
 ;   Lambda-list: (PATH)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     does this dir exist?
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:EMPTY?

```
:missing:todo:

 ; LQN:EMPTY?
 ;   [symbol]
 ; 
 ; EMPTY? names a compiled function:
 ;   Lambda-list: (L &OPTIONAL D &AUX (N (SIZE? L)))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:FILE?

```
 ; LQN:FILE?
 ;   [symbol]
 ; 
 ; FILE? names a compiled function:
 ;   Lambda-list: (PATH)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     does this file exist?
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:FLATALL\*

```
 ; LQN:FLATALL*
 ;   [symbol]
 ; 
 ; FLATALL* names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (STR NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES VECTOR &OPTIONAL))
 ;   Documentation:
 ;     flatten all sequences into new vector. if str is t strings will become
 ;       individual chars.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:FLATN$

```
 ; LQN:FLATN$
 ;   [symbol]
 ; 
 ; FLATN$ names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (HASH-TABLE) (VALUES SIMPLE-VECTOR &OPTIONAL))
 ;   Documentation:
 ;     flatten ht to vector: k0 v0 k1 v1 ...
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:FLATN\*

```
 ; LQN:FLATN*
 ;   [symbol]
 ; 
 ; FLATN* names a compiled function:
 ;   Lambda-list: (A &OPTIONAL (N 1) (STR NIL))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL FIXNUM T)
 ;                  (VALUES SEQUENCE &OPTIONAL))
 ;   Documentation:
 ;     flatten n times
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:FLT!?

```
 ; LQN:FLT!?
 ;   [symbol]
 ; 
 ; FLT!? names a compiled function:
 ;   Lambda-list: (F &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     f as float if it can be parsed; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:FLT?

```
 ; LQN:FLT?
 ;   [symbol]
 ; 
 ; FLT? names a compiled function:
 ;   Lambda-list: (F &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     f if float; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:FMT

```
 ; LQN:FMT
 ;   [symbol]
 ; 
 ; FMT names a macro:
 ;   Lambda-list: (S &REST REST)
 ;   Documentation:
 ;     format to string.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:GRP

```
:missing:todo:

 ; LQN:GRP
 ;   [symbol]
 ; 
 ; GRP names a compiled function:
 ;   Lambda-list: (V KEYFX &OPTIONAL (VALFX (FUNCTION IDENTITY)))
 ;   Derived type: (FUNCTION (SEQUENCE FUNCTION &OPTIONAL FUNCTION)
 ;                  (VALUES HASH-TABLE &OPTIONAL))
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:HEAD

```
 ; LQN:HEAD
 ;   [symbol]
 ; 
 ; HEAD names a compiled function:
 ;   Lambda-list: (S &OPTIONAL (N 10) &AUX (L (LENGTH S)))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL FIXNUM)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     first ±n elements
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:IND\*

```
 ; LQN:IND*
 ;   [symbol]
 ; 
 ; IND* names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (VECTOR &OPTIONAL FIXNUM)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get index.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:INT!?

```
 ; LQN:INT!?
 ;   [symbol]
 ; 
 ; INT!? names a compiled function:
 ;   Lambda-list: (I &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     i as int if it can be parsed; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:INT?

```
 ; LQN:INT?
 ;   [symbol]
 ; 
 ; INT? names a compiled function:
 ;   Lambda-list: (I &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     i if int; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:INUM

```
:missing:todo:

 ; LQN:INUM
 ;   [symbol]
```

#### LQN:IPREF?

```
 ; LQN:IPREF?
 ;   [symbol]
 ; 
 ; IPREF? names a compiled function:
 ;   Lambda-list: (S SUF &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T) *)
 ;   Documentation:
 ;     ignore case pref?
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:IS?

```
 ; LQN:IS?
 ;   [symbol]
 ; 
 ; IS? names a compiled function:
 ;   Lambda-list: (K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     k if k is not nil, empty sequence, or empty hash-table; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:ISUB?

```
 ; LQN:ISUB?
 ;   [symbol]
 ; 
 ; ISUB? names a compiled function:
 ;   Lambda-list: (S SUB &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     ignore case sub?
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:ISUBX?

```
 ; LQN:ISUBX?
 ;   [symbol]
 ; 
 ; ISUBX? names a compiled function:
 ;   Lambda-list: (S SUB)
 ;   Derived type: (FUNCTION (STRING STRING)
 ;                  (VALUES (OR (MOD 17592186044415) NULL) &OPTIONAL))
 ;   Documentation:
 ;     ignore case subx?
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:ISUF?

```
 ; LQN:ISUF?
 ;   [symbol]
 ; 
 ; ISUF? names a compiled function:
 ;   Lambda-list: (S SUF &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     ignore case suf?
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:JOIN

```
 ; LQN:JOIN
 ;   [symbol]
 ; 
 ; JOIN names a macro:
 ;   Lambda-list: (V &REST S)
 ;   Documentation:
 ;     join sequence v with s into new string.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:JSNLOADF

```
 ; LQN:JSNLOADF
 ;   [symbol]
 ; 
 ; JSNLOADF names a compiled function:
 ;   Lambda-list: (FN)
 ;   Derived type: (FUNCTION (STRING) *)
 ;   Documentation:
 ;     parse json from file, fn
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:JSNLOADS

```
 ; LQN:JSNLOADS
 ;   [symbol]
 ; 
 ; JSNLOADS names a compiled function:
 ;   Lambda-list: (&OPTIONAL (S *STANDARD-INPUT*) ALL)
 ;   Derived type: (FUNCTION (&OPTIONAL T T) *)
 ;   Documentation:
 ;     parse json from stream; or *standard-input*
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:JSNOUT

```
 ; LQN:JSNOUT
 ;   [symbol]
 ; 
 ; JSNOUT names a compiled function:
 ;   Lambda-list: (O &KEY (S *STANDARD-OUTPUT*) INDENT)
 ;   Derived type: (FUNCTION (T &KEY (:S STREAM) (:INDENT BOOLEAN))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     stream serialized json from o to s; or *standard-output*
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:JSNQRYF

```
 ; LQN:JSNQRYF
 ;   [symbol]
 ; 
 ; JSNQRYF names a macro:
 ;   Lambda-list: (FN Q &KEY DB)
 ;   Documentation:
 ;     run lqn query on json file, fn
 ;   Source file: /data/x/lqn/src/qry.lisp
```

#### LQN:JSNSTR

```
 ; LQN:JSNSTR
 ;   [symbol]
 ; 
 ; JSNSTR names a compiled function:
 ;   Lambda-list: (O &KEY INDENT (S (MAKE-STRING-OUTPUT-STREAM)))
 ;   Derived type: (FUNCTION (T &KEY (:INDENT BOOLEAN) (:S T))
 ;                  (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     serialize o as json to string
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:KEYS?

```
 ; LQN:KEYS?
 ;   [symbol]
 ; 
 ; KEYS? names a compiled function:
 ;   Lambda-list: (KV &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     vector with keys from kv; or d
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:KV?

```
 ; LQN:KV?
 ;   [symbol]
 ; 
 ; KV? names a compiled function:
 ;   Lambda-list: (K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     k if kv; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:KW?

```
 ; LQN:KW?
 ;   [symbol]
 ; 
 ; KW? names a compiled function:
 ;   Lambda-list: (K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     k if kw; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:LDNLOAD

```
 ; LQN:LDNLOAD
 ;   [symbol]
 ; 
 ; LDNLOAD names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read serialized data. reverse of ldnout.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:LDNOUT

```
 ; LQN:LDNOUT
 ;   [symbol]
 ; 
 ; LDNOUT names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     serialize internal representation to readable lisp data.
 ;     most notably lists and vectors are serialized as #(..), and hash-tables are serialized
 ;     as alists. see ldnload.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:LINSPACE

```
 ; LQN:LINSPACE
 ;   [symbol]
 ; 
 ; LINSPACE names a compiled function:
 ;   Lambda-list: (N &OPTIONAL (A 0.0) (B 1.0) (END T))
 ;   Derived type: (FUNCTION (FIXNUM &OPTIONAL REAL REAL BOOLEAN)
 ;                  (VALUES (AND (VECTOR T) (NOT SIMPLE-ARRAY)) &OPTIONAL))
 ;   Documentation:
 ;     n floats from a to b.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:LPAD

```
 ; LQN:LPAD
 ;   [symbol]
 ; 
 ; LPAD names a compiled function:
 ;   Lambda-list: (S N &OPTIONAL (C  ))
 ;   Derived type: (FUNCTION (STRING FIXNUM &OPTIONAL T) *)
 ;   Documentation:
 ;     left pad to length n. always of length n.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:LS

```
 ; LQN:LS
 ;   [symbol]
 ; 
 ; LS names a compiled function:
 ;   Lambda-list: (&OPTIONAL (PATTERN *.*))
 ;   Derived type: (FUNCTION (&OPTIONAL T) *)
 ;   Documentation:
 ;     list dir contents at this pattern.
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:LST!

```
 ; LQN:LST!
 ;   [symbol]
 ; 
 ; LST! names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (D `(,V)))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     coerce v to list if v; else d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:LST!?

```
 ; LQN:LST!?
 ;   [symbol]
 ; 
 ; LST!? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     v as list if it can be a list; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:LST?

```
 ; LQN:LST?
 ;   [symbol]
 ; 
 ; LST? names a compiled function:
 ;   Lambda-list: (V &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     v if list; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:MSYM?

```
 ; LQN:MSYM?
 ;   [symbol]
 ; 
 ; MSYM? names a macro:
 ;   Lambda-list: (A B &OPTIONAL D)
 ;   Documentation:
 ;     compare symbol a to b. if b is a keword or symbol
 ;     a perfect match is required. if b is a string it performs a substring
 ;     match. If b is an expression, a is compared to the evaluated value of b.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:NEW$

```
 ; LQN:NEW$
 ;   [symbol]
 ; 
 ; NEW$ names a macro:
 ;   Lambda-list: (&REST D)
 ;   Documentation:
 ;     new kv/hash-table from these (k v) pairs
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:NEW\*

```
 ; LQN:NEW*
 ;   [symbol]
 ; 
 ; NEW* names a macro:
 ;   Lambda-list: (&REST D)
 ;   Documentation:
 ;     new vector with these elements
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:NONE?

```
 ; LQN:NONE?
 ;   [symbol]
 ; 
 ; NONE? names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (EMPTY T))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     check if none; or empty.
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:NOOP

```
 ; LQN:NOOP
 ;   [symbol]
 ; 
 ; NOOP names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     do nothing. return nil.
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:NOW

```
 ; LQN:NOW
 ;   [symbol]
 ; 
 ; NOW names a compiled function:
 ;   Lambda-list: ()
 ;   Derived type: (FUNCTION NIL *)
 ;   Documentation:
 ;     timestamp.
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:NSTR

```
 ; LQN:NSTR
 ;   [symbol]
 ; 
 ; NSTR names a compiled function:
 ;   Lambda-list: (N &OPTIONAL (C  ))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY CHARACTER (*)) &OPTIONAL))
 ;   Documentation:
 ;     str of length n, filled with c
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:NUM!?

```
 ; LQN:NUM!?
 ;   [symbol]
 ; 
 ; NUM!? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     n as number if it can be parsed; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:NUM?

```
 ; LQN:NUM?
 ;   [symbol]
 ; 
 ; NUM? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     n if number; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:OUT

```
 ; LQN:OUT
 ;   [symbol]
 ; 
 ; OUT names a macro:
 ;   Lambda-list: (S &REST REST)
 ;   Documentation:
 ;     print to standard out
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:PATH?

```
 ; LQN:PATH?
 ;   [symbol]
 ; 
 ; PATH? names a compiled function:
 ;   Lambda-list: (PATH)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     does this path exist?
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:PNUM

```
:missing:todo:

 ; LQN:PNUM
 ;   [symbol]
```

#### LQN:POP\*

```
 ; LQN:POP*
 ;   [symbol]
 ; 
 ; POP* names a macro:
 ;   Lambda-list: (A &OPTIONAL D)
 ;   Documentation:
 ;     remove element from end of a. return last element. destructive.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:PREF?

```
 ; LQN:PREF?
 ;   [symbol]
 ; 
 ; PREF? names a compiled function:
 ;   Lambda-list: (S PREF &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if s starts with pref; or d
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:PROC-QRY

```
 ; LQN:PROC-QRY
 ;   [symbol]
 ; 
 ; PROC-QRY names a compiled function:
 ;   Lambda-list: (Q &OPTIONAL CONF*)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     compile lqn query
 ;   Source file: /data/x/lqn/src/qry.lisp
```

#### LQN:PSH\*

```
 ; LQN:PSH*
 ;   [symbol]
 ; 
 ; PSH* names a macro:
 ;   Lambda-list: (A O)
 ;   Documentation:
 ;     extend a with o. return a. destructive.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:QRY

```
 ; LQN:QRY
 ;   [symbol]
 ; 
 ; QRY names a macro:
 ;   Lambda-list: (DAT &REST REST)
 ;   Documentation:
 ;     query data. rest is wrapped in the pipe operator.
 ;   Source file: /data/x/lqn/src/qry.lisp
```

#### LQN:QRYD

```
 ; LQN:QRYD
 ;   [symbol]
 ; 
 ; QRYD names a macro:
 ;   Lambda-list: (DAT Q &KEY DB)
 ;   Documentation:
 ;     run lqn query on dat
 ;   Source file: /data/x/lqn/src/qry.lisp
```

#### LQN:QRYL

```
 ; LQN:QRYL
 ;   [symbol]
 ; 
 ; QRYL names a compiled function:
 ;   Lambda-list: (DAT Q &KEY DB)
 ;   Derived type: (FUNCTION (T T &KEY (:DB T)) *)
 ;   Documentation:
 ;     compile lqn query and run on dat
 ;   Source file: /data/x/lqn/src/qry.lisp
```

#### LQN:RANGE

```
 ; LQN:RANGE
 ;   [symbol]
 ; 
 ; RANGE names a compiled function:
 ;   Lambda-list: (A &OPTIONAL (B 0 B?) (LEAP 1))
 ;   Derived type: (FUNCTION (FIXNUM &OPTIONAL FIXNUM FIXNUM)
 ;                  (VALUES (AND (VECTOR T) (NOT SIMPLE-ARRAY)) &OPTIONAL))
 ;   Documentation:
 ;     declare range. from 0 to a; or a to b.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:REPL

```
 ; LQN:REPL
 ;   [symbol]
 ; 
 ; REPL names a compiled function:
 ;   Lambda-list: (S FROM TO)
 ;   Derived type: (FUNCTION (STRING STRING STRING)
 ;                  (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     replace from with to in s
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:RPAD

```
 ; LQN:RPAD
 ;   [symbol]
 ; 
 ; RPAD names a compiled function:
 ;   Lambda-list: (S N &OPTIONAL (C  ) &AUX (L (LENGTH S)))
 ;   Derived type: (FUNCTION (STRING FIXNUM &OPTIONAL T) *)
 ;   Documentation:
 ;     right pad to length n. always of length n.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:SDWN

```
 ; LQN:SDWN
 ;   [symbol]
 ; 
 ; SDWN names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     mkstr and downcase
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SEL

```
 ; LQN:SEL
 ;   [symbol]
 ; 
 ; SEL names a compiled function:
 ;   Lambda-list: (V &REST SEQS)
 ;   Derived type: (FUNCTION (VECTOR &REST T)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     new vector with indices or ranges from v.
 ;     ranges are lists that behave like arguments to seq.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SEQ

```
 ; LQN:SEQ
 ;   [symbol]
 ; 
 ; SEQ names a compiled function:
 ;   Lambda-list: (V I &OPTIONAL J)
 ;   Derived type: (FUNCTION (VECTOR FIXNUM &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     (subseq v ,@rest)
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SEQ!?

```
 ; LQN:SEQ!?
 ;   [symbol]
 ; 
 ; SEQ!? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s as seq if it can be parsed; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:SEQ?

```
 ; LQN:SEQ?
 ;   [symbol]
 ; 
 ; SEQ? names a compiled function:
 ;   Lambda-list: (S &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if sequence; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:SIZE?

```
 ; LQN:SIZE?
 ;   [symbol]
 ; 
 ; SIZE? names a compiled function:
 ;   Lambda-list: (L &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     length of sequence/number of keys in kv.
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:SOME?

```
 ; LQN:SOME?
 ;   [symbol]
 ; 
 ; SOME? names a compiled function:
 ;   Lambda-list: (V &OPTIONAL EMPTY)
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     check if some; or empty.
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:SPLT

```
 ; LQN:SPLT
 ;   [symbol]
 ; 
 ; SPLT names a macro:
 ;   Lambda-list: (S X &OPTIONAL (TRIM T) PRUNE)
 ;   Documentation:
 ;     split s at substrings x to vector. trims whitespace by default. prune removes empty strings.
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SRT

```
 ; LQN:SRT
 ;   [symbol]
 ; 
 ; SRT names a compiled function:
 ;   Lambda-list: (L &OPTIONAL (DIR S<) (KEY (FUNCTION IDENTITY)))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T) (VALUES SEQUENCE &OPTIONAL))
 ;   Documentation:
 ;     sort sequence by: s<, s> < >
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SSYM?

```
 ; LQN:SSYM?
 ;   [symbol]
 ; 
 ; SSYM? names a compiled function:
 ;   Lambda-list: (S &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if sym, not kw; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:STR!

```
 ; LQN:STR!
 ;   [symbol]
 ; 
 ; STR! names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     coerce to string
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:STR!?

```
 ; LQN:STR!?
 ;   [symbol]
 ; 
 ; STR!? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s as str if it can be parsed; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:STR?

```
 ; LQN:STR?
 ;   [symbol]
 ; 
 ; STR? names a compiled function:
 ;   Lambda-list: (S &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if string; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:STRCAT

```
 ; LQN:STRCAT
 ;   [symbol]
 ; 
 ; STRCAT names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     concatenate all strings in these sequences
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SUB?

```
 ; LQN:SUB?
 ;   [symbol]
 ; 
 ; SUB? names a compiled function:
 ;   Lambda-list: (S SUB &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if sub is substring of s; or d
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SUBDIR

```
 ; LQN:SUBDIR
 ;   [symbol]
 ; 
 ; SUBDIR names a compiled function:
 ;   Lambda-list: (&OPTIONAL (PATH (CWD)))
 ;   Derived type: (FUNCTION (&OPTIONAL T) *)
 ;   Documentation:
 ;     list subdirectories.
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:SUBFILES

```
 ; LQN:SUBFILES
 ;   [symbol]
 ; 
 ; SUBFILES names a compiled function:
 ;   Lambda-list: (&OPTIONAL (PATH (CWD)))
 ;   Derived type: (FUNCTION (&OPTIONAL T) *)
 ;   Documentation:
 ;     list subdirectories.
 ;   Source file: /data/x/lqn/src/sh.lisp
```

#### LQN:SUBX?

```
 ; LQN:SUBX?
 ;   [symbol]
 ; 
 ; SUBX? names a compiled function:
 ;   Lambda-list: (S SUB)
 ;   Derived type: (FUNCTION (STRING STRING)
 ;                  (VALUES (OR (MOD 17592186044415) NULL) &OPTIONAL))
 ;   Documentation:
 ;     returns index where substring matches s from left to right. otherwise nil
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SUF?

```
 ; LQN:SUF?
 ;   [symbol]
 ; 
 ; SUF? names a compiled function:
 ;   Lambda-list: (S SUF &OPTIONAL D)
 ;   Derived type: (FUNCTION (STRING STRING &OPTIONAL T)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if s ends with suf; or d
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SUP

```
 ; LQN:SUP
 ;   [symbol]
 ; 
 ; SUP names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SIMPLE-STRING &OPTIONAL))
 ;   Documentation:
 ;     mkstr and upcase
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:SYM!

```
 ; LQN:SYM!
 ;   [symbol]
 ; 
 ; SYM! names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: (FUNCTION * (VALUES SYMBOL &OPTIONAL))
 ;   Documentation:
 ;     stringify, make symbol
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:SYMB

```
 ; LQN:SYMB
 ;   [symbol]
 ; 
 ; SYMB names a compiled function:
 ;   Lambda-list: (&REST ARGS)
 ;   Derived type: (FUNCTION * (VALUES SYMBOL &OPTIONAL))
 ;   Documentation:
 ;     mkstr, make symbol.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:TAIL

```
 ; LQN:TAIL
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
 ;     last ±n elements
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:TRIM

```
 ; LQN:TRIM
 ;   [symbol]
 ; 
 ; TRIM names a compiled function:
 ;   Lambda-list: (S &OPTIONAL DEFAULT
 ;                 (CHARS
 ;                  (QUOTE
 ;                   (  
 ; 
 ;                     	 
 ; 
 ;                      ))))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     trim string
 ;   Source file: /data/x/lqn/src/qry-utils.lisp
```

#### LQN:TXT-EXPORT

```
 ; LQN:TXT-EXPORT
 ;   [symbol]
 ; 
 ; TXT-EXPORT names a compiled function:
 ;   Lambda-list: (FN V &OPTIONAL (PFX .txt))
 ;   Derived type: (FUNCTION (STRING VECTOR &OPTIONAL STRING)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     write lines from vector to file. see txt-read-file
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:TXT-READ-FILE

```
 ; LQN:TXT-READ-FILE
 ;   [symbol]
 ; 
 ; TXT-READ-FILE names a compiled function:
 ;   Lambda-list: (FN &AUX (RES (MAKE-ADJUSTABLE-VECTOR)))
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read lines of text from file into vector.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:TXT-READ-STREAM

```
 ; LQN:TXT-READ-STREAM
 ;   [symbol]
 ; 
 ; TXT-READ-STREAM names a compiled function:
 ;   Lambda-list: (&OPTIONAL (S *STANDARD-INPUT*) &AUX
 ;                 (RES (MAKE-ADJUSTABLE-VECTOR)))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     read lines of text from stream into vector.
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:UNIQ

```
 ; LQN:UNIQ
 ;   [symbol]
 ; 
 ; UNIQ names a compiled function:
 ;   Lambda-list: (S &OPTIONAL (FX (FUNCTION EQUAL)))
 ;   Derived type: (FUNCTION (T &OPTIONAL FUNCTION)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     remove duplicates from sequence
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:V?

```
 ; LQN:V?
 ;   [symbol]
 ; 
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T) &AUX
 ;                 (V
 ;                  (SLOT-VALUE (FIND-SYSTEM (QUOTE LQN))
 ;                              (QUOTE VERSION))))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     return/print lqn version.
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:VEC!

```
 ; LQN:VEC!
 ;   [symbol]
 ; 
 ; VEC! names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (D `#(,V)))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     coerce v to vector. if v is not a vector, list, string it returns d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:VEC!?

```
 ; LQN:VEC!?
 ;   [symbol]
 ; 
 ; VEC!? names a compiled function:
 ;   Lambda-list: (N &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     v as vector if it can be parsed; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

#### LQN:VEC?

```
 ; LQN:VEC?
 ;   [symbol]
 ; 
 ; VEC? names a compiled function:
 ;   Lambda-list: (V &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     v if vector; or d
 ;   Source file: /data/x/lqn/src/basic-utils.lisp
```

