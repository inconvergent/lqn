#### LQN:??

```
 ; LQN:??
 ;   [symbol]
 ; 
 ; ?? names a macro:
 ;   Lambda-list: (A EXPR &OPTIONAL RES)
 ;   Documentation:
 ;     evaluate expr only iff a is not nil. returns the result of expr or res; or nil.
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:FLATN$

```
 ; LQN:FLATN$
 ;   [symbol]
 ; 
 ; FLATN$ names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (HASH-TABLE)
 ;                  (VALUES (SIMPLE-ARRAY T) &OPTIONAL))
 ;   Documentation:
 ;     flatten ht to vector: k0 v0 k1 v1 ...
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:HEAD\*

```
 ; LQN:HEAD*
 ;   [symbol]
 ; 
 ; HEAD* names a compiled function:
 ;   Lambda-list: (S &OPTIONAL (N 10) &AUX (L (LENGTH S)))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL FIXNUM)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     first ±n elements
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:ISUBX?

```
 ; LQN:ISUBX?
 ;   [symbol]
 ; 
 ; ISUBX? names a compiled function:
 ;   Lambda-list: (S SUB)
 ;   Derived type: (FUNCTION (STRING STRING) *)
 ;   Documentation:
 ;     ignore case subx?
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Lambda-list: (&OPTIONAL (S *STANDARD-INPUT*))
 ;   Derived type: (FUNCTION (&OPTIONAL T) *)
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/io.lisp
```

#### LQN:LST?

```
 ; LQN:LST?
 ;   [symbol]
 ; 
 ; LST? names a compiled function:
 ;   Lambda-list: (L &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     l if list; or d
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:PNUM

```
:missing:todo:

 ; LQN:PNUM
 ;   [symbol]
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:PROC-QRY

```
 ; LQN:PROC-QRY
 ;   [symbol]
 ; 
 ; PROC-QRY names a compiled function:
 ;   Lambda-list: (CONF* Q)
 ;   Derived type: (FUNCTION (T T) (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     compile lqn query
 ;   Source file: /data/x/lqn/src/qry.lisp
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
 ;   Lambda-list: (DAT Q &KEY CONF DB)
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
 ;   Lambda-list: (DAT Q &KEY CONF DB)
 ;   Derived type: (FUNCTION (T T &KEY (:CONF T) (:DB T)) *)
 ;   Documentation:
 ;     compile lqn query and run on dat
 ;   Source file: /data/x/lqn/src/qry.lisp
```

#### LQN:REPL

```
 ; LQN:REPL
 ;   [symbol]
 ; 
 ; REPL names a compiled function:
 ;   Lambda-list: (S FROM TO)
 ;   Derived type: (FUNCTION (STRING STRING STRING)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     replace from with to in s
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:SEL\*

```
 ; LQN:SEL*
 ;   [symbol]
 ; 
 ; SEL* names a compiled function:
 ;   Lambda-list: (V &REST SEQS)
 ;   Derived type: (FUNCTION (VECTOR &REST T)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     new vector with indices or ranges from v.
 ;     ranges are lists that behave like arguments to seq*.
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:SEQ\*

```
 ; LQN:SEQ*
 ;   [symbol]
 ; 
 ; SEQ* names a compiled function:
 ;   Lambda-list: (V I &OPTIONAL J)
 ;   Derived type: (FUNCTION (VECTOR FIXNUM &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     (subseq v ,@rest)
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:SIZE

```
 ; LQN:SIZE
 ;   [symbol]
 ; 
 ; SIZE names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (MOD 4611686018427387901) &OPTIONAL))
 ;   Documentation:
 ;     length of sequence l or number of keys in kv l.
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:SPLT

```
 ; LQN:SPLT
 ;   [symbol]
 ; 
 ; SPLT names a macro:
 ;   Lambda-list: (S X &OPTIONAL PRUNE)
 ;   Documentation:
 ;     split s at substrings x to vector.
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:STRCAT

```
 ; LQN:STRCAT
 ;   [symbol]
 ; 
 ; STRCAT names a compiled function:
 ;   Lambda-list: (&REST REST)
 ;   Derived type: FUNCTION
 ;   Documentation:
 ;     concatenate all strings in sequences
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
```

#### LQN:SUBX?

```
 ; LQN:SUBX?
 ;   [symbol]
 ; 
 ; SUBX? names a compiled function:
 ;   Lambda-list: (S SUB)
 ;   Derived type: (FUNCTION (STRING STRING)
 ;                  (VALUES (OR NULL (MOD 4611686018427387901)) &OPTIONAL))
 ;   Documentation:
 ;     returns index where substring matches s from left to right. otherwise nil
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:SYM?

```
 ; LQN:SYM?
 ;   [symbol]
 ; 
 ; SYM? names a compiled function:
 ;   Lambda-list: (S &OPTIONAL D)
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     s if sym; or d
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
```

#### LQN:TAIL\*

```
 ; LQN:TAIL*
 ;   [symbol]
 ; 
 ; TAIL* names a compiled function:
 ;   Lambda-list: (S &OPTIONAL (N 10) &AUX (L (LENGTH S)))
 ;   Derived type: (FUNCTION (SEQUENCE &OPTIONAL FIXNUM)
 ;                  (VALUES
 ;                   (OR LIST (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     last ±n elements
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/utils.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
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
 ;   Source file: /data/x/lqn/src/init.lisp
```

