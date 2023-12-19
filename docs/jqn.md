#### JQN:

```
:missing:todo:

 ; JQN:||
 ;   [symbol]
```

#### JQN:$ADD%

```
 ; JQN:$ADD%
 ;   [symbol]
 ; 
 ; $ADD% names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (setf lft v) if v is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:$ADD+

```
 ; JQN:$ADD+
 ;   [symbol]
 ; 
 ; $ADD+ names a macro:
 ;   Lambda-list: (DAT LFT K V &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     do (setf lft (or v default))
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:$ADD?

```
 ; JQN:$ADD?
 ;   [symbol]
 ; 
 ; $ADD? names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (setf lft v) if (@_ k) is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:$DEL

```
 ; JQN:$DEL
 ;   [symbol]
 ; 
 ; $DEL names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     delete key
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:$NEW

```
:missing:todo:

 ; JQN:$NEW
 ;   [symbol]
```

#### JQN:$STACK

```
 ; JQN:$STACK
 ;   [symbol]
 ; 
 ; $STACK names a compiled function:
 ;   Lambda-list: (&REST REST &AUX
 ;                 (RES (MAKE-HASH-TABLE TEST (FUNCTION EQUAL))))
 ;   Derived type: (FUNCTION * (VALUES HASH-TABLE &OPTIONAL))
 ;   Documentation:
 ;     add all keys from all hash tables in rest. left to right.
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:\*ADD%

```
 ; JQN:*ADD%
 ;   [symbol]
 ; 
 ; *ADD% names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (vextend v lft) if v is not nil or empty
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:\*ADD+

```
 ; JQN:*ADD+
 ;   [symbol]
 ; 
 ; *ADD+ names a macro:
 ;   Lambda-list: (DAT LFT K V &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     do (vextend (or v default) lft)
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:\*ADD?

```
 ; JQN:*ADD?
 ;   [symbol]
 ; 
 ; *ADD? names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (vextend v lft) if (gethash k dat) is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:\*IND

```
 ; JQN:*IND
 ;   [symbol]
 ; 
 ; *IND names a macro:
 ;   Lambda-list: (O SEL)
 ;   Documentation:
 ;     get index or range from json array (vector).
 ;     if sel is an atom: (aref o ,sel)
 ;     if sel is cons: (subseq o ,@sel)
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:\*NEW

```
:missing:todo:

 ; JQN:*NEW
 ;   [symbol]
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
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR LIST HASH-TABLE (SIMPLE-ARRAY * (*))
 ;                       SB-KERNEL:EXTENDED-SEQUENCE)
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     remove none/nil, emtpy arrays, empty objects, empty keys and empty lists from `a`.
 ;   Source file: /data/x/jqn/src/jqn.lisp
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
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:@

```
 ; JQN:@
 ;   [symbol]
 ; 
 ; @ names a macro:
 ;   Lambda-list: (O K &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     get k from dict o; or default
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:@_

```
:missing:todo:

 ; JQN:@_
 ;   [symbol]
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

#### JQN:LDNOUT

```
 ; JQN:LDNOUT
 ;   [symbol]
 ; 
 ; LDNOUT names a compiled function:
 ;   Lambda-list: (O &OPTIONAL (KVKEYS T))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     serialize internal json representation to readable lisp data (ldn).
 ;   Source file: /data/x/jqn/src/io.lisp
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
 ;   Source file: /data/x/jqn/src/jqn.lisp
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
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:QRYD

```
 ; JQN:QRYD
 ;   [symbol]
 ; 
 ; QRYD names a macro:
 ;   Lambda-list: (DAT &KEY (Q _) CONF DB)
 ;   Documentation:
 ;     run jqn query on dat
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:QRYF

```
 ; JQN:QRYF
 ;   [symbol]
 ; 
 ; QRYF names a macro:
 ;   Lambda-list: (FN &KEY (Q _) DB)
 ;   Documentation:
 ;     run jqn query on file, fn
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:QRYL

```
 ; JQN:QRYL
 ;   [symbol]
 ; 
 ; QRYL names a compiled function:
 ;   Lambda-list: (DAT &KEY (Q _) CONF DB)
 ;   Derived type: (FUNCTION (T &KEY (:Q T) (:CONF T) (:DB T)) *)
 ;   Documentation:
 ;     compile jqn query and run on dat
 ;   Source file: /data/x/jqn/src/jqn.lisp
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
 ;     replace from with to in string s
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
 ;   Source file: /data/x/jqn/src/jqn.lisp
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
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

