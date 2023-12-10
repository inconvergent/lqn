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

#### JQN:IND

```
 ; JQN:IND
 ;   [symbol]
 ; 
 ; IND names a macro:
 ;   Lambda-list: (O SEL)
 ;   Documentation:
 ;     get index or range from json array (vector).
 ;     if sel is an atom: (aref o ,sel)
 ;     if sel is cons: (subseq o ,@sel)
 ;   Source file: /data/x/jqn/src/jqn.lisp
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

#### JQN:KVADD%

```
 ; JQN:KVADD%
 ;   [symbol]
 ; 
 ; KVADD% names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (setf lft v) if v is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:KVADD+

```
 ; JQN:KVADD+
 ;   [symbol]
 ; 
 ; KVADD+ names a macro:
 ;   Lambda-list: (DAT LFT K V &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     do (setf lft (or v default))
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:KVADD?

```
 ; JQN:KVADD?
 ;   [symbol]
 ; 
 ; KVADD? names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (setf lft v) if (gethash k dat) is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:KVDEL

```
 ; JQN:KVDEL
 ;   [symbol]
 ; 
 ; KVDEL names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     delete key
 ;   Source file: /data/x/jqn/src/jqn.lisp
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

#### JQN:MAYBE

```
 ; JQN:MAYBE
 ;   [symbol]
 ; 
 ; MAYBE names a macro:
 ;   Lambda-list: (FX ARG)
 ;   Documentation:
 ;     run (fx arg) only if arg is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:NILOP

```
 ; JQN:NILOP
 ;   [symbol]
 ; 
 ; NILOP names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     do nothing
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

#### JQN:VVADD%

```
 ; JQN:VVADD%
 ;   [symbol]
 ; 
 ; VVADD% names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (vextend v lft) if v is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:VVADD+

```
 ; JQN:VVADD+
 ;   [symbol]
 ; 
 ; VVADD+ names a macro:
 ;   Lambda-list: (DAT LFT K V &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     do (vextend (or v default) lft)
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:VVADD?

```
 ; JQN:VVADD?
 ;   [symbol]
 ; 
 ; VVADD? names a macro:
 ;   Lambda-list: (DAT LFT K V)
 ;   Documentation:
 ;     do (vextend v lft) if (gethash k dat) is not nil
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

