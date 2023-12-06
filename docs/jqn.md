#### JQN:&@

```
 ; JQN:&@
 ;   [symbol]
 ; 
 ; &@ names a macro:
 ;   Lambda-list: (O K &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     get k from dict o; or default
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:\*@

```
 ; JQN:*@
 ;   [symbol]
 ; 
 ; *@ names a macro:
 ;   Lambda-list: (O SEL)
 ;   Documentation:
 ;     get index or range from json array (vector).
 ;     if sel is an atom: (aref o ,sel)
 ;     if sel is cons: (subseq o ,@sel)
 ;   Source file: /data/x/jqn/src/utils.lisp
```

#### JQN:APSH!

```
:missing:todo:

 ; JQN:APSH!
 ;   [symbol]
```

#### JQN:APSH?

```
 ; JQN:APSH?
 ;   [symbol]
 ; 
 ; APSH? names a macro:
 ;   Lambda-list: (LST K V)
 ;   Documentation:
 ;     push (k . v) to lst if v
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
 ;   Derived type: (FUNCTION (T &KEY (:S STREAM) (:INDENT BOOLEAN)) *)
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
 ;     serialize internal json representation to readable lisp data.
 ;   Source file: /data/x/jqn/src/io.lisp
```

#### JQN:PROC-QRY

```
 ; JQN:PROC-QRY
 ;   [symbol]
 ; 
 ; PROC-QRY names a compiled function:
 ;   Lambda-list: (DAT Q)
 ;   Derived type: (FUNCTION (T T) (VALUES T &OPTIONAL))
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
 ;   Lambda-list: (DAT &KEY (Q _) DB)
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
 ;   Lambda-list: (DAT &KEY (Q _) DB)
 ;   Derived type: (FUNCTION (T &KEY (:Q T) (:DB T)) *)
 ;   Documentation:
 ;     compile jqn query and run on dat
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

