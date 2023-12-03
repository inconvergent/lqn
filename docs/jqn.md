#### JQN:APSH!

```
 ; JQN:APSH!
 ;   [symbol]
 ; 
 ; APSH! names a macro:
 ;   Lambda-list: (LST K V &OPTIONAL DEFAULT)
 ;   Documentation:
 ;     push (k . v) to lst if v; otherwise push (k . default)
 ;   Source file: /data/x/jqn/src/utils.lisp
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

#### JQN:DUMPS

```
 ; JQN:DUMPS
 ;   [symbol]
 ; 
 ; DUMPS names a compiled function:
 ;   Lambda-list: (O &KEY (S *STANDARD-OUTPUT*) INDENT)
 ;   Derived type: (FUNCTION (T &KEY (:S STREAM) (:INDENT BOOLEAN)) *)
 ;   Documentation:
 ;     encode o as json to stream, s
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:LOADF

```
 ; JQN:LOADF
 ;   [symbol]
 ; 
 ; LOADF names a compiled function:
 ;   Lambda-list: (FN)
 ;   Derived type: (FUNCTION (STRING) *)
 ;   Documentation:
 ;     load json from file fn
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:PROC-QRY

```
 ; JQN:PROC-QRY
 ;   [symbol]
 ; 
 ; PROC-QRY names a compiled function:
 ;   Lambda-list: (SRC Q)
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
 ;   Lambda-list: (DAT &KEY Q DB)
 ;   Documentation:
 ;     query dat
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:QRYF

```
 ; JQN:QRYF
 ;   [symbol]
 ; 
 ; QRYF names a macro:
 ;   Lambda-list: (FN &KEY Q DB)
 ;   Documentation:
 ;     query file fn
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

#### JQN:QRYL

```
 ; JQN:QRYL
 ;   [symbol]
 ; 
 ; QRYL names a compiled function:
 ;   Lambda-list: (DAT &KEY Q)
 ;   Derived type: (FUNCTION (STRING &KEY (:Q T)) *)
 ;   Documentation:
 ;     compile query and run it on dat
 ;   Source file: /data/x/jqn/src/jqn.lisp
```

