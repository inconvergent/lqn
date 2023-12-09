# JQN

WIP

## About

JSON query and transform utilities

## example

```bash
jqn sample.json '(*$ _id (items (* name id (+@val 77))))'

[
  {
    "_id": "65679d23d38d711eaf999e89",
    "items": [
      {
        "name": "Chris",
        "id": 0,
        "val": 77
      }
    ]
  },
  {
    "_id": "65679d23fe33bc4c240675c0",
    "items": [
      {
        "name": "Nina",
        "id": 10,
        "val": 77
      },
      {
        "name": "Ian",
        "id": 11,
        "val": 77
      }
  }
]
```

## opts

Command lind options

```
-v show compiled code
-m minify output [indented is default]
-l use ldn output format [json is default]
```

## expressions [expr]:
```
  _               -> select everything [default]
  (*$ s1 [... sn]) -> iterate list of objects and select
  (* s1 [... sn]) -> iterate list and select
  ($ s1 [... sn]) -> select these keys from object
```

## modes [m]
```
  ? include selector if key is present or expr is (not null) [default]
  + always include this selector
```

## selectors [sel]
```
  key          -> ?@key
  ?@key        -> mode: ?; "key": _
  +@key        -> mode: +; "key": _
  (key expr)   -> mode: ?; "key": expr
  (+@key expr) -> mode: +; "key": expr
  (+ key expr) -> mode: +; "key": expr
```
if you need case sensitive keys you can use strings instead:
```
  "?@Key"        -> mode: ?; "Key": _
  ("?@Key" expr) -> mode: ?; "Key": expr
  ("+@Key" expr) -> mode: +; "Key": expr
  (+ "Key" expr) -> mode: +; "Key": expr
```

## TODO/NOTES

current example of compiled query.

```lisp
██ COMPILED ██████████████████████████
██ q:   (*$ (?@THINGS (AREF _ 0)))
██ ---
   (LABELS ((JQN::FN ()
              NIL)
            (JQN::CTX ()
              NIL))
     (LOOP JQN::WITH #:IRES1 = (JQN::MAV)
           JQN::FOR #:DAT3 JQN::ACROSS (JQN::ENSURE-VECTOR #:DAT*0)
           JQN::FOR #:KRES2 = (JQN::NEW-HT)
           DO (PROGN
               (SETF (GETHASH "things" #:KRES2)
                       (AREF (GETHASH "things" #:DAT3) 0))
               (JQN::VEXTEND #:KRES2 #:IRES1))
           JQN::FINALLY (RETURN #:IRES1)))
```

