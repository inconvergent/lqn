# JQN

WIP

## About

JSON query and transform utilities

## example

```bash
jqn sample.json '(* _id (items (* name id (+@val 77))))'

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

## expressions [expr]:
```
 - (* sel1 sel2) -> iterate list of object and select these selectors
 - _             -> select everything [default]
```

## modes [m]
```
  ? include selector if key is present or expr is (not null) [default]
  + always include
```

## selectors [sel]
```
  symbol          -> ?@symbol; [same as: (? symbol _)]
  ?@symbol        -> mode: ?; key: "symbol"; val: _
  +@symbol        -> mode: +; key: "symbol"; val: _
  (symbol expr)   -> mode: ?; key: "symbol"; val: expr
  (+@symbol expr) -> mode: +; key: "symbol"; val: expr
  (+ symbol expr) -> mode: +; key: "symbol"; val: expr
```
if you need case sensitive keys you can use strings instead:
```
  "?@Symbol"        -> mode: ?; key: "Symbol"; val: _
  ("?@Symbol" expr) -> mode: ?; key: "Symbol"; val: expr
  ("+@Symbol" expr) -> mode: +; key: "Symbol"; val: expr
  (+ "Symbol" expr) -> mode: +; key: "Symbol"; val: expr
```

## TODO/NOTES

current example of compiled query.

```lisp
; ██ COMPILED ██████████████████████████
; ██ q:   (* _ID (+@THINGS (* NAME ID)) (+@NEW-FIELD (PRINT (@ :MSG))))
; ██ ---
;    (LOOP WITH #:ITRLST5 = (MAV)
;          FOR #:O6 ACROSS (ENSURE-VECTOR #:DAT*3)
;          FOR #:KVRES4 = (LIST)
;          DO (PROGN
;              (APSH+ #:KVRES4 :NEW-FIELD (PRINT (@ #:O6 "msg")))
;              (APSH+ #:KVRES4 :THINGS
;                     (LOOP WITH #:ITRLST8 = (MAV)
;                           FOR #:O9 ACROSS (ENSURE-VECTOR (@ #:O6 "things"))
;                           FOR #:KVRES7 = (LIST)
;                           DO (PROGN
;                               (APSH? #:KVRES7 ID (@ #:O9 "id"))
;                               (APSH? #:KVRES7 NAME (@ #:O9 "name"))
;                               (VEXTEND #:KVRES7 #:ITRLST8))
;                           FINALLY (RETURN #:ITRLST8)))
;              (APSH? #:KVRES4 _ID (@ #:O6 "_id"))
;              (VEXTEND #:KVRES4 #:ITRLST5))
;          FINALLY (RETURN #:ITRLST5))
```

