# JQN

WIP

## About

JSON query and transform utilities

## example

```bash
./jqn sample.json '(* _id (things (* name id (+@val 77))))'
[output]
```

## expressions [expr]:
```
 - (* sel1 sel2) -> iterate list of object and select these selectors
 - _             -> select everything [default]
```

## modes [m]
```
 - ? include selector if key is present or expr is (not null) [default]
 - + always include
```

## selectors [sel]
```
 - symbol          -> ?@symbol; [same as: (? symbol _)]
 - ?@symbol        -> mode: ?; key: "symbol"; val: _
 - +@symbol        -> mode: +; key: "symbol"; val: _
 - (symbol expr)   -> mode: ?; key: "symbol"; val: expr
 - (+@symbol expr) -> mode: +; key: "symbol"; val: expr
 - (+ symbol expr) -> mode: +; key: "symbol"; val: expr
```
if you need case sensitive keys you can use strings instead:
```
 - "?@Symbol"        -> mode: ?; key: "Symbol"; val: _
 - ("?@Symbol" expr) -> mode: ?; key: "Symbol"; val: expr
 - ("+@Symbol" expr) -> mode: +; key: "Symbol"; val: expr
 - (+ "Symbol" expr) -> mode: +; key: "Symbol"; val: expr
```

