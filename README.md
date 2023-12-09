# JQN

WIP

## About

JSON query and transform utilities

## example

```bash
jqn sample.json '(*$ _id (items (* name id)))'

[
  {
    "_id": "65679d23d38d711eaf999e89",
    "items": [
      {
        "name": "Chris",
        "id": 0
      }
    ]
  },
  {
    "_id": "65679d23fe33bc4c240675c0",
    "items": [
      {
        "name": "Nina",
        "id": 10
      },
      {
        "name": "Ian",
        "id": 11
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

## iterators:
```
  _                -> select everything [default]
  (*$ s1 [... sn]) -> iterate list of objects and select
  (* s1 [... sn])  -> iterate list and select
  ($ s1 [... sn])  -> select these keys from object
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

## modes [m]
```
  ? include selector if key is present or expr is (not null) [default]
  + always include this selector
```
