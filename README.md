# JQN

JQN is a terminal utility (and Common Lisp library) to query and transform
JSON.

## Example

Use in terminal like this:
```bash
jqn [options] <qry> [files ...]
```
or:
```bash
echo '{\"_id\": 1}' | jqn [options] <qry>
```
Here is a full example:
```bash
echo '[
  { "_id": "65679d23fe33bc4c240675c0",
    "things": [ { "id": 10, "name": "Winters", "extra": "extra1" },
                { "id": 11, "name": "Haii", "extra": "extra2" },
                { "id": 12, "name": "Klein" } ],
    "msg": "There is a thing!"
  },
  { "_id": "65679d235b4143d2932ea17a",
    "things": [ { "id": 31, "name": "Star", "extra": null},
                { "id": 32, "name": "Ball" } ],
    "msg": "Hello, undefined! You have 5 unread messages."
  }
]' | jqn '(*$ _id
             (things (* name ?@extra))
             (msg (string-upcase _)))'
```
which returns:
```json
[ { "_id": "65679d23fe33bc4c240675c0",
    "things": [ "Winters", "extra1", "Haii", "extra2", "Klein" ],
    "msg": "THERE IS A THING!" },
  { "_id": "65679d235b4143d2932ea17a",
    "things": [ "Star", "Ball" ],
    "msg": "HELLO, UNDEFINED! YOU HAVE 5 UNREAD MESSAGES." } ]
```

JQN queries consist of "Iterators" and "Selectors". As explained in more detail
below.

## Iterators:

Currently there are three Iterator types:

  - `(*$ s1 [... sn])` iterate list of objects and select into a new object
  - `(*  s1 [... sn])` iterate list and select into array
  - `($  s1 [... sn])` select from object into new object

## Selectors

A Selector is a triple `(mode key expr)`. Where only the key is required. The
mode is either:

  - `+` always include this selector [default]
  - `?` include selector if key is present or expr is not nil
  - `-` drop this key in `*$` and `$` modes; ignore selector entirely in `*`
        mode.

Selectors can either be written out in full, or they can be be written in short
form depending on what you want to achieve. Note that the `@` in the following
examples is used to append a mode to a key without having to wrap the selector
in `(...)`:
```lisp
_            ; select everything [default]
key          ; select key
+@key        ; same as key
?@key        ; optionally select key
(key expr)   ; assign expr to key
(?@key expr) ; optionally assign expr to key
(? key expr) ; same as (?@key expr)
```
An `expr` is any valid CL code (where you can use `_` to refer to the value of
the selected key). `expr` can also be a new Selector.
```lisp
(name (string-upcase _)) ; convert name to uppercase
(this (or _ "that"))     ; select (key) this or (string) "that".
(value (+ _ 33))         ; add 33 to value
```
To select everything, but replace some keys with new values or drop keys entirely:
```lisp
(*$ _ (value (+ _ 22))           ; add 22 to current value
      (name (string-downcase _)) ; lowercase name
      -@meta)                    ; drop this key
```
If you need case sensitive keys you can use strings instead:
```lisp
"Key"          ; select "Key"
("+@Key" expr) ; assign expr to "Key"
("?@Key" expr) ; optionally assign expr to "Key"
(+ "Key" expr) ; same as ("?@Key" expr)
```

## Options

Command line options:
  - `-v` show compiled query
  - `-m` minify output [indented is default]
  - `-l` use ldn output format [json is default]

## Install

Make sure `jqn` is available in your `quicklisp` `local-projects` folder Then
create an alias for SBCL to execute `/jqn/bin/jqn-sh.lisp.` e.g.
```
alias jqn="sbcl --script ~/path/to/jqn/bin/jqn-sh.lisp"
```

Unfortunately this will tend to be quite slow. To get around this you can
create an image that has `jqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use your image in the alias instead of SBCL.

TODO: add full example.

