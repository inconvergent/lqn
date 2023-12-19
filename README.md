# JQN - JSON Query Notation

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
]' | jqn '#{_id
            (things [name ?@extra])
            (msg (string-upcase _))}'
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

JQN queries consist of "Clauses" and "Selectors". As explained in more detail
below.

## Object Representation

Internally JSON arrays are represented as `vector`. JSON objects/dicts are
represented as `hash-table`. The terms `vector` and `kv` (key/value) are used
in the documentation, if the context makes it clear whether it is a reference
to a JSON data structure or an internal lisp structure.

## Clauses

Currently there are four Clauses, the first three have two notations. The
first notation is a little more readable and compact.

  - `#{s1 ... sn}` or `(*$ s1 ... sn)` iterate vector of `kvs` and select into
    a new vector of `kvs`
  - `[s1 ... sn]` or `(** s1 ... sn)` iterate `vector` and select into new `vector`
  - `{s1 ... sn}` or `($$ s1 ... sn)` select from `kv` into new `kv`
  - `(|| c1 c2 ...)` pipe the results from `c1` into `c2` etc. returns
    the result of the last clause.

## Selectors

A Selector is a triple `(mode key expr)`. Where only the key is required. The
mode is either:

  - `+` always include this selector (evaluate `expr` if defined) [default]
  - `?` include selector (evaluate `expr` if defined) if key is present
        and not `nil`
  - `%` include selector if key is present and not `nil`; or: include `expr`
        if it does not evaluate to `nil`.
  - `-` drop this key in `*$` and `$$` modes; ignore selector entirely in `**`
        mode. E.g. `{_ -@key}` to select everything except `key`.

Selectors can either be written out in full, or they can be be written in short
form depending on what you want to achieve. Note that the `@` in the following
examples is used to append a mode to a key without having to wrap the selector
in `(...)`:
```lisp
_            ; select everything.
key          ; select key [+ mode is default]
+@key        ; same as key
?@key        ; optionally select key
(key expr)   ; select expr as key
(?@key expr) ; select expr if key is not nil
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
#{_                          ; select all keys, then override these:
  (value (+ _ 22))           ; add 22 to current value
  (name (string-downcase _)) ; lowercase name
  -@meta}                    ; drop this key
```
If you need case sensitive keys you can use strings instead:
```lisp
"Key"          ; select "Key"
("+@Key" expr) ; assign expr to "Key"
("?@Key" expr) ; optionally assign expr to "Key"
(+ "Key" expr) ; same as ("?@Key" expr)
```

## Query Utility Functions

The internal representation of JSON data as `vectors` and `kvs` in `jqn` means
you can use the regular CL utilities such as `gethash`, `aref`, `subseq`,
`length` etc.

But for convenience there are a few special functions defined in `jqn`.

### Global Context

 - `(ctx)` returns `:pipe` if input is from `stdin`; otherwise `:file`.
 - `(fn)` name of the file that is the source for the current data; or `nil`.
 - `(fi [k])` index of the file that is the source for the current data; or `0`.

### Clause Context

 - `(@_ k [default])` returns this key from current data object (`_`).
 - `(par)` returns the parent data object. Available in `**` and `*$`.
 - `(num)` returns length of the `vector` being iterated. Available in `**` and `*$`.
 - `(cnt [k])` counts from `k`, or `0`.

### Generic

 - `(@ kv k [default])` get key `k` from `kv`. Equivalent to `gethash`.
 - `(?? fx a ...)` execute `(fx a ...)` only if `a` is not `nil`; otherwise `nil`.
 - `(>< a)` condense `a`. Remove `nil`, empty `vectors`, empty `kvs` and keys with empty `kvs`.
 - `(<> a)` ?

### Strings

 - `(mkstr a ...)` stringify and concatenate all arguments.
 - `(strcat s ...)` concatenate all strings and `vectors`/`lists` of strings.
 - `(repl s from to)` replace `from` with `to` in `s`.
 - `(sdwn s ...)` `mkstr` and downcase.
 - `(sup s ...)` `mkstr` and upcase.

### Kvs

 - `($new (k1 expr1) ...)` new `kv` with these keys and expressions.
 - `($cat ...)` add all keys from these `kvs` to a new `kv`. left to right.

### Vectors

 - `(*ind v i)` get `i` from vector `v`. Equivalent to `aref`.
 - `(*ind v i j)` get range `[i j)` from vector `v`. Equivalent to `subseq`.
 - `(*cat a ...)` concatenate all `vectors` in these `vectors`. non-vectors are
   included in their position
 - `(*new ...)` new `vector` with these elements.

## Options

Command line options:
  - `-v` show compiled query.
  - `-m` minify output [indented is default].
  - `-l` use `ldn` output format [`json` is default].

## Install

Make sure `jqn` is available in your `quicklisp` `local-projects` folder Then
create an alias for SBCL to execute `/jqn/bin/jqn-sh.lisp.` e.g.
```
alias jqn="sbcl --script ~/path/to/jqn/bin/jqn-sh.lisp"
```

Unfortunately this will tend to be quite slow. To get around this you can
create an image that has `jqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use your image in the alias instead of SBCL.

## TODO

 - add full examples
 - merge dicts
 - merge vector?
 - fix `*new`
