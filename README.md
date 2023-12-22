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

JQN queries consist of "Expressions" and "Selectors". As explained in more detail
below.

## Object Representation

Internally JSON arrays are represented as `vector`. JSON objects/dicts are
represented as `hash-table`. The terms `vector` and `kv` (key/value) are used
in the documentation, if the context makes it clear whether it is a reference
to a JSON data structure or an internal lisp structure.

## Clauses

Currently there are four special Clauses. You can also write generic CL code,
including the functions further down.

  - `#{s1 ...}` iterate vector of `kvs` and select keys a new vector of `kvs` using kv selectors.
  - ` [s1 ...]` iterate `vector` of `kvs` and select keys into new `vector` using kv selectors.
  - ` {s1 ...}` select from `kv` into new `kv` using kv selectors.
  - `#[s1 ...]` iterate `vector` and select into new `vector` using vector selectors/filters
  - ` (|| ...)` pipe the results from the first clause into the second etc. returns
    the result of the last clause.

## `kv` Selectors

A `kv` Selector is a triple `(mode key expr)`. Where only the key is required. The
mode is either:

  - `+` always include this selector (always evaluate `expr` if defined) [default]
  - `?` include selector (evaluate `expr` if defined) if the key is present
        and not `nil`
  - `%` include selector if key is present and not `nil`. (include `expr`
        if it does not evaluate to `nil`.)
  - `-` drop this key in `#{}` and `{}` clauses; ignore selector entirely in `[]`
        E.g. `{_ -@key3}` to select all keys except `key3`. (expr is ignored.)

Selectors can either be written out in full, or they can be be written in short
form depending on what you want to achieve. Note that the `@` in the following
examples is used to append a mode to a key without having to wrap the selector
in `(...)`:
```lisp
_            ; select everything in current data object
key          ; select key [+ mode is default]
+@key        ; same as key
?@key        ; optionally select key
(key expr)   ; select expr as key
(?@key expr) ; select expr if key is not nil
(? key expr) ; same as (?@key expr)
```
An `expr` is any Caluse or valid CL code. Use `_` to refer to the value of
the selected key.
```lisp
#{(key1 (sup _))       ; convert value of key1 to uppercase
  (key3 (or _ "that")) ; select the value of key3 or literally "that".
  (key2 (+ _ 33))}     ; add 33 to value of key2
```
To select everything, but replace some keys with new values or drop keys entirely:
```lisp
#{_               ; select all keys, then override these:
  (key2 (+ _ 22)) ; add 22 to the value of key2
  (key3 (sdwn _)) ; lowercase the value of key3
  -@key3}         ; drop key3
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

 - `($_ k [default])` returns this key from current data object (`_`). Only in `{}` and `#{}`.
 - `(par)` returns the parent data object. Only in `[]` and `#{}`.
 - `(num)` returns length of the `vector` being iterated. Only in `[]` and `#{}`.
 - `(cnt [k])` counts from `k`, or `0`. Only in `[]` and `#{}`.

### Generic

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
 - `($ kv k [default])` get key `k` from `kv`. Equivalent to `gethash`.
 - `($_ k ...)` is equivalent to `($ _ k ...)`.
 - `($cat ...)` add all keys from these `kvs` to a new `kv`. left to right.

### Vectors

 - `(*new ...)` new `vector` with these elements.
 - `(*ind v i)` get these index `i` from `v`. Equivalent to `aref`.
 - `(*seq v i [j])` get range `i ...` or `i ... (1- j)` from `v`. Equivalent to `subseq`.
 - `(*sel ...)` get new vector with these `*ind`s or `*seq`s
 - `(*cat a ...)` concatenate all `vectors` in these `vectors`. Non-vectors are
   included in their position.

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
 - tqn
 - prev? suf? sub? filter?

