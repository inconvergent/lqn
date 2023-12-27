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
  { "_id": "65679", "msg": "HAI!"
    "things": [ { "id": 10, "name": "Win", "extra": "ex1" },
                { "id": 12, "name": "Kle" } ]
  },
  { "_id": "6AABB", "msg": "NIH!"
    "things": [ { "id": 32, "name": "Bal" },
                { "id": 31, "name": "Sta", "extra": null} ]
  }
]' | jqn '#{_id (things #[name ?@extra]) (msg (sup _))}'
```
which returns (something like):
```json
[ { "_id": "65679", "msg": "HAI!", "things": [ "Win", "ex1", "Hai", "ex2", "Kle" ] },
  { "_id": "CAABB", "msg": "NIH!", "things": [ "Sta", "Bal" ] } ]
```

## Object Representation

Internally JSON arrays are represented as `vector`. JSON objects/dicts are
represented as `hash-table`. The terms `vector` and `kv` (key/value)
respectively is used in the documentation. If the context makes it clear
whether it is a reference to a JSON data structure or the corresponding
internal Lisp data structure.

## Clauses

The following clauses have special behaviour. You can also write generic CL code,
including the functions further down.

  - ` (|| ...)` pipe the results from the first clause into the second etc.
    returns the result of the last clause.
  - `#{s1 ...}` iterate vector of `kvs` and select keys a new vector of `kvs` using `kv` selectors.
  - `#[s1 ...]` iterate `vector` of `kvs` and select keys into new `vector` using `kv` selectors.
  - ` {s1 ...}` select from `kv` into new `kv` using `kv` selectors.
  - ` [s1 ...]` iterate `vector` and filter into new `vector` using vector vector filters
  - ` (*map fx)` map `#'fx` current `(dat)`.
  - ` (*map (fx _))` map `(fx _)` across current `(dat)`.
  - ` (*map k (fx ... k))` map `(fx ... k)` across current `(dat)`.
  - ` (*fld fx init)` TODO
  - ` (*fld fx init k)` TODO

## `kv` Selectors

A `kv` Selector is a triple `(mode key expr)`. And are used in `{...}`,
`#[...]` and `#{...}`.  Only the key is required. If `expr` is not provided the
`expr` is the value of the `key`:

The modes are:
  - `+` always include this selector (always evaluate `expr` if defined) [default]
  - `?` include selector (evaluate `expr` if defined) if the key is present and not `nil`
  - `%` include selector if expr is not `nil`.
  - `-` drop this key in `#{}` and `{}` clauses; ignore selector entirely in `#[]`
    E.g. `{_ -@key3}` to select all keys except `key3`. (expr is ignored.)

Selectors can either be written out in full, or they can be be written in short
form depending on what you want to achieve. Note that the `@` in the following
examples is used to append a mode to a key without having to wrap the selector
in `(...)`. If you need the litteral key you can use `strings`:
```lisp
_               ; select everything in current data object
key             ; select key [+ mode is default]
+@key           ; same as key
"+@key"         ; same as key
?@key           ; select key if the value is not nil.
(%@key expr)    ; select key if expr is not nil.
("?@Key" expr)  ; select "Key" if the value is not nil.
("%@Key" expr)  ; select "Key" if expr is not nil.
(:+ "Key" expr) ; same as ("+@Key" expr).
```
An `expr` is any Clause or valid CL code. Use `_` in `expr` to refer to the
value of the selected key.
```lisp
#{(key1 (sup _))       ; convert value of key1 to uppercase
  (key3 (or _ "that")) ; select the value of key3 or literally "that".
  (key2 (+ 33 _))}     ; add 33 to value of key2
```
To select everything, but replace some keys with new values or drop keys entirely:
```lisp
#{_               ; select all keys, then override these:
  (key2 (sdwn _)) ; lowercase the value of key2
  -@key3}         ; drop key3
```

## `vector` Filters

`vector` filters are similar to `kv` Selectors, but they are used with `[...]`

 - `[hello]` select all string items that contain `"hello"`.
 - `[hi hello]` select all string items that contain either `"hello"` or
   `"hi"`.
 - `[+@hi +@hello]` select all string items that contain `"hi"` and `"hello"`.
 - `[+@hi +@hello oh]` select all string items that contain (`"hi"` and
   `"hello"`) or `"oh"`.
 - `[(+@pref? _ "start") (+@post? _ "end")] select all lines that start with
   `"start"` and end with `"end"`

...

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
 - `($_ k [default])` returns this key from current data object (`_`).
   In `*map`, `[]`, `#[]`, and `#{}`.
 - `(par)` returns the parent data object.
   In `*map`, `[]`, `#[]`, and `#{}`.
 - `(num)` returns length of the `vector` being iterated.
   In `*map`, `[]`, `#[]`, and `#{}`.
 - `(cnt [k])` counts from `k`, or `0`.
   In `*map`, `[]`, `#[]`, and `#{}`.

### Generic
 - `(?? fx a ...)` execute `(fx a ...)` only if `a` is not `nil`; otherwise `nil`.
 - `(>< a)` condense `a`. Remove `nil`, empty `vectors`, empty `kvs` and keys with empty `kvs`.
 - `(<> a)` ?

### Strings
 - `(mkstr a ...)` stringify and concatenate all arguments.
 - `(strcat s ...)` concatenate all strings and `vectors`/`lists` of strings.
 - `(repl s from to)` replace `from` with `to` in `s`.
 - `(split s x)` split `s` at all `x`.
 - `(sdwn s ...)` `mkstr` and downcase.
 - `(sup s ...)` `mkstr` and upcase.
 - `(sub? s sub)` check if `sub` is a substring of `s`.
 - `(pref? s pref)` check if `pref` is a prefix of `s`.
 - `(suf? s suf)` check if `suf` is a suffix of `s`.
 - `isub?`, `ipref?`, `isuf?` are case insensitive counterparts.
 - `(fmt s)` get printed representation of `s`.
 - `(fmt f ...)` format f to `string` with these args.
 - `(out s)` output printed representation of `s` to `standard-out`. return `nil`.
 - `(out f ...)` format f to `standard-out` with these args.

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
 - `(head s [n=10])` first n items. Works on `strings` too.
 - `(tail s [n=10])` last n items. Works on `strings` too.

### Types
 - `(flt? s)` return `s` if it is a `float`.
 - `(int? s)` return `s` if it is an `integer`.
 - `(kv?  s)` return `s` if it is a `kv` (`hash-table`).
 - `(lst? s)` return `s` if it is a `list`.
 - `(num? s)` return `s` if it is a `number`.
 - `(seq? s)` return `s` if it is `str`, `vector` or `list`.
 - `(str? s)` return `s` if it is a `string`.
 - `(vec? s)` return `s` if it is a `vector`.

## Options

Command line options:
  - `-v` show compiled query.
  - `-m` minify output [indented is default].
  - `-j, -t, -l` use `json`, `txt` or `ldn` output format.

## Install

Make sure `jqn` is available in your `quicklisp` `local-projects` folder Then
create an alias for SBCL to execute `/jqn/bin/jqn-sh.lisp.` e.g.
```
alias jqn="sbcl --script ~/path/to/jqn/bin/jqn-sh.lisp"
```
Unfortunately this will tend to be quite slow. To get around this you can
create an image that has `jqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use your image in the alias instead of SBCL.

