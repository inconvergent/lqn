# LQN - Lisp Query Notation

LQN is a terminal utility (and Common Lisp library) to query and transform
Lisp data, JSON and TXT files. It consists of three terminal commands `lqn`,
`jqn` and `tqn`.

## JQN Example (JSON)

Use in terminal like this:
```bash
jqn [options] <qry> [files ...]
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
[ { "_id": "65679", "msg": "HAI!",
    "things": [ "Win", "ex1", "Hai", "ex2", "Kle" ] },
  { "_id": "CAABB", "msg": "NIH!",
    "things": [ "Sta", "Bal" ] } ]
```

## TQN Example (TXT)

`tqn` is a mode for reading lines of text into a `vector` (JSON array). `tqn`
has slightly different default behaviour to `jqn`. Notably, it ignores
`nil` in the output. `tqn` defaults to printing the vector as rows, but `-j`
will output to JSON instead. `-t` does the oposite for `jqn`.

```bash
# split string and sum as integers:
echo '1 x 1 x 7 x 100' | \
  tqn '(splt _ :x) int!? (*fld 0 +)'

# split string and make a new JSON structure:
echo '1 x 1 x 7 x 100' | \
   tqn -j '(splt _ :x) int!? (*map ($new :v _))'
```

## Object Representation

Internally JSON arrays are represented as `vector`. JSON objects/dicts are
represented as `hash-table`. The terms `vector` and `kv` (key/value)
respectively is used in the documentation. If the context makes it clear
whether it is a reference to a JSON data structure or the corresponding
internal Lisp data structure.

## Operators

The following operators have special behaviour. You can also write generic CL code,
anywhere you can use an operator. Including the functions further down.

### Pipe Operator
Pipe is the operator that surrounds all queries by default.
  - ` (|| ..)` pipe the results from the first operator into the second etc.
    Returns the result of the last operator.

For convenience pipe has the following default translations:
  - `fx`: to `(*map (fx _))`; map `fx` across all items.
  - `:word`: to `[(isub? _ "word")]` to filter by `"word"`.
  - `"Word"`: to `[(sub? _ "Word")]` to filter all items by this string with case.
  - `(expr)`: to itself.

### Map/Reduce Operators
  - `(*map fx)`: map `(fx _)` across all items.
  - `(*map (fx .. _ ..) ..)`: map `(fx .. _ ..)` across all items.
  - `(*fld init fx)`: fold `(fx acc _)` with `init` as the first `acc` value. acc is inserted as the
     first argument to `fx`
  - `(*fld init (fx .. _ ..))`: fold `(fx acc .. _ ..)`. the accumulator is
    inserted as the first argument to `fx`.
  - `(*fld init acc (fx .. acc .. nxt))`: fold `(fx .. acc .. nxt)`. use this if you need to name
    the accumulator explicity.

### Selector Operators
  - `#{s1 ..}` or `(*$ ..)`: select from vector of `kvs` into new vector of
    `kvs` using `kv` selectors.
  - `#[s1 ..]` or `($* ..)`: select from `vector` of `kvs` into new `vector`
    using `kv` selectors.
  - ` {s1 ..}` or `($$ ..)`: select from `kv` into new `kv` using `kv`
    selectors.
  - ` [s1 ..]` or `(** ..)`: select from `vector` into new `vector` using
    vector `vector` selectors.

### `kv` Selectors
A `kv` Selector is a triple `(mode key expr)`. And are used in `{}`, `#[]` and
`#{}`.  Only the key is required. If `expr` is not provided the `expr` is `_`,
that is: the value of the `key`.

The modes are:
  - `+`: always include this selector (always evaluate `expr` if defined) [default]
  - `?`: include selector (evaluate `expr` if defined) if the key is present and not `nil`
  - `%`: include selector if expr is not `nil`.
  - `-`: drop this key in `#{}` and `{}` operators; ignore selector entirely in `#[]`
    E.g. `{_ -@key3}` to select all keys except `key3`. (expr is ignored.)

Selectors can either be written out in full, or they can be be written in short
form depending on what you want to achieve. Note that the `@` in the following
examples is used to append a mode to a key without having to wrap the selector
in parenthesis. If you need the litteral key you can use `strings`:
```lisp
{_}               ; select everything in current data object
{key1 key2}       ; select key1 and key2 [+ mode is default]
{+@key}           ; same as key
{"+@Key"}         ; select "Key"
{?@key }          ; select key if the value is not nil.
{(%@key expr)}    ; select key if expr is not nil.
{("?@Key" expr)}  ; select "Key" if the value is not nil.
{("%@Key" expr)}  ; select "Key" if expr is not nil.
{(:+ "Key" expr)} ; same as ("+@Key" expr).
```
An `expr` is any Operator or valid CL code. Use `_` in `expr` to refer to the
value of the selected key.
```lisp
#{(key1 (sup _))       ; convert value of key1 to uppercase
  (key3 (or _ "that")) ; select the value of key3 or literally "that".
  (key2 (+ 33 _))}     ; add 33 to value of key2
```
To select everything, but replace some keys with new values or drop keys
entirely:
```lisp
#{_               ; select all keys, then override these:
  (key2 (sdwn _)) ; lowercase the value of key2
  -@key3}         ; drop key3
```

### `vector` Selectors
`vector` selectors are similar to `kv` Selectors, but they are used with `[]`.
`:key` (keyword) are translated to `"key"` and `fx` (symbol) is translated to
`(fx _)`. That is, symbols are called as functions with current data as the
only argument.

```lisp
[:hello]              ; items that contain `"hello"`.
[:hi :hello]          ; items that contain either `"hello"` or `"hi"`.
[:+@hi :+@hello]      ; items that contain `"hi"` and `"hello"`.
[:+@hi :+@hello "OH"] ; items that contain (`"hi"` and `"hello"`) or `"OH"`.
[int!?]               ; items that can be parsed as `int`.
[(> _ 3)]             ; items larger than `3`.
[_ :-@hi]             ; items except those that contain `"hi"`.
[(+@pref? _ "start")  ; items that start with `"start"` and end with `"end"`.
 (+@post? _ "end")]
```

## Query Utility Functions

The internal representation of JSON data as `vectors` and `kvs` in `lqn` means
you can use the regular CL utilities such as `gethash`, `aref`, `subseq`,
`length` etc.

But for convenience there are a few special functions defined in `lqn`.

### Global Context
 - `(ctx)`: returns `:pipe` if input is from `stdin`; otherwise `:file`.
 - `(fi [k])`: index of the file that is the source for the current data; or `0`.
 - `(fn)`: name of the file that is the source for the current data; or `nil`.

### Operator Context
Available in most operators.
 - `_` or `(dat)`: the current data object.
 - `($_ k [default])`: this key from current data object.
 - `(cnt [k])`: counts from `k`, or `0`.
 - `(num)`: length of the `vector` being iterated.
 - `(par)`: the parent data object.

### Generic
 - `(>< a)`: condense `a`. Remove `nil`, empty `vectors`, empty `kvs` and keys with empty `kvs`.
 - `(?? fx a ..)`: execute `(fx a ..)` only if `a` is not `nil`; otherwise `nil`.
 - `(fmt f ..)`: format f to `string` with these args.
 - `(fmt s)`: get printed representation of `s`.
 - `(out f ..)`: format f to `*standard-out*` with these args.
 - `(out s)`: output printed representation of `s` to `*standard-out*`. return `nil`.
 - `(size? o [d])`: length of `vector` or number of keys in `kv`; or `d`.

### Strings
 - `(mkstr a ..)`: stringify and concatenate all arguments.
 - `(pref? s pref [d])`: `s` if `pref` is a prefix of `s`; or `d`.
 - `(repl s from to)`: replace `from` with `to` in `s`.
 - `(sdwn s ..)`: `mkstr` and downcase.
 - `(split s x)`: split `s` at all `x`.
 - `(strcat s ..)`: concatenate all strings and `vectors`/`lists` of strings.
 - `(sub? s sub [d])`: `s` if `sub` is a substring of `s`; or `d`.
 - `(subx? s sub)`: interger where `sub` starts in `s`
 - `(suf? s suf [d])`: `s` if `suf` is a suffix of `s`; or `d`.
 - `(sup s ..)`: `mkstr` and upcase.
 - `isub?` `isubx?`, `ipref?`, `isuf?`: case insensitive counterparts.

### Kvs
 - `($ kv k [default])`: get key `k` from `kv`. Equivalent to `gethash`.
 - `($_ k ..)`: is equivalent to `($ _ k ..)`.
 - `($cat ..)`: add all keys from these `kvs` to a new `kv`. left to right.
 - `($new :k1 expr1 ..)`: new `kv` with these keys and values.

### Vectors
 - `(*cat a ..)`: concatenate all `vectors` in these `vectors`. Non-vectors are
   included in their position.
 - `(*n v i)`: get these index `i` from `v`. Equivalent to `aref`.
 - `(*new ..)`: new `vector` with these elements.
 - `(*sel ..)`: get new vector with these `*n`s or `*seq`s.
 - `(*seq v i [j])`: get range `i ..` or `i .. (1- j)` from `v`. Equivalent to
   `subseq`.
 - `(head s [n=10])`: first n items. Works on `strings` too.
 - `(tail s [n=10])`: last n items. Works on `strings` too.

### Types
 - `(flt? f)`: `f` if it is a `float`.
 - `(int? i)`: `i` if it is an `integer`.
 - `(kv?  k)`: `k` if it is a `kv` (`hash-table`).
 - `(lst? l)`: `l` if it is a `list`.
 - `(num? n)`: `n` if it is a `number`.
 - `(str? s)`: `s` if it is a `string`.
 - `(vec? v)`: `v` if it is a `vector`.
 - `(seq? s)`: `s` if it is `str`, `vector` or `list`.
 - `(str! s)`: `s` coerce `s` to string.

## Options

Command line options:
  - `-v` show compiled query.
  - `-m` minify output [indented is default].
  - `-j, -t, -l` use `json`, `txt` or `ldn` output format.

## Install

Make sure `lqn` is available in your `quicklisp` `local-projects` folder Then
create an alias for SBCL to execute shell wrappers e.g:
```
alias jqn="sbcl --script ~/path/to/lqn/bin/jqn-sh.lisp"
alias tqn="sbcl --script ~/path/to/lqn/bin/tqn-sh.lisp"
alias lqn="sbcl --script ~/path/to/lqn/bin/lqn-sh.lisp"
```
Unfortunately this will tend to be quite slow. To get around this you can
create an image that has `lqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use your image in the alias instead of SBCL.
