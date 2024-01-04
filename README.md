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
echo '[ { "_id": "65679", "msg": "HAI!",
          "things": [ { "id": 10, "name": "Win", "extra": "ex1" },
                      { "id": 12, "name": "Kle" } ] },
        { "_id": "6AABB", "msg": "NIH!",
          "things": [ { "id": 32, "name": "Bal" },
                      { "id": 31, "name": "Sta", "extra": null} ] } ]' | \
  jqn '#{_id (things #[name ?@extra]) (msg (sup _))}'
```
which returns (something like):
```json
[ { "_id": "65679", "msg": "HAI!", "things": [ "Win", "ex1", "Hai", "ex2", "Kle" ] },
  { "_id": "CAABB", "msg": "NIH!", "things": [ "Sta", "Bal" ] } ]
```

## Options

Command line options:
  - `-v` show compiled query.
  - `-j, -t, -l` use `json`, `txt` or `ldn` output format.
  - `-m` minify JSON output [indented is default].
  - `-h` show help.

## TQN Example (TXT)

`tqn` is a mode for reading lines of text into a `vector` (JSON array). `tqn`
has slightly different default behaviour to `jqn`. Notably, it ignores
`nil` in the output. `tqn` defaults to printing the `vector` as rows, but `-j`
will output to JSON instead. `-t` does the oposite for `jqn`.

```bash
# split string and sum as integers:
echo '1 x 1 x 7 x 100' | \
  tqn '(splt _ :x) int!? (*fld 0 +)'

# split string and make a new JSON structure:
echo '1 x 1 x 7 x 100' | \
  tqn -j '(splt _ :x) int!? #(($new :v _))'
```

## Lisp Example
Using the `lqn` compiler in lisp looks like this.
```lisp
(lqn:qry #((a bbbxxx xxx) (a b c) (a b (c xxx)))
          (?txpr (-@msym? _ "bbb") (+@msym? _ "xxx")
                 (sym! _ :-HIT---)))
; #((A BBBXXX XXX-HIT---) (A B C) (A B (C XXX-HIT---)))
```
See [bin/ex.lisp](bin/ex.lisp) for more examples.

## Object Representation

Internally JSON arrays are represented as `vector`. JSON objects/dicts are
represented as `hash-table`. The terms `vector` and `kv` (key/value)
respectively is used in the documentation. If the context makes it clear
whether it is a reference to a JSON data structure or the corresponding
internal Lisp data structure.

## Operators

The following operators have special behaviour. You can also write generic CL
code, anywhere you can use an operator. Including the functions further down.
Note that you can use `_`/`(dat)` to refer to the current data object.

### Pipe Operator - `||`
`(|| ..)` pipes the results from the first operator into the second etc.
Returns the result of the last operator. Pipe is the operator that surrounds
all terminal queries by default.

For convenience, particularly in the terminal, pipe has the following default
translations:
  - `fx`: to `(*map (fx _))`: map `fx` across all items.
  - `:word`: to `[(isub? _ "word")]` to filter by `"word"`.
  - `"Word"`: to `[(sub? _ "Word")]` to filter all items by this `string` with case.
  - `(expr ..)`: to itself.

### Map Operator - `#()`/`*map`
Map operations over `vector`:
  - `#(fx)` or `(*map fx)`: map `(fx _)` across all items.
  - `#((fx .. _ ..) ..)` or `(*map (fx .. _ ..) ..)`: map all `(fx .. _ ..)` across
    all items.

### Fold Operator - `*fld`
Reduce `vector`:
  - `(*fld init fx)`: fold `(fx acc _)` with `init` as the first `acc` value.
    `acc` is inserted as the first argument to `fx`.
  - `(*fld init (fx .. _ ..))`: fold `(fx acc .. _ ..)`. The accumulator is
    inserted as the first argument to `fx`.
  - `(*fld init acc (fx .. acc .. nxt))`: fold `(fx .. acc .. nxt)`. Use this
    if you need to name the accumulator explicity.

### Filter Operator - `*?`
Filter and map operations over `vector`:
  - `(*? test [expr=test])` new `vector` with `(expr _)` for all items where
    where test is not `nil`

### Selector Operators - `{}`/`$$`, `[]`/`**`, `#{}`/`*$`, `#[]`/`$*`
Select from on structure into a new data structure:
  - `#{s1 ..}` or `(*$ ..)`: from `vector` of `kvs` into new `vector` of `kvs` using `kv` selectors.
  - `#[s1 ..]` or `($* ..)`: from `vector` of `kvs` into new `vector` using `kv` selectors.
  - ` {s1 ..}` or `($$ ..)`: from `kv` into new `kv` using `kv` selectors.
  - ` [s1 ..]` or `(** ..)`: from `vector` into new `vector` using `expr` selectors.

#### KV Selectors
A `kv` Selector is a triple `(mode key expr)`. And are used in `{}`, `#[]` and
`#{}`.  Only the key is required. If `expr` is not provided the `expr` is `_`,
that is: the value of the `key`.

The modes are:
  - `+`: always include this selector (always evaluate `expr` if defined) [default]
  - `?`: include selector (evaluate `expr` if defined) if the key is present and not `nil`
  - `%`: include selector if expr is not `nil`.
  - `-`: drop this key in `#{}` and `{}` operators; ignore selector entirely in `#[]`
    E.g. `{_ -@key3}` to select all keys except `key3`. `expr` is ignored.

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
  (key3 (or _ "That")) ; select the value of key3 or literally "That".
  (key2 (+ 33 _))}     ; add 33 to value of key2
```
To select everything, but replace some keys with new values or drop keys
entirely:
```lisp
#{_               ; select all keys, then override these:
  (key2 (sdwn _)) ; lowercase the value of key2
  -@key3}         ; drop key3
```

#### EXPR Selectors
`expr` selectors are similar to `kv` Selectors, but they are used with `?xpr`,
`?txpr`, `?mxpr`, and `[]`.
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

### Transformer Operators - `?xpr`, `?txpr`, `?mxpr`
Perform operation on when pattern or condition is satisfied:
  - `(?xpr sel .. hit miss)` match current data object agains these `vector`
    selectors and execute either `hit` or `miss`. `hit`/`miss` can be a
    function name, or an expression, but must be defined.

Recursively traverse a structure of `sequences` and `kvs` and perform operations
when patterns or conditions are satisfied:
  - `(?txpr sel .. tx)` recursively traverse current data object and replace
    matches with `tx`. `tx` can be a function name or expression. Also
    traverses vectors and `kv` values (not keys).
  - `(?mxpr (sel .. tx) (sel .. tx))` multiple matches and transforms. performs
    the transform of the first match only.

## Query Utility Functions

The internal representation of JSON data as `vectors` and `kvs` in `lqn` means
you can use the regular CL utilities such as `gethash`, `aref`, `subseq`,
`length` etc.

But for convenience there are a few special functions defined in `lqn`.  `[d]`
represents a default value if key/index is missing, or if a functon fails.

### Global Query Context Fxs
Defined in the query scope:
 - `(ctx)`: returns `:pipe` if input is from `*standard-input*`; otherwise `:file`.
 - `(fi [k=0])`: index of the current file; start at `k`.
 - `(fn)`: name of the current file; or `nil`.
 - `(hld k v)`: hold this value at this key in a global key value store.
 - `(ghv k [d])`: get the value of this key; or `d`.

### Operator Context Fxs
Defined in all operators.
 - `_` or `(dat)`: the current data object.
 - `($_ k [d])`: this key from current data object; or `d`.
 - `(par)`: the parent data object.
 - `(num)`: length of the `vector` being iterated.
 - `(cnt [k=0])`: counts from `k` over the `vector` being iterated.

### Generic Fxs
General utilities:
 - `(?? fx a ..)`: execute `(fx a ..)` only if `a` is not `nil`; otherwise `nil`.
 - `(fmt f ..)`: format `f` as `string` with these (`format`) args.
 - `(fmt s)`: get printed representation of `s`.
 - `(out f ..)`: format `f` to `*standard-output*` with these (`format`) args. returns `nil`.
 - `(out s)`: output printed representation of `s` to `*standard-output*`. returns `nil`.
 - `(msym? a b)`: compare symbol `a` to `b`. if `b` is a keword or symbol
   a perfect match is required. if `b` is a string it performs a substring
   match. If `b` is an expression, `a` is compared to the evaluated value of
   `b`.

### KV / Strings / Vectors / Sequences Fxs
Access values in objects:
 - `(pck a d i ..)`: pick these indices/keys from `sequence`/`kv` into new `vector`.

Size of objects:
 - `(size? o [d])`: length of `sequence` or number of keys in `kv`

Condense objects:
 - `(>< a)`: Remove `nil`, empty `vectors`, empty `kvs` and keys with empty `kvs`.

Access values in `kvs`:
 - `($ kv k [d])`: get key `k` from `kv`.
 - `($_ k ..)`: is equivalent to `($ _ k ..)`.
 - `($cat ..)`: add all keys from these `kvs` to a new `kv`. left to right.
 - `($new :k1 expr1 ..)`: new `kv` with these keys and expressions.

Primarily for sequences (`string`, `vector`, `list`):
 - `(*cat a ..)`: concatenate all `vectors` in these `vectors`.
 - `(*n v i)`: get this index from `sequence`.
 - `(*new ..)`: new `vector` with these elements.
 - `(*sel ..)`: get new `vector` with these `*n`s or `*seq`s from `sequence`.
 - `(*seq v i [j])`: get range `i ..` or `i .. (1- j)` from `sequence`.
 - `(*head s [n=10])`: first `n` items of `sequence`.
 - `(*tail s [n=10])`: last `n` items of `sequence`.

Primarlily for string searching. `[i]` means case insensitive:
 - `([i]pref? s pref [d])`: `s` if `pref` is a prefix of `s`; or `d`.
 - `([i]sub? s sub [d])`: `s` if `sub` is a substring of `s`; or `d`.
 - `([i]subx? s sub)`: index where `sub` starts in `s`.
 - `([i]suf? s suf [d])`: `s` if `suf` is a suffix of `s`; or `d`.
 - `(repl s from to)`: replace `from` with `to` in `s`.

String maniuplation:
 - `(trim s)`: trim leading and trailing whitespace from `string`.
 - `(join s x ..)`: join sequence with `x` (strings or `chars`), returns `string`.
 - `(splt s x)`: split `s` at all `x`.
 - `(mkstr s ..)`: stringify and concatenate all arguments.
 - `(strcat s ..)`: concatenate all `strings` in these `sequences` of `strings`.
 - `(sup s ..)`: `mkstr` and upcase.
 - `(sdwn s ..)`: `mkstr` and downcase.

### Type Test Fxs
`(is? o [d])` returns `o` if not `nil`, empty `sequence` or empty `kv`; or `d`.

These functions return the argument if the argument is the corresponding type:
`flt?`, `int?`, `kv?`, `lst?`, `num?`, `str?`, `vec?`, `seq?`

These functions return the argument parsed as the corresponding type if
possible; otherwise they return the optional second argument: `int!?`, `flt!?`,
`num!?`, `str!?`, `vec!?`, `seq!?`

### Type Coercion Fxs
 - `(str! s ..)`: coerce everything to a `string`.
 - `(vec! v)`: coerce `sequence` to `vector`; or return `(*new v)`
 - `(sym! s ..)`: stringify, make `symbol`

## Install

Make sure `lqn` is available in your `quicklisp` `local-projects` folder Then
create an alias for SBCL to execute shell wrappers e.g:
```
alias jqn="sbcl --script ~/path/to/lqn/bin/jqn-sh.lisp"
alias tqn="sbcl --script ~/path/to/lqn/bin/tqn-sh.lisp"
alias lqn="sbcl --script ~/path/to/lqn/bin/lqn-sh.lisp"
```
Unfortunately this will tend to be quite slow. To get around this you can
create an image/core that has `lqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use your core in the alias instead of SBCL.

Below is an example script for creating your own core.  You can load your own
libraries which will be available to `lqn`.
```bash
#!/bin/bash
sbcl --quit \
     --eval '(ql:quickload :sb-introspect)'\
     --eval '(load "/path/to/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :lqn)'\ # add more evals to load your own pkg
     --eval '(save-lisp-and-die "/path/to/lsp.core"
               :executable t :compression nil
               :purify t     :save-runtime-options t)'
```
Then make aliases like this:
```bash
alias lqn="/path/to/lsp.core --script ~/path/to/lqn/bin/lqn-sh.lisp"
```
