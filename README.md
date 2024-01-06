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
                      { "id": 31, "name": "Sta", "extra": null} ] } ]' |\
  jqn '#{:_id (:things #[:name :?@extra]) (:msg (sup _))}'
```
which returns (something like):
```json
[ { "_id": "65679", "msg": "HAI!", "things": [ "Win", "ex1", "Hai", "ex2", "Kle" ] },
  { "_id": "CAABB", "msg": "NIH!", "things": [ "Sta", "Bal" ] } ]
```

## Why??

`lqn` started as an experiment and programming exercise. But it has turned into
a little language I think I will use in the future. Both in the terminal, and
more interestingly, as a meta language for writing macros in CL.

The main purpose of the design is to make something that is intuitive, terse,
yet flexible enough that you can write generic CL if you need to. I also wanted
to make something that requres a relatively simple compiler.  I have written
about similar approaches to making small DSLs in these blog posts:

 - https://inconvergent.net/2023/vectors-and-symbols/
 - https://inconvergent.net/2023/lets-write-a-dsl/
 - https://inconvergent.net/2023/a-vector-dsl/

There are several parts of the design that i'm not entirely happy with, so
things might change in the future.

## Terminal Options

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
echo '1 x 1 x 7 x 100' |\
  tqn '(splt _ :x) int!? (*fld 0 +)'

# split string and make a new JSON structure:
echo '1 x 1 x 7 x 100' |\
  tqn -j '(splt _ :x) int!? #((new$ :v _))'
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
Note that you can use `_` to refer to the current data object.

In the following sections `[d]` represents an optional default value.  E.g. if
key/index is missing, or if a functon returns `nil`. `k` is an initial counter
value. Whereas `..` menans that there can be arbitrary arguments, selectors or
expressions; depending on the context. `expr` denotes any expression or
operator, like `(+ 1 _)` or `#[:id]`.

### Strings and :keywords
In operators, and many functions, `:keywords` can be used to represent
lowercase `strings`. This is useful in the terminal to avoid escaping strings.
Particularly when using `Selector` operators.

### Pipe Operator - `||`
`(|| expr ..)` pipes the results from the first clause/expression into the
second etc.  Returns the result of the last clause. Pipe is the operator that
surrounds all terminal queries by default.

For convenience, particularly in the terminal, pipe has the following default
translations:
  - `fx`: to `(*map (fx _))`: map `fx` across all items.
  - `:word`: to `[(isub? _ "word")]` to filter by `"word"`.
  - `"Word"`: to `[(sub? _ "Word")]` to filter all items by this `string` with case.
  - `(..)`: to itself. That is, expressions are not translated.

### Map Operator - `#()`/`*map`
Map operations over `vector`:
  - `#(fx)` or `(*map fx)`: map `(fx _)` across all items.
  - `#(expr ..)` or `(*map expr ..)`: evaluate these expressions
    sequentially on all items in `sequence`.

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
  - `(*? test-expr [expr=test])` new `vector` where `expr` has been evaluated
    for all items where `test-expr` is not `nil`

### Selector Operators - `{}`/`$$`, `[]`/`**`, `#{}`/`*$`, `#[]`/`$*`, `@`
Select from on structure into a new data structure. `selectors` are explained
below:
  - `#{s1 sel ..}` or `(*$ sel ..)`: from `vector` of `kvs` into new `vector`
    of `kvs` using `kv selectors`.
  - `#[s1 sel ..]` or `($* sel ..)`: from `vector` of `kvs` into new `vector`
    using `kv selectors`.
  - ` {s1 sel ..}` or `($$ sel ..)`: from `kv` into new `kv` using `kv
    selectors`.
  - ` [s1 sel ..]` or `(** sel ..)`: from `vector` into new `vector` using
    `expr selectors`.

select keys or indexes:
 - `(@ k)`: get this key/index from current data object.
 - `(@ k [d])`: get this key/index from current data object.
 - `(@ o k [d])`: get this key/index from `o`.

#### KV Selectors
A `kv` Selector is a triple `(mode key expr)`. And are used in `{}`, `#[]` and
`#{}`.  Only the key is required. If `expr` is not provided the `expr` is `_`,
that is: the value of the `key`.

The modes are:
  - `+`: always include this `expr`. [default]
  - `?`: include `expr` if the key is present and not `nil`.
  - `%`: include selector if `expr` is not `nil`.
  - `-`: drop this key in `#{}` and `{}` operators; ignore selector entirely in `#[]`
    E.g. `{_ -@key3}` to select all keys except `key3`. `expr` is ignored.

`kv selectors` can either be written out in full, or they can be be written in
short form depending on what you want to achieve. Note that the `@` in the
following examples is used to append a mode to a key without having to wrap the
selector in parenthesis. If you need eg. case or spaces you can use
`"strings"`:
```lisp
{_}               ; select all keys.
{_ :-@key1}       ; select all keys except "key1".
{:key1 "Key2"}    ; select "key1" and "Key2".
{:+@key}          ; same as :key [+ mode is default].
{"+@Key"}         ; select "Key".
{:?@key }         ; select "key" if the value is not nil.
{(:%@key expr)}   ; select "key" if expr is not nil.
{("?@Key" expr)}  ; select "Key" if the value is not nil.
{("%@Key" expr)}  ; select "Key" if expr is not nil.
{(:+ "Key" expr)} ; same as ("+@Key" expr).

; Use `_` in `expr` to refer to the value of the selected key:
{(:key1 sup))          ; convert value of "key1" to uppercase
 (:key3 (or _ "That")) ; select the value of "key3" or literally "That".
 (:key2 (+ 33 _))}     ; add 33 to value of "key2"

; override and drop keys:
{_                ; select all keys, then override these:
 (:key2 (sdwn _)) ; lowercase the value of "key2"
  :-@key3}        ; drop "key3"
```
We use `{}` in the examples but all `kv selectors` have the same behaviour.

#### EXPR Selectors
`expr selectors` serve a similar purpose as `kv Selectors`, but they are used
with `[]`, `?xpr`, `?txpr`, `?mxpr` operators, and the modes behave a little
differently:
  - `+`: if there are multiple selectors with `+` mode, requires ALL
    of them to be `t`.
  - `?`: if there are any clauses with `?` mode, it will select items where
    either of these clauses is `t`
  - `-`: items that match any clause with `-` mode will ALWAYS be ignored.

If this is not what you need, you can compose boolean expressions with regular
CL boolen operators. Here are some examples:

```lisp
[:hello]               ; strings containing "hello".
[:hi "Hello"]          ; strings containing either "Hello" or "hi".
[:+@hi :+@hello]       ; strings containing "hi" and "hello".
[:+@hi :+@hello "OH"]  ; strings containing ("hi" and "hello") or "OH".
[int!?]                ; items that can be parsed as int.
[(> _ 3)]              ; numbers larger than 3.
[_ :-@hi]              ; strings except those that contain "hi".
[(+@pref? _ "start")   ; strings that start with "start" and end with "end".
 (+@post? _ "end")]
[(fx1 _)]              ; items where this expression is not nil.
[(or (fx1 _) (fx2 _))] ; ...
```

### Transformer Operators - `?xpr`, `?txpr`, `?mxpr`
Perform operation on when pattern or condition is satisfied:
  - `(?xpr sel)`: match current data object agains `expr selector`. Return the
    result if not `nil`.
  - `(?xpr sel hit-expr)`: match current data object agains `expr selector`. Evaluates
    `hit-expr` if not nil. `_` is the matching item.
  - `(?xpr sel .. hit-expr miss-expr)`: match current data object agains `expr
    selectors`.  Evaluate `hit-expr` if not `nil`; else evaluate `miss-expr`.
    `_` is the matching item.

Recursively traverse a structure of `sequences` and `kvs` and perform
operations when patterns or conditions are satisfied:
  - `(?txpr sel .. tx-expr)`: recursively traverse current data object and replace
    matches with `tx-expr`. `tx-expr` can be a function name or expression. Also
    traverses vectors and `kv` values (not keys).
  - `(?mxpr (sel .. tx-expr) .. (sel .. tx-expr))`: one or more matches and
    transforms.  Performs the transform of the first match only.

## Query Utility Functions

The internal representation of in `lqn` means you can use the regular CL
utilities such as `gethash`, `aref`, `subseq`, `length` etc.  But for
convenience there are some utility functions/macros in defined in `lqn`.

### Global Query Context Fxs
Defined in the query scope:
 - `(entry)`: returns `:pipe` if input is from `*standard-input*`; otherwise `:file`.
 - `(fi [k=0])`: counts files from `k`.
 - `(fn)`: name of the current file.
 - `(hld k v)`: hold this value at this key in a key value store.
 - `(ghv k [d])`: get the value of this key; or `d`.

### Operator Context Fxs
Defined in all operators:
 - `_`: the current data object.
 - `(itr)`: the current object in the iteration of the enclosing selector.
 - `(par)`: the object containing `(itr)`.
 - `(psize)`: number of items in `(par)`.
 - `(isize)`: number of items in `(itr)`.
 - `(cnt [k=0])`: counts from `k` in the enclosing selector.
`(itr)` and `_` can be the same thing, but in e.g. `kv selectors`, `(itr)` is
the current object, but `_` is the value of the selected key n the current
object.

### Generic Utilities
General utilities:
 - `(?? a expr [res=expr])`: execute `expr` only if `a` is not `nil`. if expr
   is not nil it returns `expr` or `res`; otherwise `nil`.
 - `(fmt f ..)`: format `f` as `string` with these (`format`) args.
 - `(fmt s)`: get printed representation of `s`.
 - `(out f ..)`: format `f` to `*standard-output*` with these (`format`) args. returns `nil`.
 - `(out s)`: output printed representation of `s` to `*standard-output*`. returns `nil`.
 - `(msym? a b)`: compare symbol `a` to `b`. if `b` is a keword or symbol
   a perfect match is required. if `b` is a string it performs a substring
   match. If `b` is an expression, `a` is compared to the evaluated value of `b`.

### KV / Strings / Vectors / Sequences
For all `sequences` and `kvs`:
 - `(@* o d i ..)`: pick these indices/keys from `sequence`/`kv` into new `vector`.
 - `(size? o [d])`: length of `sequence` or number of keys in `kv`
 - `(compct o)`: Remove `nil`, empty `vectors`, empty `kvs` and keys with empty `kvs`.

Make or join `kvs`:
 - `(cat$ ..)`: add all keys from these `kvs` to a new `kv`. left to right.
 - `(new$ :k1 expr1 ..)`: new `kv` with these keys and expressions.

Primarily for sequences (`string`, `vector`, `list`):
 - `(new* ..)`: new `vector` with these elements.
 - `(ind* s i)`: get this index from `sequence`.
 - `(sel* ..)`: get new `vector` with these `ind*s` or `seq*s` from `sequence`.
 - `(seq* v i [j])`: get range `i ..` or `i .. (1- j)` from `sequence`.
 - `(head* s [n=10])`: first `n` items of `sequence`.
 - `(tail* s [n=10])`: last `n` items of `sequence`.
 - `(cat* s ..)`: concatenate these `sequences` to a `vector`.
 - `(flatn* s n)`: flatten `sequence` `n` times into a `vector`.

Primarily for `string` searching. `[i]` means case insensitive
(TOOD: stringify kv args?):
 - `([i]pref? s pref [d])`: `s` if `pref` is a prefix of `s`; or `d`.
 - `([i]sub? s sub [d])`: `s` if `sub` is a substring of `s`; or `d`.
 - `([i]subx? s sub)`: index where `sub` starts in `s`.
 - `([i]suf? s suf [d])`: `s` if `suf` is a suffix of `s`; or `d`.
 - `(repl s from to)`: replace `from` with `to` in `s`.

String maniuplation:
 - `(sup s ..)`: `str!` and upcase.
 - `(sdwn s ..)`: `str!` and downcase.
 - `(trim s)`: trim leading and trailing whitespace from `string`.
 - `(join s x ..)`: join sequence with `x` (`strings` or `chars`), returns `string`.
 - `(splt s x)`: split `s` at all `x` into `vector` with `strings`.
 - `(strcat s ..)`: concatenate these `strings`, or all strings in one or more
   `sequences` of `strings`.

### Type Tests
`(is? o [d])` returns `o` if not `nil`, empty `sequence` or empty `kv`; or `d`.

These functions return the argument if the argument is the corresponding type:
`flt?`, `int?`, `kv?`, `lst?`, `num?`, `str?`, `vec?`, `seq?`.

These functions return the argument parsed as the corresponding type if
possible; otherwise they return the optional second argument: `int!?`, `flt!?`,
`num!?`, `str!?`, `vec!?`, `seq!?`.

### Type Coercion
 - `(str! s ..)`: coerce everything to a `string`.
 - `(vec! a)`: coerce `sequence` to `vector`; or return `(new* a)`.
 - `(sym! a ..)`: stringify, make `symbol`.

## Install
`lqn` requires [SBCL](https://www.sbcl.org/). And is pretty easy to install via
`quicklisp`. SBCL is available in most package managers. And you can get
quicklisp at https://www.quicklisp.org/beta/. Make sure `lqn` is available in
your `quicklisp` `local-projects` folder. Mine is at
`~/quicklisp/local-projects/`.

Then create an alias for SBCL to execute shell wrappers e.g:
```bash
alias jqn="sbcl --script ~/path/to/lqn/bin/jqn-sh.lisp"
alias tqn="sbcl --script ~/path/to/lqn/bin/tqn-sh.lisp"
alias lqn="sbcl --script ~/path/to/lqn/bin/lqn-sh.lisp"
```
Unfortunately this will tend to start quite slow. To make it run faster you can
create an image/core that has `lqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use your core in the alias instead of SBCL.

Below is an example script for creating your own core. You can load your own
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

# Then make aliases like this:
alias lqn="/path/to/lsp.core --script ~/path/to/lqn/bin/lqn-sh.lisp"
```

