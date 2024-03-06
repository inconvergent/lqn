# LQN - Lisp Query Notation

LQN is a Common Lisp libary, query language and terminal utility to query and
transform text files such as `JSON` and `CSV`, as well as Lisp data (`LDN`), The
terminal utilities will parse the input data to internal lisp strucutres
according to the mode. Then the `lqn` query language can be used for queries
and transformations.

`LQN` consists of three terminal commands: `lqn`, `jqn` and `tqn`. For lisp
data, `JSON` and text files respectively.

Here is a small tutorial: https://inconvergent.net/2024/lisp-query-notation/

You can find some more termianl documentation and examples in
[bin/lqn-sh.lisp](lqn), [bin/jqn-sh.lisp](jqn), and [bin/tqn-sh.lisp](tqn).

See [docs/lqn.md](docs/lqn.md) for symbol documentation.

## Object Representation

Internally `JSON` arrays are represented as `vector`. and `JSON` objects are
represented as `hash-table`; `kv` (key/value) is used in the docs for short.
In `tqn` lines of text are `vectors` of `strings`. In `lqn` Lisp files are
read as a `vector` of lisp data.

## Operators

The following operators have special behaviour. You can also write generic CL
code, anywhere you can use an operator. Including the functions further down.
Note that you can use `_` to refer to the current value.

In the following sections `[d]` represents an optional default value. E.g. if
key/index is missing, or if a functon returns `nil`. `k` is an initial counter
value. Whereas `..` menans that there can be arbitrary arguments, `Selectors`
or `exprs`; depending on the context. `expr` denotes any expression or
operator, like `(+ 1 _)` or `#[:id]`.

### Strings and :keywords
In operators, and many functions, `:keywords` can be used to represent
lowercase `strings`. This is useful in the terminal to avoid escaping strings.
Particularly when using `Selector` operators.

### Pipe Operator - `||`
`(|| expr ..)` pipes the results from the first clause/expression to the
second, and so on.  Returns the result of the last clause. Pipe is the operator that
surrounds all terminal queries by default.

For convenience the pipe has the following default translations:
  - `fx`: to `(?map (fx _))`: map `fx` across all items.
  - `:word`: to `[(isub? _ "word")]` to filter by `"word"`.
  - `"Word"`: to `[(sub? _ "Word")]` to filter by `Word`, case sensitive.
  - `(..)`: to itself. That is, expressions are not translated.

### Map Operator - `#()`/`?map`
Map operations over `vector` or `kv`:
  - `#(fx)` or `(?map fx)`: map `(fx _)` across all items.
  - `#(expr ..)` or `(?map expr ..)`: evaluate these expressions
    sequentially on all items in `sequence`.

### Get Operator - `@`
select keys, indexes or paths from nested structure:
 - `(@ k)`: get this key/index/path from current value.
 - `(@ k [d])`: get this key/index/path from current value.
 - `(@ o k [d])`: get this key/index/path from `o`.

### Selector Operators - `{}`/`$$`, `#{}`/`*$`, `#[]`/`$*`, `@`
Select from on structure into a new data structure. using selectors:
  - ` {s1 sel ..}` or `($$ sel ..)`: from `kv` into new `kv`.
  - `#{s1 sel ..}` or `(*$ sel ..)`: from `vector` of `kvs` into new `vector`
    of `kvs`.
  - `#[s1 sel ..]` or `($* sel ..)`: from `vector` of `kvs` into new `vector`.

A selector is a triple `(mode key expr)`. Only key is required. If `expr` is
not provided the `expr` is `_`, that is: the value of the `key`. The modes are
as follows:
  - `+`: always include this `expr`. [default]
  - `?`: include `expr` if the key is present and not `nil`.
  - `%`: include Selector if `expr` is not `nil`.
  - `-`: drop this key in `#{}` and `{}` operators; ignore Selector entirely in
    `#[]` E.g. `{_ -@key3}` to select all keys except `key3`. `expr` is
    ignored.

Selectors can either be written out in full, or they can be be written in short
form depending on what you want to achieve. The `@` in the following examples
is used to append a mode to a key without having to wrap the Selector in
parenthesis. If you need eg. case or spaces you can use `"strings"`. Here are
some examples using `{}`. It behaves the  same for the other Selector
operators:
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
 (:key3 (or _ "That")) ; select the value of "key3", or literally "That".
 (:key2 (+ 33 _))}     ; add 33 to value of "key2"

; override and drop keys:
{_                ; select all keys, then override these:
 (:key2 (sdwn _)) ; lowercase the value of "key2"
  :-@key3}        ; drop "key3"
```
We use `{}` in the examples but all `KV Selectors` have the same behaviour.

### Filter Operator - `[]`/`**` TODO TODO TODO
Filter .......
  - ` [s1 sel ..]` or `(** sel ..)`: from `vector` into new `vector` using
    `EXPR Selectors`.

`EXPR Selectors` serve a similar purpose as `KV Selectors`, but they are used
with `[]`, `?srch`, `?xpr`, `?txpr`, `?mxpr` operators, and the modes behave a
little differently:
  - `+`: if there are multiple `Selectors` with `+` mode, requires ALL
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

### Fold Operator - `?fld`
Reduce `vector`:
  - `(?fld init fx)`: fold `(fx acc _)` with `init` as the first `acc` value.
    `acc` is inserted as the first argument to `fx`.
  - `(?fld init (fx .. _ ..))`: fold `(fx acc .. _ ..)`. The accumulator is
    inserted as the first argument to `fx`.
  - `(?fld init acc (fx .. acc .. nxt))`: fold `(fx .. acc .. nxt)`. Use this
    if you need to name the accumulator explicity.

### Group by Operator - `?grp`
  TODO

### Recursion Operator - `?rec`
Repeat the same expression while something is true:
 - `(?rec test-expr expr)`: repeat `expr` while `test-expr`. `_` refers to the
   input value, then to the most recent evaluation of `expr`. Use `(cnt)` to
   get the number of the current iteration. `(par)` always refers to the input
   value.

### Search Operator - `?srch`
Iterate a datastructure (as if with `?txpr`) and collect the matches in a new
`vector`:
  - `(?srch sel)`: collect `_` whenever the `Selector` matches.
  - `(?srch sel .. expr)`: collect `expr` whenever the `Selector` matches.

### Transformer Operators - `?xpr`, `?txpr`, `?mxpr`
Perform operation on when pattern or condition is satisfied:
  - `(?xpr sel)`: match current value against `EXPR Selector`. Return the
    result if not `nil`.
  - `(?xpr sel hit-expr)`: match current value against `EXPR Selector`.
    Evaluates `hit-expr` if not nil. `_` is the matching item.
  - `(?xpr sel .. hit-expr miss-expr)`: match current value against `expr
    selectors`.  Evaluate `hit-expr` if not `nil`; else evaluate `miss-expr`.
    `_` is the matching item.

Recursively traverse a structure of `sequences` and `kvs` and return
a new value for each match:
  - `(?txpr sel .. tx-expr)`: recursively traverse current value and replace
    matches with `tx-expr`. `tx-expr` can be a function name or expression.
    Also traverses vectors and `kv` values.
  - `(?mxpr (sel .. tx-expr) .. (sel .. tx-expr))`: one or more matches and
    transforms.  Performs the transform of the first match only.

## Query Utilities

The internal representation of in `lqn` means you can use the regular CL
utilities such as `gethash`, `aref`, `subseq`, `length` etc.  But for
convenience there are some utility functions/macros in defined in `lqn`. Some
of them are described below. There are more in the documentation.

### Global Query Context Fxs
Defined in the query scope:
 - `(fi [k=0])`: counts files from `k`.
 - `(fn)`: name of the current file; or `":internal:"`, `"pipe"`.
 - `(hld k v)`: hold this value at this key in a key value store.
 - `(ghv k [d])`: get the value of this key; or `d`.
 - `(nope [d])`: stop execution, return `d`.
 - `(err [msg])`: raise `error` with `msg`.
 - `(wrn [msg])`: raise `warn` with `msg`.

### Operator Context Fxs
Defined in all operators:
 - `_`: the current value.
 - `(cnt)`: counts from `0` in the enclosing `Selector`.
 - `(key)`: the current `key` if the current value is a `kv`. Otherwise `(cnt)`.
 - `(itr)`: the current object in the iteration of the enclosing `Selector`.
 - `(par)`: the object containing `(itr)`.
 - `(psize)`: number of items in `(par)`.
 - `(isize)`: number of items in `(itr)`.

### Generic Utilities
General utilities:
 - `(?? a expr [res=expr])`: execute `expr` only if `a` is not `nil`. if `expr`
   is not nil it returns `expr` or `res`; otherwise `nil`.
 - `(fmt f ..)`: format `f` as `string` with these (`format`) args.
 - `(fmt s)`: get printed representation of `s`.
 - `(out f ..)`: format `f` to `*standard-output*` with these (`format`) args.
   returns `nil`.
 - `(out s)`: output printed representation of `s` to `*standard-output*`.
   returns `nil`.
 - `(msym? a b)`: compare `symbol` `a` to `b`; if `b` is a `keyword` or `symbol`
   a perfect match is required; if `b` is a `string` it performs a substring
   match; if `b` is an expression, `a` is compared to the evaluated value of
   `b`.
 - `(noop ..)`: do nothing, return `nil`.

### KV / Strings / Vectors / Sequences
For all `sequences` and `kvs`:
 - `(@* o d i ..)`: pick these indices/keys from `sequence`/`kv` into new
   `vector`.
 - `(size? o [d])`: length of `sequence` or number of keys in `kv`.
 - `(all? o [empty])`: are all items in `sequence` something? or `empty`.
 - `(some? o [empty])`: are some items in `sequence` something? or `emtpy`.
 - `(empty? o [d])`: is `sequence` or `kv` empty?.
 - `(compct o)`: Remove `nil`, empty `vectors`, empty `kvs` and keys with empty
   `kvs`.

Make or join `kvs`:
 - `(cat$ ..)`: add all keys from these `kvs` to a new `kv`. left to right.
 - `(new$ :k1 expr1 ..)`: new `kv` with these keys and expressions.

Primarily for `sequences` (`string`, `vector`, `list`):
 - `(new* ..)`: new `vector` with these elements.
 - `(ind* s i)`: get this index from `sequence`.
 - `(sel ..)`: get new `vector` with these `ind*s` or `seqs` from `sequence`.
 - `(seq v i [j])`: get range `i ..` or `i .. (1- j)` from `sequence`.
 - `(head s [n=10])`: first `n` items of `sequence`.
 - `(tail s [n=10])`: last `n` items of `sequence`.
 - `(cat* s ..)`: concatenate these `sequences` to a `vector`.
 - `(flatn* s [n=1] [str=nil])`: flatten `sequence` `n` times into a `vector`.
   if `str=t` strings are flattened into individual chars as well.
 - `(flatall* s [str=nil])`: flatten all `sequences` (except `strings`) into
   new `vector`. Use `t` as the second argument to flatten `strings` to
   individual chars as well.
 - `(flatn$ s n)`: flatten `kv` into vector `(new* k0 v0 k1 v1 ..)`

Primarily for `string` searching. `[i]` means case insensitive:
 - `([i]pref? s pref [d])`: `s` if `pref` is a prefix of `s`; or `d`.
 - `([i]sub? s sub [d])`: `s` if `sub` is a substring of `s`; or `d`.
 - `([i]subx? s sub)`: index where `sub` starts in `s`.
 - `([i]suf? s suf [d])`: `s` if `suf` is a suffix of `s`; or `d`.
 - `(repl s from to)`: replace `from` with `to` in `s`.

String maniuplation:
 - `(sup s ..)`: `str!` and upcase.
 - `(sdwn s ..)`: `str!` and downcase.
 - `(trim s)`: trim leading and trailing whitespace from `string`.
 - `(splt s x [trim=t] [prune=nil])`: split `s` at all `x` into `vector` of
   `strings`. `trim` removes whitespace. `prune` drops empty strings.
 - `(join s x ..)`: join sequence with `x` (`strings` or `chars`), returns
   `string`.
 - `(strcat s ..)`: concatenate these `strings`, or all `strings` in one or
   more `sequences` of `strings`.

### Type Coercion and Tests
`(is? o [d])` returns `o` if not `nil`, empty `sequence`, or empty `kv`; or `d`.

These functions return the argument if the argument is the corresponding type:
`flt?`, `int?`, `kv?`, `lst?`, `num?`, `str?`, `vec?`, `seq?`.

These functions return the argument parsed as the corresponding type if
possible; otherwise they return the optional second argument: `int!?`, `flt!?`,
`num!?`, `str!?`, `vec!?`, `seq!?`.

The following functions will coerce the argument, or fail if the coercion is not supported:
`str!`, `int!`, `flt!`, `lst!` `sym!`,

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
Unfortunately this will tend to have a high startup time. To make it run faster
you can create an SBCL image/core that has `lqn` preloaded and dump it using
`sb-ext:save-lisp-and-die`. Then use the core in the alias instead of SBCL.

Below is an example script for creating your own core. You can also preload
your own libraries which will be available to `lqn`.
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
