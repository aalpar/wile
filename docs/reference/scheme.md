# Wile Scheme Reference

A complete reference for Wile's Scheme language, covering lexical syntax, data types, special forms, standard procedures, libraries, and extensions. Wile implements R7RS-small with hygienic macros (Flatt 2016), first-class continuations, a full numeric tower, and Go concurrency primitives.

---

## Table of Contents

- [Lexical Syntax](#lexical-syntax)
- [Data Types](#data-types)
- [Special Forms](#special-forms)
- [Derived Forms](#derived-forms)
- [Standard Procedures](#standard-procedures)
- [Libraries](#libraries)
- [Feature Flags](#feature-flags)
- [Extensions Beyond R7RS](#extensions-beyond-r7rs)
- [Known Semantic Differences](#known-semantic-differences)

---

## Lexical Syntax

### Comments

```scheme
; Line comment — extends to end of line

#; datum       ; Datum comment — skips one complete datum
#;(this entire list is skipped) (+ 1 2)  ; evaluates to 3

#| Block comment
   Can span multiple lines
   #| and can nest |#
|#
```

### Booleans

```scheme
#t    ; true (also #true)
#f    ; false (also #false)
```

### Numbers

**Integers** — exact 64-bit signed, with automatic promotion to arbitrary precision on overflow:

```scheme
42              ; decimal
#b101010        ; binary (42)
#o52            ; octal (42)
#x2A            ; hexadecimal (42)
#e1.5           ; exact prefix → 3/2
#i42            ; inexact prefix → 42.0
```

**Floating point** — inexact IEEE 754 double-precision:

```scheme
3.14            ; decimal float
1e10            ; scientific notation
1.5e-3          ; 0.0015
+inf.0          ; positive infinity
-inf.0          ; negative infinity
+nan.0          ; not-a-number
```

**Rationals** — exact fractions, auto-reduced:

```scheme
(/ 1 3)         ; → 1/3 (exact rational)
(/ 22 7)        ; → 22/7
(+ 1/3 1/6)     ; → 1/2
```

**Complex numbers**:

```scheme
3+4i            ; rectangular form
0+1i            ; pure imaginary
(make-rectangular 3 4)   ; → 3+4i
(make-polar 5 0.927)     ; magnitude + angle
```

**Arbitrary precision** (Wile extension):

```scheme
#z12345678901234567890         ; BigInteger (exact, always decimal)
#m3.14159265358979323846       ; BigFloat (inexact, 256-bit)
```

### Characters

Characters are written with the `#\` prefix:

```scheme
#\a             ; lowercase a
#\A             ; uppercase A
#\space         ; space character
#\newline       ; newline
#\tab           ; tab
#\return        ; carriage return
#\alarm         ; bell (U+0007)
#\backspace     ; backspace (U+0008)
#\delete        ; delete (U+007F)
#\escape        ; escape (U+001B)
#\null          ; null (U+0000)
#\x41           ; hex scalar value (A)
#\x03BB         ; Unicode: λ
```

### Strings

Double-quoted, with these escape sequences:

| Escape | Character |
|--------|-----------|
| `\\` | backslash |
| `\"` | double quote |
| `\a` | alarm (U+0007) |
| `\b` | backspace (U+0008) |
| `\t` | tab (U+0009) |
| `\n` | newline (U+000A) |
| `\r` | return (U+000D) |
| `\xHH;` | hex scalar value |
| `\<newline><whitespace>` | line continuation (ignored) |

```scheme
"hello"                   ; simple string
"line 1\nline 2"          ; embedded newline
"\x03BB; calculus"        ; Unicode: "λ calculus"
"long \                   ; line continuation:
  string"                 ; "long string"
```

### Symbols

Identifiers follow R7RS rules. Most characters are allowed except whitespace and these: `( ) [ ] { } " , ' ` ; # |`

```scheme
hello
list->vector
string=?
set!
+
...
a34kTMNs
```

Vertical-bar syntax allows arbitrary characters in symbol names:

```scheme
|Hello World|           ; symbol with space
|two words|             ; another
|two\x20;words|         ; same as above (hex escape)
```

### Vectors

```scheme
#(1 2 3)                ; vector of integers
#()                     ; empty vector
```

### Bytevectors

```scheme
#u8(0 127 255)          ; bytevector
#u8()                   ; empty bytevector
```

### Pairs and Lists

```scheme
'(1 2 3)                ; proper list
'(1 . 2)                ; pair (dotted pair)
'(1 2 . 3)              ; improper list
'()                     ; empty list (null)
```

### Quasiquotation

```scheme
`(a ,(+ 1 2) b)              ; → (a 3 b)
`(a ,@(list 1 2) b)          ; → (a 1 2 b)
`#(a ,(+ 1 2))               ; → #(a 3)  (vector quasiquote)
```

### Reader Directives

```scheme
#!fold-case             ; subsequent identifiers are case-insensitive
#!no-fold-case          ; restore case sensitivity (default)
#!r7rs                  ; R7RS mode marker (accepted, no effect)
```

Every `#!<name>` token is read as a directive and discarded, so an unrecognized
one contributes no datum rather than raising. There is no `#!void` or `#!eof`
read syntax: the void value comes from `(if #f #f)` and the end-of-file object
from `(eof-object)`. Both *write* as `#<void>` and `#<eof>` — the unreadable-object
bracket the reader rejects, not a directive it would discard. Rendering them as
`#!void` / `#!eof` let the writer manufacture input the reader silently deletes:
`(write (vector 1 (if #f #f) 2))` produced `#(1 #!void 2)`, which read back as a
two-element vector.

### Datum Labels

Datum labels allow shared and circular structure in read syntax:

```scheme
#0=(a b . #0#)          ; circular list
(#1="hello" #1#)        ; shared structure
```

Circular labels are a reader feature: `read` returns the cyclic structure, but a
cyclic `quote` literal in a program is rejected at compile time.

---

## Data Types

Wile's type system spans the full R7RS specification plus Go interoperability types.

### Core Types

| Type | Predicate | Description |
|------|-----------|-------------|
| Boolean | `boolean?` | `#t` and `#f` |
| Character | `char?` | Unicode code point |
| Symbol | `symbol?` | Interned identifier |
| Pair | `pair?` | Cons cell (`car` + `cdr`) |
| Null | `null?` | Empty list `'()` |
| Vector | `vector?` | Fixed-size mutable array |
| String | `string?` | Mutable UTF-8 text |
| Bytevector | `bytevector?` | Fixed-size byte array |
| Procedure | `procedure?` | Lambda or primitive |
| Box | `box?` | Mutable single-value container |
| Hashtable | `hashtable?` | Hash table |
| Record | `record?` | User-defined record instance |

### Numeric Tower

Scheme numbers form a tower: integer < rational < real < complex. Wile maps this onto concrete types:

| Type | Predicate | Exactness | Backed by |
|------|-----------|-----------|-----------|
| Integer | `integer?`, `exact-integer?` | exact | Go `int64` |
| BigInteger | `integer?` | exact | `math/big.Int` |
| Rational | `rational?` | exact | `math/big.Rat` |
| Float | `real?` | inexact | Go `float64` |
| BigFloat | `real?` | inexact | `math/big.Float` (256-bit) |
| Complex | `complex?` | inexact | Go `complex128` |
| BigComplex | `complex?` | exact if both parts are | `math/big` parts (`3+4i` is exact) |

**Key rules**:

- Exact operations on exact inputs produce exact results: `(+ 1 1)` → `2` (exact), `(/ 1 3)` → `1/3` (exact rational)
- Inexactness is contagious: `(+ 1 1.0)` → `2.0` (inexact)
- Integer overflow promotes automatically: `(expt 2 100)` → BigInteger
- `exact` and `inexact` convert between domains

### I/O Port Types

| Type | Predicate | Description |
|------|-----------|-------------|
| Character Input Port | `input-port?`, `textual-port?` | Text input stream |
| Character Output Port | `output-port?`, `textual-port?` | Text output stream |
| Binary Input Port | `input-port?`, `binary-port?` | Byte input stream |
| Binary Output Port | `output-port?`, `binary-port?` | Byte output stream |
| String Port | `port?` | Port backed by string buffer |
| Bytevector Port | `port?` | Port backed by bytevector buffer |

### Control Flow Types

| Type | Predicate | Description |
|------|-----------|-------------|
| Continuation | `procedure?` | Captured execution context from `call/cc` |
| Promise | `promise?` | Delayed computation (`delay`/`force`) |
| Parameter | `parameter?` | Dynamic binding (`make-parameter`/`parameterize`) |
| Prompt Tag | `continuation-prompt-tag?` | Delimiter for delimited continuations |

### Error Types

| Type | Predicate | Description |
|------|-----------|-------------|
| Error Object | `error-object?` | Exception with message and irritants |
| EOF Object | `eof-object?` | End-of-file marker |

### Record Types

User-defined record types via `define-record-type`:

```scheme
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))
(point? p)     ; → #t
(point-x p)    ; → 3
```

### Void

The void value, written `#<void>` and tested by `void?`, is returned by side-effecting operations like `set!`, `display`, `vector-set!`. Not a "false" value — only `#f` is false in conditionals.

---

## Special Forms

Special forms are handled at compile time. They cannot be passed as values or applied.

### `if` — Conditional

```scheme
(if <test> <consequent>)
(if <test> <consequent> <alternate>)
```

Only `#f` is false. Everything else — including `0`, `""`, `'()`, and the void value — is true.

### `lambda` — Procedure Creation

```scheme
(lambda (<formals>) <body>)
(lambda (<formal> ... . <rest>) <body>)   ; rest parameter
(lambda <formal> <body>)                  ; all args as list
```

```scheme
(lambda (x y) (+ x y))           ; fixed arity
(lambda (x . rest) rest)          ; one required + rest
(lambda args (length args))       ; all args as list
```

### `case-lambda` — Multiple Arities

```scheme
(case-lambda
  (()      0)
  ((x)     x)
  ((x y)   (+ x y))
  ((x . rest) (apply + x rest)))
```

Dispatches on argument count. First matching clause wins.

### `define` — Definitions

```scheme
(define <variable> <expression>)
(define (<variable> <formals>) <body>)      ; shorthand for lambda
(define (<variable> . <rest>) <body>)       ; shorthand with rest param
```

```scheme
(define x 42)
(define (square n) (* n n))
(define (f x . rest) (cons x rest))
```

### `set!` — Mutation

```scheme
(set! <variable> <expression>)
```

Mutates an existing binding. The variable must already be defined. Top-level definitions are immutable by default, so a top-level `set!` raises unless the engine was built with `WithMutableTopLevel()`; see [`r7rs-differences.md`](r7rs-differences.md).

### `quote` — Literal Data

```scheme
(quote <datum>)
'<datum>               ; shorthand
```

```scheme
'(1 2 3)               ; list
'hello                 ; symbol
'#(1 2)                ; vector
```

### `begin` — Sequencing

```scheme
(begin <expression1> <expression2> ...)
```

Evaluates expressions left to right, returns the last value. At top level, `begin` splices definitions into the enclosing scope.

### `define-syntax` — Macro Definition

```scheme
(define-syntax <keyword> <transformer>)
```

Binds a macro transformer. The transformer is typically `syntax-rules` or `syntax-case`.

### `syntax-rules` — Pattern-Based Macros (R7RS)

```scheme
(syntax-rules (<literal> ...)
  (<pattern> <template>)
  ...)

(syntax-rules <ellipsis> (<literal> ...)   ; custom ellipsis
  (<pattern> <template>)
  ...)
```

Patterns match input forms; templates construct output. Pattern variables in templates are replaced by matched subforms. The ellipsis `...` matches zero or more repetitions.

```scheme
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))
```

**Pattern syntax**:

| Pattern | Matches |
|---------|---------|
| `_` | anything (wildcard, no binding) |
| `<identifier>` | anything (binds as pattern variable) |
| `<literal>` | that literal identifier (free-identifier=?) |
| `(<pattern> ...)` | list with zero or more of pattern |
| `(<pattern> <pattern> ... . <pattern>)` | improper list |
| `#(<pattern> ...)` | vector |
| `<constant>` | that datum (via `equal?`) |

### `syntax-case` — Procedural Macros (R6RS)

```scheme
(syntax-case <expr> (<literal> ...)
  (<pattern> <body>)
  (<pattern> <fender> <body>)
  ...)
```

Like `syntax-rules` but the body is arbitrary Scheme code, not a template. Pattern variables are bound as syntax objects. Use `(syntax <template>)` or `#'<template>` to construct output.

```scheme
(define-syntax my-or
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       #'(let ((t a)) (if t t b))))))
```

### `syntax` — Construct Syntax Object

```scheme
(syntax <template>)
#'<template>               ; shorthand
```

Used inside `syntax-case` bodies to construct output syntax with pattern variable substitution.

### `with-syntax` — Bind Pattern Variables

```scheme
(with-syntax ((<pattern> <expression>) ...)
  <body>)
```

Binds pattern variables from expressions, then expands body as a template.

### `quasisyntax` / `unsyntax` / `unsyntax-splicing`

```scheme
#`(a #,expr b)              ; quasisyntax with unsyntax
#`(a #,@expr-list b)        ; quasisyntax with unsyntax-splicing
```

The syntax-object equivalent of quasiquote — constructs syntax templates with computed parts.

### `let-syntax` / `letrec-syntax` — Local Macros

```scheme
(let-syntax ((<keyword> <transformer>) ...)
  <body>)

(letrec-syntax ((<keyword> <transformer>) ...)
  <body>)
```

Local macro definitions. `letrec-syntax` allows transformers to reference each other.

### `include` / `include-ci`

```scheme
(include <filename>)
(include-ci <filename>)        ; case-insensitive identifiers
```

Compile-time file inclusion. Contents are spliced into the enclosing scope.

### `cond-expand` — Feature-Based Conditional Expansion

```scheme
(cond-expand
  (<feature-requirement> <expression> ...)
  ...
  (else <expression> ...))
```

Compile-time conditional. Feature requirements:

| Form | Meaning |
|------|---------|
| `<identifier>` | Feature identifier is supported |
| `(library <name>)` | Library is available |
| `(and <req> ...)` | All requirements satisfied |
| `(or <req> ...)` | At least one satisfied |
| `(not <req>)` | Requirement not satisfied |

### `define-library` / `import` / `export`

```scheme
(define-library (<name> ...)
  (description <string>)                     ; Wile extension
  (export <export-spec> ...)
  (import <import-set> ...)
  (begin <body> ...)
  (include <filename> ...)
  (include-ci <filename> ...)
  (include-library-declarations <filename> ...)
  (cond-expand ...))
```

```scheme
(import <import-set> ...)
```

Import sets support modifiers:

```scheme
(import (scheme base))                        ; all exports
(import (only (scheme base) map for-each))    ; selected
(import (except (scheme base) set! set-car!)) ; excluded
(import (prefix (scheme base) s:))            ; prefixed
(import (rename (scheme base) (car head)))    ; renamed
```

### `dynamic-wind`

```scheme
(dynamic-wind <before> <thunk> <after>)
```

Calls `before` on entry, `thunk` for the body, `after` on exit. The before/after thunks are called even when continuations jump in or out.

### `define-record-type`

```scheme
(define-record-type <type-name>
  (<constructor> <field-name> ...)
  <predicate>
  (<field-name> <accessor>)
  (<field-name> <accessor> <modifier>)
  ...)
```

### `with-continuation-mark` (Wile Extension)

```scheme
(with-continuation-mark <key> <value> <body>)
```

Evaluates `body` with `value` marked under `key` on the current continuation frame, where the procedures under [Continuation Marks](#continuation-marks) can read it back.

### Phase Control (Wile Extension)

```scheme
(define-for-syntax <variable> <expression>)  ; macro-expansion-time binding
(begin-for-syntax <expression> ...)          ; macro-expansion-time evaluation
(eval-when (<situation> ...) <body> ...)     ; control evaluation timing
```

`eval-when` situations follow Chez Scheme (Dybvig, TSPL §12.10). `expand` and
`compile` run the body at compile time; `run`, `load`, and `eval` compile it for
runtime execution; `visit` is accepted and does nothing. Naming both kinds, as in
`(eval-when (expand run) ...)`, does both. Any other symbol is a compile error.

`define-for-syntax` and `begin-for-syntax` bodies run one phase above the form's
own phase, so they nest: at the top level a `begin-for-syntax` body runs at phase
1, and a `begin-for-syntax` inside one runs at phase 2. Each phase is hermetic,
so a name a body needs must be imported at that body's phase (see the `for-meta`
import modifiers under [Libraries](#libraries)).

### `syntax-error`

```scheme
(syntax-error <message> <irritant> ...)
```

Signals a compile-time error. Useful in macro templates for invalid pattern matches.

---

## Derived Forms

These are defined in the bootstrap environment rather than the compiler: macros expanding to core special forms, except `map` and `for-each`, which are Scheme procedures.

### `and` / `or`

```scheme
(and <test> ...)     ; short-circuit logical and; returns last true or #f
(or <test> ...)      ; short-circuit logical or; returns first true or #f
```

### `let` / `let*` / `letrec` / `letrec*`

```scheme
(let ((<var> <init>) ...) <body>)        ; parallel binding
(let <name> ((<var> <init>) ...) <body>) ; named let (loop)
(let* ((<var> <init>) ...) <body>)       ; sequential binding
(letrec ((<var> <init>) ...) <body>)     ; recursive binding
(letrec* ((<var> <init>) ...) <body>)    ; sequential recursive binding
```

Named `let` is the standard looping construct:

```scheme
(let loop ((i 0) (acc '()))
  (if (= i 5)
      (reverse acc)
      (loop (+ i 1) (cons i acc))))
; → (0 1 2 3 4)
```

### `let-values` / `let*-values` / `define-values`

```scheme
(let-values (((<var> ...) <expression>) ...) <body>)
(let*-values (((<var> ...) <expression>) ...) <body>)
(define-values (<var> ...) <expression>)
```

Bind multiple return values:

```scheme
(let-values (((q r) (floor/ 17 5)))
  (list q r))   ; → (3 2)
```

### `cond`

```scheme
(cond (<test> <expression> ...)
      ...
      (else <expression> ...))

(cond (<test> => <procedure>)   ; apply procedure to test result
      ...)
```

### `case`

```scheme
(case <key>
  ((<datum> ...) <expression> ...)
  ...
  (else <expression> ...))

(case <key>
  ((<datum> ...) => <procedure>)   ; apply procedure to key
  ...)
```

### `when` / `unless`

```scheme
(when <test> <expression> ...)
(unless <test> <expression> ...)
```

### `do` — Iteration

```scheme
(do ((<var> <init> <step>) ...)
    (<test> <result> ...)
  <command> ...)
```

```scheme
(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((= i 10) sum))    ; → 45
```

### `delay` / `delay-force` / `force` / `make-promise`

```scheme
(delay <expression>)          ; create a promise
(delay-force <expression>)    ; create an iterative promise
(force <promise>)             ; force evaluation
(make-promise <value>)        ; already-forced promise
```

Promises support proper tail recursion via `delay-force`:

```scheme
(define (stream-ref s n)
  (if (= n 0)
      (force (car s))
      (stream-ref (force (cdr s)) (- n 1))))
```

### `parameterize` — Dynamic Binding

```scheme
(parameterize ((<parameter> <value>) ...)
  <body>)
```

```scheme
(define current-precision (make-parameter 10))
(parameterize ((current-precision 20))
  (current-precision))   ; → 20
; outside: (current-precision) → 10
```

### `guard` — Exception Handling

```scheme
(guard (<variable>
        (<test> <expression> ...)
        ...
        (else <expression> ...))
  <body>)
```

```scheme
(guard (exn
        ((error-object? exn)
         (string-append "caught: " (error-object-message exn)))
        (else "unknown error"))
  (error "something went wrong"))
; → "caught: something went wrong"
```

Wile extension: `guard` correctly propagates multiple values from the body (R7RS reference implementation drops them).

### `map` / `for-each`

```scheme
(map <procedure> <list> ...)
(for-each <procedure> <list> ...)
```

These are `case-lambda` procedures written in Scheme (not Go primitives) so that continuations captured inside the procedure body work correctly. Both loops are tail-recursive, so neither has a call-depth ceiling.

### `with-continuation-barrier`

```scheme
(with-continuation-barrier <body> ...)
```

Prevents continuations from re-entering or escaping across the barrier.

---

## Standard Procedures

### Equivalence Predicates

| Procedure | Description |
|-----------|-------------|
| `(eq? a b)` | Identity comparison (same object) |
| `(eqv? a b)` | Equivalence (same type and value for primitives) |
| `(equal? a b)` | Recursive structural equality |
| `(boolean=? b ...)` | All booleans equal (variadic) |
| `(symbol=? s ...)` | All symbols equal (variadic) |

### Boolean Operations

| Procedure | Description |
|-----------|-------------|
| `(not x)` | `#t` if x is `#f`, else `#f` |

### Arithmetic

| Procedure | Description |
|-----------|-------------|
| `(+ z ...)` | Sum (0 with no args) |
| `(- z)` | Negation |
| `(- z1 z2 ...)` | Difference |
| `(* z ...)` | Product (1 with no args) |
| `(/ z)` | Reciprocal |
| `(/ z1 z2 ...)` | Division (exact: produces rational) |
| `(abs x)` | Absolute value |
| `(max x ...)` | Maximum |
| `(min x ...)` | Minimum |

### Numeric Comparison

| Procedure | Description |
|-----------|-------------|
| `(= z ...)` | Numeric equality (variadic) |
| `(< x ...)` | Strictly increasing |
| `(> x ...)` | Strictly decreasing |
| `(<= x ...)` | Non-decreasing |
| `(>= x ...)` | Non-increasing |

### Integer Division

| Procedure | Description |
|-----------|-------------|
| `(quotient n d)` | Integer quotient (truncate toward zero) |
| `(remainder n d)` | Remainder (sign of dividend) |
| `(modulo n d)` | Modulo (sign of divisor) |
| `(floor/ n d)` | Returns two values: floor quotient and remainder |
| `(floor-quotient n d)` | Floor quotient |
| `(floor-remainder n d)` | Floor remainder |
| `(truncate/ n d)` | Returns two values: truncate quotient and remainder |
| `(truncate-quotient n d)` | Truncate quotient |
| `(truncate-remainder n d)` | Truncate remainder |

### Rounding

| Procedure | Description |
|-----------|-------------|
| `(floor x)` | Round toward −∞ |
| `(ceiling x)` | Round toward +∞ |
| `(truncate x)` | Round toward zero |
| `(round x)` | Round to nearest even |

### Exact/Inexact

| Procedure | Description |
|-----------|-------------|
| `(exact z)` | Convert to exact |
| `(inexact z)` | Convert to inexact |
| `(exact->inexact z)` | R5RS alias for `inexact` |
| `(inexact->exact z)` | R5RS alias for `exact` |
| `(exact? z)` | Test if exact |
| `(inexact? z)` | Test if inexact |

### Numeric Predicates

| Procedure | Description |
|-----------|-------------|
| `(number? x)` | Is a number |
| `(complex? x)` | Is a complex number |
| `(real? x)` | Is a real number |
| `(rational? x)` | Is a rational number |
| `(integer? x)` | Is an integer |
| `(exact-integer? x)` | Is an exact integer |
| `(zero? z)` | Is zero |
| `(positive? x)` | Is positive |
| `(negative? x)` | Is negative |
| `(odd? n)` | Is odd |
| `(even? n)` | Is even |
| `(finite? z)` | Is finite |
| `(infinite? z)` | Is infinite |
| `(nan? z)` | Is NaN |

### Other Numeric

| Procedure | Description |
|-----------|-------------|
| `(gcd n ...)` | Greatest common divisor |
| `(lcm n ...)` | Least common multiple |
| `(numerator q)` | Numerator of rational |
| `(denominator q)` | Denominator of rational |
| `(rationalize x y)` | Simplest rational within y of x |
| `(square z)` | z * z |
| `(expt z1 z2)` | z1 raised to z2 |
| `(sqrt z)` | Square root (exact for perfect squares) |
| `(exact-integer-sqrt k)` | Returns two values: root and remainder |
| `(number->string z)` | Convert number to string |
| `(number->string z radix)` | Convert with radix (2, 8, 10, 16) |
| `(string->number s)` | Parse number (or `#f`) |
| `(string->number s radix)` | Parse with radix |

### Transcendental Functions

All return inexact results:

| Procedure | Description |
|-----------|-------------|
| `(exp z)` | e^z |
| `(log z)` | Natural log |
| `(log z base)` | Log base |
| `(sin z)` | Sine |
| `(cos z)` | Cosine |
| `(tan z)` | Tangent |
| `(asin z)` | Arcsine |
| `(acos z)` | Arccosine |
| `(atan y)` | Arctangent |
| `(atan y x)` | Two-argument arctangent |

### Complex Numbers

| Procedure | Description |
|-----------|-------------|
| `(make-rectangular x y)` | x + yi |
| `(make-polar r θ)` | r × e^(iθ) |
| `(real-part z)` | Real part |
| `(imag-part z)` | Imaginary part |
| `(magnitude z)` | Absolute value |
| `(angle z)` | Phase angle |

### Pairs and Lists

| Procedure | Description |
|-----------|-------------|
| `(cons a d)` | Construct pair |
| `(car p)` | First element |
| `(cdr p)` | Rest |
| `(set-car! p v)` | Mutate car |
| `(set-cdr! p v)` | Mutate cdr |
| `(list v ...)` | Construct list |
| `(make-list k)` | List of k elements (unspecified fill) |
| `(make-list k fill)` | List of k copies of fill |
| `(length lst)` | List length |
| `(append lst ...)` | Concatenate lists |
| `(reverse lst)` | Reverse list |
| `(list-ref lst k)` | Element at index k |
| `(list-set! lst k v)` | Set element at index k |
| `(list-tail lst k)` | Tail starting at index k |
| `(list-copy lst)` | Shallow copy |
| `(list? x)` | Is proper list |
| `(pair? x)` | Is pair |
| `(null? x)` | Is empty list |

### List Search

| Procedure | Description |
|-----------|-------------|
| `(memq x lst)` | Find x using `eq?` |
| `(memv x lst)` | Find x using `eqv?` |
| `(member x lst)` | Find x using `equal?` |
| `(member x lst cmp)` | Find x using custom comparator |
| `(assq x alist)` | Assoc lookup using `eq?` |
| `(assv x alist)` | Assoc lookup using `eqv?` |
| `(assoc x alist)` | Assoc lookup using `equal?` |
| `(assoc x alist cmp)` | Assoc lookup with custom comparator |

### CxR Accessors

All compositions of `car` and `cdr` up to 4 levels deep: `caar`, `cadr`, `cdar`, `cddr`, `caaar` through `cddddr` (28 total).

### Characters

| Procedure | Description |
|-----------|-------------|
| `(char=? c ...)` | Character equality |
| `(char<? c ...)` | Character ordering |
| `(char>? c ...)` | |
| `(char<=? c ...)` | |
| `(char>=? c ...)` | |
| `(char-ci=? c ...)` | Case-insensitive comparison |
| `(char-ci<? c ...)` | |
| `(char-ci>? c ...)` | |
| `(char-ci<=? c ...)` | |
| `(char-ci>=? c ...)` | |
| `(char->integer c)` | Unicode code point |
| `(integer->char n)` | Code point to character |
| `(char-alphabetic? c)` | Is alphabetic |
| `(char-numeric? c)` | Is numeric |
| `(char-whitespace? c)` | Is whitespace |
| `(char-upper-case? c)` | Is uppercase |
| `(char-lower-case? c)` | Is lowercase |
| `(char-upcase c)` | To uppercase |
| `(char-downcase c)` | To lowercase |
| `(char-foldcase c)` | Case fold for comparison |
| `(digit-value c)` | Numeric value of digit (or `#f`) |

### Strings

| Procedure | Description |
|-----------|-------------|
| `(string c ...)` | String from characters |
| `(make-string k)` | String of k unspecified chars |
| `(make-string k c)` | String of k copies of c |
| `(string-length s)` | Length |
| `(string-ref s k)` | Character at index |
| `(string-set! s k c)` | Set character at index |
| `(string-append s ...)` | Concatenate |
| `(substring s start end)` | Extract substring |
| `(string-copy s)` | Copy |
| `(string-copy s start)` | Copy from start |
| `(string-copy s start end)` | Copy range |
| `(string-copy! to at from)` | Copy into string |
| `(string-copy! to at from start)` | |
| `(string-copy! to at from start end)` | |
| `(string-fill! s c)` | Fill with character |
| `(string-fill! s c start)` | |
| `(string-fill! s c start end)` | |

**String comparison**:

| Procedure | Description |
|-----------|-------------|
| `(string=? s ...)` | Equality |
| `(string<? s ...)` | Lexicographic order |
| `(string>? s ...)` | |
| `(string<=? s ...)` | |
| `(string>=? s ...)` | |
| `(string-ci=? s ...)` | Case-insensitive |
| `(string-ci<? s ...)` | |
| `(string-ci>? s ...)` | |
| `(string-ci<=? s ...)` | |
| `(string-ci>=? s ...)` | |
| `(string-upcase s)` | Uppercase |
| `(string-downcase s)` | Lowercase |
| `(string-foldcase s)` | Case fold |

**String higher-order**:

| Procedure | Description |
|-----------|-------------|
| `(string-map proc s ...)` | Map over characters |
| `(string-for-each proc s ...)` | Apply to characters |

**String conversion**:

| Procedure | Description |
|-----------|-------------|
| `(string->list s)` | String to character list |
| `(string->list s start)` | |
| `(string->list s start end)` | |
| `(list->string lst)` | Character list to string |
| `(string->symbol s)` | String to symbol |
| `(symbol->string s)` | Symbol to string |

### Vectors

| Procedure | Description |
|-----------|-------------|
| `(vector v ...)` | Vector from elements |
| `(make-vector k)` | Vector of k unspecified elements |
| `(make-vector k fill)` | Vector of k copies of fill |
| `(vector-length v)` | Length |
| `(vector-ref v k)` | Element at index |
| `(vector-set! v k val)` | Set element at index |
| `(vector->list v)` | Vector to list |
| `(vector->list v start)` | |
| `(vector->list v start end)` | |
| `(list->vector lst)` | List to vector |
| `(vector-copy v)` | Copy |
| `(vector-copy v start)` | |
| `(vector-copy v start end)` | |
| `(vector-copy! to at from)` | Copy into vector |
| `(vector-copy! to at from start)` | |
| `(vector-copy! to at from start end)` | |
| `(vector-fill! v val)` | Fill with value |
| `(vector-fill! v val start)` | |
| `(vector-fill! v val start end)` | |
| `(vector-append v ...)` | Concatenate vectors |
| `(vector-map proc v ...)` | Map over elements |
| `(vector-for-each proc v ...)` | Apply to elements |
| `(vector->string v)` | Vector of chars to string |
| `(string->vector s)` | String to vector of chars |

### Bytevectors

| Procedure | Description |
|-----------|-------------|
| `(bytevector b ...)` | Bytevector from bytes |
| `(make-bytevector k)` | Bytevector of k zero bytes |
| `(make-bytevector k byte)` | Bytevector of k copies of byte |
| `(bytevector-length bv)` | Length |
| `(bytevector-u8-ref bv k)` | Byte at index |
| `(bytevector-u8-set! bv k byte)` | Set byte at index |
| `(bytevector-copy bv)` | Copy |
| `(bytevector-copy bv start)` | |
| `(bytevector-copy bv start end)` | |
| `(bytevector-copy! to at from)` | Copy into bytevector |
| `(bytevector-copy! to at from start)` | |
| `(bytevector-copy! to at from start end)` | |
| `(bytevector-append bv ...)` | Concatenate |
| `(utf8->string bv)` | Decode UTF-8 |
| `(utf8->string bv start)` | |
| `(utf8->string bv start end)` | |
| `(string->utf8 s)` | Encode to UTF-8 |
| `(string->utf8 s start)` | |
| `(string->utf8 s start end)` | |

### Boxes

| Procedure | Description |
|-----------|-------------|
| `(box v)` | Create box containing v |
| `(box? x)` | Is a box |
| `(unbox b)` | Extract value |
| `(set-box! b v)` | Set value |

### Hashtables

R6RS `(rnrs hashtables)`. **Any object can be a key** — the hash belongs to the
TABLE, not to the key, so which objects count as one key is the constructor's
choice.

**Choosing a constructor.** `make-equal-hashtable` is the right default for
*content*-keyed tables (lists, vectors, strings). It is the wrong one for a key
whose `equal?` **is** identity — a record type, a port, a procedure: those all
hash to a single bucket and scan linearly. Reach for `make-eq-hashtable` there,
which hashes them by identity.

| Procedure | Description |
|-----------|-------------|
| `(make-eq-hashtable [k])` | New table, keys compared with `eq?` |
| `(make-eqv-hashtable [k])` | New table, keys compared with `eqv?` |
| `(make-equal-hashtable [k])` | New table, keys compared with `equal?` (**not R6RS**; Chez/Larceny/Vicare/Ypsilon extension) |
| `(make-hashtable hash equiv [k])` | R6RS spelling; only `(make-hashtable equal-hash equal?)` is accepted |
| `(hashtable? x)` | Is a hash table |
| `(hashtable-ref ht key default)` | Lookup; `default` is **required** |
| `(hashtable-set! ht key value)` | Set entry |
| `(hashtable-delete! ht key)` | Remove entry |
| `(hashtable-contains? ht key)` | Is `key` present |
| `(hashtable-update! ht key proc default)` | Set `key` to `(proc current-or-default)` |
| `(hashtable-keys ht)` | **Vector** of keys |
| `(hashtable-entries ht)` | Two values: keys vector and index-aligned values vector |
| `(hashtable-size ht)` | Entry count |
| `(hashtable-copy ht [mutable])` | Shallow copy; **immutable unless `mutable` is true** |
| `(hashtable-clear! ht [k])` | Remove all entries; `k` accepted and ignored |
| `(hashtable-mutable? ht)` | Does `ht` accept mutation |
| `(hashtable-equivalence-function ht)` | `eq?`, `eqv?` or `equal?` |
| `(hashtable-hash-function ht)` | `equal-hash`, or `#f` for eq/eqv tables |

The optional `k` is R6RS's size hint; Wile ignores it. Hash procedures:
`equal-hash`, `string-hash`, `string-ci-hash`, `symbol-hash`, each returning an
exact non-negative integer. `equal-hash` terminates on cyclic input.

**Two behaviours worth knowing.** `(hashtable-copy ht)` with no second argument
returns an **immutable** table, reversing Wile's pre-R6RS behaviour — pass `#t`
to copy-and-mutate. And there is no `hashtable-values`: `hashtable-entries`
subsumes it, and is the only way to get keys and values paired reliably, since
two independent reads of a lock-free table need not agree.

### Control Flow

| Procedure | Description |
|-----------|-------------|
| `(apply proc arg ... args)` | Apply procedure to args |
| `(call-with-current-continuation proc)` | Capture continuation |
| `(call/cc proc)` | Alias for the above |
| `(values v ...)` | Return multiple values |
| `(call-with-values producer consumer)` | Receive multiple values |

### Delimited Continuations

| Procedure | Description |
|-----------|-------------|
| `(make-continuation-prompt-tag)` | Create prompt tag |
| `(make-continuation-prompt-tag name)` | Create named prompt tag |
| `(default-continuation-prompt-tag)` | Get default tag |
| `(continuation-prompt-tag? x)` | Is a prompt tag |
| `(call-with-continuation-prompt thunk tag handler)` | Install prompt with abort handler (all three required) |
| `(abort-current-continuation tag v ...)` | Abort to prompt |
| `(call-with-composable-continuation proc tag)` | Capture composable continuation |
| `(continuation-prompt-available? tag)` | Is a prompt with this tag on the current continuation |

### Continuation Marks

| Procedure | Description |
|-----------|-------------|
| `(current-continuation-marks)` | Snapshot marks on current continuation |
| `(current-continuation-marks tag)` | Snapshot up to prompt with tag |
| `(continuation-marks cont)` | Extract marks from captured continuation |
| `(continuation-mark-set->list marks key)` | Values for key across all frames |
| `(continuation-mark-set->list* marks keys)` | Multi-key variant, returns list of vectors |
| `(continuation-mark-set->list* marks keys none-v)` | With custom none-v for missing keys |
| `(continuation-mark-set-first marks key)` | Nearest value for key, or `#f` |
| `(continuation-mark-set-first marks key default)` | With custom default |
| `(call-with-immediate-continuation-mark key proc)` | Call proc with mark from current frame |
| `(continuation-mark-set? x)` | Is a continuation mark set |
| `(continuation? x)` | Is a captured continuation |

### Escape Continuations

| Procedure | Description |
|-----------|-------------|
| `(call-with-exit proc)` | One-shot escape continuation |
| `(call-with-continuation-barrier thunk)` | Prevent continuation re-entry |

### Exception Handling

| Procedure | Description |
|-----------|-------------|
| `(with-exception-handler handler thunk)` | Install handler |
| `(raise obj)` | Raise exception (non-continuable) |
| `(raise-continuable obj)` | Raise continuable exception |
| `(error message irritant ...)` | Signal error |
| `(error-object? x)` | Is error object |
| `(error-object-message e)` | Error message string |
| `(error-object-irritants e)` | Error irritant list |
| `(read-error? x)` | Is read error |
| `(file-error? x)` | Is file error |

### Promises

| Procedure | Description |
|-----------|-------------|
| `(promise? x)` | Is a promise |
| `(make-promise v)` | Already-forced promise |
| `(force p)` | Force evaluation |

### Parameters

| Procedure | Description |
|-----------|-------------|
| `(make-parameter init)` | Create parameter with initial value |
| `(make-parameter init converter)` | With conversion function |
| `(parameter? x)` | Is a parameter |
| `(<parameter>)` | Read current value |
| `(<parameter> new-value)` | Set value (outside parameterize) |

### Input/Output

**Current ports**:

| Procedure | Description |
|-----------|-------------|
| `(current-input-port)` | Current text input |
| `(current-output-port)` | Current text output |
| `(current-error-port)` | Current error output |

**Port predicates**:

| Procedure | Description |
|-----------|-------------|
| `(port? x)` | Is a port |
| `(input-port? x)` | Is input port |
| `(output-port? x)` | Is output port |
| `(textual-port? x)` | Is textual port |
| `(binary-port? x)` | Is binary port |
| `(input-port-open? p)` | Is open for input |
| `(output-port-open? p)` | Is open for output |
| `(eof-object)` | The EOF object |
| `(eof-object? x)` | Is EOF |

**Port operations**:

| Procedure | Description |
|-----------|-------------|
| `(close-port p)` | Close port |
| `(close-input-port p)` | Close input |
| `(close-output-port p)` | Close output |
| `(call-with-port p proc)` | Call proc, close on return |

**Text input**:

| Procedure | Description |
|-----------|-------------|
| `(read)` | Read datum from current input |
| `(read port)` | Read datum from port |
| `(read-char)` | Read character |
| `(read-char port)` | |
| `(peek-char)` | Peek at next character |
| `(peek-char port)` | |
| `(read-line)` | Read line as string |
| `(read-line port)` | |
| `(read-string k)` | Read k characters as string |
| `(read-string k port)` | |
| `(char-ready?)` | Character ready? (always `#t` in Wile) |
| `(char-ready? port)` | |

**Text output**:

| Procedure | Description |
|-----------|-------------|
| `(write obj)` | Machine-readable output |
| `(write obj port)` | |
| `(display obj)` | Human-readable output |
| `(display obj port)` | |
| `(write-simple obj)` | Write without shared structure |
| `(write-simple obj port)` | |
| `(write-shared obj)` | Write showing shared structure |
| `(write-shared obj port)` | |
| `(write-char c)` | Write character |
| `(write-char c port)` | |
| `(write-string s)` | Write string |
| `(write-string s port)` | |
| `(write-string s port start)` | |
| `(write-string s port start end)` | |
| `(newline)` | Write newline |
| `(newline port)` | |
| `(flush-output-port)` | Flush current output |
| `(flush-output-port port)` | |

**Binary I/O**:

| Procedure | Description |
|-----------|-------------|
| `(read-u8)` | Read byte |
| `(read-u8 port)` | |
| `(peek-u8)` | Peek at next byte |
| `(peek-u8 port)` | |
| `(u8-ready?)` | Byte ready? (always `#t`) |
| `(u8-ready? port)` | |
| `(write-u8 byte)` | Write byte |
| `(write-u8 byte port)` | |
| `(read-bytevector k)` | Read k bytes |
| `(read-bytevector k port)` | |
| `(read-bytevector! bv)` | Read into bytevector |
| `(read-bytevector! bv port)` | |
| `(read-bytevector! bv port start)` | |
| `(read-bytevector! bv port start end)` | |
| `(write-bytevector bv)` | Write bytevector |
| `(write-bytevector bv port)` | |
| `(write-bytevector bv port start)` | |
| `(write-bytevector bv port start end)` | |

**String ports**:

| Procedure | Description |
|-----------|-------------|
| `(open-input-string s)` | Input port from string |
| `(open-output-string)` | Output string port |
| `(get-output-string port)` | Get accumulated string |

**Bytevector ports**:

| Procedure | Description |
|-----------|-------------|
| `(open-input-bytevector bv)` | Input port from bytevector |
| `(open-output-bytevector)` | Output bytevector port |
| `(get-output-bytevector port)` | Get accumulated bytevector |

**File I/O** (requires files extension):

| Procedure | Description |
|-----------|-------------|
| `(open-input-file path)` | Open file for reading |
| `(open-output-file path)` | Open file for writing |
| `(open-binary-input-file path)` | Open binary for reading |
| `(open-binary-output-file path)` | Open binary for writing |
| `(call-with-input-file path proc)` | Open, call, close |
| `(call-with-output-file path proc)` | |
| `(file-exists? path)` | File exists |
| `(delete-file path)` | Delete file |
| `(create-directory path)` | Create directory |
| `(delete-directory path)` | Remove directory |
| `(directory-files path)` | List directory entries |
| `(current-directory)` | Process working directory |
| `(set-current-directory! path)` | Change process working directory |

### Evaluation and Loading

Requires the eval extension:

| Procedure | Description |
|-----------|-------------|
| `(eval expr env)` | Evaluate expression in environment |
| `(environment spec ...)` | Create environment from import specs |
| `(scheme-report-environment version)` | R5RS environment |
| `(null-environment version)` | Minimal environment |
| `(load path)` | Load and execute file |

`interaction-environment` lives in the introspection extension, alongside the [Introspection](#introspection) procedures.

### Expansion and Compilation

| Procedure | Description |
|-----------|-------------|
| `(expand expr)` | Fully expand expression |
| `(expand-once expr)` | Expand one macro level |
| `(compile expr)` | Compile expression |
| `(read-syntax)` | Read datum as syntax object |
| `(read-syntax port)` | |
| `(read-token)` | Read single token |
| `(read-token port)` | |

### Syntax Object Operations

| Procedure | Description |
|-----------|-------------|
| `(identifier? x)` | Is identifier syntax object |
| `(syntax->datum stx)` | Strip syntax to raw datum |
| `(datum->syntax ctx datum)` | Wrap datum with context's scopes |
| `(syntax->list stx)` | Syntax pair chain to list of syntax objects, or `#f` |
| `(generate-temporaries lst)` | Generate unique identifiers |
| `(bound-identifier=? a b)` | Same binding identity |
| `(free-identifier=? a b)` | Same free reference |
| `(syntax-source stx)` | Source file path, or `#f` |
| `(syntax-line stx)` | 1-based line number, or `#f` |
| `(syntax-column stx)` | 0-based column, or `#f` |
| `(syntax-position stx)` | 0-based byte position, or `#f` |
| `(syntax-span stx)` | Byte span (end − start), or `#f` |
| `(syntax-local-value id)` | Get compile-time value |
| `(syntax-local-value/immediate id)` | Like above, no rename-transformer chasing |
| `(make-compile-time-value v)` | Create compile-time value |
| `(syntax-local-introduce stx)` | Introduce syntax marks |
| `(syntax-local-identifier-as-binding id)` | Convert to binding form |

### Introspection

| Procedure | Description |
|-----------|-------------|
| `(environment? x)` | Is environment object |
| `(environment-bound-names env)` | List all bound names |
| `(environment-ref env sym)` | Lookup binding by symbol |
| `(environment-bound? env sym)` | Is symbol bound |
| `(interaction-environment)` | Interactive environment |
| `(available-libraries)` | Library names the resolver chain can load |
| `(disassemble proc)` | Bytecode listing for a procedure, as an alist per instruction |

### Reflection

| Procedure | Description |
|-----------|-------------|
| `(procedure-arity proc)` | Arity information |
| `(procedure-name proc)` | Name (or `#f`) |
| `(procedure-source-location proc)` | Source location (or `#f`) |
| `(procedure-bound-symbols proc)` | Closed-over symbols |
| `(procedure-type proc)` | Type tag symbol: `closure` (Scheme lambda), `foreign` (Go primitive), `case-lambda` (case-lambda closure), `parameter` (parameter object), `continuation` (captured or composable continuation), or `unknown` (any other callable) |

### Records (Procedural API)

| Procedure | Description |
|-----------|-------------|
| `(make-record-type name fields)` | Create record type |
| `(record-type? x)` | Is record type descriptor |
| `(record? x)` | Is record instance |
| `(record-type r)` | Get record's type |
| `(record-constructor rtd)` | Get constructor |
| `(record-predicate rtd)` | Get predicate |
| `(record-accessor rtd field)` | Get field accessor |
| `(record-modifier rtd field)` | Get field modifier |

### Process Context

Requires the system extension, except the two environment-variable procedures, which come from the envvars extension:

| Procedure | Description |
|-----------|-------------|
| `(command-line)` | Command-line arguments as list |
| `(exit)` | Exit with status 0 |
| `(exit status)` | Exit with status |
| `(emergency-exit)` | Exit immediately (no cleanup) |
| `(emergency-exit status)` | |
| `(get-environment-variable name)` | Env var value (or `#f`) |
| `(get-environment-variables)` | All env vars as alist |

### Time

Requires system extension:

| Procedure | Description |
|-----------|-------------|
| `(current-second)` | Seconds since epoch (inexact) |
| `(current-jiffy)` | Current time in jiffies |
| `(jiffies-per-second)` | Jiffies per second |

### Feature Detection

| Procedure | Description |
|-----------|-------------|
| `(features)` | List of supported feature identifiers |

### Load Path

| Procedure | Description |
|-----------|-------------|
| `(current-load-path)` | Absolute path of file being loaded, or `#f` |
| `(current-load-directory)` | Directory of file being loaded, or `#f` |
| `(current-load-depth)` | Nesting depth of load stack (0 in REPL) |

---

## Concurrency

### SRFI-18 Threads

Requires threads extension. Threads map to Go goroutines.

**Thread operations**:

| Procedure | Description |
|-----------|-------------|
| `(current-thread)` | Current thread object |
| `(thread? x)` | Is a thread |
| `(make-thread thunk)` | Create thread |
| `(make-thread thunk name)` | Create named thread |
| `(thread-name t)` | Thread name |
| `(thread-specific t)` | Thread-specific data |
| `(thread-specific-set! t v)` | Set thread-specific data |
| `(thread-start! t)` | Start thread |
| `(thread-yield!)` | Yield to scheduler |
| `(thread-sleep! timeout)` | Sleep for duration |
| `(thread-terminate! t)` | Terminate thread |
| `(thread-join! t)` | Wait for completion |
| `(thread-join! t timeout)` | Wait with timeout |
| `(thread-join! t timeout timeout-val)` | Wait; return `timeout-val` instead of raising on timeout |
| `(thread-state t)` | State symbol: `new`, `runnable`, `blocked`, `terminated` (**not SRFI-18**; follows Gambit) |

**Thread exceptions**:

`thread-join!` and `mutex-lock!` raise these into the *calling* thread's handler
chain, so a `guard` around the call discriminates them:

| Procedure | Description |
|-----------|-------------|
| `(join-timeout-exception? obj)` | `thread-join!` hit its timeout and no `timeout-val` was given |
| `(terminated-thread-exception? obj)` | The joined thread died via `thread-terminate!` |
| `(abandoned-mutex-exception? obj)` | `mutex-lock!` acquired a mutex whose owner terminated holding it — the lock **is** held when this is raised |
| `(uncaught-exception? obj)` | The joined thread died via an uncaught exception |
| `(uncaught-exception-reason exc)` | The condition that thread originally raised |

```scheme
(guard (e ((join-timeout-exception? e)      'timed-out)
          ((terminated-thread-exception? e) 'killed)
          ((uncaught-exception? e)          (uncaught-exception-reason e)))
  (thread-join! t 1.0))
```

**Mutexes**:

| Procedure | Description |
|-----------|-------------|
| `(make-mutex)` | Create mutex |
| `(make-mutex name)` | Create named mutex |
| `(mutex? x)` | Is a mutex |
| `(mutex-name m)` | Mutex name |
| `(mutex-specific m)` | Mutex-specific data |
| `(mutex-specific-set! m v)` | Set mutex-specific data |
| `(mutex-state m)` | Mutex state |
| `(mutex-lock! m)` | Lock |
| `(mutex-lock! m timeout)` | Lock with timeout |
| `(mutex-unlock! m)` | Unlock |
| `(mutex-unlock! m cv)` | Unlock and wait on condition |
| `(mutex-unlock! m cv timeout)` | Unlock and wait with timeout |

**Condition variables**:

| Procedure | Description |
|-----------|-------------|
| `(make-condition-variable)` | Create condition variable |
| `(make-condition-variable name)` | Create named condition variable |
| `(condition-variable? x)` | Is a condition variable |
| `(condition-variable-name cv)` | Name |
| `(condition-variable-specific cv)` | Specific data |
| `(condition-variable-specific-set! cv v)` | Set specific data |
| `(condition-variable-signal! cv)` | Signal one waiter |
| `(condition-variable-broadcast! cv)` | Signal all waiters |

**Time objects**:

| Procedure | Description |
|-----------|-------------|
| `(current-time)` | Current time object |
| `(time? x)` | Is time object |
| `(time->seconds t)` | Convert to seconds |
| `(seconds->time s)` | Convert from seconds |

### Go Concurrency Primitives

Requires gointerop extension.

**Atomic** — thread-safe mutable value:

| Procedure | Description |
|-----------|-------------|
| `(make-atomic v)` | Create atomic with initial value |
| `(atomic? x)` | Is atomic |
| `(atomic-load a)` | Read value |
| `(atomic-store! a v)` | Write value |
| `(atomic-swap! a v)` | Swap, return old |
| `(atomic-compare-and-swap! a expected new)` | CAS |

---

## Libraries

### R7RS Standard Libraries

These ship as `.sld` files embedded from `pkg/stdlib/lib/` and served through the engine's `FileResolver` chain. Require `WithLibraryPaths()` on the engine.

| Library | Contents |
|---------|----------|
| `(scheme base)` | Core language — special forms, derived syntax, basic procedures |
| `(scheme case-lambda)` | `case-lambda` |
| `(scheme char)` | Character predicates and case operations |
| `(scheme complex)` | Complex number operations |
| `(scheme cxr)` | `caar` through `cddddr` |
| `(scheme eval)` | `eval`, `environment` |
| `(scheme file)` | File I/O |
| `(scheme inexact)` | Transcendental functions, `finite?`, `infinite?`, `nan?` |
| `(scheme lazy)` | `delay`, `force`, `delay-force`, `make-promise`, `promise?` |
| `(scheme load)` | `load` |
| `(scheme process-context)` | `command-line`, `exit`, env vars |
| `(scheme r5rs)` | R5RS compatibility |
| `(scheme read)` | `read` |
| `(scheme repl)` | `interaction-environment` |
| `(scheme time)` | `current-second`, `current-jiffy`, `jiffies-per-second` |
| `(scheme write)` | `write`, `display`, `write-shared`, `write-simple` |

### Wile Extension Libraries

Extension primitives are also importable as R7RS libraries when `WithLibraryPaths()` is enabled:

| Library | Contents |
|---------|----------|
| `(wile io)` | Ports, `read`/`write`/`display`, string and bytevector ports |
| `(wile math)` | Transcendental functions, complex number ops, rounding, conversion |
| `(wile system)` | Process context, time |
| `(wile envvars)` | `get-environment-variable`, `get-environment-variables` |
| `(wile files)` | File and directory I/O |
| `(wile process)` | Process execution, subprocess management |
| `(wile threads)` | SRFI-18 threading |
| `(wile gointerop)` | Go concurrency: Atomic |
| `(wile introspection)` | Environment introspection, `features`, `available-libraries`, `disassemble` |
| `(wile eval)` | `eval`, `environment`, `load`, `expand`, `compile`, load-path accessors |
| `(wile namespace)` | Namespace introspection and management |
| `(wile all)` | Records, promises, extra string and character operations |
| `(wile sat)` | CDCL SAT solver |
| `(wile charsets)` | SRFI-14 character sets, plus `char-set-ranges` |
| `(wile algebragraph)` | Graph analytics backing `(wile algebra ...)` |

Import modifiers — `only`, `except`, `prefix`, `rename` — work on all libraries:

```scheme
(import (only (wile math) sqrt sin cos))
(import (prefix (wile system) sys:))
(import (rename (wile math) (sqrt square-root)))
```

Phase-shifting modifiers place a library's bindings at a phase other than 0:

```scheme
(import (for-syntax (scheme base)))    ; phase 1: visible to macro transformers
(import (for-template (scheme base)))  ; phase -1
(import (for-meta 2 (scheme base)))    ; phase 2: visible to a nested begin-for-syntax
```

Shifts compose additively and relative to the importing form's own phase, so
`(for-syntax (for-syntax lib))` equals `(for-meta 2 lib)`, and `(for-meta 0 lib)`
equals a plain import. A shift outside the `int8` phase range is an error
(`for-meta: phase 200 out of range [-128, 127]`). Nothing in Wile evaluates at
phase -1, so `for-template` bindings are installed but never consulted.

### Wile Scheme Libraries

| Library | Contents |
|---------|----------|
| `(wile control)` | Delimited continuation operators — `shift`/`reset`, `prompt`/`control`, `shift0`/`reset0`, `prompt0`/`control0`, `spawn`, `set`/`cupto`, all with `-at` tagged variants; `call/ec`, `new-prompt` aliases; `continuation-mark-set->iterator`, `continuation-mark-set->context` |
| `(wile strings)` | SRFI-13 surface plus Wile extras (`string-split`, `string-replace-all`), as one import |
| `(wile kanren)` | miniKanren — `run`, `run*`, `fresh`, `conde` |
| `(wile microkanren)` | microKanren core — `==`, `call/fresh`, `disj`, `conj` |
| `(wile algebra)` | Algebraic structures umbrella over 30 sub-libraries (`setoid`, `order`, `lattice`, `closure`, `heyting`, `boolean`, `monoid`, `group`, `semiring`, `ring`, `differential`, `category`, `galois`, `rewrite`, `symbolic`, `polynomial`, `matrix`, `incidence`, `interval`, `graph`, `combinatorial-graph`, `fca`, `pareto`, `abstract-domain`, `dataflow`, `unification`, `matching`, `sat`, `cfl`, `tree`). See [`docs/algebra/overview.md`](../algebra/overview.md). |

### Third-Party Libraries

| Library | Contents |
|---------|----------|
| `(chibi test)` | Test framework — `test-begin`, `test-end`, `test`, `test-assert`, `test-error`, `test-group` |
| `(chibi optional)` | Optional argument handling |
| `(chibi diff)` | Diff algorithm |
| `(chibi term ansi)` | ANSI terminal colors |
| `(srfi 1)` | List library — `xcons`, `cons*`, `iota`, `zip`, `filter`, `partition`, `fold`, `unfold`, `any`, `every`, etc. Exports the whole SRFI-1 procedure index, including the R5RS list procedures and the `c…r` compositions it re-exports; until 2026-08-09 the export list elided 48 of those names, so `(import (only (srfi 1) caar))` failed |
| `(srfi 13)` | String library — `string-prefix?`, `string-contains`, `string-join`, `string-trim`, `string-tokenize`, etc. A **subset** of SRFI-13: the R5RS string procedures stay at their R7RS homes, and the `/shared`, `string-unfold` and KMP-helper names are not supplied |
| `(srfi 14)` | Character sets |
| `(srfi 132)` | Sort libraries — list and vector sort, merge, select, median |

---

## Feature Flags

The `features` procedure and `cond-expand` recognize these identifiers:

### Language

| Feature | Description |
|---------|-------------|
| `r7rs` | R7RS compliance |
| `wile` | Wile implementation |

### Numeric

| Feature | Description |
|---------|-------------|
| `exact-closed` | Exact arithmetic closed under +, -, * |
| `ratios` | Rational numbers supported |
| `ieee-float` | IEEE 754 floating point |

### Unicode

| Feature | Description |
|---------|-------------|
| `full-unicode` | Full Unicode for characters and strings |

### Platform (detected at runtime)

| Feature | Condition |
|---------|-----------|
| `darwin`, `macosx` | macOS |
| `linux` | Linux |
| `windows` | Windows |
| `freebsd`, `openbsd`, `netbsd`, `bsd` | BSD variants |
| `posix`, `unix` | POSIX/Unix-like |
| `little-endian` | Little-endian byte order |
| `x86-family` | x86/x86-64 architecture |

### Library availability

```scheme
(cond-expand
  ((library (srfi 1))
   (import (srfi 1))
   (display "SRFI-1 available"))
  (else
   (display "no SRFI-1")))
```

---

## Extensions Beyond R7RS

### Arbitrary-Precision Number Literals

| Prefix | Type | Exactness | Precision |
|--------|------|-----------|-----------|
| `#z` | BigInteger | exact | Arbitrary |
| `#m` | BigFloat | inexact | 256-bit |

```scheme
#z12345678901234567890
#m3.14159265358979323846264338327950288
```

`#z` reads decimal digits only; a radix prefix after it (`#z#x...`) is a parse error.

Standard R7RS programs never need these — integer overflow promotes automatically, and `#e`/`#i` prefixes handle explicit conversion. These are convenience syntax for direct construction.

### `guard` Body Multiple Values

Wile's `guard` correctly propagates multiple values from the body:

```scheme
(guard (e (#f))
  (values 1 2 3))   ; → 1 2 3
```

The R7RS reference implementation would signal an error here.

### Delimited Continuations

Racket-style `call-with-continuation-prompt`, `abort-current-continuation`, and `call-with-composable-continuation` following Flatt, Yu, Findler, Felleisen (ICFP 2007). Not part of R7RS-small.

### Go Concurrency Primitives

Atomic values — backed by Go's `sync/atomic`. Available via `(wile gointerop)`.

### Phase Control

`define-for-syntax`, `begin-for-syntax`, and `eval-when` for controlling evaluation timing during macro expansion. `syntax-case` (R6RS) and `quasisyntax`/`unsyntax`/`unsyntax-splicing` for procedural macros.

### Reflection Primitives

`procedure-arity`, `procedure-name`, `procedure-source-location`, `procedure-bound-symbols`, `procedure-type` for runtime introspection of procedures.

### Boxes

`box`, `unbox`, `set-box!`, `box?` — mutable single-value containers. A box both writes and reads as `#&<datum>`, so `write` output round-trips. `#&` is a container introducer: it nests (`#&#&5` is a box holding a box) and its datum may carry its own prefixes (`#&#x1f` is a box holding 31). See [`r7rs-differences.md`](r7rs-differences.md#boxes) for the reader rules.

### Hashtables

`make-eq-hashtable`, `make-eqv-hashtable`, `make-equal-hashtable`,
`hashtable-ref`, `hashtable-set!`, etc. — the R6RS `(rnrs hashtables)` surface.
Any object can be a key; the constructor chooses the key equivalence.

---

## Known Semantic Differences

### `char-ready?` / `u8-ready?` — Always Returns `#t`

R7RS requires these to return `#f` when reading would block. Wile always returns `#t` because Go's `io.Reader` does not expose readiness status.

**Impact**: Low. These predicates are designed for select-style event loops, a pattern superseded by async-style concurrency. For non-blocking I/O patterns in Wile, read on a thread and publish the result through an atomic box:

```scheme
(let ((slot (make-atomic #f)))
  (thread-start!
    (make-thread (lambda () (atomic-store! slot (read-char port)))))
  (atomic-load slot))  ; poll, or join the thread when the value is needed
```

This is one of several documented differences; the full catalogue, including immutable pair/vector literals, immutable top-level definitions, marks-based `parameterize`, signalled import conflicts, and continuation value-count splicing, is in [`r7rs-differences.md`](r7rs-differences.md).

---

## Running Wile

### Command Line

```bash
make build                                               # Build to ./dist/<os>/<arch>/wile
./dist/<os>/<arch>/wile                                  # Start REPL
./dist/<os>/<arch>/wile --file program.scm               # Run file
./dist/<os>/<arch>/wile --file program.scm --interactive # Run file then REPL
./dist/<os>/<arch>/wile -e '(+ 1 2)'                     # Evaluate expression
```

### REPL

The REPL supports readline-style editing. All core primitives and loaded extensions are available. Type expressions directly:

```
> (define (fib n)
    (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
> (fib 10)
55
> (map fib '(0 1 2 3 4 5 6 7 8 9 10))
(0 1 1 2 3 5 8 13 21 34 55)
```
