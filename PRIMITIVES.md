# Wile Primitives Reference

Complete list of supported types, primitives, and special forms in Wile.

## Types

### Core Types

| Type | Description |
|------|-------------|
| Boolean | Truth values `#t` and `#f` |
| Character | Unicode character, e.g., `#\a`, `#\newline` |
| Symbol | Identifier name (compared by string key) |
| Pair | Cons cell with car and cdr, basis for lists |
| Null | Empty list `()` |
| Vector | Fixed-size mutable array, e.g., `#(1 2 3)` |
| String | Mutable UTF-8 text |
| Bytevector | Fixed-size byte array, e.g., `#u8(0 1 2)` |
| Box | Mutable single-value container, e.g., `#&42` |
| Hashtable | Hash table mapping hashable values to values |
| Procedure | Lambda or primitive function |

### Numeric Types

| Type | Description |
|------|-------------|
| Integer | Exact 64-bit signed integer |
| Big-Integer | Exact arbitrary-precision integer, e.g., `#z12345678901234567890` |
| Float | Inexact IEEE 754 double-precision number |
| Big-Real | Inexact arbitrary-precision floating-point number, e.g., `#m3.14159265358979323846` |
| Rational | Exact fraction with arbitrary precision |
| Complex | Inexact complex number with real and imaginary float64 parts |
| Big-Complex | Complex number with arbitrary-precision parts (exact or inexact) |

### I/O Port Types

| Type | Description |
|------|-------------|
| Character Input Port | Text input stream for reading characters |
| Character Output Port | Text output stream for writing characters |
| Binary Input Port | Byte input stream for reading bytes |
| Binary Output Port | Byte output stream for writing bytes |
| String Port | Port backed by a string buffer |
| Bytevector Port | Port backed by a bytevector buffer |

### Record Types

| Type | Description |
|------|-------------|
| Record Type | Descriptor defining a record's structure |
| Record | Instance of a user-defined record type |

### Control Flow Types

| Type | Description |
|------|-------------|
| Promise | Delayed computation for lazy evaluation |
| Continuation | Captured execution context |
| Continuation Prompt Tag | Delimiter for delimited continuations |

### Error Types

| Type | Description |
|------|-------------|
| Error Object | Exception with message and irritants |
| EOF Object | End-of-file marker `#!eof` |

### Threading Types (SRFI-18)

| Type | Description |
|------|-------------|
| Thread | Concurrent thread of execution |
| Mutex | Mutual exclusion lock |
| Condition Variable | Thread synchronization primitive |
| Time | Point in time for timeouts |

### Go Concurrency Types

| Type | Description |
|------|-------------|
| Atomic | Thread-safe mutable value |

### Meta Types

| Type | Description |
|------|-------------|
| Syntax Object | Datum with lexical context information |
| Environment | First-class evaluation environment |
| Compile-Time Value | Value available during macro expansion |
| Void | Absence of a meaningful value `#!void` |

## Special Forms (Compile-Time)

| Form | Description |
|------|-------------|
| `if` | Conditional expression |
| `lambda` | Create a procedure |
| `case-lambda` | Create a procedure with multiple arities |
| `quote` | Return datum without evaluation |
| `define` | Define a variable or procedure |
| `define-syntax` | Define a syntax transformer |
| `syntax-rules` | Create pattern-based transformer (R7RS) |
| `let-syntax` | Local syntax definitions |
| `letrec-syntax` | Local recursive syntax definitions |
| `set!` | Mutate a variable binding |
| `begin` | Sequence expressions |
| `include` | Include source file contents |
| `include-ci` | Include source file with case-insensitive symbols |
| `quasiquote` | Template with unquote escapes |
| `unquote` | Escape from quasiquote |
| `unquote-splicing` | Splice escape from quasiquote |
| `cond-expand` | Conditional expansion based on features |
| `define-for-syntax` | Define binding for macro expansion phase |
| `begin-for-syntax` | Sequence expressions at macro expansion phase |
| `eval-when` | Control evaluation timing |
| `dynamic-wind` | Establish before/after thunks |
| `define-library` | Define an R7RS library |
| `library` | R6RS alias for `define-library` |
| `import` | Import library exports into current environment |
| `export` | Declare library exports |
| `er-macro-transformer` | Explicit-renaming macro transformer |
| `with-continuation-mark` | Attach key-value mark to current continuation frame |
| `syntax-error` | Signal compile-time error |
| `syntax-case` | Pattern matching with fenders and arbitrary body (R6RS) |
| `syntax` | Construct syntax object from template |
| `with-syntax` | Bind pattern variables and expand body |
| `quasisyntax` | Template with unsyntax escapes |
| `unsyntax` | Escape from quasisyntax |
| `unsyntax-splicing` | Splice escape from quasisyntax |

### Auxiliary Syntax

| Form | Description |
|------|-------------|
| `else` | Auxiliary keyword for `cond` and `case` |
| `=>` | Auxiliary keyword for `cond` and `case` |
| `...` | Ellipsis for `syntax-rules` patterns |
| `_` | Wildcard for `syntax-rules` patterns |

## Derived Forms (Macros)

| Form | Description |
|------|-------------|
| `and` | Short-circuit logical and |
| `or` | Short-circuit logical or |
| `let` | Local bindings (parallel), including named let |
| `let*` | Local bindings (sequential) |
| `letrec` | Recursive local bindings |
| `letrec*` | Recursive local bindings (sequential) |
| `cond` | Multi-way conditional |
| `case` | Dispatch on value |
| `when` | One-armed conditional with implicit begin |
| `unless` | Negated one-armed conditional |
| `delay` | Create a promise (lazy evaluation) |
| `delay-force` | Create a promise that forces its result |
| `parameterize` | Dynamic binding for parameters |
| `guard` | Exception handling with condition clauses |
| `define-record-type` | Define a record type with constructor and accessors |
| `let-values` | Bind multiple values |
| `let*-values` | Bind multiple values sequentially |
| `define-values` | Define multiple values |
| `do` | Iteration construct |
| `map` | Apply procedure to each element of list(s) |
| `for-each` | Apply procedure to each element for side effects |
| `with-continuation-barrier` | Execute body with continuation re-entry barrier |

## Arithmetic

| Primitive | Description |
|-----------|-------------|
| `+` | Add numbers |
| `-` | Subtract numbers |
| `*` | Multiply numbers |
| `/` | Divide numbers |

## Numeric Comparisons

| Primitive | Description |
|-----------|-------------|
| `=` | Numeric equality |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |

## Type Predicates

| Primitive | Description |
|-----------|-------------|
| `void?` | Test for void value |
| `null?` | Test for empty list |
| `pair?` | Test for pair |
| `number?` | Test for number |
| `boolean?` | Test for boolean |
| `string?` | Test for string |
| `symbol?` | Test for symbol |
| `procedure?` | Test for procedure |
| `vector?` | Test for vector |
| `bytevector?` | Test for bytevector |
| `char?` | Test for character |
| `list?` | Test for proper list |

## Numeric Type Predicates

| Primitive | Description |
|-----------|-------------|
| `integer?` | Test for integer |
| `real?` | Test for real number |
| `rational?` | Test for rational number |
| `complex?` | Test for complex number |
| `exact?` | Test for exact number |
| `inexact?` | Test for inexact number |
| `exact-integer?` | Test for exact integer |
| `zero?` | Test if number is zero |
| `positive?` | Test if number is positive |
| `negative?` | Test if number is negative |
| `odd?` | Test if integer is odd |
| `even?` | Test if integer is even |

## Boolean Operations

| Primitive | Description |
|-----------|-------------|
| `not` | Logical negation |
| `boolean=?` | Boolean equality (variadic) |

## Equality Predicates

| Primitive | Description |
|-----------|-------------|
| `eq?` | Identity comparison |
| `eqv?` | Equivalence comparison |
| `equal?` | Recursive structural equality |
| `symbol=?` | Symbol equality (variadic) |

## Identifier Comparison

| Primitive | Description |
|-----------|-------------|
| `bound-identifier=?` | Compare identifiers by binding |
| `free-identifier=?` | Compare identifiers by free reference |

## Syntax Objects

| Primitive | Description |
|-----------|-------------|
| `identifier?` | Test for identifier syntax object |
| `syntax->datum` | Convert syntax object to datum |
| `datum->syntax` | Convert datum to syntax object |
| `syntax->list` | Convert syntax pair chain to list of syntax objects, or `#f` |
| `generate-temporaries` | Generate unique temporary identifiers |
| `syntax-source` | Source file of syntax object, or `#f` |
| `syntax-line` | 1-based line number, or `#f` |
| `syntax-column` | 0-based column, or `#f` |
| `syntax-position` | 0-based byte position, or `#f` |
| `syntax-span` | Byte span (end − start), or `#f` |

## Pairs and Lists

| Primitive | Description |
|-----------|-------------|
| `car` | First element of pair |
| `cdr` | Rest of pair |
| `set-car!` | Mutate car of pair |
| `set-cdr!` | Mutate cdr of pair |
| `cons` | Construct a pair |
| `list` | Construct a list |
| `make-list` | Create list of given length |
| `append` | Concatenate lists |
| `length` | Length of list |
| `reverse` | Reverse a list |
| `list-ref` | Get element by index |
| `list-set!` | Set element by index |
| `list-tail` | Get tail starting at index |
| `list-copy` | Shallow copy a list |
| `memq` | Find element using eq? |
| `memv` | Find element using eqv? |
| `member` | Find element using equal? or custom comparator |
| `assq` | Association list lookup using eq? |
| `assv` | Association list lookup using eqv? |
| `assoc` | Association list lookup using equal? or custom comparator |

## CxR Accessors (2-level)

| Primitive | Description |
|-----------|-------------|
| `caar` | (car (car x)) |
| `cadr` | (car (cdr x)) |
| `cdar` | (cdr (car x)) |
| `cddr` | (cdr (cdr x)) |

## CxR Accessors (3-level)

| Primitive | Description |
|-----------|-------------|
| `caaar` | (car (car (car x))) |
| `caadr` | (car (car (cdr x))) |
| `cadar` | (car (cdr (car x))) |
| `caddr` | (car (cdr (cdr x))) |
| `cdaar` | (cdr (car (car x))) |
| `cdadr` | (cdr (car (cdr x))) |
| `cddar` | (cdr (cdr (car x))) |
| `cdddr` | (cdr (cdr (cdr x))) |

## CxR Accessors (4-level)

| Primitive | Description |
|-----------|-------------|
| `caaaar` | (car (car (car (car x)))) |
| `caaadr` | (car (car (car (cdr x)))) |
| `caadar` | (car (car (cdr (car x)))) |
| `caaddr` | (car (car (cdr (cdr x)))) |
| `cadaar` | (car (cdr (car (car x)))) |
| `cadadr` | (car (cdr (car (cdr x)))) |
| `caddar` | (car (cdr (cdr (car x)))) |
| `cadddr` | (car (cdr (cdr (cdr x)))) |
| `cdaaar` | (cdr (car (car (car x)))) |
| `cdaadr` | (cdr (car (car (cdr x)))) |
| `cdadar` | (cdr (car (cdr (car x)))) |
| `cdaddr` | (cdr (car (cdr (cdr x)))) |
| `cddaar` | (cdr (cdr (car (car x)))) |
| `cddadr` | (cdr (cdr (car (cdr x)))) |
| `cdddar` | (cdr (cdr (cdr (car x)))) |
| `cddddr` | (cdr (cdr (cdr (cdr x)))) |

## Numeric Operations

| Primitive | Description |
|-----------|-------------|
| `abs` | Absolute value |
| `max` | Maximum of numbers |
| `min` | Minimum of numbers |
| `floor` | Round toward negative infinity |
| `ceiling` | Round toward positive infinity |
| `truncate` | Round toward zero |
| `round` | Round to nearest integer |
| `quotient` | Integer division |
| `remainder` | Integer division remainder |
| `modulo` | Modular arithmetic |
| `floor/` | Floor division returning two values |
| `floor-quotient` | Quotient from floor division |
| `floor-remainder` | Remainder from floor division |
| `truncate/` | Truncate division returning two values |
| `truncate-quotient` | Quotient from truncate division |
| `truncate-remainder` | Remainder from truncate division |
| `gcd` | Greatest common divisor |
| `lcm` | Least common multiple |
| `expt` | Exponentiation |
| `square` | Square a number |
| `sqrt` | Square root |
| `exact-integer-sqrt` | Exact integer square root |
| `exact` | Convert to exact number |
| `inexact` | Convert to inexact number |
| `numerator` | Numerator of rational |
| `denominator` | Denominator of rational |
| `rationalize` | Find rational approximation |
| `exact->inexact` | Convert to inexact (R5RS alias for `inexact`) |
| `inexact->exact` | Convert to exact (R5RS alias for `exact`) |

## Transcendental Functions

| Primitive | Description |
|-----------|-------------|
| `exp` | Exponential function |
| `log` | Natural logarithm |
| `sin` | Sine |
| `cos` | Cosine |
| `tan` | Tangent |
| `asin` | Arcsine |
| `acos` | Arccosine |
| `atan` | Arctangent |
| `finite?` | Test for finite number |
| `infinite?` | Test for infinity |
| `nan?` | Test for NaN |

## Number/String Conversion

| Primitive | Description |
|-----------|-------------|
| `number->string` | Convert number to string |
| `string->number` | Parse number from string |

## Complex Numbers

| Primitive | Description |
|-----------|-------------|
| `make-rectangular` | Create complex from real and imaginary parts |
| `make-polar` | Create complex from magnitude and angle |
| `real-part` | Real part of complex |
| `imag-part` | Imaginary part of complex |
| `magnitude` | Magnitude of complex |
| `angle` | Angle of complex |

## String Operations

| Primitive | Description |
|-----------|-------------|
| `string` | Create string from characters |
| `make-string` | Create string of given length filled with character |
| `string-length` | Length of string |
| `string-ref` | Character at index |
| `string-set!` | Set character at index |
| `string-append` | Concatenate strings |
| `substring` | Extract substring |
| `string-copy` | Copy string (with optional start/end) |
| `string-copy!` | Copy characters into a string |
| `string-fill!` | Fill string range with character |
| `string=?` | String equality |
| `string<?` | String less than |
| `string>?` | String greater than |
| `string<=?` | String less than or equal |
| `string>=?` | String greater than or equal |
| `string-ci=?` | Case-insensitive string equality |
| `string-ci<?` | Case-insensitive string less than |
| `string-ci>?` | Case-insensitive string greater than |
| `string-ci<=?` | Case-insensitive string less than or equal |
| `string-ci>=?` | Case-insensitive string greater than or equal |
| `string-upcase` | Convert string to uppercase |
| `string-downcase` | Convert string to lowercase |
| `string-foldcase` | Unicode case fold for comparison |
| `string-map` | Map procedure over string characters |
| `string-for-each` | Apply procedure to string characters |
| `symbol->string` | Convert symbol to string |
| `string->symbol` | Convert string to symbol |
| `string->list` | Convert string to list of characters |
| `list->string` | Convert list of characters to string |

## Character Operations

| Primitive | Description |
|-----------|-------------|
| `char=?` | Character equality |
| `char<?` | Character less than |
| `char>?` | Character greater than |
| `char<=?` | Character less than or equal |
| `char>=?` | Character greater than or equal |
| `char-ci=?` | Case-insensitive character equality |
| `char-ci<?` | Case-insensitive character less than |
| `char-ci>?` | Case-insensitive character greater than |
| `char-ci<=?` | Case-insensitive character less than or equal |
| `char-ci>=?` | Case-insensitive character greater than or equal |
| `char->integer` | Convert character to integer code point |
| `integer->char` | Convert integer code point to character |
| `char-alphabetic?` | Test for alphabetic character |
| `char-numeric?` | Test for numeric character |
| `char-whitespace?` | Test for whitespace character |
| `char-upper-case?` | Test for uppercase character |
| `char-lower-case?` | Test for lowercase character |
| `char-upcase` | Convert character to uppercase |
| `char-downcase` | Convert character to lowercase |
| `char-foldcase` | Convert character for case-insensitive comparison |
| `digit-value` | Numeric value of digit character |

## Vector Operations

| Primitive | Description |
|-----------|-------------|
| `make-vector` | Create vector of given length |
| `vector` | Create vector from arguments |
| `vector-length` | Length of vector |
| `vector-ref` | Element at index |
| `vector-set!` | Set element at index |
| `vector->list` | Convert vector to list |
| `list->vector` | Convert list to vector |
| `vector-copy` | Copy vector (with optional start/end) |
| `vector-copy!` | Copy elements into a vector |
| `vector-fill!` | Fill vector range with value |
| `vector-append` | Concatenate vectors |
| `vector-map` | Map procedure over vectors |
| `vector-for-each` | Apply procedure to vector elements |
| `vector->string` | Convert vector of characters to string |
| `string->vector` | Convert string to vector of characters |

## Bytevector Operations

| Primitive | Description |
|-----------|-------------|
| `make-bytevector` | Create bytevector of given length |
| `bytevector` | Create bytevector from arguments |
| `bytevector-length` | Length of bytevector |
| `bytevector-u8-ref` | Get byte at index |
| `bytevector-u8-set!` | Set byte at index |
| `bytevector-copy` | Copy bytevector (with optional start/end) |
| `bytevector-copy!` | Copy bytes into bytevector |
| `bytevector-append` | Concatenate bytevectors |
| `utf8->string` | Decode UTF-8 bytevector to string |
| `string->utf8` | Encode string to UTF-8 bytevector |

## Input/Output

| Primitive | Description |
|-----------|-------------|
| `read` | Read a datum |
| `read-token` | Read a single token |
| `read-syntax` | Read datum as syntax object |
| `read-char` | Read a character |
| `peek-char` | Peek at next character without consuming |
| `read-line` | Read a line of text |
| `read-string` | Read a string of given length |
| `char-ready?` | Test if character is ready for reading |
| `write` | Write datum in machine-readable form |
| `write-char` | Write a character |
| `write-string` | Write a string (with optional start/end) |
| `display` | Write datum in human-readable form |
| `newline` | Write newline |
| `write-simple` | Write datum without shared structure |
| `write-shared` | Write datum showing shared structure |
| `flush-output-port` | Flush output port buffer |

## Binary I/O

| Primitive | Description |
|-----------|-------------|
| `read-u8` | Read a byte |
| `peek-u8` | Peek at next byte without consuming |
| `u8-ready?` | Test if byte is ready for reading |
| `write-u8` | Write a byte |
| `read-bytevector` | Read bytevector of given length |
| `read-bytevector!` | Read bytes into existing bytevector |
| `write-bytevector` | Write bytevector (with optional start/end) |

## Port Operations

| Primitive | Description |
|-----------|-------------|
| `port?` | Test for port |
| `input-port?` | Test for input port |
| `output-port?` | Test for output port |
| `textual-port?` | Test for textual port |
| `binary-port?` | Test for binary port |
| `input-port-open?` | Test if input port is open |
| `output-port-open?` | Test if output port is open |
| `close-port` | Close a port |
| `close-input-port` | Close an input port |
| `close-output-port` | Close an output port |
| `call-with-port` | Call procedure with port, closing on return |
| `current-input-port` | Parameter for current input port |
| `current-output-port` | Parameter for current output port |
| `current-error-port` | Parameter for current error port |
| `eof-object` | Return the EOF object |
| `eof-object?` | Test for EOF object |

## File I/O

| Primitive | Description |
|-----------|-------------|
| `open-input-file` | Open file for reading |
| `open-output-file` | Open file for writing |
| `open-binary-input-file` | Open binary file for reading |
| `open-binary-output-file` | Open binary file for writing |
| `call-with-input-file` | Call procedure with input file port |
| `call-with-output-file` | Call procedure with output file port |
| `with-input-from-file` | Parameterize current-input-port from file (Scheme macro) |
| `with-output-to-file` | Parameterize current-output-port to file (Scheme macro) |
| `file-exists?` | Test if file exists |
| `delete-file` | Delete a file |

## String and Bytevector Ports

| Primitive | Description |
|-----------|-------------|
| `open-input-string` | Create input port from string |
| `open-output-string` | Create output string port |
| `get-output-string` | Get accumulated string from output port |
| `open-input-bytevector` | Create input port from bytevector |
| `open-output-bytevector` | Create output bytevector port |
| `get-output-bytevector` | Get accumulated bytevector from output port |

## Higher-Order Functions

| Primitive | Description |
|-----------|-------------|
| `apply` | Apply procedure to argument list |

## Continuations

| Primitive | Description |
|-----------|-------------|
| `call-with-current-continuation` | Capture current continuation |
| `call/cc` | Alias for call-with-current-continuation |

## Delimited Continuations

| Primitive | Description |
|-----------|-------------|
| `make-continuation-prompt-tag` | Create a continuation prompt tag |
| `default-continuation-prompt-tag` | Get the default prompt tag |
| `continuation-prompt-tag?` | Test for continuation prompt tag |
| `call-with-continuation-prompt` | Install a prompt and call thunk |
| `abort-current-continuation` | Escape to nearest prompt |
| `call-with-composable-continuation` | Capture composable delimited continuation |
| `continuation-prompt-available?` | Test if a prompt with given tag is on the current continuation |

## Escape Continuations

| Primitive | Description |
|-----------|-------------|
| `call-with-exit` | Call procedure with one-shot escape continuation |
| `call-with-continuation-barrier` | Call thunk with continuation re-entry barrier |

## Continuation Marks

| Primitive | Description |
|-----------|-------------|
| `current-continuation-marks` | Collect marks from current continuation chain |
| `continuation-mark-set->list` | Extract mark values for a key from mark set |
| `continuation-mark-set->list*` | Multi-key variant returning list of vectors |
| `continuation-mark-set-first` | Get first mark value for a key |
| `call-with-immediate-continuation-mark` | Call procedure with mark from current frame |
| `continuation-mark-set?` | Test for continuation mark set |
| `continuation?` | Test for continuation value |

## Reflection

| Primitive | Description |
|-----------|-------------|
| `procedure-arity` | Get arity information of a procedure |
| `procedure-name` | Get name of a procedure |
| `procedure-source-location` | Get source location of a procedure |
| `procedure-bound-symbols` | Get bound symbols of a procedure |
| `procedure-type` | Get type classification of a procedure |
| `procedure-documentation` | Get docstring of a procedure, or `#f` if none |
| `apropos` | Search primitives by name, doc, or category (returns list of symbols) |
| `doc-topics` | List documentation categories (returns sorted list of strings) |
| `doc-topic` | List primitives in a category (returns sorted list of symbols) |

## Multiple Values

| Primitive | Description |
|-----------|-------------|
| `values` | Return multiple values |
| `call-with-values` | Receive multiple values |

## Exception Handling

| Primitive | Description |
|-----------|-------------|
| `with-exception-handler` | Install exception handler |
| `raise` | Raise an exception |
| `raise-continuable` | Raise a continuable exception |
| `error` | Signal an error |
| `error-object?` | Test for error object |
| `error-object-message` | Get error message |
| `error-object-irritants` | Get error irritants |
| `read-error?` | Test for read error |
| `file-error?` | Test for file error |

## Promises

| Primitive | Description |
|-----------|-------------|
| `promise?` | Test for promise |
| `make-promise` | Create an already-forced promise |
| `force` | Force evaluation of promise |

## Parameters

| Primitive | Description |
|-----------|-------------|
| `make-parameter` | Create a parameter object |
| `parameter?` | Test for parameter |

## Boxes

| Primitive | Description |
|-----------|-------------|
| `box` | Create a box containing a value |
| `box?` | Test for box |
| `unbox` | Extract the value from a box |
| `set-box!` | Set the value in a box |

## Hashtables

| Primitive | Description |
|-----------|-------------|
| `make-eq-hashtable` | New table, keys compared with `eq?`; optional size hint ignored |
| `make-eqv-hashtable` | New table, keys compared with `eqv?` |
| `make-equal-hashtable` | New table, keys compared with `equal?` (**not R6RS** — Chez/Larceny/Vicare/Ypsilon extension). Prefer `make-eq-hashtable` for keys whose `equal?` is identity (record types, ports, procedures), which otherwise share one bucket |
| `make-hashtable` | R6RS `(make-hashtable hash equiv [k])`; only the built-in `(equal-hash, equal?)` pair is accepted, anything else raises |
| `hashtable?` | Test for hash table |
| `hashtable-ref` | `(hashtable-ref ht key default)` — `default` is **required**; an absent key returns it and never errors |
| `hashtable-set!` | Associate key with value |
| `hashtable-delete!` | Remove key from hash table |
| `hashtable-contains?` | Test whether a key is present |
| `hashtable-update!` | Set `key` to `(proc current-or-default)` |
| `hashtable-keys` | Return a **vector** of all keys |
| `hashtable-entries` | Return two values: a keys vector and an index-aligned values vector (replaces `hashtable-values`) |
| `hashtable-size` | Return number of entries |
| `hashtable-copy` | `(hashtable-copy ht [mutable])` — **immutable unless `mutable` is true**, per R6RS |
| `hashtable-clear!` | Remove all entries; optional size hint ignored |
| `hashtable-mutable?` | Test whether the table accepts mutation |
| `hashtable-equivalence-function` | Return `eq?`, `eqv?` or `equal?` |
| `hashtable-hash-function` | Return `equal-hash`, or `#f` for eq/eqv tables |
| `equal-hash` | Structural hash consistent with `equal?`; terminates on cycles |
| `string-hash` | Hash consistent with `string=?` (unary, unbounded) |
| `string-ci-hash` | Hash consistent with `string-ci=?` (full Unicode folding) |
| `symbol-hash` | Hash consistent with `symbol=?` |

## Evaluation

| Primitive | Description |
|-----------|-------------|
| `eval` | Evaluate expression in environment |
| `scheme-report-environment` | Get R5RS environment |
| `null-environment` | Get minimal environment |
| `environment` | Create environment from library specs |

## Introspection

| Primitive | Description |
|-----------|-------------|
| `interaction-environment` | Get interactive top-level environment (introspection extension) |
| `environment?` | Test for environment object |
| `environment-bound-names` | List all bound names in an environment |
| `environment-ref` | Look up a binding by symbol in an environment |
| `environment-bound?` | Test if a symbol is bound in an environment |

## Loading

| Primitive | Description |
|-----------|-------------|
| `load` | Load and execute a file |
| `current-load-path` | Absolute path of file being loaded, or `#f` in REPL |
| `current-load-directory` | Directory of file being loaded, or `#f` in REPL |
| `current-load-depth` | Nesting depth of load stack (0 in REPL) |

## Expansion and Compilation

| Primitive | Description |
|-----------|-------------|
| `expand` | Fully expand an expression |
| `expand-once` | Expand one level of macros |
| `compile` | Compile an expression |
| `syntax-local-value` | Get compile-time value of binding |
| `syntax-local-value/immediate` | Like `syntax-local-value`, no rename-transformer chasing |
| `make-compile-time-value` | Create a compile-time value |
| `syntax-local-introduce` | Introduce syntax marks |
| `syntax-local-identifier-as-binding` | Convert identifier to binding form |

## Records

| Primitive | Description |
|-----------|-------------|
| `make-record-type` | Create a new record type |
| `record-type?` | Test for record type |
| `record?` | Test for record instance |
| `record-type` | Get type of record |
| `record-constructor` | Get record constructor |
| `record-predicate` | Get record predicate |
| `record-accessor` | Get record field accessor |
| `record-modifier` | Get record field modifier |

## Process Context

| Primitive | Description |
|-----------|-------------|
| `command-line` | Get command-line arguments |
| `exit` | Exit with status code |
| `emergency-exit` | Exit immediately without cleanup |
| `get-environment-variable` | Get environment variable value |
| `get-environment-variables` | Get all environment variables |

## Time

| Primitive | Description |
|-----------|-------------|
| `current-second` | Current time in seconds since epoch |
| `current-jiffy` | Current time in jiffies |
| `jiffies-per-second` | Number of jiffies per second |

## Feature Detection

| Primitive | Description |
|-----------|-------------|
| `features` | List of supported features |

## Threads (SRFI-18)

| Primitive | Description |
|-----------|-------------|
| `current-thread` | Get current thread |
| `thread?` | Test for thread |
| `make-thread` | Create a new thread |
| `thread-name` | Get thread name |
| `thread-specific` | Get thread-specific data |
| `thread-specific-set!` | Set thread-specific data |
| `thread-start!` | Start a thread |
| `thread-yield!` | Yield to other threads |
| `thread-sleep!` | Sleep for duration |
| `thread-terminate!` | Terminate a thread |
| `thread-join!` | Wait for thread completion |
| `thread-state` | Thread state symbol — `new`, `runnable`, `blocked`, `terminated` (**not SRFI-18**; follows Gambit) |

## Thread Exceptions (SRFI-18)

Raised into the calling thread's handler chain, so a `guard` around the call can
discriminate them.

| Primitive | Description |
|-----------|-------------|
| `join-timeout-exception?` | `thread-join!` reached its timeout with no default value |
| `terminated-thread-exception?` | The joined thread died via `thread-terminate!` |
| `abandoned-mutex-exception?` | `mutex-lock!` acquired a mutex whose owner terminated holding it |
| `uncaught-exception?` | The joined thread died via an uncaught exception |
| `uncaught-exception-reason` | The condition that thread originally raised |

## Mutexes (SRFI-18)

| Primitive | Description |
|-----------|-------------|
| `mutex?` | Test for mutex |
| `make-mutex` | Create a new mutex |
| `mutex-name` | Get mutex name |
| `mutex-specific` | Get mutex-specific data |
| `mutex-specific-set!` | Set mutex-specific data |
| `mutex-state` | Get mutex state |
| `mutex-lock!` | Lock a mutex |
| `mutex-unlock!` | Unlock a mutex |

## Condition Variables (SRFI-18)

| Primitive | Description |
|-----------|-------------|
| `condition-variable?` | Test for condition variable |
| `make-condition-variable` | Create a condition variable |
| `condition-variable-name` | Get condition variable name |
| `condition-variable-specific` | Get condition variable-specific data |
| `condition-variable-specific-set!` | Set condition variable-specific data |
| `condition-variable-signal!` | Signal one waiting thread |
| `condition-variable-broadcast!` | Signal all waiting threads |

## Time Objects (SRFI-18)

| Primitive | Description |
|-----------|-------------|
| `current-time` | Get current time object |
| `time?` | Test for time object |
| `time->seconds` | Convert time to seconds |
| `seconds->time` | Convert seconds to time |

## Go Atomic

| Primitive | Description |
|-----------|-------------|
| `make-atomic` | Create an atomic value |
| `atomic?` | Test for atomic value |
| `atomic-load` | Load atomic value |
| `atomic-store!` | Store atomic value |
| `atomic-swap!` | Swap atomic value, return old |
| `atomic-compare-and-swap!` | Compare and swap atomically |
