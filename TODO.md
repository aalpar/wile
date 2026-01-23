TODO
----

Code Maintenance
----------------
- [ ] Dead code removal (see Dead Code Removal section below)
- [x] Project rename from "skeme" to "wile" (module path, binary name, env vars, docs)
- [ ] check all functions that accept pairs or lists that they handle partial lists correctly.
  functions should either accept partial lists (lists without the last element being a EmptyList)
  or should produce a ErrNotAPair or ErrNotAList errors.
- [ ] check all scheme primitives that they work properly with partial lists (lists without the last element being a EmptyList)
  each function should return an Error object or return the resule defined in the R7RS specification.  All functions must be
  R7RS or R6RS compliant

Code Cleanup
------------
- [ ] Siplify phase (meta environments) - extension facility has ordianl bindings for phases while Environment uses relative phases (so relationships can be more complex).  Consider the simpler approach for Wile
- [ ] begin-for-syntax is a unfamiliar R7RS function.  investigate possible implmentations
- [ ] investigate compileBeginBody implementation
- [ ] check quasiquote exapnsion for potential simplifications
- [ ] check letrec-syntax and let-syntax for simplification oppurtunities
- [ ] SyntaxValue imlpements UnwrapAllShared.  Why not change all instances of UnwrapAll to UnwrapAllShared?
- [ ] Compare methods on numbers - not all numbers need to be comparable, but all numbers should support a CompareTo method
- [ ] Compare methods on numbers should be CompareTo to conform to Comparable.
- [ ] Remove extranious methods on Numbers
- [ ] Numbers, BigInteger, BigFloat: ensure all number operations handle mixed types correctly (Integer, Float, Rational, Complex, BigInteger, BigFloat) do not share the same comparison operations and are duplicated in multiple places.
- [ ] auxiliary syntax exports: R7RS requires `(scheme base)` to export `else`, `=>`, `...`, `_` as auxiliary syntax keywords. Currently these cannot be exported because they aren't bound as values - they're pattern literals handled specially by `syntax-rules`. Need to implement auxiliary syntax binding mechanism.
- [ ] Use `values.Tuple` instead of `*values.Pair` when possible
- [ ] Use `values.Number` when possible
- [ ] Use `values.Indexable` for indexable values (except maps used `values.Mappable`)
- [ ] Use `BoolToBoolean` where possible.
- [ ] Tokenization error handling is obscure - figure something better out
- [ ] Inf Nan handling in tokenizer is inconsistent.  Some places use math.Inf(1)/math.Inf(-1) and some use predefined constants.  Standardize on one approach.
- [x] Refactor `if err := ...; err != nil` patterns to separate assignment from comparison (see CODING_STYLE.md)
- [ ] consoloidate error handling for unimplemented features (e.g., in compiler)
- [ ] syntax->datum and datum->syntax have duplicate code.  Consolidate.
- [ ] Refactor compiler primitive handling to use PrimitiveCompiler registry for less code redundancy
- [ ] Refactor compiler literal handling to use LiteralCompiler registry for less code redundancy
- [ ] Refactor compiler special form handling to use SpecialFormCompiler registry for less code redundancy
- [ ] Refactor compiler expansion handling to use Expander registry for less code redundancy
- [ ] Refactor compiler optimization handling to use Optimizer registry for less code redundancy
- [ ] Refactor compiler evaluation handling to use Evaluator registry for less code redundancy
- [ ] compile_validate.go needs to be reduced in size.  Separate methods for types instead of using interfaces.
- [ ] evalWhenCompileForRuntime uses odd loop.  Analyze and simplify.
- [ ] "Must" wrapper for ForEach so that proper list check can be removed
- [ ] compile_eval_when.go: list of hardcoded phases.  can this be moved to a table-driven approach?
- [ ] Turn tests into table-driven tests where applicable
- [ ] Unwrap and Datums: simplify code by removing unnecessary wrapping and unwrapping of Datum types. Its laborious and duplicative in a few places.  Try to keep Datum wrapping/unwrapping at the edges of functions. Keep as much code as part of the methods on the objects.
- [ ] Get rid of vardec functions (too confusing)
- [ ] Fix tokenizer warnings: unreachable code (lines 1641, 1664) and unhandled errors
- [ ] Refactor tokenizer duplicate code (see Tokenizer Refactoring Notes below)
- [ ] Refactor ExpandPrimitiveForm into PrimitiveExpander registry (like PrimitiveCompiler)
- [ ] Refactor "formName" processing for compiler-form. Form name should come from car of original form, but instead is being set on an object. Allow dynamic setting of the form-name. Note that the form sometimes is abstract, such as a parameter list or a literal - find a solution for this.
- [ ] Refactor environment functions for less code redundancy
- [ ] Rename token types for comments.
- [ ] Number parsing in tokenizer is messy.  Refactor to reduce code redundancy and improve clarity.  Evaluate removing "signed" token types.
- [ ] Consolidate tests into single table tests.  Many tests have the same code for running the tests but are in different files and test functions.  Combine into single table-driven tests where possible.
- [ ] Scheme header "Program running, send SIGQUIT (Ctrl+\\) to dump stacks." should be output to stderr and add option for "--quiet"
- [ ] fixup "( environment )" creation.
- [ ] eval should take 1 or 2 arguments (second argument is optional environment)
- [ ] parseComplex is messy - refactor to reduce code redundancy and improve clarity.

Primitive Unit Tests
--------------------

**Status:** Many primitives have dedicated test files (89 test files exist).

Each primitive file (`prim_*.go`) should have a matching `prim_*_test.go` file that:
1. Executes Scheme code to test the primitive
2. Tests all accepted types (Integer, BigInteger, Float, Rational, Complex, etc.)
3. Tests edge cases (empty, single element, boundary conditions)
4. Tests error conditions (wrong types, out of bounds, etc.)

**Test Pattern:** Use `runSchemeCode(t, code)` helper with table-driven tests:
```go
func TestPrimName(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        {"happy path", `(prim-name arg)`, values.NewInteger(42)},
        {"edge case", `(prim-name)`, values.NewInteger(0)},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, values.SchemeEquals, tc.out)
        })
    }
}
```

### Arithmetic (17 primitives)
- [x] `prim_arithmetic_test.go` - +, -, *, /, quotient, remainder, modulo, gcd, lcm, expt, sqrt, square, max, min, abs, floor, ceiling, round, truncate (all tested)
- [ ] `prim_exact_integer_sqrt_test.go` - exact-integer-sqrt, test: perfect squares, non-perfect (returns two values)
- [ ] `prim_rationalize_test.go` - rationalize, test: tolerance parameter

### Transcendental Functions (8 primitives)
- [x] `prim_trig_test.go` - exp, log, sin, cos, tan, asin, acos, atan (all tested including complex inputs, special values)

### Complex Numbers (6 primitives)
- [x] `prim_complex_test.go` - make-rectangular, real-part, imag-part, magnitude (all tested)
- [x] `prim_complex_extra_test.go` - make-polar, angle (all tested including round-trip tests)

### Numeric Predicates (15 primitives)
- [x] `prim_numeric_predicate_test.go` - zero?, odd?, even?, positive?, negative?, exact?, inexact?, exact-integer?, finite?, infinite?, nan?, number?, complex?, real?, rational?, integer? (all tested with multiple numeric types including BigInteger, BigFloat, special values)
- [x] `prim_predicate_test.go` - zero?, positive?, negative?, odd?, even? (additional coverage)
- [x] `prim_special_predicates_test.go` - finite?, infinite?, nan?, real-part (tested with rational and complex)
- [x] `prim_exact_q_test.go`, `prim_inexact_q_test.go`, `prim_exact_integer_q_test.go` - additional exactness predicate tests

### Numeric Comparisons (5 primitives)
- [x] `prim_numeric_predicate_test.go` - =, <, >, <=, >= (all tested with mixed types, chains, BigInteger, special values, NaN behavior)

### Numeric Conversion (6 primitives)
- [x] `prim_exact_test.go` - exact (tested with integer, float, rational, error cases including inf/nan)
- [x] `prim_inexact_test.go` - inexact (tested with integer, float, rational, complex)
- [x] `prim_complex_test.go`, `prim_numeric_conversion_test.go` - numerator, denominator (tested with rationals, integers, floats)
- [x] `prim_numeric_conversion_test.go` - number->string, string->number (tested with radix 2,8,10,16, invalid strings, various numeric types)

### Division Operations (6 primitives)
- [x] `prim_division_test.go` - floor/, floor-quotient, floor-remainder, truncate/, truncate-quotient, truncate-remainder (all tested with various numeric types, positive/negative numbers, multiple values return)

### List Operations (20 primitives)
- [x] `prim_list_test.go` - car, cdr, cons, list, null?, pair?, list?, set-car!, set-cdr!, list-set!, make-list, append, length, reverse, list-ref, list-tail, memq, memv, member, assq, assv, assoc (all tested with various types, edge cases, error conditions)

### Equality Predicates (3 primitives)
- [x] `prim_eq_q_test.go` - eq?, test: identical objects, symbols, small integers
- [x] `prim_eqv_q_test.go` - eqv?, test: numbers, characters, booleans
- [x] `prim_equal_q_test.go` - equal?, test: deep comparison, lists, vectors, strings

### String Operations (12 primitives)
- [x] `prim_string_test.go` - string-length, string-ref, substring, string-append, string->list, list->string, string->symbol, symbol->string, make-string, string-copy, string-upcase, string-downcase, string-foldcase, string-set!, string-fill!, string-copy!, string-map, string-for-each, string, string? (all tested with ASCII, Unicode, edge cases)

### String Comparisons (10 primitives)
- [x] `prim_string_compare_test.go` - string=?, string<?, string>?, string<=?, string>=?, string-ci=?, string-ci<?, string-ci>?, string-ci<=?, string-ci>=? (all tested with variadic args, case-insensitive, Unicode)

### Character Operations (12 primitives)
- [x] `prim_char_test.go` - char=?, char<?, char>?, char<=?, char>=?, char->integer, integer->char, char-ci=?, char-ci<?, char-ci>?, char-ci<=?, char-ci>=?, char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case?, char-upcase, char-downcase, char-foldcase, digit-value (all tested including variadic, case-insensitive, Unicode)

### Vector Operations (6 primitives)
- [x] `prim_vector_test.go` - make-vector, vector-length, vector-ref, vector->list, list->vector (all tested)
- [ ] `prim_vector_set_test.go` - vector-set!, test: mutation (used in other tests but no dedicated tests)

### Bytevector Operations (10 primitives)
- [x] `prim_bytevector_test.go` - bytevector?, make-bytevector, bytevector, bytevector-length, bytevector-u8-ref, bytevector-u8-set!, bytevector-copy, bytevector-append, utf8->string, string->utf8 (all tested with edge cases, round-trip tests)
- [ ] `prim_bytevector_copy_bang_test.go` - bytevector-copy!, test: mutation with start/end indices

### I/O Ports (25 primitives)
- [ ] `prim_open_input_file_test.go`
- [ ] `prim_open_output_file_test.go`
- [x] `prim_open_binary_input_file_test.go`
- [x] `prim_open_binary_output_file_test.go`
- [ ] `prim_open_input_string_test.go`
- [ ] `prim_open_output_string_test.go`
- [ ] `prim_open_input_bytevector_test.go`
- [ ] `prim_open_output_bytevector_test.go`
- [ ] `prim_get_output_string_test.go`
- [ ] `prim_get_output_bytevector_test.go`
- [x] `prim_close_port_test.go`
- [ ] `prim_input_port_q_test.go`
- [ ] `prim_output_port_q_test.go`
- [ ] `prim_port_q_test.go`
- [ ] `prim_input_port_open_q_test.go`
- [ ] `prim_output_port_open_q_test.go`
- [x] `prim_current_input_port_test.go` (in prim_current_port_test.go)
- [x] `prim_current_output_port_test.go` (in prim_current_port_test.go)
- [x] `prim_call_with_input_file_test.go`
- [x] `prim_call_with_output_file_test.go`
- [x] `prim_with_input_from_file_test.go`
- [x] `prim_with_output_to_file_test.go`
- [x] `prim_eof_object_test.go`
- [x] `prim_eof_object_q_test.go` (in prim_eof_object_test.go)

### Read/Write (8 primitives)
- [ ] `prim_read_test.go`
- [ ] `prim_read_syntax_test.go`
- [ ] `prim_read_token_test.go`
- [ ] `prim_write_test.go`
- [x] `prim_write_simple_test.go`
- [x] `prim_write_shared_test.go`
- [ ] `prim_display_test.go`
- [ ] `prim_write_char_test.go`
- [ ] `prim_newline_test.go`

### Control Flow (8 primitives)
- [x] `prim_apply_test.go` - apply, test: with list, with multiple args + list
- [x] `prim_map_test.go` - map, test: single list, multiple lists, empty lists
- [x] `prim_for_each_test.go` - for-each, test: side effects, return value
- [ ] `prim_call_cc_test.go` - expand existing tests for edge cases
- [x] `prim_call_with_values_test.go` - test: producer/consumer pattern
- [x] `prim_values_test.go` - values, test: zero, one, multiple values
- [x] `prim_dynamic_wind_test.go` - test: before/after thunks, with continuations
- [x] `prim_not_test.go` - not, test: #f->#t, everything else->#f

### Exception Handling (6 primitives)
- [x] `prim_with_exception_handler_test.go` (in prim_exception_test.go)
- [x] `prim_raise_test.go` (in prim_exception_test.go)
- [x] `prim_raise_continuable_test.go` (in prim_exception_test.go)
- [x] `prim_error_test.go` (in prim_exception_test.go)
- [x] `prim_error_object_q_test.go` (in prim_exception_test.go)
- [x] `prim_error_object_message_test.go` (in prim_exception_test.go)
- [x] `prim_error_object_irritants_test.go` (in prim_exception_test.go)

### Promises (4 primitives)
- [x] `prim_make_promise_test.go` (in prim_promise_test.go)
- [x] `prim_make_lazy_promise_test.go` (in prim_promise_test.go)
- [x] `prim_force_test.go` - test: memoization, multiple force calls (in prim_promise_test.go)

### Parameters (2 primitives)
- [x] `prim_make_parameter_test.go` - test: with/without converter (in prim_parameter_test.go)

### Environment/Eval (6 primitives)
- [x] `prim_eval_test.go` (in prim_eval_env_test.go)
- [x] `prim_environment_test.go` (in prim_eval_env_test.go)
- [x] `prim_interaction_environment_test.go` (in prim_eval_env_test.go)
- [x] `prim_scheme_report_environment_test.go` (in prim_eval_env_test.go)
- [x] `prim_null_environment_test.go` (in prim_eval_env_test.go)
- [x] `prim_load_test.go` (in prim_delete_load_test.go)

### Syntax Operations (8 primitives)
- [x] `prim_datum_to_syntax_test.go` (in prim_identifier_test.go)
- [x] `prim_syntax_to_datum_test.go` (in prim_identifier_test.go)
- [x] `prim_identifier_q_test.go` (in prim_identifier_test.go)
- [x] `prim_bound_identifier_equal_q_test.go` (in prim_identifier_test.go)
- [x] `prim_free_identifier_equal_q_test.go` (in prim_identifier_test.go)
- [x] `prim_syntax_local_value_test.go` (in prim_identifier_test.go)
- [x] `prim_syntax_local_introduce_test.go` (in prim_identifier_test.go)
- [x] `prim_syntax_local_identifier_as_binding_test.go` (in prim_identifier_test.go)
- [x] `prim_make_compile_time_value_test.go` (in prim_identifier_test.go)

### Expansion (3 primitives)
- [ ] `prim_expand_test.go`
- [ ] `prim_expand_once_test.go`
- [ ] `prim_compile_test.go`

### File System (2 primitives)
- [x] `prim_file_exists_q_test.go` (in prim_file_env_test.go)
- [x] `prim_delete_file_test.go` (in prim_delete_load_test.go)

### Process/Environment (5 primitives)
- [x] `prim_command_line_test.go` (in prim_file_env_test.go)
- [ ] `prim_exit_test.go`
- [ ] `prim_emergency_exit_test.go`
- [x] `prim_get_environment_variable_test.go` (in prim_file_env_test.go)
- [x] `prim_get_environment_variables_test.go` (in prim_file_env_test.go)
- [x] `prim_features_test.go` (in prim_misc_test.go)

### Time (4 primitives)
- [x] `prim_current_second_test.go` (in prim_misc_test.go)
- [x] `prim_current_jiffy_test.go` (in prim_misc_test.go)
- [x] `prim_jiffies_per_second_test.go` (in prim_misc_test.go)
- [x] `prim_time_test.go` (in prim_srfi18_time_test.go)

### Threading - SRFI-18 (3 primitives)
- [x] `prim_thread_test.go`
- [x] `prim_mutex_test.go`
- [x] `prim_condvar_test.go`

### Go Concurrency (2 primitives)
- [x] `prim_channel_test.go`
- [x] `prim_sync_test.go` - WaitGroup, RWMutex, Once, Atomic

### Miscellaneous
- [ ] `prim_utils_test.go`

Code Refactoring (see REFACTORING_PROPOSAL.md)
----------------------------------------------
- [x] Migrate primitives from `runtime/primitives/` to `registry/core/` and `extensions/*/`
- [x] Move primitive tests to `registry/core/`
- [ ] Add `go/registry/helpers/args.go` - helper functions for argument extraction (~600 lines saved)
- [ ] Add `go/machine/operation_helpers.go` - EqualTo helper functions (~300 lines saved)
- [ ] Migrate ~27 operation files to use EqualTo helpers

R7RS Missing Features
---------------------

### Tokenizer (R7RS 7.1.1 Lexical Structure)

**Completely Missing:**
- [ ] Extended symbols (`|...|`): Tokenizer returns `ExtendedSymbolStart` for `|` but never reads contents or closing `|`. Parser doesn't handle this token. R7RS requires `|<symbol element>*|` where `<symbol element>` is any character except `\` or `|`, or escape sequences.
- [ ] Escapes inside extended symbols: Within `|...|`, R7RS requires `\a`, `\b`, `\t`, `\n`, `\r`, `\|`, `\\`, and `\x<hex>;`
- [ ] `\|` escape in strings: R7RS requires `\|` to produce vertical bar. Not in `readIntraStringEscape` (tokenizer.go:687-724)
- [ ] String line continuation: R7RS requires `\<intraline-whitespace>*<line-ending><intraline-whitespace>*` to escape nothing (continuation). Current code just adds whitespace chars (line 714-715)
- [ ] Hex escape semicolon terminator in strings: R7RS requires `\x<hex>;` format. Current code reads hex digits without expecting terminating semicolon (line 693)

**Partially Implemented:**
- [ ] Scientific notation in library files: Numbers like `1e-10` fail to parse in .sld files with "strconv.ParseInt: parsing '1e-10': invalid syntax". The tokenizer handles scientific notation in the REPL but not consistently in all parsing contexts.
- [ ] Exponent markers: Only `e`/`E` supported. R7RS also allows `s`, `f`, `d`, `l` for short/single/double/long precision
- [ ] Inexact digit placeholder `#`: R7RS allows `#` in inexact numbers (e.g., `1.2###`). Not implemented

### Syntax/Macros

- [ ] `case` macro (R7RS §4.2.1): The `case` conditional expression is not implemented. Bootstrap macros in `go/registry/core/bootstrap.go` include `cond` but not `case`.
- [ ] `letrec*` macro (R7RS §4.2.2): Sequential letrec binding form not implemented.
- [ ] `let-syntax` / `letrec-syntax` (R7RS §4.3.1): Local syntax definitions not implemented.
- [ ] `syntax-error` (R7RS §4.3.1): Macro error signaling not implemented.
- [ ] `define-values` (R7RS §5.3.3): Multiple value definition not implemented.

### Macro/Library System

- [ ] Macro hygiene with library-internal bindings: Macros defined in a library that reference helper functions defined in the same library fail at the use site with "no such binding". The macro expander should preserve the library's bindings for identifiers introduced by the macro, but currently the expanded code references unbound identifiers. Workaround: export helper functions with `%` prefix.

### Primitives

- [ ] Box primitives: `box`, `box?`, `unbox`, `set-box!` (Box type exists in values/box.go but no Scheme primitives registered)
- [ ] Hashtable primitives: `make-hashtable`, `hashtable?`, `hashtable-ref`, `hashtable-set!`, `hashtable-delete!`, `hashtable-keys`, `hashtable-values`, `hashtable-size`, `hashtable-copy`, `hashtable-clear!` (Hashtable type exists in values/hashtable.go but no Scheme primitives; also current implementation only supports string keys, not arbitrary Scheme values)
- [ ] BigInteger: No automatic promotion from Integer or `#bigint` reader syntax (BigInteger type exists in values/big_integer.go)
- [ ] BigFloat: No automatic promotion from Float or `#bigfloat` reader syntax (BigFloat type exists in values/big_float.go)

Library Status
--------------

| Library               | Status | Notes |
|-----------------------|--------|-------|
| scheme/base           | ~90%   | Missing: `case`, `letrec*`, `let-syntax`, `letrec-syntax`, `syntax-error`, `define-values`, auxiliary syntax (`else`, `=>`, `...`, `_`) |
| scheme/char           | 100%   | |
| scheme/file           | 100%   | |
| scheme/write          | 100%   | |
| scheme/r5rs           | 100%   | |
| chibi/test            | 100%   | Minimal stub implementation (not full chibi library) |
| (others)              | 100%   | |

---

Dead Code Removal
-----------------

### Phase 5 (Optional): Test-Only Functions
- [ ] Consider deleting `AddScopeToSet()` and `RemoveScopeFromSet()` in `go/syntax/scope_utils.go`

### Phase 6 (Optional): Fix Method Signature
- [ ] Fix `Error.Unwrap()` to return `error` instead of `*String` for Go compatibility

---

Future Extensions
-----------------

### Standard Libraries

**Network Libraries (Racket-compatible)**
- [ ] TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
- [ ] HTTP client/server primitives
- [ ] SSL/TLS support
- [ ] DNS resolution

**OS Libraries (Racket-compatible)**
- [ ] Process execution (subprocess, system, system*)
- [ ] Process control (kill, wait)
- [ ] Fork/exec primitives
- [ ] Environment variables (getenv, putenv)
- [ ] File system operations beyond R7RS (permissions, symlinks, stat)
- [ ] Signal handling

**Unit Testing Library**
- [ ] Test case definition (test, test-case, test-suite)
- [ ] Assertions (check-equal?, check-true, check-false, check-exn)
- [ ] Test runners with reporting
- [ ] Setup/teardown fixtures

**Logging Library**
- [ ] Log levels (debug, info, warn, error, fatal)
- [ ] Structured logging with key-value pairs
- [ ] Multiple outputs (console, file, custom handlers)
- [ ] Log formatting and filtering

### Multithreading

See **DESIGN_MULTITHREADING.md** for full implementation plan.

**Approach:** SRFI-18 standard API + Go-native extensions (channels, sync primitives)

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Thread infrastructure, thread-safe globals | Complete |
| 2 | Basic thread primitives (make-thread, thread-start!, thread-join!) | Complete |
| 3 | Mutex primitives (make-mutex, mutex-lock!, mutex-unlock!) | Complete |
| 4 | Condition variables | Complete |
| 5 | Go channels (make-channel, channel-send!, channel-receive, channel-select) | Complete |
| 6 | Go sync extensions (WaitGroup, RWMutex, Once, atomics) | Complete |
| 7 | Exception handling integration | Complete |
| 8 | call/cc and dynamic-wind integration | Not started |

**Key challenges:**
- MachineContext thread safety (each thread gets own context)
- Global environment synchronization (RWMutex on bindings)
- call/cc scope limited to single thread
- dynamic-wind cleanup on thread termination

### Records (SRFI-9)

Record types for user-defined data structures. SRFI-9 is the de facto standard for R7RS-small implementations.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | `define-record-type` macro | Not started |
| 2 | Constructor, predicate, accessor generation | Not started |
| 3 | Mutator (setter) generation | Not started |
| 4 | Integration with `equal?` and `write` | Not started |

**Syntax:**
```scheme
(define-record-type <point>
  (make-point x y)
  point?
  (x point-x point-x-set!)
  (y point-y point-y-set!))
```

**References:**
- https://srfi.schemers.org/srfi-9/srfi-9.html

### Programmatic Tokenization and Parsing

See **DESIGN_PROGRAMMATIC_READER.md** for full implementation plan.

Expose tokenizer and parser to Scheme code for building custom readers, REPLs, and tooling.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Token introspection (token?, token-type, token-value, etc.) | Not started |
| 2 | Syntax introspection (syntax?, syntax-line, syntax-column, etc.) | Not started |
| 3 | EOF handling improvements | Not started |
| 4 | Advanced reader control (optional) | Not started |

### POSIX API (SRFI-170)

See **DESIGN_POSIX.md** for full implementation plan.

Comprehensive POSIX API implementing SRFI-170 with Go-native implementation.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | File information (stat, file-info) | Not started |
| 2 | Permissions and ownership | Not started |
| 3 | Links and directories | Not started |
| 4 | Temp files and misc operations | Not started |
| 5 | Environment variables | Not started |
| 6 | Process execution (subprocess, system) | Not started |
| 7 | Signal handling | Not started |
| 8 | User/group database | Not started |
| 9 | Terminal control | Not started |
| 10 | Error handling (SRFI-198) | Not started |

**Key features:**
- File system operations (stat, permissions, links, directories)
- Process control (subprocess, system, signals)
- User and group database access
- Terminal control
- Platform-aware (Unix vs Windows)

### Racket-style Scribble Syntax (At-Expressions)

Support for Racket's `@`-reader syntax for inline documentation and text processing.

**Syntax forms:**
- `@id{text}` — Function call with text argument: `(id "text")`
- `@id[arg ...]{text}` — Function call with args and text: `(id arg ... "text")`
- `@{text}` — Literal text string
- `|{text}|` — Verbatim text (no escaping)

**Implementation phases:**
- [ ] Tokenizer: Recognize `@` as reader dispatch character
- [ ] Parser: Handle `@`-expression forms and text blocks
- [ ] Integration: Enable/disable via reader flag or `#lang at-exp`

**References:**
- https://docs.racket-lang.org/scribble/reader.html
- https://docs.racket-lang.org/at-exp/index.html

### Arbitrary Precision Numbers
- [ ] Tagged literals: `#bigint`, `#bigfloat` using Go's `big.Int` and `big.Float`.

### Go FFI
- [ ] Registry-based (Phase 1) → Reflection-based (Phase 2) → Plugin support (Phase 3).
See detailed design notes in DESIGN_GO_FFI.md.

### Runtime Source Location Tracking

Track variable definition sites and enable source-level debugging at runtime.

See **DESIGN_SOURCE_TRACKING.md** for full implementation plan.

**Implementation status:** ✅ Infrastructure complete

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Binding source locations | ✅ Complete |
| 2 | Source map in templates | ✅ Complete |
| 3 | Stack traces | ✅ Complete |
| 4 | Debugger support (breakpoints, stepping) | ✅ Complete |

**What's implemented:**
- `Binding.source` field for variable definition sites
- `SourceMap` type for PC → source location mapping
- `NativeTemplate.sourceMap` and `NativeTemplate.name` fields
- `StackFrame` and `StackTrace` types
- `SchemeError` type with source location and stack trace
- `Debugger` type with breakpoints and stepping modes
- `MachineContext.CaptureStackTrace()` and `CurrentSource()` methods
- Debugger integration in VM `Run()` loop

**Remaining work to fully utilize:**
- [ ] Wire up compilation to record source locations in source map
- [ ] Wire up error handling to use `SchemeError` with `CaptureStackTrace()`
- [ ] Create debugger REPL or IDE integration (e.g., Debug Adapter Protocol)

---

Tokenizer Refactoring Notes
---------------------------
Potential helper functions to reduce ~200-300 lines:
- [ ] `readRadixPrefix` — consolidate #b/#o/#d/#x handling
- [ ] `readBooleanLiteral` — consolidate #t/#true and #f/#false
- [ ] `scanKeyword` — unify scan(), scanCaseInsensitive(), readToken()
- [ ] `readDecimalFractionWithExponent` — extract decimal+exponent pattern
- [ ] `readImaginarySuffix` — consolidate imaginary number suffixes
- [ ] `readExplicitSignNumber` — consolidate +/- number handling
- [ ] `advanceOrError` — combine next() + error check
- [ ] `checkDelimiter` — replace inline delimiter checking
- [ ] `readInfNan` — consolidate inf.0/nan.0 parsing

Reflection
----------
- [ ] procedures for reflection into the environment - lists of bound symbol names.  Parameters for procedures.  Types and predicates for types.

Event Callbacks
---------------
- [ ] variables to hold event callback methods for expansion, compiling and some low level runtime functions (setting values - for debugging).

