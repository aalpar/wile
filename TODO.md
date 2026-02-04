TODO
----

Code Cleanup
------------
### Re-Work Numeric Tower
- [ ] Refactor Numeric Tower

### Include Examples
- [ ] Include examples

### Investigate Symbol Interning Semantic
- [ ] Symbol interning semantics should be looked up in R7RS.  Look at benefits of environment interning with global interning.

### FreeIdResolution
- [ ] Try and find a more specific type for the interface, instead of 'any'.
- [ ] **Location:** `go/match/syntax_adapter.go` and `go/machine/compile_syntax_rules.go`

### Scope Matching Optimization
- [ ] **Location:** `go/syntax/` and `go/match/`
- [ ] **Issue:** Scope set matching is mostly brute force O(n×m) comparison
- [ ] **Goal:** Investigate optimization opportunities (hash-based set comparison, scope indexing)
- [ ] **Impact:** Performance improvement for complex macro expansions

---

### Quasiquote Expansion
**Status:** Only `compile_quasisyntax.go` exists (no separate `compile_quasiquote.go`).

`go/machine/compile_quasisyntax.go` shows well-structured design:
- Proper depth tracking for nested quasisyntax
- Clean separation: entry point → depth management → template transformation
- Sophisticated list handling with unsyntax-splicing segmentation

---

### Remove Extraneous Number Methods
- [ ] **Location:** `go/values/integer.go`, `float.go`, `rational.go`, `complex.go`, `big_*.go`
- [ ] **Goal:** Audit number types for unused or redundant methods
- [ ] **Note:** All number types already share common `Number` interface; may be minimal redundancy

---

### Numeric Type Unification
- [ ] **Issue:** Integer, Float, Rational, Complex, BigInteger, BigFloat have some duplicate comparison logic
- [ ] **Location:** `go/values/numeric_tower.go` provides unified dispatch (`TowerAdd`, `TowerSubtract`, `TowerCompare`, etc.) but per-type methods still exist
- [ ] **Goal:** Ensure all cross-type operations go through unified Tower functions
- [ ] **Files:** All `go/values/*_number.go` files

---

### Use values.Tuple Instead of *values.Pair
- [ ] **Goal:** Where code processes list-like data internally, prefer `values.Tuple` interface over concrete `*values.Pair`
- [ ] **Benefit:** Allows `ArrayList` and `Vector` to be used interchangeably
- [ ] **Scope:** Audit compiler and primitive implementations

---

### Use values.Number Interface
- [ ] **Goal:** Use `values.Number` interface where code accepts any numeric type
- [ ] **Benefit:** Cleaner type checking, better error messages
- [ ] **Files:** Primitive implementations, compiler

---

### Use values.Indexable Interface
- [ ] **Goal:** Use `values.Indexable` for indexable values (vectors, strings, bytevectors)
- [ ] **Note:** For maps use `values.Mappable`
- [ ] **Benefit:** Unified ref/set operations

---

### Use BoolToBoolean Helper
- [ ] **Location:** `go/utils/predicate.go` provides `BoolToBoolean(bool) *Boolean`
- [ ] **Goal:** Replace manual `if b { TrueValue } else { FalseValue }` patterns
- [ ] **Scope:** Audit all primitive implementations

---

### Tokenization Error Handling
- [ ] **Issue:** Error handling in tokenizer is difficult to understand
- [ ] **Location:** `go/tokenizer/tokenizer.go`, `go/tokenizer/error.go`
- [ ] **Goal:** Improve error propagation and messages
- [ ] **Note:** Consider structured error types with line/column info

---

### Inf/NaN Handling Consistency
- [ ] **Issue:** Some places use `math.Inf(1)/math.Inf(-1)`, others use predefined constants
- [ ] **Location:** `go/tokenizer/tokenizer.go` number parsing
- [ ] **Goal:** Standardize on single approach (prefer constants for clarity)

---

### Error Handling Consolidation
- [ ] **Issue:** Unimplemented features have scattered error handling
- [ ] **Location:** `go/machine/` compiler code
- [ ] **Goal:** Consolidate into common error types or helper functions

---

### syntax->datum and datum->syntax Consolidation
- [ ] **Location:** `go/registry/core/prim_syntax.go`
- [ ] **Current State:** `datumToSyntax()` helper exists; `PrimSyntaxToDatum` uses `stx.UnwrapAll()`
- [ ] **Goal:** Verify no duplicate recursive unwrap/wrap logic elsewhere
- [ ] **Files to check:** `go/machine/compile_quasisyntax.go`

---

### Compiler Registry Refactoring

These items propose extracting compiler dispatch into registry patterns (like `PrimitiveCompiler`):

- [ ] Refactor compiler primitive handling to use PrimitiveCompiler registry (~300 lines saved)
- [ ] Refactor compiler literal handling to use LiteralCompiler registry
- [ ] Refactor compiler special form handling to use SpecialFormCompiler registry
- [ ] Refactor compiler expansion handling to use Expander registry
- [ ] Refactor compiler optimization handling to use Optimizer registry
- [ ] Refactor compiler evaluation handling to use Evaluator registry
- [ ] Refactor ExpandPrimitiveForm into PrimitiveExpander registry

**Location:** `go/machine/compile_*.go`
**Pattern:** See existing `go/registry/` for registry architecture

---

### Validation Code Size
- [ ] **Location:** `go/validate/` directory
- [ ] **Current State:** 15 files, already well-decomposed by form type
- [ ] **Goal:** Review if further decomposition or method extraction is beneficial

---

### evalWhenCompileForRuntime Simplification
- [ ] **Location:** `go/machine/compile_eval_when.go`
- [ ] **Issue:** Two-pass algorithm (collect all expressions, then iterate)
- [ ] **Goal:** Consider single-pass algorithm that expands/compiles in the ForEach callback
- [ ] **Challenge:** Determining "last" expression requires knowing total count

---

### ForEach "Must" Wrapper
- [ ] **Goal:** Create wrapper that panics on improper list instead of returning error
- [ ] **Benefit:** Simplify code where proper list is guaranteed (e.g., after validation)
- [ ] **Location:** `go/values/pair.go`

---

### compile_eval_when.go Phase Table
- [ ] **Location:** `go/machine/compile_eval_when.go`
- [ ] **Issue:** Hardcoded list of phases
- [ ] **Goal:** Move to table-driven approach or use phase constants from `go/environment/`

---

### Table-Driven Tests
- [ ] **Goal:** Convert remaining test functions to table-driven format
- [ ] **Pattern:** Use `runSchemeCode(t, code)` helper with `[]struct{name, code, expected}`
- [ ] **Scope:** Audit `go/registry/core/*_test.go` files

---

### Datum Wrapping/Unwrapping
- [ ] **Issue:** Some code wraps/unwraps `Datum` types laboriously
- [ ] **Goal:** Keep datum wrapping/unwrapping at function edges; work with values internally
- [ ] **Location:** Review `go/machine/` compiler code

---

### Tokenizer Warnings
- [ ] **Issue:** Unreachable code and unhandled errors in tokenizer
- [ ] **Location:** `go/tokenizer/tokenizer.go`
- [ ] **Goal:** Fix warnings, ensure error handling is complete

---

### Tokenizer Refactoring
- [ ] `readRadixPrefix` — consolidate `#b/#o/#d/#x` handling
- [ ] `readBooleanLiteral` — consolidate `#t/#true` and `#f/#false`
- [ ] `scanKeyword` — unify `scan()`, `scanCaseInsensitive()`, `readToken()`
- [ ] `readDecimalFractionWithExponent` — extract decimal+exponent pattern
- [ ] `readImaginarySuffix` — consolidate imaginary number suffixes
- [ ] `readExplicitSignNumber` — consolidate `+/-` number handling
- [ ] `advanceOrError` — combine `next()` + error check
- [ ] `checkDelimiter` — replace inline delimiter checking
- [ ] `readInfNan` — consolidate `inf.0/nan.0` parsing

**Estimated reduction:** ~200-300 lines

---

### formName Processing
- [ ] **Issue:** Form name should come from car of original form, but is being set on an object
- [ ] **Location:** `go/machine/` compiler code
- [ ] **Goal:** Allow dynamic setting of form-name for abstract forms (parameter lists, literals)

---

### Environment Function Refactoring
- [ ] **Location:** `go/environment/`
- [ ] **Goal:** Reduce code redundancy in environment manipulation functions

---

### Token Type Renaming
- [ ] **Issue:** Comment token types may have unclear names
- [ ] **Location:** `go/tokenizer/token.go`
- [ ] **Goal:** Use clearer names that distinguish line/block/datum comments

---

### Number Parsing Cleanup
- [ ] **Location:** `go/tokenizer/tokenizer.go`
- [ ] **Issue:** Number parsing is messy with many code paths
- [ ] **Goal:** Reduce redundancy, improve clarity
- [ ] **Consider:** Evaluate removing "signed" token types (represent sign separately)

---

### parseComplex Refactoring
- [ ] **Location:** `go/tokenizer/tokenizer.go` or `go/parser/`
- [ ] **Issue:** Complex number parsing is messy
- [ ] **Goal:** Reduce redundancy, clearer error handling

---

### Test Consolidation
- [ ] **Issue:** Many test files have identical test runner code
- [ ] **Goal:** Combine into single table-driven tests where possible
- [ ] **Pattern:** Share `runSchemeCode` helper, consolidate similar test categories

---

Primitive Unit Tests
--------------------

**Status:** Many primitives have dedicated test files (89+ test files exist). Many items originally listed here are now covered by consolidated test files (e.g., `prim_port_extra_test.go`, `prim_read_extra_test.go`, `prim_write_extra_test.go`, `prim_numeric_extra_test.go`).

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

### Remaining Untested Primitives

#### Control Flow
- [ ] `call/cc` - expand existing tests for edge cases (no dedicated test file)

#### Expansion/Compilation
- [ ] `expand` - test expand primitive
- [ ] `expand-once` - test expand-once primitive
- [ ] `compile` - test compile primitive

#### Process/Environment
- [ ] `exit` - test exit primitive
- [ ] `emergency-exit` - test emergency-exit primitive

#### Miscellaneous
- [ ] `prim_utils_test.go` - test utility primitives

---

Code Refactoring
----------------
- [ ] Add `go/registry/helpers/args.go` - helper functions for argument extraction (~600 lines saved)
- [x] ~~Add `go/machine/operation_helpers.go`~~ - EqualTo helper functions (exists)
- [ ] Migrate ~27 operation files to use EqualTo helpers

---

R7RS Missing Features
---------------------

### Tokenizer (R7RS 7.1.1 Lexical Structure)

**Extended Symbols (`|...|`):**
- [x] Basic parsing, escape sequences, and verification complete

---

### Primitives

**BigInteger:**
- [x] Automatic promotion from Integer on overflow (resolved in `312cf48`)
- [ ] No `#bigint` reader syntax

**BigFloat:**
- [ ] No automatic promotion from Float
- [ ] No `#bigfloat` reader syntax
- [ ] BigFloat type exists in `go/values/big_float.go`
- [ ] **Effort:** Medium - similar to BigInteger

---

Library Status
--------------

| Library               | Status | Notes |
|-----------------------|--------|-------|
| scheme/base           | ~98%   | |
| scheme/char           | 100%   | |
| scheme/file           | 100%   | |
| scheme/write          | 100%   | |
| scheme/r5rs           | 100%   | |
| scheme/complex        | 100%   | |
| scheme/cxr            | 100%   | |
| scheme/read           | 100%   | |
| scheme/inexact        | 100%   | |
| scheme/process-context| 100%   | |
| scheme/time           | 100%   | |
| scheme/lazy           | 100%   | |
| scheme/load           | 100%   | |
| scheme/repl           | 100%   | |
| scheme/case-lambda    | 100%   | |
| scheme/eval           | 100%   | |
| chibi/test            | 100%   | Minimal stub implementation (not full chibi library) |

---

Dead Code Removal
-----------------

### Phase 5 (Optional): Test-Only Functions
- [ ] Consider deleting `AddScopeToSet()` and `RemoveScopeFromSet()` in `go/syntax/scope_utils.go`
- [ ] These are only used in `go/syntax/coverage_test.go`
- [ ] Main code uses different scope manipulation methods

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

---

### Multithreading

SRFI-18 standard API + Go-native extensions (channels, sync primitives). Remaining:
- [ ] call/cc and dynamic-wind integration with threads (call/cc scope limited to single thread, dynamic-wind cleanup on thread termination)

---

### Programmatic Tokenization and Parsing

Expose tokenizer and parser to Scheme code for building custom readers, REPLs, and tooling.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Token introspection (token?, token-type, token-value, etc.) | Not started |
| 2 | Syntax introspection (syntax?, syntax-line, syntax-column, etc.) | Not started |
| 3 | EOF handling improvements | Not started |
| 4 | Advanced reader control (optional) | Not started |

---

### POSIX API (SRFI-170)

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

---

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

---

### Arbitrary Precision Numbers
- [ ] Tagged literals: `#bigint`, `#bigfloat` using Go's `big.Int` and `big.Float`.

---

### Go FFI
- [ ] Registry-based (Phase 1) → Reflection-based (Phase 2) → Plugin support (Phase 3).

---

### Runtime Source Location Tracking

Track variable definition sites and enable source-level debugging at runtime.

Infrastructure is partially complete (binding source locations, source maps, stack traces). Remaining work:
- [ ] Wire up compilation to record source locations in source map
- [ ] Wire up error handling to use `SchemeError` with `CaptureStackTrace()`
- [ ] Create debugger REPL or IDE integration (e.g., Debug Adapter Protocol)

---

Reflection
----------
- [ ] Procedures for reflection into the environment:
  - List of bound symbol names
  - Parameters for procedures (arity, names if available)
  - Types and predicates for types
- [ ] **Location:** Would require new primitives in `go/registry/core/`

---

Event Callbacks
---------------
- [ ] Variables to hold event callback methods for:
  - Expansion events (before/after macro expansion)
  - Compilation events (before/after compilation)
  - Runtime debugging (variable set/get for debugging)
- [ ] **Use case:** IDE integration, debugging, profiling
- [ ] **Pattern:** Similar to dynamic-wind but for compiler/expander phases

---

1.0 Release Blockers
--------------------

Items that must be resolved before a 1.0 release.

### R7RS Conformance

### User-Facing

(No remaining items — REPL startup message already outputs to stderr.)
