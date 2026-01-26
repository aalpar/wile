TODO
----

Code Maintenance
----------------

### Partial List Handling Audit

**Goal:** Ensure all functions that accept pairs or lists handle improper lists (lists not ending in `EmptyList`) consistently per R7RS.

**Current State:** Inconsistent handling across ~30 list primitives:
- `Pair.ForEach()` returns improper tail as second return value (correct)
- `Pair.Len()` and `Pair.AsVector()` panic with `ErrNotAList` (correct)
- `PrimAppend()` returns `ErrNotAPair` for non-list (correct)
- Some primitives don't validate improper list ends explicitly

**Required Action:**
- [ ] Audit all list primitives in `go/registry/core/prim_lists.go` and `prim_pairs.go`
- [ ] Each primitive should either:
  - Accept improper lists and handle them per R7RS semantics, OR
  - Return `ErrNotAPair` or `ErrNotAList` errors appropriately
- [ ] Document expected behavior for each primitive

**Files:** `go/registry/core/prim_lists.go`, `go/registry/core/prim_pairs.go`, `go/values/pair.go`

---

Code Cleanup
------------

### Match Package SyntaxValue Usage
**Status:** ✅ DESIGN IS CORRECT - No cleanup needed

The match package intentionally uses a three-layer architecture:
1. **Layer 1 (Core):** `Matcher` operates on raw `values.Value` types
2. **Layer 2 (Adapter):** `SyntaxMatcher` converts between `syntax.SyntaxValue` and `values.Value`
3. **Layer 3 (Hygiene):** Scope addition happens AFTER expansion in `valueToSyntaxWithOrigin()`

This separation is documented and intentional per Flatt 2016 model.

---

### Scope Matching Optimization
- [ ] **Location:** `go/syntax/` and `go/match/`
- [ ] **Issue:** Scope set matching is mostly brute force O(n×m) comparison
- [ ] **Goal:** Investigate optimization opportunities (hash-based set comparison, scope indexing)
- [ ] **Impact:** Performance improvement for complex macro expansions

---

### Phase System
**Status:** ✅ ALREADY SIMPLIFIED

The phase system has been modernized:
- `PhaseRegistry` uses O(1) indexed map-based access with `GetOrCreate(phase int)`
- Thread-safe via `sync.RWMutex`
- Constants: `PhaseTemplate = -1`, `PhaseRuntime = 0`, `PhaseExpand = 1`, `PhaseCompile = 2`
- Convenience methods: `env.AtPhase(n)`, `env.Runtime()`, `env.Expand()`, `env.Compile()`

**Remaining cleanup:**
- [ ] Remove unused `MetaFrame` type in `go/environment/meta_frame.go` (only 27 lines, appears abandoned)

---

### begin-for-syntax
**Status:** ✅ FULLY IMPLEMENTED

Implementation in `go/machine/compile_begin_for_syntax.go`:
- Handles `(begin-for-syntax expr ...)` for compile-time evaluation
- Uses expand-time environment for expansion
- Creates temporary template, compiles each expression, executes at compile time
- Returns `nil` - no runtime code emitted

---

### Quasiquote Expansion
**Status:** ✅ CLEAN DESIGN

`go/machine/compile_quasisyntax.go` (180 lines) shows well-structured design:
- Proper depth tracking for nested quasisyntax
- Clean separation: entry point → depth management → template transformation
- Sophisticated list handling with unsyntax-splicing segmentation

**Potential opportunity:**
- [ ] Consolidate duplicate logic between quasiquote and quasisyntax implementations (different files)

---

### Let-syntax and Letrec-syntax
**Status:** ✅ IMPLEMENTED

Both are handled at compile time in the machine/expander, not bootstrap macros.
- `let-syntax`: Creates local syntax bindings scoped to body
- `letrec-syntax`: Creates mutually-recursive local syntax bindings

No simplification needed.

---

### Number Comparison Methods
**Status:** ✅ COMPLETE

All numeric types implement `Compare(o Number) int`:
- `Integer.Compare()`, `Float.Compare()`, `Rational.Compare()`, `Complex.Compare()`
- `BigInteger.Compare()`, `BigFloat.Compare()`
- Cross-type comparison semantics with type promotion

Method naming uses `Compare` consistently (not `CompareTo`).

---

### Remove Extraneous Number Methods
- [ ] **Location:** `go/values/integer.go`, `float.go`, `rational.go`, `complex.go`, `big_*.go`
- [ ] **Goal:** Audit number types for unused or redundant methods
- [ ] **Note:** All number types already share common `Number` interface; may be minimal redundancy

---

### Numeric Type Unification
- [ ] **Issue:** Integer, Float, Rational, Complex, BigInteger, BigFloat have some duplicate comparison logic
- [ ] **Location:** `go/values/numeric_tower.go` provides unified dispatch but per-type methods still exist
- [ ] **Goal:** Ensure all cross-type operations go through `TowerAdd`, `TowerSubtract`, `TowerCompare`, etc.
- [ ] **Files:** All `go/values/*_number.go` files

---

### Auxiliary Syntax Exports
- [ ] **Issue:** R7RS requires `(scheme base)` to export `else`, `=>`, `...`, `_` as auxiliary syntax
- [ ] **Current State:** These are registered as compile-time bindings in `specialforms.go` (lines 44-51)
- [ ] **Problem:** Export mechanism expects runtime `values.Value` objects; these are pattern literals
- [ ] **Required:** Implement auxiliary syntax binding mechanism that allows library exports of non-value bindings
- [ ] **Note:** Pattern matching for `else` and `=>` in `cond`/`case` works correctly; only export is broken

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
- [ ] **Location:** `go/utils/bool.go` provides `BoolToBoolean(bool) *Boolean`
- [ ] **Goal:** Replace manual `if b { TrueValue } else { FalseValue }` patterns
- [ ] **Scope:** Audit all primitive implementations

---

### Tokenization Error Handling
- [ ] **Issue:** Error handling in tokenizer is difficult to understand
- [ ] **Location:** `go/tokenizer/tokenizer.go`, `error.go`
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
- [ ] **Current State:** `datumToSyntax()` helper exists (lines 91-120); `PrimSyntaxToDatum` uses `stx.UnwrapAll()`
- [ ] **Goal:** Verify no duplicate recursive unwrap/wrap logic elsewhere
- [ ] **Files to check:** `go/match/syntax_adapter.go`, `go/machine/compile_quasisyntax.go`

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
- [ ] **Current State:** 14 files, already well-decomposed by form type
- [ ] **Note:** Original item mentioned "compile_validate.go" which doesn't exist; validation is in separate package
- [ ] **Goal:** Review if further decomposition or method extraction is beneficial

---

### evalWhenCompileForRuntime Simplification
- [ ] **Location:** `go/machine/compile_eval_when.go` lines 205-263
- [ ] **Issue:** Two-pass algorithm (collect all expressions, then iterate)
- [ ] **Current Flow:**
  1. Collect ALL expressions via `SyntaxForEach` (lines 221-232)
  2. Loop through collected slice with index tracking (lines 235-260)
  3. For each: expand → compile with tail-position context → pop if not last
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

### Vardec Functions
**Status:** ❓ NOT FOUND

Search found no "vardec" functions in the codebase. Either:
- Already removed in a previous commit
- Refers to something renamed
- Outdated TODO item

**Action:** Close or clarify what "vardec" referred to.

---

### Tokenizer Warnings
- [ ] **Issue:** Unreachable code and unhandled errors in tokenizer
- [ ] **Location:** `go/tokenizer/tokenizer.go` (lines mentioned: 1641, 1664)
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

### Scheme Header Output
- [ ] **Issue:** "Program running, send SIGQUIT (Ctrl+\\) to dump stacks." outputs to stdout
- [ ] **Goal:** Output to stderr, add `--quiet` option
- [ ] **Location:** `go/cmd/main.go` REPL startup

---

### Environment Creation
- [ ] **Issue:** "( environment )" creation needs fixup
- [ ] **Context:** R7RS environment primitive
- [ ] **Location:** `go/registry/core/` or `go/extensions/eval/`

---

### Eval Optional Environment
- [ ] **Issue:** `eval` should accept 1 or 2 arguments (second is optional environment)
- [ ] **R7RS:** `(eval expression [environment])` where environment defaults to interaction-environment
- [ ] **Location:** `go/extensions/eval/prim_eval.go`

---

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

---

Code Refactoring (see REFACTORING_PROPOSAL.md)
----------------------------------------------
- [x] Migrate primitives from `runtime/primitives/` to `registry/core/` and `extensions/*/`
- [x] Move primitive tests to `registry/core/`
- [ ] Add `go/registry/helpers/args.go` - helper functions for argument extraction (~600 lines saved)
- [ ] Add `go/machine/operation_helpers.go` - EqualTo helper functions (~300 lines saved)
- [ ] Migrate ~27 operation files to use EqualTo helpers

---

R7RS Missing Features
---------------------

### Tokenizer (R7RS 7.1.1 Lexical Structure)

**Extended Symbols (`|...|`):**
- [x] **Basic parsing:** `readExtendedSymbol()` exists (tokenizer.go:1920-1944)
- [x] **Escape sequences:** Calls `readIntraExtendedToken()` → `readEscapeSequence('|')`
- [ ] **Verification needed:** Confirm all R7RS escape sequences supported (`\a`, `\b`, `\t`, `\n`, `\r`, `\|`, `\\`, `\x<hex>;`)

**Hex Escape Semicolon Terminator:**
- [x] **IMPLEMENTED:** `readHexEscapeToken()` at line 599-617 validates semicolon terminator
- [x] Error message: `MessageExpectingHexSequenceTerminator`

**String Line Continuation:**
- [x] **IMPLEMENTED:** `skipLineContinuation()` at lines 698-715
- [x] Skips intraline whitespace after backslash
- [x] Calls `scanLineEnding()` then skips more whitespace

**Scientific Notation in Library Files:**
- [ ] **Issue:** Numbers like `1e-10` fail to parse in `.sld` files
- [ ] **Error:** "strconv.ParseInt: parsing '1e-10': invalid syntax"
- [ ] **Note:** Works in REPL; issue may be in library loading, not tokenizer

**Exponent Markers:**
- [ ] Only `e`/`E` supported
- [ ] R7RS also allows `s`, `f`, `d`, `l` for short/single/double/long precision hints

**Inexact Digit Placeholder:**
- [ ] R7RS allows `#` in inexact numbers (e.g., `1.2###`)
- [ ] Not implemented

---

### Syntax/Macros

**Core Macros Status:**
- [x] `case` macro (R7RS §4.2.1): IMPLEMENTED in bootstrap.go lines 109-131
- [x] `letrec*` macro (R7RS §4.2.2): IMPLEMENTED in bootstrap.go lines 79-82
- [x] `let-syntax` / `letrec-syntax` (R7RS §4.3.1): IMPLEMENTED in machine/expander
- [x] `syntax-error` (R7RS §4.3.1): REGISTERED in specialforms.go line 43
- [x] `define-values` (R7RS §5.3.3): IMPLEMENTED in bootstrap.go lines 257-268

---

### Macro/Library System

**Library-Internal Binding Hygiene:**
- [ ] **Issue:** Macros defined in a library that reference helper functions defined in the same library fail at use site with "no such binding"
- [ ] **Root Cause:** Hygiene model doesn't preserve library bindings for macro-introduced identifiers
- [ ] **Workaround:** Export helper functions with `%` prefix
- [ ] **Complexity:** High - requires changes to hygiene model to track library boundaries
- [ ] **Related:** `FreeIdResolution` struct in `compile_syntax_rules.go` tracks global vs local binding resolution

---

### Primitives

**Box primitives:**
- [ ] `box`, `box?`, `unbox`, `set-box!`
- [ ] Box type exists in `go/values/box.go` but no Scheme primitives registered
- [ ] **Effort:** Low - just need to register existing type

**Hashtable primitives:**
- [ ] `make-hashtable`, `hashtable?`, `hashtable-ref`, `hashtable-set!`, `hashtable-delete!`
- [ ] `hashtable-keys`, `hashtable-values`, `hashtable-size`, `hashtable-copy`, `hashtable-clear!`
- [ ] Hashtable type exists in `go/values/hashtable.go`
- [ ] **Issue:** Current implementation only supports string keys, not arbitrary Scheme values
- [ ] **Effort:** Medium - need key hashing for Scheme values

**BigInteger:**
- [ ] No automatic promotion from Integer when overflow
- [ ] No `#bigint` reader syntax
- [ ] BigInteger type exists in `go/values/big_integer.go`
- [ ] **Effort:** Medium - need overflow detection and promotion logic

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
| scheme/base           | ~95%   | Missing: auxiliary syntax export mechanism |
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

### Phase 6 (Optional): Fix Method Signature
- [x] `NativeError.Unwrap()` already returns `error` (line 127 of `go/values/native_error.go`)
- [x] Correctly implements Go's `errors.Unwrap` interface

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

---

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

---

### Records (SRFI-9)

Record types for user-defined data structures. SRFI-9 is the de facto standard for R7RS-small implementations.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | `define-record-type` macro | Complete (in bootstrap.go) |
| 2 | Constructor, predicate, accessor generation | Complete |
| 3 | Mutator (setter) generation | Complete |
| 4 | Integration with `equal?` and `write` | Complete |

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

---

### Programmatic Tokenization and Parsing

See **DESIGN_PROGRAMMATIC_READER.md** for full implementation plan.

Expose tokenizer and parser to Scheme code for building custom readers, REPLs, and tooling.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Token introspection (token?, token-type, token-value, etc.) | Not started |
| 2 | Syntax introspection (syntax?, syntax-line, syntax-column, etc.) | Not started |
| 3 | EOF handling improvements | Not started |
| 4 | Advanced reader control (optional) | Not started |

---

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
See detailed design notes in DESIGN_GO_FFI.md.

---

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
