TODO
----

Code Cleanup
------------
### FreeIdResolution
- [x] Try and find a more specific type for the interface, instead of 'any'.
- [x] **Location:** `internal/match/syntax_adapter.go` and `machine/compile_syntax_rules.go`
- [x] **Resolution:** Replaced `any` with `*environment.GlobalIndex` in `FreeIdResolution.Global` and `globalBindingProvider.GetGlobal()`, and `*environment.Binding` in `BindingChecker.GetBinding()` and `envBindingChecker.GetBinding()`. Import is safe: `match` → `environment` (no cycle). `SyntaxSymbol.ResolvedBinding any` stays `any` to avoid `syntax` ↔ `environment` cycle.

### Scope Matching Optimization
- [x] **Status:** CLOSED (2026-02-07) — Linear scan is optimal for practical scope set sizes
- [x] **Investigation:** Scope sets are typically 0-4 elements (one per lexical form). Hash maps cross over at ~20-30 elements; bitmaps require unbounded IDs; sorted merge adds O(n) insertion cost. Linear scan with pointer equality in a cache line is faster for these sizes.
- [x] **Changes:** Added size guard (`len(binding) > len(use)` early return), cached `Scopes()` calls in `GetBindingWithScopes`, added perfect-match early termination in `GetLocalIndexWithScopes`, documented rationale in `ScopesMatch`.

---

### Numeric Type Unification
- [x] **Status:** CLOSED (2026-02-05) — Direct dispatch is the intentional architecture
- [x] **Resolution:** Tower* functions were deleted. Each type's direct dispatch methods handle all 49 type combinations correctly. The duplicate logic is acceptable because:
  1. Direct dispatch correctly handles exact complex numbers (Tower* had a bug)
  2. Each case is explicit and testable
  3. The ~600 lines of switch-case code is the correct, battle-tested implementation
- [x] **See:** `plans/NUMERIC_TOWER_REFACTOR_COMPLETE.md` for full rationale

---

### Use values.Tuple Instead of *values.Pair
- [x] **Goal:** Where code processes list-like data internally, prefer `values.Tuple` interface over concrete `*values.Pair`
- [x] **Benefit:** Allows `ArrayList` and `Vector` to be used interchangeably
- [x] **Scope:** Audit compiler and primitive implementations
- [x] **Status:** Replaced `*values.Pair` with `values.Tuple` in list-processing primitives (prim_lists.go, helpers/list.go, helpers/numeric.go, helpers/char.go, machine/operations.go). Deleted dead code in internal/schemeutil/collections.go.

### Replace Direct Pair Index Access with Accessor Methods
- [x] **Goal:** Convert all direct `[0]`/`[1]` index access on `*Pair` to `Car()`/`Cdr()`/`SetCar()`/`SetCdr()`, and `&Pair{x, y}` literals to `NewCons(x, y)`
- [x] **Benefit:** Encapsulates Pair representation; only `pair.go` methods use raw indexing
- [x] **Scope:** values/, machine/, internal/match/ (pair.go's own method implementations excluded)
- [x] **Status:** All production code outside pair.go now uses accessor methods and NewCons

---

### Use values.Number Interface
- [x] **Status:** CLOSED (2026-02-07) — Already using `values.Number` at all major call sites
- [x] **Resolution:** All numeric fold/comparison helpers, predicates, and math extensions already use `values.Number`. Remaining type switches on individual numeric types are legitimately type-specific (extracting Go native values, R7RS eqv? semantics, overflow detection). Simplified `MaybeToInexact` to use `n.ToInexact()` instead of a manual type switch.

---

### Use values.Indexable Interface
- [x] **Status:** CLOSED (2026-02-07) — Interface exists but has no applicable call sites
- [x] **Resolution:** The `Indexable` interface is defined and implemented by `Vector`, `String`, and `ByteVector` with compile-time assertions. However, R7RS mandates separate ref/set primitives per type (`vector-ref`, `string-ref`, `bytevector-u8-ref`) with type-specific validation and error messages. No code type-switches across all three doing the same operation. The `Mappable` interface referenced in the original TODO does not exist. The interface remains available for future generic consumers.

---

### Use BoolToBoolean Helper
- [x] **Location:** `internal/schemeutil/predicate.go` provides `BoolToBoolean(bool) *Boolean`
- [x] **Goal:** Replace manual `if b { TrueValue } else { FalseValue }` patterns
- [x] **Status:** All manual patterns replaced with `BoolToBoolean` across the codebase
- [x] **Completed:** All remaining sites in `prim_gointerop.go` (7), `prim_threads.go` (2), `prim_exceptions.go` (2), `prim_all.go` (1)

---

### ForEach "Must" Wrapper
- [x] **Goal:** Create wrapper that panics on improper list instead of returning error
- [x] **Benefit:** Simplify code where proper list is guaranteed (e.g., after validation)
- [x] **Location:** `values/pair.go`

---

### compile_eval_when.go Phase Table
- [x] **Location:** `machine/compile_eval_when.go`
- [x] **Issue:** Hardcoded list of phases
- [x] **Goal:** Move to table-driven approach or use phase constants from `environment/`
- [x] **Status:** Replaced switch/map[string]bool with evalWhenBehavior bit flags and evalWhenPhaseTable

---

### Tokenizer Refactoring
- [x] `readRadixPrefix` — already consolidated: single `readRadixMarker()` function with 4 call sites
- [x] `readBooleanLiteral` — already consolidated: single `readBoolean()` function with 2 call sites
- [x] `scanKeyword` — already consolidated: `scanWith()` dispatcher + `scan()`/`scanCaseInsensitive()` wrappers
- [x] `readDecimalFractionWithExponent` — eliminated 17-line duplicate in `mayReadUnsignedFractionalRealNumberOrRationalRealNumber`
- [x] `readImaginarySuffix` — CLOSED: 12 `isImaginary()` call sites each have distinct surrounding logic (different states, error handling, return patterns); no common extractable block
- [x] `readExplicitSignNumber` — CLOSED: sign dispatch has divergent flows (imaginary, nan, digit, dot, subsequent) that don't share structure
- [x] `advanceOrError` — CLOSED: 23 bare-return sites × 1 line saved each = ~19 net lines; not worth the churn on a 2360-line file
- [x] `checkDelimiter` — already consolidated: `isDelimiter()` and `isDelimiterOrMarker()` helpers exist
- [x] `readInfNan` — already consolidated: `readSpecialNumber()` with strategy callback + `readNan()`/`readInf()` wrappers

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

### Core Primitives (Complete)

- [x] `registry/core/` — 15 source files, 107 test files, full coverage

### Previously Untested (Now Complete)

- [x] `call/cc` - extensively tested in `prim_control_test.go` (1000+ lines) and `prim_dynamic_wind_test.go`
- [x] `expand`, `expand-once`, `compile` - tested in `internal/extensions/eval/prim_eval_test.go`
- [x] `exit`, `emergency-exit` - tested via subprocess in `internal/extensions/system/prim_system_test.go`
- [x] All system primitives - tested in `internal/extensions/system/prim_system_test.go`

### Extension Primitives (Remaining)

~130 primitives across 7 files in `internal/extensions/` lack test coverage.

#### Characters — `internal/extensions/all/prim_characters.go` (207 lines, 14 primitives)
- [x] `char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?` — case-insensitive comparison via `unicode.ToLower`
- [x] `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`, `char-lower-case?` — Unicode classification predicates
- [x] `char-upcase`, `char-downcase`, `char-foldcase` — case mapping (foldcase has special ẞ→ß handling)
- [x] `digit-value` — Unicode decimal digit value (0-9) or #f; supports non-ASCII digit scripts
- [x] **Tested in `prim_characters_test.go`** — 59 test cases including Unicode scripts (Arabic-Indic, Devanagari), variadic comparison, error conditions

#### Strings — `internal/extensions/all/prim_strings.go` (423 lines, 12 primitives)
- [x] `string-ci=?`, `string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?` — case-insensitive comparison (ci=? uses `strings.EqualFold`, others use `strings.ToLower`)
- [x] `string-upcase`, `string-downcase`, `string-foldcase` — full Unicode case mapping via `x/text/cases` (can change string length)
- [x] `string-copy!` — mutable copy with optional start/end (3-5 args)
- [x] `string-fill!` — fill range with character (2-4 args)
- [x] `string-map`, `string-for-each` — higher-order string operations (multi-string, min-length semantics)
- [x] **Tested in `prim_strings_test.go`** — 68 test cases including mutative ops, higher-order with lambdas, multi-string min-length, error conditions

#### Records & Promises — `internal/extensions/all/prim_all.go` (418 lines, 12 primitives)
- [x] `make-record-type`, `record-type?`, `record?`, `record-type` — SRFI-9 record type construction and predicates
- [x] `record-constructor`, `record-predicate`, `record-accessor`, `record-modifier` — record closure factories
- [x] `promise?`, `make-promise`, `force`, `%make-lazy-promise` — R7RS promise semantics with memoization
- [x] **Tested in `prim_all_test.go`** — 72 test cases including low-level primitives, define-record-type macro integration, partial constructors, field ordering, type mismatch errors, R7RS promise memoization semantics, delay-force recursive iteration

#### Ports — `internal/extensions/io/prim_ports.go` (261 lines, 16 primitives)
- [x] `port?`, `input-port?`, `output-port?`, `textual-port?`, `binary-port?` — port type predicates
- [x] `input-port-open?`, `output-port-open?`, `close-port` — port lifecycle
- [x] `eof-object`, `eof-object?` — EOF sentinel
- [x] `call-with-port` — port with automatic close
- [x] `open-input-string`, `open-output-string`, `get-output-string` — string ports
- [x] `open-input-bytevector`, `open-output-bytevector`, `get-output-bytevector` — bytevector ports
- [x] **Tested in `prim_ports_test.go`** — 86 test cases including port predicates, textual/binary classification, lifecycle (open/close), EOF handling, string port roundtrip, bytevector port roundtrip, call-with-port auto-close, error conditions

#### Math — `internal/extensions/math/prim_math.go` (1497 lines, 30 primitives)
- [x] `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt`, `expt`, `square` — transcendental functions
- [x] `floor`, `ceiling`, `truncate`, `round` — rounding (exact→exact, inexact→inexact)
- [x] `floor/`, `floor-quotient`, `floor-remainder`, `truncate/`, `truncate-quotient`, `truncate-remainder` — integer division
- [x] `finite?`, `infinite?`, `nan?` — numeric predicates
- [x] `numerator`, `denominator`, `rationalize`, `exact-integer-sqrt` — rational/exact operations
- [x] `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle` — complex number operations
- [x] `number->string`, `string->number` — numeric conversion with radix support
- [x] **Tested in `prim_math_test.go`** — 164 test cases: transcendental functions with tolerance checks, banker's rounding, floor/truncate division with multi-value returns, numeric predicates on special values, rationalize via Stern-Brocot, exact-integer-sqrt, complex construction/decomposition, number↔string conversion with radix and exactness prefixes, error conditions

#### Go Interop — `internal/extensions/gointerop/prim_gointerop.go` (536 lines, 28 primitives)
- [x] `make-channel`, `channel?`, `channel-send!`, `channel-receive`, `channel-try-send!`, `channel-try-receive`, `channel-close!`, `channel-closed?`, `channel-length`, `channel-capacity` — Go channel primitives
- [x] `make-wait-group`, `wait-group?`, `wait-group-add!`, `wait-group-done!`, `wait-group-wait!` — sync.WaitGroup
- [x] `make-rw-mutex`, `rw-mutex?`, `rw-mutex-read-lock!`, `rw-mutex-read-unlock!`, `rw-mutex-write-lock!`, `rw-mutex-write-unlock!`, `rw-mutex-try-read-lock!`, `rw-mutex-try-write-lock!` — sync.RWMutex
- [x] `make-once`, `once?`, `once-do!`, `once-done?` — sync.Once
- [x] `make-atomic`, `atomic?`, `atomic-load`, `atomic-store!`, `atomic-swap!`, `atomic-compare-and-swap!` — atomic operations
- [x] **Tested in `prim_gointerop_test.go`** — 76 test cases: buffered channel send/receive round-trips with FIFO ordering, non-blocking try-send/try-receive with 3-value return via call-with-values, channel close semantics and error conditions, WaitGroup add/done/wait lifecycle, RWMutex lock contention (try-write-lock fails while read-locked and vice versa), Once single-execution guarantee with side-effect verification, AtomicBox load/store/swap/CAS with pointer-identity semantics, type predicate coverage for all 5 value types, 27 error cases for type mismatches and invalid operations

#### Threads — `internal/extensions/threads/prim_threads.go` (637 lines, 22 primitives)
- [x] `current-thread`, `thread?`, `make-thread`, `thread-name`, `thread-specific`, `thread-specific-set!` — thread identity/metadata
- [x] `thread-start!`, `thread-yield!`, `thread-sleep!`, `thread-terminate!`, `thread-join!` — thread lifecycle
- [x] `mutex?`, `make-mutex`, `mutex-name`, `mutex-specific`, `mutex-specific-set!`, `mutex-state`, `mutex-lock!`, `mutex-unlock!` — SRFI-18 mutexes
- [x] `condition-variable?`, `make-condition-variable`, `condition-variable-name`, `condition-variable-specific`, `condition-variable-specific-set!`, `condition-variable-signal!`, `condition-variable-broadcast!` — condition variables
- [x] `current-time`, `time?`, `time->seconds`, `seconds->time` — SRFI-18 time
- [x] **Tested in `prim_threads_test.go`** — 72 test cases: thread creation with named/auto-generated names, thread-specific storage round-trips, full start/join lifecycle returning computed values across goroutine boundary, thread-sleep with zero/float/time-object, mutex lock/unlock/relock cycles with state verification via symbol->string (NewSymbol not interned), condition variable creation and no-op signal/broadcast, time round-trip preservation within tolerance, 23 error cases for type mismatches and double-start

---

Test Coverage Enforcement
-------------------------

### covercheck Threshold (80%)
- [x] **`runtime` package** — was 0.0%, now 95.3%. Added `runtime/runtime_test.go` with tests for `Compile`, `Run`, and `Load` (success, empty input, multiple expressions, compile error, runtime error).
- [x] **`values` package** — was 77.5%, now 83.2%. Added `values/numeric_methods_coverage_test.go` with tests for uncovered numeric predicates (`IsInteger`, `IsRational`, `IsFinite`, `IsNaN`), comparison (`Compare` across all type pairs), sign/abs/negate, exactness conversions (`ToExact`, `ToInexact`), `HashCode`, and numeric tower utilities (`Simplify`, `ExactnessOf`, `ResultExactness`).
- [x] All 14 non-excluded packages now pass `make covercheck` (80% threshold).

---

Code Refactoring
----------------
- [x] ~~Add `registry/helpers/args.go`~~ - `RequireArg[T]` and `RequireType[T]` generics replace ~190 type assertion sites across 20+ prim files
- [x] ~~Add `machine/operation_helpers.go`~~ - EqualTo helper functions (exists)
- [x] ~~Migrate ~7 remaining operation files to use EqualTo helpers~~ (all 30 migrated)
- [x] ~~Add per-type numeric conversion helpers~~ - `bigInt()`, `bigFloat()`, `bigRat()`, `toComplex()`, `toBigComplex()`, `float64Val()` eliminate ~80 duplicated conversion expressions in `values/`

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

SRFI-18 standard API + Go-native extensions (channels, sync primitives). Remaining:
- [x] call/cc and dynamic-wind integration with threads
  - Add thread identity to `MachineContext`; reject cross-thread continuation invocation in `Restore`/`ErrContinuationEscape` handling
  - Run `UnwindTo(0)` on thread termination (normal exit and `thread-terminate!`) to fire dynamic-wind after thunks

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
- [ ] **Location:** Would require new primitives in `registry/core/`

---

Event Callbacks
---------------
- [ ] Variables to hold event callback methods for:
  - Expansion events (before/after macro expansion)
  - Compilation events (before/after compilation)
  - Runtime debugging (variable set/get for debugging)
- [ ] **Use case:** IDE integration, debugging, profiling
- [ ] **Pattern:** Similar to dynamic-wind but for compiler/expander phases
