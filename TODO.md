TODO
----

Code Cleanup
------------
### FreeIdResolution
- [x] Try and find a more specific type for the interface, instead of 'any'.
- [x] **Location:** `internal/match/syntax_adapter.go` and `machine/compile_syntax_rules.go`
- [x] **Resolution:** Replaced `any` with `*environment.GlobalIndex` in `FreeIdResolution.Global` and `globalBindingProvider.GetGlobal()`, and `*environment.Binding` in `BindingChecker.GetBinding()` and `envBindingChecker.GetBinding()`. Import is safe: `match` → `environment` (no cycle). `SyntaxSymbol.ResolvedBinding any` stays `any` to avoid `syntax` ↔ `environment` cycle.

### Scope Matching Optimization
- [ ] **Location:** `internal/syntax/` and `internal/match/`
- [ ] **Issue:** Scope set matching is mostly brute force O(n×m) comparison
- [ ] **Goal:** Investigate optimization opportunities (hash-based set comparison, scope indexing)
- [ ] **Impact:** Performance improvement for complex macro expansions

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
- [ ] **Location:** `internal/schemeutil/predicate.go` provides `BoolToBoolean(bool) *Boolean`
- [ ] **Goal:** Replace manual `if b { TrueValue } else { FalseValue }` patterns
- [ ] **Status:** 72 uses adopted across 20 files; ~22 manual patterns remain (compound conditionals and non-predicate sites)
- [ ] **Remaining files:** `prim_gointerop.go` (7), `prim_threads.go` (2), `prim_exceptions.go` (2), `prim_all.go` (1)

---

### ForEach "Must" Wrapper
- [ ] **Goal:** Create wrapper that panics on improper list instead of returning error
- [ ] **Benefit:** Simplify code where proper list is guaranteed (e.g., after validation)
- [ ] **Location:** `values/pair.go`

---

### compile_eval_when.go Phase Table
- [ ] **Location:** `machine/compile_eval_when.go`
- [ ] **Issue:** Hardcoded list of phases
- [ ] **Goal:** Move to table-driven approach or use phase constants from `environment/`

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
- [ ] Add `registry/helpers/args.go` - helper functions for argument extraction (~600 lines saved)
- [x] ~~Add `machine/operation_helpers.go`~~ - EqualTo helper functions (exists)
- [ ] Migrate ~7 remaining operation files to use EqualTo helpers (23/30 already migrated)

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
- [ ] call/cc and dynamic-wind integration with threads
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
