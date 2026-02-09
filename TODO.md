TODO
----

Code Quality
------------

### Sentinel Value Types

- [ ] Consider using distinct types for EmptyList and Void instead of sentinel values in `SyntaxPair` (`internal/syntax/syntax_pair.go`) and `ArrayList` (`values/array_list.go`). Both packages use the same pattern of sentinel comparison; typed singletons would make the distinction compile-time checkable.

### ArrayList Special-Case Logic

- [ ] Clean up `ArrayList.Cons` / `Append` logic (`values/array_list.go:~90`). The method has multiple special-case branches for empty lists, void values, and single-element lists that are difficult to follow.
- [ ] Clean up `ArrayList.IsList` (`values/array_list.go:~138`). Same issue — multiple branches checking length, void, and EmptyList sentinels.

### ByteVector Overflow Handling

- [x] Integer-to-byte conversion paths such as `NewByteVectorFromIntegers` and the `#u8(...)` parser (`values/byte_vector.go`) silently truncate values that overflow `uint8`. Add explicit overflow checks or document the truncation semantics.

### Environment Naming

- [x] `LocalEnvironmentFrame.CreateLocalBinding` (`environment/local_environment_frame.go`) is actually a "get-or-create" (returns existing binding if key already exists). Rename to `EnsureLocalBinding` or `GetOrCreateLocalBinding` to match its semantics.

### MachineContext.Apply

- [ ] Add unit tests for `MachineContext.Apply` (`machine/machine_context.go`).
- [ ] Make `MachineContext.Apply` symmetric with `MachineClosure` apply dispatch — currently the two paths have different calling conventions.
- [ ] `MachineContext.Apply` accepts variadic parameters but has no mechanism for returning multiple values.

### CompileTimeContinuation Environment

- [ ] `CompileTimeContinuation.env` (`machine/compile_time_continuation.go`) stores full environment bindings, but only the binding keys are needed at compile time. Replace with a key-only data structure to reduce memory during compilation.

### Test Improvements

- [ ] `operation_test.go:~212` — `OperationApply` test reports `pc=0` because the test does not set up a real function call. Improve test to use a real closure invocation so the PC reflects actual behavior.
- [ ] `operation_test.go:~240` — `OperationRestoreContinuation` test shows `CallDepth()=0` because no real call is active. Same improvement needed.
- [ ] `compile_time_continuation_test.go:~509` — `mc.Run()` should return `ErrMachineHalt` but currently does not. Investigate and fix the assertion.

---

Future Extensions
-----------------

### Go FFI

- [x] **Phase 1: `RegisterFunc`** — Register Go functions with natural signatures via reflection. Supports `int64`, `int`, `float64`, `string`, `bool`, `[]byte`, `Value`, `context.Context`, variadic params, and `(T, error)` returns. Reflection happens once at registration; runtime uses pre-computed converters. PR #139.
- [x] **Phase 2: Composite types** — `[]T` ↔ proper Scheme lists, `map[K]V` ↔ hashtables, structs ↔ alists, `func(A) B` ← Scheme procedures as Go callbacks via `reflect.MakeFunc`. Recursive converters built at registration time. Parameters callable as 0-arg (get) or 1-arg (set) callbacks. Map key types restricted to string/int64/int/bool (float64 excluded: NaN breaks lookup invariants). PR #140.
- [ ] Phase 3: Plugin support (dynamic extension loading via registry pattern)

---

### Runtime Source Location Tracking

- [x] **Per-operation source tracking** (2026-02-08) — Per-operation source attribution via `sourceRefs []uint16` indexing into deduplicated `sourceTable`. O(1) lookup. PR #137.
- [x] **Error handling integration** (2026-02-08) — `ErrExceptionEscape` carries `Source` and `StackTrace`. Public API: `EvalWithSource`, `CompileWithSource`. PR #138.
- [ ] Create debugger REPL or IDE integration (e.g., Debug Adapter Protocol)

---

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

---

### Hashtable: Replace Bucket Chaining with Native Go Map

- [ ] **Location:** `values/hashtable.go`
- [ ] **Problem:** `Hashtable` re-implements a hash map on top of `map[uint64][]hashtableEntry` — Go's map already does bucket chaining, resizing, and amortized O(1) lookup internally.
- [ ] **Options:**
  1. Typed maps for common cases (`map[int64]Value`, `map[string]Value`) with fallback
  2. Accept current design — ~50 lines, handles arbitrary `Hashable` keys correctly
- [ ] **Measurement:** Profile actual workloads before committing to a redesign.

---

### Reflection

- [ ] Procedures for reflection into the environment:
  - List of bound symbol names
  - Parameters for procedures (arity, names if available)
  - Types and predicates for types
- [ ] **Location:** Would require new primitives in `registry/core/`

---

### Event Callbacks

- [ ] Variables to hold event callback methods for:
  - Expansion events (before/after macro expansion)
  - Compilation events (before/after compilation)
  - Runtime debugging (variable set/get for debugging)
- [ ] **Use case:** IDE integration, debugging, profiling
- [ ] **Pattern:** Similar to dynamic-wind but for compiler/expander phases
