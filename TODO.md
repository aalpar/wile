TODO
----

Code Quality
------------

### Sentinel Value Types

- [x] Consider using distinct types for EmptyList and Void instead of sentinel values in `SyntaxPair` (`internal/syntax/syntax_pair.go`) and `ArrayList` (`values/array_list.go`). Already done: `emptyListType` and `voidType` are distinct types. Stale `== EmptyList` comparisons cleaned up.

### ArrayList Special-Case Logic

- [x] Clean up `ArrayList.Cons` / `Append` logic (`values/array_list.go`). Normalized `ArrayListEmptyList` from `[nil, nil]` to `[EmptyList]`. Replaced manual encoding checks in `AppendList` with `IsEmptyList()` helper.
- [x] Clean up `ArrayList.IsList` (`values/array_list.go`). Removed redundant two-nil encoding branches. `IsList` and `IsEmptyList` now have single clear checks.

### ByteVector Overflow Handling

- [x] Integer-to-byte conversion paths such as `NewByteVectorFromIntegers` and the `#u8(...)` parser (`values/byte_vector.go`) silently truncate values that overflow `uint8`. Add explicit overflow checks or document the truncation semantics.

### Environment Naming

- [x] `LocalEnvironmentFrame.CreateLocalBinding` (`environment/local_environment_frame.go`) is actually a "get-or-create" (returns existing binding if key already exists). Rename to `EnsureLocalBinding` or `GetOrCreateLocalBinding` to match its semantics.

### MachineContext.Apply

- [x] Add unit tests for `MachineContext.Apply` (`machine/machine_context.go`).
- [x] Make `MachineContext.Apply` symmetric with `MachineClosure` apply dispatch. Asymmetry is by design: closures run in VM loop (bytecode handles continuation restoration), while Parameters and ComposableContinuation return immediately via `returnImmediate()`.
- [x] `MachineContext.Apply` accepts variadic parameters but has no mechanism for returning multiple values. Mechanism exists: `Apply` sets up the call, `Run()` executes it, `GetValues()` retrieves multiple return values.

### CompileTimeContinuation Environment

- [x] `CompileTimeContinuation.env` (`machine/compile_time_continuation.go`) stores full environment bindings. Full env is justified: symbol resolution, macro detection, scope chain, and compile-phase bindings all require the complete environment. Splitting to key-only adds complexity without benefit.

### Test Improvements

- [x] `operation_test.go:~212` — `OperationApply` test reports `pc=0` because the test does not set up a real function call. Improve test to use a real closure invocation so the PC reflects actual behavior.
- [x] `operation_test.go:~240` — `OperationRestoreContinuation` test shows `CallDepth()=0` because no real call is active. Same improvement needed.
- [x] `compile_time_continuation_test.go:~509` — `mc.Run()` should return `ErrMachineHalt` but currently does not. Investigate and fix the assertion.

### Indexable Method Duplication

Investigated all items. Most are already resolved, semantically different, or too trivial to abstract.

**Extracted:**
- [x] `Vector.SchemeString()` / `ByteVector.SchemeString()` — extracted `formatIndexable()` helper in `values/utils.go`.

**Closed (no action):**
- [x] `Vector.AsList()` / `ByteVector.AsList()` — already simplified in SUBSYSTEM_SIMPLIFICATION Phase 2; both delegate to `List()`.
- [x] `Vector.EqualTo()` / `ByteVector.EqualTo()` — semantic difference: Vector uses recursive `EqualTo()`, ByteVector compares `uint8` directly. Cannot safely unify.
- [x] `Vector.Get()` / `ByteVector.Get()` — trivial one-liner (`return (*p)[i]`), no extraction benefit.
- [x] `IsVoid()` across types — single-line `return p == nil`; abstracting adds indirection for zero cognitive benefit.
- [x] `EqualTo()` preambles — type-specific assertions prevent clean unification.
- [x] `ForEach()` Pair vs ArrayList — different data structures (linked list vs array).

**Locations:** `values/utils.go`, `values/vector.go`, `values/byte_vector.go`

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

---

### Feature Flags

Three-tier feature flag system for controlling Wile behavior at different lifecycle stages.

**Tiers:**

| Tier | Set When | Mutability | Mechanism |
|------|----------|------------|-----------|
| **Compile-time** | Go build (`-tags`, `-ldflags`) | Immutable after build | Build tags + `const` via linker |
| **Runtime global** | Go initialization (`Engine` config) | Mutable from Go at any point during runtime | Go-side flag registry |
| **Extension-defined** | Extension registration | Same as runtime global | Extensions add flags via registry pattern |

**Compile-time flags** — set via Go build tags or `-ldflags`. These control code inclusion (dead code elimination) and cannot change after the binary is built. Examples: disable macro expander for minimal embed, strip debug support, select GC strategy.

**Runtime global flags** — configured during Wile initialization from Go. Mutable at any point during the Go program's lifetime. These control runtime behavior without recompilation. Examples: enable/disable tail-call optimization, set recursion depth limits, toggle debug tracing.

**Extension-defined flags** — extensions register their own flags through the same runtime registry. This lets third-party extensions participate in the feature flag system without modifying core Wile. The extension interface exposes flag registration alongside primitive registration.

**Design requirements:**
- [ ] Flag registry with typed values (bool, int, string)
- [ ] Compile-time flags via build tags and linker-injected constants
- [ ] Runtime flag registry queryable from both Go and Scheme
- [ ] Extension interface for registering custom flags (`AddFeatureFlag` on `Registry`)
- [ ] Scheme-side introspection: `(feature-flag? name)`, `(feature-flags)` to list active flags
- [ ] Thread-safe reads/writes for runtime flags (concurrent Scheme goroutines)
- [ ] Immutability enforcement: compile-time flags reject mutation attempts
- [ ] Integration with R7RS `cond-expand` for feature-based conditional compilation in Scheme
