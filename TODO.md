TODO
----

Code Quality
------------

### Public API Value Constructors

- [x] Add tests for `wile.NewRational`, `wile.NewComplex`, `wile.NewVector`, and the other numeric constructors (`NewBigInteger*`, `NewBigFloat*`) to verify round-tripping through the `wrapValue`/`unwrapValue` layer
- [x] `NewVector` allocates a temporary `[]values.Value` to unwrap arguments — acceptable for expected use cases

---

Future Extensions
-----------------

### Go FFI

- [ ] Phase 3: Plugin support (dynamic extension loading via registry pattern)

---

### Runtime Source Location Tracking

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
