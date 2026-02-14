TODO
----

**Last Updated**: 2026-02-13

### Current Project Status

**Core Language**: R7RS-small complete with hygienic macros, continuations, numeric tower
**Extensions**: 9 extension packages (io, files, math, system, threads, eval, exceptions, gointerop, all)
**Examples**: 75 examples across 13 categories, Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test infrastructure exists but needs content
**Libraries**: (chibi test), (chibi optional), (srfi 1) complete

### Summary

Items are ordered by priority: P1 (core adoption blockers), P2 (growth enablers), P3 (advanced use cases), P4 (future/nice-to-have), P5 (internal refactoring).

| Priority | Item | Category | Status | Notes |
|----------|------|----------|--------|-------|
| **P1** | **External extensions** | **Architecture** | **Proposed** | **Make extension system publicly consumable in separate repos. `plans/EXTERNAL_EXTENSIONS_PLAN.md`** |
| **P1** | **Load-path stack** | **Feature** | **Planned** | **Relative path resolution for `load`, `include`, `import`** |
| P2 | Performance refactoring (8 phases) | Performance | Planned | Full-pipeline optimization: parse → expand → compile → execute. `plans/PERFORMANCE_REFACTORING_PLAN.md` |
| P2 | Go FFI Phase 3 — Plugin support | Embedding | Not started | Dynamic extension loading via registry |
| P3 | Authorization Framework (6 phases) | Security | Not started | K8s-style verb+resource for sandboxing untrusted code. `plans/AUTHORIZATION_FRAMEWORK.md` |
| P3 | Hygiene debugging | Tooling | Planned | Scope introspection tooling for macro hygiene. `plans/HYGIENE_DEBUGGING_DESIGN.md` |
| P3 | Macro expansion tracing | Tooling | Planned | Trace generated code back to macro invocation/template. `plans/MACRO_EXPANSION_TRACING.md` |
| P3 | Plugin architecture | Architecture | Complete | Phases 1-3, 5-6 done. Phase 4 (external extraction) → `plans/EXTERNAL_EXTENSIONS_PLAN.md` |
| P3 | Unit testing library | Standard library | ⚠️ Partial | `(chibi test)` exists, infrastructure in `test/`, needs content |
| P3 | Programmatic tokenization/parsing | Tooling | Not started | Expose tokenizer/parser to Scheme code |
| P3 | TopLevelEnvironment | Architecture | Reference | Per-VM symbol interning |
| P4 | Network libraries | Standard library | Not started | TCP/UDP, HTTP, TLS, DNS |
| P4 | POSIX API / SRFI-170 (10 phases) | Standard library | Not started | Comprehensive OS access |
| P4 | Logging library | Standard library | Not started | Levels, structured output, handlers |
| P4 | Debugger / DAP integration | Tooling | Not started | Debug Adapter Protocol |
| P4 | Reflection primitives | Runtime | Not started | Expose bound symbols, arity, types to Scheme |
| P4 | Event callbacks | Tooling | Not started | Hooks for expansion, compilation, debugging |
| P4 | Feature flags (3-tier) | Runtime | Not started | Compile-time, runtime global, extension-defined |
| P4 | Scribble syntax (@-expressions) | Syntax | Not started | Racket-style text processing |
| P5 | Tokenizer consolidation | Refactoring | Planned | Consolidate ~700 lines of number parsing repetition. `plans/TOKENIZER_CONSOLIDATION_PLAN.md` |
| P5 | EmptyList/Void refactoring | Refactoring | Complete | EmptyList is `emptyListType{}`, SyntaxEmptyList is `syntaxEmptyListType{}` |
| P5 | Subsystem simplification | Refactoring | Reference | 8 simplification opportunities across core packages |
| P5 | Operation boilerplate reduction | Refactoring | Reference | ~36 operation files with identical boilerplate. See `plans/ARCHITECTURAL_REVIEW_REFACTORING.md` §2.2 |
| P5 | Hashtable redesign | Performance | Not started | Replace bucket chaining with native Go map |
| P5 | Systematic debug logging | Tooling | Reference | Debugging methodology doc. `plans/SYSTEMATIC_DEBUG_LOGGING.md` |

---

Future Extensions
-----------------

### Go FFI (P2)

- [ ] Phase 3: Plugin support (dynamic extension loading via registry pattern)

---

### Authorization Framework (P3)

Fine-grained access control for embedded engines running untrusted code. Kubernetes-style verb+resource model with a single `Authorizer` interface method, extensible by extensions without interface changes.

**Note**: This addresses a speculative use case. No current users have requested sandboxing. Prioritize after achieving broader adoption (10+ active users embedding Wile).

See `plans/AUTHORIZATION_FRAMEWORK.md` for full design.

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | `security/` package — interface, context propagation, constants | Not started |
| 2 | Built-in authorizers (FilesystemRoot, ReadOnly, DenyAll, Composite) | Not started |
| 3 | Engine integration (`WithAuthorizer` option, ctx wrapping) | Not started |
| 4 | Gate runtime primitives (files, system, eval extensions) | Not started |
| 5 | Gate compile-time code loading (include, library import) | Not started |
| 6 | Integration tests | Not started |

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

**Unit Testing Library** — ⚠️ Partial

**What Exists**:
- ✅ `(chibi test)` framework in `lib/chibi/test.scm`
- ✅ Test infrastructure: `test/` directory with runner scripts
- ✅ Automated test discovery (`*-test.scm` files)
- ✅ CI integration (`make test-scheme`)
- ✅ Cross-implementation testing support

**What's Missing**:
- [ ] Comprehensive test content (only 1 smoke test exists)
- [ ] Regression test files (`test/regression/`)
- [ ] Library-specific tests (`lib/*/test/`)
- [ ] Full coverage of R7RS features

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
