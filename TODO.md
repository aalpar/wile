TODO
----

**Last Updated**: 2026-03-02

### Current Project Status

**Version**: v1.4.0 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower
**Extensions**: 9 extension packages — 6 public (files, math, system, threads, exceptions, gointerop), 3 internal (io, eval, all); all importable as R7RS `(wile <name>)` libraries
**Examples**: 73 examples across 12 categories, 21 Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test suite: 3,187 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 915-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete

### Summary

Items are ordered by priority: P1 (core adoption blockers), P2 (growth enablers), P3 (advanced use cases), P4 (future/nice-to-have), P5 (internal refactoring).

| Priority | Item | Category | Status | Notes |
|----------|------|----------|--------|-------|
| P1 | Stack.Drain — eliminate PopAll allocation in VM hot path | Performance | Not started | `PopAll` allocates a fresh `[]values.Value` on every function call (`OpApply`, `OpPullApply`). `Apply` only iterates the slice then discards it. Add a `Drain`/view method to let `Apply` read the stack backing array in-place, then clear. ~20 LOC, two call sites. |
| P1 | Fused push opcodes | Performance | Not started | `PushLiteral`, `PushGlobal`, `PushLocal` — combine load+push into single opcodes to reduce dispatch overhead. Peephole optimizer emits fused ops. |
| P2 | ArrayList — array-backed list representation | Performance | Not started | Contiguous `[]Value` slice alternative to `*Pair` chains. O(1) element access, better cache locality. Implements `Value` and `Tuple`. Prototype existed in abandoned branch (~358 LOC + 538 test). |
| P2 | Numeric dispatch simplification | Performance | Not started | Replace `NumericKind` enum + `init()` dispatch tables with direct type switches in each numeric method. Deletes indirection layer (~-1400 net lines). Same behavior, fewer allocations. |
| P2 | Opcode resource limits | Security | Design | Per-category limits for match/expand/continuation copy. Completes defense-in-depth for embedded use. `plans/SECURITY.md` |
| P3 | ER macro transformer | Macro system | Not started | Unlocks Chibi library ecosystem. Matters after Go-side adoption creates demand for Scheme library porting. `plans/MACRO_SYSTEM.md` |
| P3 | Module decomposition Phase 1 | Architecture | Not started | Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md` |
| P3 | Network libraries | Standard library | Not started | TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases. |
| P3 | Debugger / DAP integration | Tooling | Not started | Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md` |
| P3 | Performance remaining tiers | Performance | Deferred | Rest-arg cons elimination (#1 allocator at 39.9%), sync.Pool overhead evaluation. `plans/PERFORMANCE.md` |
| P4 | Hygiene debugging | Tooling | Planned | Scope introspection for macro authors. `plans/MACRO_SYSTEM.md` |
| P4 | Macro expansion tracing | Tooling | Planned | Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md` |
| P4 | Dialect system | Architecture | Proposed | De-globalize forms registry, `WithDialect()` option, extract R7RS as default dialect. `plans/ARCHITECTURE.md` |
| P4 | Plugin shadowing | Architecture | Proposed | Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md` |
| P4 | Go FFI Phase 3 — Plugin support | Embedding | Not started | Dynamic extension loading via registry |
| P4 | Programmatic tokenization/parsing | Tooling | Not started | Expose tokenizer/parser to Scheme code |
| P4 | Reflection primitives | Runtime | Not started | Expose bound symbols, arity, types to Scheme |
| P4 | Continuation marks | Runtime | Not started | Racket-style stack annotation. Prompt infrastructure exists; needs per-frame key→value map |
| P4 | POSIX API / SRFI-170 (10 phases) | Standard library | Not started | Comprehensive OS access |
| P4 | Logging library | Standard library | Not started | Levels, structured output, handlers |
| P4 | Event callbacks | Tooling | Not started | Hooks for expansion, compilation, debugging |
| P5 | Duplicated import set parsing | Tech debt | Not started | `compile_time_continuation_library.go` vs `import_set_datum.go` — same R7RS §5.7 logic on different types. Bug-fix divergence risk. |
| P5 | Split `compile_time_continuation_library.go` | Tech debt | Not started | 1,213 lines, 6+ concerns. Split into library forms, import parsing, letrec* body. |
| P5 | Split `library.go` registry/bindings | Tech debt | Not started | 456 lines mixing registry ops and three-env auxiliary syntax lookup. |
| P5 | Unify escape mechanisms | Tech debt | Not started | `ErrExitEscape` and `ErrPromptAbort` — parallel tagged-boundary paths. |
| P5 | Fix Tuple ForEach nil semantics | Tech debt | Not started | Nil `*Pair.ForEach` returns `Void` not `EmptyList`. Latent correctness bug. |
| P5 | Port type Value method boilerplate | Tech debt | Not started | 10+ port files duplicate `IsVoid`/`EqualTo`/`SchemeString`. |
| P5 | Constructor telescoping in `match` | Tech debt | Not started | 8 constructor variants → options struct. |
| P5 | I/O port extraction helper | Tech debt | Not started | 4 hand-unrolled port extraction functions → generic `extractPort[T]`. |
| P5 | `forms` type erasure docs | Tech debt | Not started | `any` signatures undocumented; add contract doc comments. |
| P5 | Tokenizer test consolidation | Tech debt | Not started | 14 test files named by coverage goals → behavior-oriented files. |
| P5 | REPL deprecated wrappers | Tech debt | Not started | 3 deprecated delegations in `repl.go`. Delete if unused. |
| P5 | Rename `AddSearchPath` | Tech debt | Not started | Prepends but name suggests append. |
| P5 | Unify library/include paths | Tech debt | Not started | Inconsistent path resolution between `include` and library loading. |
| P5 | Error sentinel grouping | Tech debt | Not started | ~120 sentinels in flat list. Reorganize if count grows. |
| P5 | Fused lexing/parsing | Research | Research | Flap paper analysis. Actionable only after profiling confirms tokenizer is a bottleneck. `plans/PERFORMANCE.md` |
| P5 | Feature flags (3-tier) | Runtime | Not started | Compile-time, runtime global, extension-defined. No demand signal yet. |
| P5 | Scribble syntax (@-expressions) | Syntax | Not started | Racket-style text processing. No demand signal yet. |
| P5 | Hashtable redesign | Performance | Not started | Replace bucket chaining with native Go map |

---

Future Extensions
-----------------

### Go FFI (P4)

- [ ] Phase 3: Plugin support (dynamic extension loading via registry pattern)

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

**Unit Testing**:
- [ ] Regression test files (`test/regression/`)
- [ ] Library-specific tests (`lib/*/test/`)
- [ ] New test cases for features not covered by Go test extraction

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

### Tech Debt (from [plans/TECH_DEBT_REVIEW.md](plans/TECH_DEBT_REVIEW.md))

- [ ] **F10: MachineContext decomposition** [Medium, Postponed]: 1586 lines, 71 methods, 10+ responsibilities. Extract `WindingStack`, `ContinuationChain`, `ExceptionHandler` into delegate types. Postponed — requires stable method surface; do after other refactorings settle.
- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.

### Tech Debt (from `private/*_TECH_DEBT_ASSESSMENT.md`)

**machine/ — High:**
- [ ] **Duplicated import set parsing** [High, M]: `parseImportSet*` (8 functions on `syntax.SyntaxValue`) in `compile_time_continuation_library.go` mirrors `parseImportSet*FromDatum` (7 functions on `values.Tuple`) in `import_set_datum.go`. Same R7RS §5.7 modifier semantics, two implementations. Bug-fix divergence risk. Fix: common `Datum` interface both types satisfy.
- [ ] **Split `compile_time_continuation_library.go`** [High, M]: 1,213 lines doing 6+ concerns (library forms, import parsing, export, cond-expand, letrec* body, phase-aware bindings). Split into `compile_library_forms.go`, `compile_import.go`, keep letrec* body in original.

**machine/ — Medium:**
- [ ] **Split `library.go` into registry + bindings** [Medium, S]: 456 lines mixing data structures, registry ops, export application, and three-environment auxiliary syntax lookup. Split into `library_registry.go` and `library_bindings.go`.
- [ ] **Unify escape mechanisms** [Medium, M]: `ErrExitEscape` + `ExitTag` (`exit_escape.go`) and `ErrPromptAbort` + `PromptTag` (`prompt_abort.go`) are parallel paths for tagged-boundary return. Barrier/winding bug fixes must cover both. Evaluate whether `call-with-exit` can wrap the prompt abort mechanism.

**machine/ — Low:**
- [ ] **Rename `AddSearchPath` to `PrependSearchPath`** [Low, S]: `library.go:164` — prepends but name suggests append.
- [ ] **Unify library/include path resolution** [Low, S]: `DefaultLibraryPaths` hardcodes `[".", "./lib"]`; `SCHEME_INCLUDE_PATH` only used by `include`/`include-ci`, not library loading. Inconsistent.

**values/ — Medium:**
- [ ] **Fix Tuple ForEach nil semantics** [Medium, S]: `pair.go:184` — nil `*Pair.ForEach` returns `Void`, not `EmptyList`. Violates interface contract ("returns the tail value (EmptyList for proper lists)"). Pin behavior with a test.
- [ ] **Port type Value method boilerplate** [Medium, S]: 10+ port files duplicate `IsVoid`/`EqualTo`/`SchemeString`. `portBase` provides `Close`/`IsClosed` but not Value interface methods. Add `SchemeString`/`IsVoid` to `portBase`; `EqualTo` requires concrete assertions.

**internal/ — Medium:**
- [ ] **`forms` type erasure documentation** [Medium, S]: `ValidatorFunc`/`CompilerFunc` in `internal/forms/form_spec.go` use `any` to break circular imports. Deliberate choice but contract is undocumented. Add explicit doc comment on `FormSpec` specifying the concrete types each `any` parameter must satisfy.
- [ ] **Constructor telescoping in `match`** [Medium, S]: 8 constructor variants (`NewSyntaxMatcher*` × 4, `CompileSyntaxPattern*` × 4) in `syntax_adapter.go`. Collapse to `NewSyntaxMatcher(config)` with a `SyntaxMatcherConfig` options struct.
- [ ] **I/O port extraction helper** [Medium, S]: 4 functions in `internal/extensions/io/prim_read_write.go` (`getOptionalOutputPort`, `getOptionalInputPort`, `getRequiredBinaryInputPort`, `getRequiredBinaryOutputPort`) follow the same 5-step pattern, varying only in port type and error sentinel. Extract generic `extractPort[T]` parameterized on interface and sentinel.

**internal/ — Low:**
- [ ] **Tokenizer test file consolidation** [Low, M]: 14 test files, several named by coverage goals (`additional_coverage_test.go`, `final_coverage_test.go`). Consolidate into behavior-oriented files (`tokenizer_number_test.go`, `tokenizer_string_test.go`, etc.).
- [ ] **REPL deprecated wrappers** [Low, S]: `internal/repl/repl.go:370-393` — `Compile`, `Run`, `Load` marked `Deprecated`, delegate to runtime package. Delete if no callers remain.

**werr/ — Low:**
- [ ] **Error sentinel grouping** [Low, S]: ~120 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.

### Small Refactorings (P5)
- [ ] Validator prologue deduplication: 19 validators in `internal/validate/validate_*.go` repeat the same `collectList` + `improper` check + arity guard prologue (~4 lines each). Extract to `validateFormPrologue()` helper.
- [ ] Optional fill argument extraction: 3 `make-*` primitives (`PrimMakeVector`, `PrimMakeBytevector`, `PrimMakeString`) independently extract optional fill arguments with slightly different patterns. Share a helper.

### R7RS Conformance Remaining Items

All findings from `plans/R7RS-CONFORMANCE-REVIEW.md` resolved (PRs #364–#368). Remaining:

- [ ] **Shared acyclic datum labels**: `internSymbolsInValueWithVisited` uses a permanent visited set, incorrectly rejecting shared-but-acyclic structures like `'(#0=(a) #0#)`. Fix: stack-based or two-state (inProgress/done) visited map. `deduplicatePairWithVisited` needs the same fix if shared DAGs are allowed through.
- [ ] **guard body drops multiple values**: `(let ((result (begin e1 e2 ...))) ...)` captures only the first return value. R7RS §7.3's own reference implementation has the same limitation. Worth fixing but low priority.
- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned
