TODO
----

**Last Updated**: 2026-03-03

### Current Project Status

**Version**: v1.4.0 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower
**Extensions**: 9 extension packages — 6 public (files, math, system, threads, exceptions, gointerop), 3 internal (io, eval, all); all importable as R7RS `(wile <name>)` libraries
**Examples**: 73 examples across 12 categories, 21 Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test suite: 3,187 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 915-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Performance

- [ ] **Numeric dispatch simplification**: Replace `NumericKind` enum + `init()` dispatch tables with direct type switches in each numeric method. Deletes indirection layer (~-1400 net lines). Same behavior, fewer allocations.
- [ ] **ArrayList — array-backed list representation**: Contiguous `[]Value` slice alternative to `*Pair` chains. O(1) element access, better cache locality. Implements `Value` and `Tuple`. Prototype existed in abandoned branch (~358 LOC + 538 test).
- [ ] **Rest-arg cons elimination and sync.Pool evaluation**: #1 allocator at 39.9%. `plans/PERFORMANCE.md`

---

## Refactoring

Ordered by dependency — items that unblock others or carry divergence risk come first. MachineContext decomposition is last (depends on other refactorings settling).

### High Priority

- [x] **Split `compile_time_continuation_library.go`** [High, M]: Split into `compile_library_forms.go`, `compile_import.go`, `compile_cond_expand.go`, `compile_define_syntax.go`; letrec* body remains in original.

### Medium Priority

- [ ] **Split `library.go` into registry + bindings** [Medium, S]: 494 lines mixing data structures, registry ops, export application, and three-environment auxiliary syntax lookup. Split into `library_registry.go` and `library_bindings.go`.
- [ ] **Unify escape mechanisms** [Medium, M]: `ErrExitEscape` + `ExitTag` (`exit_escape.go`) and `ErrPromptAbort` + `PromptTag` (`prompt_abort.go`) are parallel paths for tagged-boundary return. Barrier/winding bug fixes must cover both. Evaluate whether `call-with-exit` can wrap the prompt abort mechanism.
- [ ] **Port type Value method boilerplate** [Medium, S]: 10+ port files duplicate `IsVoid`/`EqualTo`/`SchemeString`. `portBase` provides `Close`/`IsClosed` but not Value interface methods. Add `SchemeString`/`IsVoid` to `portBase`; `EqualTo` requires concrete assertions.
- [ ] **`forms` type erasure documentation** [Medium, S]: `ValidatorFunc`/`CompilerFunc` in `internal/forms/form_spec.go` use `any` to break circular imports. Deliberate choice but contract is undocumented. Add explicit doc comment on `FormSpec` specifying the concrete types each `any` parameter must satisfy.
- [ ] **Constructor telescoping in `match`** [Medium, S]: 8 constructor variants (`NewSyntaxMatcher*` × 4, `CompileSyntaxPattern*` × 4) in `syntax_adapter.go`. Collapse to `NewSyntaxMatcher(config)` with a `SyntaxMatcherConfig` options struct.
- [ ] **I/O port extraction helper** [Medium, S]: 4 functions in `internal/extensions/io/prim_read_write.go` (`getOptionalOutputPort`, `getOptionalInputPort`, `getRequiredBinaryInputPort`, `getRequiredBinaryOutputPort`) follow the same 5-step pattern, varying only in port type and error sentinel. Extract generic `extractPort[T]` parameterized on interface and sentinel.
- [ ] **Optional fill argument extraction**: 3 `make-*` primitives (`PrimMakeVector`, `PrimMakeBytevector`, `PrimMakeString`) independently extract optional fill arguments with slightly different patterns. Share a helper.

### Low Priority

- [ ] **Rename `AddSearchPath` to `PrependSearchPath`** [Low, S]: `library.go:164` — prepends but name suggests append.
- [ ] **Unify library/include path resolution** [Low, S]: `DefaultLibraryPaths` hardcodes `[".", "./lib"]`; `SCHEME_INCLUDE_PATH` only used by `include`/`include-ci`, not library loading. Inconsistent.
- [ ] **Tokenizer test file consolidation** [Low, M]: 14 test files, several named by coverage goals (`additional_coverage_test.go`, `final_coverage_test.go`). Consolidate into behavior-oriented files (`tokenizer_number_test.go`, `tokenizer_string_test.go`, etc.).
- [ ] **REPL deprecated wrappers** [Low, S]: `internal/repl/repl.go:370-393` — `Compile`, `Run`, `Load` marked `Deprecated`, delegate to runtime package. Delete if no callers remain.
- [ ] **Error sentinel grouping** [Low, S]: ~120 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.

### Postponed

- [ ] **F10: MachineContext decomposition** [Medium, Postponed]: 1671 lines, 71 methods, 10+ responsibilities. Extract `WindingStack`, `ContinuationChain`, `ExceptionHandler` into delegate types. Postponed — requires stable method surface; do after other refactorings settle.
- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.

---

## Features

- [ ] **Opcode resource limits** [Security, Design]: Per-category limits for match/expand/continuation copy. Completes defense-in-depth for embedded use. `plans/SECURITY.md`
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
- [ ] **ER macro transformer** [Macro system]: Unlocks Chibi library ecosystem. Matters after Go-side adoption creates demand for Scheme library porting. `plans/MACRO_SYSTEM.md`
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md`
- [ ] **POSIX API / SRFI-170** [Standard library, 10 phases]: Comprehensive OS access — stat, permissions, links, temp files, env vars, subprocess, signals, user/group, terminal, error handling.
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.

---

## Nice-to-Haves

No demand signal. Speculative or research-only.

- [ ] **Hygiene debugging** [Tooling, Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Tooling, Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Dialect system** [Architecture, Proposed]: De-globalize forms registry, `WithDialect()` option, extract R7RS as default dialect. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Architecture, Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Programmatic tokenization/parsing** [Tooling]: Expose tokenizer/parser to Scheme code. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Reflection primitives** [Runtime]: Expose bound symbols, arity, types to Scheme. New primitives in `registry/core/`.
- [ ] **Continuation marks** [Runtime]: Racket-style stack annotation. Prompt infrastructure exists; needs per-frame key→value map.
- [ ] **Logging library** [Standard library]: Levels, structured output, handlers.
- [ ] **Event callbacks** [Tooling]: Hooks for expansion, compilation, debugging. IDE integration, profiling.
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined. No demand signal yet.
- [ ] **Scribble syntax (@-expressions)** [Syntax]: Racket-style text processing. No demand signal yet.
- [ ] **Hashtable redesign** [Performance]: Replace bucket chaining with native Go map. Profile before committing.
- [ ] **Fused lexing/parsing** [Research]: Flap paper analysis. Actionable only after profiling confirms tokenizer is a bottleneck. `plans/PERFORMANCE.md`
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`lib/*/test/`), new test cases for features not covered by Go test extraction.
