TODO
----

**Last Updated**: 2026-03-09

### Current Project Status

**Version**: v1.5.0 (released)
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
- [x] **`guard` body drops multiple values** [Medium, S]: Fixed in #395 — body now uses `call-with-values` + `list` capture.
- [x] **Tuple ForEach nil returns Void instead of EmptyList** [Medium, S]: Fixed in #394 — nil guards and loop exits now return `EmptyList`/`SyntaxEmptyList`.

---

## Refactoring

Ordered by dependency — items that unblock others or carry divergence risk come first. MachineContext decomposition is last (depends on other refactorings settling).

### Medium Priority

- [x] **I/O port extraction helper** [Medium, S]: Extracted generic `extractPort[T]` in #424.
- [x] **Optional fill argument extraction**: 3 `make-*` primitives (`PrimMakeVector`, `PrimMakeBytevector`, `PrimMakeString`) independently extract optional fill arguments with slightly different patterns. Share a helper.
- [x] **Machine package tech debt** [Medium, M]: 6 phases — arity dedup, closure extraction, expander decomposition, letrec* unification, library import dedup, stale alias removal. Complete in #444. See `plans/MACHINE-TECH-DEBT.md`.

### Low Priority

- [x] **Rename `AddSearchPath` to `PrependSearchPath`** [Low, S]: Renamed in library_registry.go and all call sites.
- [x] **Unify library/include path resolution** [Low, S]: `findFile` now consults library registry search paths as fallback dirs, sharing the same paths as `import`.
- [ ] **Tokenizer test file consolidation** [Low, M]: 14 test files, several named by coverage goals (`additional_coverage_test.go`, `final_coverage_test.go`). Consolidate into behavior-oriented files (`tokenizer_number_test.go`, `tokenizer_string_test.go`, etc.).
- [ ] **REPL deprecated wrappers** [Low, S]: `internal/repl/repl.go:370-393` — `Compile`, `Run`, `Load` marked `Deprecated`, delegate to runtime package. Delete if no callers remain.
- [ ] **Error sentinel grouping** [Low, S]: ~103 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.

### Postponed

- [ ] **F10: MachineContext decomposition** [Medium, Postponed]: 1671 lines, 71 methods, 10+ responsibilities. Extract `WindingStack`, `ContinuationChain`, `ExceptionHandler` into delegate types. Postponed — requires stable method surface; do after other refactorings settle.
- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Low, Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol.
- [ ] **Match: extract opcode handlers from VM interpreter** [Low, Postponed]: 264-line switch is large but stable. Extraction adds indirection without clear benefit until new opcodes are needed.
- [ ] **Match: consolidate bytecode type files** [Low, Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Low, Postponed]: Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring.
- [ ] **Schemeutil: grab-bag reorganization** [Low, Postponed]: Moving functions risks import cycle issues. Needs careful dependency analysis.

---

## Performance & CI

- [x] **CI benchmark tracking** [CI, S]: Gabriel suite (16 benchmarks, 3 runs) runs in CI; results uploaded as 90-day artifacts. PR #430. Baseline regenerated from CI hardware in #431 — apparent 20% regression was a measurement environment mismatch (local vs CI hardware), not a code regression.

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

- [ ] **Security context** Authorizer rides on `context.valueContext`.  This is considered to be bad style - create a custom security context that implements `context.Context`.
- [ ] **Hygiene debugging** [Tooling, Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Tooling, Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Dialect system** [Architecture, Proposed]: De-globalize forms registry, `WithDialect()` option, extract R7RS as default dialect. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Architecture, Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Programmatic tokenization/parsing** [Tooling]: Expose tokenizer/parser to Scheme code. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [x] **Reflection primitives** [Runtime]: `procedure-arity`, `procedure-name`, `procedure-source-location`, `procedure-bound-symbols`, `procedure-type` in `registry/core/`.
- [ ] **Continuation marks** [Runtime]: Racket-style stack annotation. Prompt infrastructure exists; needs per-frame key→value map.
- [ ] **Logging library** [Standard library]: Levels, structured output, handlers.
- [ ] **Event callbacks** [Tooling]: Hooks for expansion, compilation, debugging. IDE integration, profiling.
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined. No demand signal yet.
- [ ] **Scribble syntax (@-expressions)** [Syntax]: Racket-style text processing. No demand signal yet.
- [ ] **Hashtable SRFI compliance** [Standard library]: Current implementation is a custom API (10 primitives, fixed FNV-1a hash, fixed `EqualTo` comparison). Not R7RS-small (hashtables aren't in the spec) but doesn't conform to any SRFI either. Gaps vs SRFI-125: no custom hash/equality functions in constructor, no `hash-table-update!`, no `hash-table-fold`/`hash-table-map`, no immutable variant, no `hash-table->alist`/`alist->hash-table` conversion, naming uses `hashtable-*` not `hash-table-*`. Decide: target SRFI-125 (broader ecosystem compat) or keep custom API. Internal design issue: bucket chaining over `map[uint64][]entry` could be replaced with native Go map.
- [ ] **Fused lexing/parsing** [Research]: Flap paper analysis. Actionable only after profiling confirms tokenizer is a bottleneck. `plans/PERFORMANCE.md`
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`lib/*/test/`), new test cases for features not covered by Go test extraction.
