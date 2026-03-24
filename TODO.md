TODO
----

**Last Updated**: 2026-03-22

### Current Project Status

**Version**: v1.9.0 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower
**Extensions**: 11 extension packages — 7 public (files, math, system, threads, exceptions, gointerop, introspection), 4 internal (io, eval, namespace, all); all importable as R7RS `(wile <name>)` libraries. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Examples**: 76 examples across 12 categories, 21 Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test suite: 3,248 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 915-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Refactoring

### Low Priority

- [ ] **ExpanderTimeContinuation convention deviations** [Low, M]: wile-goast call-clustering found 40 deviations across 4 conventions (`SourceContext` 76%, `WrapForeignErrorf` 68%, `NewSyntaxCons` 64%, `IsSyntaxEmptyList` 60%). Some explained (let-syntax/letrec-syntax delegate to impl which wraps), others not yet checked.
- [ ] **Error sentinel grouping** [Low, S]: ~105 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.

### Postponed

- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Low, Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol.
- [ ] **VM dispatch loop extraction** [Low, Postponed]: `MachineContext.Run()` is 539 lines with ~63 inlined opcode cases. Extraction adds indirection without clear benefit — Go has no computed goto, and method dispatch adds measurable overhead on the hot path. The two-tier model (inlined ops + `OpComplex` side table for ~16 complex ops) already extracts the most complex operations. Intentional performance-over-readability trade-off.
- [ ] **Match: consolidate bytecode type files** [Low, Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Low, Postponed]: Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring.
- [ ] **Schemeutil: grab-bag reorganization** [Low, Postponed]: Moving functions risks import cycle issues. Needs careful dependency analysis.

---

## Performance & CI

### Actionable

- [ ] **Procedure inlining** [Performance, Research]: Explore peephole inlining of known procedures at compile time. `plans/PERFORMANCE.md`
- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.md`

### Research

- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. 6-phase incremental plan written. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.md`

---

## Features

- [ ] **Opcode resource limits** [Security, Design]: Per-category limits for match/expand/continuation copy. Completes defense-in-depth for embedded use. `plans/SECURITY.md`
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
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

- [ ] **Doc metadata for Scheme-defined procedures** [Tooling]: 44 primitives migrated to Scheme lost their `PrimitiveSpec` doc/params/category metadata (REPL `,doc`, extension library exports). Need a mechanism to register documentation for `define`-based procedures — either a doc-only `PrimitiveSpec` entry or a parallel doc registry that macro sources can populate.
- [ ] **Hygiene debugging** [Tooling, Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Tooling, Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Dialect system** [Architecture, Proposed]: De-globalize forms registry, `WithDialect()` option, extract R7RS as default dialect. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Architecture, Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Programmatic tokenization/parsing** [Tooling]: Expose tokenizer/parser to Scheme code. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Logging library** [Standard library]: Levels, structured output, handlers.
- [ ] **Event callbacks** [Tooling]: Hooks for expansion, compilation, debugging. IDE integration, profiling.
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined. No demand signal yet.
- [ ] **Scribble syntax (@-expressions)** [Syntax]: Racket-style text processing. No demand signal yet.
- [ ] **Hashtable SRFI compliance** [Standard library]: Current implementation is a custom API (10 primitives, fixed FNV-1a hash, fixed `EqualTo` comparison). Not R7RS-small (hashtables aren't in the spec) but doesn't conform to any SRFI either. Gaps vs SRFI-125: no custom hash/equality functions in constructor, no `hash-table-update!`, no `hash-table-fold`/`hash-table-map`, no immutable variant, no `hash-table->alist`/`alist->hash-table` conversion, naming uses `hashtable-*` not `hash-table-*`. Decide: target SRFI-125 (broader ecosystem compat) or keep custom API. Internal design issue: bucket chaining over `map[uint64][]entry` could be replaced with native Go map.
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`lib/*/test/`), new test cases for features not covered by Go test extraction.
- [ ] **Type system**: type system that covers all the base types and can be expanded.  Discover useful properties of types to track (if any).  Types should be a distinct type (exists at the top of the hierarchy) - except for maybe some generic object type.
- [ ] **Parser unit tests**: unit tests for parser.
- [ ] **Source file tracking in Syntax Objects**: need some utilities around finding source locations and providing source lines.
- [ ] **Exceptions and Error stack traces**: Both Foreign and Native errors should track stacktraces with source code references.
- [ ] **Foreign Stack trace entry in stack traces that cross from Native -> Foreign -> Native callback.**
- [ ] **Implement let-syntax*** [Core language, S]: Implement `let-syntax*`.
- [ ] **User labels/tags to distinguish FS resolvers** Use tags or labels to distinguish bootstrap loadee from include/library loaders in fileResolver.
- [ ] **Disassembler** Implement a disassembler for Wile
- [ ] **CompilationError** does not have source location, nor does it have an identity as a SchemeError and no Wrap* constructor.  Look into CompilationError and determine where it sits between Scheme and Foreign errors
- [ ] **RuntimeError** does not have an identity as SchemeError or ForeignError.  It also does not have a constructor
- [ ] **cond-expand (library ...) with fs.FS** `FindLibraryFile` in `features.go` uses `os.Stat` directly; `cond-expand (library ...)` cannot detect libraries in a virtual `fs.FS`. Requires passing `FileResolver` into the `FeatureRequirement` interface.

