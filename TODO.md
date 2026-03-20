TODO
----

**Last Updated**: 2026-03-18

### Current Project Status

**Version**: v1.6.1 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower
**Extensions**: 10 extension packages — 7 public (files, math, system, threads, exceptions, gointerop, introspection), 3 internal (io, eval, all); all importable as R7RS `(wile <name>)` libraries. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Examples**: 76 examples across 12 categories, 21 Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test suite: 3,248 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 915-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Testing

### High Priority

- [x] **machine/ unit test coverage** [High, L]: Added 49 test files covering types/utilities, operations, VM runtime, compilation, expansion, library system, macro runtime, and infrastructure. ~500+ test cases. 3 of 52 files remain: `doc.go` (no code), `operations.go` (already covered by `operation_test.go`), one consolidated.
- [x] **engine.go unit tests** [High, M]: Added `engine_unit_test.go` with 7 test functions covering Eval, EvalMultiple, Compile+Run, Define+Get, Call, Close, error wrapping, and options.
- [x] **REPL test coverage** [High, M]: Added `debug_test.go` (23+ subtests for DebugContext: break, delete, list, enable, disable, step, next, finish, continue, backtrace, where). Extended `meta_test.go` with command listing, debug delegation, unknown command handling.

---

## Refactoring

### Medium Priority

- [x] **Missing primitive registration guide comment** [Medium, S]: Added "ADDING A NEW CORE PRIMITIVE" comment in `registry/core/register.go` (4 items).
- [x] **Value type guide comment** [Medium, S]: Added "ADDING A NEW VALUE TYPE" comment near the `Value` interface in `values/values.go` (7 conditional items + cross-reference to `values/numeric_kind.go` for numeric types).
- [x] **Type switch exhaustiveness linter** [Medium, M]: Added `cmd/typeswitchlint` — scans for type switches on `values.*`, reports switches without `default:` that may be missing concrete types. 8 warnings found across extensions/math, extensions/system, extensions/threads, internal/syntax, registry/helpers. Run: `go run ./cmd/typeswitchlint .` (default: warnings only; `-v` for all).
- [x] **Special form dual-dispatch verification** [Medium, M]: Added `forms.Verify()` checking that every registered form has both a validator and a compiler (with exceptions for expand-time-only forms: let-syntax, letrec-syntax, syntax-rules). `TestFormRegistrationConsistency` in `internal/forms/consistency_test.go` runs after init() from both validate and machine packages. Full unification blocked by import cycle; verification catches the same class of bug at test time.

### Low Priority

- [ ] **Error sentinel grouping** [Low, S]: ~105 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.
- [x] **extractPort thunk→bool refactor** [Low, S]: Refactored `extractPort` to return `(T, Tuple, bool, error)` — callers resolve the default. 6 call sites updated.

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

- [ ] **Procedure inlining** [Performance, Research]: Explore peephole inlining of known procedures at compile time. No plan yet.

### Architectural (Tier 3)

- [ ] **NaN-boxing / tagged pointers** [Performance, L]: Encode small values (fixnums, booleans, chars) in 64 bits instead of 16-byte Go interface. Halves stack/binding sizes. Massive change, awkward in Go. `plans/PERFORMANCE.md`

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
- [ ] **Security context** Authorizer rides on `context.valueContext`.  This is considered to be bad style - create a custom security context that implements `context.Context`.
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
