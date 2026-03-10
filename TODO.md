TODO
----

**Last Updated**: 2026-03-10

### Current Project Status

**Version**: v1.5.0 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower
**Extensions**: 10 extension packages — 7 public (files, math, system, threads, exceptions, gointerop, introspection), 3 internal (io, eval, all); all importable as R7RS `(wile <name>)` libraries
**Examples**: 76 examples across 12 categories, 21 Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test suite: 3,248 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 915-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned
- [x] **`guard` body drops multiple values** [Medium, S]: Fixed in #395 — body now uses `call-with-values` + `list` capture.
- [x] **Tuple ForEach nil returns Void instead of EmptyList** [Medium, S]: Fixed in #394 — nil guards and loop exits now return `EmptyList`/`SyntaxEmptyList`.
- [x] **Variadic math primitives accept excess args** [Low, S]: Fixed in #446 — `log`, `atan`, `number->string`, `string->number` now reject extra arguments. Also added missing radix validation to `string->number`.

---

## Refactoring

Ordered by dependency — items that unblock others or carry divergence risk come first. MachineContext decomposition is last (depends on other refactorings settling).

### Medium Priority

- [x] **I/O port extraction helper** [Medium, S]: Extracted generic `extractPort[T]` in #424.
- [x] **Optional fill argument extraction**: 3 `make-*` primitives (`PrimMakeVector`, `PrimMakeBytevector`, `PrimMakeString`) independently extract optional fill arguments with slightly different patterns. Share a helper.
- [x] **Machine package tech debt** [Medium, M]: 6 phases — arity dedup, closure extraction, expander decomposition, letrec* unification, library import dedup, stale alias removal. Complete in #444. See `plans/MACHINE-TECH-DEBT.md`.
- [x] **Math extension file split** [Low, S]: Split 1,292-line `prim_math.go` into 5 files by R7RS section (transcendental, rounding, rational, complex, conversion). Complete in #446.
- [x] **Raw string panics** [Medium, S]: 4 panics used raw strings instead of `werr.WrapForeignErrorf(sentinel, ...)` in `extensions/math/prim_complex.go`, `machine/edit_plan.go`, `machine/native_template.go`. Wrapped each with the appropriate sentinel.
- [x] **Bare sentinel panics** [Medium, S]: `NumberToFloat64`, `NumberToComplex128` (`values/promotion.go`) and `ExactnessOf` (`values/numeric_tower.go`) panicked with bare `werr.ErrNotANumber` — no `WrapForeignErrorf` wrapping. Added call-site context.
- [x] **internal/forms tests** [Medium, S]: `internal/forms/form_spec.go` (105 lines) is the only internal package with zero test files. Central registry used by `validate` and `machine`. Add `form_spec_test.go` covering registration, duplicate detection, and lookup miss behavior. Complete in #452.
- [x] **Expander time continuation decomposition** [Medium, M]: Split 1,327-line `expander_time_continuation.go` into 4 files: `expander_let_syntax.go` (let-syntax/letrec-syntax), `expander_primitive_forms.go` (if, begin, set!, define, import, etc.), `expander_lambda.go` (lambda, case-lambda, helpers), core dispatch remaining in original file.
- [x] **Quasiquote/quasisyntax duplication** [Medium, M]: Extracted shared `expandQuasi`, `expandQuasiList`, `expandQuasiListWithSplice` into `machine/quasi_expand.go` with `quasiKeywords` config struct. Fixed latent `list*` bug in quasisyntax improper list expansion (replaced with nested `cons`).
- [x] **goast/unmapper.go split** [Medium, S]: Split 1,277-line `unmapper.go` into 5 files by AST node category: dispatch+helpers, decl, stmt, expr, types. PR #465.
- [ ] **Pooling contract documentation** [Medium, S]: `machine/pool.go` has 4 global pools (stack, sub-context, continuation, env-frame). Continuation frames pooled only on normal return; `call/cc` escapes leave frames for GC via `MarkChainShared`. Intentional but undocumented. Add `docs/dev/POOLING.md`.

### Low Priority

- [x] **Rename `AddSearchPath` to `PrependSearchPath`** [Low, S]: Renamed in library_registry.go and all call sites.
- [x] **Unify library/include path resolution** [Low, S]: `findFile` now consults library registry search paths as fallback dirs, sharing the same paths as `import`.
- [x] **Tokenizer test file consolidation** [Low, M]: Consolidated 14 coverage-goal-named test files into 10 behavior-oriented files mirroring source structure. All 191 tests preserved. PR #448.
- [x] **REPL deprecated wrappers** [Low, S]: Deleted `Compile`, `Run`, `Load` wrappers from `internal/repl/repl.go`. Internal call sites now use `wileruntime` directly.
- [ ] **Error sentinel grouping** [Low, S]: ~103 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.
- [ ] **Operation file consolidation** [Low, M]: 28 single-method `machine/operation_*.go` files (30–50 lines each). Group into families: `operations_stack.go` (Push/Pop/Pull/Drop/PeekK), `operations_load.go` (LoadLiteral/LoadGlobal/LoadLocal/LoadVoid), `operations_branch.go`, etc. Reduces 28 files to ~8.
- [ ] **internal/validate test coverage** [Low, M]: 15 code files but only 2 test files. Each special form has its own validator but most lack dedicated unit tests. Coverage comes from integration tests. Add targeted tests when modifying validators.

### Postponed

- [x] **F10: MachineContext decomposition** [Medium, Postponed]: Split 1,639-line `machine_context.go` into 5 files by responsibility: `machine_context_continuation.go` (286 lines), `machine_context_winding.go` (149 lines), `machine_context_subcontext.go` (106 lines), `machine_context_apply.go` (320 lines), core reduced to 857 lines.
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
- [ ] **Type system**: type system that covers all the base types and can be expanded.  Discover useful properties of types to track (if any).  Types should be a distinct type (exists at the top of the hierarchy) - except for maybe some generic object type.
- [ ] **Parser unit tests**: unit tests for parser.
- [ ] **Source file tracking in Syntax Objects**: need some utilities around finding source locations and providing source lines.
- [ ] **Exceptions and Error stack tracrs**: Both Foreign and Native errors should track stacktraces with source code references.
- [ ] **Foreign Stack trace entry in stack traces that cross from Native -> Foriegn -> Native callback.
- [ ] **Area for blog articles** Git blog area in repo
- [ ] **Finish blog article** Finish blog article on appropriateness of Scheme for sandboxing.
- [x] **Go AST extension Phase 1 — Core** [Standard library, M]: Extension (`extensions/goast/`, `(wile goast)`) wrapping `go/ast`, `go/parser`, `go/token`, `go/format`. S-expression alist representation. ~28 node types (declarations, statements, expressions, basic types). All 5 primitives (`go-parse-file`, `go-parse-string`, `go-parse-expr`, `go-format`, `go-node-type`). Bidirectional mapper + round-trip tests. `plans/GO-AST.md`
- [ ] **Go AST extension Phase 2 — Advanced** [Standard library, S]: Concurrency (`GoStmt`, `DeferStmt`, `SelectStmt`, `CommClause`), switch (`SwitchStmt`, `TypeSwitchStmt`, `CaseClause`), `SliceExpr`, `TypeAssertExpr`, `ChanType`, `Ellipsis`, `LabeledStmt`. ~12 additional node types. `plans/GO-AST.md`
- [ ] **Go AST extension Phase 3 — Comments & generics** [Standard library, S]: `Comment`/`CommentGroup` attachment for round-trip structural fidelity. `BadExpr`/`BadStmt`/`BadDecl` for error recovery. `IndexListExpr` for generics. `plans/GO-AST.md`
- [ ] **Implement let-syntax*** [Core language, S]: Implement `let-syntax*`.
- [ ] **Native forms migration** [Refactoring, M]: Migrate Go primitives to Scheme where equivalent or superior. 43 of 52 done (Phases 0–2, 4 complete in #460, #462, #463). Phase 3 (9 list algorithms + port helpers, benchmark-gated) remains. See `plans/NATIVE-FORMS-MIGRATION.md`.
- [ ] **User labels/tags to distinguish FS resolvers** Use tags or labels to distinguish bootstrap loadee from include/library loaders in fileResolver.
- [ ] **Benchmark** benchmark result of moving primitives to Scheme
- [ ] **Procedure Inlining?** how can peephole inline procedures?
- [ ] **Operations for cxr** What about instructions for CxR?
