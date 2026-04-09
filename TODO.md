TODO
----

**Last Updated**: 2026-04-05

### Current Project Status

**Version**: v1.10.3 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower, core-compiled let forms
**Extensions**: 11 extension packages — 7 public (files, math, process, system, threads, gointerop, introspection), 4 internal (io, eval, namespace, all); all importable as R7RS `(wile <name>)` libraries. Extension API contracts (ValueType enum, PrimitiveSpec type declarations) in Phase 1. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Embedding**: CLI uses public Engine API; embedded stdlib via `stdlib.FS` (`go:embed` + `fs.Sub`); `AllExtensions()`/`WithAllExtensions()` convenience options; `Engine.AvailableLibraries()` API for library discovery.
**Documentation**: Complete documentation system — `procedure-documentation`, `,doc`, `,apropos`, `,topics`, `,topic`, library-level `(description)` clause, docstring examples, examples filtering. All 397 primitive specs + ~243 stdlib procedures + 29 special forms + 15 macros + 11 syntax compilers + 34 library descriptions documented.
**MCP Server**: Built-in MCP server mode (`wile --mcp`) with eval, doc, apropos, topic, libraries, and reset tools.
**Examples**: 75 examples across 13 categories, 23 benchmarks (16 Gabriel + Larceny R7RS + Schelog + miniKanren)
**Tests**: Go test suite comprehensive; Scheme test suite: 3,852 lines across 19 files (11 scheme + 8 algebra) + R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1), (wile algebra) complete; stdlib embedded in binary

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- [x] **Peephole optimizer double-restore** [High, Bug, Fixed]: `callForeignCached` and `applyForeign` would double-restore when PrimCallCC inline mode called `ApplyCallable` with a `ForeignClosure` (e.g., `(call/cc procedure?)`). Fixed with `savedCont` pointer-identity guard. `plans/OPTIMIZER-FIX.md`
- [x] **Degenerate form pipeline tests** [Correctness, Done]: Full-pipeline tests (string → tokenize → parse → expand → compile → run) for degenerate forms of all core special forms and macro-based derived forms. PR #571.
- [x] **Sub-context winding stack inheritance hazard** [High, Correctness, Fixed]: `NewSubContext(windingStack)` now requires the winding stack as a constructor parameter. Forgetting is a compile error. `machine/machine_context_subcontext.go`.
- [x] **`cond-expand (library ...)` bypasses FileResolver** [High, S, Fixed]: `IsSatisfied` now accepts a `FileResolver` parameter. `libraryRequirement` uses `resolver.ResolveAndOpen` (with `.sld`/`.scm` fallback) instead of `os.Stat`. `machine/compilation/features.go`.
- [x] **syntax-rules ellipsis and hygiene bugs** [High, Bug, Fixed]: Three bugs fixed — scope-aware duplicate binding detection (unified scoped binding API, PR #607), cross-group ellipsis zipping, and nested ellipsis depth tracking (PR #606). `plans/2026-04-03-syntax-rules-ellipsis-hygiene-design.md`
- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Refactoring

### High Priority

- [x] **`WalkSubExprs` for validated expression traversal** [High, S, Done]: `WalkSubExprs(expr, fn(child, role))` with `ChildRole` enum (`RoleNormal`, `RoleCallProc`, `RoleClosureBody`). B1 capture analysis migrated to use it; immediately-applied lambda handled as B1-specific post-check in `RoleCallProc`.
- [x] **Extract interface types from `environment/` `any` fields** [High, M, Done]: `FileResolver` interface defined in `environment/file_resolver.go` (stdlib types only); `machine/compilation/` adds type alias. `LibrarySearcher` interface (`GetSearchPaths() []string`) eliminates type assertions in `file_resolver.go`. `authorizer any` → `security.Authorizer` (security package only imports `werr`). 15 type assertions removed across 7 files. `plan: plans/2026-03-31-environment-any-fields.md`
- [x] **`Stack.Pull()` is O(n) in VM hot path** [High, M, Done]: Replaced `Pull()` + `Drain()` in `OpPullApply` with O(1) `PullDrain()` that splits `stack[0]` (proc) from `stack[1:]` (args) without copying. Unfused `OpPull` unchanged (rare after peephole). `plans/2026-03-31-pulldrain-design.md`
- [x] **Split `ffi.go` by concern** [Medium, S, Done]: 1010 lines split into `ffi.go` (spec), `ffi_arg_converters.go`, `ffi_ret_converters.go`, `ffi_wrapper.go`. PR #599.
- [x] **Engine initialization order invariant** [Medium, S, Done]: `engine.go:122-142` documents the 6-step dependency DAG. Negative tests in `engine_init_order_test.go` verify unbootstrapped namespace fails eval and library system fails without bootstrap. `plans/2026-04-01-engine-init-order.md`

### Medium Priority

- [x] **`machine/` mega-package decomposition** [Medium, L, Done]: Phase 1 (PR #592) — `MacroEvaluator` and `ExpanderCtx` interfaces. Phase 2 (PR #593) — 95 files moved to `machine/compilation/` subpackage; `typedCompiler` adapter deleted; bridge types in `machine/syntax_bridge_types.go`; shared test helpers in `machine/testutil/`. Compiler imports `machine`, never reverse. `plans/2026-03-30-machine-decomposition-design.md`
- [x] **`file_resolver.go` chain of responsibility** [Medium, M, Done]: Extracted `osSearchDirs`, `openAuthorized`, `walkOSLibraries`, and `walkFSDir` as shared helpers. `OSFileResolver` and `FSFileResolver` now delegate to these instead of duplicating directory-collection and walk-callback logic. 541 → 469 lines.
- [x] **Timing-dependent concurrency tests** [Medium, M, Done]: 10 of 11 `time.Sleep` calls replaced with observation-based synchronization (`internal/testutil` package: `PollUntil`, `ReadyExtension`, `stableGoroutineCount`). 1 deliberate-race sleep kept. PR #602. `plans/2026-04-01-timing-dependent-tests.md`

### Low Priority

- [x] **ExpanderTimeContinuation convention deviations** [Low, M, Done]: Fixed 18 deviations in expander files: 13 bare `return nil, err` wrapped with `WrapForeignErrorf` context, 5 `.IsEmptyList()` replaced with `syntax.IsSyntaxEmptyList()`. SourceContext and NewSyntaxCons conventions were already followed in expander files; remaining deviations (if any) are in other compilation files.
- [ ] **Error sentinel grouping** [Low, S]: ~109 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.
- [x] **Opcode metadata consolidation (D5)** [Low, S, Done]: Added `OperandKind` enum (7 categories) to `opcodeInfo`. `Disassemble()` and `instructionToOperation()` now switch on metadata instead of per-opcode case branches. Adding a new promoted op dropped from 5 edit sites to 3. `Run()` untouched (hot path). `plans/2026-04-05-structural-reduction.md`

### Structural Reduction — Investigated, No Action

These items from `plans/2026-04-05-structural-reduction.md` were investigated and determined to not warrant changes:

- [x] **Promoted op table (Phase 2)** [Rejected]: Replacing 34 switch cases with table-driven dispatch regressed ~1.5% geo mean across 16 Gabriel benchmarks (15/16 slower, worst ackermann +3.4%). Go compiles contiguous-integer switches to jump tables; the table-driven `default:` branch adds range check + array index + indirect load. The maintenance cost of hand-unrolled cases is the accepted trade-off.
- [x] **PrimitiveSpec dead fields (D1)** [Stale]: Originally flagged as 5% `ParamTypes` / 2% `ReturnType` usage. Extension contracts Phase 1 (PRs #577-578) populated both fields broadly (170 and 129 specs respectively). No longer dead.
- [x] **ForeignClosure redundant fields (D2)** [Accepted]: `doc` field duplicates `PrimitiveSpec.Doc` but costs only ~3.2KB total (8 bytes × ~400 closures), is set once at registration and cannot diverge. Removing requires circular import workarounds or Closure interface changes — complexity exceeds benefit. `validate` field is active (contract enforcement at dispatch).
- [x] **Namespace root/child state waste (D3)** [Accepted]: Child namespaces have ~6 nil/unused fields out of 16. Not worth splitting: (1) every nil field has a delegation method, (2) splitting forces an interface (hot-path dispatch cost) or wrapper (indirection), (3) children are rare (~handful per VM lifetime), saving ~24 bytes is meaningless, (4) zero-value mutexes and nil maps cost nothing.
- [x] **LocalIndex / BindingID overlap (D4)** [Audited]: `BindingID` used in `internal/validate` (mutation/capture/escape analysis) and `machine/compilation` (inline candidates). Not replaceable by `LocalIndex` — `LocalIndex` is relative (slot+depth from a reference frame, same binding gives different keys at different depths), `BindingID` is absolute (frame pointer + slot, stable identity). Both needed.

### Postponed

- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Low, Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol.
- [ ] **VM dispatch loop extraction** [Low, Postponed]: `MachineContext.Run()` is 547 lines with 65 inlined opcode cases. Extraction adds indirection without clear benefit — Go has no computed goto, and method dispatch adds measurable overhead on the hot path. The two-tier model (inlined ops + `OpComplex` side table for 16 complex ops) already extracts the most complex operations. Intentional performance-over-readability trade-off.
- [ ] **Match: consolidate bytecode type files** [Low, Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Low, Postponed]: Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring.
- [ ] **Schemeutil: grab-bag reorganization** [Low, Postponed]: Moving functions risks import cycle issues. Needs careful dependency analysis.

---

## Performance & CI

### Completed

- [x] **GC pressure reduction** [Performance, Done]: FreeList migration for continuation/stack pools, pre-sized binding arrays in env frame pool, env frame leak fix in context release. -8.9% geo mean. PRs #562-563. `plans/GC-PRESSURE-REDUCTION.md`
- [x] **Core-let compilation** [Performance, Done]: `let`, `let*`, `letrec`, `letrec*` compiled as core forms with `ValidatedLet` + `OpPushEnv`, eliminating lambda overhead for all binding forms. PR #570. `plans/CORE-LET-IMPL.md`

### Actionable

- [x] **Procedure inlining** [Performance, Done]: Let-bound lambda calls inlined as synthetic let forms. PR #605. `plans/PROCEDURE-INLINING.md`
- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.md`
- [x] **B2 escape analysis for let-bound closures** [Performance, Done]: Tracks whether let-bound closures escape their scope. Enables `!Captured` optimization. PR #604. Design: `plans/ESCAPE-ANALYSIS.md`.
- [ ] **B3 effective capture refinement** [Performance, Research]: Propagate B2 escape results back into B1 capture status. A binding marked `Captured` by B1 is effectively non-captured if every lambda that references it is stored in a non-escaping binding (B2). Cross-binding analysis over B1+B2 results.

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
- [ ] **POSIX API / SRFI-170** [Standard library, 10 phases]: Comprehensive OS access — stat, permissions, links, temp files, env vars, subprocess, signals, user/group, terminal, error handling. Phase 1 (directory ops + process extension) completed in PR #565; remaining phases not started.
- [x] **Algebra library** [Standard library]: General-purpose algebraic structures as an R7RS library `(wile algebra)`. Partial orders, lattices (flat, powerset, product, map), fixpoint (Kleene + widening), monoids, semirings (boolean, tropical, counting), groups, rings (integer, modular), fields (rational), Galois connections. Design: `plans/2026-03-25-algebra-library-design.md`. 158 tests across 8 test files.

- [x] **Documentation system** [Tooling, High, Done]: REPL-accessible documentation for all Scheme bindings. Complete infrastructure: `procedure-documentation` primitive (PRs #579, #581); `,doc` REPL command (unified path via `callableDoc()`); `,apropos` / `,a` search (PR #585); `,topics` / `,topic` category browsing; library-level `(description)` clause (PR #586); `(available-libraries)` primitive (PR #590); docstring examples with filtering (PRs #589, #591). All 397 primitive specs + ~243 stdlib procedures + 29 special forms + 15 macros + 11 syntax compilers + 34 library descriptions documented. CxR accessors (28) also documented (PR #587).
  - [ ] **Eager documentation index** [Tooling, High]: Scan `.sld` files at engine init to populate a doc index before any library is imported. LLMs (the primary doc consumers) can't discover library functions via `apropos`/`doc` until the library is loaded — this makes all available libraries searchable from the first tool call. `plans/2026-04-08-eager-doc-index-design.md`
  - [ ] **Scribble-style `@` reader notation** [Reader extension, Low Priority]: Racket-style at-expressions for rich documentation markup. Reader recognizes `@cmd[datum ...]{text ...}` and desugars to S-expressions. Enables structured doc content beyond plain strings. Depends on docstring validation being in the validator layer (done in PR #584).
- [ ] **Scheme linter** [Tooling, High, Needs Scoping]: Static analysis for Wile Scheme code — catch "plausible but wrong" before execution. Scope, design, and feasibility TBD. Potential checks: unused bindings, arity mismatches on known procedures, type mismatches at call sites, unreachable code after tail calls, style warnings. Research: what do Racket (Check Syntax), Guile, CHICKEN lint tools actually check? How much can be done at expand time vs requiring a separate pass? Interaction with the type system (if built) is a key design question.
- [ ] **Extension API contracts** [Embedding, High, Phase 1 Done]: Stronger type/contract declarations on extension APIs. Phase 1 complete (PRs #577, #578): `ValueType` enum on `PrimitiveSpec` for param/return type declarations, contract validator infrastructure in `ForeignClosure` dispatch. Remaining: Phase 2 compile-time (compiler consults `ParamTypes` for static call sites — error before execution, zero runtime cost) and runtime (`buildValidator` wires `ParamTypes` → `SetValidator` for dynamically-constructed calls), integration with linter. `plans/2026-03-26-extension-contracts-design.md`
- [ ] **Environment profiles** [Embedding]: Replace SafeExtensions/AllExtensions with named profiles (Tiny, Console, Small, KitchenSink), orthogonal sandbox modifier, virtual env map. `plans/2026-03-26-environment-profiles-impl.md`
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.
- [x] **MCP server** [Tooling, Done]: Built-in MCP server mode (`wile --mcp`) exposing eval, doc, apropos, topic, libraries, and reset tools. Session hardening with configurable timeouts. PR #588. `plans/2026-03-26-wile-mcp-server-design.md`
- [x] **`(available-libraries)` primitive** [Embedding, Done]: `LibraryEnumerator` interface, `Engine.AvailableLibraries()` Go API, `(available-libraries)` Scheme primitive. PR #590. `plans/AVAILABLE-LIBRARIES.md`
- [x] **OpaqueValue type** [Values, Embedding]: Generic opaque wrapper for Go objects in Scheme. `SchemeString()` → `#<tag:id>`, identity-based equality, `opaque?` and `opaque-tag` predicates. Enables wile-goast shared sessions (Track A1) and other Go-object-wrapping use cases.

---

## Nice-to-Haves

No demand signal. Speculative or research-only.

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
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`stdlib/lib/*/test/`), new test cases for features not covered by Go test extraction.
- [ ] **Type system**: type system that covers all the base types and can be expanded.  Discover useful properties of types to track (if any).  Types should be a distinct type (exists at the top of the hierarchy) - except for maybe some generic object type.
- [ ] **Parser unit tests**: unit tests for parser.
- [ ] **Source file tracking in Syntax Objects**: need some utilities around finding source locations and providing source lines.
- [ ] **Exceptions and Error stack traces**: Both Foreign and Native errors should track stacktraces with source code references.
- [ ] **Foreign Stack trace entry in stack traces that cross from Native -> Foreign -> Native callback.**
- [ ] **Area for blog articles** Git blog area in repo
- [ ] **Finish blog article** Finish blog article on appropriateness of Scheme for sandboxing.
- [x] **Go AST extension Phase 2 — Advanced** [Standard library, S]: Concurrency (`GoStmt`, `DeferStmt`, `SelectStmt`, `CommClause`), switch (`SwitchStmt`, `TypeSwitchStmt`, `CaseClause`), `SliceExpr`, `TypeAssertExpr`, `ChanType`, `Ellipsis`, `LabeledStmt`. 13 node types. PR #480. `plans/GO-AST.md`
- [ ] **Go AST extension Phase 3 — Comments & generics** [Standard library, S]: `Comment`/`CommentGroup` attachment for round-trip structural fidelity. `BadExpr`/`BadStmt`/`BadDecl` for error recovery. `IndexListExpr` for generics. `plans/GO-AST.md`
- [ ] **Implement let-syntax*** [Core language, S]: Implement `let-syntax*`.
- [ ] **User labels/tags to distinguish FS resolvers** Use tags or labels to distinguish bootstrap loadee from include/library loaders in fileResolver.
- [x] **Disassembler** [Tooling, Done]: Bytecode disassembler — `(disassemble proc)` primitive, `,dis` REPL command, MCP tool. PR #603.
- [ ] **CompilationError** does not have source location, nor does it have an identity as a SchemeError and no Wrap* constructor.  Look into CompilationError and determine where it sits between Scheme and Foreign errors
- [ ] **RuntimeError** does not have an identity as SchemeError or ForeignError.  It also does not have a constructor
- [x] **Scheme Disassembly** [Tooling, Done]: Addressed by `(disassemble proc)` and `,dis` REPL command. PR #603.
- [x] **Primitive Search** — addressed by `(apropos "pattern")` and `,apropos` REPL command (PR #585).
- [x] **Expression Evaluation** — addressed by MCP server eval tool and `,doc` system (PR #588).
- [x] **Proposal** — MCP server tool descriptions now accurate (PR #588).
- [ ] **Consider ValueType Refactoring** ValueType does not seem to have a grounding in Scheme or Go, which begs the question of what sorts of type domains is it attempting to describe?  Ask specific questions of AI to determine the use and scope of the type domains
- [ ] **Evaluate Need for Primitive Annotation Enforcement** — enforcement may not be needed.
- [ ] **Namespace registry is `any`** - namespace's registry should have a type
- [ ] **MCP eval fails on schelog `include`** — `(include "examples/logic/schelog/schelog.scm")` followed by `(solve-puzzle %houses)` produces `#!void` for `schelog:unbind-ref!` at line 113. CLI (`wile -f`) works fine. Likely an `include` resolution or session-state issue in MCP eval mode.
