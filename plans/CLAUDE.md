# plans/ -- Plan File Conventions

**Plans go in `plans/`.** Do not create plan files in any other location.

**Plan file naming**: Use `UPPERCASE-WITH-HYPHENS.md` (e.g., `362-BIGCOMPLEX-INFNAN-GUARD.md`) or date-prefixed `YYYY-MM-DD-description.md` for time-stamped designs. Issue-linked plans are prefixed with the issue number.

## R7RS Investigation Procedure

When investigating R7RS conformance issues:
1. Document each test BEFORE running it in `plans/R7RS_TEST_INVESTIGATION_IN_PROGRESS.md`
2. Save the file, run the test (max 15s timeout), log results, save again
3. Keep error summary at top; use bisection technique to isolate errors

## Before Starting Work

**ALWAYS check existing project artifacts before planning or proposing solutions:**

1. **Check `plans/` directory** -- Read relevant plan files to understand existing design decisions, phase status, and what's already been explored
2. **Check `TODO.md`** -- Verify the task isn't already completed or documented as deferred
3. **Check existing patterns** -- Search the codebase for prior art before proposing new designs

**Do not:**
- Create new plan files without reading existing ones in `plans/`
- Propose architectural approaches without checking how similar problems are already solved
- Start implementation without verifying assumptions against actual code

---

## Forward-Looking Plans

Open designs and implementation work. These are the active items.

### Performance & Optimization

| File | Contents | Status |
|------|----------|--------|
| `PERFORMANCE.md` | Remaining optimizations (env frame slimming), benchmark baseline, fused lexing research | Env frame slimming **open**; procedure inlining complete (PR #605); NaN-boxing blocked by unsafe |
| `UNBOXED-FLOAT-PIPELINE.md` | Three-layer unboxed float pipeline (value register, tagged stack, binding unboxing) to eliminate Float heap allocations in arithmetic loops | **Proposed** -- 4 phases, not started |

### Architecture & Infrastructure

| File | Contents | Status |
|------|----------|--------|
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing, environment introspection | **Proposed** -- all 4 features unimplemented |
| `2026-03-30-machine-decomposition-design.md` | Machine package decomposition: compiler/VM/expander separation | **Approved** (design) -- partial work via PR #593 (compilation subpackage) |
| `2026-04-13-sourceload-design.md` | Isolated file-finding package: `sourceload/` under `machine/compilation/` | **Complete** |
| `2026-04-13-sourceload-impl.md` | sourceload implementation plan | **Complete** |
| `SECURITY.md` | Opcode resource limits (match steps, expand steps, continuation copy depth) | **Rejected** -- existing limits sufficient (call depth + stack size + context timeout) |
| `2026-04-14-error-stack-traces-design.md` | Error stack traces: SourcedError, CompilationError.Source, cross-boundary traces | **Phase 1 complete** -- core compiler wrapping done; Phases 2-4 (remaining files) + P3 (cross-boundary) open |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | **Proposed** -- not started |
| `MACRO_SYSTEM.md` | Hygiene debugging, macro expansion tracing | **Planned** -- OriginInfo core fields exist (PR #324); extended fields + hygiene debugging tools not started |

### Extension Contracts (Phase 2+)

Phase 1 infrastructure complete (PRs #577-578): `ForeignClosure.SetValidator/Validator`, `PrimitiveSpec.ParamTypes` with `TypeConstraint` interface (PR #629). No validation wired in yet; no extension annotations applied.

| File | Contents | Status |
|------|----------|--------|
| `2026-03-26-extension-contracts-impl.md` | Extension contracts remaining work: Phases 2-4 outlines | **Open** -- Phase 1 done, Phases 2-4 not started |
| `2026-03-26-extension-contracts-phase2-design.md` | Phase 2 design: ForeignClosure validation, auto-coercion | **Open** |
| `2026-03-26-extension-contracts-phase2-impl.md` | Phase 2 implementation plan | **Open** -- 0/8 tasks completed |

### Environment Profiles

| File | Contents | Status |
|------|----------|--------|
| `2026-03-26-environment-profiles.md` | Named profiles (Tiny, Console, Small, KitchenSink), sandbox modifier | **Draft** -- design complete, no implementation |
| `2026-03-26-environment-profiles-impl.md` | Environment profiles implementation plan (10 tasks) | **Draft** -- 0/10 tasks completed |

### Documentation & Discovery

| File | Contents | Status |
|------|----------|--------|
| `PRIM-APROPOS-EXPORT-INDEX.md` | Fix Scheme-level `(apropos)` asymmetry (export index on Namespace) | **Deferred** -- MCP tool covers LLM discovery gap; small change when prioritized |

### Tech Debt

| File | Contents | Status |
|------|----------|--------|
| `TECH-DEBT-2026-04.md` | Tech debt assessment: 8 phases, 27 tasks | Phases 1-7 **complete**; Phase 8.1, 8.3, 8.5 **complete**; Phases 8.2, 8.4 opportunistic |
| `TECH-DEBT-2026-04-IMPL.md` | Tech debt implementation tracker | 24/27 tasks complete |
| `2026-04-13-resolver-extraction-impl.md` | Resolver extraction: FileResolver → `machine/compilation/resolver/` | **Complete** |

---

## Completed Plans

Historical reference. Work has shipped; plans preserved for design context.

### Core Language & VM

| File | Contents | Completed |
|------|----------|-----------|
| `CORE-LET.md` | Core-let design: `let`/`let*`/`letrec`/`letrec*` as ValidatedExpr forms | PR #570 |
| `CORE-LET-IMPL.md` | Core-let implementation plan | PR #570 |
| `OPTIMIZER-FIX.md` | Fix `callForeignCached`/`applyForeign` savedCont double-restore | PR #573 |
| `2026-03-31-pulldrain-design.md` | O(1) PullDrain for OpPullApply dispatch | PRs #596, #598 |
| `2026-03-31-high-risk-bugfixes.md` | Sub-context winding stack hazard + cond-expand FileResolver bypass | PR #597 |
| `2026-04-01-engine-init-order.md` | Engine initialization order invariant: document + negative tests | PR #601 |
| `2026-04-01-timing-dependent-tests.md` | Replace timing-dependent `time.Sleep` with observation-based sync | PR #602 |
| `2026-04-01-disassembler-design.md` | Bytecode disassembler: Go layer, Scheme primitive, REPL `,disasm`, MCP tool | PR #603 |
| `2026-03-31-environment-any-fields.md` | Replace `any` fields in Namespace with typed interfaces | PR #594 |
| `2026-04-11-eval-stack-limit-design.md` | Eval stack size limit: `WithMaxStackSize`, `checkStackSize`, `ErrStackOverflow` | PR #636 |
| `2026-04-11-eval-stack-limit-impl.md` | Eval stack limit implementation plan | PR #636 |
| `2026-04-11-eval-subcontext-design.md` | Funnel `prim_eval.go` through `NewSubContext`: pool-backed release | PR #637 |

### Compiler Optimizations

| File | Contents | Completed |
|------|----------|-----------|
| `CAPTURE-ANALYSIS.md` | Capture analysis design for let bindings (B1) | PR #604 |
| `CAPTURE-ANALYSIS-IMPL.md` | Capture analysis implementation plan | PR #604 |
| `ESCAPE-ANALYSIS.md` | Escape analysis design for let-bound closures (B2) | PR #604 |
| `ESCAPE-ANALYSIS-IMPL.md` | Escape analysis implementation plan | PR #604 |
| `PROCEDURE-INLINING.md` | Procedure inlining for let-bound closures: synthetic let transform | PR #605 |
| `GC-PRESSURE-REDUCTION.md` | FreeList migration, pre-sized bindings, env frame leak fix | PRs #562-563 |
| `2026-04-05-walk-sub-exprs-design.md` | Shared ChildRole visitor for macro expansion sub-expressions | Implemented |

### Syntax & Macro Fixes

| File | Contents | Completed |
|------|----------|-----------|
| `2026-04-03-syntax-rules-ellipsis-hygiene-design.md` | syntax-rules ellipsis + hygiene bug analysis (3 bugs from SRFI-42) | PRs #606-607 |
| `2026-04-03-syntax-rules-ellipsis-hygiene-impl.md` | syntax-rules bug fix implementation plan | PRs #606-607 |
| `SRFI-42-SYNTAX-BUGS.md` | SRFI-42 syntax-rules bugs: cross-group ellipsis, nested depth, scoped bindings | PRs #606-607 |
| `2026-04-04-compilation-coverage.md` | machine/compilation coverage improvement (68.6% -> 83%) | PR #608 |
| `2026-03-25-degenerate-form-tests.md` | Degenerate form full-pipeline tests | PR #571 |

### Type System & Values

| File | Contents | Completed |
|------|----------|-----------|
| `OPAQUE-VALUES.md` | OpaqueValue type implementation plan | PR #566 |
| `2026-03-24-opaque-values-design.md` | OpaqueValue design document | PR #566 |
| `EXTENSIBLE-TYPE-CONSTRAINTS.md` | Extensible type constraint system: `TypeConstraint` interface replacing closed `ValueType` enum | PR #629 |
| `2026-04-09-extensible-type-constraints-impl.md` | TypeConstraint interface implementation plan | PR #629 |

### OS & Extensions

| File | Contents | Completed |
|------|----------|-----------|
| `OS-PRIMITIVES.md` | SRFI-170 subset: directory ops + process extension | PR #565 |
| `2026-03-24-os-primitives.md` | OS primitives design brainstorm | PR #565 |
| `2026-03-26-extension-contracts-design.md` | Extension API contract system: Phase 1 design | PRs #577-578 |
| `AVAILABLE-LIBRARIES.md` | Library discovery: `LibraryEnumerator`, `(available-libraries)` | PR #590 |
| `2026-03-26-wile-mcp-server-design.md` | Wile MCP server design (implemented as integrated `--mcp` flag in `cmd/wile`) | PR #588 |
| `MCP-EVAL-HARDENING.md` | MCP eval tool hardening: timeout, output limits | Implemented |

### Algebra Libraries

| File | Contents | Completed |
|------|----------|-----------|
| `2026-03-25-algebra-library-design.md` | Algebra library design document | Implemented |
| `2026-03-25-algebra-rewrite-design.md` | Term rewriting library design | Implemented |
| `2026-04-09-orthogonal-algebra-types.md` | Orthogonal algebra types: Heyting algebra, Boolean algebra | PRs #630-631 |
| `2026-04-10-orthogonal-algebra-phase2-design.md` | Phase 2: Setoid, Category, Closure Operator, Differential Ring | PR #631 |
| `2026-04-10-orthogonal-algebra-phase2-impl.md` | Phase 2 implementation plan | PR #631 |
| `2026-04-10-symbolic-algebra-design.md` | Symbolic algebra: theory projections, recursive normalizer, traced rewriting | PRs #632-633 |
| `2026-04-10-symbolic-algebra-impl.md` | Symbolic algebra implementation plan (Phases 1-2 complete; Phase 3 in wile-goast) | PRs #632-633 |

### Documentation System

| File | Contents | Completed |
|------|----------|-----------|
| `DOCUMENTATION-SEARCH.md` | Consolidated reference: `SearchDoc`, export index, keywords, topic browsing | Current |
| `2026-03-27-procedure-documentation-design.md` | `procedure-documentation` primitive: NativeTemplate doc field | PR #579 |
| `2026-03-27-scheme-library-docstrings-design.md` | Scheme library docstrings: conventions, phasing, scope | Implemented |
| `2026-03-27-special-form-macro-docstrings-design.md` | Special form & macro docstrings: `BindingMeta.Doc`, `BindingSpec` | Implemented |
| `2026-04-06-structured-docstring-metadata-design.md` | Structured docstring metadata: `Parameters:`, `Returns:`, `Category:` | Implemented |
| `2026-04-06-structured-docstring-metadata-impl.md` | Structured docstring metadata implementation plan | Implemented |
| `2026-03-28-library-level-documentation-design.md` | Library-level documentation (description fields, metadata) | Implemented |
| `2026-03-29-doc-examples-filtering.md` | Example filtering in documentation display | Implemented |
| `2026-04-07-documentation-gaps-plan.md` | Documentation coverage gap analysis (Phases 1-4) | Implemented |
| `2026-03-27-apropos-topic-browsing-design.md` | Original apropos & topic browsing design | Implemented |
| `2026-04-08-doc-keywords-design.md` | Keywords field design | Implemented |
| `2026-04-08-doc-keywords-impl.md` | Keywords implementation plan | Implemented |
| `2026-04-08-unified-apropos-design.md` | Unified `SearchDoc` design (Scheme + REPL) | Implemented |
| `2026-04-08-unified-apropos-impl.md` | Unified search implementation plan | Implemented |
| `LIBRARY-EXPORT-INDEX.md` | Static export index for unloaded library discovery | PRs #623-625 |
| `LIBRARY-EXPORT-INDEX-IMPL.md` | Export index implementation plan | PRs #623-625 |
| `2026-04-08-eager-doc-index-design.md` | Eager library metadata scan design | **Superseded** by `LIBRARY-EXPORT-INDEX.md` |

### REPL & Public API

| File | Contents | Completed |
|------|----------|-----------|
| `2026-04-07-public-repl-api-design.md` | Public REPL API for embedders: Engine-centric components, docparse promotion | PR #617 |
| `2026-04-07-public-repl-api-impl.md` | Public REPL API implementation plan | PR #617 |

### Refactoring & Code Quality

| File | Contents | Completed |
|------|----------|-----------|
| `2026-04-05-structural-reduction.md` | Full-codebase structural reduction: CallContext, Thread outcome, OperandKind | PRs #610-612 (Phase 2 intentionally rejected) |
| `TEST-COVERAGE-AND-REFACTORING.md` | machine/ test coverage, engine.go tests, typeswitchlint, form consistency | PR #541 + subsequent |
| `COMPILATION-COVERAGE.md` | Compilation test coverage plan | **Superseded** by `2026-04-04-compilation-coverage.md` |

### Retired Plans (files deleted in PR #504, work completed)

These plan files were removed from the repository after their work shipped:

| Former File | Work | Completed |
|-------------|------|-----------|
| `UNIFY-ESCAPE-MECHANISMS.md` | Unified `call-with-exit` and prompt/abort escape paths | PR #418 |
| `MACHINE-TECH-DEBT.md` | Machine package debt: arity dedup, expander decomposition, letrec* unification | PR #444 |
| `REMOVE-SYMBOL-INTERNING.md` | Remove symbol canonicalization, compare by string key | PR #440 |
| `FIX-GUARD-MULTIPLE-VALUES.md` | `guard` body drops multiple values -- `call-with-values` fix | PR #395 |
| `FIX-TUPLE-FOREACH-NIL.md` | Tuple ForEach nil returns EmptyList instead of Void | PR #394 |
| `CONTINUATION_MARKS.md` | Racket-style per-frame key-value annotations on continuation chain | PR #542 |
| `ENVIRONMENT-CLEANUP.md` | Environment package cleanup: constructor duplication, dead delegation | PRs #471, #607 |

---

Go AST/static analysis plans moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans).

## Design Documents (outside plans/)

| File | Purpose |
|------|---------|
| `docs/design/DESIGN.md` | Macro system design (three-layer architecture) |
| `docs/design/EMBEDDING.md` | Embedding API design (Engine, Value boundary, interop) |
| `docs/design/CONTINUATION_ESCAPE_DESIGN.md` | First-class continuation escape mechanism |
| `docs/design/DELIMITED_CONTINUATIONS.md` | Delimited continuations: prompts, abort, composable |
| `docs/design/SOURCE_LOADING.md` | FileResolver chain, embedded stdlib, library import resolution |
| `docs/design/PEEPHOLE_OPTIMIZER.md` | Superinstruction formation, 3-pass pipeline, EditPlan, promoted opcodes, savedCont invariant |
| `docs/dev/ENVIRONMENT_SYSTEM.md` | Environment system architecture |
| `docs/dev/NUMERIC_TOWER.md` | Numeric tower (direct dispatch, lattice model) |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Documented R7RS specification deviations |
| `docs/EXTENSIONS.md` | Extension system: architecture, authoring, registry, phases, FFI |
| `docs/EXTENSION_LIBRARIES.md` | R7RS library integration for extensions |

## Developer Documentation (outside plans/)

| File | Purpose |
|------|---------|
| `docs/dev/CONTINUATION_WORKLOAD_OPTIMIZATIONS.md` | Performance optimization guide |
| `docs/dev/DEBUG_METHODOLOGY.md` | Systematic debug logging methodology and Go gotchas |
