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
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing, environment introspection | Env introspection **complete**; other 3 sections **proposed** -- not started |
| `SECURITY.md` | Opcode resource limits (match steps, expand steps, continuation copy depth) | **Rejected** -- existing limits sufficient (call depth + stack size + context timeout) |
| `2026-04-14-error-stack-traces-design.md` | Error stack traces: SourcedError, CompilationError.Source, cross-boundary traces | **Phases 1-4 complete** -- all compiler/syntax/library/expander wrapping done; P3 (cross-boundary) deferred |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | **Proposed** -- not started |
| `MACRO_SYSTEM.md` | Hygiene debugging, macro expansion tracing | **Planned** -- OriginInfo core fields exist (PR #324); extended fields + hygiene debugging tools not started |

### Extension Contracts (Phase 2+)

Phase 1 infrastructure complete (PRs #577-578): `ForeignClosure.SetValidator/Validator`, `PrimitiveSpec.ParamTypes` with `TypeConstraint` interface (PR #629). No validation wired in yet; no extension annotations applied.

| File | Contents | Status |
|------|----------|--------|
| `2026-03-26-extension-contracts-impl.md` | Extension contracts remaining work: Phases 2-4 outlines | **Open** -- Phase 1 done, Phases 2-4 not started |
| `2026-03-26-extension-contracts-phase2-design.md` | Phase 2 design: ForeignClosure validation, auto-coercion | **Open** |
| `2026-03-26-extension-contracts-phase2-impl.md` | Phase 2 implementation plan | **Open** -- 3/8 tasks completed (1-3 done: validate field + dispatch paths) |
| `2026-04-20-paramtypes-audit-design.md` | Phase 5 design: audit `ParamTypes` across 500 primitives; per-slot analysis, extractor table, declared-too-narrow is load-bearing under Phase-2 enforcement | **Complete. 5.A (PR #678), 5.B (PR #679 analyzer+inventory), 5.C (`2026-04-20-paramtypes-annotation-bugs.md` sidecar), 5.D (one partial narrowing: `get-output-bytevector` → `TypeBinaryOutputPort`), 5.E (`2026-04-20-paramtypes-axis-c-findings.md` R7RS sweep — 0 new findings, 1 re-affirmation of Phase 4 H.1). Final bucket counts: 72 Single-strict, 110 Single-coercing, 3 Declared-too-narrow (all FP), 106 Declared-too-wide, 13 Union, 93 Variadic-rest, 79 Unguarded. Four-axis framework closed. Vocabulary-extension design at `2026-04-21-type-constraint-extension-design.md`.** |
| `2026-04-21-type-constraint-extension-design.md` | Julia-subset nominal type lattice: `OpaqueTypeConstraint` with `reflect.Type` storage, `Subtype` as primary operation, `AnyType` root, ~24 opaque singletons (Box/Promise/concurrency/syntax/error/prompt/mark), `RecordTypeConstraint` retrofit as subtype of `*Record`. Advisory `ReturnNullable` on `PrimitiveSpec`. Explicitly excludes refinement types (violates invertibility on return side) and union types (creates runtime-check duplication). **Gated sequence**: Phase 1a specification mechanism only (no primitive changes) → Phase 1b primitive re-audit under new vocabulary → Phase 2+ hierarchy/enforcement/closure. | **Design draft** — implementation deferred to separate plan file(s) |

### Environment Profiles

Moved to **Completed Plans** below.

### Benchmarks

| File | Contents | Status |
|------|----------|--------|
| `2026-04-16-recurrence-categories-design.md` | Set closure, graph reachability, matrix ops benchmark categories | **Design only** -- no implementation started; matrix_ops blocked on `(wile algebra matrix)` |
| `2026-04-16-recurrence-impl-plan.md` | 5-task impl for set_closure + graph_reachability generators | **0/5 tasks** -- generate.py unchanged |

### MCP Server

| File | Contents | Status |
|------|----------|--------|
| `2026-04-17-mcp-server-sota-design.md` | Bring `cmd/wile/mcp.go` to current MCP best-practices: progress notifications, doc/rationale polish, opt-in streamable-HTTP transport, elicitation capability | **Proposed** -- 5 phases, not started |
| `2026-04-18-mcp-triggering-rewrite.md` | Rewrite MCP server instructions + 9 tool descriptions + wile-scheme prompt to trigger LLM tool use on algebra/modular/polynomial domains. Text-only, no code logic changes. Closes `powerset_lattice` regression | **Design awaiting user review** -- not implemented |

### Coverage & Tooling

| File | Contents | Status |
|------|----------|--------|
| `2026-04-18-scheme-line-coverage.md` | Scheme-side line coverage: `executed []bool` on `NativeTemplate`, `WithCoverage` engine option, `--cover PATH` CLI flag, Go cover v1 output | **Design locked-in** -- active branch `feat/scheme-coverage`; 0/56 steps complete |

### Algebra Roadmap

| File | Contents | Status |
|------|----------|--------|
| `2026-04-17-algebra-foundations-directions.md` | 6 prioritized directions extending `(wile algebra ...)`: §5.1 matrix (semiring-parameterized), §5.2 Möbius/incidence, §5.3 AC-matching, etc. | **Directions** -- funding-gated roadmap |
| `2026-04-18-gonum-integration-directions.md` | Generic graph analytics via gonum (SCC, Louvain, centrality) + benchmark statistics. Two independent tracks: `goastgraph/` and `bench-stats/` | **Directions** -- funding-gated roadmap |
| `2026-04-20-matrix-sparse-dense-design.md` | Matrix library open design questions: (1) freeze vs iterator-replace `sparse-semiring-matrix-entries` alist shape, (2) polymorphic `matrix?` / `matrix-ref` protocol across dense + sparse, (3) mixed-operand sparse/dense ops. Library investigation (gonum/SciPy/Julia/BLAS) converges on unified-API + concrete-dispatch pattern. Reference-semantics subsection aligns matrices with pair/vector mutation idioms (pure + `!` forms, destination-first, `eq?` sharing). | **Design draft** -- Path D recommended (Q1c + Q2a + Q3a-subset); OQ1 resolved (expose both pure and bang forms); OQ2-OQ6 open; follow-up impl plan pending |
| `2026-04-21-matrix-path-d-impl.md` | Matrix Path D implementation plan: 10 phases (scaffold → iterator API → unified accessors → bang-first arithmetic → capability predicate → sparse-error paths → aliasing enforcement → umbrella + docs). D1 resolved to dispatch-table with predicate-cond rep-tag (b-i); D2 resolved to bare `(error ...)` matching existing `matrix.scm` style; D3 resolved to bang-first. OQ6 views additively extensible via `matrix-rep-tag` single extension point. | **Implementation complete pending merge** -- PRs #684 (P2 scaffold), #685 (P3 iterator), #686 (P4 accessors), #687 (P5a add), #688 (P5b mul), #689 (P6 capability), #690 (P7 dense-only ops), #691 (P8-P10 copy+docs); test count 112→234 |

### Tech Debt

| File | Contents | Status |
|------|----------|--------|
| `TECH-DEBT-2026-04.md` | Tech debt assessment: 8 phases, 27 tasks | 24/27 complete; 6.2, 6.4 **open**; 8.2, 8.4 opportunistic |
| `TECH-DEBT-2026-04-IMPL.md` | Tech debt implementation tracker | 24/27 complete; 6.2 (context.TODO), 6.4 (typeswitchlint guide) **open** |

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
| `CONSTANT-BINDINGS.md` | Constant/imported binding flag design (moved to `memory/`) | Implemented |
| `CONSTANT-BINDINGS-IMPL.md` | Constant bindings implementation plan (moved to `memory/`) | Implemented |
| `2026-04-11-eval-stack-limit-design.md` | Eval stack size limit: `WithMaxStackSize`, `checkStackSize`, `ErrStackOverflow` | PR #636 |
| `2026-04-11-eval-stack-limit-impl.md` | Eval stack limit implementation plan | PR #636 |
| `2026-04-11-eval-subcontext-design.md` | Funnel `prim_eval.go` through `NewSubContext`: pool-backed release | PR #637 |
| `2026-03-30-machine-decomposition-design.md` | Machine package decomposition design (moved to `memory/`) | Implemented (PR #593 + compilation subpackage) |
| `2026-04-12-expansion-ops-to-compilation-design.md` | Move expansion ops to compilation/ design (moved to `memory/`) | Implemented |
| `2026-04-12-expansion-ops-to-compilation-impl.md` | Expansion ops move 5-task impl (moved to `memory/`) | Implemented |
| `2026-04-13-sourceload-design.md` | sourceload package design (moved to `memory/`) | Implemented |
| `2026-04-13-sourceload-impl.md` | sourceload 10-task impl (moved to `memory/`) | Implemented |
| `2026-04-13-resolver-extraction-impl.md` | Resolver extraction impl (moved to `memory/`) | Implemented |
| `2026-04-16-timer-interrupts-design.md` | Wall-clock timer interrupts with continuation capture (moved to `memory/`) | PR #659 |
| `2026-04-16-timer-interrupts-impl.md` | Timer interrupts 8-task implementation plan (moved to `memory/`) | PR #659 |
| `2026-04-16-error-diagnostics-design.md` | Error context + NativeError enrichment + compiler Phase 2-4 migration (moved to `memory/`) | All layers complete |
| `2026-04-16-error-diagnostics-impl.md` | Error diagnostics 10-task implementation (moved to `memory/`) | 10/10 complete |

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

### Environment Profiles

| File | Contents | Completed |
|------|----------|-----------|
| `2026-03-26-environment-profiles.md` | Named profiles (Tiny, Console, ConsoleWithLoad, Small, KitchenSink), sandbox modifier | PR #662 |
| `2026-03-26-environment-profiles-impl.md` | Environment profiles 10-task implementation plan | PR #662 |

### OS & Extensions

| File | Contents | Completed |
|------|----------|-----------|
| `OS-PRIMITIVES.md` | SRFI-170 subset: directory ops + process extension | PR #565 |
| `2026-03-24-os-primitives.md` | OS primitives design brainstorm | PR #565 |
| `2026-03-26-extension-contracts-design.md` | Extension API contract system: Phase 1 design | PRs #577-578 |
| `AVAILABLE-LIBRARIES.md` | Library discovery: `LibraryEnumerator`, `(available-libraries)` | PR #590 |
| `2026-03-26-wile-mcp-server-design.md` | Wile MCP server design (implemented as integrated `--mcp` flag in `cmd/wile`) | PR #588 |
| `MCP-EVAL-HARDENING.md` | MCP eval tool hardening: timeout, output limits | Implemented |
| `2026-04-15-srfi-132-design.md` | SRFI-132 Sort Libraries design (moved to `memory/`) | Implemented |
| `2026-04-15-srfi-132-impl.md` | SRFI-132 10-task impl plan (moved to `memory/`) | Implemented |

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
| `2026-04-12-sage-algebra-validation-design.md` | Sage algebra validation harness design (moved to `memory/`) | Implemented |
| `2026-04-12-sage-algebra-validation-impl.md` | Sage validation 9-task impl plan (moved to `memory/`) | Implemented |
| `2026-04-14-algebra-documentation-design.md` | Algebra user-facing docs + examples design (moved to `memory/`) | Implemented |
| `2026-04-14-algebra-documentation-impl.md` | Algebra documentation 10-task impl plan (moved to `memory/`) | Implemented |
| `2026-04-18-polynomial-library.md` | `(wile algebra polynomial)` — ring-parameterized univariate polynomials: plus/negate/minus/times, Horner eval, formal derivative, divmod, GCD, polynomial-ring capstone, `with-polynomial` macro | 12/12 tasks, 60 tests passing (commits `69b98203`..`78bb7e2f` on `feat/scheme-coverage`) |

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
| `PRIM-APROPOS-EXPORT-INDEX.md` | Scheme-level `(apropos)` asymmetry fix: ExportIndex on Namespace, lazy build in PrimApropos (moved to `memory/`) | Post-#623 |
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
| `docs/compiler/macro-system.md` | Macro system design (three-layer architecture) |
| `docs/embedding/api-design.md` | Embedding API design (Engine, Value boundary, interop) |
| `docs/continuations/escape-design.md` | First-class continuation escape mechanism |
| `docs/continuations/delimited.md` | Delimited continuations: prompts, abort, composable |
| `docs/embedding/source-loading.md` | FileResolver chain, embedded stdlib, library import resolution |
| `docs/compiler/peephole-optimizer.md` | Superinstruction formation, 3-pass pipeline, EditPlan, promoted opcodes, savedCont invariant |
| `docs/environment/system.md` | Environment system architecture |
| `docs/numeric/tower.md` | Numeric tower (direct dispatch, lattice model) |
| `docs/reference/r7rs-differences.md` | Documented R7RS specification deviations |
| `docs/extensions/architecture.md` | Extension system: architecture, authoring, registry, phases, FFI |
| `docs/extensions/libraries.md` | R7RS library integration for extensions |

## Developer Documentation (outside plans/)

| File | Purpose |
|------|---------|
| `docs/continuations/optimizations.md` | Performance optimization guide |
| `docs/dev/debug-methodology.md` | Systematic debug logging methodology and Go gotchas |
