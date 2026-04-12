# plans/ -- Plan File Conventions

**Plans go in `plans/`.** Do not create plan files in any other location.

**Plan file naming**: Use `UPPERCASE-WITH-HYPHENS.md` (e.g., `362-BIGCOMPLEX-INFNAN-GUARD.md`) or date-prefixed `YYYY-MM-DD-description.md` for time-stamped designs. Issue-linked plans are prefixed with the issue number.

## R7RS Investigation Procedure

When investigating R7RS conformance issues:
1. Document each test BEFORE running it in `plans/R7RS_TEST_INVESTIGATION_IN_PROGRESS.md`
2. Save the file, run the test (max 15s timeout), log results, save again
3. Keep error summary at top; use bisection technique to isolate errors

## Plan Files

| File | Contents | Status |
|------|----------|--------|
| `PERFORMANCE.md` | Remaining optimizations (env frame slimming), benchmark baseline, fused lexing research | Procedure inlining **Complete** (PR #605); env frame slimming open; NaN-boxing blocked by unsafe |
| `UNBOXED-FLOAT-PIPELINE.md` | Three-layer unboxed float pipeline (value register, tagged stack, binding unboxing) to eliminate Float heap allocations in arithmetic loops | Proposed — 4 phases |
| `GC-PRESSURE-REDUCTION.md` | FreeList migration, pre-sized bindings, env frame leak fix | **Complete** (PRs #562-563) |
| `CORE-LET.md` | Core-let design: `let`/`let*`/`letrec`/`letrec*` as ValidatedExpr forms | **Complete** (design) |
| `OPAQUE-VALUES.md` | OpaqueValue type implementation plan | **Complete** (PR #566) |
| `2026-03-24-opaque-values-design.md` | OpaqueValue design document | **Complete** |
| `OS-PRIMITIVES.md` | SRFI-170 subset: directory ops + process extension | **Complete** Phase 1 (PR #565) |
| `2026-03-24-os-primitives.md` | OS primitives design brainstorm | **Complete** |
| `OPTIMIZER-FIX.md` | Fix `callForeignCached`/`applyForeign` double-restore | **Complete** (PR #573) |
| `2026-03-25-degenerate-form-tests.md` | Degenerate form full-pipeline tests | **Complete** (PR #571) |
| `2026-03-25-algebra-library-design.md` | Algebra library design document | **Complete** |
| `2026-03-25-algebra-rewrite-design.md` | Term rewriting library design | **Complete** |
| `2026-04-09-orthogonal-algebra-types.md` | Orthogonal algebra types: Heyting algebra, Boolean algebra (Monoid Action / Module deferred) | **Implemented** |
| `2026-04-10-orthogonal-algebra-phase2-design.md` | Orthogonal algebra phase 2: Setoid, Category, Closure Operator, Differential Ring | **Implemented** |
| `2026-04-10-orthogonal-algebra-phase2-impl.md` | Phase 2 implementation plan | **Implemented** |
| `2026-04-10-symbolic-algebra-design.md` | Symbolic algebra design: theory projections, recursive normalizer, traced rewriting | **Implemented** (PRs #632, #633) |
| `2026-04-10-symbolic-algebra-impl.md` | Symbolic algebra implementation plan | **Phases 1-2 complete**; Phase 3 (wile-goast integration) in wile-goast |
| `2026-04-11-eval-stack-limit-design.md` | Eval stack size limit: `WithMaxStackSize`, `checkStackSize`, `ErrStackOverflow` | **Complete** (PR #636) |
| `2026-04-11-eval-stack-limit-impl.md` | Eval stack limit implementation plan | **Complete** (PR #636) |
| `2026-04-11-eval-subcontext-design.md` | Funnel `prim_eval.go` through `NewSubContext`: `NewSubContextWithTemplate`, pool-backed release | **Complete** (PR #637) |
| `AVAILABLE-LIBRARIES.md` | Library discovery design: `LibraryEnumerator` interface, `(available-libraries)` primitive | **Approved** (design) |
| `CAPTURE-ANALYSIS.md` | Capture analysis design for let bindings (B1) | **Complete** |
| `CAPTURE-ANALYSIS-IMPL.md` | Capture analysis implementation plan | **Complete** |
| `ESCAPE-ANALYSIS.md` | Escape analysis design for let-bound closures (B2) | **Complete** (design) |
| `ESCAPE-ANALYSIS-IMPL.md` | Escape analysis implementation plan | **Complete** |
| `PROCEDURE-INLINING.md` | Procedure inlining for let-bound closures: synthetic let transform, BindingID-keyed candidate registry, configurable threshold | **Complete** (PR #605) |
| `2026-03-27-procedure-documentation-design.md` | `procedure-documentation` primitive: NativeTemplate doc field, compileBody extraction, Guile-style convention | **Complete** (PR #579) |
| `2026-03-27-scheme-library-docstrings-design.md` | Scheme library docstrings design: conventions, phasing, scope (300 procs across 29 files) | **Complete** |
| `2026-03-27-special-form-macro-docstrings-design.md` | Special form & macro docstrings: `BindingMeta.Doc`, `BindingSpec`, `DocEntry`, post-bootstrap `ApplyDocs` | **Complete** |
| `2026-04-06-structured-docstring-metadata-design.md` | Structured docstring metadata: parse `Parameters:`, `Returns:`, `Category:` from Guile-style docstrings for `,doc`/`,apropos`/`,topics` parity | **Complete** |
| `2026-03-26-extension-contracts-design.md` | Extension API contract system: ValueType enum, PrimitiveSpec type declarations, validation, doc integration | **Approved** (design) |
| `EXTENSIBLE-TYPE-CONSTRAINTS.md` | Extensible type constraint system: `TypeConstraint` interface replacing closed `ValueType` enum | **Implemented** (PR #629) |
| `2026-04-09-extensible-type-constraints-impl.md` | TypeConstraint interface implementation plan | **Implemented** (PR #629) |
| `2026-03-26-extension-contracts-impl.md` | Extension contracts remaining work: Phases 2-4 outlines (Phase 1 complete) | Open |
| `2026-03-26-extension-contracts-phase2-design.md` | Extension contracts phase 2 design: ForeignClosure validation, auto-coercion | Open |
| `2026-03-26-extension-contracts-phase2-impl.md` | Extension contracts phase 2 implementation plan | Open |
| `2026-03-26-environment-profiles.md` | Environment profiles design: named profiles (Tiny, Console, Small, KitchenSink), sandbox modifier | Draft |
| `2026-03-26-environment-profiles-impl.md` | Environment profiles implementation plan | Draft |
| `2026-03-26-wile-mcp-server-design.md` | Wile MCP server design: standalone binary exposing Scheme evaluation | Draft |
| `SECURITY.md` | Opcode resource limits (match steps, expand steps, continuation copy depth) | Proposed — not implemented |
| `MACRO_SYSTEM.md` | Hygiene debugging, macro expansion tracing | Planned — not started |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | Proposed |
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing, environment introspection | All proposed |
| `TECH-DEBT-2026-04.md` | Tech debt assessment: 8 phases, 27 tasks. Phases 1-7 complete; Phase 8.5 done, 8.1-8.4 opportunistic | Assessment document |
| `TECH-DEBT-2026-04-IMPL.md` | Tech debt implementation: Phases 1-7 complete, Phase 8.5 complete (PR #637) | **Phases 1-7 + 8.5 Complete** |
| `2026-03-30-machine-decomposition-design.md` | Machine package decomposition: compiler/VM/expander separation | **Approved** (design) |
| `2026-03-31-environment-any-fields.md` | Replace `any` fields in Namespace with typed interfaces | **Complete** (PR #594) |
| `2026-03-31-high-risk-bugfixes.md` | Sub-context winding stack hazard + cond-expand FileResolver bypass | **Complete** (PR #597) |
| `2026-03-31-pulldrain-design.md` | O(1) PullDrain for OpPullApply dispatch | **Complete** (PRs #596, #598) |
| `2026-04-01-disassembler-design.md` | Bytecode disassembler: Go layer, Scheme primitive, REPL `,disasm`, MCP tool | **Complete** (PR #603) |
| `2026-04-01-engine-init-order.md` | Engine initialization order invariant: document + negative tests | **Complete** (PR #601) |
| `2026-04-01-timing-dependent-tests.md` | Replace timing-dependent `time.Sleep` with observation-based sync | **Complete** (PR #602) |
| `2026-04-03-syntax-rules-ellipsis-hygiene-design.md` | syntax-rules ellipsis + hygiene bug analysis (3 bugs from SRFI-42) | Bugs B+C complete; Bug A partial |
| `2026-04-03-syntax-rules-ellipsis-hygiene-impl.md` | syntax-rules bug fix implementation plan | Bugs B+C complete; Bug A partial |
| `2026-04-04-compilation-coverage.md` | machine/compilation coverage improvement (68.6% → 80%) | Partial (PR #608) |
| `2026-04-05-walk-sub-exprs-design.md` | Shared ChildRole visitor for macro expansion sub-expressions | **Approved** (design) |
| `COMPILATION-COVERAGE.md` | Compilation test coverage plan | Draft |
| `MCP-EVAL-HARDENING.md` | MCP eval tool hardening: timeout, output limits | **Complete** |
| `TEST-COVERAGE-AND-REFACTORING.md` | machine/ test coverage (52 files), engine.go tests, REPL tests, type switch linter, form dual-dispatch | Open |
| `2026-04-05-structural-reduction.md` | Full-codebase structural reduction: CallContext interface, promoted op table, Thread outcome type, plus 7 deferred findings | Phases 1, 3, D5 complete; Phase 2 rejected; D1 stale |
| `FIX-GUARD-MULTIPLE-VALUES.md` | `guard` body drops multiple values — `call-with-values` fix | Draft |
| `FIX-TUPLE-FOREACH-NIL.md` | Tuple ForEach nil returns Void instead of EmptyList | Draft |
| `UNIFY-ESCAPE-MECHANISMS.md` | Unified `call-with-exit` and prompt/abort escape paths | **Complete** (#418) |
| `MACHINE-TECH-DEBT.md` | Machine package debt: arity dedup, expander decomposition, letrec* unification, file splits | **Complete** (#444) |
| `CONTINUATION_MARKS.md` | Racket-style per-frame key-value annotations on the continuation chain | Proposed |
| `REMOVE-SYMBOL-INTERNING.md` | Remove symbol canonicalization, compare by string key | Proposed |
| `ENVIRONMENT-CLEANUP.md` | Environment package cleanup: constructor duplication, dead delegation, semantic inconsistency | Proposed |
| **Documentation Search** | | |
| `DOCUMENTATION-SEARCH.md` | Current-state architecture doc for doc search: `SearchDoc`, export index, keywords, topic browsing | **Current** — consolidated reference |
| `2026-03-27-apropos-topic-browsing-design.md` | Original apropos & topic browsing design | **Complete** |
| `2026-04-08-doc-keywords-design.md` | Keywords field design | **Complete** |
| `2026-04-08-doc-keywords-impl.md` | Keywords implementation plan | **Complete** |
| `2026-04-08-unified-apropos-design.md` | Unified `SearchDoc` design (Scheme + REPL) | **Complete** |
| `2026-04-08-unified-apropos-impl.md` | Unified search implementation plan | **Complete** |
| `2026-04-08-eager-doc-index-design.md` | Eager library metadata scan design | **Superseded** by `LIBRARY-EXPORT-INDEX.md` |
| `LIBRARY-EXPORT-INDEX.md` | Static export index design for unloaded library discovery | **Complete** (PRs #623, #625) |
| `LIBRARY-EXPORT-INDEX-IMPL.md` | Export index implementation plan | **Complete** (PRs #623, #624, #625) |
| `PRIM-APROPOS-EXPORT-INDEX.md` | Fix Scheme-level `(apropos)` asymmetry (export index on Namespace) | Proposed (deferred) |
| `2026-04-07-public-repl-api-design.md` | Public REPL API for embedders: Engine-centric components, docparse promotion | **Implemented** (PR #617) |
| `2026-04-07-public-repl-api-impl.md` | Public REPL API implementation plan | **Implemented** (PR #617) |
| `2026-04-06-structured-docstring-metadata-design.md` | Structured docstring metadata: `Parameters:`, `Returns:`, `Category:` from Guile-style docstrings | **Complete** |
| `2026-04-06-structured-docstring-metadata-impl.md` | Structured docstring metadata implementation plan | **Complete** |
| `2026-03-28-library-level-documentation-design.md` | Library-level documentation (description fields, metadata) | **Complete** |
| `2026-03-29-doc-examples-filtering.md` | Example filtering in documentation display | **Complete** |
| `2026-04-07-documentation-gaps-plan.md` | Documentation coverage gap analysis | **Phases 1-4 Complete** |

Go AST/static analysis plans moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans).

## Before Starting Work

**ALWAYS check existing project artifacts before planning or proposing solutions:**

1. **Check `plans/` directory** -- Read relevant plan files to understand existing design decisions, phase status, and what's already been explored
2. **Check `TODO.md`** -- Verify the task isn't already completed or documented as deferred
3. **Check existing patterns** -- Search the codebase for prior art before proposing new designs

**Do not:**
- Create new plan files without reading existing ones in `plans/`
- Propose architectural approaches without checking how similar problems are already solved
- Start implementation without verifying assumptions against actual code

## Developer Documentation (outside plans/)

| File | Purpose |
|------|---------|
| `docs/dev/CONTINUATION_WORKLOAD_OPTIMIZATIONS.md` | Performance optimization guide |
| `docs/dev/DEBUG_METHODOLOGY.md` | Systematic debug logging methodology and Go gotchas |

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
