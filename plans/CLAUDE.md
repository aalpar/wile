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
| `PERFORMANCE.md` | Remaining optimizations (procedure inlining, env frame slimming), benchmark baseline, fused lexing research | Procedure inlining + env frame slimming open; NaN-boxing blocked by unsafe |
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
| `AVAILABLE-LIBRARIES.md` | Library discovery design: `LibraryEnumerator` interface, `(available-libraries)` primitive | **Approved** (design) |
| `CAPTURE-ANALYSIS.md` | Capture analysis design for let bindings (B1) | **Complete** |
| `ESCAPE-ANALYSIS.md` | Escape analysis design for let-bound closures (B2) | **Complete** (design) |
| `PROCEDURE-INLINING.md` | Procedure inlining for let-bound closures: synthetic let transform, BindingID-keyed candidate registry, configurable threshold | **Complete** (PR #605) |
| `2026-03-27-procedure-documentation-design.md` | `procedure-documentation` primitive: NativeTemplate doc field, compileBody extraction, Guile-style convention | **Complete** (PR #579) |
| `2026-03-27-scheme-library-docstrings-design.md` | Scheme library docstrings design: conventions, phasing, scope (300 procs across 29 files) | **Complete** |
| `2026-03-27-special-form-macro-docstrings-design.md` | Special form & macro docstrings: `BindingMeta.Doc`, `BindingSpec`, `DocEntry`, post-bootstrap `ApplyDocs` | **Complete** |
| `2026-04-06-structured-docstring-metadata-design.md` | Structured docstring metadata: parse `Parameters:`, `Returns:`, `Category:` from Guile-style docstrings for `,doc`/`,apropos`/`,topics` parity | **Approved** |
| `2026-03-26-extension-contracts-design.md` | Extension API contract system: ValueType enum, PrimitiveSpec type declarations, validation, doc integration | **Approved** (design) |
| `2026-03-26-extension-contracts-impl.md` | Extension contracts remaining work: Phases 2-4 outlines (Phase 1 complete) | Open |
| `2026-03-26-environment-profiles.md` | Environment profiles design: named profiles (Tiny, Console, Small, KitchenSink), sandbox modifier | Draft |
| `2026-03-26-environment-profiles-impl.md` | Environment profiles implementation plan | Draft |
| `2026-03-26-wile-mcp-server-design.md` | Wile MCP server design: standalone binary exposing Scheme evaluation | Draft |
| `SECURITY.md` | Opcode resource limits (match steps, expand steps, continuation copy depth) | Proposed — not implemented |
| `MACRO_SYSTEM.md` | Hygiene debugging, macro expansion tracing | Planned — not started |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | Proposed |
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing, environment introspection | All proposed |
| `TEST-COVERAGE-AND-REFACTORING.md` | machine/ test coverage (52 files), engine.go tests, REPL tests, type switch linter, form dual-dispatch | Open |
| `2026-04-05-structural-reduction.md` | Full-codebase structural reduction: CallContext interface, promoted op table, Thread outcome type, plus 7 deferred findings | Phases 1, 3 complete; Phase 2 rejected (benchmarked ~1.5% regression) |

Go AST/static analysis plans moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans).
=======
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing | Open items remain |
| `FIX-GUARD-MULTIPLE-VALUES.md` | `guard` body drops multiple values — `call-with-values` fix | Draft |
| `FIX-TUPLE-FOREACH-NIL.md` | Tuple ForEach nil returns Void instead of EmptyList | Draft |
| `UNIFY-ESCAPE-MECHANISMS.md` | Unified `call-with-exit` and prompt/abort escape paths | Completed (#418) |
| `MACHINE-TECH-DEBT.md` | Machine package debt: arity dedup, expander decomposition, letrec* unification, file splits | Complete (#444) |
| `CONTINUATION_MARKS.md` | Racket-style per-frame key-value annotations on the continuation chain | Proposed |
| `REMOVE-SYMBOL-INTERNING.md` | Remove symbol canonicalization, compare by string key | Proposed |
| `ENVIRONMENT-CLEANUP.md` | Environment package cleanup: constructor duplication, dead delegation, semantic inconsistency | Proposed |
| `GO-AST.md` | Go AST extension design and phased implementation | Phases 1, 2 & 4 complete; Phase 3 not started |
| `GO-STATIC-ANALYSIS.md` | Go static analysis extensions umbrella design (SSA, callgraph, CFG, lint) | Phases 1-3 complete; Phase 4 not started |
| `GO-SSA-PHASE-1A.md` | SSA extension Phase 1A implementation plan (core instructions) | Complete |
| `GO-SSA-PHASE-1BC.md` | SSA extension Phase 1B+1C implementation plan (collections, type ops, closures) | Complete |
| `GO-CALLGRAPH-PHASE-2.md` | Callgraph extension implementation plan (Phase 2 of GO-STATIC-ANALYSIS) | Complete |
| `GO-CFG-PHASE-3.md` | CFG + dominance extension implementation plan (Phase 3 of GO-STATIC-ANALYSIS) | Complete |
| `GO-LINT-PHASE-4.md` | Analysis passes extension implementation plan (Phase 4 of GO-STATIC-ANALYSIS) | Not started |
| `GO-AST-PHASE-2.md` | AST extension Phase 2 implementation plan (concurrency, switch, advanced) | Complete (#480) |
| `GO-AST-PHASE-3.md` | AST extension Phase 3 implementation plan (comments, error recovery, generics) | Not started |

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
