# plans/ -- Plan File Conventions

**Plans go in `plans/`.** Do not create plan files in any other location.

**Plan file naming**: ALL CAPS. Use `UPPERCASE-WITH-HYPHENS.md` (e.g., `362-BIGCOMPLEX-INFNAN-GUARD.md`). Issue-linked plans are prefixed with the issue number.

## R7RS Investigation Procedure

When investigating R7RS conformance issues:
1. Document each test BEFORE running it in `plans/R7RS_TEST_INVESTIGATION_IN_PROGRESS.md`
2. Save the file, run the test (max 15s timeout), log results, save again
3. Keep error summary at top; use bisection technique to isolate errors

## Plan Files

| File | Contents | Status |
|------|----------|--------|
| `PERFORMANCE.md` | Allocation optimization (completed fixes + remaining tiers), block-allocated pairs (complete), unified pool manager (complete), fused lexing research | Mixed |
| `SECURITY.md` | Extension-level sandboxing model, authorization framework, opcode resource limits | Phases 1-6 done, rest proposed/design |
| `MACRO_SYSTEM.md` | ER macro transformer, hygiene debugging design, macro expansion tracing | Proposed/Planned |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | Proposed |
| `ARCHITECTURE.md` | Engine refactor (complete), dialect system, module decomposition, plugin shadowing, environment introspection | Mixed |
| `TESTING.md` | Scheme test expansion (complete) | Complete |
| `REPL_ENHANCEMENTS.md` | Meta-commands (,help, ,doc, ,edit), autocomplete, pager integration | Complete (see `2026-02-26-repl-enhancements.md`) |
| `2026-02-26-repl-enhancements.md` | Step-by-step implementation plan for REPL_ENHANCEMENTS | Complete |
| `SIGNALS_REVIEW.md` | Six-lens signals analysis: mode transitions, feedback loops, saturation, temporal coupling, cross-talk, signal integrity | Complete (PR #361) |
| `SIGNALS_REMEDIATION.md` | Implementation plan for SIGNALS_REVIEW findings P1–P6 + P4b | Complete (PR #361) |
| `TECH_DEBT_REVIEW.md` | Staff-engineer debt assessment: numeric dispatch duplication, extension friction, testing gaps, consistency debt | Reference (F1 complete; others open) |
| `STAFF_ENGINEER_REVIEW.md` | Net-new debt findings beyond TECH_DEBT_REVIEW: N1 VM opcode dedup, N2 VM error pattern | Reference (open) |
| `362-BIGCOMPLEX-INFNAN-GUARD.md` | Fix: BigFloat Inf/NaN-capable, BigComplex guard patched for Float×BigComplex case (#362) | Complete (PR #363) — approach differs from plan; see status block at top of file |
| `2026-02-27-crosscheck-design.md` | Design doc for crosscheck plugin (parallel cross-purpose agents) | Complete (implemented) |
| `2026-02-27-crosscheck-impl.md` | Step-by-step implementation plan for crosscheck plugin | Complete |
| `PRECISION-GUARANTEES.md` | Precision loss policy: tier model, known bugs, proposed PrecisionMode setting | Reference |
| `R7RS-CONFORMANCE-REVIEW.md` | Full R7RS-small conformance audit: 5 critical, 6 high, 8 medium, 7 low findings across all spec sections | Reference |
| `R7RS-PORT-IO-CONFORMANCE.md` | Detailed port/IO conformance report with bufio analysis | Reference |
| `R7RS-CONFORMANCE-FIXES.md` | Phased conformance fixes (C2, C3, L6, etc.) excluding H1, E1, L7 | Plan |
| `H1-APPLY-TAIL-POSITION.md` | apply as compile-time special form: OpUnpackListToStack + OpApply for proper tail recursion | Design approved |

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
| `docs/dev/ENVIRONMENT_SYSTEM.md` | Environment system architecture |
| `docs/dev/NUMERIC_TOWER.md` | Numeric tower (direct dispatch, lattice model) |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Documented R7RS specification deviations |
| `docs/EXTENSIONS.md` | Extension system: architecture, authoring, registry, phases, FFI |
| `docs/EXTENSION_LIBRARIES.md` | R7RS library integration for extensions |
