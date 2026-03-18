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
| `PERFORMANCE.md` | Allocation history, fused lexing research, Tier 3 architectural changes | Tier 1-2 complete; Tier 3 + fused lexing open |
| `SECURITY.md` | Extension-level sandboxing, authorization framework, opcode resource limits | Phases 1-6 done, resource limits proposed |
| `MACRO_SYSTEM.md` | ER macro transformer, hygiene debugging, macro expansion tracing | ER macros complete; hygiene debugging + tracing planned |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | Proposed |
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing | Open items remain |
| `CONTINUATION_MARKS.md` | Racket-style per-frame key-value annotations on the continuation chain | Proposed |
| `REMOVE-SYMBOL-INTERNING.md` | Remove symbol canonicalization, compare by string key | Complete (all 7 tasks verified 2026-03-14) |
| `ENVIRONMENT-CLEANUP.md` | Environment package cleanup: constructor duplication, dead delegation, semantic inconsistency | Complete (all 7 tasks verified 2026-03-14; Task 6 superseded) |
| `OPCODE-PROMOTION.md` | Promote hot primitives to dedicated opcodes; Larceny profiling data, tiered plan | Phase 1+2 complete (#497, #498); Phase 3 open |
| `FLAT-CLOSURES.md` | Flat closure implementation: 3-PR plan (infra+analysis, behavioral change, cleanup) + Machine Modernization Roadmap | **Reverted** — +7.4% geo-mean regression across 31 benchmarks (Larceny + Schelog + Kanren). Zero benchmarks improved. New `freeVars` slice allocation (+831 MB) exceeded savings from eliminated parent-chain walks. |
| `STACK-FRAMES.md` | Replace continuation pool with contiguous `[]callFrame` slice — 12 tasks, 3 PRs (infra, behavioral change, cleanup) | **Closed** — implemented and reverted. Dispatch improved 5% on fib but regressed continuation-heavy benchmarks 10-20%. Net negative. |
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
| `docs/dev/ENVIRONMENT_SYSTEM.md` | Environment system architecture |
| `docs/dev/NUMERIC_TOWER.md` | Numeric tower (direct dispatch, lattice model) |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Documented R7RS specification deviations |
| `docs/EXTENSIONS.md` | Extension system: architecture, authoring, registry, phases, FFI |
| `docs/EXTENSION_LIBRARIES.md` | R7RS library integration for extensions |
