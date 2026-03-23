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
| `PERFORMANCE.md` | Remaining optimizations (procedure inlining, env frame slimming), benchmark baseline, fused lexing research | Procedure inlining + env frame slimming open; NaN-boxing blocked by unsafe |
| `UNBOXED-FLOAT-PIPELINE.md` | Three-layer unboxed float pipeline (value register, tagged stack, binding unboxing) to eliminate Float heap allocations in arithmetic loops | Proposed — 4 phases |
| `SECURITY.md` | Opcode resource limits (match steps, expand steps, continuation copy depth) | Proposed — not implemented |
| `MACRO_SYSTEM.md` | Hygiene debugging, macro expansion tracing | Planned — not started |
| `DEBUGGER.md` | Inline breakpoint traps, snap-to-next breakpoint resolution | Proposed |
| `ARCHITECTURE.md` | Dialect system, module decomposition, plugin shadowing, environment introspection | All proposed |
| `TEST-COVERAGE-AND-REFACTORING.md` | machine/ test coverage (52 files), engine.go tests, REPL tests, type switch linter, form dual-dispatch | Open |

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
