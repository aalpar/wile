# plans/ — Plan File Conventions

**Plans go in `plans/`.** Do not create plan files in any other location.

**Plan file naming**: Use `UPPERCASE_WITH_UNDERSCORES.md` (e.g., `OPTIMIZATION_PLAN.md`).

## R7RS Investigation Procedure

When investigating R7RS conformance issues:
1. Document each test BEFORE running it in `plans/R7RS_TEST_INVESTIGATION_IN_PROGRESS.md`
2. Save the file, run the test (max 15s timeout), log results, save again
3. Keep error summary at top; use bisection technique to isolate errors

## Plan Files

### Active Plans

| File | Purpose | Status |
|------|---------|--------|
| `PERFORMANCE_REFACTORING_PLAN.md` | Full-pipeline performance refactoring (Phase 7 remaining) | Phases 0–6 complete, Phase 7 remaining |
| `CONTINUATION_WORKLOAD_OPTIMIZATIONS.md` | Allocation reduction for continuation-heavy workloads (6 optimizations) | Complete |
| `ENVIRONMENT_INTROSPECTION.md` | Read-only environment introspection primitives | Planned |
| `OPCODE_RESOURCE_LIMITS.md` | Per-category resource limits for VM opcodes | Design |
| `MACRO_EXPANSION_TRACING.md` | Trace macro-generated code to source | Planned |
| `HYGIENE_DEBUGGING_DESIGN.md` | Scope provenance and debugging primitives | Planned |
| `CALL_WITH_EXIT_AND_WITH_BAFFLE.md` | S7-inspired escape continuation + continuation barrier | Planned |
| `BREAKPOINT_SNAP_TO_NEXT.md` | Snap-to-next breakpoint resolution for optimized bytecode | Proposed |
| `BREAKPOINT_INLINE_TRAPS.md` | Inline breakpoint traps — remove per-instruction debugger check from VM loop | Proposed |
| `COMPILER_OPTIMIZER.md` | Bytecode optimizer: peephole, constant folding, ops prealloc (Phase 5) | Complete |
| `INTEGER_OPCODE_DISPATCH.md` | Integer opcode dispatch — replace interface dispatch with switch for hot-path ops | Complete (design doc for Phase 6) |
| `PHASE6_SWITCH_DISPATCH.md` | Phase 6 implementation plan: switch dispatch for 17 hot-path ops, 3 waves | Complete |
| `APPLY_OPTIMIZATION_OPPORTUNITIES.md` | Apply call path optimization opportunities | Reference |
| `ER_MACRO_TRANSFORMER.md` | `er-macro-transformer` (explicit renaming macros) | Proposed |
| `SIGNALS_ANALYSIS.md` | Full codebase review from a signals engineering perspective (2026-02-17) | Reference |
| `STRUCTURAL_REDUCTION_ANALYSIS.md` | Full codebase review — dependency minimization, state tightness, composability (2026-02-17) | Reference |

### Proposed Designs (Future)

| File | Purpose | Status |
|------|---------|--------|
| `EXTERNAL_EXTENSIONS_PLAN.md` | Public extension system — Phase 5 + Tier 2 remaining | Phase 5+ remaining |
| `PLUGIN_SHADOWING_DESIGN.md` | Primitive shadowing for extensions | Proposed |
| `AUTHORIZATION_FRAMEWORK.md` | K8s-style verb+resource authorization for sandboxing | Proposed |
| `FUSED_LEXING_PARSING.md` | Flap paper analysis + sketch for fusing tokenizer into parser | Research |

### Architectural Review

| File | Purpose | Status |
|------|---------|--------|
| `ARCHITECTURAL_REVIEW_FIXES.md` | Complete fix history (EXEMPT from cleanup) | Reference |
| `ARCHITECTURAL_REVIEW_REFACTORING.md` | Open refactoring: 2.1 deferred; 4.2, 4.4 low-priority remaining | Reference |
| `STRUCTURAL_ANALYSIS.md` | Dependency metrics, type precision | Reference |

### Reference

| File | Purpose | Status |
|------|---------|--------|
| `SYSTEMATIC_DEBUG_LOGGING.md` | Debug methodology (pattern-based) | Reference |

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
