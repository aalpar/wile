# plans/ — Plan File Conventions

**Plans go in `plans/`.** Do not create plan files in any other location.

**Plan file naming**: Use `UPPERCASE_WITH_UNDERSCORES.md` (e.g., `OPTIMIZATION_PLAN.md`).

## R7RS Investigation Procedure

When investigating R7RS conformance issues:
1. Document each test BEFORE running it in `plans/R7RS_TEST_INVESTIGATION_IN_PROGRESS.md`
2. Save the file, run the test (max 15s timeout), log results, save again
3. Keep error summary at top; use bisection technique to isolate errors

## Plan Files

### Project Overview

| File | Purpose | Status |
|------|---------|--------|
| `PROJECT_STATUS.md` | Comprehensive project status and known gaps | Reference |

### Active Plans (Not Started)

| File | Purpose | Status |
|------|---------|--------|
| `TOKENIZER_CONSOLIDATION_PLAN.md` | Number parsing consolidation (~295 LOC savings) | Planned |
| `EMPTY_LIST_VOID_REFACTORING.md` | EmptyList gets its own type (not `*Pair`) | Planned |
| `MACRO_EXPANSION_TRACING.md` | Trace macro-generated code to source | Planned |
| `HYGIENE_DEBUGGING_DESIGN.md` | Scope provenance and debugging primitives | Planned |
| `PERFORMANCE_REFACTORING_PLAN.md` | Full-pipeline performance refactoring (8 phases) | Planned |

### Proposed Designs (Future)

| File | Purpose | Status |
|------|---------|--------|
| `EXTERNAL_EXTENSIONS_PLAN.md` | Public extension system (EnvironmentAccess interface) | Proposed |
| `PLUGIN_ARCHITECTURE_PROPOSAL.md` | Three-layer plugin architecture | Proposed |
| `PLUGIN_SHADOWING_DESIGN.md` | Primitive shadowing for extensions | Proposed |
| `AUTHORIZATION_FRAMEWORK.md` | K8s-style verb+resource authorization for sandboxing | Proposed |

### Completed / Reference

| File | Purpose | Status |
|------|---------|--------|
| `SCHEME_TEST_INFRASTRUCTURE_PLAN.md` | Test infra complete, content pending | Infra complete |
| `ERROR_HANDLING_AUDIT.md` | All actionable items resolved; 2 acceptable `fmt.Errorf` remain | Complete |

### Architectural Review

| File | Purpose | Status |
|------|---------|--------|
| `ARCHITECTURAL_REVIEW.md` | 4 deferred LOW items (L3, L11, L15, L19) | Tracking |
| `ARCHITECTURAL_REVIEW_FIXES.md` | Complete fix history (EXEMPT from cleanup) | Reference |
| `ARCHITECTURAL_REVIEW_STAFF.md` | Staff-level analysis; P0-P1 panics done, helpers coverage done | Reference |
| `ARCHITECTURAL_REVIEW_REFACTORING.md` | Open refactoring opportunities (Tier 1 done, Tiers 2-4 open) | Reference |
| `STAFF_REVIEW_2026_02.md` | February 2026 staff review; P0-P2 complete, P3 open | Reference |

### Code Quality / Analysis

| File | Purpose | Status |
|------|---------|--------|
| `CODE_CONSOLIDATION_ARCHITECTURAL.md` | Operation code generation (optional, HIGH risk) | Reference |
| `ALGEBRAIC_REDUCTIONS.md` | Operation boilerplate (V), scope resolution deferred (VI) | Reference |
| `SYSTEMATIC_DEBUG_LOGGING.md` | Debug methodology (pattern-based) | Reference |

## Design Documents (outside plans/)

| File | Purpose |
|------|---------|
| `docs/design/CONTINUATION_ESCAPE_DESIGN.md` | First-class continuation escape mechanism |
| `docs/design/DELIMITED_CONTINUATIONS.md` | Delimited continuations: prompts, abort, composable |
| `docs/dev/ENVIRONMENT_SYSTEM.md` | Environment system architecture |
| `docs/dev/NUMERIC_TOWER.md` | Numeric tower (direct dispatch, lattice model) |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Documented R7RS specification deviations |
