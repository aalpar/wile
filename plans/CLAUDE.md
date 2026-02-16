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
| `PERFORMANCE_REFACTORING_PLAN.md` | Full-pipeline performance refactoring (8 phases) | Phase 1-2 complete, Phase 3+ remaining |
| `ENVIRONMENT_INTROSPECTION.md` | Read-only environment introspection primitives | Planned |
| `OPCODE_RESOURCE_LIMITS.md` | Per-category resource limits for VM opcodes | Design |
| `MACRO_EXPANSION_TRACING.md` | Trace macro-generated code to source | Planned |
| `HYGIENE_DEBUGGING_DESIGN.md` | Scope provenance and debugging primitives | Planned |

### Proposed Designs (Future)

| File | Purpose | Status |
|------|---------|--------|
| `EXTERNAL_EXTENSIONS_PLAN.md` | Public extension system — Phases 1-4 complete, 6/9 extensions public | In Progress |
| `PLUGIN_SHADOWING_DESIGN.md` | Primitive shadowing for extensions | Proposed |
| `AUTHORIZATION_FRAMEWORK.md` | K8s-style verb+resource authorization for sandboxing | Proposed |
| `FUSED_LEXING_PARSING.md` | Flap paper analysis + sketch for fusing tokenizer into parser | Research |

### Architectural Review

| File | Purpose | Status |
|------|---------|--------|
| `ARCHITECTURAL_REVIEW_FIXES.md` | Complete fix history (EXEMPT from cleanup) | Reference |
| `ARCHITECTURAL_REVIEW_REFACTORING.md` | Open refactoring: 2.1 deferred; 4.2, 4.4 low-priority remaining | Reference |
| `STRUCTURAL_ANALYSIS.md` | Dependency metrics, type precision | Reference |

### Testing & Methodology

| File | Purpose | Status |
|------|---------|--------|
| `SCHEME_TEST_INFRASTRUCTURE_PLAN.md` | Test infra complete, content pending | Infra complete |
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
