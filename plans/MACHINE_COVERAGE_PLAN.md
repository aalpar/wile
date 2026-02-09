# Plan: Improve `machine/` Package Test Coverage

> **Cross-reference**: Detailed subplans exist for specific coverage areas:
> - `COMPILE_VALIDATED_COVERAGE.md` — `compile_validated.go` coverage (Categories D/E)
> - `QUASISYNTAX_COVERAGE_PLAN.md` — `compile_quasisyntax.go` coverage (Category E)

## Current State

Package coverage: **72.7%** (545 functions across 74 files).

- **92 functions at 0%** — 19 are interface boilerplate (`SchemeString`, `IsVoid`, `EqualTo`, `Name`), 73 are logic
- **30 files at 100%** — mostly operation bytecode files and small types
- Biggest gaps by file: `machine_context.go` (56.9%), `compile_time_continuation.go` (71.1%), `expander_time_continuation.go` (73.4%)

## Classification of Uncovered Code

### Category A: Getters/Setters (trivial, easy to test)

22 functions across `machine_context.go`, `machine_continuation.go`, `parameter.go`. These are one-line accessor methods that are called by external packages (registry/core) but not from machine-internal tests.

| File | Functions | Count |
|------|-----------|-------|
| `machine_context.go` | `ParentMC`, `EscapeCont`, `SetEscapeCont`, `SetPC`, `SetContext`, `Context`, `ExpanderContext`, `ExceptionHandler`, `SetExceptionHandler`, `WindingStack`, `SetWindingStack`, `PromptTag`, `SetPromptTag` | 13 |
| `machine_continuation.go` | `PromptTag`, `SetPromptTag`, `PromptHandler`, `SetPromptHandler` | 4 |
| `parameter.go` | `Value`, `SetValue`, `Converter`, `HasConverter` | 4 |
| `native_template.go` | `SourceMap` | 1 |

**Effort**: Very low. Table-driven tests, one file.

### Category B: Interface boilerplate (`SchemeString`, `IsVoid`, `EqualTo`)

19 functions across 7 files. These implement `values.Value` interface with trivial logic (nil checks, string literals, identity comparison).

| File | Count |
|------|-------|
| `composable_continuation.go` | 3 |
| `operation_push_wind.go` | 3 |
| `operation_pop_wind.go` | 3 |
| `parameter.go` | 3 |
| `primitive_expander.go` | 4 (`Name` included) |
| `prompt_tag.go` | 3 |
| `prompt_abort.go` | 1 (`Error`) |

**Effort**: Very low. Trivial assertions.

### Category C: Continuation/winding/prompt system (tested externally)

19 functions in `machine_context.go` and `machine_continuation.go` that implement `call/cc`, dynamic-wind unwinding/rewinding, delimited continuations, and exception handlers. These are thoroughly tested by `registry/core/prim_control_test.go` (181 test cases) and `registry/core/prim_prompt_test.go`, but coverage isn't counted because those tests are in a different package.

| File | Functions |
|------|-----------|
| `machine_context.go` | `FindEscapeContinuation`, `PushExceptionHandler`, `PopExceptionHandler`, `UnwindTo`, `RestoreWithWinding`, `FindPrompt`, `SliceContinuationAt`, `GraftContinuation`, `SaveContinuationWithPrompt` |
| `machine_context.go` | `RunWithEscapeHandling` (34.1%), `RewindTo` (15.4%), `RestoreWithWindingFrom` (42.1%) |
| `machine_continuation.go` | `NewMachineContinuationWithPrompt`, `DeepCopy` |
| `composable_continuation.go` | `NewComposableContinuation`, `Cont`, `WindingStack` |
| `operation_apply.go` | `applyComposableContinuation`, `applyParameter` |
| `dynamic_wind.go` | `Depth`, `FindCommonWindingPrefix` (60%) |

**Effort**: High. These require complex multi-step scenarios (capture continuation inside dynamic-wind inside sub-context). Duplicates existing integration tests.

### Category D: Legacy/Tier-2 compiler paths

13 functions in `compile_time_continuation.go` that handle forms going through the old (pre-validation) compilation path. These are used for extension forms (`syntax-case`, `import`, `define-library`, `include`, `cond-expand`, etc.) that pass through validation as `ValidatedLiteral` and are dispatched via `SyntaxCompiler` bindings.

| Function | Coverage | What it handles |
|----------|----------|-----------------|
| `CompileProcedureCall` | 0% | Old procedure call path (superseded by `compileValidatedCall`) |
| `compileProcedureArgumentList` | 0% | Helper for above |
| `compileBeginBody` | 0% | Old begin body (superseded by `CompileValidatedBegin`) |
| `CompilePrimitiveOrProcedureCall` | 62.5% | Dispatch to SyntaxCompiler or procedure call |
| `expandQuasiquote` | 26.5% | Quasiquote template expansion to list/cons/append |
| `expandQuasiquoteImproperList` | 0% | Improper list in quasiquote |
| `processCondExpand` | 0% | `cond-expand` clause processing |
| `processIncludeLibraryDeclarations` | 0% | `include` inside library declarations |
| `parseImportSetForSyntax` | 0% | `(for (lib ...) syntax)` import sets |
| `parseImportSetForTemplate` | 0% | `(for (lib ...) template)` import sets |
| `parseImportSetForMeta` | 0% | `(for (lib ...) (meta N))` import sets |
| `CompileCondExpand` | 71.7% | `cond-expand` compilation |
| `CompileDefineSyntax` | 72.6% | `define-syntax` compilation |

**Effort**: Medium. Most can be tested by compiling appropriate Scheme forms. Some (`processIncludeLibraryDeclarations`) need filesystem setup.

### Category E: Macro compilation (syntax-rules, quasisyntax, with-syntax)

15 functions across 3 files handling advanced macro features.

| File | Functions at 0% |
|------|-----------------|
| `compile_syntax_rules.go` | `compileClause`, `compileClauseWithEllipsis`, `collectFreeIdentifiers`, `GetHasLocalBinding` |
| `compile_quasisyntax.go` | `compileQuasisyntaxTemplate`, `expandQuasisyntax`, `expandQuasisyntaxList`, `quasisyntaxNeedsRuntime` (+ `CompileQuasisyntax` at 37.5%) |
| `compile_with_syntax.go` | `buildWithSyntaxBegin` (+ `CompileWithSyntax` at 28.6%, `compileWithSyntaxBody` at 70%) |

**Effort**: Medium. These need full runtime with `define-syntax` support (use `coverage_fullruntime_test.go` external test package pattern).

### Category F: Expander functions

11 functions in `expander_time_continuation.go` and `expander_context.go`.

| Function | What it does |
|----------|-------------|
| `ExpandPrimitiveForm` | Dispatches to registered primitive expander |
| `ExpandOnce` | Single-step macro expansion |
| `expandQuasisyntax` | Returns quasisyntax unchanged (expansion-time stub) |
| `expandUnsyntax` | Returns unsyntax unchanged |
| `expandUnsyntaxSplicing` | Returns unsyntax-splicing unchanged |
| `expandWithSyntax` | Expands with-syntax body |
| `expandLetrecSyntax` | Expands letrec-syntax (delegates to expandLetSyntaxImpl) |
| `expandSyntaxError` | Raises expansion-time error |
| `formatIrritants` | Formats syntax-error irritants |
| `expander_context.go: Expand` | Bridge: calls ExpandExpression |
| `expander_context.go: ExpandOnce` | Bridge: calls ExpandOnce |

**Effort**: Low-Medium. The stubs are trivial. `ExpandOnce`, `expandSyntaxError`, `expandLetrecSyntax` need macro setup.

### Category G: Miscellaneous

| File:Function | Coverage | Notes |
|--------------|----------|-------|
| `operation_foreign_function_call.go:goErrorToSchemeException` | 0% | Wraps Go errors as Scheme exceptions |
| `operation_syntax_case.go:Apply` | 0% | Runtime syntax-case match operation |
| `operation_syntax_rules_transform.go:HasBinding` | 0% | Binding check for pattern variable resolution |
| `operations.go:AsList` | 0% | Convert Operations to []values.Value |
| `register.go:registerSyntaxCompiler` | 20% | Registration helper |
| `operation_push_wind.go:Apply` | 71.4% | Push dynamic-wind frame |

## Prioritized Plan

### Phase 1: Low-hanging fruit (Categories A + B)

**Target**: +~40 functions covered, ~2-3% package coverage increase.

Create `go/machine/value_methods_test.go`:
- Test all `SchemeString`, `IsVoid`, `EqualTo` methods on: `Parameter`, `PromptTag`, `ComposableContinuation`, `PrimitiveExpander`, `OperationPushWind`, `OperationPopWind`, `ErrPromptAbort.Error()`
- Test all getters/setters on: `MachineContext`, `MachineContinuation`, `Parameter`, `NativeTemplate`
- These are all trivial one-liner assertions, table-driven

### Phase 2: Expander stubs + ExpandOnce (Category F)

**Target**: +~11 functions covered, ~1% package coverage increase.

Create `go/machine/expander_coverage_test.go`:
- Test `ExpandOnce` with a simple macro (define-syntax + single expansion step)
- Test `expandSyntaxError` with a `(syntax-error "msg" ...)` form
- Test `expandLetrecSyntax` with a `letrec-syntax` form
- Test `expandQuasisyntax`, `expandUnsyntax`, `expandUnsyntaxSplicing` return input unchanged
- Test `ExpandPrimitiveForm` dispatches to registered expander
- Test `expander_context.go` `Expand`/`ExpandOnce` bridge functions

### Phase 3: Macro compilation (Category E) — external test package

**Target**: +~15 functions covered, ~1.5% package coverage increase.

Add tests to `go/machine/coverage_fullruntime_test.go` (external package, has `runtime` import):
- `syntax-rules` with ellipsis patterns → exercises `compileClauseWithEllipsis`, `compileClause`, `collectFreeIdentifiers`
- `syntax-rules` with free identifiers that have local bindings → exercises `GetHasLocalBinding`
- `quasisyntax` with `unsyntax` and `unsyntax-splicing` → exercises `CompileQuasisyntax`, `compileQuasisyntaxTemplate`, `expandQuasisyntax`, `expandQuasisyntaxList`, `quasisyntaxNeedsRuntime`
- `with-syntax` with pattern bindings → exercises `CompileWithSyntax`, `compileWithSyntaxBody`, `buildWithSyntaxBegin`

### Phase 4: Continuation/winding system (Category C)

**Target**: +~19 functions covered, ~3% package coverage increase.

Create `go/machine/continuation_coverage_test.go`:
- Use foreign closures + `call/cc`-like patterns to exercise continuation capture/restore
- Test `RunWithEscapeHandling` with continuation escape
- Test `UnwindTo`/`RewindTo` with explicit winding stacks
- Test `FindPrompt`/`SliceContinuationAt`/`GraftContinuation` by constructing continuation chains with prompt tags
- Test `PushExceptionHandler`/`PopExceptionHandler` directly on `MachineContext`
- Test `DeepCopy` on `MachineContinuation`
- Test `ComposableContinuation` constructor and accessors

This is the hardest phase. Consider whether the value justifies the effort given these are already tested by `registry/core`.

### Phase 5: Legacy compiler paths (Category D)

**Target**: +~8 functions covered, ~1% package coverage increase.

Selective tests for paths that are actually reachable:
- `expandQuasiquote` with improper list → exercises `expandQuasiquoteImproperList`
- `cond-expand` with `(library ...)` feature → exercises `processCondExpand`
- `CompileProcedureCall` and `compileProcedureArgumentList` may be dead code if all calls now go through `compileValidatedCall`. Verify and either test or remove.
- `parseImportSetForSyntax/ForTemplate/ForMeta` are R6RS features not used in R7RS — may be dead code. Verify.

### Phase 6: Cleanup

- Identify and remove dead code (functions at 0% that have no callers)
- `compileBeginBody` appears to be dead code (superseded by `CompileValidatedBegin`)
- `CompileProcedureCall` may be dead (superseded by `compileValidatedCall`)
- R6RS import set variants may be dead

## Expected Impact

| Phase | Functions Covered | Est. Coverage Increase | Effort |
|-------|-------------------|----------------------|--------|
| 1: Getters/boilerplate | ~40 | +2-3% | Low |
| 2: Expander stubs | ~11 | +1% | Low |
| 3: Macro compilation | ~15 | +1.5% | Medium |
| 4: Continuations | ~19 | +3% | High |
| 5: Legacy compiler | ~8 | +1% | Medium |
| 6: Dead code removal | N/A | +1-2% (fewer uncovered lines) | Low |
| **Total** | **~93** | **+9-11%** → ~82-84% | |

## Non-Goals

- Do not aim for 100% on `machine_context.go`. The continuation system is inherently integration-tested. Duplicating those tests at the unit level adds maintenance cost without catching new bugs.
- Do not test error propagation branches (`if err != nil { return err }`) that require impossible internal states.
- Do not test `RunWithEscapeHandling` exhaustively — it's a complex state machine already covered by 181 control flow tests in `registry/core`.

## File Organization

| New File | Package | Tests |
|----------|---------|-------|
| `value_methods_test.go` | `machine` | Phase 1: boilerplate + accessors |
| `expander_coverage_test.go` | `machine` | Phase 2: expander stubs |
| `coverage_fullruntime_test.go` (append) | `machine_test` | Phase 3: macro compilation |
| `continuation_coverage_test.go` | `machine` | Phase 4: continuation system |
