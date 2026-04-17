# Move Expansion Operations to compilation/

**Status**: Proposed
**Date**: 2026-04-12
**Related**: `plans/2026-03-30-machine-decomposition-design.md` (parent plan, Phase 2)

## Motivation

`machine/` mixes VM runtime (frame lifecycle) with expansion-time operations
(syntax-rules transform, syntax-case, build-syntax) and their bridge types.
The environment-dependency analysis shows two distinct groups:

1. **Frame Lifecycle** — functions using `environment.EnvironmentFrame` as live
   mutable state (closure capture, continuation save/restore, pool management)
2. **Compiled Indexing & Expansion Support** — functions using `Binding`,
   `LocalIndex`, `GlobalIndex` as compiled metadata, plus expansion-time
   VM operations

Group 2's expansion operations are already produced by `compilation/` (the
compiler creates `SyntaxRulesClause`, `SyntaxCaseClause`, emits
`NewOperationSyntaxRulesTransform()`, etc.) and dispatched polymorphically by
the VM through the `InlinedOperation` interface. Moving them completes the
producer-consumer colocation and removes expansion-specific code from the VM
package.

## Files Moving

| File | LOC | Types/Functions |
|------|-----|-----------------|
| `operation_syntax_rules_transform.go` | ~230 | `OperationSyntaxRulesTransform`, `envBindingChecker` |
| `operation_syntax_case.go` | ~300 | `OperationSyntaxCaseMatch`, `OperationBindPatternVars`, `OperationSyntaxCaseNoMatch`, `OperationSyntaxTemplateExpand`, `OperationStoreSyntaxCaseInput`, `OperationClearSyntaxCaseInput`, `syntaxCaseState` |
| `operation_build_syntax.go` | ~77 | `OperationBuildSyntaxList` |
| `syntax_bridge_types.go` | ~126 | `FreeIdResolution`, `SyntaxRulesClause`, `ClausesWrapper`, `SyntaxCaseClause` |
| **Total** | **~733** | |

Plus their test files:
- `operation_syntax_case_test.go`
- `operation_build_syntax_test.go`
- (syntax_rules_transform tests are in `syntax_rules_test.go` — integration tests stay)

## What Stays in machine/

- `NativeTemplate`, `Instruction`, `OpCode` — bytecode representation
- All non-expansion operations (`operations_*.go`, `call_promoted*.go`, etc.)
- `OperationBase`, `sameType`, `fieldMatches` — operation infrastructure
- `MachineContext`, `MachineClosure`, `ForeignClosure` — VM state and closures
- `MachineContinuation`, `Stack`, pool, winding, exceptions — frame lifecycle
- `ExpanderCtx`, `MacroEvaluator` — anti-cycle interfaces (by design)
- `disassemble.go` — public diagnostic API, referenced by engine and extensions
- `CallContext` — extension-facing interface

## Interface Changes

### 1. syntaxCaseState → opaque field on MachineContext

`syntaxCaseState` is an unexported struct stored on `MachineContext.syntaxCase`.
After the move, compilation/ cannot define fields on machine/ types.

**Solution:** Change the field to `any` with accessor methods. Same pattern as
`expanderCtx ExpanderCtx`.

In `machine/machine_context.go`:
```go
// Before:
syntaxCase *syntaxCaseState

// After:
syntaxCase any // *compilation.syntaxCaseState; nil when not active
```

Add accessor methods on MachineContext:
```go
func (p *MachineContext) SyntaxCaseState() any     { return p.syntaxCase }
func (p *MachineContext) SetSyntaxCaseState(v any) { p.syntaxCase = v }
```

In compilation/, `ensureSyntaxCaseState` type-asserts from `any`:
```go
func ensureSyntaxCaseState(mc *machine.MachineContext) *syntaxCaseState {
    if v := mc.SyntaxCaseState(); v != nil {
        return v.(*syntaxCaseState)
    }
    state := &syntaxCaseState{}
    mc.SetSyntaxCaseState(state)
    return state
}
```

### 2. Export sameType and fieldMatches

The moving operations use `sameType` and `fieldMatches` (unexported helpers in
`operation_helpers.go`). These are generic comparison helpers with no machine/
dependencies.

```go
// Before:
func sameType[T any](p, v *T, ok bool) bool
func fieldMatches[T comparable, Op any](p, v *Op, ok bool, getField func(*Op) T) bool

// After:
func SameType[T any](p, v *T, ok bool) bool
func FieldMatches[T comparable, Op any](p, v *Op, ok bool, getField func(*Op) T) bool
```

Update all existing callers in machine/ (mechanical rename). The moving files
use `machine.SameType` and `machine.FieldMatches`.

### 3. Bridge types become compilation-local

`SyntaxRulesClause`, `SyntaxCaseClause`, `ClausesWrapper`, `FreeIdResolution`
move from `machine` to `compilation`. These are already created by compilation/
(currently as `machine.SyntaxRulesClause`, etc.).

After the move:
- compilation/ references them directly (no `machine.` prefix)
- No external consumers — these types are only used within machine/ and
  compilation/ today. `internal/match/` uses `FreeIdResolution` only via
  interfaces (`localScopesProvider`, `globalBindingProvider`, etc.), not
  by concrete type.

## Dependency Flow

Before:
```
compilation/ ──imports──► machine/
                          (SyntaxRulesClause, operations, OperationBase, MachineContext)
```

After:
```
compilation/ ──imports──► machine/
                          (OperationBase, SameType, FieldMatches, MachineContext,
                           SetSyntaxCaseState/SyntaxCaseState accessors)
```

No new import directions. compilation/ → machine/ is the existing allowed
direction. machine/ never imports compilation/.

## Files NOT Moving

These were in the analysis Group 2 but have reasons to stay:

| File | Reason |
|------|--------|
| `native_template.go` | Core bytecode type, coupled to VM via `sideTable []InlinedOperation` |
| `instruction.go` | Core bytecode type, coupled to NativeTemplate.code |
| `operations_load_store.go` | VM operations, implement `InlinedOperation.Apply(*MachineContext)` — same as all other operations |
| `disassemble.go` | Public API, referenced by engine, extensions, REPL |
| `expander_ctx.go` | Anti-cycle interface, must stay in machine/ by design |
| `macro_evaluator.go` | Anti-cycle interface + impl, creates MachineContexts |

## Impact on External Consumers

**Zero changes:**
- All extensions, registry, internal packages
- Engine API (engine.go, disassemble.go)
- REPL

The moved types were never imported outside of machine/ and compilation/.

## Phasing

Single phase — the changes are small and mechanical:

1. Export `SameType` and `FieldMatches` in `operation_helpers.go`
2. Update all callers of `sameType`/`fieldMatches` in machine/ to capitalized names
3. Add `SyntaxCaseState()`/`SetSyntaxCaseState()` accessors to MachineContext
4. Change `MachineContext.syntaxCase` from `*syntaxCaseState` to `any`
5. Move the 4 source files + 2 test files to compilation/
6. Change `package machine` → `package compilation` in moved files
7. Add `machine.` prefix to: `MachineContext`, `OperationBase`,
   `NewOperationBase`, `NewOperationBaseWithGoName`, `SameType`, `FieldMatches`
8. Remove `machine.` prefix from: `SyntaxRulesClause`, `SyntaxCaseClause`,
   `ClausesWrapper`, `FreeIdResolution` (now local to compilation/)
9. Update compilation/ files that reference `machine.SyntaxRulesClause` etc.
   to use the now-local type names
10. `make lint && make covercheck`
