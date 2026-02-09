# ApplyCallable Refactoring — Full Picture

## What Was Done

Unified procedure dispatch across the VM. Before this change, 5 call sites
independently type-switched over Scheme's callable types. After, there is one
canonical dispatcher (`MachineContext.ApplyCallable`) and all polymorphic call
sites delegate to it.

### Callable Type Inventory

| Type | Scheme construct | Dispatch mechanism |
|------|------------------|--------------------|
| `*MachineClosure` | `lambda`, most builtins | `Apply`: bind params, set template/pc |
| `*CaseLambdaClosure` | `case-lambda` | `ApplyCaseLambda`: arity match → `Apply` |
| `*Parameter` | `make-parameter` | `applyParameter`: get/set, converter |
| `*ComposableContinuation` | `call-with-composable-continuation` | `applyComposableContinuation`: graft frames |

### API Tiers

```
ApplyCallable(values.Value, ...values.Value)   ← polymorphic callee (runtime dispatch)
  ├── Apply(*MachineClosure, ...)               ← statically-known closure
  ├── ApplyCaseLambda(*CaseLambdaClosure, ...)  ← statically-known case-lambda
  ├── applyParameter(...)                       ← private, parameter objects
  └── applyComposableContinuation(...)          ← private, delimited continuations
```

`ApplyCallable` is for sites that receive a `values.Value` and don't know the
concrete type. Sites that already hold a typed pointer (e.g., `*MachineClosure`
from `DynamicWindFrame.After`) should call `Apply`/`ApplyCaseLambda` directly —
no indirection cost, no type-switch branch that can't fail.

### Files Changed

| File | Change |
|------|--------|
| `machine/machine_context.go` | Added `ApplyCallable`, `returnImmediate`, `applyParameter` (moved), `applyComposableContinuation` (moved), `immediateReturnTemplate` |
| `machine/operation_apply.go` | Simplified to delegate to `ApplyCallable`; removed two functions; trimmed stale doc |
| `registry/core/prim_control.go` | `PrimApply`: replaced MC/CLC type switch with `sub.ApplyCallable(proc, ...)` |
| `internal/extensions/exceptions/prim_exceptions.go` | `PrimWithExceptionHandler`, `callExceptionHandler`: replaced type switches |
| `ffi.go` | Merged MC/CLC dispatch into `sub.ApplyCallable(v, ...)`, kept Parameter fast-path |
| `values/array_list.go` | Documentation only (unrelated) |

### Key Design Decision: `immediateReturnTemplate`

Parameters and composable continuations don't use bytecode — they complete
during the `Apply` phase. The standard sub-context lifecycle is:

```
sub.ApplyCallable(callable, args...)  →  sub.Run()  →  sub.GetValue()
```

`Run()` iterates `template.operations[pc..]`. For a `*MachineClosure`, `Apply`
sets `template` and `pc=0`, so `Run` executes the closure's bytecode. But
parameters have no bytecode. The old code returned `ErrMachineHalt` from the
sub-context, which forced every caller to treat halt as success — conflating
"parameter returned" with "VM ran out of operations."

`immediateReturnTemplate` solves this: it's an empty `*NativeTemplate` (0
operations). Setting it as the template causes `Run()` to return `nil`
immediately (the `for pc < len(ops)` loop body never executes). This lets the
standard `Apply → Run → GetValue` lifecycle work uniformly across all callable
types without special-casing halt at the caller.

### Key Design Decision: `returnImmediate`

The "return from a non-bytecode callable" epilogue appeared twice in
`applyParameter` (get and set branches). Extracted to `returnImmediate()`:

```go
func (p *MachineContext) returnImmediate() (*MachineContext, error) {
    if p.cont != nil {
        p.Restore(p.cont)      // bytecode path: restore saved continuation
    } else {
        p.template = immediateReturnTemplate  // sub-context: make Run() a no-op
        p.pc = 0
    }
    return p, nil
}
```

This names the concept and prevents the two sites from drifting.

---

## Remaining Work

### 1. `engine.go` — Merge `callClosure`/`callCaseLambda`, keep the rest

**Priority**: Medium — real duplication, but scoped to two methods.

**Status**: Revised from original "collapse all 3 into `ApplyCallable`" proposal.
Full collapse has too many boundary implications for a cleanup refactoring.

`Engine.Call()` type-switches the callee into three private methods:

```
engine.go:232  Call() → switch {
engine.go:257    callClosure()         — NewSubContext → Apply → Run → GetValue
engine.go:275    callCaseLambda()      — NewSubContext → ApplyCaseLambda → Run → GetValue
engine.go:293    callParameter()       — direct read/write, no sub-context
engine.go:249    ComposableContinuation → error
}
```

`callClosure` and `callCaseLambda` are identical except for the `Apply` vs
`ApplyCaseLambda` call — that's genuine duplication worth fixing.

`callParameter` is NOT the same lifecycle. It never creates a `MachineContext`.
It reads `param.Value()` or writes `param.SetValue()` directly from Go. Three
reasons to leave it alone:

1. **Performance at the embedding boundary.** Collapsing into `ApplyCallable`
   means every `Engine.Call()` on a parameter allocates a `MachineContext`, a
   sub-context, runs `applyParameter` (sets `immediateReturnTemplate`), calls
   `Run()` (enters loop, checks `pc < len(ops)`, exits), then `GetValue()`.
   That's substantial machinery for a field read. For Go programs calling
   parameters in tight loops, this matters.

2. **Error type change at the public API.** Current `callParameter` returns
   `*RuntimeError` directly:
   ```go
   return nil, &RuntimeError{Message: "parameter: converter error", Cause: err}
   ```
   After collapsing, `applyParameter` returns `*SchemeError` (via `mc.Error()`),
   which `wrapRuntimeError` wraps as `*RuntimeError{Cause: *SchemeError{...}}`.
   Anyone doing `errors.As(err, &schemeErr)` now finds a `*SchemeError` where
   none existed before. This is a breaking change at the embedding boundary.

3. **~24 lines is not worth the API risk.** The duplication between
   `callParameter` and `applyParameter` is real but small. The two serve
   different callers (Go embedders vs VM internals) with different error
   contracts.

The `ComposableContinuation` rejection must also stay. A composable continuation
splices captured frames onto the current continuation chain. When called from
`Engine.Call()`, the sub-context's `cont` is `nil`. `GraftContinuation(segment,
nil)` sets the bottom frame's parent to nil. The captured bytecode runs, but if
it expects to return a value to its prompt handler (the entire point of
composable continuations), that handler doesn't exist in the Go-initiated
context. The handler's logic is silently skipped. Removing the explicit
rejection turns a clear error into a subtle semantic bug.

**Proposed replacement** — merge the closure methods, keep everything else:

```go
func (p *Engine) Call(ctx context.Context, proc Value, args ...Value) (Value, error) {
    unwrappedArgs := make([]values.Value, len(args))
    for i, arg := range args {
        unwrappedArgs[i] = unwrapValue(arg)
    }
    callee := unwrapValue(proc)
    switch cls := callee.(type) {
    case *machine.MachineClosure:
        return p.callSchemeProc(ctx, cls, unwrappedArgs)
    case *machine.CaseLambdaClosure:
        mcls, ok := cls.FindMatchingClause(len(unwrappedArgs))
        if !ok {
            return nil, &RuntimeError{
                Message: fmt.Sprintf("no matching clause in case-lambda for %d arguments", len(unwrappedArgs)),
            }
        }
        return p.callSchemeProc(ctx, mcls, unwrappedArgs)
    case *machine.Parameter:
        return p.callParameter(ctx, cls, unwrappedArgs)
    case *machine.ComposableContinuation:
        return nil, &RuntimeError{Message: "cannot call composable continuation from Go"}
    default:
        return nil, &RuntimeError{Message: "not a procedure"}
    }
}
```

Where `callSchemeProc` replaces both `callClosure` and `callCaseLambda`:

```go
func (p *Engine) callSchemeProc(ctx context.Context, cls *machine.MachineClosure, args []values.Value) (Value, error) {
    tpl := machine.NewNativeTemplate(0, 0, false)
    cont := machine.NewMachineContinuation(nil, tpl, p.env)
    mc := machine.NewMachineContext(ctx, cont)

    sub := mc.NewSubContext()
    _, err := sub.Apply(cls, args...)
    if err != nil {
        return nil, p.wrapRuntimeError(err)
    }
    err = sub.Run()
    if err != nil && !errors.Is(err, machine.ErrMachineHalt) {
        return nil, p.wrapRuntimeError(err)
    }
    return wrapValue(sub.GetValue()), nil
}
```

**Gains**: Deletes ~15 lines (`callCaseLambda` disappears, `callClosure` renamed).
**Preserves**: Error types, parameter fast-path, ComposableContinuation rejection.
**Principle**: At embedding boundaries, explicit control over each callable
type's behavior is more valuable than consistency with internal dispatch.

### 2. `ffi.go` — Document or close the `ComposableContinuation` gap

**Priority**: Low — unlikely to hit in practice.

The validation switch at `ffi.go:543` accepts `MachineClosure |
CaseLambdaClosure | Parameter` but not `ComposableContinuation`. Then
`ApplyCallable` on line 572 handles all four types. The gap means passing a
composable continuation as a Go callback produces `"expected procedure"` before
`ApplyCallable` sees it.

This is probably correct — composable continuations splice continuation frames
and don't return normally, which is incompatible with Go callback return
conventions. But the gap is undocumented.

**Action**: Add a comment to the validation switch:

```go
// ComposableContinuation intentionally excluded: splices continuation
// frames, incompatible with Go callback return convention.
```

### 3. Remaining `sub.Apply()` call sites

**Priority**: None — these are correct as-is.

~20 sites across the codebase call `sub.Apply(cls, ...)` with a statically-typed
`*MachineClosure`. These are NOT candidates for `ApplyCallable` — the callee
type is known at compile time. Examples:

| File | Why `Apply` is correct |
|------|----------------------|
| `machine_context.go:642,667,717` | `DynamicWindFrame.Before/After` typed `*MachineClosure` |
| `prim_control.go:153` | `PrimCallCC` already resolved to `*MachineClosure` |
| `prim_parameters.go:52` | `Converter()` returns `*MachineClosure` |
| `prim_prompt.go:105,238` | Already type-asserted to `*MachineClosure` |
| `prim_vectors.go:346,415` | Already type-asserted to `*MachineClosure` |
| `prim_lists.go:414,498` | Already type-asserted to `*MachineClosure` |
| `prim_files.go:146,180` | Already type-asserted to `*MachineClosure` |
| `prim_threads.go:128` | Already type-asserted to `*MachineClosure` |

These sites use the **typed tier** of the API (`Apply`/`ApplyCaseLambda`), not
the polymorphic tier (`ApplyCallable`). Converting them would add a type switch
that always takes the same branch — pure overhead.

### 4. `immediateReturnTemplate` mutability

**Priority**: Low — theoretical risk only.

`immediateReturnTemplate` is a package-level `var` pointing to a mutable
`*NativeTemplate`. Nothing currently mutates it, but nothing prevents it either.

**Action**: Add an invariant comment:

```go
// INVARIANT: must remain empty (0 operations). Do not append to this template.
var immediateReturnTemplate = &NativeTemplate{}
```

### 5. `ArrayList` empty-list encodings

**Priority**: Low — not related to `ApplyCallable`, observed during documentation.

`IsEmptyList` recognizes three representations: `[EmptyList]`, `[nil, nil]`, and
`[Void, Void]`. `ArrayListEmptyList` uses `NewArrayList(nil, nil)`.

A future cleanup could normalize to a single encoding (`[EmptyList]`), removing
the multi-way checks from `IsEmptyList`, `IsList`, and `SchemeString`. Requires
auditing all `ArrayList` construction sites to ensure none produce the
`[nil, nil]` form.

---

## Architecture Diagram

Before:

```
OperationApply ──┐
PrimApply ───────┤
PrimWithExc ─────┤  each site has its own
callExcHandler ──┤  switch { MC, CLC, Param, CC }
ffi callback ────┘
```

After:

```
OperationApply ──┐
PrimApply ───────┤
PrimWithExc ─────┼──> mc.ApplyCallable(callable, args...)
callExcHandler ──┤      ├── Apply(*MachineClosure)
ffi callback ────┘      ├── ApplyCaseLambda(*CaseLambdaClosure)
                        ├── applyParameter(*Parameter)
                        └── applyComposableContinuation(*ComposableContinuation)
```

After engine.go cleanup (revised — Engine keeps its own dispatch):

```
                  ┌── polymorphic tier (runtime dispatch) ──┐
                  │                                         │
OperationApply ──┐│                                         │
PrimApply ───────┤│                                         │
PrimWithExc ─────┼┼──> mc.ApplyCallable(callable, args...)  │
callExcHandler ──┤│      ├── Apply(*MachineClosure)         │
ffi callback ────┘│      ├── ApplyCaseLambda(...)           │
                  │      ├── applyParameter(...)            │
                  │      └── applyComposableContinuation(.) │
                  └─────────────────────────────────────────┘

                  ┌── typed tier (no dispatch) ─────────────┐
                  │                                         │
DynamicWind ─────┐│                                         │
PrimCallCC ──────┤│                                         │
PrimParameters ──┼┼──> mc.Apply(*MachineClosure)            │
PrimPrompt ──────┤│                                         │
PrimVectors ─────┘│                                         │
                  └─────────────────────────────────────────┘

                  ┌── embedding boundary (explicit control) ┐
                  │                                         │
Engine.Call() ────┼──> switch callee.(type) {               │
                  │      MC, CLC → callSchemeProc(mc, ...)  │
                  │      Parameter → callParameter(direct)  │
                  │      CC → error (no prompt context)     │
                  │    }                                    │
                  └─────────────────────────────────────────┘
```
