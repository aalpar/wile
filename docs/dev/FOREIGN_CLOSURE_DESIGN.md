# ForeignClosure Design

**PR:** #335
**Date:** 2026-02-25

## Problem

All Go-implemented primitives were wrapped in `*MachineClosure` with a two-op
bytecode template (`OpForeignFunctionCall` + `OpRestoreContinuation`). Every
primitive call — even a trivial `(+ 1 2)` — entered the VM loop, dispatched
two opcodes, and restored a continuation. The `atomicBody` flag on
`NativeTemplate` distinguished foreign closures from Scheme closures at
runtime, a workaround for the type system not reflecting the behavioral
difference.

## Solution

Two new types replace the single `*MachineClosure` representation:

```
Closure (interface)
├── *MachineClosure   — compiled Scheme, executed by VM loop
└── *ForeignClosure   — Go function, called directly via applyForeign
```

`ForeignClosure` holds a `ForeignFunction` directly with no `NativeTemplate`.
`applyForeign` does arity check, arg binding, panic recovery, and continuation
restore — the same work the VM loop did, but without template indirection or
opcode dispatch.

### Closure Interface

```go
type Closure interface {
    values.Callable
    closureMarker()  // unexported, restricts to machine package
}
```

Distinguished from other `Callable` types (`CaseLambdaClosure`, `Parameter`,
`ComposableContinuation`) which have different application semantics.
Storage sites (`DynamicWindFrame.Before/After`, `MachineContinuation.promptHandler`,
`Parameter.converter`) use the `Closure` interface.

### Dispatch

```go
func (p *MachineContext) ApplyCallable(callable values.Value, args ...values.Value) {
    switch cls := callable.(type) {
    case *MachineClosure:    p.Apply(cls, args...)
    case *ForeignClosure:    p.applyForeign(cls, args...)
    case *CaseLambdaClosure: p.ApplyCaseLambda(cls, args...)
    case *Parameter:         p.applyParameter(cls, args)
    case *ComposableContinuation: p.applyComposableContinuation(cls, args)
    }
}
```

### State Removed

- `NativeTemplate.atomicBody` — structural via `*ForeignClosure` type
- Two-op bytecode template per foreign closure (for leaf primitives)
- `computeNoCopyApply()` call in `NewForeignClosure`

### State Retained

- `NativeTemplate.noCopyApply` — still needed for Scheme closures
  (compiler calls `computeNoCopyApply()` at `compile_validated.go:454`)

## Edge Case 1: Template-Pointer Guard (PrimCallCC Inline Mode)

`PrimCallCC` in inline mode calls `mc.ApplyCallable(mcls, contClosure)` to set
up the VM for continued execution of a user lambda. `Apply` sets `mc.template`,
`mc.env`, `mc.pc = 0`. After `Apply` returns, `Run()` continues executing the
new template.

When `PrimCallCC` is a `*ForeignClosure`, `applyForeign` calls `fn(p)` which
calls `ApplyCallable` which calls `Apply` — setting `p.template` to the
lambda's template. After `fn(p)` returns, `applyForeign` must NOT call
`returnImmediate()` because `Apply` already configured the VM state.

**Detection:** Save `p.template` before calling `fn(p)`. After the call, if
`p.template` changed, the function set up VM state — skip `returnImmediate()`.

```go
savedTemplate := p.template
err := fcls.fn(p)
// ...
if p.template != savedTemplate {
    return p, nil  // VM state configured by fn, don't restore
}
// Normal path: restore continuation
```

This is a pointer comparison — reliable because `Apply` always sets
`p.template` to the callee's template (a different `*NativeTemplate` pointer).

**Why this matters:** Without this guard, `applyForeign` overwrites the
lambda's template/env/pc with `returnImmediate()`, causing the VM to resume
stale state. Tests pass because most primitives don't call `Apply` internally;
only `PrimCallCC` inline mode does.

## Edge Case 2: Go Stack Overflow (Recursive Foreign Closures)

`applyForeign` calls `fn(p)` synchronously on the Go call stack. For leaf
primitives (`+`, `car`, `cons`), this is fine — they return immediately.

But some foreign closures create sub-contexts and call `Run()`:

```go
// newComposeAbortEscapeClosure (call/cc escape closure)
fn := func(innerMC *MachineContext) error {
    sub := innerMC.NewSubContext()
    _, err := sub.ApplyCallable(cc, val)  // graft continuation
    err = sub.Run()                        // execute restored frames
    // ...
}
```

When the restored computation invokes another escape closure, the pattern
repeats: `applyForeign` → `fn` → `sub.Run()` → `ApplyCallable` →
`applyForeign` → `fn` → `sub.Run()` → ...

Each level adds ~4 Go stack frames that persist until the inner `Run()`
returns. The `ctak` benchmark (continuation-based Takeuchi) creates thousands
of nested continuation invocations, consuming the 1GB Go stack limit.

**In the old bytecode path:** `Apply(*MachineClosure)` returned to the
existing `Run()` loop. `OpForeignFunctionCall` was just another iteration of
the loop — iterative, not recursive. Each level added only `fn` +
`sub.Run()` ≈ 2 persistent frames instead of 4.

### Fix: NewVMForeignClosure

Foreign closures that do nested VM execution use the bytecode trampoline:

```go
func NewVMForeignClosure(env, pcnt, variadic, fn) *MachineClosure {
    tpl := NewNativeTemplate(pcnt, 0, variadic)
    tpl.AppendOperations(
        NewOperationForeignFunctionCall(fn),
        NewOperationRestoreContinuation(),
    )
    tpl.computeNoCopyApply()
    // ...
    return NewClosureWithTemplate(tpl, env)
}
```

This creates a `*MachineClosure` (not `*ForeignClosure`), so `ApplyCallable`
dispatches through `Apply` → VM loop → `OpForeignFunctionCall` — keeping the
loop iterative.

**Only `newComposeAbortEscapeClosure` uses this path.** All other registered
primitives (~200+) use the direct `*ForeignClosure` / `applyForeign` fast path.

### Decision Criteria: Which Path?

| Closure creates sub-context + `Run()`? | Use |
|-----------------------------------------|-----|
| No (leaf primitive) | `NewForeignClosure` → `*ForeignClosure` → `applyForeign` |
| Yes (nested VM execution) | `NewVMForeignClosure` → `*MachineClosure` → VM loop |

The distinction is about Go stack safety, not performance. The bytecode path
is slightly slower (two opcode dispatches) but prevents unbounded Go stack
growth.

## PairBlock

Separate, simpler refactoring. `values.PairBlock` is a named `[]Pair` type
with a `LinkWith([]Value) Tuple` method that fills cars and wires cdrs in a
single loop. Replaces hand-inlined block allocation in `values.List()` and
`MachineContext.buildRestArg()`.

## Performance

Gabriel benchmark suite, 6 runs averaged, comparing `master~2` (pre-refactoring)
vs post-refactoring:

| Benchmark | Change |
|-----------|--------|
| sumfp | -21.2% |
| sum | -18.9% |
| primes | -18.4% |
| ackermann | -16.2% |
| fib | -16.1% |
| peval | -16.0% |
| triangl | -13.7% |
| tak | -13.2% |
| divrec | -10.1% |
| nqueens | -9.8% |
| cpstak | -9.0% |
| sieve | -7.7% |
| diviter | -7.1% |
| takl | -3.5% |
| ctak | +2.0% |
| deriv | +6.6% |
| **geo-mean** | **-11.1%** |

The biggest improvements are in function-call-heavy benchmarks where the
eliminated VM loop overhead compounds. `ctak` is neutral (uses bytecode path).
`deriv` regression is within measurement noise (14.9% spread in baseline).
