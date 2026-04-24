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
`applyForeign` does arity check, arg binding, error conversion, and
continuation restore — the same work the VM loop did, except panic recovery,
which remains in the bytecode path's `OperationForeignFunctionCall`. Foreign
functions reachable via the direct fast path must return errors rather than
panic; arithmetic methods on `values.Number` (which can panic on
`ErrDivisionByZero`, `ErrNotANumber`, etc.) should not be invoked from
`*ForeignClosure` primitives without explicit recovery.

### Closure Interface

```go
type Closure interface {
    values.Callable
    NamedCallable    // Name() + Doc() — for stack traces, (procedure-documentation)
    closureMarker()  // unexported, restricts to machine package
}
```

Distinguished from other `Callable` types (`CaseLambdaClosure`, `Parameter`,
`ComposableContinuation`) which have different application semantics.
Storage sites (`DynamicWindFrame.Before/After`, `MachineContinuation.promptHandler`,
`Parameter.converter`) use the `Closure` interface.

The `NamedCallable` embedding lets stack-trace builders and Scheme-level
introspection (`(procedure-documentation)`, error messages) read `Name()`
and `Doc()` without a type switch. `CaseLambdaClosure` also satisfies
`NamedCallable` but is not a `Closure` because its application path is
distinct (arity dispatch, not direct invocation).

### Dispatch

```go
func (p *MachineContext) ApplyCallable(callable values.Value, args ...values.Value) {
    switch cls := callable.(type) {
    case *MachineClosure:         p.Apply(cls, args...)
    case *ForeignClosure:         p.applyForeign(cls, args...)
    case *CaseLambdaClosure:      p.ApplyCaseLambda(cls, args...)
    case *Parameter:              p.applyParameter(cls, args)
    case *ComposableContinuation: p.applyComposableContinuation(cls, args)
    case *CapturedContinuation:   p.applyCapturedContinuation(cls, args)
    }
}
```

### State Removed

- `NativeTemplate.atomicBody` — structural via `*ForeignClosure` type
- Two-op bytecode template per foreign closure (for leaf primitives)
- `computeNoCopyApply()` call in `NewForeignClosure`
- `NativeTemplate.noCopyApply` — removed in PR #561 (SRFI-18 thread safety)

## Edge Case 1: Template and Continuation Guards (PrimCallCC Inline Mode)

`PrimCallCC` in inline mode calls `mc.ApplyCallable(mcls, contClosure)` to set
up the VM for continued execution of the user-supplied procedure. After
`fn(p)` returns, `applyForeign` must avoid two distinct double-actions
depending on what type the user's procedure was.

**Case A — User procedure is a `*MachineClosure`.** `ApplyCallable` dispatches
to `Apply`, which sets `p.template`, `p.env`, `p.pc = 0` so `Run()` continues
in the new template. `applyForeign` must NOT call `returnImmediate()` — that
would overwrite the VM state configured by `Apply`.

**Case B — User procedure is a `*ForeignClosure`.** `ApplyCallable` dispatches
to a nested `applyForeign`, which already consumes the saved continuation via
`RestoreAndRelease(p.cont)`. The outer `applyForeign` must NOT consume it
again — that would restore from a frame that has already been freed and
returned to the pool.

**Detection.** Save both `p.template` and `p.cont` before calling `fn(p)`.
After the call, the template guard skips the entire restore path if the
template changed (Case A); the cont guard skips only the restore step if
the continuation was already consumed (Case B).

```go
savedTemplate := p.template
savedCont := p.cont

err := fcls.fn(p)
// ... error handling, immediate timeout check ...

// Case A: VM state was reconfigured by the foreign function.
if p.template != savedTemplate {
    return p, nil
}

// Case B: continuation was consumed by a nested applyForeign.
if p.cont == savedCont {
    if p.cont != nil {
        p.RestoreAndRelease(p.cont)
    } else {
        p.template = immediateReturnTemplate
        p.pc = 0
    }
}
return p, nil
```

Both checks are pointer comparisons, reliable because each callable that
manipulates VM state writes a distinct pointer: `Apply` sets `p.template`
to the callee's template; nested `applyForeign` advances `p.cont` past the
consumed frame (typically to its parent) before returning.

The savedCont guard was added in PR #573 after a test failure exposed
double-restore in the inline-Foreign-via-Foreign path. `callForeignCached`
(`machine/call_foreign_cached.go:83-126`) carries the same dual-guard
structure for the peephole-optimized fast path.

**Why this matters.** Without these guards, `applyForeign` either
overwrites VM state set up by `Apply` (Case A) or restores from a freed
continuation frame (Case B), and the VM resumes stale state in either
case. Most primitives don't call `Apply` or `ApplyCallable` internally;
only `PrimCallCC` inline mode (and any future primitives that themselves
invoke user procedures) trigger these paths.

## Edge Case 2: Go Stack Overflow (Recursive Foreign Closures)

`applyForeign` calls `fn(p)` synchronously on the Go call stack. For leaf
primitives (`+`, `car`, `cons`), this is fine — they return immediately.

But some callables create sub-contexts and call `Run()`:

```go
// CapturedContinuation (call/cc escape value)
// applyCapturedContinuation:
sub := innerMC.NewSubContext()
_, err := sub.ApplyCallable(cc, val)  // graft continuation
err = sub.Run()                        // execute restored frames
// ...
```

When the restored computation invokes another `CapturedContinuation`, the
pattern repeats: `applyCapturedContinuation` → `sub.Run()` → `ApplyCallable` →
`applyCapturedContinuation` → `sub.Run()` → ...

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
    // tpl.computeNoCopyApply() — removed in PR #561
    // ...
    return NewClosureWithTemplate(tpl, env)
}
```

This creates a `*MachineClosure` (not `*ForeignClosure`), so `ApplyCallable`
dispatches through `Apply` → VM loop → `OpForeignFunctionCall` — keeping the
loop iterative.

**`NewVMForeignClosure` now has zero callers in production code.** Call/cc
escape values became `CapturedContinuation` (dispatched via
`applyCapturedContinuation`), which handles the nested VM execution directly
without going through the bytecode trampoline. All registered primitives
(~200+) use the direct `*ForeignClosure` / `applyForeign` fast path.

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
