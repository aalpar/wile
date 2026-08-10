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
`applyForeign` does arity check, arg binding, optional contract validation
(`ForeignClosure.SetValidator`), error conversion, and continuation restore —
the same work the VM loop did. **There is no per-call panic recovery on any
live path.** An earlier revision of this sentence said recovery "remains in the
bytecode path's `OperationForeignFunctionCall`"; that operation, its opcode and
its only builder were deleted in 2026-08 after they were measured to have zero
production references. Foreign functions reachable
via the direct fast path must return errors rather than panic; the
type-coercion helpers in `pkg/values/promotion.go` and
`pkg/values/numeric_tower.go` panic on `ErrNotANumber` / `ErrNotAReal` /
`ErrInvariantViolation` when handed unexpected types, and
`emptyList.Car`/`Cdr` (`pkg/values/empty_list.go`) panic on `ErrNotAPair`.
None of them may be invoked from `*ForeignClosure` primitives
without an explicit type check or `defer recover()`. (R7RS-specified errors like
division by zero already return `(Number, error)` — those flow through the
normal error path.) A panic that escapes anyway is contained at the VM boundary
by `MachineContext.RunResumable`, but only as an uncatchable `*SchemeError`
returned to the embedder, not as a Scheme condition `guard` can see. This
contract is otherwise enforced by code review only; there is no test or lint
that catches a primitive that violates it.

### Closure Interface

```go
type Closure interface {
    values.Callable
    NamedCallable    // Name() + Doc() — for (procedure-name) and (procedure-documentation)
    closureMarker()  // unexported, restricts to machine package
}
```

Distinguished from other `Callable` types (`CaseLambdaClosure`, `Parameter`,
`ComposableContinuation`) which have different application semantics.
Storage sites (`DynamicWindFrame.Before/After`, `MachineContinuation.promptHandler`,
`Parameter.converter`) use the `Closure` interface.

The `NamedCallable` embedding lets `(procedure-name)` and `(procedure-documentation)`
read `Name()`/`Doc()` through one interface assertion instead of a six-way
type switch. The two consumers are `PrimProcedureName` and
`PrimProcedureDocumentation` in `pkg/registry/core/prim_reflection.go`.
Stack-trace builders use a different mechanism — they read `*NativeTemplate.Name()`
off the saved continuation directly (`MachineContext.CaptureStackTrace` in
`pkg/machine/machine_context.go`), not through `NamedCallable`. `CaseLambdaClosure` also satisfies `NamedCallable`
but is not a `Closure`: it doesn't define `closureMarker()`, and its application
path is arity dispatch (`ApplyCaseLambda` → `Apply` of the matched clause),
not direct invocation.

The unexported `closureMarker()` enforces *package locality* — only types
defined inside `machine/` can satisfy `Closure`. The "direct invocation"
semantic is convention, enforced at the `ApplyCallable` dispatch table, not
by the type system.

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

**Detection.** Save both `p.template` and `p.cont` before calling `fn(p)`, and
clear the `p.reconfigured` flag that `Apply` sets. After the call, the
reconfigured/template guard skips the entire restore path if the foreign
function repointed the VM (Case A); the cont guard skips only the restore step
if the continuation was already consumed (Case B). `reconfigured` is
authoritative for in-place `Apply` because it catches self-application, where
the template is unchanged; the template comparison additionally covers
continuation-restore paths that repoint the template without going through
`Apply`.

```go
savedTemplate := p.template
savedCont := p.cont
p.reconfigured = false

err := fcls.fn(p)
// ... error handling, immediate timeout check ...

// Case A: VM state was reconfigured by the foreign function.
if p.reconfigured || p.template != savedTemplate {
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

The two saved-state checks are pointer comparisons, reliable because each
callable that manipulates VM state writes a distinct pointer: `Apply` sets
`p.template` to the callee's template; nested `applyForeign` advances `p.cont`
past the consumed frame (typically to its parent) before returning. The
`reconfigured` flag is only meaningful inside the clear→call→read window; no
other opcode may read it without first establishing its own clear.

The savedCont guard was added in PR #573 after a test failure exposed
double-restore in the inline-Foreign-via-Foreign path. `callForeignCached`
(`pkg/machine/call_foreign_cached.go`) carries the analogous guard on **both**
of its arms: the non-tail arm restores only when `mc.cont == savedCont`, and
the tail arm guards `returnImmediate()` the same way, because a foreign
function reachable in tail position (via
`call-with-immediate-continuation-mark` or `apply`) can consume the frame
itself, and restoring again would pop a second frame and silently drop an
activation.

**Why this matters.** Without these guards, `applyForeign` either
overwrites VM state set up by `Apply` (Case A) or restores from a freed
continuation frame (Case B), and the VM resumes stale state in either
case. Most primitives don't call `Apply` or `ApplyCallable` internally;
only `PrimCallCC` inline mode (and any future primitives that themselves
invoke user procedures) trigger these paths.

**Generality.** The Case A/B framing is illustrative, not exhaustive.
`ApplyCallable` (`pkg/machine/machine_context_apply.go`) dispatches to six
callable types, and four of them can mutate
either pointer: `*MachineClosure` and `*CaseLambdaClosure` both reach
`Apply` (template change → Case A); `*Parameter` and `*ComposableContinuation`
consume `p.cont` via `returnImmediate`/`Restore` (Case B). The guards fire
defensively for any callable in the dispatch table — the named cases are the
only ones that occur today via `PrimCallCC`, but the guards do not depend on
which case actually triggered.

## Edge Case 2: Go Stack Overflow (Recursive Foreign Closures)

`applyForeign` calls `fn(p)` synchronously on the Go call stack. For leaf
primitives (`+`, `car`, `cons`), this is fine — they return immediately.

But some callables create sub-contexts and drive them to completion:

```go
// applyParameter, converter path:
sub := p.NewSubContext()
defer ReleaseSubContext(sub)
_, err := sub.ApplyCallable(converter, newVal)
err = sub.RunWithinBoundary()   // execute the converter on a nested Run loop
// ...
```

When the nested computation reaches another such callable, the pattern
repeats and each level adds Go stack frames that persist until the inner run
returns. Deeply nested invocations consume the 1GB Go stack limit.

The historical instance was `call/cc` resume: `applyCapturedContinuation` used
to graft the captured chain into a fresh sub-context and `Run()` it, nesting
O(live continuation depth) Go frames per resume, which is what overflowed on
`ctak` (continuation-based Takeuchi). That is no longer how resume works; see
"Resume Is a Trampoline, Not a Nested Run" below.

**In the old bytecode path:** `Apply(*MachineClosure)` returned to the
existing `Run()` loop. `OpForeignFunctionCall` was just another iteration of
the loop — iterative, not recursive. Each level added only `fn` +
`sub.Run()` ≈ 2 persistent frames instead of 4.

### The `NewVMForeignClosure` escape hatch — deleted 2026-08

The bytecode trampoline was kept as a documented escape hatch: a
`NewVMForeignClosure` built a `*MachineClosure` whose two-op template was
`OperationForeignFunctionCall` + `OperationRestoreContinuation`, so
`ApplyCallable` dispatched through the VM loop instead of `applyForeign`.

It is gone. At the deletion commit the whole cluster was unreachable:
`NewVMForeignClosure` had zero Go references outside its own declaration and
doc comment, `OpForeignFunctionCall` was never a Go identifier at all (the
operation reported `OpKind() == OpComplex`), and
`NewOperationForeignFunctionCall` was called only from that dead builder and
from tests — including one guard test asserting panic pass-through on a
production path that did not exist. Keeping a documented escape hatch that
nothing exercised meant carrying a second, silently divergent dispatch path;
the recorded scoping decision was to delete rather than to re-site the guard.

**Consequence for the next reader:** the "which path?" question below no longer
has two answers. Every primitive is a `*ForeignClosure` via
`machine.NewForeignClosure`, unconditionally, and the Go-stack-depth argument
above is history, not a live trade-off. A future nested-VM primitive that needs
the trampoline re-adds it deliberately, with a caller.

### Resume Is a Trampoline, Not a Nested Run

`applyCapturedContinuation` (`pkg/machine/captured_continuation.go`) no longer
runs the captured chain at all. After checking thread identity and barrier
validity it returns an `*ErrResumeContinuation` control signal, which the
nearest `DefaultPromptTag` driver (`MachineContext.RunResumable`) reinstalls
onto its own live continuation and keeps looping. Resume therefore costs O(1)
Go frames regardless of continuation depth, and the winding stack reconciles
exactly once instead of twice on an escape-out. The bytecode trampoline was
never the fix here; returning the segment unrun was.

### Decision Criteria: Which Path?

There is one path: `NewForeignClosure` → `*ForeignClosure` → `applyForeign`.
The second row of this table used to read "nested VM execution →
`NewVMForeignClosure` → `*MachineClosure` → VM loop"; that row named a Go-stack
safety valve nothing had used since resume became a trampoline, and it was
deleted with the code (above).

**Arity gotcha.** A variadic closure binds its rest list into slot
`ParamCount-1`, so `ParamCount: 0` with `IsVariadic: true` makes `bindArgs`
index `bnds[:-1]` and panic. `PrimitiveSpec.Validate`
(`../../pkg/registry/primitive_registry.go`) rejects that combination, and `AddPrimitive`
panics on a spec that fails it, so a registered primitive fails loudly at
startup rather than on first call. A direct `NewForeignClosure(env, 0, true,
fn)` bypasses the check and still panics on first call. A no-fixed-argument
variadic is `ParamCount: 1`.

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
