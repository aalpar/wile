# Design: OpCallForeignCached — Direct-Call Opcode for Primitives

**Date:** 2026-03-02
**Branch:** `perf/inline-continuation-evals`
**Status:** Approved

## Problem

Every call to a known primitive (`+`, `-`, `<=`, `car`, etc.) pays the full
continuation round-trip cost even though foreign closures never use it:

```
SaveContinuation     ~20ns  (alloc cont, transfer/inline evals)
PushCachedBinding    ~2ns   (push callee to stack)
... Push args ...
PullApply            ~15ns  (Pull from bottom, PopAll copy, type-switch in ApplyCallable)
RestoreAndRelease    ~10ns  (restore cont, pool releases)
```

Foreign closures (`*ForeignClosure`) are leaf calls — they execute Go code and
return. They never change `template` (sub-context primitives like `apply` use
`NewSubContext`, not `mc.Apply`). The only `*ForeignClosure` that could change
template would be `call/cc`, but that uses `NewVMForeignClosure` which produces
a `*MachineClosure`, not `*ForeignClosure`.

## Design

### Nanopass Principle

The compiler is unchanged. It emits the same predictable opcode sequence it
always does. A peephole optimization pass recognizes the pattern and rewrites
it.

### Two New Opcodes

| Opcode | Position | After `fcls.fn(mc)` returns |
|--------|----------|-----------------------------|
| `OpCallForeignCached` | Non-tail | `mc.pc++` (continue to next instruction) |
| `OpCallForeignCachedTail` | Tail | `returnImmediate()` (pop to caller's caller) |

Both take `Arg = idx` into `cachedBindings[]`.

### Peephole Pattern Recognition

**Non-tail pattern:**
```
SaveContinuation(off)     ; i      — deleted
PushCachedBinding(idx)    ; i+1    — replaced with OpCallForeignCached(idx)
... 0+ Push instructions  ; i+2..j — unchanged (args stay on stack)
PullApply                 ; j+1    — deleted
```

**Tail pattern** (no SaveContinuation):
```
PushCachedBinding(idx)    ; i      — replaced with OpCallForeignCachedTail(idx)
... 0+ Push instructions  ; i+1..j — unchanged
PullApply                 ; j+1    — deleted
```

**Match conditions:**
1. `PushCachedBinding` loads the callee
2. `cachedBindings[code[i].Arg].Value()` is `*ForeignClosure` at optimization time
3. Sequence ends at `PullApply` (post-fusion)
4. No branch target in the interior of the sequence (prevents fusing across
   control flow boundaries)
5. Non-tail variant: `SaveContinuation` immediately precedes `PushCachedBinding`
   and its offset lands exactly on `PullApply`

### Run() Dispatch — OpCallForeignCached (non-tail)

```
case OpCallForeignCached:
    fcls := mc.template.cachedBindings[instr.Arg].Value().(*ForeignClosure)
    vs := mc.evals.PopAll()

    // Arity check
    if !fcls.isVariadic {
        if len(vs) != fcls.paramCount { → error }
    } else {
        if len(vs) < fcls.paramCount-1 { → error }
    }

    // Bind args (reuse closure's own env — noCopyApply by construction)
    env := fcls.env
    bnds := env.LocalEnvironment().Bindings()
    // fixed: bnds[i].SetValue(vs[i])
    // variadic: bnds[:n-1] = vs[:n-1], bnds[n-1] = buildRestArg(vs, n-1)

    mc.env = env
    mc.envPooled = false

    // Counters
    mc.counters.ClosuresApplied++
    mc.counters.NoCopyApplies++
    mc.counters.ForeignCalls++

    savedTemplate := mc.template
    err := fcls.fn(mc)
    if err != nil {
        // ErrPromptAbort, ErrExitEscape, ErrExceptionEscape → pass through
        // anything else → goErrorToSchemeException(mc, err)
        return ...
    }

    if mc.template != savedTemplate {
        // Foreign fn configured VM state (defensive; no current ForeignClosure does this)
        continue
    }
    mc.pc++
```

### Run() Dispatch — OpCallForeignCachedTail

Identical to non-tail except the final step:

```
    if mc.template != savedTemplate {
        continue
    }
    mc = mc.returnImmediate()
```

### Error Handling

Same as `applyForeign`: classify and pass through control-flow errors, wrap
anything else via `goErrorToSchemeException`. No `defer/recover` — Number
arithmetic methods return `(Number, error)` for division by zero; the remaining
panics in promotion functions are for impossible states (programming errors)
that should crash loudly.

### What Changes

| File | Change |
|------|--------|
| `machine/opcode.go` | Add `OpCallForeignCached`, `OpCallForeignCachedTail` constants + opcodeTable metadata |
| `machine/machine_context.go` | Two new `case` arms in `Run()` |
| `machine/peephole.go` | New `fuseCallForeignCached` rule in `Optimize()` |
| Tests | `peephole_test.go`, `machine_context_test.go` or new test file |

### What Does NOT Change

- **Compiler** — zero changes (nanopass: compiler emits predictable opcodes)
- **`applyForeign`** — still used by non-cached-binding paths
- **`ApplyCallable`** — still the generic dispatch for runtime-resolved callees
- **`OperationForeignFunctionCall`** — still used for `NewVMForeignClosure`
- **`computeNoCopyApply`** — orthogonal (MachineClosure env copying)

### Ordering

`Optimize()` already runs before `computeNoCopyApply()`. The new rule fits in
the existing pipeline:

```
markDeadLoadVoidEdits → fuseLoadPush → fusePullApply → fuseCallForeignCached → Apply()
```

Note: `fuseCallForeignCached` runs after `fusePullApply` because it matches on
`PullApply` (the fused opcode), not on the `Pull + Apply` pair.

### Estimated Savings

Per call: eliminates `SaveContinuation` (~20ns) + `RestoreAndRelease` (~10ns) +
`Pull` + `ApplyCallable` type-switch. For fib(10) with ~177 calls × 5 primitive
calls each = ~885 primitive calls, estimated ~17µs total savings (~35% of
current fib time).

## Scope Decisions

- **Pattern A only**: Cached-binding callees where `*ForeignClosure` is known at
  optimization time. Covers all registered primitives. Pattern B (runtime
  speculation for `OpLoadGlobal` callees) deferred — primitives always resolve
  to cached bindings.
- **No `defer/recover`**: Number panics are impossible-state assertions, not
  user-triggerable. Division by zero is a regular error return.
- **`savedTemplate` check retained**: Defensive. No current `*ForeignClosure`
  changes template, but the check is one branch with zero cost on the happy path.
