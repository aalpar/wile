# PullDrain: O(1) Procedure Extraction in OpPullApply

**Status**: Approved
**Date**: 2026-03-31

## Problem

`Stack.Pull()` is O(n) — it shifts all elements left via `copy()` to remove the
bottom element. Every non-promoted procedure call goes through `OpPullApply`
(or unfused `OpPull` + `OpApply`), which calls `Pull()` then `Drain()`. For
typical 1–3 arg calls the cost is negligible, but `apply` with long argument
lists pays O(n) on every call.

## Invariant

The procedure is always at stack position 0 when `PullApply` executes:

- **Non-tail calls**: `SaveContinuation` saves and empties the eval stack.
  Only proc+args are pushed onto the fresh empty stack.
- **Tail calls**: `drainAndApply` empties the stack on function entry. No
  intermediate expression leaves values on the stack.

Static arg-count is NOT reliable at runtime due to `OpPush` with `multiValues`
(splices N values where the compiler pushed 1) and `OpUnpackListToStack` (for
`apply`). This rules out CPython-style `CALL(nargs)`.

## Constraint

Left-to-right evaluation order is required (stricter than R7RS "unspecified").
This rules out "evaluate operator last" strategies.

## Design

### New method: `Stack.PullDrain()`

Returns `(proc, args)` by splitting `stack[0]` from `stack[1:]`. O(1) — no
`copy()`, just slice header arithmetic. Same zero-allocation contract as
`Drain()` (returned slice shares backing array, valid until next mutation).

```go
func (p *Stack) PullDrain() (values.Value, []values.Value) {
    n := len(*p)
    if n == 0 {
        panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow,
            "Stack.PullDrain: stack is empty"))
    }
    first := (*p)[0]
    var rest []values.Value
    if n > 1 {
        rest = (*p)[1:n:n]
    }
    *p = (*p)[:0]
    return first, rest
}
```

### New method: `MachineContext.pullDrainAndApply()`

Parallel to `drainAndApply` but splits proc from args in one step:

```go
func (p *MachineContext) pullDrainAndApply() (*MachineContext, error) {
    proc, vs := p.evals.PullDrain()
    p.counters.StackDrains++
    p.counters.StackElementsDrained += uint64(len(vs))
    p.counters.RecordStackDepth(len(vs))
    p.SetValue(proc)
    result, err := p.ApplyCallable(proc, vs...)
    if err != nil {
        return nil, applyCallableError(p, err)
    }
    return result, nil
}
```

### OpPullApply dispatch

```go
case OpPullApply:
    var err error
    mc, err = mc.pullDrainAndApply()
    if err != nil {
        return err
    }
```

## What doesn't change

- `OpPull` (unfused) — stays as-is. Rarely survives peephole.
- `Pull()` method — remains on `Stack` for unfused case and tests.
- Calling convention — no opcode encoding changes.
- `drainAndApply` — stays for `OpApply`, `OpCallLocal`, `OpCallCachedBinding`.
- Peephole optimizer — `fusePullApply` works identically.

## Files changed

| File | Change |
|------|--------|
| `machine/stack.go` | Add `PullDrain()` |
| `machine/machine_context_apply.go` | Add `pullDrainAndApply()` |
| `machine/machine_context.go` | Change `OpPullApply` case |
| `machine/stack_test.go` | Tests for `PullDrain()` |

## Risk

Low. The proc-at-position-0 invariant is structurally guaranteed by the
compiler. `PullDrain` contract matches `Drain` exactly.
