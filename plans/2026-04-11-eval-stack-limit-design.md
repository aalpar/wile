# Eval Stack Size Limit

> Tech debt Task 1.4 from `plans/TECH-DEBT-2026-04.md`

## Problem

The eval stack (`*Stack` on `MachineContext`) grows without bound. A program
with a million-argument call `(f a1 a2 ... a1000000)` or deeply nested
non-tail expressions allocates unbounded memory. `WithMaxCallDepth` caps
recursion depth but not stack size — a sandbox that gates one without the
other leaves an OOM vector.

## Design

### Approach

Check stack size in the `Run()` loop at the opcodes that grow the eval stack
(Approach B from brainstorming). No structural changes to `Stack`. Follows the
`maxCallDepth` pattern exactly.

### Sentinel

New sentinel `ErrStackOverflow` in `werr/werr.go`.

### Engine Option

`WithMaxStackSize(n uint64)` in `options.go`. Opt-in only — no default limit.
Zero = unlimited. No `stackSizeSet bool` needed — unlike `maxCallDepth` there
is no default value, so zero-value = not called = unlimited.

### MachineContext

New field `maxStackSize uint64` on `MachineContext`. Propagated to sub-contexts
via `NewSubContext` and `NewThreadSubContext` (same as `maxCallDepth`).

Accessor pair: `MaxStackSize() uint64`, `SetMaxStackSize(n uint64)`.

### Enforcement Points

Six opcode sites in `MachineContext.Run()`:

| Opcode | Growth | Check |
|--------|--------|-------|
| `OpPush` | `Push` (single value) or `PushAll` (multi-value) | After push/pushAll |
| `OpPushLiteral` | `Push` (fused literal load+push) | After push |
| `OpPushGlobal` | `Push` (fused global load+push) | After push |
| `OpPushLocal` | `Push` (fused local load+push) | After push |
| `OpPushCachedBinding` | `Push` (fused cached load+push) | After push |
| `OpUnpackListToStack` | `Push` per element (apply rest-arg) | After ForEach loop |

Check at each site:

```go
if mc.maxStackSize > 0 && uint64(mc.evals.Len()) > mc.maxStackSize {
    return werr.WrapForeignErrorf(werr.ErrStackOverflow,
        "eval stack size %d exceeds limit %d", mc.evals.Len(), mc.maxStackSize)
}
```

### What Is NOT Checked

Foreign functions and complex operations that push directly to the eval stack
(e.g., `operation_cont_mark.go`). These pushes are bounded by bytecode
structure, not user input, and the next VM-loop push will catch any
accumulated growth.

### PushAll Multi-Value Behavior

`OpPush` with `PushAll` (the multi-value path from `(values ...)`) pushes the
entire slice before a single `checkStackSize()` runs. The stack may
temporarily exceed the limit by the number of multi-values in a single Push.
This is acceptable for a resource cap — the check still fires and returns the
error.

### Sub-Context Propagation

`NewSubContext` and `NewThreadSubContext` copy `maxStackSize` from the parent,
same as `maxCallDepth`.

## Tests

1. Engine with `WithMaxStackSize(N)` evaluates expression exceeding limit →
   `errors.Is(err, werr.ErrStackOverflow)`
2. Same expression with no limit set → succeeds
3. Sub-context inherits `maxStackSize` from parent
4. All existing tests pass unchanged (no default limit)

## Files Changed

| File | Change |
|------|--------|
| `werr/werr.go` | Add `ErrStackOverflow` sentinel |
| `options.go` | Add `WithMaxStackSize` |
| `engine.go` | Wire `maxStackSize` into `MachineContext` |
| `machine/machine_context.go` | Add field, accessors, enforcement in `Run()` |
| `machine/machine_context_subcontext.go` | Propagate to sub-contexts |
| `engine_unit_test.go` or `wile_test.go` | Integration tests |
| `machine/machine_context_test.go` | Sub-context propagation test |

## Effort

S — mechanical, follows established pattern.
