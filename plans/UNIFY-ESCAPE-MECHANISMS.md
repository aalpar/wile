# Unify Escape Mechanisms

## Problem

`call-with-exit` and the prompt/abort system implement parallel tagged-boundary
escape paths with independent error types, propagation checks, and unwinding
logic. Bug fixes to barrier/winding behavior must be applied to both.

| Concern | `ErrExitEscape` | `ErrPromptAbort` |
|---------|-----------------|------------------|
| Tag type | `*ExitTag` (empty struct) | `*PromptTag` (id + name) |
| Payload | Single `values.Value` | `[]values.Value` |
| FFC check | Dedicated `errors.As` branch | Dedicated `errors.As` branch |
| Catch site | `PrimCallWithExit` | `PrimCallWithContinuationPrompt` / `RunWithEscapeHandling` |

Both use the same pattern: return tagged error, pass through FFC unchanged,
catch at boundary by tag match, unwind dynamic-wind frames.

## Approach

Reimplement `call-with-exit` using `ErrPromptAbort` with a private `PromptTag`.
Delete `ExitTag`, `ErrExitEscape`, and all associated propagation checks.

## Changes

### Delete

- `machine/exit_escape.go` — `ExitTag`, `ErrExitEscape`, `NewErrExitEscape`, `NewExitTag`
- `ErrExitEscape` check in `OperationForeignFunctionCall.Apply()`
- `ErrExitEscape` check in `applyCallableError()` in `foreign_closure.go`

### Modify

**`registry/core/prim_exit.go`** — `PrimCallWithExit`:
- `NewExitTag()` becomes `NewPromptTag("exit")`
- Exit closure returns `&ErrPromptAbort{Tag: tag, Values: []values.Value{val}}`
- Catch block: `errors.As(err, &abortErr) && abortErr.Tag == tag`
- Result extraction: `mc.SetValue(abortErr.Values[0])`
- Remove `machine.ExitTag` and `machine.ErrExitEscape` imports

**`docs/dev/PROMPT_ABORT_SYSTEM.md`**:
- Remove `ErrExitEscape` from error priority list (step 3)
- Add note that `call-with-exit` uses `ErrPromptAbort` with a private tag

## Semantics Preserved

- **One-shot validity**: `atomic.Bool` check unchanged — prevents stale escape calls
- **Single-value escape**: Closure remains arity-1, wraps value in 1-element slice
- **Dynamic-wind unwinding**: Same `sub.UnwindTo(mc.WindingStack().Depth())` path
- **Barrier pass-through**: `ErrPromptAbort` already passes through barriers (tested)
- **Thread check**: In exit closure before emitting abort, unchanged
- **Tag isolation**: Private `PromptTag` created per invocation, caught in sub-context
  before reaching `RunWithEscapeHandling`

## Why This Works

`PrimCallWithExit` runs proc in a sub-context and catches the error from
`sub.Run()`. The private `PromptTag` is created in `PrimCallWithExit` and
passed to the exit closure — no other code holds a reference. Therefore:

1. `PrimCallWithContinuationPrompt` never matches our tag (different pointer)
2. `RunWithEscapeHandling` never sees our tag (caught in sub-context first)
3. Nested `call-with-exit` invocations each have their own private tag

## Trade-off

`PromptTag` has `id + name` fields (16 bytes + string) vs `ExitTag` at 0 bytes.
One allocation per `call-with-exit` invocation. Negligible.
