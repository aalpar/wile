# Continuation Escape Design

## Summary

Call/cc escapes use the composable-continuation-then-abort model, following
Racket's approach where `call/cc` is defined in terms of composable
continuations and prompt abort:

```scheme
(call/cc f) ≡
  (call-with-composable-continuation
    (lambda (k)
      (f (lambda (v) (abort-current-continuation default-prompt-tag (k v)))))
    default-prompt-tag)
```

The escape closure captures a `ComposableContinuation` at call/cc time.
When invoked, it applies the composable continuation in a sub-context (running
the captured frames to completion), then aborts to `DefaultPromptTag` with
the result. This produces a regular `ErrPromptAbort` that the standard prompt
handling path catches — no special-case escape detection needed.

## Design Rationale

### Why composable-continuation-then-abort?

The previous design used a `continuationEscapePayload` carrier tunneled
through `ErrPromptAbort`, with a dedicated `HandleContinuationEscapeAbort`
function to detect and process escape payloads. This required:
- A carrier type implementing `values.Value` (solely for transport)
- Special-case detection in two places (PrimCallCC sub-context + RunWithEscapeHandling)
- A `pendingEscape` field for nested escape scenarios
- `escapeCont` tracking for sub-context chain breaks

The composable-continuation-then-abort model eliminates all of this:
- The escape closure does its own work (applies cc, runs frames, aborts with result)
- `RunWithEscapeHandling` handles all aborts uniformly via `FindPrompt`
- No special carrier type, no detection logic, no pending escape mechanism

### Dynamic-wind integration

When the escape closure applies the composable continuation,
`applyComposableContinuation` calls `RestoreWithWindingFrom` which handles
all dynamic-wind transitions (unwinding source frames, rewinding target
frames). The escape closure's sub-context runs the restored frames to
completion, executing any dynamic-wind thunks along the way.

The abort to `DefaultPromptTag` then propagates to `RunWithEscapeHandling`,
which does a final `RestoreWithWindingFrom` from the current winding state
to the prompt's winding state (nil for the context-level prompt), unwinding
any remaining frames.

### Thread and barrier checks

Both checks happen at the point of escape closure invocation, before any
continuation manipulation:

1. **Thread check**: Compares capture-time thread ID with invocation-time
   thread ID. Prevents cross-thread continuation invocation that would
   corrupt VM state (per SRFI-18 semantics).

2. **Barrier check**: Compares capture-time `*BarrierToken` pointer with
   invocation-time pointer. Pointer inequality means the continuation
   would cross a `with-continuation-barrier` boundary. `BarrierToken` is
   an opaque identity type — only pointer identity matters.

### Two execution modes in PrimCallCC

**Inline mode** (`mc.Parent() != nil`): The lambda runs directly in the
current VM context via `mc.Apply()`. This preserves the full continuation
chain, critical for cooperative coroutines and patterns that capture/invoke
multiple continuations. PC is compensated for `OperationForeignFunctionCall`'s
post-increment.

**Sub-context mode** (`mc.Parent() == nil`): Falls back to an isolated
sub-context when call/cc is inside a foreign function's sub-context. The
escape closure's abort to `DefaultPromptTag` is caught directly by PrimCallCC
(tag match → extract value → return nil), ensuring call/cc works in contexts
without `RunWithEscapeHandling` (e.g., threads that call `Run()` directly).

## Code Locations

| Component | File | Line |
|-----------|------|------|
| `PrimCallCC` | `registry/core/prim_control.go` | 115 |
| `newComposeAbortEscapeClosure` | `registry/core/prim_control.go` | 192 |
| `ComposableContinuation` | `machine/composable_continuation.go` | 29 |
| `BarrierToken` | `machine/barrier_token.go` | 23 |
| `applyComposableContinuation` | `machine/machine_context.go` | 485 |
| `RunWithEscapeHandling` | `machine/machine_context.go` | 1227 |
| `RestoreWithWindingFrom` | `machine/machine_context.go` | 1079 |

For operational details (error propagation paths, RunWithEscapeHandling
pseudocode, end-to-end examples), see `docs/dev/PROMPT_ABORT_SYSTEM.md`.
