# Continuation Escape Design

## Summary

**Are all three fields needed?**

| Field | Verdict | Reason |
|-------|---------|--------|
| WindingStack | **Required** | Target state - where we're going |
| SourceWindingStack | **Removed** | Redundant - `sub.WindingStack()` always provides correct source state |
| EscapeCont | **Architectural debt** | Only needed because primitives use sub-contexts |

### Why SourceWindingStack Was Removed

Initially added to fix `after` thunks not running on escape from bytecode-based dynamic-wind. However, analysis showed it was redundant:

1. **Escape via primitives** (apply, for-each): Inner sub-context has nil winding stack (from `NewSubContext()`), so `SourceWindingStack = nil`, triggering fallback to `sub.WindingStack()` in `PrimCallCC`
2. **Direct escape**: `innerMC = sub`, so `SourceWindingStack = sub.WindingStack()` anyway

In all cases, `sub.WindingStack()` in `PrimCallCC` provides the correct source state for unwinding. The field was removed and all tests pass.

## Overview

When a captured continuation is invoked, it creates an `ErrContinuationEscape` that propagates up through the call stack. This document explains the three winding-related fields and why each is needed.

## The Current Fields

```
ErrContinuationEscape {
    Continuation  *MachineContinuation  // WHERE to jump
    Value         values.Value          // WHAT value to return
    WindingStack  WindingStack          // TARGET winding state (from capture time)
    EscapeCont    *MachineContinuation  // OUTER continuation (for nested escapes)
}
```

Note: `SourceWindingStack` was removed - the source state is obtained from `sub.WindingStack()` in `PrimCallCC`.

## Field Purposes

### WindingStack (Target)

**Captured when**: call/cc creates the continuation
**Contains**: The winding stack at the point where call/cc was called
**Used for**: Knowing what dynamic-wind frames should be active AFTER the escape

```scheme
(dynamic-wind
  (lambda () (print "A-before"))
  (lambda ()
    (call/cc (lambda (k) ...))  ; WindingStack = [frame-A] captured here
    ...)
  (lambda () (print "A-after")))
```

### Source Winding Stack (Current)

**Note**: No longer stored in `ErrContinuationEscape`. Instead, obtained from `sub.WindingStack()` in `PrimCallCC`.

**Obtained when**: Handling continuation escape in `PrimCallCC`
**Contains**: The winding stack at the point of invocation
**Used for**: Knowing what dynamic-wind frames need to be UNWOUND

```scheme
(define saved-k #f)

(dynamic-wind
  (lambda () (print "A-before"))
  (lambda ()
    (call/cc (lambda (k) (set! saved-k k)))
    ...)
  (lambda () (print "A-after")))

; Later, invoke from inside a different dynamic-wind:
(dynamic-wind
  (lambda () (print "B-before"))
  (lambda ()
    (saved-k 'value))  ; SourceWindingStack = [frame-B] captured here
  (lambda () (print "B-after")))
```

When `saved-k` is invoked:
- SourceWindingStack = [frame-B] (where we ARE)
- WindingStack = [frame-A] (where we're GOING)

The escape handler must:
1. Unwind frame-B (call B-after)
2. Rewind frame-A (call A-before)
3. Resume at the captured continuation

### EscapeCont (Outer Continuation)

**Set when**: call/cc is invoked inside a sub-context (like a foreign function's sub.Run())
**Contains**: The continuation to resume after the inner escape completes
**Used for**: Complex nested scenarios where escaping from a sub-context

This handles the case where call/cc captures inside a primitive's sub-context:

```scheme
(+ 1 (call/cc (lambda (k)
       (dynamic-wind
         before
         (lambda () (k 42))  ; Escape from inside dynamic-wind's sub-context
         after))))
```

## Flow Diagram

```
CAPTURE TIME (call/cc):
┌─────────────────────────────────┐
│ WindingStack = [A, B]           │  ← Winding state when captured
│ Continuation = PC, env, stack   │  ← Machine state when captured
└─────────────────────────────────┘

INVOKE TIME (calling the continuation):
┌─────────────────────────────────┐
│ SourceWindingStack = [A, C, D]  │  ← Winding state NOW
│ Value = 'result                 │  ← Value to return
└─────────────────────────────────┘

ESCAPE HANDLING:
┌─────────────────────────────────┐
│ 1. Find common prefix: [A]      │
│ 2. Unwind [D, C]: call afters   │
│ 3. Rewind [B]: call befores     │
│ 4. Restore Continuation         │
│ 5. Set Value                    │
└─────────────────────────────────┘
```

## Why These Fields Are Needed

| Field | Purpose | Without it... |
|-------|---------|---------------|
| WindingStack | Know target state | Can't rewind to correct dynamic extent |
| EscapeCont | Handle sub-context escapes | Lose outer context in nested scenarios |

**Source winding state** is obtained from `sub.WindingStack()` in `PrimCallCC`, not stored in the error struct.

## Simplification Possibility

**EscapeCont** might be eliminable if we restructure how sub-contexts work. Currently it handles a specific edge case where:
1. call/cc is called inside a sub-context (mc.NewSubContext())
2. The sub-context has no continuation chain (cont = nil)
3. We need to remember where to return after the escape

If all execution happened on a single MachineContext without sub-contexts, EscapeCont wouldn't be needed. However, foreign functions (primitives) currently use sub-contexts for isolation.

## Implementation Notes

1. **WindingStack** (target) is captured when the escape closure is created in `PrimCallCC`
2. **Source winding stack** is obtained from `sub.WindingStack()` when handling escape in `PrimCallCC`
3. **EscapeCont** is set only for sub-context escapes (when mc.Parent() == nil)

The escape handling in `PrimCallCC` and `RunWithEscapeHandling` uses these to:
```go
// In PrimCallCC, after catching ErrContinuationEscape:
sourceStack := sub.WindingStack()  // Current winding state
targetStack := escapeErr.WindingStack  // Captured target state

// RestoreWithWindingFrom handles:
// 1. Find common prefix
// 2. Unwind source frames (call after thunks)
// 3. Rewind target frames (call before thunks)
// 4. Restore continuation state
mc.RestoreWithWindingFrom(escapeErr.Continuation, sourceStack, targetStack)
```

## Code Locations

| Field | Set in | Used in |
|-------|--------|---------|
| WindingStack | `PrimCallCC` - escape closure creation | `PrimCallCC` - escape handling |
| Source stack | (from `sub.WindingStack()`) | `PrimCallCC` - `RestoreWithWindingFrom` |
| EscapeCont | `PrimCallCC` | `RunWithEscapeHandling` |

## Future Simplification

The `EscapeCont` field exists because foreign functions (Go primitives) use `NewSubContext()` to call Scheme closures. This creates a new execution context with `cont = nil`, breaking the continuation chain.

**Potential fix**: If primitives used the same MachineContext (just saving/restoring continuation like bytecode does), EscapeCont wouldn't be needed. This would require:
1. Changing `Apply` on primitives to use SaveContinuation instead of NewSubContext
2. Updating all primitives that call Scheme closures (map, for-each, apply, call-with-values, etc.)

This is significant refactoring but would simplify the continuation model.
