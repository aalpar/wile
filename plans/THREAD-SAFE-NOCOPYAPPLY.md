# Thread-Safe NoCopyApply

**Date:** 2026-03-22
**Status:** Superseded — NoCopyApply removed entirely (PR #561)

> **Historical (as of 2026-03-22).** This plan was never implemented. PR #561
> removed NoCopyApply entirely instead of gating it with a `threadShared`
> latch. Everything below describes the proposed-but-not-implemented design.

## Problem

`NoCopyApply` is a single-threaded optimization that reuses a closure's own
environment frame for parameter bindings instead of allocating a fresh frame.
When SRFI-18 threads call the same closure concurrently, all callers write
arguments to the same binding slots. `Binding.value` is a `values.Value`
interface (two machine words); concurrent writes produce torn reads — one
thread's type pointer with another's data pointer.

Affected call paths:

| Path | File | Copy path today? |
|------|------|------------------|
| `MachineClosure.Apply` | `machine_context_apply.go` | Yes (gated on `tpl.NoCopyApply()`) |
| `ForeignClosure` via `callForeignCached` | `call_foreign_cached.go` | No — always reuses closure env |
| `ForeignClosure` via `applyForeign` | `machine_context_apply.go` | No — always reuses closure env |

## Fix

Add an atomic `threadShared uint32` flag to `MachineClosure` and
`ForeignClosure`. The flag is a one-way latch:

```
0 → single-threaded (NoCopyApply eligible)
1 → thread-shared   (always copy)
```

### Latch condition

At the start of `Apply` / `callForeignCached` / `applyForeign`:

```go
if mc.threadID != 0 && atomic.LoadUint32(&cls.threadShared) == 0 {
    atomic.StoreUint32(&cls.threadShared, 1)
}
```

Multiple threads racing to latch all write the same value — idempotent,
no CAS needed.

### Read condition

NoCopyApply is taken only when both conditions hold:

```go
tpl.NoCopyApply() && atomic.LoadUint32(&mcls.threadShared) == 0
```

Once latched, all callers (including primordial) use the copy path.

## MachineClosure changes

**Struct** (`machine_closure.go`):

```go
type MachineClosure struct {
    env          *environment.EnvironmentFrame
    template     *NativeTemplate
    threadShared uint32 // atomic; 0=single-threaded, 1=thread-shared
}
```

**Apply** (`machine_context_apply.go`): Add latch before the NoCopyApply
branch. Change the branch condition from `tpl.NoCopyApply()` to
`tpl.NoCopyApply() && atomic.LoadUint32(&mcls.threadShared) == 0`.

No changes to `Copy()`, `EqualTo()`, or `NewClosureWithTemplate()` — the
flag is runtime state, not identity.

## ForeignClosure changes

**Struct** (`foreign_closure.go`):

```go
type ForeignClosure struct {
    fn           ForeignFunction
    env          *environment.EnvironmentFrame
    paramCount   int
    isVariadic   bool
    name         string
    threadShared uint32 // atomic; 0=single-threaded, 1=thread-shared
}
```

**Copy path** — added to both `callForeignCached` and `applyForeign`:

```go
// Latch
if mc.threadID != 0 && atomic.LoadUint32(&fcls.threadShared) == 0 {
    atomic.StoreUint32(&fcls.threadShared, 1)
}

var env *environment.EnvironmentFrame
var bnds []environment.Binding

if atomic.LoadUint32(&fcls.threadShared) == 0 {
    // existing no-copy path
    env = fcls.env
    bnds = env.LocalEnvironment().Bindings()
    mc.envPooled = false
} else {
    // new copy path
    env = acquireEnvFrame()
    fcls.env.InitApplyFrame(env)
    bnds = env.LocalEnvironment().Bindings()
    mc.envPooled = true
}
```

`RestoreAndRelease` / `returnImmediate` already handle `envPooled`, so
the pooled frame lifecycle is covered.

## Testing

1. **Unit test — latch mechanism**: Verify `threadShared` stays `0` when
   called from `threadID == 0`, latches to `1` from `threadID != 0`, and
   stays `1` on subsequent `threadID == 0` calls. Both closure types.

2. **Concurrency test — correctness**: Define a NoCopyApply function
   (e.g., `(define (tag? node t) (and (pair? node) (eq? (car node) t)))`),
   spawn N SRFI-18 threads calling it on different data, verify correct
   results.

3. **Performance regression**: Run `make bench-gabriel` before and after.
   Single-threaded closures should show zero regression.

## Scope

**In scope:**
- `MachineClosure.Apply` — flag + gated NoCopyApply
- `ForeignClosure` — flag + new copy path in `callForeignCached` and `applyForeign`
- Tests

**Not in scope:**
- `CaseLambdaClosure` — delegates to `MachineClosure.Apply`, covered transitively
- `Binding.value` synchronization on shared parent chains — by-design, requires Scheme-level mutexes
- `NativeTemplate.computeNoCopyApply` — escape analysis stays single-threaded
- `restArgBuf` — per-MachineContext, not shared

**Invariants preserved:**
- Single-threaded code sees zero change (flag never latched)
- Latch is one-way and idempotent — no ABA, no unlock
- `acquireEnvFrame` / `RestoreAndRelease` lifecycle unchanged
