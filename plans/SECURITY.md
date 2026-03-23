# Security Plans

---

# Opcode Resource Limits Design

**Status:** Design (not yet implemented)
**Date:** 2026-02-15

## Problem

The VM loop checks `ctx.Done()` every 1024 operations (`contextCheckMask`), but several opcodes can block *inside* a single `Apply()` call for unbounded time before returning to the loop. A malicious or runaway Scheme program can exploit this to consume unbounded CPU within a single opcode execution.

## Threat Model

An untrusted Scheme program (or malicious input to a trusted program) triggers unbounded computation inside a single VM opcode, bypassing the context cancellation check in the main VM loop.

The attacker controls Scheme source code (or input data that drives macro expansion). The embedder controls engine configuration. The goal is to give embedders per-category knobs that cap the work any single opcode can do.

## Existing Limits

| Limit | Location | Default | Error Sentinel |
|-------|----------|---------|----------------|
| Call depth | `machine_context.go:69` | 0 (unlimited) | `ErrCallDepthExceeded` |
| read-string allocation | `prim_read_write.go:34` | 100 MB | `ErrAllocationLimitExceeded` |
| read-bytevector allocation | `prim_read_write.go:38` | 100 MB | `ErrAllocationLimitExceeded` |
| VM loop context check | `machine_context.go:38` | Every 1024 ops | (context error) |
| Match VM context check | `match.go:237` | Every 1024 iterations | (context error) |

## Limit Categories

### Category 1: Match Steps

**What it caps:** Iterations of the match VM's bytecode dispatch loop.

**Enforcement point:** `Matcher.MatchSyntaxWithLiterals()` in `internal/match/match.go`, line 234+. The loop already has an `iterations` counter and a batched `ctx.Done()` check every 1024 iterations. The limit check piggybacks on this existing counter.

**API:**
```go
wile.WithMaxMatchSteps(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrMatchStepsExceeded = NewStaticError("match step limit exceeded")`

### Category 2: Expand Steps

**What it caps:** Total recursive calls to `expandSyntaxValue()` during a single template expansion.

**Enforcement point:** At the top of `SyntaxMatcher.expandSyntaxValue()` in `internal/match/syntax_adapter.go`, line 255+. Counter resets at the start of each `Expand()` call.

**API:**
```go
wile.WithMaxExpandSteps(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrExpandStepsExceeded = NewStaticError("expand step limit exceeded")`

### Category 3: Continuation Copy Depth

**What it caps:** The number of continuation frames walked during `DeepCopy()` and `GraftContinuation()` when invoking a composable continuation.

**Enforcement point:** Inside `MachineContinuation.DeepCopy()` in `machine/machine_continuation.go`.

**This is distinct from `maxCallDepth`:**
- `maxCallDepth` limits how deep the *live* call stack grows during execution.
- `maxContinuationCopyDepth` limits how much work a single *continuation invocation* does when copying a captured continuation segment.

**API:**
```go
wile.WithMaxContinuationCopyDepth(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrContinuationCopyDepthExceeded = NewStaticError("continuation copy depth exceeded")`

### ForeignFunctionCall: Embedder Responsibility

`ForeignFunctionCall` calls arbitrary Go code via the `ForeignFunction` signature. The context is accessible via `mc.Context()`. The VM cannot impose a step limit on opaque Go code. This is documented as the embedder's responsibility.

## New Error Sentinels

Added to `values/foreign_error.go`, grouped with existing resource-exhaustion errors:

```go
ErrMatchStepsExceeded              = NewStaticError("match step limit exceeded")
ErrExpandStepsExceeded             = NewStaticError("expand step limit exceeded")
ErrContinuationCopyDepthExceeded   = NewStaticError("continuation copy depth exceeded")
```

## API Summary

```go
// Existing
wile.WithMaxCallDepth(n uint64) EngineOption

// New
wile.WithMaxMatchSteps(n uint64) EngineOption
wile.WithMaxExpandSteps(n uint64) EngineOption
wile.WithMaxContinuationCopyDepth(n uint64) EngineOption
```

All follow the same convention:
- 0 = unlimited (default)
- \> 0 = hard cap
- Exceeded -> wrapped sentinel error, propagated as Scheme exception

## Plumbing Summary

All three new limits follow the same path as `maxCallDepth`:

```
EngineOption (options.go)
  -> engineConfig field (options.go)
    -> Engine field (engine.go)
      -> MachineContext field (machine_context.go)
        -> inherited by sub-contexts (NewSubContext, NewSubContextForThread)
          -> passed to enforcement point (match/expand/deep-copy)
```

## Future Work

- **Default non-zero limits:** Once implemented and tested, consider changing defaults from 0 to sensible non-zero values.
- **Macro expansion depth:** The expander recursively expands macro results without tracking depth. A `maxExpansionDepth` limit may be needed as a fourth category.
