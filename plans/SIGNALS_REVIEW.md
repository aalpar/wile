# Signals Review — Wile Codebase

Date: 2026-02-27
Scope: Full codebase (177k lines Go)
Method: Six-lens signals analysis (mode transitions, feedback loops, saturation, temporal coupling, cross-talk, signal integrity)
Remediation: All proposed changes (P1–P6 + P4b) implemented and merged in PR #361 (2026-02-28). See `plans/SIGNALS_REMEDIATION.md`.

## Executive Summary

The system is **conditionally stable**. No unbounded feedback loops exist. Pooling amortizes allocations to near-zero after warmup. The continuation/dynamic-wind system has strong invariant enforcement (callDepth caching, shared flags, deep copies). The weakest transition is dynamic-wind unwinding during continuation escape — a non-atomic sequence that calls user-provided thunks mid-transition.

Three findings warrant action; the rest are observations about well-handled dynamics.

## Findings

### F1: Partial Unwind Creates Irrecoverable Half-State

**Lens**: Mode Transition
**Where**: `machine/machine_context.go` — `unwindStackTo()`
**Severity**: Low-Medium (terminal on trigger, but trigger is rare)

`unwindStackTo` runs after-thunks from innermost to outermost. If after-thunk at index `i` fails, thunks for indices `> i` have already executed but `p.windingStack` is not updated (truncation only happens after all thunks succeed). The winding stack now claims exited extents are still active.

In practice this is **terminal** — the error propagates to `Engine.Run` and the VM stops. But the winding stack state is inconsistent for the duration of error propagation, and any future error recovery mechanism that catches winding errors would observe the corruption.

### F2: restArgBuf Temporal Contract — Correct but Implicit

**Lens**: Temporal Coupling
**Where**: `machine/machine_context.go` — `buildRestArg()`
**Severity**: Low (maintenance hazard, not a current bug)

`restArgBuf` is a reusable `PairBlock` for variadic rest-arg lists. The returned Tuple's cons cells point into this buffer. Safety relies on the `NoCopyApply` invariant (no SaveContinuation in the template, so no nested calls can overwrite the buffer) plus sub-context isolation (each sub-context has its own `restArgBuf`).

`PrimList` documents the copy requirement: "The rest-arg list may be backed by a reusable buffer, so we must copy the spine." Any new variadic primitive that stores its rest-arg list without copying creates a latent aliasing bug.

### F3: callDepth Panic Crashes Host Process

**Lens**: Mode Transition
**Where**: `machine/machine_context.go:311-314` — `PopContinuation()`
**Severity**: Medium (low probability, critical impact if triggered)

`PopContinuation` panics on callDepth underflow. In the embedding product vision, a panic from Scheme code kills the host Go application. `SaveContinuation` returns an error for overflow (recoverable); `PopContinuation` panics for underflow (unrecoverable). The asymmetry is a product risk for embedded use.

### F4: Context Cancellation — Well-Characterized

**Lens**: Feedback Loop (Negative)
**Where**: `machine/machine_context.go:32-38` — `contextCheckMask`
**Severity**: Negligible

The VM checks `ctx.Done()` every 1024 operations via bitwise AND. Worst-case cancellation latency is ~100μs at 10M ops/sec, ~1ms at 1M ops/sec. Well-documented, sound design. No action needed.

### F5: Pool Degradation Under call/cc — Graceful Slope

**Lens**: Saturation
**Where**: `machine/pool.go` — four `sync.Pool` instances
**Severity**: Low (slope degradation, not cliff)

When `call/cc` marks continuation chains as shared, those frames exit the pool lifecycle (never released back). Under heavy continuation usage, pool hit rate drops, forcing GC allocation. This is graceful degradation — proportional to call/cc usage, not a cliff. VMCounters already track the relevant ratios.

### F6: Error Signal Distortion Across 5 Wrapping Layers

**Lens**: Signal Integrity
**Where**: `security/context.go:48`, `engine.go:532-552`
**Severity**: Low-Medium (works today, fragile to changes)

Authorization denial traverses a 5-layer error chain: `ErrAccessDenied` → `fmt.Errorf` → `goErrorToSchemeException` → `ErrExceptionEscape` → `RuntimeError`. The chain preserves `errors.Is` matching today because every layer implements `Unwrap()`. But no test explicitly verifies the full chain — any layer that drops `Unwrap()` breaks `errors.Is` silently.

### F7: No Default Call Depth Limit

**Lens**: Saturation
**Where**: `machine/machine_context.go:74`, `engine.go:53`
**Severity**: High for embedded use

`maxCallDepth` defaults to 0 (unlimited). Unbounded recursion grows the continuation chain until OOM. Each non-tail call allocates ~500 bytes. At 1M depth: ~500MB. The error path (`ErrCallDepthExceeded`) is implemented but the default is permissive. Untrusted Scheme code can OOM the host process.

### F8: PopAll Allocates on Every Apply

**Lens**: Cross-talk (GC Pressure)
**Where**: `machine/stack.go` — `PopAll()`
**Severity**: Low (optimization opportunity)

Every function application calls `PopAll()` which does `make([]values.Value, n)`. At 1M applies/sec with 2-4 args, this is 1M small allocations/sec. Short-lived (consumed immediately by Apply), but contributes to GC scheduling pressure. VMCounters track this via `StackPopAlls` and `StackElementsCopied`.  Evaluate any plans for PopAll stack reuse solutions in ./plans/ directory or .claude/plans/*

---

## Proposed Changes

### P1: Incremental Winding Stack Update in unwindStackTo

**Addresses**: F1 (partial unwind half-state)
**File**: `machine/machine_context.go`
**Rationale**: Currently `p.windingStack` is truncated only after all after-thunks succeed. If a thunk fails mid-sequence, the winding stack claims exited extents are still active. Updating after each successful thunk makes the state always consistent, even on partial failure.

```go
// BEFORE (current):
func (p *MachineContext) unwindStackTo(stack WindingStack, commonDepth int) error {
	for i := len(stack) - 1; i >= commonDepth; i-- {
		frame := stack[i]
		if frame.After != nil {
			sub := p.NewSubContext()
			sub.windingStack = stack[:i:i]
			_, err := sub.ApplyCallable(frame.After)
			if err != nil {
				ReleaseSubContext(sub)
				return err
			}
			err = sub.Run()
			ReleaseSubContext(sub)
			if err != nil {
				return err
			}
		}
	}
	p.windingStack = stack[:commonDepth:commonDepth]
	return nil
}

// AFTER (proposed):
func (p *MachineContext) unwindStackTo(stack WindingStack, commonDepth int) error {
	for i := len(stack) - 1; i >= commonDepth; i-- {
		frame := stack[i]
		if frame.After != nil {
			sub := p.NewSubContext()
			sub.windingStack = stack[:i:i]
			_, err := sub.ApplyCallable(frame.After)
			if err != nil {
				ReleaseSubContext(sub)
				// Truncate to reflect that extents > i are already exited.
				p.windingStack = stack[:i:i]
				return err
			}
			err = sub.Run()
			ReleaseSubContext(sub)
			if err != nil {
				p.windingStack = stack[:i:i]
				return err
			}
		}
		// This extent is now exited; update winding stack immediately.
		p.windingStack = stack[:i:i]
	}
	p.windingStack = stack[:commonDepth:commonDepth]
	return nil
}
```

**Trade-off**: One additional slice header assignment per iteration (cheap — no allocation, just pointer+len+cap update). Gains: winding stack always reflects reality, even on partial failure.

---

### P2: Replace callDepth Panic with Error Return

**Addresses**: F3 (host process crash on underflow)
**Files**: `machine/machine_context.go` (PopContinuation + callers)
**Rationale**: A panic in an embedded interpreter kills the host process. Converting to an error return lets the host decide how to handle the condition. The asymmetry with SaveContinuation (which returns an error for overflow) is also resolved.

```go
// BEFORE (current):
func (p *MachineContext) PopContinuation() *MachineContinuation {
	p.callDepth--
	if p.callDepth < 0 {
		panic("callDepth underflow in PopContinuation")
	}
	q := p.cont
	p.template = q.template
	p.env = q.env
	p.evals = q.evals
	p.cont = q.parent
	p.pc = q.pc
	p.singleValue = q.singleValue
	p.multiValues = q.multiValues
	p.envPooled = q.envPooled
	return q
}

// AFTER (proposed):
func (p *MachineContext) PopContinuation() (*MachineContinuation, error) {
	p.callDepth--
	if p.callDepth < 0 {
		p.callDepth = 0
		return nil, values.WrapForeignErrorf(values.ErrCallDepthExceeded,
			"callDepth underflow in PopContinuation")
	}
	q := p.cont
	p.template = q.template
	p.env = q.env
	p.evals = q.evals
	p.cont = q.parent
	p.pc = q.pc
	p.singleValue = q.singleValue
	p.multiValues = q.multiValues
	p.envPooled = q.envPooled
	return q, nil
}
```

Callers (the `Run()` loop and any direct call sites) add an error check. The cost is one branch per function return — negligible in a VM that already does a switch dispatch per instruction.

**Trade-off**: Slightly more verbose call sites. Gains: embedded safety — interpreter bugs produce Scheme errors, not process crashes.

---

### P3: Default maxCallDepth for Embedded Safety

**Addresses**: F7 (unbounded recursion → OOM)
**Files**: `engine.go` (NewEngine)
**Rationale**: The embedding product vision requires safe defaults. Untrusted Scheme code should not be able to OOM the host. The mechanism (`ErrCallDepthExceeded`) already exists; only the default is missing.

```go
// BEFORE (current — in NewEngine or engineConfig):
// maxCallDepth defaults to 0 (unlimited)

// AFTER (proposed):
const DefaultMaxCallDepth = 10000

// In NewEngine, after applying options:
if cfg.maxCallDepth == 0 {
	cfg.maxCallDepth = DefaultMaxCallDepth
}
```

Add `WithUnlimitedCallDepth()` engine option for callers that explicitly want no limit:

```go
func WithUnlimitedCallDepth() EngineOption {
	return func(cfg *engineConfig) {
		cfg.unlimitedCallDepth = true
	}
}
```

**Trade-off**: Existing callers that rely on unlimited depth must add `WithUnlimitedCallDepth()`. This is a breaking change for v1.x, but CLAUDE.md says "Break freely in minor versions — no stability guarantees until real users exist." The default should be safe; opting out should be explicit.

---

### P4: Integration Test for Error Chain Traversal

**Addresses**: F6 (error signal distortion across wrapping layers)
**Files**: New test in `engine_test.go` or `engine_error_chain_test.go`
**Rationale**: The `errors.Is` chain from `RuntimeError` down to each sentinel is an implicit contract. Making it explicit via test prevents silent breakage when wrapping layers change.

```go
func TestErrorChain_SecurityDenial_PreservesErrAccessDenied(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx,
		WithExtension(eval.Extension{}),
		WithAuthorizer(security.DenyAll()),
	)
	if err != nil {
		t.Fatal(err)
	}
	defer eng.Close()

	_, err = eng.Eval(ctx, `(eval '(+ 1 2))`)

	// The error must be a RuntimeError.
	var re *RuntimeError
	if !errors.As(err, &re) {
		t.Fatalf("expected RuntimeError, got %T: %v", err, err)
	}

	// The sentinel must be reachable through the full wrapping chain.
	if !errors.Is(err, security.ErrAccessDenied) {
		t.Errorf("errors.Is(err, ErrAccessDenied) = false; chain broken\nerror: %v", err)
	}
}

func TestErrorChain_CallDepthExceeded(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithMaxCallDepth(5))
	if err != nil {
		t.Fatal(err)
	}
	defer eng.Close()

	_, err = eng.Eval(ctx, `(define (f) (f)) (f)`)

	if !errors.Is(err, values.ErrCallDepthExceeded) {
		t.Errorf("errors.Is(err, ErrCallDepthExceeded) = false; chain broken\nerror: %v", err)
	}
}
```

**Trade-off**: None — pure test addition, no production code changes. Gains: the unwrap chain becomes a tested contract.

---

### P5: Document restArgBuf Safety Contract

**Addresses**: F2 (implicit temporal contract)
**File**: `machine/machine_context.go`
**Rationale**: The safety invariant is documented in the existing SAFETY comment but not in a form that new primitive authors would discover. Adding a doc reference on the `ForeignFunction` type or in `registry/CLAUDE.md` makes the contract visible at the point where new primitives are written.

```go
// In registry/CLAUDE.md or similar, add:

// ## Variadic Primitives and restArgBuf
//
// When a variadic closure is applied via the noCopyApply path, the rest-arg
// list is backed by a reusable buffer (MachineContext.restArgBuf). The list
// is valid only for the duration of the current foreign function call.
//
// If your primitive stores the rest-arg list (e.g., returns it, puts it in
// a data structure, or passes it to a sub-context that outlives the call),
// you MUST copy the list spine first. See PrimList in prim_lists.go for
// the canonical copy pattern.
```

**Trade-off**: None — documentation only. Gains: reduces probability of aliasing bugs in future primitives.

---

### P6: Observe Pool Effectiveness Under call/cc Load (Optional)

**Addresses**: F5 (pool degradation visibility)
**File**: `machine/counters.go`
**Rationale**: VMCounters already track `SharedFrameRestores` and `ContinuationPoolReleases`. Adding a derived metric (or just documenting the ratio) makes pool degradation visible without code changes.

No code change needed — just document in `counters.go`:

```go
// Pool effectiveness under call/cc:
//   ratio = SharedFrameRestores / (SharedFrameRestores + ContinuationPoolReleases)
//   0.0 = no call/cc impact (all frames recycled)
//   1.0 = all frames shared (no recycling, full GC pressure)
//   > 0.5 = pool is losing more than it saves; consider profiling GC pauses
```

**Trade-off**: None — comment only.

---

## Stability Assessment

| Condition | State |
|-----------|-------|
| Normal operation, well-behaved Scheme | **Stable** — pools converge, zero-alloc steady state |
| Heavy call/cc (coroutines, generators) | **Conditionally stable** — graceful GC pressure increase |
| Unbounded recursion, no maxCallDepth | **Unstable** — heap grows without bound until OOM |
| Dynamic-wind thunk failure during escape | **Terminal** — winding stack corrupted, VM dies |
| Compiler bug (unbalanced Save/Pop) | **Crash** — panic kills host process |

## Priority Order

All changes implemented in PR #361 (2026-02-28).

| Priority | Change | Effort | Risk | Status |
|----------|--------|--------|------|--------|
| 1 | P3: Default maxCallDepth | Small | Low (breaking but v1.x) | ✓ Done |
| 2 | P4: Error chain integration tests | Small | None | ✓ Done |
| 2b | P4b: Fix fmt.Errorf in security.Check | Trivial | None | ✓ Done |
| 3 | P2: callDepth panic → error | Medium | Low (signature change) | ✓ Done |
| 4 | P1: Incremental winding stack update | Small | Low | ✓ Done |
| 5 | P5: Document restArgBuf contract | Trivial | None | ✓ Done |
| 6 | P6: Pool effectiveness documentation | Trivial | None | ✓ Done |
