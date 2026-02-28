# Signals Review Remediation Plan

**Status: COMPLETE** — All 7 tasks merged in PR #361 (2026-02-28). Branch: `fix/signals-remediation`.

**Goal:** Implement all six proposed fixes from `plans/SIGNALS_REVIEW.md` (P1–P6) plus one additional fix (P4b) in a single feature branch, one commit per task.

**Architecture:** Seven independent tasks, ordered by signals-review priority. P3 and P2 touch the VM; P4/P4b touch the security/error chain; P1 touches dynamic-wind; P5/P6 are documentation-only. No circular dependencies between tasks — any can fail without blocking others.

**Tech Stack:** Go 1.23, `github.com/frankban/quicktest` for tests, `make lint && make covercheck` after each task.

---

## Pre-work: Create Feature Branch

```bash
git fetch origin
git checkout master
git pull --rebase origin master
git checkout -b fix/signals-remediation
```

---

## Task 1: P3 — Default maxCallDepth for Embedded Safety

**Priority:** 1 (highest — embedded safety)
**Signals finding:** F7 — unbounded recursion can OOM host
**Files:**
- Modify: `engine.go` (add const + `callDepthSet` field + default logic)
- Modify: `options.go` (update `WithMaxCallDepth` to set `callDepthSet`)
- Modify: `wile_test.go` (add `TestDefaultCallDepth`)

**Step 1: Write the failing test**

In `wile_test.go`, add after `TestWithMaxCallDepth` (after line ~1017):

```go
func TestDefaultCallDepth(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Engine created without WithMaxCallDepth must have a finite default limit.
	eng, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.Eval(ctx, "(define (f) (f)) (f)")
	c.Assert(errors.Is(err, values.ErrCallDepthExceeded), qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

```bash
go test -v -run TestDefaultCallDepth .
```

Expected: FAIL — no error is returned because maxCallDepth defaults to 0 (unlimited).

**Step 3: Add `DefaultMaxCallDepth` const and `callDepthSet` field**

In `engine.go`, add the const immediately before `type Engine struct`:

```go
// DefaultMaxCallDepth is the default call depth limit for new engines.
// At ~500 bytes per frame, 10000 frames ≈ 5MB. Use WithMaxCallDepth(0)
// to opt out of the limit explicitly.
const DefaultMaxCallDepth uint64 = 10000
```

In `options.go`, add `callDepthSet bool` to `engineConfig` (after `maxCallDepth uint64`):

```go
type engineConfig struct {
	// ... existing fields ...
	maxCallDepth  uint64
	callDepthSet  bool   // true if WithMaxCallDepth was explicitly called
	authorizer    security.Authorizer
}
```

**Step 4: Apply the default in `NewEngine`**

In `engine.go`, add after the options loop (after `for _, opt := range opts { opt(cfg) }`):

```go
	// Apply default call depth when the caller did not set one explicitly.
	// WithMaxCallDepth(0) means unlimited — callDepthSet tracks whether the
	// caller opted in, so we don't override an explicit zero.
	if !cfg.callDepthSet {
		cfg.maxCallDepth = DefaultMaxCallDepth
	}
```

**Step 5: Update `WithMaxCallDepth` in `options.go`**

In `options.go`, update the `WithMaxCallDepth` function:

```go
// WithMaxCallDepth sets the maximum recursion depth for the VM.
// When the continuation stack exceeds this depth, ErrCallDepthExceeded is returned.
// A value of 0 means unlimited (no depth check). When not called, the engine
// uses DefaultMaxCallDepth (10000).
func WithMaxCallDepth(n uint64) EngineOption {
	return func(cfg *engineConfig) {
		cfg.maxCallDepth = n
		cfg.callDepthSet = true
	}
}
```

**Step 6: Run test to verify it passes**

```bash
go test -v -run TestDefaultCallDepth .
go test -v -run TestWithMaxCallDepth .
```

Expected: PASS for both. The "zero means unlimited" case in `TestWithMaxCallDepth` must still pass because it calls `WithMaxCallDepth(0)` which sets `callDepthSet = true`.

**Step 7: Run full test suite and lint**

```bash
go test ./... && make lint && make covercheck
```

**Step 8: Commit**

```bash
git add engine.go options.go wile_test.go
git commit -m "feat: default MaxCallDepth=10000 for embedded safety (P3)"
```

---

## Task 2: P4 — Error Chain Integration Tests

**Priority:** 2
**Signals finding:** F6 — error chain has no explicit test; any layer losing Unwrap() breaks errors.Is silently
**Files:**
- Create: `engine_error_chain_test.go`

**Step 1: Identify required imports**

The new test file lives in package `wile` (the root package). Required imports:
- `context`
- `errors`
- `testing`
- `github.com/aalpar/wile/extensions/eval` (or internal; check existing sandbox imports)
- `github.com/aalpar/wile/security`
- `github.com/aalpar/wile/values`
- `github.com/frankban/quicktest`

Check the exact eval import path used in `engine_sandbox_test.go`:
```bash
grep "eval" engine_sandbox_test.go | grep import -A5
```

**Step 2: Create the test file**

```go
// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// ...
package wile

import (
	"context"
	"errors"
	"testing"

	eval "github.com/aalpar/wile/internal/extensions/eval"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// TestErrorChain_SecurityDenial_PreservesErrAccessDenied verifies that the full
// wrapping chain from ErrAccessDenied through goErrorToSchemeException to
// RuntimeError preserves errors.Is reachability.
func TestErrorChain_SecurityDenial_PreservesErrAccessDenied(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithExtension(eval.Extension),
		WithAuthorizer(security.DenyAll()),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.Eval(ctx, `(eval '(+ 1 2))`)
	c.Assert(err, qt.IsNotNil)

	// Outermost error must be a RuntimeError.
	var re *RuntimeError
	if !errors.As(err, &re) {
		t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
	}

	// ErrAccessDenied must be reachable through the full wrapping chain.
	c.Assert(errors.Is(err, security.ErrAccessDenied), qt.IsTrue,
		qt.Commentf("errors.Is chain broken; full error: %v", err))
}

// TestErrorChain_CallDepthExceeded verifies that ErrCallDepthExceeded is
// reachable through the RuntimeError wrapping chain.
func TestErrorChain_CallDepthExceeded(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithMaxCallDepth(5))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.Eval(ctx, `(define (f) (f)) (f)`)
	c.Assert(err, qt.IsNotNil)

	var re *RuntimeError
	if !errors.As(err, &re) {
		t.Fatalf("expected *RuntimeError, got %T: %v", err, err)
	}

	c.Assert(errors.Is(err, values.ErrCallDepthExceeded), qt.IsTrue,
		qt.Commentf("errors.Is chain broken; full error: %v", err))
}
```

**Step 3: Run the tests**

```bash
go test -v -run TestErrorChain .
```

Expected: PASS — the chain already works; these tests make the contract explicit.

**Step 4: Run full test suite and lint**

```bash
go test ./... && make lint
```

**Step 5: Commit**

```bash
git add engine_error_chain_test.go
git commit -m "test: verify error chain for security denial and call depth (P4)"
```

---

## Task 3: P4b — Fix `fmt.Errorf` in `security/context.go`

**Priority:** 3 (same PR, separate commit — user directive)
**Signals finding:** F6 / project convention — `security/context.go:48` uses banned `fmt.Errorf`
**Files:**
- Modify: `security/context.go`

**Step 1: Read the current implementation**

```bash
# Verify exact current code at the target line
sed -n '38,50p' security/context.go
```

Current at line 48:
```go
return fmt.Errorf("%s %s %q: %w", req.Action, req.Resource, req.Target, err)
```

**Step 2: Verify the error chain tests pass before changing**

```bash
go test -v -run TestErrorChain .
```

Expected: PASS.

**Step 3: Apply the fix**

In `security/context.go`, replace the `fmt.Errorf` with `values.WrapForeignErrorf`:

```go
return values.WrapForeignErrorf(err, "%s %s %q", req.Action, req.Resource, req.Target)
```

Update the import block — remove `"fmt"`, add `"github.com/aalpar/wile/values"`:

```go
import (
	"context"

	"github.com/aalpar/wile/values"
)
```

**Step 4: Run the error chain tests**

```bash
go test -v -run TestErrorChain .
go test -v ./security/...
```

Expected: PASS — `WrapForeignErrorf` preserves the `Unwrap()` chain.

**Step 5: Run full test suite and lint**

```bash
go test ./... && make lint
```

**Step 6: Commit**

```bash
git add security/context.go
git commit -m "fix: replace fmt.Errorf with WrapForeignErrorf in security.Check (P4b)"
```

---

## Task 4: P2 — Convert `PopContinuation` Panic to Error Return

**Priority:** 4
**Signals finding:** F3 — panic in embedded interpreter kills host process
**Files:**
- Modify: `machine/machine_context.go` (definition)
- Modify: `machine/machine_context_test.go` (3 call sites)
- Modify: `machine/source_tracking_coverage_test.go` (1 call site)

**Context note:** `PopContinuation` is NOT called in the VM hot path. The `Run()` loop uses `RestoreAndRelease` instead. `PopContinuation` is a public API only called from tests today. The change is low-risk: same logic, just error return instead of panic.

**Step 1: Write a test for the underflow case**

In `machine/machine_context_test.go`, add a test for underflow behavior:

```go
func TestPopContinuation_Underflow(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Popping from an empty continuation chain must return an error, not panic.
	_, err := mc.PopContinuation()
	c.Assert(errors.Is(err, values.ErrCallDepthExceeded), qt.IsTrue)
}
```

**Step 2: Run test to verify it panics (pre-fix)**

```bash
go test -v -run TestPopContinuation_Underflow ./machine/ 2>&1
```

Expected: FAIL with panic: `callDepth underflow in PopContinuation`.

**Step 3: Change the signature and implementation**

In `machine/machine_context.go`, replace `PopContinuation`:

```go
// PopContinuation pops the current continuation from the machine context and returns it.
// It restores the machine context to the state saved in the popped continuation.
// Returns ErrCallDepthExceeded if callDepth would go below zero (compiler bug).
//
// Note: Unlike Restore(), we do NOT copy evals here because PopContinuation is used
// for normal function return where the continuation is consumed once. Restore() is
// used for continuation re-entry (call/cc) where the same continuation may be invoked
// multiple times, requiring the copy to prevent stack corruption.
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

**Step 4: Fix all call sites in test files**

In `machine/machine_context_test.go`, find the 3 call sites (lines ~147, ~152, ~1219) and update each.

For the first call in `TestMachineContext_PushContinuation_2` (line ~147):
```go
// BEFORE:
mc.PopContinuation()
qt.Assert(t, mc.cont, qt.Equals, bottom1)

// AFTER:
_, err := mc.PopContinuation()
qt.Assert(t, err, qt.IsNil)
qt.Assert(t, mc.cont, qt.Equals, bottom1)
```

For the second call in the same function (line ~152) — `err` is already in scope, use `=` not `:=`:
```go
// BEFORE:
mc.PopContinuation()
qt.Assert(t, mc.cont, qt.Equals, bottom0)

// AFTER:
_, err = mc.PopContinuation()
qt.Assert(t, err, qt.IsNil)
qt.Assert(t, mc.cont, qt.Equals, bottom0)
```

For the loop at line ~1219:
```go
// BEFORE:
for range 3 {
    mc.PopContinuation()
}

// AFTER:
for range 3 {
    _, err := mc.PopContinuation()
    qt.Assert(t, err, qt.IsNil)
}
```

In `machine/source_tracking_coverage_test.go` at line ~425:
```go
// BEFORE:
mc.PopContinuation()
c.Assert(d.ShouldStep(mc), qt.IsTrue)

// AFTER:
_, err := mc.PopContinuation()
c.Assert(err, qt.IsNil)
c.Assert(d.ShouldStep(mc), qt.IsTrue)
```

Note: In `source_tracking_coverage_test.go`, verify whether `err` is already declared in scope — use `:=` for first declaration, `=` for subsequent ones in the same function.

**Step 5: Run diagnostics**

```bash
go build ./machine/ 2>&1
```

Expected: No errors. All callers updated.

**Step 6: Run tests**

```bash
go test -v -run TestPopContinuation ./machine/
go test ./machine/
```

Expected: PASS for all, including new underflow test.

**Step 7: Run full suite and lint**

```bash
go test ./... && make lint
```

**Step 8: Commit**

```bash
git add machine/machine_context.go machine/machine_context_test.go machine/source_tracking_coverage_test.go
git commit -m "fix: convert PopContinuation panic to error return (P2)"
```

---

## Task 5: P1 — Incremental Winding Stack Update in `unwindStackTo`

**Priority:** 5
**Signals finding:** F1 — partial unwind leaves winding stack claiming exited extents are still active
**Files:**
- Modify: `machine/machine_context.go` (function body only)

**Context:** The change makes `p.windingStack` reflect reality at every step of the unwind loop. Each iteration now updates the stack immediately when the extent exits — whether or not the after-thunk was present. Cost: one extra slice header assignment per iteration (no allocation; slice headers are 3 words).

**Step 1: Read and understand the current implementation**

Read `machine/machine_context.go` at `unwindStackTo` (lines ~1260–1283).

Current:
```go
func (p *MachineContext) unwindStackTo(stack WindingStack, commonDepth int) error {
    for i := len(stack) - 1; i >= commonDepth; i-- {
        frame := stack[i]
        if frame.After != nil {
            sub := p.NewSubContext()
            sub.windingStack = stack[:i:i]
            _, err := sub.ApplyCallable(frame.After)
            if err != nil {
                ReleaseSubContext(sub)
                return err               // winding stack NOT updated — stale
            }
            err = sub.Run()
            ReleaseSubContext(sub)
            if err != nil {
                return err               // winding stack NOT updated — stale
            }
        }
    }
    p.windingStack = stack[:commonDepth:commonDepth]  // update only on full success
    return nil
}
```

**Step 2: Write or verify a test for the partial-unwind behavior**

Check if any existing test covers `unwindStackTo` error paths:
```bash
grep -n "unwindStackTo\|TestUnwind\|TestDynamicWind" machine/machine_context_test.go | head -10
```

If no test covers the partial unwind consistency, note this as a documentation gap (testing the error path requires a thunk that fails after a previous thunk succeeds — complex to set up). The change is low-risk: it only affects error paths, not the success path.

**Step 3: Apply the fix**

In `machine/machine_context.go`, replace the body of `unwindStackTo`:

```go
func (p *MachineContext) unwindStackTo(stack WindingStack, commonDepth int) error {
	// Run after thunks from innermost to outermost (reverse order).
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
				// Propagate escapes and exceptions.
				p.windingStack = stack[:i:i]
				return err
			}
		}
		// This extent is now exited; update winding stack immediately.
		p.windingStack = stack[:i:i]
	}
	// Update current winding stack to common ancestor.
	p.windingStack = stack[:commonDepth:commonDepth]
	return nil
}
```

**Step 4: Run tests**

```bash
go test ./machine/
go test ./...
```

Expected: PASS. The success path is functionally identical to before (same final assignment). Only the error paths are different.

**Step 5: Lint and covercheck**

```bash
make lint && make covercheck
```

**Step 6: Commit**

```bash
git add machine/machine_context.go
git commit -m "fix: update winding stack incrementally in unwindStackTo (P1)"
```

---

## Task 6: P5 — Document `restArgBuf` Safety Contract

**Priority:** 6
**Signals finding:** F2 — implicit temporal coupling; new variadic primitive authors will not know about the copy requirement
**Files:**
- Modify: `registry/CLAUDE.md`

**Step 1: Find the insertion point**

`registry/CLAUDE.md` does not have a `## Gotchas` section yet. Create one at the end of the file — it's a non-obvious hazard for primitive authors.

**Step 2: Add the documentation**

Add a new `## Gotchas` section at the end of `registry/CLAUDE.md`:

```markdown
- **Variadic rest-arg buffer**: When a variadic primitive is applied via the `noCopyApply`
  path, the rest-arg list (the `values.Tuple` for `...args`) is backed by
  `MachineContext.restArgBuf` — a reusable `PairBlock`. The list is valid only for the
  duration of the foreign function call. If your primitive stores the rest-arg list (returns
  it, puts it in a data structure, or passes it to a sub-context that outlives the call),
  you MUST copy the spine first. See `PrimList` in `registry/core/prim_lists.go` for the
  canonical copy pattern. Failure to copy creates a latent aliasing bug that corrupts the
  next variadic call's arguments.
```

**Step 3: Verify the CLAUDE.md renders cleanly**

```bash
# No automated check — just read the file to confirm formatting
head -200 registry/CLAUDE.md | tail -30
```

**Step 4: Commit**

```bash
git add registry/CLAUDE.md
git commit -m "docs: document restArgBuf safety contract for variadic primitive authors (P5)"
```

---

## Task 7: P6 — Pool Effectiveness Documentation in `counters.go`

**Priority:** 7
**Signals finding:** F5 — pool degradation under call/cc is graceful but the metric is implicit
**Files:**
- Modify: `machine/counters.go`

**Step 1: Find the insertion point**

`machine/counters.go` defines `VMCounters`. The fields `SharedFrameRestores` (line ~38) and `ContinuationPoolReleases` (line ~36) are the two operands for the metric.

**Step 2: Add the comment block**

Add a comment immediately before `ContinuationPoolReleases` in the `VMCounters` struct:

```go
	// Pool effectiveness under call/cc:
	//   ratio = SharedFrameRestores / (SharedFrameRestores + ContinuationPoolReleases)
	//   0.0 = no call/cc impact (all frames recycled via pool)
	//   1.0 = all frames shared (no recycling, pure GC pressure)
	//   > 0.5 = pool losing more than it saves; consider profiling GC pauses
	ContinuationPoolReleases uint64
```

**Step 3: Run build to confirm no issues**

```bash
go build ./machine/
```

**Step 4: Commit**

```bash
git add machine/counters.go
git commit -m "docs: document pool effectiveness ratio for call/cc workloads (P6)"
```

---

## Final Verification

After all 7 tasks are complete:

```bash
make lint && make covercheck
go test -race ./...
```

Then open a PR:

```bash
gh pr create --title "fix: signals review remediation (P1–P6)" \
  --body "Implements all six proposed changes from plans/SIGNALS_REVIEW.md plus a convention fix in security/context.go (P4b). One commit per task, ordered by signals-review priority."
```

---

## Dependency Notes

- **Task 3 (P4b) depends on Task 2 (P4 tests)**: Run the error chain tests before and after the `fmt.Errorf` fix to confirm the chain is preserved.
- **Task 2 does NOT depend on Task 1**: `TestErrorChain_CallDepthExceeded` uses `WithMaxCallDepth(5)`, which already exists today.
- All other tasks are independent.

## Priority Table (from SIGNALS_REVIEW.md)

| Task | Change | Effort | Risk |
|------|--------|--------|------|
| 1 (P3) | Default maxCallDepth | Small | Low (breaking but v1.x, CLAUDE.md permits) |
| 2 (P4) | Error chain tests | Small | None |
| 3 (P4b) | Fix fmt.Errorf in security.Check | Trivial | None |
| 4 (P2) | callDepth panic → error | Medium | Low (signature change, test-only callers) |
| 5 (P1) | Incremental winding stack update | Small | Low |
| 6 (P5) | Document restArgBuf | Trivial | None |
| 7 (P6) | Pool effectiveness comment | Trivial | None |
