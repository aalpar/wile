# Stack Frames Implementation Plan

> **Status: CLOSED.** Implemented and reverted (PR #518, 2026-03-17). Dispatch improved 5% on fib but regressed continuation-heavy benchmarks 10-20% (ctak +20%, takl +13%, nqueens +13%). Net negative. Pool-based `MachineContinuation` linked list retained. This plan is preserved as a historical record.

---

**Goal:** Replace the per-call `MachineContinuation` pool-allocated linked list with a contiguous `[]callFrame` slice, eliminating 65.78% of allocation bytes and reducing SaveContinuation/RestoreAndRelease round-trip cost from 18.4ns to 9.3ns.

**Architecture:** The `MachineContext.cont *MachineContinuation` linked list becomes `MachineContext.callStack []callFrame`. SaveContinuation appends a value-type frame to the slice. PopContinuation/RestoreAndRelease reslice. `call/cc` materializes the callStack into a heap-allocated `*MachineContinuation` chain (cold path: 0.3% of continuation operations). `ComposableContinuation` and `CapturedContinuation` continue to store `*MachineContinuation` chains. A hybrid `materializedBase *MachineContinuation` field handles composable continuation grafts.

**Tech Stack:** Go 1.24, pure Go (no unsafe), `machine/` package only.

**Parent design:** `plans/FLAT-CLOSURES.md` § T1. Profiling results in T1.1.

---

## Invariants (Must Hold After Every Task)

1. `make test` passes — zero regressions
2. `make lint` clean
3. `make covercheck` passes
4. Gabriel benchmarks produce identical results
5. ZebraPuzzle produces correct answer

## Terminology

| Term | Meaning |
|------|---------|
| **callStack** | `[]callFrame` on `MachineContext` — contiguous array of saved frames |
| **callFrame** | Value type holding saved state for one non-tail call |
| **materialized chain** | `*MachineContinuation` linked list created from callStack for `call/cc` capture |
| **materializedBase** | `*MachineContinuation` field on `MachineContext` — residual chain from composable continuation graft |
| **hot path** | SaveContinuation → RestoreAndRelease/PopContinuation (99.7% of continuation ops) |
| **cold path** | `call/cc` capture → materialize callStack to heap chain (0.3% of continuation ops) |

---

## Task 1: Define `callFrame` Type

**Files:**
- Create: `machine/call_frame.go`
- Test: `machine/call_frame_test.go`

**Step 1: Write the test**

```go
// machine/call_frame_test.go
package machine

import (
	"testing"
	"unsafe"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestCallFrameZeroValue(t *testing.T) {
	c := qt.New(t)
	var f callFrame
	c.Assert(f.env, qt.IsNil)
	c.Assert(f.template, qt.IsNil)
	c.Assert(f.singleValue, qt.IsNil)
	c.Assert(f.evals, qt.IsNil)
	c.Assert(f.promptTag, qt.IsNil)
	c.Assert(f.shared, qt.IsFalse)
	c.Assert(f.pc, qt.Equals, 0)
}

func TestCallFrameSize(t *testing.T) {
	// callFrame should be under 256 bytes so append's memmove is cheap.
	size := unsafe.Sizeof(callFrame{})
	t.Logf("callFrame size: %d bytes", size)
	if size > 256 {
		t.Errorf("callFrame too large: %d bytes (limit 256)", size)
	}
}

func TestCallFrameInlineEvals(t *testing.T) {
	c := qt.New(t)
	var f callFrame
	f.inlineEvalsLen = 2
	f.inlineEvals[0] = values.NewInteger(1)
	f.inlineEvals[1] = values.NewInteger(2)
	c.Assert(f.inlineEvalsLen, qt.Equals, uint8(2))
	c.Assert(f.inlineEvals[0].SchemeString(), qt.Equals, "1")
	c.Assert(f.inlineEvals[1].SchemeString(), qt.Equals, "2")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -run='TestCallFrame' -v ./machine/`
Expected: FAIL — `callFrame` undefined.

**Step 3: Write the implementation**

```go
// machine/call_frame.go
package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

// callFrame holds the saved state for one non-tail call.
// Stored by value in MachineContext.callStack ([]callFrame) — no heap
// allocation per frame. Go's append copies the struct via memmove.
//
// Field set matches the save/restore table in vm_state.go:79-95.
// Fields NOT saved per-frame: windingStack (managed separately),
// promptTag/promptHandler (only on prompt frames).
type callFrame struct {
	env         *environment.EnvironmentFrame
	freeVars    []values.Value
	template    *NativeTemplate
	singleValue values.Value
	multiValues MultipleValues
	pc          int
	threadID    uint64
	callDepth   int
	envPooled   bool
	marks       []markEntry

	// Prompt fields — non-nil only for frames created by
	// call-with-continuation-prompt. Zero cost for normal frames.
	promptTag     *PromptTag
	promptHandler Closure

	// shared is set during call/cc capture. When true, the frame's
	// eval stack must be copied (not transferred) on restore, and the
	// frame must not be zeroed until the captured continuation is GC'd.
	shared bool

	// Inline eval storage — same semantics as MachineContinuation.
	// When evals is nil, values are in inlineEvals[0:inlineEvalsLen].
	inlineEvalsLen uint8
	inlineEvals    [inlineEvalsCap]values.Value
	evals          *Stack
}

// restoreInlineEvalsFromFrame clears dst and pushes the frame's inline
// eval values. Mirrors restoreInlineEvals but operates on *callFrame.
func restoreInlineEvalsFromFrame(dst *Stack, f *callFrame) {
	dst.Clear()
	for i := uint8(0); i < f.inlineEvalsLen; i++ {
		dst.Push(f.inlineEvals[i])
	}
}
```

**Step 4: Run test to verify it passes**

Run: `go test -run='TestCallFrame' -v ./machine/`
Expected: PASS

**Step 5: Verify build**

Run: `make lint`
Expected: clean

**Step 6: Commit**

Do not commit — per CLAUDE.md, the user structures commits.

---

## Task 2: Add `callStack` Field to `MachineContext`

**Files:**
- Modify: `machine/machine_context.go:61-81` (add field to struct)
- Modify: `machine/pool.go:134-142` (`AcquireTopLevelContext` — initialize callStack)
- Modify: `machine/machine_context_subcontext.go:38-52` (`NewSubContext` — initialize callStack)
- Modify: `machine/machine_context.go:86-112` (`NewMachineContext` — initialize callStack)
- Test: `machine/call_frame_test.go` (add initialization tests)

### Step 1: Write tests for callStack initialization

Append to `machine/call_frame_test.go`:

```go
func TestAcquireTopLevelContext_HasCallStack(t *testing.T) {
	c := qt.New(t)
	env := newBenchEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)
	c.Assert(mc.callStack, qt.IsNotNil)
	c.Assert(len(mc.callStack), qt.Equals, 0)
	c.Assert(cap(mc.callStack) >= 16, qt.IsTrue)
}

func TestNewSubContext_HasCallStack(t *testing.T) {
	c := qt.New(t)
	env := newBenchEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)
	sub := mc.NewSubContext()
	defer ReleaseSubContext(sub)
	c.Assert(sub.callStack, qt.IsNotNil)
	c.Assert(len(sub.callStack), qt.Equals, 0)
}
```

### Step 2: Run tests — expect FAIL (callStack field doesn't exist)

### Step 3: Add the field and initialization

In `machine/machine_context.go:61-81`, add to the struct:

```go
callStack        []callFrame          // contiguous call stack (replaces cont chain on hot path)
materializedBase *MachineContinuation // residual chain from composable continuation graft
```

In `machine/pool.go:134-142` (`AcquireTopLevelContext`), after `mc.evals = acquireStack()`:

```go
if mc.callStack == nil {
	mc.callStack = make([]callFrame, 0, 64)
} else {
	mc.callStack = mc.callStack[:0]
}
```

In `machine/machine_context_subcontext.go:38-52` (`NewSubContext`), after `mc.evals = acquireStack()`:

```go
if mc.callStack == nil {
	mc.callStack = make([]callFrame, 0, 64)
} else {
	mc.callStack = mc.callStack[:0]
}
```

In `machine/machine_context.go:86-112` (`NewMachineContext`), add to the struct literal:

```go
callStack: make([]callFrame, 0, 64),
```

In the pool reset function for `subContextPool` (`pool.go:62-65`), preserve the callStack backing array:

```go
func(mc *MachineContext) {
	releaseStack(mc.evals)
	saved := mc.callStack
	*mc = MachineContext{}
	if saved != nil {
		mc.callStack = saved[:0]
	}
},
```

### Step 4: Run tests — expect PASS

Run: `go test -run='TestCallFrame|TestAcquire' -v ./machine/`

### Step 5: Run full suite

Run: `make test`
Expected: all pass (callStack is allocated but unused).

### Step 6: `make lint`

---

## Task 3: Dual-Write SaveContinuation

**Goal:** SaveContinuation writes to BOTH `callStack` AND the continuation chain. This validates the callFrame content without changing behavior.

**Files:**
- Modify: `machine/machine_context_continuation.go:197-227` (`SaveContinuation`)
- Test: `machine/call_frame_test.go` (add dual-write validation)

### Step 1: Write test

```go
func TestSaveContinuation_DualWrite(t *testing.T) {
	c := qt.New(t)
	env := newBenchEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)
	mc.SetValue(values.NewInteger(42))

	err := mc.SaveContinuation(3)
	c.Assert(err, qt.IsNil)

	// Continuation chain written
	c.Assert(mc.cont, qt.IsNotNil)
	c.Assert(mc.cont.pc, qt.Equals, 3)

	// callStack also written
	c.Assert(len(mc.callStack), qt.Equals, 1)
	frame := &mc.callStack[0]
	c.Assert(frame.pc, qt.Equals, 3)
	c.Assert(frame.template, qt.Equals, tpl)
	c.Assert(frame.env, qt.Equals, env)
}
```

### Step 2: Run — expect FAIL

### Step 3: Add dual-write to SaveContinuation

In `machine/machine_context_continuation.go`, in `SaveContinuation`, after `p.cont = cont` (line 224), insert before `p.marks = nil`:

```go
// Dual-write: mirror the continuation frame into callStack.
// This is transitional — Task 5 removes the cont chain write.
p.callStack = append(p.callStack, callFrame{
	env:         cont.env,
	freeVars:    cont.freeVars,
	template:    cont.template,
	singleValue: cont.singleValue,
	multiValues: cont.multiValues,
	pc:          cont.pc,
	threadID:    cont.threadID,
	callDepth:   cont.callDepth,
	envPooled:   cont.envPooled,
	marks:       cont.marks,
	// evals: mirrored below
})
csTop := &p.callStack[len(p.callStack)-1]
if cont.evals == nil {
	csTop.inlineEvalsLen = cont.inlineEvalsLen
	csTop.inlineEvals = cont.inlineEvals
} else {
	// evals was transferred to cont, not available here.
	// callStack frame stores nil; Task 5 will own the transfer.
	csTop.evals = nil
	csTop.inlineEvalsLen = 0
}
```

### Step 4: Run test — expect PASS

### Step 5: Run `make test` — all pass (callStack populated but not read)

---

## Task 4: Dual-Read PopContinuation/RestoreAndRelease

**Goal:** After every Pop/Restore, assert that callStack depth matches cont chain length. This catches field mismatches before the behavioral change.

**Files:**
- Modify: `machine/machine_context_continuation.go` (add assertion after Pop/Restore)
- Test: `machine/call_frame_test.go`

### Step 1: Write test

```go
func TestSaveAndPop_CallStackInSync(t *testing.T) {
	c := qt.New(t)
	env := newBenchEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)
	mc.SetValue(values.Void)

	// Save 3 times
	for i := 0; i < 3; i++ {
		err := mc.SaveContinuation(0)
		c.Assert(err, qt.IsNil)
	}
	c.Assert(len(mc.callStack), qt.Equals, 3)

	// RestoreAndRelease 3 times
	for i := 0; i < 3; i++ {
		mc.RestoreAndRelease(mc.cont)
	}
	c.Assert(len(mc.callStack), qt.Equals, 0)
}
```

### Step 2: Run — expect FAIL (RestoreAndRelease doesn't pop callStack)

### Step 3: Add callStack pop to RestoreAndRelease and PopContinuation

In `RestoreAndRelease` (`machine_context_continuation.go:80`), at the very top (after counter increment, before field reads):

```go
// Pop callStack mirror (transitional dual-read).
if len(p.callStack) > 0 {
	top := len(p.callStack) - 1
	p.callStack[top] = callFrame{} // break GC refs
	p.callStack = p.callStack[:top]
}
```

In `PopContinuation` (`machine_context_continuation.go:161`), after the callDepth decrement and underflow check:

```go
// Pop callStack mirror (transitional dual-read).
if len(p.callStack) > 0 {
	top := len(p.callStack) - 1
	p.callStack[top] = callFrame{}
	p.callStack = p.callStack[:top]
}
```

### Step 4: Run test — expect PASS

### Step 5: Run `make test` — all pass

---

## Task 5: Switch SaveContinuation to callStack-Only

**Goal:** SaveContinuation writes to callStack and no longer allocates a `MachineContinuation`. This is the behavioral change for the hot path.

**This is the hardest task. It changes the invariant from "cont is a linked list" to "callStack is the primary store, cont is only for materialized chains."**

**Files:**
- Modify: `machine/machine_context_continuation.go` (rewrite SaveContinuation, RestoreAndRelease, PopContinuation)
- Modify: `machine/machine_context.go:366-371` (OpRestoreContinuation in Run loop)
- Modify: `machine/call_foreign_cached.go:87` (RestoreAndRelease call)
- Modify: `machine/machine_context_apply.go:154,248` (returnImmediate pattern)
- Test: comprehensive integration tests

### Note on scope

This task description covers the design direction. The actual implementation will require careful attention to every `mc.cont` reference identified in the codebase search (50+ sites across 16 files). The engineer implementing this should:

1. `grep -rn 'mc\.cont\b\|p\.cont\b' machine/*.go` to find all sites
2. Classify each as: (a) hot-path save/restore, (b) call/cc capture, (c) composable continuation, (d) debugger, (e) stack trace
3. Convert (a) to callStack operations
4. Convert (b) to `materializeCallStack()` + callStack
5. Leave (c) operating on materialized chains
6. Convert (d) and (e) to callStack array walks

### Key changes

**SaveContinuation becomes:**

```go
func (p *MachineContext) SaveContinuation(off int) error {
	p.callDepth++
	if p.maxCallDepth > 0 && uint64(p.callDepth) > p.maxCallDepth {
		p.callDepth--
		return werr.WrapForeignErrorf(werr.ErrCallDepthExceeded,
			"call depth %d exceeds limit %d", p.callDepth+1, p.maxCallDepth)
	}
	p.counters.ContinuationsSaved++

	frame := callFrame{
		env:         p.env,
		freeVars:    p.freeVars,
		template:    p.template,
		singleValue: p.singleValue,
		multiValues: p.multiValues,
		pc:          p.pc + off,
		threadID:    p.threadID,
		callDepth:   p.callDepth - 1,
		envPooled:   p.envPooled,
		marks:       p.marks,
	}

	n := p.evals.Len()
	if n <= inlineEvalsCap {
		frame.inlineEvalsLen = uint8(n)
		for i := range n {
			frame.inlineEvals[i] = (*p.evals)[i]
		}
		p.evals.Clear()
		p.counters.InlineEvalsSaved++
	} else {
		frame.evals = p.evals
		p.evals = acquireStack()
	}

	p.callStack = append(p.callStack, frame)
	p.marks = nil
	return nil
}
```

**RestoreAndRelease reads from callStack:**

The method signature changes. Instead of taking a `*MachineContinuation`, it reads from the top of callStack. The `Run` loop's `OpRestoreContinuation` case and `callForeignCached`'s restore call need updating.

**PopContinuation reads from callStack:**

Same pattern — reads from top of callStack, returns env release info.

**`mc.cont` usage in Run loop:**

- `OpRestoreContinuation`: change from `mc.RestoreAndRelease(mc.cont)` to `mc.RestoreFromCallStack()`
- The `if mc.cont == nil` halt check changes to `if len(mc.callStack) == 0 && mc.materializedBase == nil`

**`mc.cont` in returnImmediate / applyForeign:**

These check `if p.cont != nil` to decide whether to restore or halt. With callStack: `if len(p.callStack) > 0 || p.materializedBase != nil`.

### Tests

Full integration test suite exercising:
- Simple non-tail call + return (fib)
- Tail calls (no SaveContinuation)
- `call/cc` capture + escape
- `call/cc` capture + re-invocation
- Composable continuations (abort + handler)
- `dynamic-wind` + continuation escape
- Continuation marks across stack frames
- Debugger step-out (uses mc.cont for frame identity)
- ZebraPuzzle (Schelog backtracking stress test)

Run: `make test && make bench-gabriel`

---

## Task 6: Materialize CallStack for `call/cc`

**Files:**
- Create: `machine/call_frame_materialize.go`
- Modify: `machine/machine_context_continuation.go` (`CurrentContinuation`, `FindPrompt`, `SliceContinuationAt`)
- Test: `machine/call_frame_materialize_test.go`

### Implementation

```go
// machine/call_frame_materialize.go

// materializeCallStack converts the callStack into a heap-allocated
// MachineContinuation linked chain. Used by call/cc capture (cold path).
//
// After materialization, all callStack frames are marked shared.
// The returned chain's bottom frame's parent is materializedBase
// (for composable continuation graft residuals).
func (p *MachineContext) materializeCallStack() *MachineContinuation {
	if len(p.callStack) == 0 {
		return p.materializedBase
	}
	// Build chain bottom-up: callStack[0] is deepest, callStack[N-1] is shallowest.
	var chain *MachineContinuation = p.materializedBase
	for i := range p.callStack {
		f := &p.callStack[i]
		cont := &MachineContinuation{
			vmState: vmState{
				env:         f.env,
				freeVars:    f.freeVars,
				template:    f.template,
				singleValue: f.singleValue,
				multiValues: slices.Clone(f.multiValues),
				pc:          f.pc,
				threadID:    f.threadID,
				callDepth:   f.callDepth,
				envPooled:   false,
				marks:       cloneMarks(f.marks),
			},
			parent:        chain,
			promptTag:     f.promptTag,
			promptHandler: f.promptHandler,
		}
		if f.evals == nil {
			cont.inlineEvalsLen = f.inlineEvalsLen
			cont.inlineEvals = f.inlineEvals
		} else {
			cont.evals = f.evals.Copy()
		}
		chain = cont
	}
	// Mark callStack frames as shared
	for i := range p.callStack {
		p.callStack[i].shared = true
	}
	return chain
}
```

**CurrentContinuation** becomes:

```go
func (p *MachineContext) CurrentContinuation() *MachineContinuation {
	chain := p.materializeCallStack()
	if chain != nil {
		chain.MarkChainShared()
	}
	return chain
}
```

**FindPrompt** scans callStack first, falls through to materializedBase:

```go
func (p *MachineContext) FindPrompt(tag *PromptTag) (int, *MachineContinuation, bool) {
	// Scan callStack (newest first)
	for i := len(p.callStack) - 1; i >= 0; i-- {
		if p.callStack[i].promptTag == tag {
			return i, nil, true
		}
	}
	// Scan materializedBase chain
	for frame := p.materializedBase; frame != nil; frame = frame.parent {
		if frame.promptTag == tag {
			return -1, frame, true
		}
	}
	// Check context-level prompt
	if p.promptTag == tag {
		return -1, nil, true
	}
	return -1, nil, false
}
```

Note: `FindPrompt` signature changes — callers must be updated. This is a breaking internal API change that must be propagated to all call sites.

### Tests

- Materialize empty callStack → returns materializedBase
- Materialize with 3 frames → linked chain with correct parent pointers
- Materialize preserves inline evals
- Materialize copies evals (not transfers)
- Materialize marks frames shared
- FindPrompt finds prompt in callStack
- FindPrompt finds prompt in materializedBase
- FindPrompt finds context-level prompt

---

## Task 7: CaptureStackTrace and CollectContinuationMarks

**Files:**
- Modify: `machine/machine_context.go:912+` (`CaptureStackTrace`)
- Modify: `machine/continuation_mark_set.go:101-126` (`CollectContinuationMarks`)

### Changes

Both methods currently walk `p.cont` linked list. Change to walk `p.callStack` array (newest first), then fall through to `p.materializedBase` chain if present.

**CaptureStackTrace:**

```go
func (p *MachineContext) CaptureStackTrace(maxDepth int) StackTrace {
	trace := make(StackTrace, 0, 16)
	if p.template != nil {
		trace = append(trace, StackFrame{
			FunctionName: p.template.Name(),
			CurrentLoc:   p.template.SourceAt(p.pc),
		})
	}
	// Walk callStack (newest first)
	for i := len(p.callStack) - 1; i >= 0 && len(trace) < maxDepth; i-- {
		f := &p.callStack[i]
		frame := StackFrame{}
		if f.template != nil {
			frame.FunctionName = f.template.Name()
			frame.CurrentLoc = f.template.SourceAt(f.pc - 1)
		}
		trace = append(trace, frame)
	}
	// Walk materializedBase chain
	for cont := p.materializedBase; cont != nil && len(trace) < maxDepth; cont = cont.parent {
		frame := StackFrame{}
		if cont.template != nil {
			frame.FunctionName = cont.template.Name()
			frame.CurrentLoc = cont.template.SourceAt(cont.pc - 1)
		}
		trace = append(trace, frame)
	}
	// ... truncation logic unchanged ...
	return trace
}
```

**CollectContinuationMarks:** Same pattern — walk callStack then materializedBase.

---

## Task 8: Debugger Adaptation

**Files:**
- Modify: `machine/debugger.go:171,200`

### Changes

The debugger's `StepOut` mode stores `mc.cont` as the target frame (`stepFrame`). With callStack, frame identity by pointer no longer works — frames are values in an array, not heap objects.

**Fix:** `StepOut` stores the call depth instead of a frame pointer:

```go
// In Debugger:
// stepFrame *MachineContinuation  // REMOVE
// stepFrameDepth int              // ALREADY EXISTS, reuse for StepOut

func (p *Debugger) StepOut(mc *MachineContext) {
	p.stepMode = StepOut
	p.stepFrameDepth = mc.CallDepth()
}

func (p *Debugger) ShouldStep(mc *MachineContext) bool {
	// ...
	case StepOut:
		return mc.CallDepth() < p.stepFrameDepth
	// ...
}
```

The `stepFrame` field is removed.

---

## Task 9: Composable Continuation Graft

**Files:**
- Modify: `machine/machine_context_apply.go:330-357` (`applyComposableContinuation`)

### Design

When a composable continuation is applied, its `AcquireSegment()` returns a `*MachineContinuation` chain. Currently, `GraftContinuation(segment, p.cont)` splices it onto the continuation chain, then `p.Restore(segment)` resumes from the top.

With callStack, the graft target is the current call state (not `p.cont`). The approach:

1. Set `p.materializedBase` to the grafted chain (segment → current materializedBase)
2. `p.Restore(segment)` restores the top frame into `mc` as before
3. Subsequent `RestoreAndRelease` calls pop from callStack first; when callStack empties, they read from `materializedBase`

This hybrid means the normal fast path (callStack) handles new frames created after the graft, while the materialized chain handles frames that were part of the composable continuation.

### Key change in RestoreAndRelease

```go
func (p *MachineContext) restoreFromCallStackOrMaterialized() {
	if len(p.callStack) > 0 {
		// Fast path: pop from callStack
		// ... (same as Task 5)
	} else if p.materializedBase != nil {
		// Slow path: restore from materialized chain
		cont := p.materializedBase
		p.materializedBase = cont.parent
		p.RestoreAndRelease(cont) // original linked-list path
	} else {
		// Halt: no more frames
		p.template = immediateReturnTemplate
		p.pc = 0
	}
}
```

---

## Task 10: Remove Continuation Pool from Hot Path

**Files:**
- Modify: `machine/pool.go` (remove `continuationPool` usage from hot path; keep for materialized chains)

### Note

`continuationPool` cannot be fully removed in this task — materialized chains (`call/cc`, composable continuations) still use `acquireContinuation` / `releaseContinuation`. The pool stays but is now cold-path only.

What changes: `SaveContinuation` no longer calls `acquireContinuation`. `RestoreAndRelease` no longer calls `releaseContinuation` for callStack frames (only for materializedBase frames).

---

## Task 11: Cleanup and Counter Updates

**Files:**
- Modify: `machine/counters.go` (add `CallStackPops`, `MaterializedCaptures`)
- Remove: dual-write code from Tasks 3-4 (if any transitional code remains)
- Update: `machine/CLAUDE.local.md`, `machine/CLAUDE.md` to reflect new architecture

---

## Task 12: Benchmark Validation

**Goal:** Confirm the projected 5.9%–12.5% speedup on Fibonacci and measure actual impact across the Gabriel suite.

Run: `make bench-gabriel`

Compare against baseline from `plans/PERFORMANCE.md` "Benchmark Baseline (2026-03-16, ec26f1c8)".

Run: `go test -run='^$' -bench='BenchmarkRun/Fibonacci' -benchmem -count=6 .`

Expected:
- ns/op: 5-13% lower than 27,307ns baseline
- allocs/op: lower than 179 (continuation pool factory allocations eliminated)
- B/op: lower than 10,051 (65.78% of allocation bytes were continuation pool)

Record results in `plans/FLAT-CLOSURES.md` under a new "T1 Results" section.

---

## Dependency Order

```
Task 1 ──→ Task 2 ──→ Task 3 ──→ Task 4 ──→ Task 5 ──┐
                                                       ├──→ Task 10 ──→ Task 11 ──→ Task 12
                                            Task 6 ──→ ┤
                                            Task 7 ──→ ┤
                                            Task 8 ──→ ┤
                                            Task 9 ──→ ┘
```

Tasks 5-9 can be done in any order after Task 4, but Task 5 is the critical path — it changes the behavior. Tasks 6-9 adapt the cold-path consumers. Task 10 cleans up the pool. Task 11-12 are post-implementation.

---

## Root Cause Analysis: Continuation Chain Consumers

**Date:** 2026-03-17

The behavioral change (Tasks 5-9) exposed a systematic issue: every consumer of `mc.cont` needs to be updated to either:
- (a) Materialize the callStack before reading `p.cont`, or
- (b) Walk `p.callStack` directly

### Bug Found: `SliceContinuationAt` Did Not Materialize

**Symptom:** `(+ 1 (call-with-current-continuation (lambda (k) (k 3))))` returned 3 instead of 4.

**Root cause:** `PrimCallCC` (prim_control.go:151) calls `mc.SliceContinuationAt(nil)` to capture the continuation chain. `SliceContinuationAt` read from `p.cont`, which is now nil for the normal call/return path (SaveContinuation writes to callStack only). Result: empty segment → composable continuation had no frames → escape value passed through without resuming the `(+ 1 ...)` computation.

**Fix:** Add `if len(p.callStack) > 0 { p.cont = p.materializeCallStack() }` at the top of `SliceContinuationAt`. Applied (2026-03-17).

**Invariant established:** Any method that reads `p.cont` for continuation chain traversal MUST first check `len(p.callStack) > 0` and materialize if needed. The complete list of such methods:

| Method | Status |
|--------|--------|
| `CurrentContinuation` | Fixed — materializes via `materializeCallStack()` |
| `FindPrompt` | Fixed — materializes when prompt found in callStack |
| `SliceContinuationAt` | Fixed — materializes at top |
| `CaptureStackTrace` | Fixed — walks callStack first |
| `CollectContinuationMarks` | Fixed — walks callStack first |
| `GetImmediateMark` | Fixed — checks callStack top frame |
| `CreateEscapeContinuation` | New method — materializes internally |
| `Parent` | Returns sentinel for nil-check callers |
| `Restore` | Unchanged — only used with materialized chains |
| `RestoreAndRelease` | Unchanged — only used with materialized chains |
| `restoreFromCallStack` | New method — reads callStack directly |

### Remaining Failures (2 outside `machine/`)

1. **`TestRoundTrip_RaiseContinuable/resume-computation`** (internal/bootstrap) — `resumeFromContinuation` in `prim_exceptions.go` likely reads `mc.cont` or uses `RestoreAndRelease(mc.cont)` to resume after `raise-continuable`. Needs materialization.

2. **`TestEvalDynamicContextInheritance`** (internal/extensions/eval) — `eval` creates sub-contexts that may interact with the continuation chain. Needs investigation.

### Fix Plan

For each remaining failure:
1. Read the crash stack trace to find the `mc.cont` access
2. Add materialization (`materializeCallStack()`) before the access
3. Verify fix with the specific test
4. Run full suite to check for regressions

---

## PR Strategy

| PR | Tasks | Theme | Behavioral Change? |
|----|-------|-------|--------------------|
| A | 1-4 | Infrastructure + dual-write validation | No |
| B | 5-9 | Switch to callStack (behavioral change) | **Yes** |
| C | 10-12 | Cleanup + benchmarks | No |
