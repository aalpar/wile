# PullDrain Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace the O(n) `Pull()` + `Drain()` sequence in `OpPullApply` with an O(1) `PullDrain()` split.

**Architecture:** Add `Stack.PullDrain()` that returns `(proc, args)` by splitting `stack[0]` from `stack[1:]` without copying. Add `MachineContext.pullDrainAndApply()` that uses it. Wire into `OpPullApply` dispatch.

**Design doc:** `plans/2026-03-31-pulldrain-design.md`

---

### Task 1: Add `Stack.PullDrain()` with tests

**Files:**
- Modify: `machine/stack.go` (add method after `Pull()`, line ~49)
- Modify: `machine/stack_test.go` (add tests after existing `TestStackPull`, line ~254)

**Step 1: Write the failing tests**

Add to `machine/stack_test.go` after `TestStackPull` (line 254):

```go
func TestStack_PullDrain(t *testing.T) {
	c := qt.New(t)

	t.Run("multiple elements", func(t *testing.T) {
		s := NewStack()
		s.Push(values.NewInteger(1))
		s.Push(values.NewInteger(2))
		s.Push(values.NewInteger(3))

		proc, args := s.PullDrain()
		c.Assert(proc, valuestest.SchemeEquals, values.NewInteger(1))
		c.Assert(len(args), qt.Equals, 2)
		c.Assert(args[0], valuestest.SchemeEquals, values.NewInteger(2))
		c.Assert(args[1], valuestest.SchemeEquals, values.NewInteger(3))
		c.Assert(s.Len(), qt.Equals, 0)
	})

	t.Run("single element", func(t *testing.T) {
		s := NewStack()
		s.Push(values.NewInteger(42))

		proc, args := s.PullDrain()
		c.Assert(proc, valuestest.SchemeEquals, values.NewInteger(42))
		c.Assert(args, qt.IsNil)
		c.Assert(s.Len(), qt.Equals, 0)
	})

	t.Run("empty stack panics", func(t *testing.T) {
		s := NewStack()
		c.Assert(func() { s.PullDrain() }, qt.PanicMatches, `.*stack is empty.*`)
	})
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestStack_PullDrain ./machine/`
Expected: FAIL — `PullDrain` not defined.

**Step 3: Implement `Stack.PullDrain()`**

Add to `machine/stack.go` immediately after the `Pull()` method (after line 49):

```go
// PullDrain removes and returns the bottom value (position 0) as the first
// return, and all remaining values as the second return. The stack is cleared.
// This is O(1) — no element shifting, just slice header arithmetic.
//
// The returned args slice shares the stack's backing array (same contract
// as Drain). Valid only until the next stack mutation.
func (p *Stack) PullDrain() (values.Value, []values.Value) {
	n := len(*p)
	if n == 0 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "Stack.PullDrain: stack is empty"))
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

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestStack_PullDrain ./machine/`
Expected: PASS

**Step 5: Run full stack tests to check for regressions**

Run: `go test -v -run TestStack ./machine/`
Expected: All existing stack tests still pass.

---

### Task 2: Add `pullDrainAndApply()` and wire into `OpPullApply`

**Files:**
- Modify: `machine/machine_context_apply.go` (add method after `drainAndApply`, line ~243)
- Modify: `machine/machine_context_apply.go` (update `drainAndApply` doc comment, line ~230)
- Modify: `machine/machine_context.go` (change `OpPullApply` case, lines 485-491)

**Step 1: Add `pullDrainAndApply()` method**

Add to `machine/machine_context_apply.go` after `drainAndApply` (after line 243):

```go
// pullDrainAndApply splits the eval stack into proc (position 0) and args
// (positions 1..n) in O(1), then applies. Used by OpPullApply.
//
// Unlike drainAndApply (which takes the callable from the value register),
// this method extracts the callable from the bottom of the eval stack —
// matching the SECD calling convention where the procedure is evaluated
// first and pushed first.
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

**Step 2: Update `drainAndApply` doc comment**

Change the doc comment on `drainAndApply` (line 230-232) to remove the `OpPullApply` reference:

Old:
```go
// drainAndApply drains all arguments from the eval stack, updates counters,
// and applies the callable. This is the common pattern shared by OpApply,
// OpPullApply, OpCallLocal, and OpCallCachedBinding.
```

New:
```go
// drainAndApply drains all arguments from the eval stack, updates counters,
// and applies the callable. This is the common pattern shared by OpApply,
// OpCallLocal, and OpCallCachedBinding.
```

**Step 3: Change `OpPullApply` dispatch**

In `machine/machine_context.go`, replace the `OpPullApply` case (lines 485-491):

Old:
```go
		case OpPullApply:
			mc.SetValue(mc.evals.Pull())
			var err error
			mc, err = mc.drainAndApply(mc.GetValue())
			if err != nil {
				return err
			}
```

New:
```go
		case OpPullApply:
			var err error
			mc, err = mc.pullDrainAndApply()
			if err != nil {
				return err
			}
```

**Step 4: Run the existing `OpPullApply` dispatch test**

Run: `go test -v -run TestRunDispatch_OpPullApply ./machine/`
Expected: PASS — behavior is identical, only implementation changed.

**Step 5: Run full machine test suite**

Run: `go test ./machine/...`
Expected: All tests pass.

---

### Task 3: Lint and integration tests

**Step 1: Run linter**

Run: `make lint`
Expected: Clean.

**Step 2: Run full test suite**

Run: `make test`
Expected: All tests pass.

**Step 3: Run benchmarks to verify no regression**

Run: `make bench-gabriel`
Expected: No regression. Potential improvement on `apply`-heavy benchmarks.

---

### Task 4: Update TODO.md

**Files:**
- Modify: `TODO.md` (mark item done, line ~39)

**Step 1: Mark the TODO item as done**

Change the `Stack.Pull()` line from:
```
- [ ] **`Stack.Pull()` is O(n) in VM hot path** [High, M]: ...
```
To:
```
- [x] **`Stack.Pull()` is O(n) in VM hot path** [High, M, Done]: Replaced `Pull()` + `Drain()` in `OpPullApply` with O(1) `PullDrain()` that splits `stack[0]` (proc) from `stack[1:]` (args) without copying. Unfused `OpPull` unchanged (rare after peephole). `plans/2026-03-31-pulldrain-design.md`
```
