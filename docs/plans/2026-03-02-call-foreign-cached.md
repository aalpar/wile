# OpCallForeignCached Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add two peephole-emitted opcodes that bypass the SaveContinuation/RestoreAndRelease round-trip for calls to known `*ForeignClosure` primitives.

**Architecture:** The compiler is unchanged (nanopass principle). A new peephole rule in `Optimize()` recognizes `SaveContinuation + PushCachedBinding + ... + PullApply` sequences where the cached binding holds a `*ForeignClosure`, and rewrites them to a single `OpCallForeignCached` (non-tail) or `OpCallForeignCachedTail` (tail). Two new `case` arms in `Run()` execute the foreign function directly, eliminating continuation machinery.

**Tech Stack:** Go, machine package bytecode VM

**Design doc:** `docs/plans/2026-03-02-call-foreign-cached-design.md`

---

### Task 1: Add opcode constants and metadata

**Files:**
- Modify: `machine/opcode.go`
- Test: `machine/opcode_test.go`

**Step 1: Add the two new opcode constants**

In `machine/opcode.go`, add after the `OpPushCachedBinding` line and before `OpComplex`:

```go
	// Wave 7: direct foreign call operations (Arg = index into cachedBindings)
	// Emitted by peephole only — compiler never produces these.
	OpCallForeignCached     // Non-tail: call ForeignClosure, then mc.pc++
	OpCallForeignCachedTail // Tail: call ForeignClosure, then returnImmediate()
```

**Step 2: Add opcodeTable entries**

In the `opcodeTable` var, add before the `OpComplex` entry:

```go
	OpCallForeignCached:     {name: "CallForeignCached"},
	OpCallForeignCachedTail: {name: "CallForeignCachedTail"},
```

Neither `writesValue` nor `isBranch` — the opcode writes the value register as a side effect of calling `fcls.fn(mc)`, but it also reads the eval stack, so it's not a pure "writes value" in the dead-code-elimination sense. Not a branch.

**Step 3: Run existing opcode tests to verify nothing breaks**

Run: `go test -v -run TestOpCode ./machine/...`

Expected: PASS (existing tests still pass with new constants)

**Step 4: Add String() test for new opcodes**

In `machine/opcode_test.go`, add cases to the existing String test table:

```go
{OpCallForeignCached, "CallForeignCached"},
{OpCallForeignCachedTail, "CallForeignCachedTail"},
```

Run: `go test -v -run TestOpCode ./machine/...`

Expected: PASS

**Step 5: Commit**

```
feat(machine): add OpCallForeignCached and OpCallForeignCachedTail constants
```

---

### Task 2: Add instructionToOperation decomposition

**Files:**
- Modify: `machine/native_template.go`

The new opcodes are peephole-only, never emitted by the compiler, so they don't need `operationToInstruction` entries. But `instructionToOperation` is used by tests to decompose instructions back to operations. For the fused call opcodes, decompose back to `LoadCachedBinding` (same pattern as `OpPushCachedBinding` → `LoadCachedBinding` and `OpPullApply` → `Pull`).

**Step 1: Add cases to instructionToOperation**

After the `case OpPullApply:` block in `instructionToOperation`, add:

```go
	// --- Wave 7: direct foreign call operations ---
	// Decomposed back to LoadCachedBinding for test assertions.
	case OpCallForeignCached:
		return NewOperationLoadCachedBinding(instr.Arg)
	case OpCallForeignCachedTail:
		return NewOperationLoadCachedBinding(instr.Arg)
```

**Step 2: Run tests**

Run: `go test -v -run TestNativeTemplate ./machine/...`

Expected: PASS

**Step 3: Commit**

```
feat(machine): add instructionToOperation decomposition for CallForeignCached
```

---

### Task 3: Implement the peephole rule

**Files:**
- Modify: `machine/peephole.go`
- Test: `machine/peephole_test.go`

**Step 1: Write failing tests for the peephole rule**

Add test cases to `machine/peephole_test.go`. The test needs a `NativeTemplate` with `cachedBindings` containing a `*ForeignClosure`, and instruction sequences that match the patterns.

Test cases needed:
1. **Non-tail match**: `SaveCont(off) + PushCachedBinding(0) + PullApply` where binding holds `*ForeignClosure` → rewrites to `OpCallForeignCached(0)`
2. **Non-tail with args**: `SaveCont(off) + PushCachedBinding(0) + PushLocal(x) + PushLocal(y) + PullApply` → `PushLocal(x) + PushLocal(y) + OpCallForeignCached(0)`
3. **Tail match**: `PushCachedBinding(0) + PullApply` (no SaveCont) → `OpCallForeignCachedTail(0)`
4. **Tail with args**: `PushCachedBinding(0) + PushLocal(x) + PullApply` → `PushLocal(x) + OpCallForeignCachedTail(0)`
5. **No match: non-ForeignClosure binding** — binding holds a `*MachineClosure` → no rewrite
6. **No match: branch target in interior** — `PushCachedBinding` is a branch target → no rewrite
7. **SaveCont offset doesn't land on PullApply** — no rewrite (offset points elsewhere)

Each test constructs a `NativeTemplate` manually, calls `Optimize()`, and asserts the resulting `code` slice.

Helper needed: create an env + `*ForeignClosure` and add it as a cached binding.

```go
func TestFuseCallForeignCached(t *testing.T) {
	env := newBenchEnv()
	stubFn := func(mc *MachineContext) error {
		mc.SetValue(values.Void)
		return nil
	}
	fcls := NewForeignClosure(env, 2, false, stubFn)

	tcs := []struct {
		name     string
		code     []Instruction
		binding  values.Value // what cachedBindings[0].Value() returns
		expected []Instruction
	}{
		{
			name: "non-tail: SaveCont + PushCachedBinding + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3}, // offset 3 → lands on PullApply at index 2
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			binding: fcls,
			expected: []Instruction{
				{Op: OpCallForeignCached, Arg: 0},
			},
		},
		{
			name: "non-tail with args: SaveCont + PushCachedBinding + PushLocal + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 5},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpPullApply},
			},
			binding: fcls,
			expected: []Instruction{
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPushLocal, Arg: 2},
				{Op: OpCallForeignCached, Arg: 0},
			},
		},
		{
			name: "tail: PushCachedBinding + PullApply (no SaveCont)",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			binding: fcls,
			expected: []Instruction{
				{Op: OpCallForeignCachedTail, Arg: 0},
			},
		},
		{
			name: "tail with args: PushCachedBinding + PushLocal + PullApply",
			code: []Instruction{
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPushLocal, Arg: 1},
				{Op: OpPullApply},
			},
			binding: fcls,
			expected: []Instruction{
				{Op: OpPushLocal, Arg: 1},
				{Op: OpCallForeignCachedTail, Arg: 0},
			},
		},
		{
			name: "no match: binding is MachineClosure",
			code: []Instruction{
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
			binding: NewClosureWithTemplate(
				NewNativeTemplate(0, 0, false),
				env,
			),
			expected: []Instruction{
				{Op: OpSaveContinuation, Arg: 3},
				{Op: OpPushCachedBinding, Arg: 0},
				{Op: OpPullApply},
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tpl := NewNativeTemplate(0, 0, false)
			for _, instr := range tc.code {
				tpl.code = append(tpl.code, instr)
				tpl.sourceRefs = append(tpl.sourceRefs, 0)
			}
			// Set up cached binding with the test value.
			bd := environment.NewBinding(environment.BindingTypeVariable)
			bd.SetValue(tc.binding)
			tpl.cachedBindings = append(tpl.cachedBindings, bd)

			tpl.Optimize()

			qt.Assert(t, tpl.code, qt.DeepEquals, tc.expected)
		})
	}
}
```

Note: The `environment.NewBinding` and `bd.SetValue` calls need verification against actual API. Check `environment/binding.go` for exact constructors.

Run: `go test -v -run TestFuseCallForeignCached ./machine/...`

Expected: FAIL (fuseCallForeignCached not yet implemented)

**Step 2: Implement fuseCallForeignCached**

Add to `machine/peephole.go`:

```go
// fuseCallForeignCached replaces SaveContinuation + PushCachedBinding + ... + PullApply
// sequences with a single OpCallForeignCached when the cached binding holds a
// *ForeignClosure. For tail calls (no SaveContinuation), emits OpCallForeignCachedTail.
//
// This eliminates the continuation round-trip for known primitive calls.
func fuseCallForeignCached(tpl *NativeTemplate, plan *EditPlan) {
	code := tpl.code
	targets := branchTargets(code)

	for i := 0; i < len(code); i++ {
		// Try non-tail pattern: SaveContinuation + PushCachedBinding + ... + PullApply
		if code[i].Op == OpSaveContinuation && i+1 < len(code) && code[i+1].Op == OpPushCachedBinding {
			bindIdx := code[i+1].Arg
			fcls, ok := tpl.cachedBindings[bindIdx].Value().(*ForeignClosure)
			if !ok || fcls == nil {
				continue
			}
			// Verify SaveContinuation offset lands on PullApply
			pullApplyPos := i + int(code[i].Arg)
			if pullApplyPos >= len(code) || code[pullApplyPos].Op != OpPullApply {
				continue
			}
			// Check no branch targets in interior (i+1 through pullApplyPos)
			hasBranchTarget := false
			for j := i + 1; j <= pullApplyPos; j++ {
				if targets[j] {
					hasBranchTarget = true
					break
				}
			}
			if hasBranchTarget {
				continue
			}
			// Replace: delete SaveCont and PushCachedBinding, replace PullApply with CallForeignCached
			plan.Delete(i, i+2)          // delete SaveCont + PushCachedBinding
			plan.Replace(pullApplyPos, pullApplyPos+1,
				[]Instruction{{Op: OpCallForeignCached, Arg: bindIdx}},
				tpl.sourceRefs[pullApplyPos],
			)
			continue
		}

		// Try tail pattern: PushCachedBinding + ... + PullApply (no preceding SaveContinuation)
		if code[i].Op == OpPushCachedBinding {
			// Skip if preceded by SaveContinuation (handled above)
			if i > 0 && code[i-1].Op == OpSaveContinuation {
				continue
			}
			bindIdx := code[i].Arg
			fcls, ok := tpl.cachedBindings[bindIdx].Value().(*ForeignClosure)
			if !ok || fcls == nil {
				continue
			}
			// Scan forward for PullApply
			pullApplyPos := -1
			for j := i + 1; j < len(code); j++ {
				if code[j].Op == OpPullApply {
					pullApplyPos = j
					break
				}
				// Only allow Push-family ops between callee and PullApply
				if !isPushOp(code[j].Op) {
					break
				}
			}
			if pullApplyPos < 0 {
				continue
			}
			// Check no branch targets in interior
			hasBranchTarget := false
			for j := i; j <= pullApplyPos; j++ {
				if targets[j] {
					hasBranchTarget = true
					break
				}
			}
			if hasBranchTarget {
				continue
			}
			// Replace: delete PushCachedBinding, replace PullApply with CallForeignCachedTail
			plan.Delete(i, i+1)
			plan.Replace(pullApplyPos, pullApplyPos+1,
				[]Instruction{{Op: OpCallForeignCachedTail, Arg: bindIdx}},
				tpl.sourceRefs[pullApplyPos],
			)
		}
	}
}

// isPushOp returns true for opcodes that push a value onto the eval stack
// (used to validate the interior of a call sequence).
func isPushOp(op OpCode) bool {
	switch op {
	case OpPush, OpPushLiteral, OpPushGlobal, OpPushLocal, OpPushCachedBinding:
		return true
	default:
		return false
	}
}
```

**Step 3: Wire fuseCallForeignCached into Optimize()**

In `machine/peephole.go`, add the call inside `Optimize()` after `fusePullApply` and before `plan.Apply()`:

```go
	fuseCallForeignCached(p, plan)
```

So the pipeline becomes:
```go
	markDeadLoadVoidEdits(p.code, plan)
	fuseLoadPush(p.code, p.sourceRefs, plan)
	fusePullApply(p.code, p.sourceRefs, plan)
	fuseCallForeignCached(p, plan)
	plan.Apply()
```

**Step 4: Run peephole tests**

Run: `go test -v -run TestFuseCallForeignCached ./machine/...`

Expected: PASS

Run: `go test -v ./machine/...` to verify no regressions.

**Step 5: Commit**

```
feat(machine): peephole rule to fuse foreign calls into OpCallForeignCached
```

---

### Task 4: Implement Run() dispatch for OpCallForeignCached

**Files:**
- Modify: `machine/machine_context.go`

**Step 1: Write failing test**

A test that constructs a `NativeTemplate` with `OpCallForeignCached` directly, sets up a `*ForeignClosure` in `cachedBindings`, pushes args onto the eval stack, and calls `Run()`. Verify the foreign function executes and the value register contains the result.

Add to `machine/machine_context_test.go` (or a new `machine/call_foreign_cached_test.go`):

```go
func TestOpCallForeignCached_NonTail(t *testing.T) {
	env := newBenchEnv()

	// Foreign function: adds two args
	addFn := func(mc *MachineContext) error {
		bnds := mc.env.LocalEnvironment().Bindings()
		a := bnds[0].Value().(*values.Integer)
		b := bnds[1].Value().(*values.Integer)
		mc.SetValue(a.Add(b))
		return nil
	}
	fcls := NewForeignClosure(env, 2, false, addFn)

	// Build template with: PushLiteral(3) + PushLiteral(4) + CallForeignCached(0)
	tpl := NewNativeTemplate(0, 0, false)
	litIdx3 := tpl.MaybeAppendLiteral(values.NewInteger(3))
	litIdx4 := tpl.MaybeAppendLiteral(values.NewInteger(4))

	bd := environment.NewBinding(environment.BindingTypeVariable)
	bd.SetValue(fcls)
	tpl.cachedBindings = append(tpl.cachedBindings, bd)

	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx3)})
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx4)})
	tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: 0})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	err := mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(7))
}
```

Note: Verify `NewBinding` constructor, `AppendInstruction`, and `AcquireTopLevelContext` signatures against actual code. The `environment.NewBinding` may require different args.

Run: `go test -v -run TestOpCallForeignCached_NonTail ./machine/...`

Expected: FAIL (unimplemented opcode)

**Step 2: Implement the non-tail case in Run()**

In `machine/machine_context.go`, add after the `case OpPushCachedBinding:` block and before `case OpComplex:`:

```go
		// --- Wave 7: direct foreign call operations ---

		case OpCallForeignCached:
			mc, err = callForeignCached(mc, instr, false)
			if err != nil {
				return err
			}

		case OpCallForeignCachedTail:
			mc, err = callForeignCached(mc, instr, true)
			if err != nil {
				return err
			}
```

Add a helper function (in `machine_context.go` or a new file `call_foreign_cached.go`):

```go
// callForeignCached executes a *ForeignClosure resolved from cachedBindings[instr.Arg].
// This is the fast path for peephole-optimized primitive calls, bypassing
// SaveContinuation/RestoreAndRelease entirely.
//
// For non-tail calls (tail=false): advances mc.pc after the call.
// For tail calls (tail=true): calls returnImmediate() to pop to the caller's caller.
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	fcls := mc.template.cachedBindings[instr.Arg].Value().(*ForeignClosure)
	vs := mc.evals.PopAll()
	mc.counters.StackPopAlls++
	mc.counters.StackElementsCopied += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	l := fcls.paramCount

	// Arity check.
	if !fcls.isVariadic {
		if len(vs) != l {
			return nil, applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"expected %d arguments, got %d", l, len(vs)))
		}
	} else {
		if len(vs) < l-1 {
			return nil, applyCallableError(mc, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"expected at least %d arguments, got %d", l-1, len(vs)))
		}
	}

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	// Bind args into closure's own env (noCopyApply by construction).
	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	if !fcls.isVariadic {
		for i := range bnds[:l] {
			bnds[i].SetValue(vs[i])
		}
	} else {
		for i := range bnds[:l-1] {
			bnds[i].SetValue(vs[i])
		}
		bnds[l-1].SetValue(mc.buildRestArg(vs, l-1))
	}

	mc.env = env
	mc.envPooled = false

	savedTemplate := mc.template
	err := fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	// If the foreign function changed the template (defensive — no current
	// ForeignClosure does this), let the VM continue from wherever it pointed.
	if mc.template != savedTemplate {
		return mc, nil
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
		mc.pc++
	}
	return mc, nil
}
```

**Step 3: Run the test**

Run: `go test -v -run TestOpCallForeignCached ./machine/...`

Expected: PASS

**Step 4: Add tail-call test**

```go
func TestOpCallForeignCached_Tail(t *testing.T) {
	env := newBenchEnv()

	addFn := func(mc *MachineContext) error {
		bnds := mc.env.LocalEnvironment().Bindings()
		a := bnds[0].Value().(*values.Integer)
		b := bnds[1].Value().(*values.Integer)
		mc.SetValue(a.Add(b))
		return nil
	}
	fcls := NewForeignClosure(env, 2, false, addFn)

	// Build template: PushLiteral(5) + PushLiteral(6) + CallForeignCachedTail(0)
	tpl := NewNativeTemplate(0, 0, false)
	litIdx5 := tpl.MaybeAppendLiteral(values.NewInteger(5))
	litIdx6 := tpl.MaybeAppendLiteral(values.NewInteger(6))

	bd := environment.NewBinding(environment.BindingTypeVariable)
	bd.SetValue(fcls)
	tpl.cachedBindings = append(tpl.cachedBindings, bd)

	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx5)})
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx6)})
	tpl.AppendInstruction(Instruction{Op: OpCallForeignCachedTail, Arg: 0})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	err := mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(11))
}
```

**Step 5: Add error-handling test (arity mismatch)**

```go
func TestOpCallForeignCached_ArityError(t *testing.T) {
	env := newBenchEnv()
	fcls := NewForeignClosure(env, 2, false, func(mc *MachineContext) error {
		return nil
	})

	// Push only 1 arg but closure expects 2
	tpl := NewNativeTemplate(0, 0, false)
	litIdx := tpl.MaybeAppendLiteral(values.NewInteger(1))

	bd := environment.NewBinding(environment.BindingTypeVariable)
	bd.SetValue(fcls)
	tpl.cachedBindings = append(tpl.cachedBindings, bd)

	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: 0})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	err := mc.Run()
	qt.Assert(t, err, qt.IsNotNil)
}
```

**Step 6: Run all tests**

Run: `go test -v ./machine/...`

Expected: PASS

**Step 7: Commit**

```
feat(machine): implement OpCallForeignCached dispatch in Run()
```

---

### Task 5: Integration test — end-to-end through compiler + peephole

**Files:**
- Test: `machine/call_foreign_cached_test.go` or `machine/peephole_test.go`

**Step 1: Write integration test using Scheme code**

Use `testhelpers.RunSchemeCode` to compile and run Scheme expressions that should trigger the peephole optimization. Verify correctness of results.

```go
func TestCallForeignCached_Integration(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "simple add", Code: `(+ 3 4)`, Expected: values.NewInteger(7)},
		{Name: "nested non-tail", Code: `(+ (+ 1 2) (+ 3 4))`, Expected: values.NewInteger(10)},
		{Name: "comparison", Code: `(<= 1 2)`, Expected: values.TrueValue},
		{Name: "car", Code: `(car '(1 2 3))`, Expected: values.NewInteger(1)},
		{Name: "variadic", Code: `(+ 1 2 3 4 5)`, Expected: values.NewInteger(15)},
		{Name: "tail call primitive", Code: `(define (f x) (+ x 1)) (f 5)`, Expected: values.NewInteger(6)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

Note: These test the same results as before — the optimization is transparent. The value is confirming the peephole doesn't break anything.

**Step 2: Run integration tests**

Run: `go test -v -run TestCallForeignCached_Integration ./machine/...`

Expected: PASS

**Step 3: Run full test suite**

Run: `make test`

Expected: PASS

**Step 4: Run lint**

Run: `make lint`

Expected: PASS

**Step 5: Commit**

```
test(machine): integration tests for OpCallForeignCached optimization
```

---

### Task 6: Benchmark and validate savings

**Files:**
- Test: `machine/fib_bench_test.go` or `machine/call_foreign_cached_bench_test.go`

**Step 1: Run existing fib benchmark (baseline already captured on this branch)**

Run: `go test -bench=BenchmarkRun/Fibonacci -benchtime=3s -count=5 ./machine/...`

Capture the ns/op.

**Step 2: Add a microbenchmark for the new opcode**

```go
func BenchmarkCallForeignCached(b *testing.B) {
	env := newBenchEnv()
	fcls := NewForeignClosure(env, 2, false, stubLe)

	tpl := NewNativeTemplate(0, 0, false)
	litIdx5 := tpl.MaybeAppendLiteral(values.NewInteger(5))
	litIdx1 := tpl.MaybeAppendLiteral(values.NewInteger(1))

	bd := /* create binding holding fcls */
	tpl.cachedBindings = append(tpl.cachedBindings, bd)

	// PushLiteral(5) + PushLiteral(1) + CallForeignCached(0) + Branch(-3)
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx5)})
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx1)})
	tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: 0})
	tpl.AppendInstruction(Instruction{Op: OpBranch, Arg: -3})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	mc.SetMaxCallDepth(0) // disable depth check

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mc.pc = 0
		_ = mc.Run()
	}
	b.StopTimer()
	ReleaseTopLevelContext(mc)
}
```

Note: The Branch(-3) creates a loop so b.N controls iteration count. Adjust the branch offset to match instruction count. Verify the loop terminates — may need to set a max iteration or break condition.

**Step 3: Compare against BenchmarkDeferRecoverFib (which measures applyForeign)**

The new opcode should be faster than the `applyForeign` path measured by `BenchmarkDeferRecoverFib` because it skips the `SaveContinuation`/`RestoreAndRelease` round-trip.

**Step 4: Commit**

```
bench(machine): add BenchmarkCallForeignCached microbenchmark
```

---

### Task 7: Final validation

**Step 1: Run full test suite**

Run: `make test`

Expected: PASS

**Step 2: Run lint + covercheck**

Run: `make lint && make covercheck`

Expected: PASS

**Step 3: Run integration tests**

Run: `go test -v ./...`

Expected: PASS
