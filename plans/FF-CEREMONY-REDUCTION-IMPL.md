# ForeignFunction Call Ceremony Reduction — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reduce per-call overhead for peephole-optimized ForeignFunction calls by encoding compile-time-known arity in the instruction and eliminating redundant runtime checks.

**Architecture:** Bit-pack `paramCount` into the instruction's `Arg` field alongside the cached binding index. Split variadic/non-variadic into separate opcodes. Replace `checkArity` + `bindArgs` with a paramCount guard and unrolled SetValue calls. Slow path falls back to full ceremony for set!-reassigned bindings.

**Design doc:** `plans/FF-CEREMONY-REDUCTION.md`

---

### Task 1: Add encoding helpers and new opcodes

**Files:**
- Modify: `machine/instruction.go`
- Modify: `machine/opcode.go`

**Step 1: Add encoding helpers to `instruction.go`**

After `DecodeLocalIndex` (line 80), add:

```go
// EncodeForeignCallArg packs a cachedBindings index and paramCount into
// a single int32 for OpCallForeignCached instructions.
//
//	bits  0-15: cachedBindings index (0..65535)
//	bits 16-23: paramCount (0..255)
//	bits 24-31: reserved
func EncodeForeignCallArg(bindingIdx int32, paramCount int) int32 {
	return (bindingIdx & 0xFFFF) | int32(paramCount&0xFF)<<16
}

// DecodeForeignCallArg unpacks the cachedBindings index and paramCount
// from a bit-packed Instruction.Arg.
func DecodeForeignCallArg(arg int32) (bindingIdx int32, paramCount int) {
	return arg & 0xFFFF, int(arg>>16) & 0xFF
}
```

Update the `String()` method (line 38) to decode the new opcodes:

```go
if instr.Op == OpCallForeignCached || instr.Op == OpCallForeignCachedTail ||
	instr.Op == OpCallForeignCachedVar || instr.Op == OpCallForeignCachedVarTail {
	bindingIdx, paramCount := DecodeForeignCallArg(instr.Arg)
	return fmt.Sprintf("%s binding=%d params=%d", instr.Op, bindingIdx, paramCount)
}
```

**Step 2: Add new opcodes to `opcode.go`**

In the const block, after `OpCallForeignCachedTail` (line 77), add:

```go
OpCallForeignCachedVar     // Variadic: call ForeignClosure, then mc.pc++
OpCallForeignCachedVarTail // Variadic tail: call ForeignClosure, then returnImmediate()
```

In `opcodeTable` (line 121), after the `OpCallForeignCachedTail` entry, add:

```go
OpCallForeignCachedVar:     {name: "CallForeignCachedVar"},
OpCallForeignCachedVarTail: {name: "CallForeignCachedVarTail"},
```

**Step 3: Run tests**

Run: `go test ./machine/ -run TestInstruction -v` and `go test ./machine/ -run TestOpCode -v`

**Step 4: Commit**

```
feat(machine): add foreign call arg encoding and variadic opcodes
```

---

### Task 2: Write tests for the new encoding

**Files:**
- Modify: `machine/instruction_test.go` (or create if absent)

**Step 1: Add encoding round-trip tests**

```go
func TestEncodeForeignCallArg(t *testing.T) {
	tcs := []struct {
		name       string
		bindingIdx int32
		paramCount int
	}{
		{name: "zero/zero", bindingIdx: 0, paramCount: 0},
		{name: "small", bindingIdx: 5, paramCount: 2},
		{name: "max binding", bindingIdx: 0xFFFF, paramCount: 3},
		{name: "max params", bindingIdx: 42, paramCount: 255},
		{name: "both max", bindingIdx: 0xFFFF, paramCount: 255},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			encoded := EncodeForeignCallArg(tc.bindingIdx, tc.paramCount)
			gotIdx, gotPC := DecodeForeignCallArg(encoded)
			qt.Assert(t, gotIdx, qt.Equals, tc.bindingIdx)
			qt.Assert(t, gotPC, qt.Equals, tc.paramCount)
		})
	}
}
```

**Step 2: Run test**

Run: `go test ./machine/ -run TestEncodeForeignCallArg -v`

**Step 3: Commit**

```
test(machine): add foreign call arg encoding round-trip tests
```

---

### Task 3: Rewrite `callForeignCached` fast path

**Files:**
- Modify: `machine/call_foreign_cached.go`

**Step 1: Rewrite the file**

Replace the current `callForeignCached` with the new fast path that:
- Decodes `bindingIdx` and `paramCount` from `instr.Arg`
- Guards on `fcls.paramCount != paramCount` (replaces `checkArity`)
- Unrolls bindArgs for arity 1-3 via switch
- Removes template change check
- Falls back to `callForeignCachedSlow` on type assertion failure or paramCount mismatch

Add `callForeignCachedVar` for variadic calls that:
- Decodes the same way
- Guards on `fcls.paramCount != paramCount`
- Calls `bindArgs` with variadic path (no unrolling — variadic is rare)

Extract `callForeignCachedSlow` that handles both failure modes:
- Non-ForeignClosure → Drain + ApplyCallable
- ForeignClosure with wrong paramCount → full ceremony with checkArity

Keep `callForeignCachedReassigned` for the non-ForeignClosure case (reuse existing function).

The key structure:

```go
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	bindingIdx, paramCount := DecodeForeignCallArg(instr.Arg)
	callable := mc.template.cachedBindings[bindingIdx].Value()

	fcls, ok := callable.(*ForeignClosure)
	if !ok {
		return callForeignCachedReassigned(mc, callable)
	}
	if fcls.paramCount != paramCount {
		return callForeignCachedMismatch(mc, fcls)
	}

	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	switch paramCount {
	case 0:
		// no args
	case 1:
		bnds[0].SetValue(vs[0])
	case 2:
		bnds[0].SetValue(vs[0])
		bnds[1].SetValue(vs[1])
	case 3:
		bnds[0].SetValue(vs[0])
		bnds[1].SetValue(vs[1])
		bnds[2].SetValue(vs[2])
	default:
		for i := range bnds[:paramCount] {
			bnds[i].SetValue(vs[i])
		}
	}

	mc.env = env
	mc.envPooled = false

	err := fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
		mc.RestoreAndRelease(mc.cont)
	}
	return mc, nil
}

func callForeignCachedVar(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
	bindingIdx, paramCount := DecodeForeignCallArg(instr.Arg)
	callable := mc.template.cachedBindings[bindingIdx].Value()

	fcls, ok := callable.(*ForeignClosure)
	if !ok {
		return callForeignCachedReassigned(mc, callable)
	}
	if fcls.paramCount != paramCount {
		return callForeignCachedMismatch(mc, fcls)
	}

	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	bindArgs(bnds, vs, paramCount, true, mc.buildRestArg)

	mc.env = env
	mc.envPooled = false

	err := fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	if tail {
		mc = mc.returnImmediate()
	} else {
		mc.RestoreAndRelease(mc.cont)
	}
	return mc, nil
}

func callForeignCachedMismatch(mc *MachineContext, fcls *ForeignClosure) (*MachineContext, error) {
	vs := mc.evals.Drain()
	mc.counters.StackDrains++
	mc.counters.StackElementsDrained += uint64(len(vs))
	mc.counters.RecordStackDepth(len(vs))

	err := checkArity(fcls.paramCount, fcls.isVariadic, len(vs))
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	mc.counters.ClosuresApplied++
	mc.counters.NoCopyApplies++
	mc.counters.ForeignCalls++

	env := fcls.env
	bnds := env.LocalEnvironment().Bindings()
	mc.counters.NoCopyBindingsSaved += uint64(len(bnds))

	bindArgs(bnds, vs, fcls.paramCount, fcls.isVariadic, mc.buildRestArg)

	mc.env = env
	mc.envPooled = false

	err = fcls.fn(mc)
	if err != nil {
		return nil, applyCallableError(mc, err)
	}

	// Mismatch means set! changed the binding. Cannot know tail/non-tail
	// from the closure alone — but the opcode handler passes it through.
	// For now, treat as non-tail (conservative). The slow path is rare.
	mc.RestoreAndRelease(mc.cont)
	return mc, nil
}
```

**Note:** The mismatch path needs tail awareness. The simplest fix: add a `tail bool` parameter to `callForeignCachedMismatch` and handle both cases. Refine during implementation.

**Step 2: Run tests**

Run: `go test ./machine/ -run TestOpCallForeignCached -v`

Existing tests use the old Arg encoding (plain bindingIdx). They will need updating in Task 5.

**Step 3: Commit**

```
perf(machine): rewrite callForeignCached fast path with arity encoding
```

---

### Task 4: Add Run() dispatch for new opcodes

**Files:**
- Modify: `machine/machine_context.go`

**Step 1: Add cases to `Run()` switch**

After the existing `OpCallForeignCachedTail` case (~line 516), add:

```go
case OpCallForeignCachedVar:
	var err error
	mc, err = callForeignCachedVar(mc, instr, false)
	if err != nil {
		return err
	}

case OpCallForeignCachedVarTail:
	var err error
	mc, err = callForeignCachedVar(mc, instr, true)
	if err != nil {
		return err
	}
```

**Step 2: Run tests**

Run: `go test ./machine/ -count=1 -v -run TestOpCallForeignCached`

**Step 3: Commit**

```
feat(machine): add Run() dispatch for variadic foreign call opcodes
```

---

### Task 5: Update tests for new Arg encoding

**Files:**
- Modify: `machine/call_foreign_cached_test.go`

**Step 1: Update existing tests**

Every test that constructs `Instruction{Op: OpCallForeignCached, Arg: cbIdx}` must use `EncodeForeignCallArg(cbIdx, paramCount)` instead of bare `cbIdx`.

For example, the "non-tail: add 3+4=7" test (line 73):
```go
// Before:
tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})
// After:
tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: EncodeForeignCallArg(cbIdx, 2)})
```

The "variadic: sum with rest args" test (line 157) changes opcode:
```go
// Before:
tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})
// After:
tpl.AppendInstruction(Instruction{Op: OpCallForeignCachedVar, Arg: EncodeForeignCallArg(cbIdx, 2)})
```

**Step 2: Add set!-reassignment edge case tests**

Add to the test table:

```go
{
	name: "mismatch: binding replaced with different-arity ForeignClosure",
	setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
		env := environment.NewTopLevelEnvironment().Runtime()
		// Compile-time: 2-arg closure
		fcls2 := foreignAddClosure()
		bd := environment.NewBinding(fcls2, environment.BindingTypeVariable)

		tpl := NewNativeTemplate(0, 0, false)
		cbIdx := tpl.AppendCachedBinding(bd)

		// Runtime: replace binding with 0-arg closure (paramCount mismatch)
		fcls0 := foreignErrorClosure()
		bd.SetValue(fcls0)

		tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 2})
		// Push 0 args (matching the replacement closure's arity)
		tpl.AppendInstruction(Instruction{
			Op:  OpCallForeignCached,
			Arg: EncodeForeignCallArg(cbIdx, 2), // encoded as 2-arg from compile time
		})
		return tpl, env
	},
	// paramCount mismatch triggers slow path.
	// Slow path runs checkArity with actual closure (0 params, 0 args) → succeeds.
	// foreignErrorClosure returns an error.
	wantErr: werr.ErrNotAProcedure,
},
{
	name: "reassigned: binding replaced with non-ForeignClosure",
	setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
		env := environment.NewTopLevelEnvironment().Runtime()
		fcls := foreignAddClosure()
		bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

		tpl := NewNativeTemplate(0, 0, false)
		cbIdx := tpl.AppendCachedBinding(bd)

		// Replace with a non-callable value at runtime
		bd.SetValue(values.NewInteger(42))

		tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 2})
		tpl.AppendInstruction(Instruction{
			Op:  OpCallForeignCached,
			Arg: EncodeForeignCallArg(cbIdx, 2),
		})
		return tpl, env
	},
	wantErr: werr.ErrNotAProcedure,
},
```

**Step 3: Run tests**

Run: `go test ./machine/ -run TestOpCallForeignCached -v`

**Step 4: Commit**

```
test(machine): update call_foreign_cached tests for arity encoding
```

---

### Task 6: Update `native_template.go` conversion

**Files:**
- Modify: `machine/native_template.go`

**Step 1: Add cases to `instructionToOperation`**

After the existing `OpCallForeignCachedTail` case (line 195), add:

```go
case OpCallForeignCachedVar:
	bindingIdx, _ := DecodeForeignCallArg(instr.Arg)
	return NewOperationLoadCachedBinding(bindingIdx)
case OpCallForeignCachedVarTail:
	bindingIdx, _ := DecodeForeignCallArg(instr.Arg)
	return NewOperationLoadCachedBinding(bindingIdx)
```

Also update the existing `OpCallForeignCached`/`OpCallForeignCachedTail` cases to decode:

```go
case OpCallForeignCached:
	bindingIdx, _ := DecodeForeignCallArg(instr.Arg)
	return NewOperationLoadCachedBinding(bindingIdx)
case OpCallForeignCachedTail:
	bindingIdx, _ := DecodeForeignCallArg(instr.Arg)
	return NewOperationLoadCachedBinding(bindingIdx)
```

**Step 2: Run tests**

Run: `go test ./machine/ -run TestNativeTemplate -v`

**Step 3: Commit**

```
feat(machine): add native_template conversion for variadic opcodes
```

---

### Task 7: Update peephole optimizer

**Files:**
- Modify: `machine/peephole.go`

**Step 1: Update non-tail emission (line 293-298)**

Replace:
```go
plan.Replace(pullIdx, pullIdx+1,
	[]Instruction{{Op: OpCallForeignCached, Arg: bindingIdx}},
	tpl.sourceRefs[pullIdx],
)
```

With:
```go
encodedArg := EncodeForeignCallArg(bindingIdx, fcls.paramCount)
callOp := OpCallForeignCached
if fcls.isVariadic {
	callOp = OpCallForeignCachedVar
}
plan.Replace(pullIdx, pullIdx+1,
	[]Instruction{{Op: callOp, Arg: encodedArg}},
	tpl.sourceRefs[pullIdx],
)
```

**Step 2: Update tail emission (line 372-375)**

Replace:
```go
plan.Replace(pullIdx, pullIdx+1,
	[]Instruction{{Op: OpCallForeignCachedTail, Arg: bindingIdx}},
	tpl.sourceRefs[pullIdx],
)
```

With:
```go
encodedArg := EncodeForeignCallArg(bindingIdx, fcls.paramCount)
callOp := OpCallForeignCachedTail
if fcls.isVariadic {
	callOp = OpCallForeignCachedVarTail
}
plan.Replace(pullIdx, pullIdx+1,
	[]Instruction{{Op: callOp, Arg: encodedArg}},
	tpl.sourceRefs[pullIdx],
)
```

**Step 3: Run tests**

Run: `go test ./machine/ -run TestPeephole -v`

**Step 4: Commit**

```
perf(machine): emit arity-encoded foreign call opcodes in peephole
```

---

### Task 8: Full test suite + lint

**Step 1: Run full test suite**

Run: `go test ./...`

Fix any failures. Common issues:
- Tests that manually construct `OpCallForeignCached` instructions with bare binding indices
- Peephole test expectations that check for specific Arg values

**Step 2: Run lint**

Run: `make lint`

**Step 3: Run covercheck**

Run: `make covercheck`

**Step 4: Commit any fixes**

```
fix(machine): fix test expectations for arity-encoded foreign calls
```

---

### Task 9: Benchmark

**Step 1: Capture baseline on master**

```bash
git stash && git checkout master
make bench-gabriel > /tmp/gabriel-baseline.txt
go test -bench=Bench -count=3 ./registry/core/ > /tmp/micro-baseline.txt
git checkout - && git stash pop
```

**Step 2: Run benchmarks on branch**

```bash
make bench-gabriel > /tmp/gabriel-ceremony.txt
go test -bench=Bench -count=3 ./registry/core/ > /tmp/micro-ceremony.txt
```

**Step 3: Compare**

```bash
benchstat /tmp/gabriel-baseline.txt /tmp/gabriel-ceremony.txt
benchstat /tmp/micro-baseline.txt /tmp/micro-ceremony.txt
```

**Step 4: Record results**

Add benchmark results to `plans/FF-CEREMONY-REDUCTION.md` under a new `## Results` section.

**Step 5: Commit**

```
docs: record ceremony reduction benchmark results
```

---

### Task 10: Update TODO.md

**Files:**
- Modify: `TODO.md`

**Step 1: Update the performance item**

Change the `Optimize hot-path ForeignFunction calls` entry to reflect that ceremony reduction is complete and what remains (promotion/demotion evaluation).

**Step 2: Commit**

```
docs: update TODO.md with ceremony reduction status
```

---

## Verification Checklist

- [ ] `make lint` passes
- [ ] `make covercheck` passes
- [ ] `go test ./...` passes
- [ ] `make bench-gabriel` shows no regression (expect improvement)
- [ ] New opcodes in `opcode.go`, `opcodeTable`, `Run()`, `native_template.go`, `peephole.go`
- [ ] Old `checkArity` call removed from fast path
- [ ] Old `isVariadic` branch removed from fast path
- [ ] Old template change check removed
- [ ] set!-reassignment edge cases tested
- [ ] Benchmark results recorded
