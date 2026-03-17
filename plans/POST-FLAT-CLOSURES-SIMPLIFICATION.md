# Post-Flat-Closures Simplification

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Unify the Apply copy paths now that flat closures eliminated cross-lambda env chain walks, remove dead code, and clean up stale plan/TODO artifacts from the reverted stack-frames experiment.

**Architecture:** The Apply method's 3-way switch (noCopyApply / flat-copy / linked-copy) collapses to 2-way (noCopyApply / copy). The "linked copy" path's binding memcpy is dead work — the closure env's bindings are always compile-time void/unknown placeholders that get overwritten by `bindArgs` + body opcodes. `InitFlatApplyFrame` already skips this copy for exactly the right reason; that reasoning is universal, not flat-specific. Rename `InitFlatApplyFrame` → `InitApplyFrame` (it becomes THE apply init), delete the old binding-copying version, remove `IsFlat()` (zero callers), clean up counters, and update stale docs.

**Tech Stack:** Go 1.24, `machine/` and `environment/` packages, `plans/` and `TODO.md`

---

## Invariants (Must Hold After Every Task)

1. `make test` passes — zero regressions
2. `make lint` clean
3. `make covercheck` passes
4. Gabriel benchmarks produce identical results
5. Flat closure integration tests pass

---

## Task 1: Unify Apply Copy Paths

The core behavioral change. Collapse the 3-way switch to 2-way.

**Files:**
- Modify: `machine/machine_context_apply.go:47-86`

**Step 1: Rewrite the Apply switch**

Replace the 3-way switch (lines 47-86) with a 2-way switch:

```go
	switch {
	case tpl.NoCopyApply():
		// No-copy path: the template contains no SaveContinuation and no
		// MakeClosure/MakeFlatClosure, so mc.env is never captured. Safe to
		// mutate the closure's own bindings in place, eliminating both the
		// EnvironmentFrame and []Binding allocations.
		env = mcls.env
		bnds = env.LocalEnvironment().Bindings()
		// envPooled: closure's own env, not from pool.
		p.envPooled = false
		p.counters.NoCopyApplies++
		p.counters.NoCopyBindingsSaved += uint64(len(bnds))

	default:
		// Copy path: acquire a fresh frame from the pool and set up binding
		// slots WITHOUT copying values. bindArgs overwrites parameter slots
		// and body code (OpStoreLocal) initializes the rest, so the memcpy
		// is dead work. Free variables (if any) live in mcls.freeVars, not
		// in binding slots.
		env = acquireEnvFrame()
		mcls.env.InitFlatApplyFrame(env)
		bnds = env.LocalEnvironment().Bindings()
		p.envPooled = true
		p.counters.EnvsCopied++
	}
```

This removes:
- The `case mcls.freeVars != nil:` branch (line 60-70)
- The `BindingsCopied` and `KeysShared` counter increments (lines 84-85)
- The `FlatCopyApplies` counter increment (line 69)

The shared tail (lines 88-94) is unchanged — `p.freeVars = mcls.freeVars` still handles both nil (linked) and non-nil (flat).

**Step 2: Run tests**

Run: `make test`
Expected: PASS. Zero-capture closures (fib, tak) now use InitFlatApplyFrame instead of InitApplyFrame. The only behavioral difference is skipping the void-value memcpy.

**Step 3: Run lint**

Run: `make lint`
Expected: clean

---

## Task 2: Update Apply Counter Tests

Two tests reference the removed counters.

**Files:**
- Modify: `machine/machine_context_test.go:533-554` (`TestMachineContext_Apply_Counters`)
- Modify: `machine/flat_closure_test.go:469-514` (`TestFlatClosure_ApplyFlatCopyPath`)

**Step 1: Fix `TestMachineContext_Apply_Counters`**

This test creates a linked closure (zero captures) and verifies counters. After Task 1, it takes the unified copy path instead of the old linked copy path.

Replace lines 542-553:

```go
	before := mc.Counters()
	qt.Assert(t, before.ClosuresApplied, qt.Equals, uint64(0))
	qt.Assert(t, before.EnvsCopied, qt.Equals, uint64(0))

	_, err := mc.Apply(cls, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, err, qt.IsNil)

	after := mc.Counters()
	qt.Assert(t, after.ClosuresApplied, qt.Equals, uint64(1))
	qt.Assert(t, after.EnvsCopied, qt.Equals, uint64(1))
```

(Removes the `BindingsCopied` assertions.)

**Step 2: Fix `TestFlatClosure_ApplyFlatCopyPath`**

The `FlatCopyApplies` counter no longer exists. Replace the counter assertion (line 509) with the unified counter:

Replace:
```go
	qt.Assert(t, mc.counters.FlatCopyApplies, qt.Equals, uint64(1))
```
With:
```go
	// Unified copy path — no separate flat counter
```

Also update the test comment at line 471 to reflect the unified path.

**Step 3: Fix `TestFlatClosure_ApplyNoCopyPath`**

Remove the `FlatCopyApplies` assertion (line 549):

Replace:
```go
	// noCopy path should NOT increment FlatCopyApplies
	qt.Assert(t, mc.counters.FlatCopyApplies, qt.Equals, uint64(0))
```
With:
```go
	// noCopy path should not increment EnvsCopied
	qt.Assert(t, mc.counters.EnvsCopied, qt.Equals, uint64(0))
```

**Step 4: Run tests**

Run: `make test`
Expected: PASS

---

## Task 3: Remove Dead Counters

**Files:**
- Modify: `machine/counters.go:29-70` (struct fields)
- Modify: `machine/counters.go:121-175` (String method)
- Modify: `box_pressure_test.go:134,203` (FlatCopyApplies references)

**Step 1: Remove counter fields from VMCounters**

Remove these fields from the struct (lines 34, 51, 54):
- `BindingsCopied uint64`
- `KeysShared uint64`
- `FlatCopyApplies uint64`

**Step 2: Update String() method**

Remove corresponding format lines and arguments:
- `"bindings_copied:..."` (line 126 format, line 152 arg)
- `"keys_shared:..."` (line 138 format, line 164 arg)
- `"flat_copy_applies:..."` (line 141 format, line 167 arg)

**Step 3: Update box_pressure_test.go**

Line 134: Replace `c.FlatCopyApplies` with `c.EnvsCopied` in the ReportMetric call. Update the metric name:
```go
b.ReportMetric(float64(c.EnvsCopied), "copy_applies/op")
```

Line 203: Replace `c.FlatCopyApplies` with `c.EnvsCopied` in the log line:
```go
tc.name, c.OpsExecuted, c.ClosuresApplied, c.EnvsCopied, c.NoCopyApplies)
```

**Step 4: Run tests**

Run: `make test && make lint`
Expected: PASS, clean

---

## Task 4: Delete `IsFlat()` and Update `MachineClosure` Doc Comment

**Files:**
- Modify: `machine/machine_closure.go:46-49,88-92`

**Step 1: Delete `IsFlat()`**

Remove lines 88-92 (the `IsFlat` method). It has zero callers.

**Step 2: Update struct doc comment**

The doc comment at lines 26-45 references "linked closures" as the primary model. Update to reflect the unified design:

Replace lines 26-49 with:

```go
// MachineClosure is a callable pairing compiled code with its lexical
// environment at definition time.
//
//	closure = ⟨λ, E, FV⟩, where:
//	  λ  = template  — compiled bytecode (NativeTemplate)
//	  E  = env       — pointer to enclosing EnvironmentFrame (parameter shape)
//	  FV = freeVars  — captured free variables (nil when no captures)
//
// Two representations coexist:
//   - Zero-capture closures: freeVars is nil, env provides parameter
//     bindings only. Created by OpMakeClosure.
//   - Flat closures: freeVars holds captured values, env provides
//     parameter shape. Created by OpMakeFlatClosure.
//
// Both representations use the same Apply path. The env frame is always
// allocated fresh (via InitApplyFrame) for non-noCopyApply closures;
// binding values are NOT copied because bindArgs and body opcodes
// initialize all slots.
//
// See BIBLIOGRAPHY.md "Linked Closure Representation".
type MachineClosure struct {
	env      *environment.EnvironmentFrame
	freeVars []values.Value // nil for zero-capture closures; populated for flat closures
	template *NativeTemplate
}
```

**Step 3: Run tests**

Run: `make test && make lint`
Expected: PASS, clean

---

## Task 5: Rename `InitFlatApplyFrame` → `InitApplyFrame`

Now that the flat init path is the only init path, rename it.

**Files:**
- Modify: `environment/environment_frame.go:188-222` (rename methods, update comments)
- Modify: `environment/local_environment_frame.go:219-229` (rename `initFlatApplyInto`)
- Modify: `machine/machine_context_apply.go` (update call site)
- Modify: `environment/environment_frame_test.go:905-984` (rename test functions)
- Modify: `machine/flat_closure_test.go:471` (update comment)
- Modify: `environment/environment_frame.go:231` (update `ResetForPool` comment)

**Step 1: Delete old `InitApplyFrame` and `copyForApplyInto`**

Delete `InitApplyFrame` method (lines 188-202). It's now dead code — the only production call site was replaced in Task 1.

Delete `copyForApplyInto` method (`local_environment_frame.go:198-209`). Its only callers were `InitApplyFrame` and `NewApplyFrame`.

Delete `NewApplyFrame` method (`environment_frame.go:162-183`). Only used in tests — and those tests tested the old behavior we're removing.

**Step 2: Rename `InitFlatApplyFrame` → `InitApplyFrame`**

In `environment/environment_frame.go`:

Rename the method at line 209. Update the doc comment to remove "flat closure" specificity:

```go
// InitApplyFrame populates dst for a closure Apply. Binding values are NOT
// copied — bindArgs overwrites parameter slots and body code (OpStoreLocal)
// initializes the rest, so the memcpy is dead work. Parent and global
// pointers are set for OpPopEnv (let scopes) and the OpStoreGlobal
// fallback path.
func (p *EnvironmentFrame) InitApplyFrame(dst *EnvironmentFrame) {
```

**Step 3: Rename `initFlatApplyInto` → `initApplyInto`**

In `environment/local_environment_frame.go`:

Rename at line 219. Update the doc comment:

```go
// initApplyInto sets up dst with the correct number of binding slots
// without copying binding values. Used by EnvironmentFrame.InitApplyFrame:
// bindArgs overwrites parameter slots, and body code (OpStoreLocal)
// initializes the rest, so copying from source is pure waste.
//
// Keys are shared (CoW) for error messages and debugging. Pooled frames
// (the common case) are pre-zeroed by ResetForPool, so the resliced
// bindings already contain valid zero-value Binding structs.
func (p *LocalEnvironmentFrame) initApplyInto(dst *LocalEnvironmentFrame) {
```

Update the call in `InitApplyFrame` to call `initApplyInto`.

**Step 4: Update call site in machine**

In `machine/machine_context_apply.go`, replace `mcls.env.InitFlatApplyFrame(env)` with `mcls.env.InitApplyFrame(env)`.

**Step 5: Update `ResetForPool` comment**

In `environment/environment_frame.go:231`, the comment says "The next copyForApplyInto call will reslice". Update to "The next initApplyInto call will reslice".

**Step 6: Rename environment tests**

In `environment/environment_frame_test.go`:

- Delete `TestInitApplyFrame_PopulatesExistingFrame` (lines 834-861) — tests the deleted binding-copy behavior.
- Delete `TestInitApplyFrame_MatchesNewApplyFrame` (lines 863-888) — tests deleted `NewApplyFrame`.
- Delete `TestInitApplyFrame_PanicsOnNilParent` (lines 890-903) — redundant with the renamed test below.
- Rename `TestInitFlatApplyFrame_*` tests to `TestInitApplyFrame_*` (lines 909-984). These test the surviving behavior.

**Step 7: Update machine tests**

In `machine/flat_closure_test.go:471`, update the comment from "should use InitFlatApplyFrame, not InitApplyFrame" to "should use InitApplyFrame (no binding value copy)".

In `machine/fib_bench_test.go:76-88`:
- Update comment at line 76 to reference `InitApplyFrame` (already the right name after rename)
- Update the call at line 86: `closureEnv.InitApplyFrame(frame)` (already correct after rename)

**Step 8: Run tests**

Run: `make test && make lint && make covercheck`
Expected: all pass

---

## Task 6: Update `native_template.go` Comment

**Files:**
- Modify: `machine/native_template.go:41`

**Step 1: Update stale comment**

Line 41 references `NewApplyFrame()` which was deleted. Update to reference `InitApplyFrame()`:

Replace: `instead of allocating a fresh copy via NewApplyFrame().`
With: `instead of allocating a fresh frame via InitApplyFrame().`

**Step 2: Run lint**

Run: `make lint`

---

## Task 7: Housekeeping — Update TODO.md

**Files:**
- Modify: `TODO.md:101-103`

**Step 1: Mark flat closures done**

Change line 101 from `- [ ] **Flat closures**` to `- [x] **Flat closures**`.

**Step 2: Update stack frames entry**

Change line 102 from `- [ ] **Stack frames replacing continuation chains**` to `- [x] **Stack frames replacing continuation chains** [Performance, L, Closed]: Implemented and reverted (PR #518). Dispatch improved 5% on fib but regressed continuation-heavy benchmarks 10-20% (ctak +20%, takl +13%, nqueens +13%). Net negative — pool-based MachineContinuation linked list retained.`

---

## Task 8: Housekeeping — Update Plan Files

**Files:**
- Modify: `plans/STACK-FRAMES.md` (add closed banner)
- Modify: `plans/CLAUDE.md` (verify status is accurate)

**Step 1: Add closed banner to STACK-FRAMES.md**

Insert at line 1:

```markdown
> **Status: CLOSED.** Implemented and reverted (PR #518, 2026-03-17). Dispatch improved 5% on fib but regressed continuation-heavy benchmarks 10-20%. Net negative. Pool-based `MachineContinuation` linked list retained. This plan is preserved as a historical record of the attempt and the lessons learned.

---

```

**Step 2: Verify plans/CLAUDE.md**

The entry for STACK-FRAMES.md already says "**Closed**". Verify it's accurate and matches the new banner.

---

## Task 9: Run Gabriel Benchmarks

**Step 1: Run benchmarks**

Run: `make bench-gabriel`

Verify:
- All benchmarks produce identical results (correctness)
- Zero-capture benchmarks (fib, tak, sum, sieve) should be equal or faster (binding memcpy eliminated)
- No benchmark regresses more than noise threshold (3%)

**Step 2: Run full validation**

Run: `make lint && make covercheck`

---

## Dependency Order

```
Task 1 ──→ Task 2 ──→ Task 3 ──→ Task 4 ──→ Task 5 ──→ Task 6 ──→ Task 9
                                                            │
                                                 Task 7 ───┘
                                                 Task 8 ───┘
```

Tasks 7-8 (housekeeping) can run in parallel with Task 6. Task 9 is the final validation gate.
