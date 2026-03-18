# Flat Closures Implementation Plan

**Status:** Implemented and **reverted** (PR #520). All three PRs merged (#514, #515, #516, #519), then reverted after benchmarking showed a **+7.4% geo-mean regression across 31 benchmarks** (Larceny R7RS + Schelog + miniKanren) with zero benchmarks improved. The regression comes from new allocations: per-closure `freeVars` slice (+438 MB) and larger `MachineClosure` structs (+379 MB) on nqueens. The savings flat closures were supposed to deliver (eliminating parent-chain walks and per-call env copying) were negligible: parent-chain walks were 1-2 pointer chases, and per-call binding copies were already dead work (overwritten by `bindArgs`). Code reverted to linked closures; plan and benchmark evidence preserved below.
**Design:** `plans/PERFORMANCE.md` Tier 3, Item 8
**Branch strategy:** Three PRs (Infrastructure + Analysis, Behavioral Change, Cleanup) — all merged

**Goal:** Replace linked-closure environment capture with flat closures that copy only referenced free variables, using `*values.Box` for shared mutation. Eliminates parent-chain walks, per-call env copying, and EnvironmentFrame allocation for closures.

**Parent design:** This plan implements the design in `plans/PERFORMANCE.md` section "8. Flat Closures -- Multi-Pass Pipeline". That document is the source of truth for the *what*; this document is the source of truth for the *how* and *when*.

---

## PR A: Infrastructure + Analysis (no behavioral change)

All existing tests must continue to pass after every task. Pass 1 runs but its output is not consumed yet -- the VM still uses linked closures.

### A1. Add `freeVars` field to `vmState`

**Files:**
- `machine/vm_state.go` -- add `freeVars []values.Value` field, update save/restore table comment
- `machine/machine_context_continuation.go` -- save/restore `freeVars` in all four methods

**Details:**

Add to `vmState`:
```go
freeVars []values.Value // flat closure: captured free variables (nil for linked closures)
```

Save/restore behavior (new row in the table at `vm_state.go:79-94`):

| Field     | SaveCont saves | Restore | RestoreAndRelease | PopContinuation |
|-----------|----------------|---------|-------------------|-----------------|
| freeVars  | yes            | yes     | yes               | yes             |

Implementation in each method:

- `SaveContinuation` (`machine_context_continuation.go:194`): Already delegates to `NewMachineContinuationFromMachineContext` which copies vmState fields. Add `q.freeVars = mc.freeVars` at `machine_continuation.go:102-111`.
- `Restore` (`machine_context_continuation.go:31`): Add `p.freeVars = cont.freeVars` alongside `p.env = cont.env`.
- `RestoreAndRelease` (`machine_context_continuation.go:79`): Add `p.freeVars = cont.freeVars` in both the shared and unshared paths.
- `PopContinuation` (`machine_context_continuation.go:159`): Add `p.freeVars = q.freeVars` alongside `p.env = q.env`.

**Invariant:** `freeVars` is nil throughout this PR. No behavioral change.

**Tests:** Unit test that SaveContinuation/Restore round-trips a non-nil `freeVars` slice. Unit test that PopContinuation restores `freeVars`.

---

### A2. Add `freeVars` field to `MachineClosure`

**Files:**
- `machine/machine_closure.go` -- add field, add constructor, update `Copy()`

**Details:**

Extend `MachineClosure`:
```go
type MachineClosure struct {
    env      *environment.EnvironmentFrame // used by InitApplyFrame for frame shape (non-nil even for flat closures)
    freeVars []values.Value                // nil for linked closures
    template *NativeTemplate
}
```

Add constructor:
```go
func NewClosureWithFreeVars(tpl *NativeTemplate, freeVars []values.Value) *MachineClosure
```

Update `Copy()`: flat closures don't need env copy. `freeVars` is immutable (mutations go through `*values.Box`), so `Copy()` could share the slice. Current implementation conservatively clones via `slices.Clone` — can be relaxed to share once flat closures are validated end-to-end.

Add predicate:
```go
func (p *MachineClosure) IsFlat() bool {
    return p.freeVars != nil
}
```

**Invariant:** `IsFlat()` always returns false throughout this PR. `freeVars` is never set to non-nil.

**Tests:** Unit test for `NewClosureWithFreeVars`, `IsFlat()`, `Copy()` on flat closures.

---

### A3. Define new types: `CaptureEntry`, `FreeVarInfo`

**File:** `machine/free_var_info.go` (new)

**Details:**

```go
// CaptureEntry describes one free variable in a closure's capture list.
type CaptureEntry struct {
    SourceSlot  int  // binding slot in the source scope
    SourceDepth int  // de Bruijn depth from the closure (1 = immediate parent)
    ClosureSlot int  // index in the flat freeVars array
    Boxed       bool // true if this var needs *values.Box wrapping
    FromFreeVars bool // true: source is enclosing closure's freeVars[SourceSlot]
                      // false: source is mc.env local bindings at SourceDepth
}

// FreeVarInfo is the analysis result for one NativeTemplate.
type FreeVarInfo struct {
    Captures []CaptureEntry    // free variables to capture, ordered by ClosureSlot
    Mutated  map[[2]int]bool   // (slot, depth) pairs targeted by set! in this template
}
```

Add `freeVarInfo *FreeVarInfo` field to `NativeTemplate` (`native_template.go`). Add accessor `FreeVarInfo() *FreeVarInfo`.

**Tests:** Unit tests for `CaptureEntry` and `FreeVarInfo` construction. Verify `NativeTemplate.FreeVarInfo()` returns nil by default.

---

### A4. Add new opcodes

**Files:**
- `machine/opcode.go` -- 5 new constants + opcodeTable entries
- `machine/machine_context.go` -- 5 new cases in `Run()`
- `machine/native_template.go` -- cases in `operationToInstruction()` and `instructionToOperation()`
- `machine/operation_flat_closure.go` (new) -- operation types

**New opcodes:**

| Opcode | Wave | Arg | writesValue | isBranch | Semantics |
|--------|------|-----|-------------|----------|-----------|
| `OpLoadFreeVar` | 10 | closure slot | true | false | `value_reg = mc.freeVars[arg]` |
| `OpBox` | 10 | -- | true | false | `value_reg = values.NewBox(value_reg)` |
| `OpUnbox` | 10 | -- | true | false | `value_reg = value_reg.(*values.Box).Value` |
| `OpSetBox` | 10 | -- | false | false | `value_reg.(*values.Box).Value = evals.Pop()` |
| `OpMakeFlatClosure` | 10 | -- | true | false | Create closure from template + flat free-var array |

Per the opcode checklist (`opcode.go:22-29`):

1. `opcode.go` -- add `OpLoadFreeVar`, `OpBox`, `OpUnbox`, `OpSetBox`, `OpMakeFlatClosure` after `OpComplex`. Add entries to `opcodeTable` with metadata.
2. `machine_context.go Run()` -- add dispatch cases. `OpLoadFreeVar`, `OpBox`, `OpUnbox` are inlined (simple, hot). `OpSetBox` is inlined. `OpMakeFlatClosure` goes to side table via `OpComplex` (needs template + loop over captures).
3. `native_template.go` -- add cases in `operationToInstruction()` (line 271) and `instructionToOperation()` (line 120).
4. `operation_flat_closure.go` -- `OperationMakeFlatClosure` type implementing `InlinedOperation`.
5. No compiler emission yet (Pass 3 will emit these in PR B).
6. Tests below.
7. No peephole rules yet (PR C will add fusion).

**`OpMakeFlatClosure` implementation** (side table, `InlinedOperation`):

```go
func (p *OperationMakeFlatClosure) Apply(mc *MachineContext) (*MachineContext, error) {
    tpl := mc.evals.Pop().(*NativeTemplate)
    info := tpl.freeVarInfo
    freeVars := make([]values.Value, len(info.Captures))
    for i, c := range info.Captures {
        if c.FromFreeVars {
            freeVars[i] = mc.freeVars[c.SourceSlot]
        } else {
            bd := mc.env.GetLocalBindingBySlotDepth(c.SourceSlot, c.SourceDepth-1)
            freeVars[i] = bd.Value()
        }
    }
    cls := NewClosureWithFreeVars(tpl, freeVars)
    mc.SetValue(cls)
    mc.pc++
    return mc, nil
}
```

Note: `SourceDepth-1` because the capture's depth is relative to the closure being created (depth=1 = immediately enclosing scope), but `GetLocalBindingBySlotDepth` is called from within that scope (depth=0 = current env).

**Design decision: `OpMakeFlatClosure` dispatched via `OpComplex`?**

No. Promote it to an inlined opcode in `Run()` (like `OpMakeClosure`). The loop over captures is short (typically 1-5 entries), and closure creation is hot enough to avoid the side-table indirection. The `Apply` signature shown above becomes the inline case body.

However, `OpMakeFlatClosure` needs the `FreeVarInfo` from the child template. The child template is on the eval stack (pushed by `OpPushLiteral`). The inline case pops it, reads `tpl.freeVarInfo`, and loops. This is the same pattern as `OpMakeClosure` which pops both template and env from the stack.

**Tests:** Dispatch tests for each new opcode in `machine_context_test.go`. Test `OpLoadFreeVar` reads from `mc.freeVars`. Test `OpBox`/`OpUnbox`/`OpSetBox` round-trip. Test `OpMakeFlatClosure` builds correct `freeVars` array. Test error paths: `OpLoadFreeVar` with nil `freeVars`, `OpUnbox` on non-Box value, `OpSetBox` on non-Box value.

---

### A5. Implement Pass 1: FreeVarAnalysis

**File:** `machine/pass_free_var_analysis.go` (new)

**Algorithm:**

1. Scan `template.code[]` for `OpLoadLocal` and `OpStoreLocal` where `depth > 0` (decoded via `DecodeLocalIndex`). Each references a free variable.
2. Deduplicate by `(slot, depth)`. Assign `ClosureSlot` indices (0, 1, 2, ...).
3. If opcode is `OpStoreLocal`, mark `(slot, depth)` in `Mutated` set.
4. Propagate from inner templates: for each sub-template in `tpl.literals`, take its `FreeVarInfo`. For each capture with `SourceDepth > 1`, add `(SourceSlot, SourceDepth-1)` to current template's free var set.

**Execution order:** Bottom-up. Analyze innermost templates first (leaves of the literal tree), then work outward. Recursive function that processes children before self.

**Also scan `OpPushLocal`:** The peephole optimizer fuses `OpLoadLocal + OpPush` into `OpPushLocal`. Since Pass 1 runs AFTER peephole in the current pipeline, it must also check `OpPushLocal` with `depth > 0`. However, the design says passes 1-3 run BEFORE peephole. This is a key ordering decision -- see A6.

**Integration point:** Call `analyzeFreeVars(tpl)` in `compileClosureBody` -- but do NOT consume the result yet. The analysis populates `tpl.freeVarInfo` as metadata only.

**Tests:** Table-driven tests in `machine/pass_free_var_analysis_test.go`:
- Lambda with no free vars -> empty `Captures`
- Lambda with one free var (depth=1) -> one `CaptureEntry`
- Lambda with free var at depth=2 -> `CaptureEntry` with `SourceDepth=2`
- `set!` on captured var -> appears in `Mutated`
- `set!` on non-captured local -> in `Mutated` but not in `Captures`
- Nested closures: inner closure references grandparent var -> propagation creates entry in parent's `FreeVarInfo`
- Multiple closures sharing a free var -> deduplicated to one `CaptureEntry`
- Closure with no body references (all globals) -> empty `Captures`

---

### A6. Pipeline insertion point

**File:** `machine/compile_closure.go` -- modify `compileClosureBody` Phase 5

**Current Phase 5:**
```go
tpl.Optimize()              // peephole
tpl.computeNoCopyApply()    // escape analysis
```

**New Phase 5:**
```go
analyzeFreeVars(tpl)        // Pass 1: metadata only (PR A)
// Pass 2 + 3 inserted here in PR B
tpl.Optimize()              // peephole (existing)
tpl.computeNoCopyApply()    // escape analysis (existing)
```

**Critical ordering:** Passes 1-3 run BEFORE peephole. This means Pass 1 only sees canonical opcodes (`OpLoadLocal`, `OpStoreLocal`), never fused variants (`OpPushLocal`). This simplifies the analysis -- only two opcodes to check, not three.

The design document explicitly states this: "Passes 1-3 run **before** peephole optimization, so they only see canonical opcodes."

**Tests:** Compile a lambda with known free vars via the full pipeline. Verify `tpl.FreeVarInfo()` is populated. Verify existing tests still pass (analysis is metadata-only).

---

### A7. PR A acceptance criteria

- [ ] All existing tests pass (`make test`)
- [ ] `make lint` clean
- [ ] `make covercheck` passes
- [ ] New unit tests for: vmState freeVars save/restore, MachineClosure flat constructors, CaptureEntry/FreeVarInfo types, all 5 new opcodes dispatch, Pass 1 analysis (table-driven)
- [ ] Gabriel benchmarks: no regression (analysis pass is metadata-only, cost should be negligible)
- [ ] `freeVars` is nil everywhere at runtime -- no behavioral change

---

## PR B: Behavioral Change (Box Insertion + Closure Flattening)

This PR activates flat closures. After merging, closures that reference free variables use the flat representation. Linked closures remain as fallback for closures with no free variables (they're already optimal -- nothing to capture).

### B1. Implement Pass 2: BoxInsertion

**File:** `machine/pass_box_insertion.go` (new)

**Boxing criterion:** A variable needs boxing when it is both:
- Captured by any closure (appears in any nested template's `FreeVarInfo.Captures`)
- Mutated by any site (appears in `FreeVarInfo.Mutated` of the defining template OR any nested template)

**Two-scan approach:**

Scan 1 -- collect box requests: Walk all nested templates in `tpl.literals`. For each nested template's `FreeVarInfo`, collect `(slot, depth)` pairs that are both captured and mutated. Translate depths relative to current template. These identify bindings in the current template that must be boxed.

Scan 2 -- rewrite bytecodes in the defining scope using `EditPlan`:

- **Lambda parameters** that need boxing: insert at top of template body (after bindArgs):
  ```
  OpLoadLocal(slot, 0)  ; load parameter value
  OpBox                 ; wrap in *values.Box
  OpPush                ; push boxed value
  OpStoreLocal(slot, 0) ; replace parameter with boxed version
  ```

- **let/define initial assignments** that need boxing: insert `OpBox` before the `OpPush` that precedes the initial `OpStoreLocal`.

- **Reads** of boxed variables (any scope, including defining scope): insert `OpUnbox` after each `OpLoadLocal(slot, depth)` referencing a boxed variable.

- **Writes** (`set!`) of boxed variables: replace `OpStoreLocal(slot, depth)` with load-and-set-box:
  ```
  OpLoadLocal(slot, depth) ; load the *values.Box into value_reg
  OpSetBox                 ; box.Value = evals.Pop()
  ```

**Branch offset adjustment:** Use `EditPlan` (`machine/edit_plan.go`) which handles offset fixup for `isBranch` opcodes automatically.

**Tests:** Table-driven tests in `machine/pass_box_insertion_test.go`:
- Parameter boxing: verify `OpBox` + `OpPush` + `OpStoreLocal` inserted at top
- Let-binding boxing: verify `OpBox` inserted before initial store
- Read-through-box: verify `OpUnbox` after `OpLoadLocal` for boxed var
- Write-through-box: verify `OpStoreLocal` replaced by `OpLoadLocal` + `OpSetBox`
- Non-boxed variable: no rewriting (captured but not mutated, or mutated but not captured)
- Multiple boxed variables: all correctly rewritten
- Branch targets: verify EditPlan adjusts branch offsets across insertions

---

### B2. Implement Pass 3: ClosureFlatten

**File:** `machine/pass_closure_flatten.go` (new)

**Bytecode rewrites** in the capturing template (the closure body):

- `OpLoadLocal(slot, depth>0)` -> `OpLoadFreeVar(closureSlot)` where `closureSlot` comes from `FreeVarInfo.Captures`.
- `OpStoreLocal(slot, depth>0)` -- should not exist after Pass 2 (all stores to free boxed vars became `OpLoadLocal + OpSetBox`; non-boxed free vars are never stored to). Emit error if encountered.
- `OpMakeClosure` -> `OpMakeFlatClosure` when the child template has a non-empty `FreeVarInfo`.

**Resolving `FromFreeVars`:** After Pass 1 assigns `ClosureSlot` indices, Pass 3 resolves the `FromFreeVars` flag:
- `SourceDepth == 1`: value comes from enclosing scope's local bindings. `FromFreeVars = false`.
- `SourceDepth > 1`: value is itself a free variable of the enclosing scope. `FromFreeVars = true`, `SourceSlot` rewritten to enclosing scope's `ClosureSlot` for that variable.

**Tests:** Table-driven tests in `machine/pass_closure_flatten_test.go`:
- `OpLoadLocal(slot, depth>0)` replaced by `OpLoadFreeVar(closureSlot)`
- `FromFreeVars` true vs false resolution
- Nested capture: inner closure captures from grandparent through parent's freeVars
- `OpMakeClosure` -> `OpMakeFlatClosure` when child has captures
- `OpMakeClosure` unchanged when child has no captures
- Error on unexpected `OpStoreLocal(slot, depth>0)` after box insertion

---

### B3. Update Apply path for flat closures

**File:** `machine/machine_context_apply.go` -- modify `Apply()`

**New branching logic:**

```go
func (p *MachineContext) Apply(mcls *MachineClosure, vs ...values.Value) (*MachineContext, error) {
    tpl := mcls.Template()
    // ... arity check (unchanged) ...

    if mcls.IsFlat() {
        // Flat closure: no env copy needed. Allocate minimal frame
        // for parameters only. Set mc.freeVars for OpLoadFreeVar.
        env := acquireEnvFrame()
        mcls.template.initParameterOnlyFrame(env, mcls.env) // see B4
        bnds := env.LocalEnvironment().Bindings()
        p.envPooled = true
        p.freeVars = mcls.freeVars
        bindArgs(bnds, vs, l, tpl.IsVariadic(), nil)
        p.template = tpl
        p.env = env
        p.pc = 0
        p.counters.FlatClosureApplies++
        return p, nil
    }

    // ... existing linked closure path (unchanged) ...
}
```

**Key invariant:** Flat closures always allocate a fresh parameter frame (never noCopyApply reuse of closure's env, because the closure has no env). `p.freeVars` points to the closure's immutable `freeVars` slice. Boxed vars mutate through `*values.Box`, not the array slot.

**Counter:** Add `FlatClosureApplies` to `VMCounters`.

---

### B4. Parameter-only environment frame

**Question for implementation:** Flat closures need an `EnvironmentFrame` for parameters only (no parent chain for free var access). Two options:

**(a) Reuse existing `acquireEnvFrame` + `InitApplyFrame` with a dummy parent.** The closure stores a reference env at construction time (compile-time env) that provides the `LocalEnvironmentFrame` shape (parameter count, names). `InitApplyFrame` copies the shape but the parent chain is irrelevant since free vars use `OpLoadFreeVar`.

**(b) New lightweight constructor** that creates a frame with just the parameter bindings and no parent chain.

**Recommendation:** Option (a). The compile-time env is already stored as a literal in the parent template (Phase 3 of `compileClosureBody`). `InitApplyFrame` copies the `LocalEnvironmentFrame` shape, which is exactly what we need. The parent pointer exists but is never walked (no `OpLoadLocal` with `depth>0` after flattening). This avoids introducing a new constructor.

For flat closures, `MachineClosure.env` stores the compile-time env (same as linked closures). The difference is that `OpLoadLocal(slot, depth>0)` no longer exists in the bytecode -- all free var access goes through `OpLoadFreeVar`. The parent chain is dead code at runtime.

**Update:** On reflection, `MachineClosure.env` must remain non-nil for flat closures so that `InitApplyFrame` works. The design document says `env` is nil for flat closures -- this is wrong for the Apply path. Instead: `env` stays populated (used only by `InitApplyFrame` to get the frame shape), `freeVars` is non-nil, and `IsFlat()` checks `freeVars != nil`.

---

### B5. Update `computeNoCopyApply` for flat closures

**File:** `machine/native_template.go`

**Current logic:** Scans for `OpSaveContinuation` or `OpMakeClosure`. If neither found, `noCopyApply = true`.

**New logic:** After flattening, `OpMakeClosure` may be replaced by `OpMakeFlatClosure`. The existing check for `OpMakeClosure` still works because:
- If the template has been flattened, `OpMakeClosure` is gone (replaced by `OpMakeFlatClosure`)
- `OpMakeFlatClosure` does NOT capture `mc.env` -- it reads from `mc.freeVars` and local bindings
- Therefore `OpMakeFlatClosure` does not cause the "env captured" problem that requires copying

Add `OpMakeFlatClosure` to the list of opcodes that do NOT force `noCopyApply = false`. In practice, this means no code change -- `computeNoCopyApply` already only checks for `OpSaveContinuation` and `OpMakeClosure`, and `OpMakeFlatClosure` is a different opcode.

However, the flat closure Apply path bypasses `noCopyApply` entirely (always allocates a fresh frame). So this is informational -- `noCopyApply` only matters for linked closures.

**No code change required.** Document this interaction in a comment.

---

### B6. Pipeline integration -- activate passes 2 and 3

**File:** `machine/compile_closure.go` -- modify `compileClosureBody` Phase 5

```go
// Phase 5: Free variable analysis and closure optimization.
info := analyzeFreeVars(tpl)         // Pass 1: metadata
if info != nil {
    insertBoxes(tpl, info)           // Pass 2: box insertion
    flattenClosures(tpl, info)       // Pass 3: rewrite to flat closures
}
tpl.Optimize()                       // peephole (existing)
tpl.computeNoCopyApply()             // escape analysis (existing)
```

Pass 1 returns nil when there are no free variables (nothing to do). Passes 2 and 3 are skipped for templates with no free vars -- these remain as linked closures, which is already optimal.

---

### B7. Integration tests

**File:** `machine/flat_closure_integration_test.go` (new)

Scheme programs compiled and executed through the full pipeline, verifying identical behavior to the linked-closure model:

| Test case | What it exercises |
|-----------|------------------|
| Simple capture: `(let ((x 1)) (lambda () x))` | Basic flat closure, one free var |
| `set!` on captured var: `(let ((x 0)) (let ((inc (lambda () (set! x (+ x 1))))) (inc) (inc) x))` | Boxing, mutation through box |
| Two closures sharing boxed var | Shared box identity |
| Nested closures: `(lambda (x) (lambda (y) (lambda () (+ x y))))` | Transitive capture, `FromFreeVars` |
| Closure captures nothing | No flattening, linked closure path |
| Mixed boxed/non-boxed captures | Selective boxing |
| `call/cc` across flat closure | Continuation saves/restores `freeVars` |
| Recursive closure with `set!` on captured var | Box + SaveContinuation interaction |
| Variadic closure with captures | Rest-arg + flat closure |
| `case-lambda` with captures | Multiple clauses, shared `freeVars` |
| `dynamic-wind` + flat closure | Winding stack orthogonal to `freeVars` |
| Tail call between flat closures | `freeVars` updated on tail apply |

---

### B8. Gabriel benchmark regression

Run full Gabriel benchmark suite. Compare against baseline (linked closures). Expected:
- Correctness: identical results
- Performance: improvement on closure-heavy benchmarks (fib, tak, cpstak, deriv)
- No regression on benchmarks that don't use closures heavily

Record results in `plans/PERFORMANCE.md` under a new "Flat Closures Results" section.

---

### B9. PR B acceptance criteria

- [ ] All existing tests pass (`make test`)
- [ ] `make lint` clean
- [ ] `make covercheck` passes
- [ ] New tests: Pass 2 (box insertion), Pass 3 (closure flatten), Apply path (flat vs linked), integration tests (all cases in B7)
- [ ] Gabriel benchmarks: no correctness regression, performance improvement recorded
- [ ] `freeVars` is non-nil at runtime for closures with free variables
- [ ] Linked closures still work for closures with no free variables

---

## PR C: Cleanup + Optimization

After PR B is validated, remove the linked closure path and add peephole optimizations.

### C1. Remove linked closure path

**Precondition:** PR B merged, full test suite green, Gabriel benchmarks validated.

Once all closures go through the flat path:
- Remove `MachineClosure.env` field (or repurpose for parameter frame shape only)
- Remove `noCopyApply` path in `Apply()` for `MachineClosure` (flat closures always allocate fresh)
- Remove `InitApplyFrame` usage from flat closure Apply
- Simplify `MachineClosure.Copy()` (no env copy needed)

**Risk:** This is a breaking change for any code that accesses `MachineClosure.env` directly. Grep for all access sites first.

---

### C2. Peephole fusion for flat closure opcodes

**File:** `machine/peephole.go`

New fusion rules:

| Pattern | Fused opcode | Benefit |
|---------|-------------|---------|
| `OpLoadFreeVar + OpPush` | `OpPushFreeVar` | Eliminates 1 dispatch |
| `OpLoadFreeVar + OpUnbox` | `OpLoadFreeVarUnboxed` | Eliminates 1 dispatch + type assert |
| `OpLoadFreeVar + OpUnbox + OpPush` | `OpPushFreeVarUnboxed` | Eliminates 2 dispatches |

Each fused opcode needs: constant in `opcode.go`, case in `Run()`, entries in `operationToInstruction`/`instructionToOperation`, fusion rule in `peephole.go`, tests.

**Priority:** `OpPushFreeVar` is highest value (most common pattern). Others are incremental.

---

### C3. Collapse Pass 2 into single bottom-up pass

**Optimization:** The two-scan approach in Pass 2 (scan all nested templates, then rewrite) can be collapsed into a single bottom-up pass by processing inner templates before outer templates and propagating box requests upward during analysis. Same result, fewer scans.

This is a performance optimization of the compiler itself, not of generated code. Lower priority.

---

### C4. PR C acceptance criteria

- [ ] All existing tests pass
- [ ] `make lint` clean
- [ ] `make covercheck` passes
- [ ] Linked closure path removed (or gated behind build tag if cautious)
- [ ] Peephole fusion tests for new fused opcodes
- [ ] Gabriel benchmarks: further improvement from fusion, no regression

---

## Cross-Cutting Concerns

### Interaction with existing optimizations

| Optimization | Interaction | Action needed |
|-------------|-------------|---------------|
| `noCopyApply` | Flat closures bypass this entirely | None (orthogonal) |
| Env frame pooling | Flat closures still pool parameter frames | None |
| `OpCallForeignCached` | Only applies to foreign closures | None |
| `OpCallLocal` / `OpCallCachedBinding` | Fused call ops dispatch through `drainAndApply` which calls `Apply()` | Apply branching (B3) handles it |
| Continuation marks | `marks` field on vmState, orthogonal to `freeVars` | None |
| SRFI-18 threads | `threadID` on vmState, orthogonal | None |
| Delimited continuations | Save/restore of vmState handles `freeVars` | Covered by A1 |
| Opcode promotion (Wave 9) | Promoted primitives don't touch env/freeVars | None |

### `case-lambda` closures

`CaseLambdaClosure` wraps multiple `MachineClosure` values, one per arity clause. Each clause is independently flattened. The `CaseLambdaClosure` dispatches to the matching clause, which then goes through the normal Apply path.

No special handling needed -- each clause's `MachineClosure` carries its own `freeVars`.

### Sub-contexts

`NewSubContext` creates fresh MachineContexts for foreign function calls. These inherit `freeVars = nil` by default (foreign functions don't use flat closure machinery). The Apply that runs inside a sub-context will set `freeVars` if it applies a flat closure.

No special handling needed.

### Debugger

The debugger (`machine/debugger.go`) inspects `mc.env` for variable display. Flat closures still have `mc.env` (for parameters) and `mc.freeVars` (for captured vars). The debugger should display both.

**Deferred:** Debugger integration is a follow-up, not a blocker. Free variables in flat closures are unnamed values (indexed by slot). Adding debug names requires storing the variable names in `FreeVarInfo`, which is straightforward but not essential for correctness.

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Box insertion breaks semantics for shared mutation | Medium | High | Integration test: two closures sharing boxed var |
| `FromFreeVars` resolution wrong for deeply nested closures | Medium | High | Test: 3+ nesting levels with transitive capture |
| Continuation save/restore misses `freeVars` | Low | High | Unit tests for all 4 save/restore methods |
| Performance regression from boxing overhead | Low | Medium | Benchmark before/after; boxing only when necessary |
| `EditPlan` branch fixup wrong after box insertion | Low | High | Test: box insertion across branch boundaries |
| Tail calls don't update `freeVars` | Low | High | Test: tail call between two different flat closures |

---

## Open Questions

1. **Should Pass 1 run for top-level templates?** ~~Verify this assumption.~~ **Verified (2026-03-17).** The assumption is correct and already implemented. `RunFlatClosurePipeline` is called only from `compileClosureBody` (`compile_closure.go:114`), which is invoked only for `lambda` (`compile_closure.go:129`) and `case-lambda` (`compile_validated.go:416`) bodies. Top-level templates never enter the pipeline. Top-level `let` bindings produce `depth > 0` opcodes (nested `LocalEnvironmentFrame` scopes within a single template), but these are not cross-lambda free variables — no action needed.

2. **Pool `freeVars` arrays?** ~~Measure first -- premature optimization otherwise.~~ **Measured (2026-03-17).** Line-level `memprofile` on `machine_context.go:793` (`fv := make([]values.Value, len(info.Captures))`).

   **Original finding (micro-benchmarks only): Not worth pooling.** The freeVars array allocation is proportional to closure *creation* count, not *call* count. Since closures are typically created once and called many times, freeVars allocation is noise in the typical case.

   - **Pathological case** (HotBoxCreation: 1000 new closures per benchmark iteration):
     - `fv := make(...)`: 11.1% of allocs by count, 6.9% by bytes
     - Smaller than env frame allocation (16.6% by bytes) and closure struct (10.5% by bytes) at the same site
   - **Typical case** (Counter: closure created once, called 1000×):
     - `fv := make(...)`: 0.3% of allocs by count, 0.09% by bytes
     - Dominated entirely by `drainAndApply` (98%+)
   - Observed capture counts: 1 element in 4/5 benchmarks, 3 elements in 1/5. Matches prediction that small arrays dominate.

   **Corrected finding (end-to-end Gabriel profiling, 2026-03-17).** The micro-benchmark analysis was correct for its test cases but drew a general conclusion from non-representative workloads. End-to-end profiling of the nqueens Gabriel benchmark revealed `fv := make(...)` as the **#1 new allocation site**: 11.1M objects (16.7% of total), responsible for +24% total allocation count and +29% total bytes vs pre-flat-closures. The Gabriel benchmarks ARE the "closures created in a tight loop" pattern the original analysis dismissed.

   **Inline freeVars optimization attempted and reverted.** Added `inlineFreeVars [3]values.Value` to `MachineClosure` (same pattern as `inlineEvals` on `MachineContinuation`). Results:
   - Eliminated 100% of freeVars slice allocations (11M objects gone)
   - Total alloc count: 66.4M → 55.6M (-16%), recovering 83% of the allocation regression
   - But total alloc **bytes** increased 5% (3.38GB → 3.55GB) — larger closure structs offset the savings
   - Runtime: -0.1% geo-mean (within noise). Only nqueens improved (-6.2%)
   - **Reverted.** The optimization consolidated two small allocations into one larger one. GC pressure depends on bytes scanned, not just object count.

   **Key lesson:** Reducing allocation count without reducing total bytes does not help. The successful prior optimizations (split value register, inline continuation evals) eliminated allocation entirely — replacing heap operations with register/stack assignments. Merely merging allocations is fundamentally weaker.

   **Remaining options (not yet attempted):**
   - Pooling `MachineClosure` structs themselves (requires identifying a release point — closures have GC-determined lifetimes, no explicit "done" signal)
   - Escape analysis at compile time to skip `freeVars` allocation for closures that don't escape their creating scope

3. **`values.Box` allocation pressure:** ~~Measure allocation profile after PR B.~~ **Measured (2026-03-17).** See `box_pressure_test.go` for benchmarks and `TestBoxPressureProfile` for per-opcode histograms.

   **Finding: Box allocation pressure is negligible.** No action needed.

   - Gabriel benchmarks contain zero `set!` on captured variables. Box pressure is 0% of allocations for purely functional code.
   - In the pathological case (HotBoxCreation: new closure with fresh box every iteration, 1000×):
     - `values.NewBox`: **5.45%** of allocation count, **1.61%** of allocation bytes (23MB / 1.4GB total)
     - Dominant allocators: env frame pool factory (32%), `NewEnvironmentFrameWithParent` (24%), `initFlatApplyInto` (10.7%), `NewClosureWithFreeVars` (10.2%)
   - In the typical case (Counter: box created once, closure called 1000×):
     - `values.NewBox` doesn't appear in the allocation profile (below noise floor)
     - Dominated entirely by env frame pool (48.8%) and `initFlatApplyInto` (48.6%)
   - Per opcode histogram: `OpBox` fires 2× in Counter/SharedMutation/NestedCapture (once per boxed variable definition), 1001× in HotBoxCreation (once per loop iteration creating a new closure). `OpUnbox` and `OpSetBox` are proportional to closure invocations, not Box allocations.

   **Conclusion:** Pooling `*values.Box` is not warranted. The 16-byte allocation (one pointer) is amortized over many `OpUnbox`/`OpSetBox` operations. The env frame pool factory and binding array copies are 20-60× more impactful.

---

## Regression Analysis and Revert (2026-03-17)

All measurements: Apple M4 Max, Go 1.24, back-to-back on same machine with freshly built binaries.
Before = `ec26f1c8` (pre-flat-closures). After = `20160b4b` (flat closures + post-simplification PR #519).
Both binaries built and benchmarked in the same session — no historical data trusted.

### Gabriel Benchmark Regression (16 benchmarks, 6 runs each)

| Benchmark | Before(s) | After(s) | Delta |
|-----------|-----------|----------|-------|
| tak | 0.1122 | 0.1135 | +1.2% |
| takl | 1.0871 | 1.1891 | +9.4% |
| ctak | 1.6423 | 1.7073 | +4.0% |
| cpstak | 0.1813 | 0.1958 | +8.0% |
| fib | 0.3728 | 0.3670 | -1.6% |
| triangl | 0.0387 | 0.0381 | -1.6% |
| sum | 0.0312 | 0.0306 | -1.9% |
| sumfp | 0.6246 | 0.6673 | +6.8% |
| diviter | 2.5827 | 2.6304 | +1.8% |
| divrec | 0.8723 | 0.8618 | -1.2% |
| deriv | 0.1026 | 0.1068 | +4.1% |
| ackermann | 0.4849 | 0.4737 | -2.3% |
| sieve | 0.0811 | 0.0810 | -0.1% |
| nqueens | 1.9100 | 2.1534 | **+12.7%** |
| primes | 0.2378 | 0.2556 | +7.5% |
| peval | 0.0684 | 0.0703 | +2.8% |
| **GEO MEAN** | | | **+3.0%** |

Faster (>2%): 1 | Noise (±2%): 7 | Slower (>2%): 8

### Extended Benchmark Regression (31 benchmarks, Larceny + Schelog + Kanren, 3 runs each)

Filtered to benchmarks ≥10ms avg. Sorted by regression magnitude.

| Benchmark | Before(s) | After(s) | Delta | Category |
|-----------|-----------|----------|-------|----------|
| nqueens | 0.2679 | 0.2658 | -0.8% | |
| ack | 1.6007 | 1.6050 | +0.3% | |
| fib | 0.4215 | 0.4248 | +0.8% | |
| paraffins | 3.3127 | 3.3410 | +0.9% | |
| kanren-zebra | 16.3818 | 16.7265 | +2.1% | Continuation-heavy |
| equal | 2.6508 | 2.7099 | +2.2% | |
| sboyer | 4.0985 | 4.2999 | +4.9% | GC-heavy |
| schelog-zebra | 6.6516 | 7.0232 | +5.6% | Continuation-heavy |
| nboyer | 3.5512 | 3.7774 | +6.4% | GC-heavy |
| destruc | 0.0367 | 0.0395 | +7.6% | |
| ctak | 0.1642 | 0.1780 | +8.4% | Continuation-heavy |
| triangl | 3.1873 | 3.4558 | +8.4% | |
| fft | 0.6538 | 0.7119 | +8.9% | |
| takl | 0.1385 | 0.1512 | +9.2% | |
| mbrot | 0.1643 | 0.1829 | +11.3% | |
| peval | 0.1171 | 0.1304 | +11.4% | |
| fibc | 0.0250 | 0.0281 | +12.4% | Continuation-heavy |
| conform | 0.3458 | 0.3887 | +12.4% | |
| browse | 0.0648 | 0.0730 | +12.7% | |
| puzzle | 0.2652 | 0.2996 | +13.0% | |
| gcbench | 0.4552 | 0.5168 | +13.5% | GC-heavy |
| cpstak | 0.0190 | 0.0216 | +13.7% | Continuation-heavy |
| quicksort | 0.1054 | 0.1257 | +19.3% | |
| **GEO MEAN (31)** | | | **+7.4%** | |

Faster (>2%): 0 | Noise (±2%): 5 | Slower (>2%): 26

### Allocation Profile (nqueens, `--memprofile`, alloc_objects)

| Allocator | Before (objects) | After (objects) | Before (MB) | After (MB) | Delta (MB) |
|-----------|-----------------|-----------------|-------------|------------|------------|
| `NewClosureWithFreeVars` | — | 12.28M | — | 562 | +562 (NEW) |
| `Run()` inline (freeVars slices) | — | 11.15M | — | 438 | +438 (NEW) |
| `NewClosureWithTemplate` | 11.96M | — | 183 | — | -183 (gone) |
| `initApplyInto` | 12.85M | 12.26M | 541 | 523 | -18 |
| `init.func8` (env frame pool) | 12.02M | 12.07M | 917 | 921 | +4 |
| `NewEnvironmentFrameWithParent` | 11.96M | 11.74M | 887 | 896 | +9 |
| `NumericChainCompare` | 5.01M | 5.51M | 77 | 84 | +7 |
| `NewBox` | — | 0.59M | — | ~5 | +5 (NEW) |
| **TOTAL** | **53.7M** | **65.9M (+22.7%)** | **2,621** | **3,452** | **+831 (+31.7%)** |

### Why the Predicted Savings Didn't Materialize

The flat closures hypothesis had three predicted savings:

1. **Eliminate parent-chain walks** — Free var access O(1) instead of O(depth). But parent-chain walks were already 1-2 pointer chases. Neither profile shows measurable time in binding lookup.

2. **Eliminate per-call env copying** — `copyForApplyInto` went 541→523 MB (-3%). Closures still need env frames for parameter bindings. Binding values were already dead work, overwritten by `bindArgs`.

3. **Reduce EnvironmentFrame allocation** — `NewEnvironmentFrameWithParent` went 887→896 MB (+1%). Nothing was eliminated; closures still allocate env frames for parameters.

Meanwhile, two new costs dwarfed the savings:
- **`freeVars` slices**: `make([]values.Value, N)` per closure creation = +438 MB
- **Larger `MachineClosure` struct**: slice header added 24 bytes per closure = +379 MB

### Post-Revert Verification

After reverting (PR #520, commit `d114ee04`), Gabriel benchmark geo-mean returned to +0.4% vs pre-flat-closures baseline — within noise. Regression fully recovered.

### Baseline Correction

The commit `2f6eb2f1` claimed a 3.37× geo-mean speedup from `1c1db76` to `ec26f1c8`. **This was wrong.** Back-to-back benchmarking on the same machine (2026-03-17) measured **1.34× geo-mean** (range: 1.07×–2.92×). The saved baseline at `1c1db76` was likely run on a different or throttled machine. Saved baselines should not be trusted without verifying run conditions.

| Workload type | Verified speedup (`1c1db76` → `ec26f1c8`) |
|---|---|
| Arithmetic-heavy (sumfp) | 2.92× |
| General Gabriel suite | 1.34× geo-mean |
| Continuation-heavy (kanren, schelog, ctak/cpstak) | 1.10× geo-mean |

---

## Machine Modernization Roadmap

Flat closures (PRs #514-#516) were attempted as the first milestone in bringing Wile's machine up-to-date with Racket and Chibi, but were reverted due to regression. This section documents the remaining gaps, organized by value and dependency order.

### Constraint: No `unsafe` Packages

Wile is pure Go — no `unsafe`, no `reflect` on internal layouts, no CGo, no direct manipulation of Go runtime structures. Tasks that require `unsafe` operations are documented below for completeness but **will NOT be completed for Wile**. This is a hard constraint, not a deferral.

### What's Done (Matching Racket/Chibi)

| Feature | Evidence | Racket/Chibi Equivalent |
|---------|----------|------------------------|
| ~~Flat closures~~ | ~~Reverted (PR #520) — +7.4% regression~~ | ~~Chez flat closures (Dybvig 1987)~~ |
| ~~Box for mutated captures~~ | ~~Reverted with flat closures~~ | ~~Racket/Chibi shared mutation cells~~ |
| Continuation marks | `vm_state.go:199-213`, Phases 1-3 complete | Racket `with-continuation-mark` |
| Delimited continuations | `PromptTag`, `ComposableContinuation`, `ErrPromptAbort` | Racket prompts (Flatt et al. ICFP 2007) |
| Hygienic macros (Flatt 2016) | Sets-of-scopes model | Racket's scope-set hygiene |
| syntax-case | R6RS-style | Chez/Racket syntax-case |
| ER macro transformers | Per TODO.md | Chibi ER macros |
| Promoted primitive opcodes | 11 primitives, 22 opcodes (`OPCODE-PROMOTION.md`) | Chez/Chibi inline primitives |

### Remaining Tasks

#### T1. Stack Frames Replacing Continuation Chains

**Requires `unsafe`:** No. Uses Go slices (`[]callFrame`) and index arithmetic.

**Priority:** Highest-value single optimization remaining.

**Status:** Attempted and reverted (2026-03-17). Profiling predicted 5.9-12.5% on fib. Implementation achieved -5.1% on fib but regressed continuation-heavy benchmarks 10-20% (ctak +20%, takl +13%, nqueens +13%). Net negative on Gabriel suite. Reverted. See T1.1.5 below.

---

##### T1.1 Profiling Results (2026-03-17)

All measurements on Apple M4 Max, Go 1.24, 6-run median.

###### T1.1.1 Allocation Profile (Fibonacci benchmark, `-memprofile`)

`BenchmarkRun/Fibonacci` (42,240 iterations of fib(10)):

**By allocation count** (10M total objects):

| Source | Objects | % of total |
|--------|---------|-----------|
| `copyForApplyInto` (env frame binding copy) | 4,718,735 | 47.00% |
| `init.func8` (continuation pool factory) | 4,522,329 | **45.05%** |
| `Run()` misc | 196,609 | 1.96% |
| All other | 601,158 | 5.99% |

**By allocation bytes** (524MB total):

| Source | MB | % of total |
|--------|-----|-----------|
| `init.func8` (continuation pool factory) | 345 | **65.78%** |
| `copyForApplyInto` (env frame binding copy) | 144 | 27.45% |
| `buildRestArg` | 7 | 1.33% |
| All other | 28.5 | 5.44% |

**Key finding:** The continuation pool factory (`init.func8`) is the **#1 allocation source by bytes** (65.78%) and **#2 by count** (45.05%). These are pool-miss allocations — every time Go's GC clears `sync.Pool`, the factory must re-allocate `MachineContinuation` structs. With a `[]callFrame` contiguous stack, these allocations **disappear entirely** — the backing array is allocated once and reused via `append`/reslice.

###### T1.1.2 Micro-Benchmarks (pool round-trip vs callStack append/pop)

| Benchmark | ns/op | allocs/op | What it measures |
|-----------|-------|-----------|-----------------|
| `ContinuationRoundTrip` | **18.4** | 0 | Full SaveContinuation + RestoreAndRelease (current) |
| `ContinuationPoolFull` | 11.0 | 0 | Pool acquire + field populate + release |
| `ContinuationPool` (bare) | 10.4 | 0 | Pool acquire + release (no field copy) |
| `CallStackAppendPop` | **9.3** | 0 | `append(callFrame{...})` + zero + reslice (proposed) |
| `CallStackAppendPopDeep` | 9.0 | 0 | Same at depth 20 (cache-warm backing array) |

**Per round-trip savings:** 18.4 - 9.3 = **9.1ns** (2.0x faster for the push/pop operation).

Note: both benchmarks show 0 allocs/op because the pool is warm from the previous iteration and the `[]callFrame` capacity is pre-allocated. The allocation profile (T1.1.1) reveals the real cost — pool factory calls during GC, which the micro-benchmark doesn't trigger.

###### T1.1.3 Counter-Based Profiling (fib)

| | fib(10) | fib(15) | fib(20) |
|---|---------|---------|---------|
| ops_executed | 2,562 | 28,604 | 317,415 |
| continuations_saved | 177 | 1,973 | 21,891 |
| continuations_restored | 177 | 1,973 | 21,891 |
| continuation_pool_releases | 177 | 1,973 | 21,891 |
| inline_evals_saved | 177 | 1,973 | 21,891 |
| shared_frame_restores | 0 | 0 | 0 |
| stack_pool_releases | 0 | 0 | 0 |
| save % of ops | 6.9% | 6.9% | 6.9% |
| envs_copied | 177 | 1,973 | 21,891 |
| closures_applied | 265 | 2,959 | 32,836 |
| no_copy_applies | 88 | 986 | 10,945 |

**Observations:**
- SaveContinuation is a stable 6.9% of all ops for tree-recursive fibonacci.
- 100% of continuations use inline eval storage (≤ 2 items on stack at save time).
- 0 shared frame restores (no call/cc in fibonacci).
- 0 stack pool releases (all inlined — no stack transfer needed).
- Every SaveContinuation is paired with exactly one RestoreAndRelease (0 abandoned frames).

###### T1.1.4 Min/Max Impact Estimates

**Input data:**
- fib(10) benchmark time: 27,307 ns/op (`BenchmarkRun/Fibonacci`)
- fib(10) save/restore cycles: 177 per invocation
- Per-cycle savings: 9.1ns (micro-benchmark delta)
- Continuation pool factory: 65.78% of allocation bytes

**Minimum estimate (dispatch savings only, no GC benefit):**
```
min_savings = 177 cycles × 9.1 ns/cycle = 1,611 ns
min_speedup = 1,611 / 27,307 = 5.9%
```

**Maximum estimate (dispatch + GC pressure reduction):**

Eliminating 65.78% of allocation volume reduces GC work. Assuming GC overhead is 8-12% of total benchmark time (typical for allocation-heavy Go programs), eliminating the dominant allocation source saves:
```
gc_savings = 27,307 × 0.10 × 0.66 = 1,802 ns  (midpoint: 10% GC, 66% of allocs removed)
max_savings = 1,611 + 1,802 = 3,413 ns
max_speedup = 3,413 / 27,307 = 12.5%
```

**Projected range: 5.9% – 12.5% speedup on fib(10).**

For continuation-heavier workloads (VM profile shows SaveContinuation at 13.8% of ops for the Schelog/ZebraPuzzle workload vs 6.9% for fib), the impact would be proportionally larger. The 8.7:1 save-to-restore ratio in ZebraPuzzle (abandoned continuations from call/cc) means most pool acquires are wasted — callStack would make these free.

**Decision gate: min_speedup (5.9%) > 5% threshold → PROCEED with implementation.**

###### T1.1.5 Actual Results (Post-Implementation)

Implementation completed and reverted (2026-03-17). The callStack approach replaced `MachineContinuation` pool with `[]callFrame` for SaveContinuation/Restore. Cold-path consumers (`call/cc`, composable continuations, marks, stack traces) materialized the callStack into linked chains on demand.

**Gabriel benchmark results (6 runs, M4 Max):**

| Benchmark | Baseline | CallStack | Change | Category |
|-----------|----------|-----------|--------|----------|
| tak | 0.1123 | 0.1071 | **-4.6%** | Win |
| fib | 0.3715 | 0.3598 | **-3.2%** | Win |
| sum | 0.0311 | 0.0282 | **-9.3%** | Win |
| sieve | 0.0808 | 0.0763 | **-5.6%** | Win |
| divrec | 0.8759 | 0.8359 | **-4.6%** | Win |
| ackermann | 0.4851 | 0.4734 | **-2.4%** | Win |
| triangl | 0.0382 | 0.0375 | -1.8% | Neutral |
| deriv | 0.1028 | 0.1045 | +1.7% | Neutral |
| primes | 0.2367 | 0.2420 | +2.2% | Neutral |
| peval | 0.0675 | 0.0694 | +2.8% | Neutral |
| diviter | 2.5677 | 2.6538 | +3.4% | Neutral |
| cpstak | 0.1806 | 0.1975 | **+9.4%** | Loss |
| sumfp | 0.6206 | 0.6848 | **+10.3%** | Loss |
| takl | 1.0883 | 1.2303 | **+13.0%** | Loss |
| nqueens | 1.9047 | 2.1606 | **+13.4%** | Loss |
| ctak | 1.6532 | 1.9841 | **+20.0%** | Loss |

**Root cause of regressions:** The split representation (callStack for hot path, `*MachineContinuation` chains for cold path) required O(depth) materialization every time a cold-path consumer needed the chain. In the old model, the chain already existed — no conversion needed. The materialization tax (heap allocation per frame + GC pressure) exceeded the dispatch savings on continuation-heavy workloads.

**Key lesson:** Profiling the dispatch cost alone (T1.1.2, T1.1.4) was insufficient. The estimate missed the representation-conversion tax that dominates when cold-path consumers are exercised. The pool-based linked list is well-optimized for Wile's workload mix because the linked list IS the universal representation — every consumer reads it directly with zero conversion.

**Verdict:** The pool-based `*MachineContinuation` linked list is retained. T1 is closed.

---

##### T1.0 Current Architecture

The continuation chain is a singly-linked list of heap-allocated `MachineContinuation` frames:

```
MachineContext.cont → frame_N → frame_N-1 → ... → frame_0 → nil
```

Each non-tail call:
1. `acquireContinuation()` — pool acquire, returns zeroed `*MachineContinuation`
2. Copy 12 fields from `MachineContext` into the frame (`machine_continuation.go:96-114`)
3. Transfer or inline the eval stack (`machine_context_continuation.go:209-223`)
4. Link `cont.parent = mc.cont`, set `mc.cont = cont`

Each normal return:
1. Copy 10 fields back from continuation to `MachineContext` (`machine_context_continuation.go:80-150`)
2. Transfer or copy the eval stack
3. `releaseContinuation(cont)` — pool release, zero all fields

**Key data from the codebase:**

| Metric | Source |
|--------|--------|
| `SaveContinuation` = 13.8% of all ops | `private/VM_EXECUTION_PROFILE.md` |
| Save-to-restore ratio = 8.7:1 | `private/VM_EXECUTION_PROFILE.md` (74.8M saves / 8.7M restores) |
| Inline evals threshold = 2 values | `machine_continuation.go:35` (covers >95% of cases per fib profile) |
| Fields saved per frame = 12 | `vm_state.go:79-95` table |
| `MachineContinuation` struct size = `vmState` + 4 fields (parent, promptHandler, shared, inline evals) | `machine_continuation.go:37-51` |
| `vmState` fields: env, freeVars, template, singleValue, multiValues, evals, pc, windingStack, promptTag, threadID, callDepth, envPooled, marks | `vm_state.go:96-213` |

**Sites that read/write `*MachineContinuation`:** 14 production files, 63 references total. Key consumers: `SaveContinuation`, `Restore`, `RestoreAndRelease`, `PopContinuation`, `FindPrompt`, `SliceContinuationAt`, `GraftContinuation`, `DeepCopy`, `MarkChainShared`, `CaptureStackTrace`, `CollectContinuationMarks`, `CollectMarksFromContinuation`, `ComposableContinuation`, `CapturedContinuation`, `debugger.go`.

---

##### T1.1 Profiling Phase (BEFORE Implementation)

**Goal:** Establish min/max performance impact estimates before writing any implementation code. The profiling phase produces numbers that determine whether T1 is worth pursuing and which sub-components contribute most to the current cost.

###### T1.1.1 Allocation Profile: Where Do Allocations Come From?

**Method:** Run Gabriel benchmarks + ZebraPuzzle with `-memprofile`. Identify what fraction of total allocations (by count and bytes) come from the continuation system vs other sources (env frames, closures, pairs, etc.).

**Measurements:**
- `acquireContinuation` / `NewMachineContinuationFromMachineContext` — pool acquire path
- `acquireStack` — stack allocation in SaveContinuation's non-inline path
- `evals.Copy()` — stack copy in Restore/RestoreAndRelease shared path
- `cloneMarks` — mark copy in Restore path
- Compare against: `acquireEnvFrame`, `NewClosureWithFreeVars`, `values.List` block allocations

**Deliverable:** Table showing allocation breakdown by source, fraction of total, for both a closure-heavy benchmark (nqueens) and a call/cc-heavy benchmark (ZebraPuzzle).

###### T1.1.2 Micro-Benchmark: Continuation Pool Round-Trip Cost

**Method:** Isolated benchmark measuring just the pool acquire→fill→release cycle, independent of VM execution.

```go
func BenchmarkContinuationPoolRoundTrip(b *testing.B) {
    // Setup: create a realistic MachineContext state
    for b.Loop() {
        cont := acquireContinuation()
        // fill fields (simulating NewMachineContinuationFromMachineContext)
        releaseContinuation(cont)
    }
}
```

**Compare against:** The equivalent operation for a contiguous stack:

```go
func BenchmarkCallStackAppendPop(b *testing.B) {
    stack := make([]callFrame, 0, 64)
    for b.Loop() {
        stack = append(stack, callFrame{/* fill fields */})
        stack = stack[:len(stack)-1]
    }
}
```

**Deliverable:** ns/op and allocs/op for both approaches. The difference is the per-call savings. Multiply by `ContinuationsSaved` counter to project total impact.

###### T1.1.3 Counter-Based Profiling: Chain Walk Costs

**Method:** Add temporary instrumentation (reverted after measurement) to count chain-walk lengths for:
- `FindPrompt` — how many frames walked on average?
- `CaptureStackTrace` — how many frames captured?
- `CollectContinuationMarks` — how many frames inspected?
- `SliceContinuationAt` — how many frames deep-copied?
- `MarkChainShared` — how many frames marked?

**Deliverable:** Average and max chain-walk lengths per benchmark. These determine whether replacing linked-list walks with array scans matters (short chains = negligible; long chains = meaningful).

###### T1.1.4 Min/Max Impact Estimates

**Minimum estimate (conservative):** Assume only the pool acquire/release overhead is eliminated. All other costs (field copies, stack handling, mark cloning) remain identical.

```
min_savings = (ns_pool_roundtrip - ns_append_pop) × ContinuationsSaved_per_benchmark
min_speedup = min_savings / total_benchmark_time
```

**Maximum estimate (optimistic):** Assume pool overhead + GC pressure reduction from fewer heap objects + cache locality improvement from contiguous frames.

```
max_savings = min_savings × cache_locality_multiplier (estimate 1.5-2x)
             + GC_savings (proportional to heap_reduction from eliminating pooled objects)
```

**Decision gate:** If `min_speedup` < 3% on the benchmark most dominated by SaveContinuation (ZebraPuzzle or nqueens), T1 is not worth the blast radius. If `min_speedup` > 5%, proceed to implementation.

---

##### T1.2 Design: Contiguous Call Stack

###### T1.2.1 New Type: `callFrame`

Replace heap-allocated `MachineContinuation` with a value-type `callFrame` stored contiguously in a slice:

```go
// callFrame holds the saved state for one non-tail call.
// Stored by value in []callFrame — no heap allocation per frame.
type callFrame struct {
    env          *environment.EnvironmentFrame
    freeVars     []values.Value
    template     *NativeTemplate
    singleValue  values.Value
    multiValues  MultipleValues
    pc           int
    threadID     uint64
    callDepth    int
    envPooled    bool
    marks        []markEntry

    // Prompt fields (non-nil only for prompt frames).
    promptTag     *PromptTag
    promptHandler Closure

    // Eval stack: inline storage for the common case (0-2 values).
    // When evals is nil, values are in inlineEvals[0:inlineEvalsLen].
    // When evals is non-nil, it owns the stack (transferred from mc).
    inlineEvalsLen uint8
    inlineEvals    [inlineEvalsCap]values.Value
    evals          *Stack
}
```

The `callFrame` is a value type (not a pointer). `[]callFrame` allocates all frames contiguously. Each `append` copies the struct value — no heap allocation for the frame itself. The only heap allocations within a frame are the pointer fields (`env`, `freeVars`, `template`, `marks`, `evals`, `promptHandler`), which are shared by pointer.

**Critical difference from `MachineContinuation`:** No `parent` pointer. Parent is implicit — `callStack[i-1]` is the parent of `callStack[i]`.

###### T1.2.2 MachineContext Changes

```go
type MachineContext struct {
    vmState
    callStack []callFrame  // replaces cont *MachineContinuation
    // ... other fields unchanged ...
}
```

**Initial capacity:** `make([]callFrame, 0, 64)`. Most programs use < 64 call depth. The Gabriel benchmark max depths are 20-50. ZebraPuzzle (backtracking) goes deeper but `call/cc` escapes reset the depth.

###### T1.2.3 SaveContinuation → Push Frame

```go
func (p *MachineContext) SaveContinuation(off int) error {
    p.callDepth++
    if p.maxCallDepth > 0 && uint64(p.callDepth) > p.maxCallDepth {
        p.callDepth--
        return werr.WrapForeignErrorf(werr.ErrCallDepthExceeded, ...)
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
        callDepth:   p.callDepth - 1, // parent's depth
        envPooled:   p.envPooled,
        marks:       p.marks,
    }

    // Inline eval stack into frame (same logic as today).
    n := p.evals.Len()
    if n <= inlineEvalsCap {
        frame.inlineEvalsLen = uint8(n)
        for i := range n {
            frame.inlineEvals[i] = (*p.evals)[i]
        }
        // frame.evals stays nil (sentinel)
        p.evals.Clear()
        p.counters.InlineEvalsSaved++
    } else {
        frame.evals = p.evals // transfer ownership
        p.evals = acquireStack()
    }

    p.callStack = append(p.callStack, frame)
    p.marks = nil
    return nil
}
```

**Cost:** One struct copy (the `callFrame` is ~160 bytes). No pool acquire. Go's `append` amortizes the backing array growth.

###### T1.2.4 PopContinuation → Pop Frame

```go
func (p *MachineContext) PopContinuation() error {
    p.callDepth--
    if p.callDepth < 0 {
        p.callDepth = 0
        return werr.WrapForeignErrorf(werr.ErrContinuationUnderflow, ...)
    }
    top := len(p.callStack) - 1
    frame := &p.callStack[top]

    p.template = frame.template
    p.env = frame.env
    p.freeVars = frame.freeVars
    p.pc = frame.pc
    p.singleValue = frame.singleValue
    p.multiValues = frame.multiValues
    p.envPooled = frame.envPooled
    p.marks = frame.marks

    if frame.evals == nil {
        restoreInlineEvalsFromFrame(p.evals, frame)
    } else {
        releaseStack(p.evals)
        p.evals = frame.evals
    }

    // Zero the frame to break GC references, then shrink.
    p.callStack[top] = callFrame{}
    p.callStack = p.callStack[:top]
    return nil
}
```

**Cost:** One struct read + field assignments. No pool release. The frame struct is zeroed in place.

**Note:** Current `PopContinuation` returns the frame for the caller (the `Run` loop) to release the old env. With the contiguous stack, the frame is ephemeral — the env release must happen within `PopContinuation` itself or via a returned env pointer. This is a design detail to resolve during implementation.

###### T1.2.5 RestoreAndRelease → Pop Frame (Normal Return)

The `RestoreAndRelease` path handles shared/unshared frames differently. With contiguous stacks, the `shared` flag changes meaning:

**Unshared (common case):** Same as PopContinuation above.

**Shared (call/cc captured):** The frame's evals must be copied, not transferred. The frame itself stays in the array (it was copied to a `MaterializedContinuation` chain during capture). The `shared` flag can be a `bool` on `callFrame`, set during `call/cc` capture on all frames from top to the prompt.

###### T1.2.6 call/cc: Materialize to Heap

When `call/cc` captures, the contiguous stack must be materialized into a heap-allocated linked chain (for storage in `ComposableContinuation`/`CapturedContinuation`):

```go
func (p *MachineContext) materializeCallStack() *MachineContinuation {
    if len(p.callStack) == 0 {
        return nil
    }
    // Build linked chain from bottom to top.
    var chain *MachineContinuation
    for i := range p.callStack {
        frame := &p.callStack[i]
        cont := &MachineContinuation{
            vmState: vmState{
                env:         frame.env,
                freeVars:    frame.freeVars,
                template:    frame.template,
                singleValue: frame.singleValue,
                multiValues: slices.Clone(frame.multiValues),
                pc:          frame.pc,
                threadID:    frame.threadID,
                callDepth:   frame.callDepth,
                envPooled:   false, // materialized frames are not pooled
                marks:       cloneMarks(frame.marks),
            },
            parent: chain,
        }
        // Copy evals (snapshot for re-invocation).
        if frame.evals == nil {
            cont.inlineEvalsLen = frame.inlineEvalsLen
            cont.inlineEvals = frame.inlineEvals
        } else {
            cont.evals = frame.evals.Copy()
        }
        chain = cont
    }
    // Mark all frames in callStack as shared.
    for i := range p.callStack {
        p.callStack[i].shared = true
    }
    return chain
}
```

**Cost:** O(depth) heap allocations. This is the *cold path* — `call/cc` is rare relative to normal calls. The profiling data shows `call-with-current-continuation` at 200K calls in ZebraPuzzle vs 74.8M `SaveContinuation` ops — a 374:1 ratio.

###### T1.2.7 FindPrompt → Backwards Array Scan

```go
func (p *MachineContext) FindPrompt(tag *PromptTag) (int, bool) {
    for i := len(p.callStack) - 1; i >= 0; i-- {
        if p.callStack[i].promptTag == tag {
            return i, true
        }
    }
    if p.promptTag == tag {
        return -1, true
    }
    return -1, false
}
```

Returns an index instead of a frame pointer. Callers adapted to use index.

###### T1.2.8 SliceContinuationAt → Array Slice + Materialize

```go
func (p *MachineContext) SliceContinuationAt(promptIndex int) *MachineContinuation {
    end := len(p.callStack)
    if promptIndex >= 0 {
        end = promptIndex // exclude the prompt frame
    }
    // Materialize frames [0..end) into a linked chain.
    // Same logic as materializeCallStack but bounded.
    ...
}
```

###### T1.2.9 CaptureStackTrace → Array Walk

```go
func (p *MachineContext) CaptureStackTrace(maxDepth int) StackTrace {
    trace := make(StackTrace, 0, 16)
    if p.template != nil {
        trace = append(trace, StackFrame{...})
    }
    for i := len(p.callStack) - 1; i >= 0 && len(trace) < maxDepth; i-- {
        frame := &p.callStack[i]
        trace = append(trace, StackFrame{
            FunctionName: frame.template.Name(),
            CurrentLoc:   frame.template.SourceAt(frame.pc - 1),
        })
    }
    return trace
}
```

###### T1.2.10 CollectContinuationMarks → Array Walk

```go
func (p *MachineContext) CollectContinuationMarks(tag *PromptTag) *ContinuationMarkSet {
    var frames [][]markEntry
    if len(p.marks) > 0 {
        frames = append(frames, cloneMarks(p.marks))
    }
    for i := len(p.callStack) - 1; i >= 0; i-- {
        frame := &p.callStack[i]
        if len(frame.marks) > 0 {
            frames = append(frames, cloneMarks(frame.marks))
        }
        if frame.promptTag == tag {
            break
        }
    }
    return &ContinuationMarkSet{frames: frames}
}
```

###### T1.2.11 ComposableContinuation and CapturedContinuation

These types continue to store `*MachineContinuation` linked chains. They receive materialized chains from `materializeCallStack`/`SliceContinuationAt`. Their `AcquireSegment`, `DeepCopy`, and `GraftContinuation` logic is unchanged — they operate on the materialized heap chain, not the live call stack.

When a `ComposableContinuation` is applied, its chain is grafted onto the current `mc.cont` (which no longer exists as a field). Instead:
- Materialize the composable's chain frames back into the callStack via a `graftToCallStack` method
- Or: maintain a hybrid where the callStack has a `materialized *MachineContinuation` base that `PopContinuation` falls through to when the array is empty

**Hybrid approach (recommended):**

```go
type MachineContext struct {
    vmState
    callStack           []callFrame
    materializedBase    *MachineContinuation // from composable continuation graft
    // ...
}
```

When `PopContinuation` empties the callStack and `materializedBase != nil`, switch to walking the materialized chain. This avoids the O(depth) cost of converting a materialized chain back into callStack frames on every composable continuation invocation.

---

##### T1.3 Implementation Phases

###### Phase 0: Profiling (T1.1)

Deliverables: allocation profile, micro-benchmarks, chain-walk instrumentation, min/max impact estimates.

**Decision gate:** If min_speedup < 3% on the heaviest benchmark, stop.

###### Phase 1: Infrastructure (No Behavioral Change)

**PR A: Add `callFrame` type alongside existing continuation chain.**

- Define `callFrame` struct in `machine/call_frame.go`
- Add `callStack []callFrame` field to `MachineContext`
- Add `materializedBase *MachineContinuation` field
- Initialize `callStack` in constructors (`NewMachineContext`, `AcquireTopLevelContext`, `NewSubContext`)
- All existing continuation logic unchanged — `callStack` is allocated but unused

**Tests:** Verify `callFrame` struct size. Verify `callStack` is initialized. All existing tests pass.

###### Phase 2: Dual-Path Save/Restore

**PR B: `SaveContinuation` writes to both `callStack` AND continuation chain.**

- `SaveContinuation` pushes a `callFrame` AND creates a `MachineContinuation` (dual-write)
- `PopContinuation` pops from both (dual-read, verify agreement)
- Counter: `callStackDepth` vs `callDepth` — assert equal after every operation
- This phase validates the frame content is correct without changing behavior

**Tests:** All existing tests pass. Add assertion that callStack depth matches continuation chain depth after every Save/Pop.

###### Phase 3: Switch to Call Stack (Behavioral Change)

**PR C: `SaveContinuation` writes to `callStack` only. `PopContinuation`/`RestoreAndRelease` read from `callStack` only.**

- Remove dual-write from Phase 2
- `mc.cont` field removed from normal Save/Pop path
- `call/cc` capture calls `materializeCallStack` to create linked chain for `ComposableContinuation`
- `FindPrompt` scans `callStack` array backwards
- `CaptureStackTrace` walks `callStack` array
- `CollectContinuationMarks` walks `callStack` array
- `materializedBase` handles composable continuation graft

**Tests:** All existing tests pass. Gabriel benchmarks. ZebraPuzzle (call/cc stress). Integration tests for call/cc + composable continuations + continuation marks + dynamic-wind across stack frame boundaries.

###### Phase 4: Cleanup

**PR D: Remove `MachineContinuation` pool. Clean up dead code.**

- Remove `continuationPool` from `pool.go`
- Remove `acquireContinuation`/`releaseContinuation`
- `MachineContinuation` retained ONLY for materialized chains (call/cc, composable continuations)
- Remove `shared` flag from `MachineContinuation` (sharing is a callStack concept now)
- Remove `MarkChainShared` (replaced by marking `callFrame.shared` in the array)

**Tests:** All existing tests pass. Verify no pool acquire/release in the normal call path.

---

##### T1.4 Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| `callFrame` struct too large for efficient append | Low | Medium | Measure. ~160 bytes is acceptable for Go's `append` — it copies via `memmove`, which is fast for structs < 256 bytes. Profile confirms or denies. |
| Composable continuation graft to callStack is complex | Medium | High | Hybrid approach: `materializedBase` field avoids converting chains back to array frames. Composable continuations stay as linked chains. |
| `call/cc` materialize is too slow for ZebraPuzzle | Low | Medium | ZebraPuzzle has 200K call/cc captures vs 74.8M saves. Even if materialize is 10x slower per operation, net savings from 74.6M cheaper saves dominate. |
| eval stack ownership transfer broken | Medium | High | Phase 2 dual-path validates field-by-field agreement before Phase 3 switches. |
| `callStack` growth unbounded for deep recursion | Low | Low | `maxCallDepth` limit already enforced in `SaveContinuation`. `callStack` cannot grow past this limit. |
| Shared frame semantics change breaks call/cc | Medium | High | Phase 2 dual-path catches disagreements. Integration tests for all call/cc patterns (escape, composable, re-invocation). |

---

##### T1.5 Files Modified

| File | Change | Phase |
|------|--------|-------|
| `machine/call_frame.go` (new) | `callFrame` type definition | 1 |
| `machine/machine_context.go` | Add `callStack`, `materializedBase` fields; update `Run` loop for new `PopContinuation` signature | 1-3 |
| `machine/machine_context_continuation.go` | Rewrite `SaveContinuation`, `PopContinuation`, `RestoreAndRelease`, `Restore`, `FindPrompt`, `SliceContinuationAt` | 2-3 |
| `machine/machine_continuation.go` | Retain for materialized chains; remove pool interaction | 3-4 |
| `machine/vm_state.go` | No change (shared by both callFrame and MachineContext) | — |
| `machine/pool.go` | Remove `continuationPool`; add callStack initial capacity | 4 |
| `machine/composable_continuation.go` | `AcquireSegment` returns materialized chain (unchanged); add `graftToCallStack` or `materializedBase` path | 3 |
| `machine/captured_continuation.go` | `applyCapturedContinuation` triggers `materializeCallStack` | 3 |
| `machine/continuation_mark_set.go` | `CollectContinuationMarks` and `CollectMarksFromContinuation` walk array or chain | 3 |
| `machine/machine_context_subcontext.go` | `NewSubContext` initializes `callStack` | 1 |
| `machine/debugger.go` | Walk `callStack` instead of continuation chain | 3 |
| `machine/machine_context_winding.go` | `RestoreWithWinding` uses callStack for frame traversal | 3 |
| `machine/stack_frame.go` | `CaptureStackTrace` walks `callStack` | 3 |
| `machine/counters.go` | Add `CallStackGrowths`, `MaterializedCaptures` | 1 |
| Test files (6+) | Update to new APIs, add callStack-specific tests | 1-4 |

**Total:** ~15 production files, ~6 test files.

#### T2. NaN-Boxing / Tagged Pointers

**Requires `unsafe`:** **YES. Will NOT be completed for Wile.**

**Gap:** `values.Value` is a Go interface (16 bytes per value, `values/values.go:85`). Every eval stack slot, every binding, every continuation-saved value pays 16 bytes. Racket/Chez/Chibi encode small values (fixnums, booleans, chars, `()`) in a single machine word (8 bytes) with no heap allocation.

**Why it requires `unsafe`:** Encoding type tags in pointer bits requires `unsafe.Pointer` to reinterpret `uint64` as heap pointers. Go's type system provides no safe mechanism to represent a tagged union of `int64 | *Pair | bool | char | ()` in 8 bytes. Go interfaces are the safe equivalent — they're 16 bytes because they store both a type pointer and a data pointer.

**Alternatives considered:**
- `uint64` with manual bit tagging: still requires `unsafe.Pointer` to recover heap pointers from the tagged word
- Separate typed stacks (one for fixnums, one for pointers): destroys the uniform `values.Value` interface, would require rewriting every value-consuming site in the codebase
- Accept the Go interface overhead: **this is Wile's position**

**Impact of not doing this:** The eval stack, bindings, and continuation frames remain 2x larger than a C-based implementation. The promoted opcodes (`OpAdd`, `OpNumLt`, etc.) mitigate part of the cost by avoiding dispatch ceremony, but values still flow through 16-byte interfaces. This is the "Go tax" — the cost of memory safety, garbage collection, and type safety.

---

#### T3. Custom Memory Allocator

**Requires `unsafe`:** **YES. Will NOT be completed for Wile.**

**Gap:** Racket uses a precise GC (originally Boehm, now Chez's collector). Chibi uses a custom copying collector. Both allocators are tuned for Scheme's allocation patterns (many small, short-lived objects).

**Why it requires `unsafe`:** Implementing a custom allocator in Go requires `unsafe.Pointer` to manage raw memory. Go's garbage collector is the only option without unsafe/CGo.

**Mitigation already in place:** `sync.Pool` for continuation frames, eval stacks, sub-contexts, and environment frames (`machine/pool.go`). Block-allocated pairs (`values.List()` allocates `make([]Pair, N)`). These approximate arena allocation within safe Go.

---

#### T4. Computed Goto / Direct-Threaded Dispatch

**Requires `unsafe`:** **YES. Will NOT be completed for Wile.**

**Gap:** Chez and Chibi use computed goto (GCC's `&&label` extension) for the VM dispatch loop, jumping directly to the next opcode handler via a function pointer table. This eliminates the branch prediction overhead of a central switch statement.

**Why it requires `unsafe`:** Go has no computed goto, no first-class labels, and no way to build a jump table manually. The Go compiler *may* generate a jump table for a dense `switch` on integer opcodes, but this is an implementation detail of the compiler, not guaranteed. There is no safe mechanism to force direct-threaded dispatch.

**Current state:** Wile's `Run()` loop uses a `switch instr.Op` with ~58 inlined cases. The Go compiler likely generates a jump table for this (dense integer range). This is the best achievable in safe Go.

---

#### T5. Procedure Inlining

**Requires `unsafe`:** No. Compiler-level bytecode transformation.

**Gap:** Wile performs no compile-time inlining of known procedures. Racket/Chez inline small known procedures at call sites when the binding is immutable.

**Design direction:**
- Reuse the flat closure infrastructure: `pass_free_var_analysis.go` already identifies `set!` targets. Bindings not in the `Mutated` set are immutable candidates.
- Inline criterion: procedure body is a single expression (or small number of instructions), binding is not `set!`-ed, callee is in scope.
- New pass between Pass 3 (ClosureFlatten) and Pass 4 (Peephole): scan for `OpCallCachedBinding` where the target is a known small closure, replace with inlined body.

**Prerequisite:** Design document scoping which procedures qualify and what "small enough" means. The opcode promotion work already inlines the 11 hottest primitives at the VM level — this would extend to user-defined functions.

**Priority:** Medium. Fills the last compiler-level gap vs Chez.

---

#### T6. Environment Frame Slimming for Flat Closures

**Requires `unsafe`:** No. Struct redesign.

**Gap:** Flat closures still create full `EnvironmentFrame` objects via `InitFlatApplyFrame` (`environment_frame.go:209`). The `EnvironmentFrame` struct (`environment_frame.go:93-108`) carries 6 fields: `parent`, `local`, `global`, `phaseLevel`, `phases`, `topLevel`. Flat closure bodies only need `local` (for parameter bindings). The other 5 fields are set but never read at runtime — the parent chain is dead code after flattening.

**Design direction:**
- Lightweight parameter-only frame type: `struct { bindings []Binding }` with no parent/global/phase fields
- Or: make `EnvironmentFrame` fields lazy — only populate parent/global/phases when first accessed (check for flat closure context)
- Eliminates 5 pointer/int copies per flat closure call

**Prerequisite:** Pairs naturally with T1 (stack frames). If the call frame representation changes, do both simultaneously.

**Priority:** Low-medium. Incremental improvement.

---

#### T7. De-Globalize Forms Registry

**Requires `unsafe`:** No. Internal refactor.

**Gap:** `internal/forms/form_spec.go` has a package-level global `registry` map populated by `init()` in `machine/register.go`. All engines in the same process share it. This blocks the Dialect system (`plans/ARCHITECTURE.md` Phase 1).

**Racket model:** Per-namespace form registration. Different modules can have different special form sets.

**Status:** Designed in `ARCHITECTURE.md` Phase 1 with blast radius analysis. Not implemented.

**Priority:** Low for performance. Required for v2.0.0 Dialect system goal. Orthogonal to the performance-focused tasks above.

---

#### T8. Opcode Promotion Phase 3

**Requires `unsafe`:** No.

**Gap:** `plans/OPCODE-PROMOTION.md` Phase 3 lists remaining candidates: `cons` (saves dispatch but not allocation), `modulo` (700K calls in primes/sieve), `not` (Scheme-defined `MachineClosure`, requires compiler recognition not peephole).

**Priority:** Low. The 11 promoted primitives already cover the dominant call volume.

---

#### T9. Flat Closure Deferred Items (C1, C2 remaining, C3)

**Requires `unsafe`:** No.

**C1 — Remove linked closure path:** Deferred. The linked path correctly serves zero-capture closures that save continuations. Removing it would force zero-capture closures through the flat path, allocating a zero-length `freeVars` slice — wasteful. **Current design is correct. Close this item.**

**C2 — Remaining fused opcodes:** `OpLoadFreeVarUnboxed` and `OpPushFreeVarUnboxed` deferred. Box pressure measured as negligible (Open Question 3 above). **Close this item — profiling shows insufficient value.**

**C3 — Collapse Pass 2 into single bottom-up pass:** Compiler-internal optimization, no runtime benefit. **Close this item.**

---

### Summary: Achievable vs Blocked

| Task | Requires `unsafe` | Status |
|------|-------------------|--------|
| T1. Stack frames replacing continuation chains | No | **Closed — attempted, net regression on Gabriel suite, reverted** |
| T2. NaN-boxing / tagged pointers | **Yes** | **Will NOT be done** |
| T3. Custom memory allocator | **Yes** | **Will NOT be done** |
| T4. Computed goto / direct-threaded dispatch | **Yes** | **Will NOT be done** |
| T5. Procedure inlining | No | Open — needs design document |
| T6. Environment frame slimming | No | Open — pairs with T1 |
| T7. De-globalize forms registry | No | Open — blocked on Dialect system priority |
| T8. Opcode promotion Phase 3 | No | Open — low priority |
| T9. Flat closure deferred items | No | Closed — profiling shows insufficient value |

### The Go Tax

Three of the four blocked tasks (T2, T3, T4) represent fundamental differences between Go's runtime model and C-based Scheme implementations. Collectively they account for roughly a 2-3x performance gap that is inherent to the language choice:

- **Value representation (T2):** 16-byte interfaces vs 8-byte tagged words — 2x memory overhead on the hottest data structures
- **Allocation control (T3):** Go's GC vs custom collectors — less control over allocation patterns, mitigated by `sync.Pool`
- **Dispatch mechanism (T4):** Go switch vs computed goto — branch prediction overhead, mitigated by dense opcode numbering

These are accepted costs of Wile's core design decision: pure Go, no CGo, `go get` installable. The remaining achievable tasks (T1, T5, T6) close the gap on *architectural* differences. After those, remaining performance differences are language-level, not design-level.
