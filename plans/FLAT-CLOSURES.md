# Flat Closures Implementation Plan

**Status:** All three PRs merged (#514, #515, #516). Remaining: C1 (remove linked path — deferred, serves as zero-capture fallback), C2 partial (OpPushFreeVar done; OpLoadFreeVarUnboxed, OpPushFreeVarUnboxed not done), C3 not done. Open questions 1-3 unresolved.
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

   **Finding: Not worth pooling.** The freeVars array allocation is proportional to closure *creation* count, not *call* count. Since closures are typically created once and called many times, freeVars allocation is noise in the typical case.

   - **Pathological case** (HotBoxCreation: 1000 new closures per benchmark iteration):
     - `fv := make(...)`: 11.1% of allocs by count, 6.9% by bytes
     - Smaller than env frame allocation (16.6% by bytes) and closure struct (10.5% by bytes) at the same site
   - **Typical case** (Counter: closure created once, called 1000×):
     - `fv := make(...)`: 0.3% of allocs by count, 0.09% by bytes
     - Dominated entirely by `drainAndApply` (98%+)
   - Observed capture counts: 1 element in 4/5 benchmarks, 3 elements in 1/5. Matches prediction that small arrays dominate.

   **If revisited:** A `sync.Pool` for 1-element `[]values.Value` slices would only help when closures are created in a tight loop — a pattern not seen in Gabriel benchmarks or typical Scheme code. The env frame pool factory (32% of total bytes in HotBoxCreation) would be a higher-leverage target.

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
