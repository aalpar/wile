# Compiler Optimizer Plan (Phase 5)

**Status:** COMPLETE
**Parent:** `plans/PERFORMANCE_REFACTORING_PLAN.md` (Phase 5 of 7)
**Estimated Impact:** 5–15% overall execution improvement
**Risk:** Medium (correctness regressions in optimization passes)

## Background: What Is a Bytecode Optimizer?

A bytecode optimizer transforms compiled bytecode *after* the compiler generates it, but *before* the VM executes it. It's a rewrite pass over the instruction sequence that produces equivalent but more efficient code.

```
Source → Tokenizer → Parser → Expander → Validator → Compiler → [Optimizer] → VM
                                                                  ^^^^^^^^
                                                                  NEW PHASE
```

The optimizer doesn't change what programs *do* — it changes how efficiently the VM *does it*. Think of it as an editor that tidies up the compiler's output, removing redundancies the compiler couldn't easily avoid while generating code recursively.

### Why Can't the Compiler Just Emit Better Code?

The compiler generates code by recursively walking the syntax tree. Each node emits operations independently. For example, compiling `(define x 42)` emits:

```
LoadLiteralInteger(42)   ; compile the value 42
Push                     ; define needs value on stack
StoreLocal(x)            ; store to binding
LoadVoid                 ; define returns void
```

The `Push` before `StoreLocal` exists because `StoreLocal` was designed to consume from the stack. The compiler can't easily look ahead to see that the next operation will be a store — that pattern awareness is exactly what an optimizer adds.

### Three Optimization Strategies

| Strategy | When | How | Examples |
|----------|------|-----|---------|
| **Peephole** | After compilation | Scan for fixed patterns of 2–4 adjacent ops, replace with better sequence | Push→Pop elimination, Load→Push→Store fusion |
| **Constant folding** | During compilation | Detect operations on known-constant operands, evaluate at compile time | `(+ 1 2)` → `3`, `(not #f)` → `#t` |
| **Operations prealloc** | During compilation | Pre-size the operations slice to avoid repeated `append` growth | Estimate operation count from AST size |

## Current Architecture

### The Instruction Set

Wile has 29 operations, each implementing the `Operation` interface:

```go
type Operation interface {
    values.Value
    Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}
```

Operations are stored in `NativeTemplate.operations` (a `[]Operation` slice). Each operation is a separate Go struct allocated on the heap, dispatched via **interface method call** in the VM loop:

```go
// machine_context.go:527
mc, err = mc.template.operations[mc.pc].Apply(mc.ctx, mc)
```

This means every instruction execution involves:
1. Slice index → interface value (contains type pointer + data pointer)
2. Interface dispatch → look up `Apply` method via type pointer's method table
3. Indirect call to the concrete `Apply` implementation

### How Operations Accumulate Today

During compilation, operations are appended one-by-one:

```go
// native_template.go:105-111
func (p *NativeTemplate) appendOperationsWithSource(src *syntax.SourceContext, ops ...Operation) {
    idx := p.internSource(src)
    p.operations = append(p.operations, ops...)
    for range ops {
        p.sourceRefs = append(p.sourceRefs, idx)
    }
}
```

Neither `operations` nor `sourceRefs` are pre-allocated. Go's `append` doubles capacity when full, so a template that ends up with 20 operations may trigger 4–5 reallocations (cap 0→1→2→4→8→16→32).

### Parallel Source Tracking

Every operation `[i]` has a parallel source reference `sourceRefs[i]` pointing into a deduplicated `sourceTable`. The optimizer must maintain this 1:1 correspondence — if it removes or reorders operations, it must update `sourceRefs` accordingly. Incorrect source refs cause wrong file/line in error messages, not crashes.

### What the Compiler Emits for Common Patterns

Understanding these patterns is essential for knowing where the optimizer can improve things.

**Function call `(f x y)` — the hot path:**
```
SaveContinuation(+8)    ; save return address (non-tail only)
<compile f>              ; result in value register
Push                     ; push f to eval stack
<compile x>              ; result in value register
Push                     ; push x to eval stack
<compile y>              ; result in value register
Push                     ; push y to eval stack
Pull                     ; dequeue f from bottom → value register
Apply                    ; pop all args, call f
```

**Tail call `(f x y)` in tail position:**
```
<compile f>              ; result in value register
Push                     ; push f to eval stack
<compile x>              ; result in value register
Push                     ; push x to eval stack
<compile y>              ; result in value register
Push                     ; push y to eval stack
Pull                     ; dequeue f from bottom → value register
Apply                    ; pop all args, call f (no SaveContinuation!)
```

**Define `(define x 42)`:**
```
LoadLiteralInteger(42)   ; compile value
Push                     ; push to stack
StoreLocal(x)            ; store from stack to binding
LoadVoid                 ; define returns unspecified
```

**If `(if test c a)`:**
```
<compile test>           ; result in value register
Push                     ; push test result
BranchOnFalse(→alt)      ; pop and branch if #f
<compile consequent>     ; result in value register
Branch(→end)             ; skip alternative
alt:
<compile alternative>    ; result in value register
end:
```

**Lambda/closure creation:**
```
LoadLiteral(template)    ; load compiled template
Push                     ; push to stack
LoadLiteral(env)         ; load environment frame
Push                     ; push to stack
MakeClosure              ; pop both, create closure → value register
```

**Literal variable reference `x`:**
```
LoadLocal(x)             ; load from local binding
```
or
```
LoadGlobal(gi)           ; load from global binding
```

## Phase 5 Sub-Phases

### Phase 5.0: Operations Preallocation

**What:** Pre-size the `operations` and `sourceRefs` slices before compilation to avoid append-triggered reallocations.

**Why:** Every `append` that exceeds capacity allocates a new, larger backing array and copies all existing elements. For a template with N operations, this wastes O(N) memory in abandoned arrays that the GC must collect.

**How:**

1. Add an `estimateOperationCount` function that walks the validated AST and returns a rough count:
   - Each `ValidatedCall` → ~(3 + 2×argCount) operations (save, compile-proc, push, per-arg×push, pull, apply)
   - Each `ValidatedIf` → ~4 + children ops (push, branch-false, branch-end, void-load)
   - Each `ValidatedDefine` → ~3 + child ops (push, store, load-void)
   - Each `ValidatedLambda` → ~5 (load-template, push, load-env, push, make-closure)
   - Each `ValidatedLiteral` → 1 (load literal)
   - Each `ValidatedSymbol` → 1 (load local/global)
   - Each `ValidatedBegin` → sum of children
   - Each `ValidatedDynamicWind` → ~25 (inline bytecode is large)
   - Default → 2 (conservative estimate for unknown forms)

2. Pre-allocate in `NewNativeTemplate`:
   ```go
   func NewNativeTemplateWithCapacity(pcnt, vcnt int, vd bool, estOps int) *NativeTemplate {
       q := &NativeTemplate{
           // ...
           operations:  make(Operations, 0, estOps),
           sourceRefs:  make([]uint16, 0, estOps),
           sourceTable: []*syntax.SourceContext{nil},
       }
       return q
   }
   ```

3. Call `estimateOperationCount` at the start of `compileBody` and `CompileExpression` to provide the estimate.

**Files:** `machine/native_template.go`, `machine/compile_validated.go`, `machine/compile_time_continuation.go`

**Verification:** `make test`, then `make bench` — should see slight improvement in allocation-heavy benchmarks.

**Correctness risk:** Zero — this is purely a capacity hint. Under-estimates are fine (falls back to normal append growth). Over-estimates waste a small amount of memory.

---

### Phase 5.1: Peephole Optimizer Infrastructure

**What:** Create a post-compilation optimization pass that scans the operations slice for known patterns and rewrites them.

**Why:** The compiler emits code pattern-by-pattern without cross-pattern awareness. Redundant sequences (e.g., Push immediately followed by Pop) are common and can be eliminated mechanically.

**How:**

1. **New file: `machine/optimizer.go`**

   The optimizer is a function that takes a `*NativeTemplate` and rewrites its operations in-place:

   ```go
   // Optimize applies peephole optimizations to the template's operations.
   // Operations and sourceRefs are modified in-place.
   // Branch targets are adjusted after optimization.
   func Optimize(tpl *NativeTemplate) {
       changed := true
       for changed {
           changed = false
           changed = changed || eliminatePushPop(tpl)
           changed = changed || eliminateDeadLoadVoid(tpl)
           // ... more rules
       }
       // Recompute branch offsets after ops were removed
       fixBranchTargets(tpl)
   }
   ```

2. **Branch target fixup:** This is the trickiest part. When operations are removed, all branch offsets that jump *over* the removed operations must be adjusted.

   The strategy:
   - Before optimization, build a map: `originalIndex → Operation` for all branch ops
   - After optimization (operations removed), build `originalIndex → newIndex` mapping
   - Walk all branch operations and recompute their offsets using the mapping

   Alternative (simpler): Use **absolute target indices** internally during optimization, convert back to relative offsets at the end.

3. **Hook into compilation pipeline:** Call `Optimize(tpl)` at the end of `compileBody()` (after `RestoreContinuation` is appended) and at the end of top-level `CompileExpression()`.

4. **Opt-out flag:** Add an `optimize bool` field to `CompileTimeContinuation` (default `true`), settable via an engine option like `WithOptimizer(false)` for debugging.

**Files:** `machine/optimizer.go` (new), `machine/optimizer_test.go` (new), `machine/compile_time_continuation.go` (hook), `engine.go` (option)

**Key design decision:** Peephole rules operate on a *window* of 2–4 adjacent operations, not on the whole sequence. This keeps them simple, local, and composable. Fixed-point iteration (run until no rule fires) handles cases where one optimization creates opportunities for another.

---

### Phase 5.2: Peephole Rule — Push/Pop Elimination

**What:** Remove adjacent `Push` → `Pop` pairs that cancel out.

**Pattern:**
```
... value in register ...
Push         ; move value register → stack
Pop          ; move stack top → value register
... value back in register, unchanged ...
```

**After optimization:**
```
... value in register ...
(both ops removed)
... value still in register ...
```

**Why this is safe:** `Push` copies the value register to the stack top. `Pop` copies the stack top back to the value register. If nothing reads the stack between them, the net effect is zero.

**When this happens:** Almost never in isolation (the compiler is smarter than that), but it can appear after other optimizations remove intermediate operations. This rule is primarily a "cleanup" rule.

**Implementation:**
```go
func eliminatePushPop(tpl *NativeTemplate) bool {
    changed := false
    ops := tpl.operations
    for i := 0; i < len(ops)-1; i++ {
        _, isPush := ops[i].(*OperationPush)
        _, isPop := ops[i+1].(*OperationPop)
        if isPush && isPop {
            removeOps(tpl, i, 2)
            changed = true
            // Don't increment i — re-check at same position
        }
    }
    return changed
}
```

**Verification:** Unit test with hand-crafted templates containing Push→Pop sequences.

---

### Phase 5.3: Peephole Rule — LoadVoid Deduplication at Define Boundaries

**What:** When multiple defines appear in sequence, each emits a trailing `LoadVoid`. All but the last are dead (overwritten by the next define's compilation). Remove them.

**Pattern (two sequential defines):**
```
<compile value1>
Push
StoreLocal(x)
LoadVoid          ← dead: overwritten by next define's LoadLiteralInteger
<compile value2>
Push
StoreLocal(y)
LoadVoid          ← keep: this is the final value
```

**After:**
```
<compile value1>
Push
StoreLocal(x)
<compile value2>
Push
StoreLocal(y)
LoadVoid
```

**Why this matters:** Function bodies with many `define` forms (common in Scheme) emit one dead `LoadVoid` per non-last define. In a body with 10 defines, that's 9 wasted operations.

**Implementation:** Scan for `LoadVoid` followed by anything that overwrites the value register (any Load operation). Remove the `LoadVoid`.

```go
func eliminateDeadLoadVoid(tpl *NativeTemplate) bool {
    changed := false
    ops := tpl.operations
    for i := 0; i < len(ops)-1; i++ {
        _, isVoid := ops[i].(*OperationLoadVoid)
        if isVoid && overwritesValueRegister(ops[i+1]) {
            removeOps(tpl, i, 1)
            changed = true
        }
    }
    return changed
}

func overwritesValueRegister(op Operation) bool {
    switch op.(type) {
    case *OperationLoadVoid,
        *OperationLoadLiteralInteger,
        *OperationLoadLiteralByLiteralIndexImmediate,
        *OperationLoadLocalByLocalIndexImmediate,
        *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate,
        *OperationPop,
        *OperationPull,
        *OperationPeekK:
        return true
    }
    return false
}
```

---

### Phase 5.4: Peephole Rule — Push/Pull Elimination for Self-Calls

**What:** In procedure calls, the compiler always emits `Push` (to save the procedure) then later `Pull` (to retrieve it after args are pushed). For zero-argument calls `(f)`, this becomes:

```
<compile f>
Push          ; push f
Pull          ; immediately pull f back (no args between)
Apply
```

**After:**
```
<compile f>
Apply         ; f is already in value register
```

**Note:** This only applies when there are exactly zero arguments (no Push between the Push-of-proc and Pull). With arguments present, the stack contains interleaved values and Pull is necessary.

**Implementation:** Detect `Push` immediately followed by `Pull` (a special case of Push/Pop but for Pull instead of Pop). Same removal logic.

---

### Phase 5.5: Constant Folding (Compile-Time Evaluation)

**What:** Detect expressions whose operands are all compile-time constants and evaluate them during compilation instead of emitting code.

**Examples:**
```scheme
(+ 1 2)       → 3        ; instead of emitting call to +
(* 3 4)       → 12
(not #f)      → #t
(zero? 0)     → #t
(< 3 5)       → #t
(string-length "hello") → 5
```

**Why this is more complex than peephole:** Constant folding doesn't look at *bytecode patterns* — it looks at the *AST* during compilation. It requires the compiler to recognize:
1. The procedure being called is a known primitive (not a variable that could be redefined)
2. All arguments are literal constants
3. The primitive is side-effect-free (pure function)

**Why this is tricky in Scheme:** In standard Scheme, *any* binding can be redefined. `(define + -)` is legal. So the compiler can't assume `+` means addition unless it can prove the binding hasn't been shadowed.

**Safe approach:** Only fold operations on values that are statically known at compile time:
- Integer arithmetic on literal integers where the result is also an integer
- Boolean operations (`not`) on literal booleans
- Only for bindings that resolve to known primitive implementations

**Implementation (conservative):**

1. Add a set of "foldable primitives" (pure, no side effects, deterministic):
   ```go
   var foldablePrimitives = map[string]bool{
       "+": true, "-": true, "*": true,
       "not": true, "zero?": true, "positive?": true, "negative?": true,
       "=": true, "<": true, ">": true, "<=": true, ">=": true,
   }
   ```

2. In `compileValidatedCall`, before emitting the call:
   - Check if proc is a symbol resolving to a known primitive
   - Check if all args are `ValidatedLiteral` with constant values
   - If both: evaluate at compile time, emit `LoadLiteral(result)` instead

3. Guard against overflow/errors: wrap the fold in a recover. If the arithmetic panics (division by zero, overflow), fall through to normal compilation.

**Files:** `machine/compile_validated.go` (fold check in `compileValidatedCall`), `machine/constant_fold.go` (new, fold logic)

**Phase 6 dependency:** Constant folding for arithmetic has higher payoff *after* Phase 6 (switch dispatch), because folded constants avoid both the Apply overhead AND the interface dispatch. But it's independently valuable now.

---

### Phase 5.6: Peephole Rule — Branch-Over-Branch Simplification

**What:** Simplify conditional branches where both paths end up at the same place, or where the condition result is statically known.

**Pattern 1 — Empty alternative removal:**
```
Push
BranchOnFalse(→alt)
<consequent>
Branch(→end)
alt:
LoadVoid          ; empty alternative loads void
end:
```

This was already emitted correctly, but if the consequent is a single operation that leaves the value register set, the `Branch(→end)` can be eliminated when `alt` == `end - 1`:

The `LoadVoid` at `alt` is only reached when the branch is taken. If the code after `end` doesn't use the value register, the whole alternative branch is dead. But proving "doesn't use value register" requires data flow analysis, which is out of scope for Phase 5.

**Pattern 2 — Constant condition:**
```
LoadLiteralInteger(5)     ; always truthy (non-#f)
Push
BranchOnFalse(→alt)       ; never taken!
<consequent>
Branch(→end)
alt:
<alternative>             ; dead code
end:
```

If the value pushed before `BranchOnFalse` is a known non-`#f` constant, the branch is never taken. Remove the branch, the alternative code, and the skip branch.

**Implementation:** This is more complex because it requires understanding the data flow into the branch. Defer to a follow-up if Phase 5.5 (constant folding) doesn't already handle the common cases.

**Recommendation:** Mark this as **stretch goal** for Phase 5. The compiler already generates decent branching code. The high-value optimizations are 5.0–5.5.

---

### Phase 5.7: Optimizer Statistics & Debugging

**What:** Add counters for optimization activity and a way to inspect what the optimizer changed.

**Implementation:**

1. Add `OptimizerStats` struct:
   ```go
   type OptimizerStats struct {
       PushPopEliminated    int
       DeadLoadVoidRemoved  int
       PushPullEliminated   int
       ConstantsFolded      int
       TotalOpsRemoved      int
       TotalOpsOriginal     int
   }
   ```

2. Return stats from `Optimize()` for logging.

3. Add `--dump-bytecode` CLI flag (or engine option) that prints the operations before and after optimization. This is invaluable for debugging and for understanding whether optimizations are firing.

**Files:** `machine/optimizer.go` (stats), `engine.go` (option), `cmd/main.go` (flag)

---

## Implementation Order

```
5.0 Prealloc ──→ 5.1 Infrastructure ──→ 5.2 Push/Pop ──→ 5.3 Dead LoadVoid
                                                                    │
                                                    5.4 Push/Pull ◄─┘
                                                         │
                                              5.5 Constant Fold
                                                         │
                                              5.6 Branch Simplification (stretch)
                                                         │
                                              5.7 Stats & Debugging
```

Phases 5.0 (prealloc) and 5.1 (infrastructure) are independent of each other. Phases 5.2–5.4 depend on 5.1. Phase 5.5 is independent of 5.1 (it operates during compilation, not as a post-pass). Phase 5.6 depends on both 5.1 and 5.5.

## Critical Invariants

These MUST be maintained by any optimization pass:

1. **Source refs parallel array:** `len(operations) == len(sourceRefs)` at all times. When removing ops, remove corresponding sourceRefs entries.

2. **Branch offsets:** All branch operations use *relative* offsets from their own position. Removing operations before a branch target changes the offset. The `fixBranchTargets` pass must handle this.

3. **Stack discipline:** Operations that push to the stack must be balanced by operations that pop. The optimizer must never remove a Push without also removing its matching Pop (or vice versa).

4. **Value register liveness:** Removing an operation is only safe if nothing depends on its side effects (value register state, stack state, environment mutation, control flow).

5. **Template literals pool:** The optimizer should NOT modify the literals pool. Unused literals are harmless (small overhead, no correctness issue). Renumbering `LiteralIndex` values would require updating all `LoadLiteral` and `StoreGlobal` operations — too error-prone for the benefit.

6. **Continuation semantics:** `SaveContinuation` offsets work the same as branches (relative offset). They must be fixed up in `fixBranchTargets` alongside branches.

7. **Recursive templates:** Optimization must be applied to *each* `NativeTemplate` independently. Lambda bodies are separate templates. The optimizer runs on each template after it's fully compiled.

## Branch Target Fixup — Detailed Design

This is the most complex part of the optimizer, so here's a detailed design.

### The Problem

When we remove operations from the middle of a template, every branch that crosses the removal point has the wrong offset. Example:

```
Index  Operation
0      LoadLiteralInteger(5)
1      Push
2      BranchOnFalse(offset=4)    ; target = 2+4 = index 6
3      LoadLiteralInteger(42)      ; consequent
4      Push                        ← REMOVED by optimizer
5      Pop                         ← REMOVED by optimizer
6      Branch(offset=2)            ; target = 6+2 = index 8
7      LoadVoid                    ; alternative
8      ...                         ; after if
```

After removing indices 4 and 5:

```
Index  Operation
0      LoadLiteralInteger(5)
1      Push
2      BranchOnFalse(offset=?)     ; was 4, but target (old 6) is now at new index 4
3      LoadLiteralInteger(42)
4      Branch(offset=?)            ; was 2, but target (old 8) is now at new index 6
5      LoadVoid
6      ...
```

### The Solution: Index Remapping

1. **Before any removals:** Record which indices will be removed (a bitset or set).

2. **Build remap table:** `oldIndex → newIndex`. For each old index, the new index is `oldIndex - (count of removed indices before oldIndex)`.

3. **Fix branches and SaveContinuations:**
   ```go
   for i, op := range tpl.operations {
       switch o := op.(type) {
       case *OperationBranchOffsetImmediate:
           oldSource := oldIndexOf[i]
           oldTarget := oldSource + int(o.Offset)
           newTarget := remap[oldTarget]
           newOffset := newTarget - i
           tpl.operations[i] = NewOperationBranchOffsetImmediate(newOffset)
       // ... same for BranchOnFalse, BranchOnNotFalse, SaveContinuation
       }
   }
   ```

### Alternative: Two-Phase with Tombstones

Instead of removing in-place and fixing up:

1. **Phase 1:** Mark removed operations as `nil` (tombstones).
2. **Phase 2:** Compact: build new slice skipping nils, build remap table simultaneously.
3. **Phase 3:** Fix all relative offsets using the remap table.

This is cleaner because all removals happen in Phase 1 without worrying about index shifts, and the remap only needs to be computed once in Phase 2.

Recommend this approach.

## Testing Strategy

### Unit Tests (`machine/optimizer_test.go`)

For each peephole rule, test:
1. **Pattern matches:** Construct a template with the target pattern, verify removal.
2. **Pattern doesn't match:** Construct similar-but-not-matching patterns, verify no change.
3. **Branch fixup:** Templates with branches that cross the optimization site.
4. **Multiple patterns:** Templates with overlapping optimization opportunities.
5. **No ops:** Empty template, single-op template.

### Integration Tests

Run existing Scheme tests through the optimizer:
- `make test` must pass (all 1000+ tests exercise the optimized compilation path)
- Gabriel benchmarks must produce identical output
- The hygiene test suite is particularly important (optimizer must not break macro expansion semantics)

### Benchmark Comparisons

Before/after on:
- `make bench` (Go-level micro-benchmarks)
- `make bench-gabriel` (Scheme-level Gabriel suite)
- `make bench-schelog` (logic programming stress test)

### Regression Testing

Add a `TestOptimizerDoesNotChangeResults` that compiles+runs a set of expressions both with and without the optimizer, verifying identical results.

## Expected Impact

| Sub-phase | What | Expected Improvement |
|-----------|------|----------------------|
| 5.0 Prealloc | Fewer allocs during compilation | 2–5% compilation speed |
| 5.2 Push/Pop | Fewer stack operations | <1% (rare pattern) |
| 5.3 Dead LoadVoid | Fewer wasted ops in define-heavy bodies | 1–3% in define-heavy code |
| 5.4 Push/Pull | Faster zero-arg calls | <1% (uncommon) |
| 5.5 Constant fold | Eliminate calls for literal arithmetic | 2–5% in arithmetic-heavy code |
| 5.6 Branch simplify | Eliminate dead branches | 1–2% (stretch goal) |
| **Total** | | **5–10%** |

The big wins come in Phase 6 (switch dispatch) and Phase 7 (tagged integers). Phase 5 establishes the infrastructure and picks off the easy wins.

## Files Summary

| File | Action | Purpose |
|------|--------|---------|
| `machine/optimizer.go` | Create | Optimizer pass, peephole rules, branch fixup |
| `machine/optimizer_test.go` | Create | Unit tests for all rules |
| `machine/constant_fold.go` | Create | Compile-time constant evaluation |
| `machine/constant_fold_test.go` | Create | Tests for constant folding |
| `machine/native_template.go` | Modify | Add `NewNativeTemplateWithCapacity`, prealloc support |
| `machine/compile_validated.go` | Modify | Hook constant folding into call compilation |
| `machine/compile_time_continuation.go` | Modify | Hook optimizer into compilation pipeline, estimate function |
| `engine.go` | Modify | Add `WithOptimizer(bool)` option |

## Glossary

| Term | Meaning |
|------|---------|
| **Peephole optimization** | Pattern matching over a small window (2–4) of adjacent instructions |
| **Constant folding** | Evaluating expressions with known-constant operands at compile time |
| **Dead code elimination** | Removing instructions whose results are never used |
| **Branch target fixup** | Recalculating relative jump offsets after instructions are inserted or removed |
| **Tombstone** | A placeholder (nil) marking a removed instruction before compaction |
| **Fixed-point iteration** | Running optimization passes repeatedly until no more changes occur |
| **Value register** | The VM's primary result register (`mc.singleValue`) |
| **Eval stack** | The VM's operand stack (`mc.evals`) for intermediate values |
| **NativeTemplate** | The compiled bytecode container (ops + literals + source map) |
| **LiteralIndex** | Index into the template's literals pool |

## Open Questions

1. **Should the optimizer run on macro transformer templates?** These are compiled templates used by `syntax-rules` and `syntax-case`. They execute during expansion, not runtime. Optimizing them would speed up macro expansion but adds risk. **Recommendation:** Skip initially, add later if macro expansion is a bottleneck.

2. **Should constant folding handle `string-append` and other allocating primitives?** Folding `(string-append "a" "b")` → `"ab"` at compile time saves a call but creates a string object in the literals pool that might not be used. **Recommendation:** Start with numeric and boolean operations only.

3. **Per-template or global optimization?** Each template is optimized independently. Cross-template optimization (e.g., inlining a small closure) is out of scope for Phase 5. **Recommendation:** Per-template only.
