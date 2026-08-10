# Peephole Optimizer

The peephole optimizer is a post-compilation pass that rewrites
`NativeTemplate` bytecode to reduce dispatch overhead. In a
switch-dispatch interpreter like Wile's VM, the dominant cost is not
computation but dispatch: fetching the next opcode, branching into the
switch case, and returning to the loop head. Fusing adjacent instructions
into single "superinstructions" (Ertl & Gregg 2003) eliminates these
dispatch cycles without changing observable semantics.

The optimizer runs on every compiled template — top-level expressions,
lambda bodies, library bodies, and macro transformers.

## Pipeline Overview

```
NativeTemplate.Optimize()
│
├─ Pass 1: EditPlan
│   ├─ markDeadLoadVoidEdits   (dead code elimination)
│   ├─ fuseLoadPush            (4 Load variants + Push → PushVariant)
│   └─ fusePullApply           (Pull + Apply → PullApply)
│   └─ plan.Apply()
│
├─ Pass 2: EditPlan
│   └─ fuseCallForeignCached   (SaveCont+PushCachedBinding...PullApply → CallForeignCached)
│   └─ plan2.Apply()           (also: promoted primitive specialization)
│
├─ Pass 3: EditPlan
│   └─ fuseCallGeneric         (SaveCont+PushLocal...PullApply → CallLocal)
│   └─ plan3.Apply()
│
├─ Pass 4: EditPlan
│   └─ fusePromotedCompoundArgs (tail promoted call whose args are calls;
│   └─ plan4.Apply()             gated on a preceding OpReleaseEnvFrame)
│
└─ optimizeSubTemplates()      (recurse into lambda sub-templates in literals pool)
```

Each pass uses a fresh `EditPlan`. This is not optional — branch offset
fixup is computed from the pre-compaction code, so edits from different
passes cannot share a plan. Pass 2 depends on Pass 1's output
(`PullApply` must exist before `fuseCallForeignCached` can match it).
Pass 3 depends on Pass 2 having claimed all foreign-closure patterns
first. Pass 4 picks up the tail calls Passes 2 and 3 could not claim
because an argument was itself a call.

## Pass 1: Dead Code Elimination and Basic Fusion

Three rules share one `EditPlan`:

### Dead LoadVoid Elimination

The compiler emits `LoadVoid` to initialize the value register in
contexts where the value might be read before being written (e.g., after
a branch). If the next instruction unconditionally writes the value
register, the `LoadVoid` is dead.

```
LoadVoid              ← dead: next instruction overwrites value register
LoadLiteral(42)
```

The `writesValueRegister` predicate checks `opcodeTable[op].writesValue`
— a per-opcode metadata flag set for `LoadLiteral`, `LoadGlobal`,
`LoadLocal`, `LoadCachedBinding`, `Pop`, `Pull`, `PeekK`,
`MakeClosure`, and `LoadVoid` itself.

### Load+Push Fusion

Every argument push compiles to a two-instruction sequence: load the
value into the value register, then push it onto the eval stack. Fusing
eliminates one dispatch per argument.

| Before | After |
|--------|-------|
| `LoadLiteral(n)` + `Push` | `PushLiteral(n)` |
| `LoadGlobal(n)` + `Push` | `PushGlobal(n)` |
| `LoadLocal(n)` + `Push` | `PushLocal(n)` |
| `LoadCachedBinding(n)` + `Push` | `PushCachedBinding(n)` |

The fused instruction inherits the `Arg` from the `Load` and the source
attribution from the `Load` (not the `Push`).

**Branch target constraint:** If the `Push` is a branch target, fusion
is forbidden. This occurs at convergence points — for example, both
branches of an `if` expression may push their results to a shared `Push`
instruction. Fusing would bind the `Push` to only one branch's `Load`.

### Pull+Apply Fusion

Every function call ends with `Pull` (dequeue the callable from the
bottom of the eval stack) followed by `Apply` (dispatch the call).
Fusing saves one dispatch per call site.

```
Pull + Apply  →  PullApply
```

Same branch target constraint as Load+Push: if `Apply` is a branch
target, fusion is skipped.

## Pass 2: Foreign Call Fusion

This pass recognizes call sequences where the callee is a known
`*ForeignClosure` (a Go-implemented primitive resolved at compile time
via cached bindings). Two patterns:

### Non-Tail Pattern

```
SaveContinuation(off)       ← saves caller state; off targets return point
PushCachedBinding(idx)      ← callee (verified to be *ForeignClosure)
... 0+ Push-family ops ...  ← arguments
PullApply                   ← SaveCont offset targets one past here
```

Rewrite:
- **Keep** `SaveContinuation` (the fused call still needs stack isolation;
  `CallForeignCached` uses `Drain()` which needs the saved evals boundary)
- **Delete** `PushCachedBinding` (the callee index moves into the fused op)
- **Replace** `PullApply` with `CallForeignCached(idx)`

At runtime, `callForeignCached` looks up `cachedBindings[idx]`, drains
the eval stack for arguments, calls the Go function directly, and
restores from `SaveContinuation`.

### Tail Pattern

```
PushCachedBinding(idx)      ← callee, NOT preceded by SaveCont or push-family
... 0+ Push-family ops ...  ← arguments
PullApply
```

Rewrite:
- **Delete** `PushCachedBinding`
- **Replace** `PullApply` with `CallForeignCachedTail(idx)`

At runtime, `callForeignCached` with `tail=true` calls
`returnImmediate()` instead of restoring a continuation.

### Validation Guards

All patterns require:

1. The binding at `idx` holds a `*ForeignClosure` (type assertion at
   compile time)
2. `SaveContinuation.Arg` points exactly to `PullApply` (offset check)
3. No branch targets in the interior between callee push and `PullApply`
4. All intermediate instructions are push-family (`Push`, `PushLiteral`,
   `PushGlobal`, `PushLocal`, `PushCachedBinding`)
5. A `claimed` map tracks which `PullApply` indices have been consumed,
   preventing the same instruction from matching both non-tail and tail
   patterns

### Promoted Primitive Specialization

When the foreign closure is a promoted primitive (`eq?`, `car`, `+`,
etc.) with matching arity, Pass 2 applies a stronger rewrite:

**Non-tail promoted:**
- **Delete** both `SaveContinuation` AND `PushCachedBinding`
- **Replace** `PullApply` with the promoted opcode (e.g., `OpEqQ`)

**Tail promoted:**
- **Delete** `PushCachedBinding`
- **Replace** `PullApply` with the promoted tail opcode (e.g., `OpEqQTail`)

Promoted ops eliminate even more overhead: they use fixed `Pop(arity)`
instead of `Drain()`, skip arity checking (arity was verified at compile
time), and execute inlined Go logic directly in the `Run()` switch — no
indirect function call, no env frame allocation.

## Pass 3: Generic Call Fusion

Handles callables that are not foreign closures — `MachineClosure`s
loaded via `PushLocal` (let-bound lambdas) or `PushCachedBinding`
(top-level non-foreign bindings).

| Callee Push | Fused Opcode |
|-------------|--------------|
| `PushLocal(n)` | `CallLocal(n)` |
| `PushCachedBinding(n)` | `CallCachedBinding(n)` |

Same non-tail and tail patterns as Pass 2, but without promoted
specialization and without `ForeignClosure` type checking. Pass 3 only
matches `PullApply` instructions not already claimed by Pass 2.

## Pass 4: Promoted Tail Calls With Compound Arguments

Pass 2's tail pattern requires every instruction between the callee push
and `PullApply` to be push-family, so a promoted call whose arguments are
themselves calls (fib's tail `(+ (fib ...) (fib ...))`) never matches.
`fusePromotedCompoundArgs` walks the argument region with `walkCallArgs`
instead, counts the arguments, and rewrites the tail `PullApply` to the
promoted tail opcode when the count equals the promoted arity.

The safety gate is a preceding `OpReleaseEnvFrame`: codegen emits it only
where it has proved the env frame dead (no capture, no escaping closure,
only capture-safe callees). Without that proof an argument may capture a
continuation that re-enters needing the frame's locals, which the inline
tail op cannot preserve: it pops the eval stack and returns, abandoning
the frame. This is what separates fib's tail `+` (proof present)
from a tail `cons` inside `map`, whose unknown callback may call `call/cc`
(no proof); the latter stays on the generic apply path. Pass 4 is
tail-only: the non-tail case would also need its outer
`SaveContinuation` removed.

## The EditPlan Abstraction

All three passes use `EditPlan` to accumulate edits and apply them
atomically. This separation is critical: pattern matching runs against
the original bytecode positions, while the actual code rewrite happens in
a single pass that handles compaction and offset fixup together.

### API

```go
plan := NewEditPlan(tpl)
plan.Delete(start, end)                         // mark [start, end) for removal
plan.Replace(start, end, instrs, sourceRef)     // replace [start, end) with instrs
plan.Insert(at, instrs, sourceRef)              // insert before position at
plan.Apply()                                    // apply all edits atomically
```

`Delete` is sugar for `Replace` with nil instructions. `Insert` is sugar
for `Replace` with `start == end`.

### Apply Algorithm

```
Apply()
 1. Sort edits by start position
 2. Validate: no overlaps, all within [0, codeLen]
 3. buildEditRemap: old PC → new PC mapping
 4. fixSurvivingBranches: adjust Arg for Branch/BranchOnFalse/SaveCont
 5. rewriteCode: splice surviving segments + replacements
 6. gcSideTable: remove unreferenced OpComplex entries, remap Arg values
```

### Branch Offset Fixup

Branch instructions (`Branch`, `BranchOnFalseValue`, `SaveContinuation`)
encode their target as a relative offset in `Arg`: the absolute target is
`pc + Arg`. When instructions are removed or inserted, both the source
position and the target position shift. The fixup algorithm:

1. `buildEditRemap` constructs `remap[]` where `remap[oldPos]` is the
   new position. Positions inside a replaced range `[start, end)` all
   map to the replacement's start — a branch into the middle of a
   deleted sequence lands at whatever replaced it.

2. `fixSurvivingBranches` iterates original instructions that are NOT
   inside any edit range. For each branch instruction:
   ```
   absTarget = oldPos + Arg
   newArg = remap[absTarget] - remap[oldPos]
   ```
   Replacement instructions are left as-is — the optimizer emits them
   with correct offsets (typically inheriting the source instruction's
   `Arg`).

### SideTable Garbage Collection

`OpComplex` instructions index into `NativeTemplate.sideTable` via
`Arg`. When edits delete `OpComplex` instructions, sideTable entries
become unreferenced. `gcSideTable` marks referenced entries, builds an
old→new index mapping, remaps all surviving `OpComplex.Arg` values, and
compacts the slice in place.

## Promoted Opcodes

Eighteen primitives have dedicated opcodes inlined directly in the
`Run()` switch loop. Each has a non-tail and tail variant (36 opcodes
total):

| Category | Primitives |
|----------|-----------|
| Predicates | `null?`, `pair?`, `eq?`, `vector?` |
| Accessors | `car`, `cdr`, `vector-ref` |
| Arithmetic | `+`, `-`, `*`, `/` |
| Comparison | `<`, `<=`, `>`, `>=`, `=` |
| Constructor | `cons` |
| Mutator | `set-cdr!` |

Each is one `promotedOp` descriptor (identity, name, arity, opcode pair,
inline Go implementation), and the `promotedOps` list of descriptors is
what makes the optimizer aware of it. The identity is a
`*machine.PrimitiveIdentity` minted beside the descriptors and declared
on the primitive's `registry.PrimitiveSpec`, so every `*ForeignClosure`
the registry builds from that spec carries it.
`promotedOpForIdentity` indexes the list by that token. Passes 2 and 4
check this mapping when fusing foreign call sequences.

Matching is by identity, not by the closure's name: an embedder's own
procedure may be named `cons` without being the registered `cons`, and a
narrowed surface may re-populate `+` with a replacement that must not be
inlined.

### Runtime Fallback

Promoted opcodes record the original `cachedBindings` index in their
`Arg`. At runtime, `execPromoted` verifies that the binding still holds
the expected `*ForeignClosure` with the expected primitive identity. If
the binding was reassigned via `set!` (e.g., `(set! eq? car)`),
`callPromotedFallback` takes over:

1. Pop arguments using `PopN(arity)` (not `Drain()` — the eval stack
   may contain outer arguments from a containing call)
2. For non-tail: manually call `SaveContinuation(1)` to create the
   stack frame that the optimizer deleted
3. Call `ApplyCallable` with the replacement callable
4. For non-tail: restore the continuation after the call

This fallback is invisible to user code — the promoted opcode silently
degrades to a generic call.

## The `savedCont` Invariant

### The Bug

Both `applyForeign` and `callForeignCached` shared a pattern:

```go
savedTemplate := mc.template
err = fcls.fn(mc)
if mc.template != savedTemplate { return mc, nil }
mc.RestoreAndRelease(mc.cont)  // ← unconditional restore
```

The template check catches the case where a foreign function calls
`mc.Apply()` on a `MachineClosure` (changing the template). But it
misses the case where a foreign function calls `mc.ApplyCallable()` with
a `*ForeignClosure`, which triggers a **nested** `applyForeign` that
consumes the `SaveContinuation` frame via its own `RestoreAndRelease`.
The outer caller then double-restores from the wrong frame.

### Concrete Failure

```scheme
(list (call-with-current-continuation procedure?))
;; Expected: (#t)
;; Got: #t (the list call was skipped entirely)
```

Trace: `callForeignCached` calls PrimCallCC. PrimCallCC enters inline
mode (`mc.cont != nil`). It calls `mc.ApplyCallable(procedure?, k)`.
`procedure?` is a `*ForeignClosure`, so `applyForeign` runs
`PrimProcedureQ`, then restores from `mc.cont` — consuming the
`SaveContinuation` frame. Back in `callForeignCached`, `mc.template`
matches `savedTemplate` (it was restored to the same value). The
unconditional `RestoreAndRelease(mc.cont)` now pops the **next** frame
(the `list` call's `SaveContinuation`), skipping the `list` call.

### The Fix

Save `mc.cont` before calling the foreign function. After the call, only
restore if the continuation is still the expected frame:

```go
savedTemplate := mc.template
savedCont := mc.cont
err = fcls.fn(mc)
if mc.template != savedTemplate { return mc, nil }
if mc.cont == savedCont {
    mc.RestoreAndRelease(mc.cont)
}
```

The pointer-identity check covers all cases:

| Scenario | `mc.cont` after | Match? | Action |
|----------|----------------|--------|--------|
| Normal foreign function | Unchanged | Yes | Restore |
| PrimCallCC + MachineClosure | Unchanged | Yes | But template check returns early |
| PrimCallCC + ForeignClosure | Consumed (advanced) | No | Skip restore |

This fix applies to both `applyForeign` (the unfused path, a
pre-existing bug) and `callForeignCached` (the fused path).

## Invariants and Constraints

**Semantic equivalence.** Fused instructions must produce identical
observable behavior to their unfused sequences. The `Run()` loop
implementations of fused opcodes are the enforcement point.

**Idempotency.** A second `Optimize()` call finds no patterns to match
and returns without modification.

**Branch target safety.** Fusion is forbidden when the second instruction
in a pair is a branch target. This prevents binding a convergence point
to only one incoming path.

**Source attribution.** Fused instructions inherit the source reference
from the semantically meaningful instruction (the `Load`, not the
`Push`; the `Pull`, not the `Apply`). This preserves source-map accuracy
for error reporting.

**Non-overlapping edits.** `EditPlan` validates that no two edits
overlap. This is guaranteed by construction — each pass scans linearly
and skips matched instructions.

**Sub-template recursion.** `optimizeSubTemplates` walks the literals
pool and recursively optimizes any `*NativeTemplate` values (compiled
lambda bodies). Non-template literals are skipped.

## Code Locations

| Component | File |
|-----------|------|
| `Optimize()` entry point | `machine/peephole.go` |
| `EditPlan` abstraction | `machine/edit_plan.go` |
| `Instruction` type | `machine/instruction.go` |
| Opcode constants + metadata | `machine/opcode.go` |
| `NativeTemplate` | `machine/native_template.go` |
| Promoted opcode execution | `machine/call_promoted.go` |
| Promoted arithmetic | `machine/call_promoted_arithmetic.go` |
| `callForeignCached` (fused runtime) | `machine/call_foreign_cached.go` |
| `applyForeign` (unfused runtime) | `machine/machine_context_apply.go` |
| `promotedOpForIdentity` mapping | `machine/call_promoted.go` |
| Peephole tests | `machine/peephole_test.go` |
| Opcode fusion integration tests | `wile/opcode_fusion_test.go` |
| call/cc regression tests | `wile/callcc_engine_test.go` |

## References

- Ertl, M. A. & Gregg, D. (2003). "The Structure and Performance of
  Efficient Interpreters." *Journal of Instruction-Level Parallelism*, 5.
  (Superinstruction formation theory.)
- See `BIBLIOGRAPHY.md` at project root.
