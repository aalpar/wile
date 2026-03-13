# ForeignFunction Call Ceremony Reduction

**Goal:** Reduce per-call overhead for peephole-optimized ForeignFunction calls by encoding compile-time-known arity in the instruction and eliminating redundant runtime checks.

**Depends on:** Nothing. Independent of operator promotion/demotion (which should be evaluated after this lands).

**Branch:** `perf/ff-ceremony-reduction`

---

## Problem

`callForeignCached` repeats work the peephole optimizer already did. The peephole inspects the `*ForeignClosure` to decide whether to emit `OpCallForeignCached` — it knows `paramCount`, `isVariadic`, and `argCount`. But the runtime re-validates all three via `checkArity` and branches on `isVariadic` in `bindArgs`. The template change check guards against something no ForeignClosure does. These redundancies cost ~30-50ns per call, and every primitive call pays them.

## Design

### Instruction Encoding

Pack `paramCount` into `Arg` alongside the existing `cachedBindings` index:

```
int32 Arg:
  bits  0-15: cachedBindings index  (0..65535)
  bits 16-23: paramCount            (0..255)
  bits 24-31: reserved
```

Helpers:

```go
func encodeForeignCallArg(bindingIdx int32, paramCount int) int32 {
    return bindingIdx | int32(paramCount)<<16
}

func decodeForeignCallArg(arg int32) (bindingIdx int32, paramCount int) {
    return arg & 0xFFFF, int(arg>>16) & 0xFF
}
```

### Opcode Split

Replace the isVariadic branch with separate opcodes:

| Current | New | Condition |
|---------|-----|-----------|
| `OpCallForeignCached` | `OpCallForeignCached` | Non-variadic (reused, new Arg encoding) |
| `OpCallForeignCachedTail` | `OpCallForeignCachedTail` | Non-variadic tail (reused, new Arg encoding) |
| — | `OpCallForeignCachedVar` | Variadic |
| — | `OpCallForeignCachedVarTail` | Variadic tail |

Net: 37 opcodes to 39. Minimal icache impact.

### Fast Path (non-variadic, non-tail)

```go
func callForeignCached(mc *MachineContext, instr Instruction, tail bool) (*MachineContext, error) {
    bindingIdx, paramCount := decodeForeignCallArg(instr.Arg)
    callable := mc.template.cachedBindings[bindingIdx].Value()

    fcls, ok := callable.(*ForeignClosure)
    if !ok || fcls.paramCount != paramCount {
        return callForeignCachedSlow(mc, callable)
    }

    vs := mc.evals.Drain()
    // counters ...

    env := fcls.env
    bnds := env.LocalEnvironment().Bindings()

    switch paramCount {
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
```

### What's Eliminated

| Removed | Why safe |
|---------|----------|
| `checkArity()` | `paramCount` comparison catches set!-to-different-arity. If paramCount matches and type assertion passed, arity is a compile-time guarantee. |
| `isVariadic` branch in `bindArgs` | Separate opcodes. Non-variadic handler contains no variadic code. |
| `bindArgs` function call | Inlined switch on encoded paramCount. Unrolled for arity 1-3. |
| Template change check | No ForeignClosure changes templates. |

### Slow Path

Two failure modes, one handler:

1. **Binding is no longer a `*ForeignClosure`** — set! replaced with a Scheme closure or other callable. Drain + ApplyCallable.
2. **`*ForeignClosure` with different `paramCount`** — set! replaced with a different-arity ForeignClosure. Full ceremony: checkArity, bindArgs with actual isVariadic.

The slow path lives in a separate function so the fast path stays small and inlineable.

### Peephole Changes

`fuseCallForeignCached` (peephole.go:208) changes at four sites (non-tail emit, tail emit, × 2 patterns):

```go
// Current:
plan.Replace(pullIdx, pullIdx+1,
    []Instruction{{Op: OpCallForeignCached, Arg: bindingIdx}}, ...)

// Proposed:
encodedArg := encodeForeignCallArg(bindingIdx, fcls.paramCount)
op := OpCallForeignCached
if fcls.isVariadic {
    op = OpCallForeignCachedVar
}
plan.Replace(pullIdx, pullIdx+1,
    []Instruction{{Op: op, Arg: encodedArg}}, ...)
```

Promoted opcodes (`eq?`, `vector?`, `vector-ref`) are unaffected — their check happens before the foreign-cached emission.

---

## Files Changed

| File | Change |
|------|--------|
| `opcode.go` | Add `OpCallForeignCachedVar`, `OpCallForeignCachedVarTail` + table entries |
| `instruction.go` | Add `encodeForeignCallArg`, `decodeForeignCallArg` |
| `call_foreign_cached.go` | Rewrite fast path, extract slow path to separate function, add variadic handler |
| `machine_context.go` | Add 4 switch cases in `Run()` for new opcodes |
| `peephole.go` | Emit encoded Arg, choose variadic vs non-variadic opcode |
| `native_template.go` | Add conversion cases for new opcodes |

## Testing

**Correctness:** Existing test suite covers the happy path (every primitive call routes through `callForeignCached`). Add targeted tests for set! edge cases:
- set! to different-arity ForeignClosure (paramCount mismatch → slow path)
- set! to non-ForeignClosure (type assertion → slow path)
- set! to same-arity ForeignClosure (paramCount matches, different fn — still correct)

**Measurement:** Run `make bench-gabriel` and the 6 list algorithm micro-benchmarks (`prim_bench_test.go`) before and after. Expected: 5-15% Gabriel improvement, narrower Scheme-vs-Go gap on list algorithms.

## Not In Scope

- **Operator promotion/demotion** — evaluate after this lands, when the baseline has changed.
- **Counter optimization** — follow-up (build-tag gating or batching).
- **Pointer-identity inline cache** — follow-up if measurement shows more is needed.
- **Scheme list algorithm re-migration** — re-attempt after per-call cost drops.
