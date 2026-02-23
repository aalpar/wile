# Block-Allocated Pairs Plan

Date: 2026-02-22
Status: Implemented
Prerequisite: None (independent of other allocation optimizations)
Supersedes: ArrayList-as-Pair approach (abandoned — mutation and aliasing issues)

## Problem Statement

Every variadic function call allocates N cons cells via `values.List(vs[l-1:]...)` to pack rest arguments. These cons cells account for 49.6% of all remaining allocations (662 per fib(10) iteration) after the noCopyApply, 2-arg fast path, and Pull fixes.

`values.List()` calls `NewCons` N times for an N-element list. Each `NewCons` allocates a separate `[2]Value` (32 bytes) on the heap. N heap objects means N entries in the GC's object graph.

## Design

### Core Idea

Allocate all N pairs for a list in a single `make([]Pair, N)` call, then link them into a cons chain. Each `&block[i]` is a valid `*Pair`. The result is semantically identical to individually-allocated cons cells — same type, same pointer identity per cell, same mutation behavior.

```
Individual allocation (current):

  NewCons → heap obj A    Pair{1, →}
  NewCons → heap obj B    Pair{2, →}
  NewCons → heap obj C    Pair{3, EmptyList}
  3 heap objects, 3 GC entries

Block allocation (proposed):

  make([]Pair, 3) → heap obj X    [Pair{1, →}, Pair{2, →}, Pair{3, EmptyList}]
                                   &X[0]       &X[1]       &X[2]
  1 heap object, 1 GC entry
  Each &X[i] is a valid *Pair with stable address (Go GC is non-moving)
```

### Why This Preserves Semantics

Block-allocated pairs produce real `*Pair` pointers:

| Operation | Individual alloc | Block alloc | Same? |
|-----------|-----------------|-------------|-------|
| `(pair? x)` | `*Pair` → true | `*Pair` → true | Yes |
| `(cdr x)` | returns `*Pair` (next cell) | returns `*Pair` (`&block[i+1]`) | Yes |
| `(set-cdr! x val)` | modifies `x[1]`, visible to parent | modifies `x[1]`, visible to parent | Yes |
| `(set-cdr! (cdr x) val)` | modifies parent's cdr cell | modifies parent's cdr cell (same pointer) | Yes |
| Circular via `set-cdr!` | works | works | Yes |
| `equal?` | pointer identity + structural | same | Yes |
| SchemeWriter cycle detection | pointer identity on `*Pair` | same pointers | Yes |
| Compile-time code (`*Pair` only) | `*Pair` | `*Pair` | Yes |

### What Changes

Modify `values.List()` to block-allocate internally. No API change — same signature, same return type, same semantics. All call sites benefit automatically.

```go
func List(os ...Value) Tuple {
    n := len(os)
    if n == 0 {
        return EmptyList
    }
    block := make([]Pair, n)
    for i := 0; i < n-1; i++ {
        block[i][0] = os[i]
        block[i][1] = &block[i+1]
    }
    block[n-1][0] = os[n-1]
    block[n-1][1] = EmptyList
    return &block[0]
}
```

### Files Changed

| File | Change | Complexity |
|------|--------|------------|
| `values/utils.go` | Rewrite `List()` to block-allocate | Small |
| `values/utils_test.go` | Add block-allocation-specific tests | Small |

That's it. No other files need changes.

## Expected Impact

- **Allocation objects**: N cons cells → 1 `[]Pair` block per `List()` call
- **For fib(10)**: 662 cons allocations/iter → estimated ~200 block allocations/iter (each block replaces 1-3 cons cells)
- **GC pressure**: fewer objects in the GC graph → less mark/sweep work
- **Cache locality**: consecutive pairs are contiguous in memory → better L1/L2 cache behavior during list traversal

The impact on the 49.6% allocation share depends on the rest-arg size distribution. For the common 1-element rest arg (e.g., `(<= n 1)`), the block is `make([]Pair, 1)` — same allocation count but slightly less overhead (no per-object GC header for the second cell). For 2+ element rest args, the savings scale linearly.

## Risks

1. **GC can't partially free a block.** If `set-cdr!` orphans most of a block-allocated list, the entire `[]Pair` allocation stays alive as long as any `&block[i]` is reachable. For short-lived rest args (the common case), this doesn't matter — the whole block becomes garbage together. For long-lived lists modified by `set-cdr!`, this could retain more memory than individually-allocated cells. In practice, rest-arg lists are consumed and discarded, not mutated.

2. **Single-element blocks have no savings.** `make([]Pair, 1)` allocates one Pair, same as `NewCons`. The overhead of creating the slice header (24 bytes: ptr + len + cap) may slightly increase allocation size for the 1-element case. Benchmark will reveal whether a special case for N=1 is worth keeping.

## Future Extensions

### Block Reuse for noCopyApply (deferred)

For noCopyApply variadic closures, the rest-arg list is rebuilt on every call. If the block size matches, we could overwrite the existing block's car values instead of allocating a new one. This would make repeat calls zero-allocation for rest args.

**Prerequisite**: Prove that the `*Pair` pointers from the block don't escape the closure's body. For foreign closures (Go primitives), this is true — they access args via `mc.Arg(i)`, not by holding `*Pair` references. For Scheme closures, it depends on the body.

**Risk**: Same aliasing hazard as the ArrayList reuse attempt. If a `*Pair` from the old block is passed as a new rest arg, overwriting the block creates a self-referential structure. Defer until escape analysis (static or dynamic) can prove safety.

### Pooling Blocks by Size (deferred)

Pool `[]Pair` blocks by common sizes (1, 2, 3) to avoid even the single allocation. But sync.Pool overhead is already 17.3-21.9% of CPU — adding more pools may be net-negative. Benchmark pool-free block allocation first.

## Execution Order

1. Rewrite `values.List()` to block-allocate
2. Run full test suite
3. Run Gabriel benchmarks + ZebraPuzzle
4. Profile to measure actual allocation reduction
5. Decide whether 1-element special case (`NewCons` directly) is worth keeping
