# ArrayList-as-Pair Refactoring Plan

Date: 2026-02-22
Status: Abandoned (superseded by BLOCK_ALLOCATED_PAIRS.md)
Prerequisite: None (independent of other allocation optimizations)

## Problem Statement

Every variadic foreign closure call allocates cons cells via `values.List(vs[l-1:]...)` to pack rest arguments. These cons cells account for 49.6% of all remaining allocations in fib(10) after the noCopyApply, 2-arg fast path, and Pull fixes. The cons cells are created in `Apply`, consumed immediately by the primitive, and discarded.

For `noCopyApply` closures, the rest-arg binding is overwritten on every call. Using `*ArrayList` instead of a `*Pair` chain eliminates per-call cons allocation — the same `*ArrayList` can be reused by overwriting its elements.

The blocker: `*ArrayList` does not satisfy `pair?`, so `(pair? (list 1))` returns `#f`. Making `*ArrayList` a valid pair representation requires changes across the runtime.

## Design

### Core Invariant

`*ArrayList` with `len >= 2` is a pair. `*ArrayList` with `len == 1 && (*al)[0] == EmptyList` is the empty list. This extends the existing `*Pair` representation — both types satisfy `pair?` when appropriate.

### ArrayList Conventions (existing)

```
Proper list (1 2 3):      [1, 2, 3, EmptyList]   len=4, pair, list
Single pair (1 . 2):      [1, 2]                  len=2, pair, not list
Empty list ():            [EmptyList]              len=1, not pair
```

- `Car()` = `(*al)[0]`
- `Cdr()` for len > 2: returns `&(*al)[1:]` (sub-slice ArrayList)
- `Cdr()` for len == 2: returns `(*al)[1]` directly (terminator or improper cdr)
- `Cdr()` for len <= 1: returns `EmptyList`
- `IsPair()` = `len >= 2` (new method)
- `IsList()` = last element is EmptyList (existing)

### What Changes

#### Phase 1: ArrayList methods (values/)

| Change | File | Complexity |
|--------|------|------------|
| Add `IsPair() bool` | `array_list.go` | Trivial |
| Add `SetCar(v Value)` | `array_list.go` | Trivial — `(*al)[0] = v` |
| Verify `Car()`, `Cdr()`, `ForEach()`, `IsEmptyList()`, `Length()` are correct | `array_list.go` | Audit + tests |
| Add `tupleEqualToDeep` for cross-type `*Pair` vs `*ArrayList` comparison | `utils.go` | Moderate |
| Update `equalToDeep` to handle `*Pair` vs `*ArrayList` via `tupleEqualToDeep` | `utils.go` | Moderate |

#### Phase 2: Predicates and mutation (registry/core/)

| Change | File | Complexity |
|--------|------|------------|
| `pair?` — recognize `*ArrayList` with `IsPair()` | `prim_predicates.go:107` | Small |
| `set-car!` — handle `*ArrayList` via `SetCar` | `prim_pairs.go:65` | Small |
| `set-cdr!` — handle `*ArrayList` by truncating to `[car, newCdr]` | `prim_pairs.go:77` | Small |
| `list-set!` — handle `*ArrayList` (index into slice directly) | `prim_lists.go:247` | Small |
| `IsPair` in embedding API | `value_helpers.go:32` | Small |

#### Phase 3: Apply optimization (machine/)

| Change | File | Complexity |
|--------|------|------------|
| Use `*ArrayList` for noCopyApply variadic rest args | `machine_context.go:375` | Small |
| Reuse existing `*ArrayList` by overwriting elements when size matches | `machine_context.go:375` | Small |

#### Phase 4: Verify no regressions

Run full test suite. The following should NOT change:

- **Compiler/expander code** — uses `*Pair` only (per CLAUDE.md). Not affected because Apply only uses ArrayList for foreign closures (noCopyApply=true), and compile-time code constructs lists via `NewCons`, not Apply.
- **SchemeWriter** — uses `*Pair` pointer identity for cycle tracking. Not affected because SchemeWriter operates on user-constructed data (cons cells), not rest-arg lists.
- **Macro pattern matching** (`internal/match/`) — operates on syntax objects backed by `*Pair`. Not affected.

## Sites That Need Changes (complete inventory)

### Runtime sites (MUST change)

| File:Line | Function | Current | Change to |
|-----------|----------|---------|-----------|
| `registry/core/prim_predicates.go:109` | `PrimPairQ` | `o.(*values.Pair)` | Type switch: `*Pair` → true, `*ArrayList` → `IsPair()` |
| `registry/core/prim_pairs.go:66` | `PrimSetCar` | `RequireArg[*Pair]` | Type switch: `*Pair` → `SetCar`, `*ArrayList` → `SetCar` |
| `registry/core/prim_pairs.go:78` | `PrimSetCdr` | `RequireArg[*Pair]` | Type switch: `*Pair` → `SetCdr`, `*ArrayList` → truncate to `[car, newCdr]` |
| `registry/core/prim_lists.go:248` | `PrimListSet` | `RequireArg[*Pair]` | Type switch: `*Pair` → existing, `*ArrayList` → index directly `(*al)[k] = val` |
| `values/utils.go:124` | `equalToDeep` | `*Pair` rejects `*ArrayList` | Cross-type comparison via `tupleEqualToDeep` |
| `value_helpers.go:32` | `IsPair` | `o.(*values.Pair)` | Also check `*ArrayList.IsPair()` |
| `machine/machine_context.go:375` | `Apply` variadic | `values.List(...)` | `*ArrayList` for noCopyApply closures |

### Compile-time sites (DO NOT change)

| File:Line | Function | Why unchanged |
|-----------|----------|---------------|
| `machine/compile_time_continuation.go:341` | `internSymbolsInValue` | Compile-time — `*Pair` only |
| `machine/native_template.go:430` | `DeduplicateLiteral` | Compile-time — `*Pair` only |
| `internal/match/syntax_adapter.go:486` | `capturedValueToSyntax` | Macro expander — `*Pair` only |
| `internal/syntax/syntax_value.go:133` | `SyntaxObject.IsPair()` | Compile-time predicate |

## set-cdr! Semantics on ArrayList

`set-cdr!` on `*ArrayList` truncates to `[car, newCdr]`:
```go
*p = values.ArrayList{p.Car(), val}
```

This mutates the ArrayList in place. The caller's reference to `*ArrayList` still points to the same object. Sub-slices returned by prior `Cdr()` calls become stale, but this is the same as `set-cdr!` on `*Pair` — mutating cdr invalidates prior cdr references.

R7RS does not guarantee that `set-cdr!` preserves prior cdr references. The mutation is observable only through the original pair, which is correct.

## list-set! Semantics on ArrayList

`list-set!` on `*ArrayList` indexes directly:
```go
(*al)[k] = val
```

O(1) instead of O(k) traversal. Correct because ArrayList elements are contiguous and the index maps directly to the slice position (elements are stored in order, with EmptyList terminator at the end, so element k is at index k).

## Expected Impact

Based on pre-optimization profiling:
- `values.NewCons` is 49.6% of remaining allocations (662 per fib(10) iteration)
- For ParamCount=2 primitives (<=, -, /, =, <, >, >=, min, max): 1 cons cell → 0 (ArrayList reuse)
- For ParamCount=1 primitives (+, *): 2 cons cells → 1 ArrayList allocation (first call only, reused thereafter)
- Estimated allocation reduction: ~50% of remaining allocs
- Combined with prior fixes: potentially **>50% total** vs original master

## Risks

1. **ArrayList correctness** — ArrayList's Cdr() behavior, ForEach, Length, IsEmptyList must all be verified against test cases that cover the terminator convention. If ArrayList has bugs, they'll surface as subtle runtime errors.

2. **Cross-type equal?** — Comparing `*Pair` and `*ArrayList` element-wise is new. Must handle cycles (though ArrayList can't be circular, a Pair can — the comparison must terminate).

3. **set-cdr! truncation** — Truncating ArrayList on `set-cdr!` changes its length. Code that holds sub-slice references from prior `Cdr()` calls sees stale data. This is consistent with `*Pair` mutation semantics but should be tested.

4. **Hidden *Pair assumptions** — Other code (CxR accessors, append, reverse, etc.) may use `Tuple` but assume `Cdr()` returns `*Pair` or EmptyList, not `*ArrayList`. Need to verify all Tuple consumers handle `*ArrayList` sub-slices.

## Execution Order

1. Audit and test ArrayList methods (Car, Cdr, ForEach, Length, IsEmptyList, IsList) exhaustively
2. Add `IsPair()`, `SetCar()` to ArrayList
3. Add `tupleEqualToDeep` and update `equalToDeep`
4. Update `pair?`, `set-car!`, `set-cdr!`, `list-set!`, `IsPair` embedding API
5. Update Apply to use ArrayList for noCopyApply variadic rest args
6. Run full test suite + Gabriel benchmarks + ZebraPuzzle
7. Profile to measure actual allocation reduction
