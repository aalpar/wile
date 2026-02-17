# Phase 4: Expansion Allocation Optimization

**Status:** Sub-phases 4.1, 4.2, and 4.3 complete. Sub-phase 4.4 remaining.

## What Was Implemented (4.1 + 4.2)

### 4.1: Structural Sharing in `mapSyntaxTree`

**File:** `internal/syntax/scope_utils.go`, function `mapSyntaxTree`

The pair branch of `mapSyntaxTree` now checks pointer identity of car/cdr after recursive transformation. If both are unchanged, the original `*SyntaxPair` is returned instead of allocating a new one via `NewSyntaxCons`.

```go
// Structural sharing: if children are unchanged, return original pair.
// Only symbols accumulate scopes, so most pairs pass this check.
if newCar == s.Values[0] && newCdr == s.Values[1] {
    return s
}
return NewSyntaxCons(newCar, newCdr, s.SourceContext())
```

This works because syntax objects are immutable -- all scope operations return new objects, so pointer identity implies value identity. The identity check cascades bottom-up: an unchanged inner pair causes the outer pair's cdr check to succeed too, propagating structural sharing up the tree.

The vector branch already had this optimization (checking a `changed` flag across elements). This extends the same principle to pairs.

`mapSyntaxTree` is the shared traversal used by both `AddScope` (called via `SyntaxPair.AddScope` -> `mapSyntaxTree`) and `FlipScope`. Both benefit from this change.

### 4.2: Structural Sharing in `addScopeToPairSkipFreeIds`

**File:** `machine/operation_syntax_rules_transform.go`, function `addScopeToPairSkipFreeIds`

Same identity check pattern applied to the free-ID-skipping traversal used during syntax-rules template expansion.

```go
// Structural sharing: if children are unchanged, return original pair.
// Free IDs are returned unchanged, so pairs containing only free IDs
// and unchanged subtrees avoid allocation entirely.
if newCar == pair.SyntaxCar() && newCdr == pair.SyntaxCdr() {
    return pair
}
return syntax.NewSyntaxCons(newCar, newCdr, pair.SourceContext())
```

Free identifiers are returned unchanged by design (they skip the intro scope), so pairs containing only free IDs and unchanged subtrees avoid allocation entirely. The comparison uses `pair.SyntaxCar()` / `pair.SyntaxCdr()` (which are the typed `SyntaxValue` accessors to `Values[0]`/`Values[1]`).

## Design Rationale

**Pointer identity is the correct check (not deep equality)** because syntax objects are immutable. All scope operations (`AddScope`, `FlipScope`) return new objects when they modify anything, and return the original object when they don't. So `==` on interface values is both O(1) and semantically correct.

**The optimization is a strict subset of behavior.** If children are unchanged, the parent pair carries no new information either -- same car, same cdr, same source context. Returning the original avoids allocating an identical copy.

**No interaction with FlipScope.** `mapSyntaxTree` is shared by both `AddScope` and `FlipScope`, so both benefit from the 4.1 change automatically.

**No interaction with ER macros.** ER macros don't use `addScopeToSyntaxSkipFreeIds` (they handle hygiene via rename/compare), so 4.2 has no effect on ER macro expansion.

## Design Decisions

**Pointer identity over deep equality.** O(1) vs O(n), and semantically correct for immutable objects. Go's `==` on interface values compares the concrete type and pointer, which is exactly what we need.

**Applied to both traversal sites independently.** The two functions (`mapSyntaxTree` and `addScopeToPairSkipFreeIds`) have different signatures and different skip logic, so a shared abstraction would add complexity without clarity. The pattern is three lines of code at each site.

**Tests verify pointer identity (not just value equality).** The tests use `qt.Equals` which checks `==` on interface values, confirming the optimization returns the exact same object rather than an equal copy.

## Impact Estimates

- ~50-70% fewer pair allocations per `AddScope` call on typical syntax trees (most nodes are not symbols -- they are numbers, strings, booleans, nested pairs of those).
- For a 50-node tree with 15 symbols: drops from ~35 pair allocs to ~10-15 (only pairs on the path to a changed symbol are reallocated).
- Macros with many free IDs (e.g., `cond` with `else`, `=>`, `if`, `begin`) see 60-80% fewer pair allocs in template expansion via `addScopeToPairSkipFreeIds`.

## Remaining Sub-Phases

### 4.3: `SourceContext.WithScope` Idempotency

Early return from `WithScope` if the scope is already present in the scope set. Independent of 4.1/4.2. Would reduce allocation further for cases where the same scope is added multiple times (e.g., re-expansion).

### 4.4: Expander MachineContext Pooling

Pool `MachineContext` instances for macro invocation lifecycle. Each macro invocation currently allocates a fresh sub-context. Independent of 4.1-4.3, but should be measured after those are in place to establish the remaining allocation baseline.

## Verification

- `make test` -- all pass
- `make lint` -- clean
- `go test -run TestHygiene ./machine/...` -- pass
- `make bench-schelog` -- no regression
- New tests in `internal/syntax/coverage_test.go`: `TestMapSyntaxTreeStructuralSharing` with 5 sub-tests:
  - `pair of non-symbols returned unchanged` -- pair of `SyntaxObject` values returns same pointer
  - `pair with symbol child is reallocated` -- pair with symbol car gets new pair, but unchanged cdr propagates
  - `nested pairs share unchanged subtrees` -- `(1 . (2 . 3))` returns same pointer for entire tree
  - `vector structural sharing` -- vector of non-symbols returns same pointer (pre-existing optimization, regression test)
  - `FlipScope structural sharing on non-symbol tree` -- FlipScope also benefits from the pair identity check
