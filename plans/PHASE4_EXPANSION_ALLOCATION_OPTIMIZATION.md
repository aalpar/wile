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

### 4.4: Expander MachineContext Pooling

**Files:** `machine/pool.go`, `machine/expander_time_continuation.go`, `machine/pool_test.go`

The two macro expansion call sites (`expandMacroInvocation` and `ExpandOnce`) previously allocated a fresh `MachineContext` via `NewMachineContextFromMachineClosure`, which internally created an intermediate `MachineContinuation` (with a `Stack`) that was immediately unpacked and discarded. This created 3 allocations per macro invocation: `MachineContext` + `MachineContinuation` + `Stack`.

New `acquireMacroContext(ctx, cls)` pulls a zeroed `MachineContext` from the existing `subContextPool` and a `Stack` from `stackPool`, sets only the 4 fields needed (`ctx`, `env`, `template`, `evals`), and eliminates the intermediate `MachineContinuation` entirely. Callers use `defer ReleaseSubContext(mc)` for cleanup.

```go
func acquireMacroContext(ctx context.Context, cls *MachineClosure) *MachineContext {
    mc := acquireSubContext()
    mc.ctx = ctx
    mc.env = cls.env
    mc.template = cls.template
    mc.evals = acquireStack()
    return mc
}
```

Safe because: (1) transformer runs in isolation (single `Apply` + `Run`, result extracted, released), (2) result is a syntax tree with no MachineContext references, (3) `defer` handles all error paths, (4) uses the same pool infrastructure proven in Phase 2.

The dead nil checks at both call sites were removed — neither `NewMachineContextFromMachineClosure` nor pool acquisition ever returned nil.

`NewMachineContextFromMachineClosure` is retained (used in tests, may be useful for non-pooled top-level contexts).

## Remaining Sub-Phases

### 4.3: `SourceContext.WithScope` Idempotency ✅

**Files:** `internal/syntax/source_context.go`, `internal/syntax/syntax_symbol.go`

Made `WithScope` and `WithScopes` idempotent: they return the receiver unchanged when the scope(s) are already present. Propagated this through `SyntaxSymbol.AddScope`, which now checks pointer identity on the returned `*SourceContext` and returns `p` when unchanged.

This is the missing link that lets 4.1/4.2's structural sharing cascade from leaves to the root. Previously, `WithScope` always allocated a new `SourceContext` + scope slice, so `SyntaxSymbol.AddScope` always returned a new symbol, which prevented `mapSyntaxTree`'s identity check from succeeding on any pair containing symbols.

**Changes:**
- `WithScope`: `slices.Contains` check before allocation — returns `p` if scope already present
- `WithScopes`: loop checking all scopes present — returns `p` if all are
- `SyntaxSymbol.AddScope`: compares `WithScope` result pointer; returns `p` if unchanged

**Tests** (in `internal/syntax/coverage_test.go`):
- `TestWithScope_Idempotent` — same pointer on duplicate scope, new pointer on novel scope
- `TestWithScopes_Idempotent` — all-present, subset, and mixed cases
- `TestSyntaxSymbol_AddScope_Idempotent` — symbol pointer identity
- `TestSyntaxSymbol_AddScope_Idempotent_PreservesResolvedBinding` — idempotency with ResolvedBinding set
- `TestStructuralSharing_CascadesFromIdempotentSymbols` — pair of pre-scoped symbols returns same pointer
- `TestStructuralSharing_CascadesNestedPairs` — nested pairs cascade
- `TestStructuralSharing_PartialChange` — mixed: unchanged car reused, changed cdr allocated

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
