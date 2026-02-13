# EmptyList and Void Type Refactoring Plan

**Status:** Design ready, not implemented

## Problem

`EmptyList = NewCons(nil, nil)` — a `*Pair` singleton. Creates type confusion:
- `case *values.Pair` matches EmptyList → requires `IsEmptyList()` guards in ~13 type switches
- `pair?` needs defensive check (must return `#f` for `'()`)
- `(*Pair)(nil).IsVoid() == true` conflates void with nil-pointer-of-pair-type
- ~160+ `IsEmptyList()` call sites, many purely defensive
- Same pattern in `SyntaxEmptyList` (~138 call sites) and `ArrayListEmptyList`

**FIXME comments**: `pair.go:28`, `pair.go:201`, `array_list.go:27`, `syntax_pair.go:30`

## Options

| Option | Scope | Key Change | Risk |
|--------|-------|------------|------|
| **A (Recommended)** | ~30-40 files | `emptyListType struct{}` implementing `Value` + `Tuple` (like `voidType`/`eofType`) | Medium |
| B | ~70-80 files | Option A + remove `IsVoid()` from `Value` interface (eliminates 46 boilerplate methods) | Medium-High |
| C | ~100+ files | Option B + restructure ArrayList, minimize Tuple interface | High |

**Recommendation**: Implement Option A first as single PR. Compiler catches most breakage.

## Implementation Steps (Option A)

1. Create `emptyListType` in `values/empty_list.go` — implements `Value` and `Tuple`
2. Update `Pair` — remove EmptyList var, simplify methods
3. Update `ArrayList` — remove `ArrayListEmptyList` singleton
4. Simplify utility functions (`IsEmptyList`, `IsList`)
5. Simplify predicates (`PrimPairQ` no longer needs EmptyList guard)
6. Create parallel `syntaxEmptyListType` in syntax layer
7. Fix ~13 type switches (remove EmptyList guards)
8. Remove 4 FIXME comments

## Critical Files

| File | Change |
|------|--------|
| `values/empty_list.go` | **New** — `emptyListType` |
| `values/pair.go` | Remove EmptyList var, update methods |
| `values/utils.go` | Simplify `IsEmptyList()`, `IsList()` |
| `values/array_list.go` | Remove `ArrayListEmptyList` |
| `syntax/syntax_pair.go` | Create `syntaxEmptyListType` |
| `registry/core/prim_predicates.go` | Simplify `PrimPairQ` |
| ~13 files with `case *Pair` switches | Remove EmptyList guards |
