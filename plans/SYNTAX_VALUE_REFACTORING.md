# Plan: Refactor Match Package to Pure SyntaxValue Operations

**Status**: Substantially complete — core refactoring done in commit `1c30313` (2026-01-27)
**Branch**: fix/scm-conformance-e (originally fix/scm-conformance-scopes)

## Overview

The match package was refactored from a two-layer architecture (raw `values.Value` matcher + `syntaxMap` bridge) to operate directly on `syntax.SyntaxValue`. Phases 1-3 (parallel syntax types, bytecode instruction refactoring, matcher unification) are complete. Phases 4-5 below are remaining cleanup.

## Remaining Work

### Phase 4: Unify Expansion (Medium Risk) - PARTIAL
- [ ] Remove `expandValue()`, keep only `expandSyntaxValue()` (renamed to `expand()`)
- [ ] Remove remaining `valueToSyntaxWithOrigin()` and `syntaxToValue()` functions

### Phase 5: Cleanup (Low Risk) - PENDING
- [ ] Remove deprecated functions
- [ ] Update documentation and CLAUDE.md files
- [ ] Update consumer files in machine package

**Note**: The core refactoring (Phases 1-3) was completed as part of the nested let-syntax hygiene fix. Phases 4-5 are cleanup/polish that can be done opportunistically.

## Key Implementation Details

### Symbol Comparison for Matching

Current `values.EqualTo()` uses pointer equality. For syntax:

```go
func syntaxSymbolsEqual(a, b *syntax.SyntaxSymbol) bool {
    return a.Sym.Key == b.Sym.Key  // Value equality, not pointer
}
```

### Empty List Handling

All checks must use `syntax.IsSyntaxEmptyList()` instead of `values.IsEmptyList()`.

### Pair Access

| Current | Proposed |
|---------|----------|
| `pr[0]`, `pr[1]` | `pr.SyntaxCar()`, `pr.SyntaxCdr()` |
| `car.(*values.Pair)` | `car.(*syntax.SyntaxPair)` |

## Verification

After implementation:

1. **Run full test suite**: `cd go && make test`
2. **Verify source location preservation**: Check error messages point to correct template positions
3. **Verify hygiene**: All existing hygiene tests in `go/machine/hygiene_test.go` must pass
4. **Run R7RS tests**: `SCHEME_LIBRARY_PATH=./lib ./dist/scheme --file go/integration/testdata/r7rs-tests.scm`

## Files to Modify (Remaining)

| File | Change |
|------|--------|
| `go/match/syntax_adapter.go` | Remove `expandValue()`, `syntaxToValue()`, `valueToSyntaxWithOrigin()` |
| `go/match/match.go` | Remove any remaining raw-value expansion paths |
| `go/machine/operation_syntax_rules_transform.go` | Update expansion calls if needed |
