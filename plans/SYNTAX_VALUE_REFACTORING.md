# Plan: Refactor Match Package to Pure SyntaxValue Operations

**Status**: Complete

## Overview

The match package was refactored from a two-layer architecture (raw `values.Value` matcher + `syntaxMap` bridge) to operate directly on `syntax.SyntaxValue`. All five phases are complete.

## Completed Work

### Phase 1-3: Core Refactoring — Done (commit `1c30313`, 2026-01-27)
Parallel syntax types, bytecode instruction refactoring, matcher unification.

### Phase 4: Unify Expansion — Done
- [x] Removed `expandValue()` — only `expandSyntaxValue()` remains
- [x] Removed `valueToSyntaxWithOrigin()` — already gone by Phase 3
- [x] `syntaxToValue()` remains — still needed by pattern compiler (`SyntaxCompiler.Compile`)

### Phase 5: Cleanup — Done
- [x] Removed legacy `Matcher.Match(*values.Pair)` — entire values-based VM loop deleted
- [x] Removed legacy `Matcher.Expand()` / `ExpandPreservingSyntax()` and all supporting functions
- [x] Removed dead `valueToSyntax()`, `literalScopesMatch()` (standalone)
- [x] Removed `countRemainingElements()` (values.Pair version)
- [x] Removed `valuePathEntry` / `valueStack` from Matcher struct
- [x] Converted legacy tests to syntax-native API, deleted `expand_test.go`
- [x] Updated CLAUDE.md documentation

### Remaining (not in scope)
- Pattern compiler (`SyntaxCompiler.Compile`) still accepts `*values.Pair`. Converting it to operate on `*syntax.SyntaxPair` directly would eliminate the last `syntaxToValue()` call but is a separate, larger refactoring.

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
