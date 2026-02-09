# Plan: Refactor Match Package to Pure SyntaxValue Operations

**Status**: Substantially complete — core refactoring done in commit `1c30313` (2026-01-27)
**Branch**: fix/scm-conformance-e (originally fix/scm-conformance-scopes)

## Problem Statement

The match package currently has a two-layer architecture:
1. **Layer 1 (Matcher)**: Operates on raw `values.Value` types
2. **Layer 2 (SyntaxMatcher)**: Bridges syntax ↔ values via `syntaxMap`

This causes two problems:

1. **Template source info lost**: When templates are converted via `syntaxToValue()`, per-element source locations are discarded. Error messages point to wrong locations.

2. **Complex bookkeeping**: The `syntaxMap` requires creating fresh symbol instances for input to distinguish from template symbols via pointer identity.

## Design Goals

1. Operate directly on `syntax.SyntaxValue` throughout the match package
2. Preserve per-element source locations from templates
3. Eliminate the `syntaxMap` mechanism
4. Unify the two expansion paths (`expandValue` and `expandSyntaxValue`)
5. Maintain correct hygiene semantics

## Architecture Changes

### Data Structure Changes

| Current | Proposed |
|---------|----------|
| `valuePathEntry{pr: *values.Pair}` | `syntaxPathEntry{pr: *syntax.SyntaxPair}` |
| `captureContext{bindings: map[string]values.Value}` | `captureContext{bindings: map[string]syntax.SyntaxValue}` |
| `syntaxMap map[values.Value]syntax.SyntaxValue` | **Eliminated** |

### Bytecode Instruction Changes

All bytecode instructions change their `Value` field from `values.Value` to `syntax.SyntaxValue`:

- `ByteCodeCompareCar.Value`
- `ByteCodeCompareCdr.Value`

Capture instructions store `syntax.SyntaxValue` directly in bindings.

### Intro Scope Mechanism

**Current**: Uses `syntaxMap` pointer lookup to distinguish captured input (preserve scopes) from template content (add intro scope).

**Proposed**: Uses **scope comparison** via `patternVarSyntax map[string]*syntax.SyntaxSymbol`:

```go
// During expansion, for each symbol in template:
if capturedVal, ok := ctx.bindings[symName]; ok {
    if patternSym, hasPattern := patternVarSyntax[symName]; hasPattern {
        // Only substitute if scopes match (set equality)
        if !scopesMatch(templateScopes, patternScopes) {
            // Scopes differ - keep as literal, apply intro scope
            return applyHygieneToSymbol(sym, introScope, ...)
        }
    }
    // Scopes match - substitute with captured value (already has original scopes)
    return capturedVal
}
// Not a pattern variable - apply intro scope
return applyHygieneToSymbol(sym, introScope, ...)
```

## Files to Modify

### Primary Changes

| File | Change |
|------|--------|
| `go/match/match.go` | Rewrite `Matcher` to use `syntax.SyntaxValue`; rename stack types |
| `go/match/syntax_adapter.go` | Merge essential expansion logic into unified matcher; delete `syntaxMap`, `syntaxToValue`, `valueToSyntax` |
| `go/match/syntax_compiler.go` | Compile `*syntax.SyntaxPair` directly instead of `*values.Pair` |
| `go/match/pattern_analyzer.go` | Analyze `*syntax.SyntaxPair` directly |
| `go/match/bytecode_compare_car.go` | Change `Value` field type |
| `go/match/bytecode_compare_cdr.go` | Change `Value` field type |

### Consumer Updates

| File | Change |
|------|--------|
| `go/machine/compile_syntax_rules.go` | Update calls to match package |
| `go/machine/operation_syntax_rules_transform.go` | Update expansion calls |
| `go/machine/operation_syntax_case.go` | Update matcher usage |

## Migration Strategy (5 Phases)

### Phase 1: Add Parallel Syntax Types (Low Risk) - DONE
- [x] Change `captureContext.bindings` to `map[string]syntax.SyntaxValue`
- [x] Create `syntaxPathEntry` alongside `valuePathEntry`
- [x] Add utility functions: `countRemainingSyntaxElements()`, `syntaxValuesEqualForMatch()`

### Phase 2: Refactor Bytecode Instructions (Medium Risk) - DONE
- [x] Create new bytecode instruction versions using `syntax.SyntaxValue`
- [x] Update `SyntaxCompiler` to emit new instructions

### Phase 3: Unify Matcher (Medium-High Risk) - DONE
- [x] Refactored matcher to use syntax objects directly for matching (commit `1c30313`)
- [x] Eliminated `syntaxMap` bridging layer
- [x] Pattern matching now preserves scope sets for `bound-identifier=?` semantics

### Phase 4: Unify Expansion (Medium Risk) - PARTIAL
- [ ] Remove `expandValue()`, keep only `expandSyntaxValue()` (renamed to `expand()`)
- [ ] Remove remaining `valueToSyntaxWithOrigin()` and `syntaxToValue()` functions

### Phase 5: Cleanup (Low Risk) - PENDING
- [ ] Remove deprecated functions
- [ ] Update documentation and CLAUDE.md files
- [ ] Update consumer files in machine package

**Note**: The core refactoring (Phases 1–3) was completed as part of the nested let-syntax hygiene fix. Phases 4–5 are cleanup/polish that can be done opportunistically.

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

## Benefits

1. **Better error messages**: Per-element source locations preserved from templates
2. **Simpler code**: One expansion path instead of two
3. **Less bookkeeping**: No `syntaxMap` pointer identity tricks
4. **Cleaner hygiene**: Scope comparison is more explicit than map lookup

## Risks

1. **Performance**: Syntax objects are larger than raw values (carry `SourceContext`). Mitigated by: macro expansion is compile-time, `syntaxMap` lookup elimination offsets overhead.

2. **Scope comparison edge cases**: The `patternVarSyntax` mechanism is already implemented and tested in `expandSyntaxValue`. This proposal extends its use to replace `syntaxMap`.

## Notes

- The linter may have reverted some changes to syntax_compiler.go - verify before resuming
- Task list created with IDs 1-5 tracking the 5 phases
- Current state: Phase 1 just started, one edit attempted
