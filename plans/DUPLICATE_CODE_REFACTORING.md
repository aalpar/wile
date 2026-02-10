# Duplicate Code Refactoring Plan

**Status**: Complete
**Created**: 2026-01-31
**Last Updated**: 2026-02-09
**Initial Issues**: 39 duplicate code blocks
**Remaining Issues**: 0 duplicate code blocks
**Eliminated**: 13 duplicates (33% reduction), 24 closed as intentional

## Overview

The linter identified 39 duplicate code blocks across 6 packages. 13 have been eliminated (output port helpers, parity checks, integer division, variadic comparisons, optional position extraction, registry helpers). 24 numeric tower type-switch duplicates were closed as intentional architecture (see below). 2 match package VM loop duplicates were eliminated by delegating `MatchSyntax` to `MatchSyntaxWithLiterals` (PR #157).

## Closed: Numeric Tower Type Switch Duplicates (24 duplicates)

**Status**: Closed — intentional architecture

The N×N direct dispatch across 7 numeric types is a deliberate design choice. The previous `Tower*` dispatch layer was removed because it added indirection without benefit. Each numeric type implements its own complete type switch for all operations. This was decided in `SUBSYSTEM_SIMPLIFICATION.md` §Deferred: "Do not reintroduce a dispatch table."

These 24 duplicates are structural artifacts of the direct dispatch model, not accidental duplication:

- **integer.go**: 6 duplicates in Add/Subtract/Multiply (3 pairs)
- **big_integer.go**: 6 duplicates in Add/Subtract/Multiply (3 pairs)
- **float.go**: 2 duplicates in Add/Subtract
- **big_float.go**: 6 duplicates (3 pairs)
- **rational.go**: 6 duplicates (3 pairs)
- **complex.go**: 2 duplicates in Subtract/Multiply
- **big_complex.go**: 2 duplicates

---

## Remaining: Match Package Duplicates (2 duplicates)

**Location**: `internal/match/match.go`
**Complexity**: Low (investigation complete)

### Analysis

`MatchSyntax` (lines 186-334, ~148 lines) and `MatchSyntaxWithLiterals` (lines 344-519, ~175 lines) are near-identical VM loops. Of 13 `case` arms in the bytecode switch, **12 are character-for-character identical**. The sole difference is `ByteCodeCompareCar`:

```
MatchSyntax                          MatchSyntaxWithLiterals
├── ByteCodeCompareCar (4 lines)     ├── ByteCodeCompareCar (28 lines)
│   └── syntaxValuesEqualForMatch()  │   ├── literal hygiene check (24 lines)
│                                    │   │   guarded: if literalMatcher != nil && literalSyntax != nil
│                                    │   └── syntaxValuesEqualForMatch()
├── [12 more cases: IDENTICAL]       ├── [12 more cases: IDENTICAL]
```

The hygiene block is already guarded by `if literalMatcher != nil && literalSyntax != nil`, so passing `nil, nil` skips it entirely and produces identical behavior to `MatchSyntax`.

### Solution

`MatchSyntax` becomes a thin delegation:

```go
func (p *Matcher) MatchSyntax(ctx context.Context, target *syntax.SyntaxPair) error {
    return p.MatchSyntaxWithLiterals(ctx, target, nil, nil)
}
```

This eliminates ~130 lines of duplicated VM loop while preserving the public API.

### Callers

| Method | Production callers | Test callers |
|--------|-------------------|--------------|
| `MatchSyntax` | 0 | 12 (syntax_match_test.go, literal_match_test.go) |
| `MatchSyntaxWithLiterals` | 1 (syntax_adapter.go:190) | 4 (syntax_match_test.go) |

### Files to modify

| File | Changes |
|------|---------|
| `internal/match/match.go` | Replace `MatchSyntax` body with delegation to `MatchSyntaxWithLiterals` |

### Impact

- ~130 lines of duplicated VM loop eliminated
- Single bytecode execution path — bug fixes apply to both callers automatically
- Zero behavioral change (nil guards already in place)

---

## Summary

| Category | Count | Status |
|----------|-------|--------|
| Eliminated (phases 1-13) | 13 | ✅ Done |
| Numeric tower (intentional) | 24 | ✅ Closed |
| Match package VM loop | 2 | ✅ Done |
| **Total** | **39** | **39/39 resolved** |
