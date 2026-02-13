# ForEach/SyntaxForEach Duplication Analysis

**Status:** Investigated — decided NOT to consolidate.

## Decision

`SyntaxPair.ForEach()` and `SyntaxPair.SyntaxForEach()` share structural similarity (~20 lines each) but operate on fundamentally different type abstractions (`values.Value` vs `SyntaxValue`). Go generics cannot unify them without sacrificing type safety.

This is **honest duplication** — two operations that happen to share iteration structure but serve different pipeline stages (runtime evaluation vs syntax transformation). Forcing unification creates worse complexity than accepting the structural similarity.

## Files

- `internal/syntax/syntax_pair.go`: `ForEach()`, `SyntaxForEach()`
- `internal/syntax/syntax_vector.go`: `ForEach()`, `SyntaxForEach()`
