# ForEach/SyntaxForEach Duplication Analysis

**Status**: Investigated and decided NOT to consolidate.

**Date**: 2026-02-11

## Summary

`SyntaxPair.ForEach()` and `SyntaxPair.SyntaxForEach()` share nearly identical iteration logic (~20 lines each). Initial investigation suggested consolidation, but type system analysis revealed this is **honest duplication** - two fundamentally different operations that happen to share structural similarity.

## The Duplication

Both methods iterate over a syntax pair list with the same structure:

```go
// ForEach - unwraps to values.Value
func (p *SyntaxPair) ForEach(ctx context.Context, fn values.ForEachFunc) (values.Value, error) {
    pr := p
    i := 0
    for pr != nil && !pr.IsEmptyList() {
        hasNext := !values.IsEmptyList(pr.Cdr())
        err := fn(ctx, i, hasNext, pr.Car())  // ← Unwrapped value
        // ... iteration logic
    }
    return pr, nil
}

// SyntaxForEach - keeps SyntaxValue wrapper
func (p *SyntaxPair) SyntaxForEach(ctx context.Context, fn SyntaxForEachFunc) (SyntaxValue, error) {
    pr := p
    i := 0
    for pr != nil && !pr.IsEmptyList() {
        hasNext := !IsSyntaxEmptyList(pr.Cdr().(SyntaxValue))
        err := fn(ctx, i, hasNext, pr.Car().(SyntaxValue))  // ← Wrapped syntax
        // ... identical iteration logic
    }
    return pr, nil
}
```

## Type System Constraints

| Aspect | ForEach | SyntaxForEach | Unifiable? |
|--------|---------|---------------|------------|
| Callback signature | `values.ForEachFunc` | `SyntaxForEachFunc` | ❌ Different function types |
| Element type | `values.Value` (unwrapped) | `SyntaxValue` (wrapped) | ❌ Different interfaces |
| Return type | `values.Value` | `SyntaxValue` | ❌ Different interfaces |
| Nil value | `values.Void` | `SyntaxVoid` | ❌ Different constants |
| Empty check | `values.IsEmptyList()` | `IsSyntaxEmptyList()` | ❌ Different predicates |

**Fundamental conflict**: Go generics cannot unify these because `values.Value` and `SyntaxValue` are different interfaces with different semantics.

## Why Generics Don't Help

**Attempt 1: Type parameter**
```go
func genericForEach[T any](p *SyntaxPair, fn func(context.Context, int, bool, T) error, ...) (T, error)
```
❌ Loses all type safety - `T any` doesn't constrain to `values.Value` or `SyntaxValue`.

**Attempt 2: Union constraint**
```go
func genericForEach[T values.Value | SyntaxValue](...)
```
❌ Union constraints don't work with interfaces in Go - both are interface types, not concrete types.

**Attempt 3: Callback abstraction**
```go
func genericForEach[T, F any](
    p *SyntaxPair,
    fn F,  // Can't constrain this properly
    extract func(*SyntaxPair) T,
    isEmptyFunc func(T) bool,
    nilValue T,
) (T, error)
```
❌ More complex, harder to understand, and less type-safe than the 20-line duplication.

## Semantic Difference

These methods serve fundamentally different purposes at different pipeline stages:

| Method | Purpose | Used By | When |
|--------|---------|---------|------|
| `ForEach` | Iterate over **data values** for evaluation | Runtime, primitives, VM | After macro expansion, during execution |
| `SyntaxForEach` | Iterate over **syntax objects** preserving hygiene | Macro expander, compiler | During macro expansion, before execution |

**ForEach** is about computing with values. **SyntaxForEach** is about transforming syntax while preserving source context and hygiene information.

## Decision

**Do NOT consolidate.** This is honest duplication where forcing unification creates worse complexity than accepting the structural similarity.

### Why This Is Acceptable

1. **Type-level distinction is meaningful**: The two methods operate on fundamentally different abstractions (values vs syntax).

2. **Each method is simple**: 20 lines of straightforward iteration logic. No hidden complexity.

3. **Abstraction cost exceeds benefit**: Any consolidation would require parameterizing over types, callbacks, nil values, and predicates - creating a meta-abstraction that's harder to understand than two clear implementations.

4. **Changes are rare**: Iteration logic is stable. The "DRY cost" of maintaining both is minimal.

5. **Clarity wins**: Two focused methods with clear purposes are better than one generic method with configurable behavior.

## Related Issues

- **Item III** from `plans/ALGEBRAIC_REDUCTIONS.md` (attempted consolidation, abandoned)
- Similar duplication exists in `SyntaxVector.ForEach()` / `SyntaxVector.SyntaxForEach()` for the same reasons

## Precedent

This same pattern appears in other Scheme implementations:
- Racket: separate iteration over `syntax` vs `datum`
- Chez Scheme: unwrap operations distinct from syntax operations

The duplication reflects a genuine semantic distinction, not accidental code evolution.

## Future Considerations

If Go gains:
- Interface union types that work with generics
- Higher-kinded types (type constructors as parameters)
- More flexible constraint systems

Then consolidation might become feasible without sacrificing type safety. Until then, keep them separate.

## Files

- `internal/syntax/syntax_pair.go`: `ForEach()` (line 239), `SyntaxForEach()` (line 262)
- `internal/syntax/syntax_vector.go`: `ForEach()` (line 103), `SyntaxForEach()` (line 125)
- `internal/syntax/syntax_tuple.go`: `SyntaxForEachFunc` type definition
- `values/tuple.go`: `ForEachFunc` type definition

## References

- Go generics proposal: https://go.dev/blog/intro-generics
- Go FAQ on union types: https://go.dev/doc/faq#unions
