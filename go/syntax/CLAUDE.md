# CLAUDE.md

Package `syntax` implements Scheme syntax representation with hygiene support.

## Purpose

Wraps Scheme values with source location and scope information for:
- Source location tracking (file, line, column)
- Macro hygiene via Flatt's "sets of scopes" model
- Macro expansion chain tracking

## Key Types

**SyntaxValue** - Interface for all syntax objects:
- `SourceContext()` - Source location and scopes
- `Unwrap()` - Shallow unwrap to underlying value
- `UnwrapAll()` - Deep recursive unwrap

**Scope-aware types** - Only `SyntaxSymbol` and `SyntaxPair` have `AddScope(scope)` for hygiene. `SyntaxObject` (self-evaluating literals) does not need scopes.

**Key Functions**:
- `UnwrapAllShared(sv, cache)` - Recursive unwrap preserving object identity via cache. Essential for datum labels (R7RS §2.4) where `#n#` must be `eq?` to `#n=`. Pre-registers placeholders before recursing to handle circular structures.

**Concrete Types**:
- `SyntaxPair` - Cons cells with recursive scope propagation
- `SyntaxSymbol` - Symbols with scope tracking (core for hygiene)
  - `ResolvedBinding` - Pre-resolved GlobalIndex for cross-library macro hygiene (see below)
- `SyntaxVector` - Vectors (no individual element scopes)
- `SyntaxObject` - Wrapper for other values

**SourceContext** - Location and hygiene metadata:
- `File`, `Start`, `End` - Source position
- `Scopes` - Scope set for hygiene
- `Origin` - Macro expansion chain

**Scope** - Hygiene scope with unique ID

## Hygiene Algorithm

```go
// Binding's scopes must be subset of use's scopes
func ScopesMatch(useScopes, bindingScopes []*Scope) bool
```

## Cross-Library Macro Hygiene

`SyntaxSymbol.ResolvedBinding` enables proper hygiene for macros that reference helpers from their definition library:

```go
// In SyntaxSymbol:
ResolvedBinding any // *environment.GlobalIndex for free identifiers in macro templates
```

**Problem solved**: When macro defined in library A references helper `foo` also in A, and the macro is used in library B, `foo` must resolve to A's binding, not B's environment.

**Solution**: At macro definition time, free identifiers are resolved to their `GlobalIndex` and stored in `SyntaxRulesClause.freeIds`. During template expansion, these bindings are attached to free identifier symbols via `WithResolvedBinding()`. At compile time, `CompileSymbol` checks for `ResolvedBinding` first, using it directly if present.

This keeps the resolution context separate from the physical source context (SourceContext).

## Gotchas

- **Double-wrapping panics**: SyntaxObject prevents wrapping syntax values
- **Pointer equality**: `EqualTo()` uses pointer comparison, not content
- **Scope propagation**: `AddScope()` on SyntaxPair recursively propagates to nested symbols only; pairs themselves don't store scopes
- **Empty list sentinel**: `SyntaxEmptyList` with both elements nil
- **Vector scope ignored**: `SyntaxVector.AddScope()` returns same vector unchanged
- **Immutable**: All operations return new syntax objects
- **ForEach returns tail**: Returns remaining cdr for improper lists

## Testing

Uses quicktest with suite pattern and custom `SyntaxEquals` checker.

### Test File Organization

Tests are organized by syntax type with a coverage file for edge cases:

| Test File | Tests For |
|-----------|-----------|
| `syntax_pair_test.go` | SyntaxPair operations |
| `syntax_value_test.go` | SyntaxValue interface, SyntaxObject |
| `syntax_vector_test.go` | SyntaxVector operations |
| `utils_test.go` | Utility functions |
| `coverage_test.go` | Additional edge case coverage |

When adding tests, use the type-specific test file or `coverage_test.go` for cross-cutting concerns.

## References

See `BIBLIOGRAPHY.md` at project root for Flatt 2016 "Binding as Sets of Scopes" - the academic paper describing the hygiene model implemented in this package.
