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
- `AddScope(scope)` - Add hygiene scope (returns new value)

**Concrete Types**:
- `SyntaxPair` - Cons cells with recursive scope propagation
- `SyntaxSymbol` - Symbols with scope tracking (core for hygiene)
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

## Gotchas

- **Double-wrapping panics**: SyntaxObject prevents wrapping syntax values
- **Pointer equality**: `EqualTo()` uses pointer comparison, not content
- **Recursive scope propagation**: `AddScope()` on SyntaxPair propagates to all nested elements
- **Empty list sentinel**: `SyntaxEmptyList` with both elements nil
- **Vector scope ignored**: `SyntaxVector.AddScope()` returns same vector unchanged
- **Immutable**: All operations return new syntax objects
- **ForEach returns tail**: Returns remaining cdr for improper lists

## Testing

Uses quicktest with suite pattern and custom `SyntaxEquals` checker.
