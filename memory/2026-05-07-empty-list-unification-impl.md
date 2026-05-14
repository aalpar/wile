# EmptyList* unification — implementation plan

**Date**: 2026-05-07
**Branch**: `feat/empty-list-unification`
**Status**: ✅ **SHIPPED** — PR #727 (`feat/empty-list-unification`), merge commit `76e93098`.
**Motivation**: Chez evidence (`memory/empty-list-duality-chez-evidence.md`)

## Problem

Wile has two empty-list types — `values.emptyListType` (the value-level
singleton) and `*syntax.syntaxEmptyListType` (the syntax-level singleton).
The split is not Chez-conformant: Chez's `(equal? (syntax ()) '())` is `#t`,
but Wile's strict pointer-type `EqualTo` on `*syntaxEmptyListType` makes
this `#f`. The duality also produces an asymmetric `EqualTo`:
`values.EmptyList.EqualTo(SyntaxEmptyList)` is `#t` (delegates via Tuple)
while `SyntaxEmptyList.EqualTo(values.EmptyList)` is `#f` (strict assert).

The existing comment at `internal/syntax/syntax_empty_list.go:50-53`
claims this matches Chez/Racket; the Chez REPL evidence directly
contradicts that claim.

## Goal

Collapse the duality. There is one empty-list singleton: `values.EmptyList`.
It implements both the value-level interfaces (`Value`, `Tuple`) and the
syntax-level interfaces (`SyntaxValue`, `SyntaxTuple`). `SyntaxEmptyList`
becomes an alias for `values.EmptyList`.

`SchemeString` returns `"()"` (matching Chez printer for `#'()`).
`EqualTo` is naturally symmetric since there's one type.

## Cleavage decision

Per the conversation: option (a) — relocate `SourceContext` and the
syntax-shape interfaces to `values/`. Reason: SyntaxValue's `Unwrap`/
`UnwrapAll` return `values.Value`, so the interface cannot live in a
package downstream of `values/`. The marker-interface alternative loses
type safety. See plans/2026-05-07-structural-reduction-roadmap.md for the
broader `values/` size concern; the eventual values/ split (Tier-A
roadmap) will subdivide; this plan does not pre-empt that split.

## Files moving from `internal/syntax/` → `values/`

Required by the dependency chain:

- `source_context.go` (`SourceContext`, `OriginInfo`, `FormatOriginChain`)
- `source_indexes.go` (`SourceIndexes`)
- `Scope` type + `nextScopeID` + scope constructors (currently in `syntax_value.go`)
- Pure scope-set utilities from `scope_utils.go` (`HasScope`, `AddScopeToSet`, `RemoveScopeFromSet`, `FlipScopeInSet`, `ScopesMatch`)
- `SyntaxValue` interface (so `values.emptyListType` can implement it)
- `SyntaxTuple` interface (5 production sites force this)
- `SyntaxVector` (referenced by `SyntaxTuple.AsSyntaxVector()`)

Stays in `internal/syntax/`:

- `SyntaxObject`, `SyntaxSymbol`, `SyntaxPair` (data containers)
- `SyntaxComment`, `SyntaxDatum*`, `SyntaxDirective`
- SyntaxValue-aware utilities (`FlipScope`, `AddScopeToSyntax`, `SyntaxList`, `EqualTo`, `IsSyntaxList`, `IsSyntaxVoid`, `IsSyntaxEmptyList`, `mapSyntaxTree`, `UnwrapAllShared`)
- `syntaxVoidType` (syntax-level void wrapper)
- `syntax_empty_list.go` — **deleted**

## Implementation steps

### Phase 1 — Add new types to values/, alias from internal/syntax/

1. New file `values/source_indexes.go` — `SourceIndexes`.
2. New file `values/scope.go` — `Scope`, `nextScopeID`, scope constructors, pure scope-set utils (`HasScope`, `AddScopeToSet`, `RemoveScopeFromSet`, `FlipScopeInSet`, `ScopesMatch`).
3. New file `values/source_context.go` — `SourceContext`, `OriginInfo`, `FormatOriginChain`.
4. New file `values/syntax_value.go` — `SyntaxValue` interface.
5. New file `values/syntax_tuple.go` — `SyntaxTuple` interface, `SyntaxForEachFunc`.
6. New file `values/syntax_vector.go` — `SyntaxVector`.
7. In `internal/syntax/`, replace the originals with type aliases:
   ```go
   type SourceContext = values.SourceContext
   type SourceIndexes = values.SourceIndexes
   type Scope = values.Scope
   type SyntaxValue = values.SyntaxValue
   type SyntaxTuple = values.SyntaxTuple
   type SyntaxVector = values.SyntaxVector
   var NewScope = values.NewScope
   ...
   ```
8. Build green; existing import sites keep working.

### Phase 2 — Implement SyntaxValue/SyntaxTuple on values.emptyListType

1. Add to `values/empty_list.go`:
   - `SourceContext() *SourceContext { return nil }`
   - `Unwrap() Value { return p }` (or return self)
   - `UnwrapAll() Value { return p }`
   - `SyntaxCar() SyntaxValue { panic ... }`
   - `SyntaxCdr() SyntaxValue { panic ... }`
   - `SyntaxForEach(...) (SyntaxValue, error) { return p, nil }`
   - `SyntaxAppend(vs SyntaxValue) SyntaxValue { return vs }`
   - `AsSyntaxVector() *SyntaxVector { return NewSyntaxVector(nil) }`
2. Replace `var SyntaxEmptyList SyntaxTuple = &syntaxEmptyListType{}` with `var SyntaxEmptyList SyntaxTuple = values.EmptyList` (in the appropriate location after the alias is in place).
3. Update `IsSyntaxEmptyList` to delegate to `values.IsEmptyList`.
4. Delete `internal/syntax/syntax_empty_list.go` and its test file.
5. Build green; behavior changes:
   - `(equal? (syntax ()) '())` → `#t` (Chez-conformant)
   - `(syntax ())` prints `()` not `#'()` (Chez-conformant)

### Phase 3 — Migrate import sites away from aliases

1. `gofmt`-driven find-and-replace: for each moved type, change `syntax.X` to `values.X` at the 159 import sites.
2. Drop the type aliases from `internal/syntax/`.
3. Build green.

### Phase 4 — Regression test + verification

1. Add Scheme-level test: `(equal? (syntax ()) '()) → #t`.
2. Add Go-level test: `values.EmptyList.EqualTo(SyntaxEmptyList) == true` and reverse direction.
3. Run `make lint && make covercheck && make ci`.
4. Update or remove the misleading comment at the deleted `syntax_empty_list.go:50-53`.

## Acceptance criteria

- `values.EmptyList` and `SyntaxEmptyList` are the same Go pointer.
- `(equal? (syntax ()) '())` returns `#t`.
- All existing tests pass.
- `make lint` clean.
- `make covercheck` no regressions.

## Out of scope

- Renaming `internal/syntax/` → `internal/expander/`. Separate small PR.
- Splitting `values/` into subpackages. Tier-A roadmap initiative.
- Generalizing this merge to other syntax types (`SyntaxPair` etc.). Pairs
  carry symbols and source context — the phase distinction is essential
  there per Chez's `(syntax '()) → #<syntax (quote ())>` printout.
