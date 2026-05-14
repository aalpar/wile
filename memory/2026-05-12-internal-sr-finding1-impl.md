# `internal/` structural reduction — Finding 1: `*SyntaxPair` empty-list duality

**Status**: ✅ **SHIPPED** — PR #741 (`feat/internal-sr-finding1`), merge commit `6b51d64b`.

Implementation plan for **Finding 1** of
`plans/2026-05-07-internal-structural-reduction.md`, the last open item
on the internal/ plan after PRs #739 and #740.

## Decision

Finish the migration that `values/` already completed. `*SyntaxPair`
currently admits two representations of the empty list:

1. `SyntaxEmptyList` — the canonical singleton (aliases `values.EmptyList`,
   the `emptyListType{}` zero-size struct).
2. `*SyntaxPair{Values: [2]SyntaxValue{nil, nil}}` — a pair with both
   slots nil, reported as empty by `IsEmptyList()`.

The second representation is constructed **only in tests**
(verified by `grep -rEn 'NewSyntaxCons\(\s*nil\s*,\s*nil'` returning
exactly 3 hits: `syntax_pair_test.go:33,85` and `coverage_test.go:549`).
Zero production callers create nil-nil pairs.

The `values.Pair` migration (PR-pre-history, before the recent
empty-list duality merge) shipped the canonical fix: `*Pair.IsEmptyList()`
returns `false` unconditionally; the empty list flows exclusively
through the singleton; defensive guards on `*Pair` are deleted.

This PR applies the same template to `*SyntaxPair`. The migration is
self-contained within `internal/syntax/` plus one defensive guard in
the same package and three test updates.

## Scope

| File | Change |
|------|--------|
| `internal/syntax/syntax_pair.go` | Hardcode `IsEmptyList()` to `return false`. Delete 7 defensive guards (AddScope, Unwrap, Append, SyntaxAppend, SchemeString, AsVector, AsSyntaxVector). Simplify inner-loop conditions in `ForEach` / `SyntaxForEach` (`pr.IsEmptyList()` checks become dead). |
| `internal/syntax/syntax_value.go` | Delete the dead `IsEmptyList()` guard in `UnwrapAllShared`'s `*SyntaxPair` case (line 161). |
| `internal/syntax/syntax_pair_test.go` | Remove the test case at `:33` that constructed `NewSyntaxCons(nil, nil, nil)` and expected `"#'()"` — the pre-migration short-circuit no longer fires. Update `TestSyntaxPair_NewSyntaxCons` at `:85` (still passes; the test only verifies non-nil return). |
| `internal/syntax/coverage_test.go` | `TestSyntaxPair_SourceContext` at `:549` is unaffected (constructs nil-nil pair but only reads source context, never queries IsEmptyList). |

Net: ~50 LOC removed, ~10 LOC added (doc comments).

## Phases

### Phase 1 — Plan + branch

Commit this plan file.

### Phase 2 — Hardcode `IsEmptyList()` to false

`internal/syntax/syntax_pair.go:217-223`:

```go
// IsEmptyList returns false. A *SyntaxPair is never the empty list;
// SyntaxEmptyList (the values.EmptyList singleton aliased into the
// syntax phase) is the only representation of the empty list. Mirrors
// the values.Pair migration; see Finding 1 of
// plans/2026-05-07-internal-structural-reduction.md.
func (p *SyntaxPair) IsEmptyList() bool {
    return false
}
```

### Phase 3 — Delete defensive guards

Eight production sites become dead after Phase 2:

| File:Line | Dead branch |
|-----------|-------------|
| `syntax_pair.go:54` | `AddScope` — `if p.IsEmptyList() { return p }` |
| `syntax_pair.go:130` | `Unwrap` — `if p.IsEmptyList() { return values.EmptyList }` |
| `syntax_pair.go:156` | `Append` — `if p.IsEmptyList() { return vs }` |
| `syntax_pair.go:186` | `SyntaxAppend` — `if values.IsEmptyList(p) { return vs }` (same dispatch via Tuple interface) |
| `syntax_pair.go:237` | `ForEach` inner-loop guard `for pr != nil && !pr.IsEmptyList()` — simplify to `for pr != nil` (loop exits via the type-assertion `!ok` branch when cdr is not a `*SyntaxPair`) |
| `syntax_pair.go:260` | `SyntaxForEach` — same simplification |
| `syntax_pair.go:289` | `SchemeString` — `if p.IsEmptyList() { return "#'()" }` |
| `syntax_pair.go:325` | `AsVector` — `if p.IsEmptyList() { return values.NewVector() }` |
| `syntax_pair.go:348` | `AsSyntaxVector` — `if p.IsEmptyList() { return NewSyntaxVector(p.SourceContext()) }` |
| `syntax_value.go:161` | `UnwrapAllShared`'s `*SyntaxPair` case — `if v.IsEmptyList() { return values.EmptyList }` |

After deletion, the methods walk the pair normally. Production
behavior preserved because production never creates nil-nil pairs.

### Phase 4 — Update tests

Three test sites construct `NewSyntaxCons(nil, nil, ...)`:

1. `syntax_pair_test.go:33` — `{NewSyntaxCons(nil, nil, nil), "#'()"}`
   in `TestSyntaxPair_SchemeString`. The pair no longer prints as
   `"#'()"` because `IsEmptyList()` no longer fires. The test case
   tested a pre-migration path that no longer exists. Two options:
   (a) delete the test case; (b) replace with a test that constructs
   `SyntaxEmptyList` and asserts its SchemeString. Approach (b)
   keeps the empty-list-rendering coverage; the assertion shifts to
   `values.EmptyList.SchemeString() == "()"` (note: not `"#'()"` —
   `SyntaxEmptyList` is the value-level singleton; its rendering
   inherits from `emptyListType.SchemeString()`). **Plan: delete the
   test case.** The `"#'()"` rendering on the value-level singleton
   would be a new behavioral choice (does the syntax phase want a
   different empty-list print form than the value phase?) — out of
   scope for this PR.

2. `syntax_pair_test.go:85` — `TestSyntaxPair_NewSyntaxCons`
   constructs `pr := NewSyntaxCons(nil, nil, nil)` and asserts
   `pr != nil`. Still passes — `NewSyntaxCons` doesn't reject nil
   contents, just creates the pair. Test renamed to clarify that
   `NewSyntaxCons` accepts nil components but the result is a pair,
   not the empty list.

3. `coverage_test.go:549` — `TestSyntaxPair_SourceContext`
   constructs the nil-nil pair only to verify `SourceContext()`
   plumbing. Unaffected.

### Phase 5 — Verify

`make lint && make covercheck && make ci`. The lockstep deletion
relies on the `values.Pair` migration as precedent — if the same
shape works there (which it has, in production, for some time), it
works here. The existing `syntax_pair_test.go` exercises every
modified method.

## Risk

- **Behavior change for tests that pass `NewSyntaxCons(nil, nil, ...)`
  and then read `IsEmptyList()` or call methods that previously
  short-circuited.** Mitigation: delete the one test case that relied
  on this; the other two nil-nil test sites don't depend on the
  short-circuits. Verified by audit.

- **Latent production callers that I missed.** Mitigation: the test
  suite covers every method that previously had a defensive guard
  (ForEach, SchemeString, Append, AsVector, etc.) on non-nil-nil
  pairs. If any production path was relying on a nil-nil
  short-circuit, those tests would fail. Plus: `make ci` covers
  end-to-end Scheme execution; any actual runtime use of the
  short-circuit would surface.

- **`UnwrapAllShared` placeholder behavior.** The function
  pre-registers `placeholder := values.NewCons(nil, nil)` in the
  cache before recursing. This is a `*values.Pair`, not a
  `*SyntaxPair`, so it's unaffected by my migration. The
  pre-registration step is independent.

## Commit cadence

1. `docs(plans): impl plan for internal/ SR finding 1 (SyntaxPair empty-list duality)`
2. `refactor(syntax): finish empty-list duality migration on *SyntaxPair`

Single implementation commit — the changes are tightly coupled (the
guards become dead exactly when IsEmptyList returns false). Splitting
would land a half-migrated state that the test suite would fail on.

## Closes

This PR closes the internal/ structural-reduction plan.
`plans/2026-05-07-internal-structural-reduction.md` becomes 7/7 — all
findings shipped (Findings 7, 4, 3, 2, 6 in PR #739; Finding 5 in PR
#740; Finding 1 here).
