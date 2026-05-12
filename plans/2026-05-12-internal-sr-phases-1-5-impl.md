# `internal/` structural reduction — Phases 1-5 (Findings 7, 4, 3, 2, 6)

Implementation plan for the first five phases of
`plans/2026-05-07-internal-structural-reduction.md`.

## Scope

This PR ships **5 of 7 findings** from the parent plan, batching the
XS/S items that the parent plan flagged as "mostly independent". The
two remaining findings are deferred:

- **Finding 5** (S-M, `WalkBindingRefs` higher-order traversal) —
  larger refactor; touches `markCaptured`/`markEscaped` analyses;
  separate PR.
- **Finding 1** (M, `*SyntaxPair` empty-list duality) — gated on a
  cross-package audit (`syntax/` has 14 dependents); separate PR after
  confirming the `values/`-side migration template.

| Finding | Size | What | Sequence |
|---------|------|------|----------|
| 7 | XS | Delete dead `SyntaxObject.IsPair()` / `IsEmptyList()` | 1 |
| 4 | S  | `bindLocalSymbol` + `extendEnvWithSymbols` collapse 5 sites | 2 |
| 3 | S  | `detectDuplicateSymbols` fold collapses 5 sites | 3 |
| 2 | S  | `parseLetBindingPairs` helper collapses 3 sites (uses Finding 3) | 4 |
| 6 | S  | `match.Matcher` option-functions, 4 ctors → 1 + N options | 5 |

Sequence: Finding 7 first (pure delete), then 4 (env-extension helper
unblocks several callers), then 3 (dup-detect helper used by Finding
2), then 2 (parse-binding helper depends on 3), then 6 (independent,
match package).

## Phases

### Phase 1 — Finding 7: Delete dead `SyntaxObject.IsPair()` / `IsEmptyList()`

`internal/syntax/syntax_value.go:122-136` (`NewSyntaxObject`) rejects
`*values.Pair` / `*values.Vector` / `*values.Symbol` etc. at
construction, so the `IsPair()` method at `:160-164` and `IsEmptyList()`
at `:166-169` cannot return `true` for any legitimately-constructed
value. Confirmed by grep: zero production callers.

**Action**: delete the two methods. Any consumer that needs pair/empty
detection should switch on the syntax-level type
(`*SyntaxPair`, `SyntaxEmptyList`), which is the correct dispatch site
anyway.

**Risk**: zero if the `values.Value` interface does not require these
methods. Verify by checking the interface definition before deleting.
If the interface requires them, fall back to keeping the methods with
`return false` bodies and a doc comment.

### Phase 2 — Finding 4: env-extension helpers

`internal/validate/validate_lambda.go:56-87`,
`validate_let.go:672-690`, plus inlined versions at
`validate_let.go:316-322`, `:380-419`, `:514-521` — 5 sites that all
call:

```go
childEnv.MaybeCreateLocalBinding(sym.Sym, BindingTypeVariable,
                                  sym.Scopes(), sym.SourceContext())
```

inside a `for` over either `*ValidatedParams` or `[]ValidatedLetBinding`
or `[]*SyntaxSymbol`.

**Action**: extract two helpers in
`internal/validate/env_helpers.go` (new file):

```go
// bindLocalSymbol binds sym in env as a local variable using the
// canonical four-argument shape.
func bindLocalSymbol(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol)

// extendEnvWithSymbols creates a child frame and binds all symbols
// as variables. Returns env unchanged if syms is empty.
func extendEnvWithSymbols(env *environment.EnvironmentFrame, syms []*syntax.SyntaxSymbol) *environment.EnvironmentFrame
```

Lambda's existing site uses `*ValidatedParams.Required + Rest`; supply
those as a flat slice. The 3 inlined let-family sites need the
per-iteration form (`bindLocalSymbol`), since their env evolves with
init validation.

**Action B**: in `validate_let.go:672-690`'s `createLetValidationEnv`,
replace the inlined loop with `extendEnvWithSymbols(env, syms)`.
Likewise in `validate_lambda.go:56-87`.

### Phase 3 — Finding 3: `detectDuplicateSymbols` fold

5 inlined `seen[bindingIdentity]` loops:
- `internal/validate/validate_define.go:131-184` (required params + rest)
- `internal/validate/validate_let.go:222-234` (let*)
- `internal/validate/validate_let.go:430-447` (letrec)
- `internal/validate/validate_let.go:596-611`
  (`checkDuplicateBindingNames` — already factored for
  `[]ValidatedLetBinding`)

**Action**: extract to
`internal/validate/dup_detect.go` (new file or add to env_helpers.go):

```go
// detectDuplicateSymbols returns the duplicates in order of second
// appearance. Equality is by (key, scope-fingerprint) tuple. Empty
// result = no duplicates.
func detectDuplicateSymbols(syms []*syntax.SyntaxSymbol) []*syntax.SyntaxSymbol
```

The existing `checkDuplicateBindingNames` becomes a thin wrapper that
maps `[]ValidatedLetBinding` → `[]*SyntaxSymbol` and delegates. The
3 raw-symbol sites in `validate_let.go` and the 2 in
`validate_define.go` call `detectDuplicateSymbols` and decide
reporting locally.

### Phase 4 — Finding 2: `parseLetBindingPairs` helper

Three hand-unrolled "parse `((name init) ...)`" loops in
`internal/validate/validate_let.go:184-212` (let*),
`:386-420` (letrec), `:622-654` (`validateLetBindingPairs`).

**Action**: extract to `validate_let.go` (or `env_helpers.go`):

```go
type rawLetBinding struct {
    name *syntax.SyntaxSymbol
    init syntax.SyntaxValue
}

// parseLetBindingPairs parses ((name init) ...) into raw bindings.
// Does NOT validate init — caller validates in the appropriate env.
func parseLetBindingPairs(
    bindingsPair *syntax.SyntaxPair,
    formName string,
    result *ValidationResult,
) ([]rawLetBinding, bool)
```

Then:
- `validateLetBindingPairs` becomes parse-then-validate: calls helper,
  then loops `init = validateExpr(ctx, env, raw.init, result)`.
- `validateLetStarBindingsAndBody` calls the helper, supplies its own
  evolving-env init validation.
- `validateLetrecBindingsAndBody` calls the helper, builds env from
  raw names, validates inits in that env.

The dup-detect helper from Phase 3 plugs in cleanly here for each
caller's duplicate check.

### Phase 5 — Finding 6: `match.Matcher` option-functions

`internal/match/match.go:78-94` has 4 telescoping constructors:
```go
NewMatcher(variables, codes)
NewMatcherWithEllipsisVars(variables, codes, ellipsisVars)
NewMatcherFull(variables, codes, ellipsisVars, ellipsisID)
NewMatcherFullWithDepths(variables, codes, ellipsisVars, ellipsisDepths, ellipsisID)
```

**Action**: collapse to one constructor + N options, matching the
existing `wile.Engine` idiom (`WithProfile`, `WithSandbox`,
`WithSourceFS`):

```go
type MatcherOption func(*Matcher)

func WithEllipsisVars(v map[int]map[string]struct{}) MatcherOption
func WithEllipsisDepths(d map[int]int) MatcherOption
func WithEllipsisID(id string) MatcherOption

func NewMatcher(variables map[string]struct{}, codes []SyntaxCommand, opts ...MatcherOption) *Matcher
```

Delete the 3 telescoping variants. Update callers (search `grep -rn
'NewMatcherWithEllipsisVars\|NewMatcherFull\|NewMatcherFullWithDepths'`).

### Phase 6 — Verify

`make lint && make covercheck && make ci`, with the `internal/` test
suite given particular attention (validate tests cover the
let/letrec/lambda/define paths that Phases 2-4 touch).

## Risk

- **Phase 2/3/4 cross-cutting**: validate package has the densest
  cohesion in the codebase. Each helper extraction touches several
  callers; one mis-edit means tests fail. Mitigated by running the
  test suite after each phase rather than at the end.
- **Phase 5 import-set audit**: `match` is consumed by
  `machine/compilation` only (low Ca). The 4 callers are localized;
  search and grep should find all.
- **No behavior change.** All five findings are pure refactors. No
  bench impact expected (none of these helpers live on the VM hot
  path).

## Commit cadence

Following `feedback_commit_cadence.md` (progressive commits):

1. `docs(plans): impl plan for internal/ structural reduction phases 1-5`
2. `refactor(syntax): delete dead SyntaxObject.IsPair/IsEmptyList (Finding 7)`
3. `refactor(validate): extract bindLocalSymbol + extendEnvWithSymbols (Finding 4)`
4. `refactor(validate): extract detectDuplicateSymbols (Finding 3)`
5. `refactor(validate): extract parseLetBindingPairs (Finding 2)`
6. `refactor(match): collapse 4 telescoping ctors to 1 + N options (Finding 6)`

Each commit builds and passes its own tests independently.
