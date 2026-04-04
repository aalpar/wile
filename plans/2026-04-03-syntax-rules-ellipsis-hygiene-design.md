# syntax-rules Ellipsis and Hygiene Bug Fixes

**Date:** 2026-04-03
**Status:** Bugs B+C complete. Bug A: validator partially fixed (Tasks 1-2, to be revised
in Task 4 for consistency), environment layer designed (Tasks 3-5).

## Background

SRFI-42 (Eager Comprehensions), a pure `define-syntax`/`syntax-rules` library, was used
as a diagnostic tool against Wile's macro expander. Three bugs were found in template
expansion. See `plans/SRFI-42-SYNTAX-BUGS.md` for full diagnostic results.

## Bug A: Scope-aware duplicate binding detection

**Root cause:** `internal/validate/validate_let.go:390-401` (and 555-565) checks for
duplicate `let` binding names using bare string comparison (`ns.Sym.Key`), ignoring
scope sets. Two hygienically distinct identifiers with the same printed name are
incorrectly flagged as duplicates.

**Current fix (to be revised):** Uses `bindingIdentity{key, scopeFingerprint}` with
string-based scope ID comparison. This works but introduces a second scope resolution
mechanism (fingerprint equality) alongside the environment's `scopesCompatible` /
`ScopesMatch` (subset check). The two mechanisms answer different questions and could
diverge.

**Revised fix:** Replace `bindingIdentity` + `scopeFingerprint` with pairwise
`scopesCompatible` checks using the same function the environment uses. Two bindings
in the same `let` are duplicates iff they have the same string key AND are
scope-compatible (one's scopes are a subset of the other's — meaning they would
resolve to the same binding under Flatt's model). Two bindings that are not
scope-compatible are distinct even if they share a printed name.

This eliminates `bindingIdentity`, `scopeFingerprint`, and the `map[bindingIdentity]bool`
pattern from the validator entirely. The duplicate check becomes:

```go
for i, a := range nameSyms {
    for j := i + 1; j < len(nameSyms); j++ {
        b := nameSyms[j]
        if a.Sym.Key == b.Sym.Key && scopesCompatible(a.Scopes(), b.Scopes()) {
            // duplicate
        }
    }
}
```

Where `scopesCompatible` is the same function used by the environment's
`resolveLocal`. This can be imported from `environment/` or factored into a
shared location (e.g., `syntax.ScopesCompatible`).

**Files:** `internal/validate/validate_let.go` (four sites), `validate_define.go` (two sites)

**Risk:** The validator fix is necessary but not sufficient. The environment storage
layer has the same bare-key problem — see "Bug A: Environment Layer" below.

## Bug A: Environment Layer — same-frame scope-distinct bindings

### Problem

The validator fix (using `bindingIdentity` with scope fingerprints) allows two
hygienically distinct `x` bindings through validation. But the environment storage
layer cannot represent them: `LocalEnvironmentFrame.keys` is `map[values.Symbol]int`,
keyed by `Symbol{Key string}` — a single slot per string key per frame.

### Failure chain

Trace for `(let ((x_scope1 1) (x_scope2 2)) x_scope2)` after macro expansion:

1. **Validation** (fixed): `bindingIdentity{key:"x", scopeKey:"3"}` and
   `bindingIdentity{key:"x", scopeKey:"7"}` are distinct. Passes.

2. **Compilation — binding creation** (`compile_let.go:149`):
   ```
   MaybeCreateLocalBindingWithScopes(Symbol{"x"}, ..., scopes1, ...)  → slot 0, created=true
   MaybeCreateLocalBindingWithScopes(Symbol{"x"}, ..., scopes2, ...)  → slot 0, created=false
   ```
   Second call hits `p.local.keys[Symbol{"x"}]` (`environment_frame.go:518`)
   which returns slot 0. Returns the existing binding. Both `x`s share slot 0.

3. **Compilation — symbol resolution** (`CompileSymbol` → `GetLocalIndexWithScopes`):
   `resolveLocal` at `environment_frame.go:382` does `env.local.keys[Symbol{"x"}]`
   → slot 0. Binding at slot 0 has `scopes1`. If looking up `x_scope2`,
   `scopesCompatible(binding, scopes2)` may or may not match depending on scope
   subset relations. Even if it matches, it's the wrong binding.

4. **Runtime**: `OpStoreLocal` for both init expressions writes to slot 0. Second
   overwrites first. `OpLoadLocal` for either `x` reads slot 0 — always gets the
   second init value regardless of which `x` is referenced.

### Fix: Unified scoped bindings

**Principle:** One form for scoped bindings. No scope-aware vs scope-unaware
split. Nil scopes means "match any." Optimize for runtime; compile-time and
expansion-time cost is acceptable.

#### Storage: `map[Symbol][]int`

Change `LocalEnvironmentFrame.keys` from `map[Symbol]int` to `map[Symbol][]int`.
Each string key maps to a slice of slot indices. Common case: one element.
Multi-slot only when hygienic expansion creates same-name bindings with different
scope sets in the same frame.

#### Resolution: drop `checkScopes`, always check scopes

`resolveLocal` currently takes a `checkScopes bool`. Remove it. Always check
scope compatibility. `nil` use-scopes means "match any" (replaces `false`):

```go
func scopesCompatible(binding *Binding, scopes []*syntax.Scope) bool {
    if scopes == nil {
        return true  // nil = match any (no scope constraint)
    }
    bs := binding.Scopes()
    if len(bs) == 0 {
        return true  // binding with no scopes matches any reference
    }
    return syntax.ScopesMatch(scopes, bs)
}

func (p *EnvironmentFrame) resolveLocal(
    key *values.Symbol,
    scopes []*syntax.Scope,
    visitor func(binding *Binding, slot int, depth int) any,
) any {
    env := p
    depth := 0
    for env != nil && env.hasLocal() {
        for _, i := range env.local.keys[*key] {
            binding := &env.local.bindings[i]
            if scopesCompatible(binding, scopes) {
                result := visitor(binding, i, depth)
                if result != nil {
                    return result
                }
            }
        }
        if env.IsTopLevel() { break }
        env = env.parent
        depth++
    }
    return nil
}
```

#### API unification

Eliminate the scope-aware/scope-unaware pairs. Each function takes scopes;
nil means "match any."

| Before (two forms) | After (one form) |
|---------------------|------------------|
| `GetLocalIndex(key)` | `GetLocalIndex(key, nil)` |
| `GetLocalIndexWithScopes(key, scopes)` | `GetLocalIndex(key, scopes)` |
| `GetBinding(key)` | `GetBinding(key, nil)` |
| `GetBindingWithScopes(key, scopes)` | `GetBinding(key, scopes)` |
| `MaybeCreateLocalBinding(key, bt)` | `MaybeCreateLocalBinding(key, bt, nil, nil)` |
| `MaybeCreateLocalBindingWithScopes(key, bt, scopes, src)` | `MaybeCreateLocalBinding(key, bt, scopes, src)` |

Callers that have scopes (via `SyntaxSymbol`) pass them directly. Callers
without scopes pass nil. No dispatch pattern, no branching on scope presence.

The `CompileSymbol` fast path (`compile_time_continuation.go:188-196`) that
branches on `len(symbolScopes) == 0` becomes a single call:
`p.env.GetLocalIndex(sym, symbolScopes)`. Nil and empty scopes both work.

`compile_let.go:80,96,116` become
`childEnv.GetLocalIndex(b.Name.Sym, b.Name.Scopes())`.

#### Creation: scope-aware deduplication

`MaybeCreateLocalBinding` checks existing slots for scope compatibility before
creating a new one:

```go
func (p *EnvironmentFrame) MaybeCreateLocalBinding(
    key *values.Symbol, bt BindingType,
    scopes []*syntax.Scope, source *syntax.SourceContext,
) (*LocalIndex, bool) {
    slots := p.local.keys[*key]
    for _, i := range slots {
        if scopesCompatible(&p.local.bindings[i], scopes) {
            // update metadata if needed
            return NewLocalIndex(i, 0), false
        }
    }
    // No compatible binding — new slot
    i := len(p.local.bindings)
    p.local.keys[*key] = append(slots, i)
    p.local.bindings = append(p.local.bindings, ...)
    return NewLocalIndex(i, 0), true
}
```

#### Runtime performance

`LocalEnvironmentFrame.keys` is mutated at compile time only. At runtime,
`copyForApplyInto` shares the keys map via CoW. The `[]int` slices are never
mutated at runtime. No additional allocation. The common-case `[]int` iteration
(one element) is negligible. The `nil`-scopes fast exit in `scopesCompatible`
is a single pointer comparison.

#### Scope of change

| Area | Call sites | Change |
|------|-----------|--------|
| `environment/` | `resolveLocal`, `scopesCompatible`, `LocalEnvironmentFrame.keys`, 6 public functions | Storage + API unification |
| `machine/compilation/` | ~20 callers of `GetLocalIndex`/`GetBinding`/etc. | Add scopes arg (most already have it; rest pass nil) |
| `environment/*_test.go` | Signature updates + new multi-slot tests | Mechanical |

Not affected: `GlobalEnvironmentFrame.keys` — globals don't have same-frame
scope collisions.

## Bug B: Cross-group ellipsis zipping

**Root cause:** `findMatchingEllipsisID` in `internal/match/match.go:492-540` returns a
single ellipsis ID. When template variables span multiple groups (e.g., `(cons a b) ...`
where `a` is from group 0, `b` from group 1), no single ID contains both. Falls back to
first partial match; second group's variables are treated as free identifiers.

**Fix — Step 1:** New function `findMatchingEllipsisIDs` (plural) that returns all IDs
contributing variables to the template expression. Single-group case returns `[]int{id}`
(common path unchanged).

**Fix — Step 2:** In `expandSyntaxEllipsis` (`syntax_expand.go:370-438`), when multiple
IDs are returned:

1. Validate equal iteration counts across all groups. Unequal counts are an expansion
   error per R7RS (repetition counts must match).
2. For each iteration `k`, build a temporary `captureContext` merging bindings from
   `ctx.children[id0][k]`, `ctx.children[id1][k]`, etc. Union the `bindings` maps;
   propagate `children` from whichever child has them.
3. Iterate the merged contexts using the existing expansion path.

Single-ID path is unchanged — zero allocation overhead for the common case.

**Files:** `internal/match/match.go`, `internal/match/syntax_expand.go`

**Risk:** Medium. The merge must handle overlapping variable names (shouldn't occur in
well-formed patterns; SRFI-42 groups are disjoint by construction). Guard with an
assertion during development.

## Bug C: Nested ellipsis depth annotation

**Root cause:** For pattern `((a ...) ...)`, the compiler assigns ID 0 (inner) and
ID 1 (outer). Both capture variable `a`. During template expansion of `(list a ...) ...`,
`findMatchingEllipsisID({a})` returns ID 0 (first match, sorted order), so the expander
iterates only the inner captures from the first outer iteration. The outer iterations
stored in `ctx.children[1]` are never traversed.

**Fix — Step 1:** Track nesting depth during pattern compilation. Add `ellipsisDepth`
counter to `SyntaxCompiler`, incremented on entering nested ellipsis, decremented on
exit. Store depth with each ID:

```go
ellipsisDepths map[int]int  // ellipsisID -> nesting depth (0 = innermost)
```

For `((a ...) ...)`: inner ID 0 → depth 0, outer ID 1 → depth 1.
For `(a ... b ...)`: both ID 0 and ID 1 → depth 0 (siblings, not nested).

**Fix — Step 2:** Depth-aware ID selection in template expansion. When multiple IDs
match the same variables at different depths, select the highest-depth ID first (the
outermost). The template is processed outside-in: the outer `...` expands first using
`ctx.children[outerID]`, then the inner `...` expands within each child using
`childCtx.children[innerID]`.

The capture tree already has the correct hierarchical shape from matching. The fix only
changes which ID is selected during expansion.

**Fix — Step 3:** Thread depth metadata from `CompiledPattern` through `Matcher` to the
expander. Add `ellipsisDepths` field alongside existing `ellipsisVars`.

**Files:** `internal/match/syntax_compiler.go`, `internal/match/match.go`,
`internal/match/syntax_expand.go`

**Risk:** Medium-high. Most complex change. Interaction with Bug B's cross-group zipping
needs care — cross-group applies to same-depth siblings, depth annotation applies to
nested groups. The two are orthogonal but share the ID selection code path.

## Implementation order

A → B → C. Increasing complexity, decreasing SRFI-42 impact. Each fix is independently
testable and shippable.

## Testing strategy

### Unit tests (`internal/match/`)

**Bug B** (`syntax_expand_test.go`):
- Cross-group: pattern `(m (a ...) (b ...))`, template `(list (cons a b) ...)` → zipped
- Mismatched counts: different-length inputs → expansion error
- Three groups: `(m (a ...) (b ...) (c ...))`, template `(list a b c) ...`

**Bug C** (`syntax_expand_test.go`):
- Nested: pattern `(m ((a ...) ...))`, template `(list (list a ...) ...)` → nested output
- Mixed nested+flat: `(m ((a ...) ...) (b ...))` → correct outer iteration

**Bug A** (`validate_test.go`):
- Same key, different scopes → no error
- Same key, same scopes → still errors

### Integration tests

- Bug A repro: `(do-ec (:parallel (:range i 3) (:string ch "abc")) ...)`
- Bug B repro: `(:generator-proc (:range 5))`
- Bug C repro: `(m ((1 2) (3 4)))` with nested ellipsis macro
- End-to-end: full SRFI-42 load, `(list-ec (: i 5) (* i i))` → `(0 1 4 9 16)`

### Invariant

No existing tests break. These fixes make previously-rejected valid programs work.
