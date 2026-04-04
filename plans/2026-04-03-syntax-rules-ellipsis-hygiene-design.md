# syntax-rules Ellipsis and Hygiene Bug Fixes

**Date:** 2026-04-03
**Status:** Bugs B+C complete. Bug A partial — validator fixed, environment layer deferred.

## Background

SRFI-42 (Eager Comprehensions), a pure `define-syntax`/`syntax-rules` library, was used
as a diagnostic tool against Wile's macro expander. Three bugs were found in template
expansion. See `plans/SRFI-42-SYNTAX-BUGS.md` for full diagnostic results.

## Bug A: Scope-aware duplicate binding detection

**Root cause:** `internal/validate/validate_let.go:390-401` (and 555-565) checks for
duplicate `let` binding names using bare string comparison (`ns.Sym.Key`), ignoring
scope sets. Two hygienically distinct identifiers with the same printed name are
incorrectly flagged as duplicates.

**Fix:** Change the `seen` map key from `string` to a struct incorporating scope identity:

```go
type bindingIdentity struct {
    key      string
    scopeKey string // deterministic string from sorted scope IDs
}
```

Build the scope fingerprint from `ns.Scopes()` — sort by `Scope.ID()`, join as string.
Empty scopes produce empty string (backwards compatible).

**Files:** `internal/validate/validate_let.go` (two sites)

**Risk:** Low. The environment already handles scope-distinct bindings correctly
(`MaybeCreateLocalBindingWithScopes`). Only the validation guard is wrong.

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
ellipsisDepths map[int]int  // ellipsisID -> compilation order (lower = inner)
```

The compiler assigns IDs and order values sequentially during depth-first traversal
(inner patterns are compiled before outer patterns). For `((a ...) ...)`: inner ID 0
→ order 0, outer ID 1 → order 1. Siblings like `(a ... b ...)` get different order
values (0 and 1) but this is harmless: siblings capture different variables, so the
order comparison in `findMatchingEllipsisIDs` never fires for them.

**Fix — Step 2:** Order-aware ID selection in template expansion. When multiple IDs
match the same variables, select the one with the highest compilation order (the
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
