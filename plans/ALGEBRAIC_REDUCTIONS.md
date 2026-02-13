# Algebraic Reductions

Structural simplifications: multiple expressions of the same underlying operation collapsed into one.

---

## IV. Syntax Interface Boilerplate

8 syntax types repeat identical `SourceContext()`, `IsVoid()`, `UnwrapAll()`. Extract `syntaxBase` embedded struct. (`IsVoid` can't be defaulted via embedding due to nil receiver semantics.)

### Files

`internal/syntax/syntax_symbol.go`, `syntax_pair.go`, `syntax_vector.go`, `syntax_value.go`, `syntax_comment.go`, `syntax_datum_comment.go`, `syntax_datum_label.go`, `syntax_directive.go`

---

## V. Operation Interface Boilerplate

~27 VM operation types repeat identical `SchemeString()`, `IsVoid()`, `EqualTo()`. `String()` is not universal — some types have it, some don't. `sameType`/`fieldMatches` helpers in `operation_helpers.go` already partially abstract `EqualTo`.

### Files

`operation_*.go` files in `machine/`.

---

## VI. Scope-Aware Symbol Resolution

**Status**: Deferred — duplication serves distinct architectural purposes

Compiler (`compile_time_continuation.go`) and expander (`expander_time_continuation.go`) both duplicate scope-aware binding resolution with the same if-no-scopes/else-with-scopes branch. Investigation confirmed the duplication exists but serves different purposes: the compiler uses it for pre-lookup dispatch optimization (choose fast vs slow lookup), while the expander uses it for post-lookup correctness checking (does this binding shadow a macro?). Both implement Flatt's hygiene rule (`bindingScopes ⊆ useScopes`) but at different pipeline stages for different concerns (performance vs correctness).

Consolidation would require careful abstraction to preserve both semantic clarity and performance. Deferred pending a clearer approach or additional motivation from other refactoring work.

### Files

`machine/compile_time_continuation.go`, `machine/expander_time_continuation.go`

### See Also

`plans/SCOPE_AWARE_RESOLUTION_INVESTIGATION.md` — detailed investigation findings, semantic analysis, deferral rationale

---

## IX. Scope Propagation Asymmetry

`SyntaxPair.AddScope` (interface dispatch) and `FlipScope` in `scope_utils.go` (concrete type switch) both recursively traverse syntax trees. Same traversal, different per-node operation.

### Files

`internal/syntax/scope_utils.go`, `internal/syntax/syntax_pair.go`

---

## Priority Lattice

Ordered by risk — start from the bottom (safe leaf reductions), work up.

| # | Reduction | Risk | Status |
|---|-----------|------|--------|
| VI | Scope-aware symbol resolution | Medium | Deferred |
| V | Operation base type | Medium | Open |

Numeric tower dispatch is tracked separately in `plans/NUMERIC_TOWER_REFACTORING.md`.

## Implementation Notes

**Deferred items**: Item VI (scope-aware resolution) is deferred. It involves structural duplication where the repeated pattern serves distinct purposes in different contexts. Consolidation would require careful abstraction to avoid obscuring semantic differences or regressing performance. See dedicated plan files for detailed analysis.

**Independence**: All remaining reductions are independent.

**Testing**: Every reduction must pass `go test ./... -count=1`.
