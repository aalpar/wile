# Algebraic Reductions

Structural simplifications: multiple expressions of the same underlying operation collapsed into one.

See also `REFACTORING_OPPORTUNITIES.md` for previously identified reductions (form-checking predicates, CxR factory, compile-time argument extraction, eval-at-compile-time, type assertion helpers).

---

## I. Numeric Tower Dispatch

7 numeric types × 5 arithmetic methods = 35 type switches with 7 cases each. `numeric_tower.go` has `Simplify` and `ExactnessOf` — but no unified dispatch (`BinaryOp`, `Promote` do not exist). A tower-based dispatch would need to be built from scratch. Each type reimplements its own full cross-type dispatch.

### Files

All 7 numeric type files in `values/` + `values/numeric_tower.go`.

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

Compiler (`compile_time_continuation.go`) and expander (`expander_time_continuation.go`) both duplicate scope-aware binding resolution with the same if-no-scopes/else-with-scopes branch. The binding lookup unification (now complete) may have reduced this, but the compiler/expander duplication likely remains.

### Files

`machine/compile_time_continuation.go`, `machine/expander_time_continuation.go`

---

## IX. Scope Propagation Asymmetry

`SyntaxPair.AddScope` (interface dispatch) and `FlipScope` in `scope_utils.go` (concrete type switch) both recursively traverse syntax trees. Same traversal, different per-node operation.

### Files

`internal/syntax/scope_utils.go`, `internal/syntax/syntax_pair.go`

---

## Priority Lattice

Ordered by risk — start from the bottom (safe leaf reductions), work up.

| # | Reduction | Risk |
|---|-----------|------|
| IV | Syntax interface boilerplate | Low-Medium |
| IX | Scope propagation | Low-Medium |
| VI | Scope-aware symbol resolution | Medium |
| V | Operation base type | Medium |
| I | Numeric tower dispatch | High |

## Implementation Notes

**Independence**: All remaining reductions are independent.

**Risk ordering**: Start with Low-Medium risk items (IV, IX), then Medium risk (V, VI), then High risk (I).

**Testing**: Every reduction must pass `go test ./... -count=1`. The numeric tower reduction should additionally run the R7RS numeric test suite to verify exactness preservation and tower promotion semantics.
