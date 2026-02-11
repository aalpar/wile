# Algebraic Reductions

Structural simplifications: multiple expressions of the same underlying operation collapsed into one.

See also `REFACTORING_OPPORTUNITIES.md` for previously identified reductions (form-checking predicates, CxR factory, compile-time argument extraction, eval-at-compile-time, type assertion helpers).

---

## Completed

- **Port Base Type** — `values/port_base.go` now provides `portBase` with `closed`, `clsr`, `Close()`, `IsClosed()`, `guardClosed()`. All 10 port types embed it.
- **Binding Lookup Duplication** — `environment/environment_frame.go` now has `resolveLocal()` and `resolveGlobal()` as shared walk functions. `GetBinding`, `GetBindingWithScopes`, `GetLocalIndex` are thin wrappers. `GetLocalIndexWithScopes` has its own walk (Flatt's collect-then-maximize algorithm requires cross-frame candidate accumulation).

---

## I. Numeric Tower Dispatch

7 numeric types × 5 arithmetic methods = 35 type switches with 7 cases each. `numeric_tower.go` has `Simplify` and `ExactnessOf` — but no unified dispatch (`BinaryOp`, `Promote` do not exist). A tower-based dispatch would need to be built from scratch. Each type reimplements its own full cross-type dispatch.

### Files

All 7 numeric type files in `values/` + `values/numeric_tower.go`.

---

## II. Structural Equality Cycle Detection

Three cycle-detecting equality functions in `values/utils.go` (`pairEqualToDeep`, `vectorEqualToDeep`, `arrayListEqualToDeep`) share similar visited-set / cycle-guard logic. Vectors and array lists could share element-wise comparison, though `arrayListEqualToDeep` has extra void-checking logic that `vectorEqualToDeep` lacks. Pairs need special handling for improper lists.

### Files

`values/utils.go`

---

## III. ForEach / SyntaxForEach

`internal/syntax/syntax_pair.go`: `ForEach` and `SyntaxForEach` duplicate the same list-spine iteration. Similarly `Append` and `SyntaxAppend`. Only callback type differs.

### Files

`internal/syntax/syntax_pair.go`

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

## VII. Optional Range Argument Parsing

> **Cross-reference**: Described in detail in `CODE_CONSOLIDATION_PLAN.md` Phase 4 (`ParseOptionalRange` helper).

7 primitives parse optional `[start [end]]` with identical ~20-line boilerplate: `vector->list`, `vector-copy`, `vector-fill!`, `bytevector-copy`, `bytevector-copy!`, `string->list`, `string-copy`.

---

## VIII. Chain Equality Predicates

`boolean=?` and `symbol=?` in `registry/core/prim_equality.go` implement identical variadic chain comparison (validate type → loop → short-circuit).

### Files

`registry/core/prim_equality.go`

---

## IX. Scope Propagation Asymmetry

`SyntaxPair.AddScope` (interface dispatch) and `FlipScope` in `scope_utils.go` (concrete type switch) both recursively traverse syntax trees. Same traversal, different per-node operation.

### Files

`internal/syntax/scope_utils.go`, `internal/syntax/syntax_pair.go`

---

## X. Bootstrap Environment Initialization

`internal/bootstrap/` contains `NewTopLevelEnvironmentFrameTiny()` and `NewLibraryEnvironmentFrame()` which share sequential initialization steps (registry creation, extension loading, compiler/expander registration, bootstrap macro loading). Only the initial environment creation differs.

### Files

`internal/bootstrap/`

---

## Priority Lattice

Ordered by risk — start from the bottom (safe leaf reductions), work up.

| # | Reduction | Risk |
|---|-----------|------|
| VII | Optional range parsing | Low |
| VIII | Chain equality | Low |
| II | Equality cycle detection | Low |
| III | ForEach/SyntaxForEach | Low |
| X | Bootstrap initialization | Low |
| IV | Syntax interface boilerplate | Low-Medium |
| IX | Scope propagation | Low-Medium |
| VI | Scope-aware symbol resolution | Medium |
| V | Operation base type | Medium |
| I | Numeric tower dispatch | High |

## Implementation Notes

**Independence**: Most reductions are independent. VI may benefit from the completed binding lookup unification.

**Risk ordering**: Start with leaf reductions (VII, VIII, II) that touch few files and have obvious correctness. The numeric tower (I) is highest-value but highest-risk — it changes core arithmetic dispatch.

**Testing**: Every reduction must pass `go test ./... -count=1`. The numeric tower reduction should additionally run the R7RS numeric test suite to verify exactness preservation and tower promotion semantics.
