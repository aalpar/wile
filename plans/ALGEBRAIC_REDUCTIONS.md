# Algebraic Reductions

Structural simplifications: multiple expressions of the same underlying operation collapsed into one.

See also `REFACTORING_OPPORTUNITIES.md` for previously identified reductions (form-checking predicates, CxR factory, compile-time argument extraction, eval-at-compile-time, type assertion helpers).

---

## Completed

- **Port Base Type** — `values/port_base.go` now provides `portBase` with `closed`, `clsr`, `Close()`, `IsClosed()`, `guardClosed()`. All 10 port types embed it.
- **Binding Lookup Duplication** — `environment/environment_frame.go` now has `resolveLocal()` and `resolveGlobal()` as shared walk functions. `GetBinding`, `GetBindingWithScopes`, `GetLocalIndex` are thin wrappers. `GetLocalIndexWithScopes` has its own walk (Flatt's collect-then-maximize algorithm requires cross-frame candidate accumulation).
- **Optional Range Argument Parsing (VII)** — Already complete. `helpers.ParseSubrange` exists and is used by all 7 primitives: `vector->list`, `vector-copy`, `vector-fill!`, `bytevector-copy`, `bytevector-copy!`, `string->list`, `string-copy`.
- **Chain Equality Predicates (VIII)** — `helpers.ChainEquality()` consolidates variadic chain comparison. `PrimBooleanEq` and `PrimSymbolEq` now use shared helper. Reduced ~30 lines of duplication per primitive to ~10 lines each.
- **Structural Equality Cycle Detection (II)** — `compareIndexable[T]()` generic helper in `values/utils.go` consolidates cycle detection between `vectorEqualToDeep` and `arrayListEqualToDeep`. `pairEqualToDeep` kept separate for improper list handling.
- **Bootstrap Environment Initialization (X)** — `initializeEnvironment()` helper in `internal/bootstrap/environment_tiny.go` consolidates common sequence (registry creation, extension loading, compiler/expander registration, bootstrap macro loading) for both `NewTopLevelEnvironmentFrameTiny` and `NewLibraryEnvironmentFrame`.

## Intentionally Not Consolidated

- **ForEach / SyntaxForEach (III)** — Different return types (`values.Value` vs `SyntaxValue`) make consolidation more complex than the duplication it eliminates. Go's type system constraints outweigh benefits; code is clearer as-is.

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

**Independence**: Remaining reductions are independent. VI may benefit from the completed binding lookup unification.

**Risk ordering**: All low-risk items (II, VII, VIII, X) are complete. Item III intentionally not consolidated. Remaining items start at Low-Medium risk.

**Testing**: Every reduction must pass `go test ./... -count=1`. The numeric tower reduction should additionally run the R7RS numeric test suite to verify exactness preservation and tower promotion semantics.
