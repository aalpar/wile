# Numeric Tower Refactoring

**Status**: Deferred — intentional architecture, not recommended for runtime indirection

Consolidated from `ALGEBRAIC_REDUCTIONS.md` (Section I) and `SUBSYSTEM_SIMPLIFICATION.md` (deleted — completed).

---

## Current Architecture

7 numeric types × 5 arithmetic methods = 35 type switches with 7 cases each (332 type-switch branches total). `numeric_tower.go` has `Simplify` and `ExactnessOf` — but no unified dispatch (`BinaryOp`, `Promote` do not exist). Each type reimplements its own full cross-type dispatch.

### Files

All 7 numeric type files in `values/` + `values/numeric_tower.go`.

---

## Architectural Decision: Direct Dispatch Over DRY

The 332 type-switch branches across 7 numeric types are **intentional** architecture. The previous `Tower*` dispatch layer was removed because it added indirection without benefit. The N×N dispatch is the explicit design choice — directness over DRY.

Do not reintroduce a dispatch table. If the repetition becomes painful (e.g., adding an 8th numeric type), consider codegen from a specification table rather than runtime indirection.

---

## If Revisited

A tower-based dispatch would need to be built from scratch. Any consolidation must:

- Pass `go test ./... -count=1`
- Run the R7RS numeric test suite to verify exactness preservation and tower promotion semantics
- Preserve the exact zero exception: `(* 0 x)` returns exact 0 even when x is inexact (Chez behavior, R7RS §6.2.2 permission)
- Maintain number type switch order: Integer → BigInteger → Float → BigFloat → Rational → Complex → BigComplex

The preferred approach if repetition becomes painful is **codegen from a specification table** rather than runtime indirection.
