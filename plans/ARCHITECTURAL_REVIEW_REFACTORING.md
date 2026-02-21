# Architectural Review — Remaining Refactoring

Updated: 2026-02-21

Completed items have been removed. See git history for the original document.

---

## Tier 2: Systemic Duplication (HIGH impact)

### 2.1 Numeric Tower Type-Switch Copy-Paste — DEFERRED INDEFINITELY

Multiple attempts to unify the numeric tower dispatch have failed. The 7x7
type-switch matrix across arithmetic methods is deeply entangled with
promotion semantics, exactness contagion, and special-case handling. Each
attempt introduced subtle regressions. Do not attempt further unification
without a concrete design validated against the full R7RS numeric edge-case suite.

**Scope:** 7 files, ~2,100 lines
**Files:** `values/{integer,big_integer,float,big_float,rational,complex,big_complex}.go`

---

## Tier 4: Organizational (LOW impact)

### 4.2 Validator Prologue Duplication

**Scope:** 19 validators
**Files:** `internal/validate/validate_*.go`
**Effort:** Low

All 19 validators repeat the same `collectList` + `improper` check + arity guard prologue (~4 lines each). A `validateFormPrologue()` helper would deduplicate.

### 4.4 Optional Fill Argument Extraction

**Scope:** 3 sites
**Files:** `registry/core/prim_vectors.go` (PrimMakeVector), `registry/core/prim_byte_vectors.go` (PrimMakeBytevector), `registry/core/prim_strings.go` (PrimMakeString)
**Effort:** Low

Three `make-*` primitives each independently extract optional fill arguments with slightly different patterns. Could share a helper.
