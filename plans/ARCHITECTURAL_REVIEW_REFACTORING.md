# Architectural Review — Refactoring Opportunities

Updated: 2026-02-16

## Overview

Full-codebase architectural review covering: numeric tower, registry/core primitives, machine package, error handling, internal packages, and values/API surface. Findings are organized by priority tier.

Completed items have been removed. See git history for the original document.

---

## Tier 2: Systemic Duplication (HIGH impact)

### 2.1 Numeric Tower Type-Switch Copy-Paste — DEFERRED

> **DEFERRED INDEFINITELY**
>
> Multiple attempts to unify the numeric tower dispatch have failed. The 7x7
> type-switch matrix across arithmetic methods is deeply entangled with
> promotion semantics, exactness contagion, and special-case handling (e.g.,
> `(* 0 +inf.0)`). Each attempt introduced subtle regressions or required
> so much parameterization that the "unified" version was harder to reason
> about than the copy-paste it replaced.
>
> **This item is deferred until a clean abstraction emerges.** Do not
> attempt further unification without a concrete design that has been
> validated against the full R7RS numeric edge-case suite first.

**Scope:** 7 files, ~2,100 lines
**Files:** `values/{integer,big_integer,float,big_float,rational,complex,big_complex}.go`

**Known issues (for reference, NOT currently actionable):**
- Inconsistent case ordering across methods and types
- Conversion helpers reimplemented per-type rather than centralized
- Same 7-branch type switch copy-pasted across all arithmetic methods

---

## Tier 4: Organizational (LOW impact)

### 4.2 Validator Prologue Duplication

**Scope:** 19 validators
**Files:** `internal/validate/validate_*.go`
**Effort:** Low

All 19 validators repeat the same `collectList` + `improper` check + arity guard prologue (~4 lines each). The list collection step uses a shared `collectList()` helper, but the error-reporting boilerplate (improper-list check + arity guard) is still duplicated. A `validateFormPrologue()` helper would deduplicate.

### 4.4 Optional Fill Argument Extraction

**Scope:** 3 sites
**Files:** `registry/core/prim_vectors.go` (PrimMakeVector), `registry/core/prim_byte_vectors.go` (PrimMakeBytevector), `registry/core/prim_strings.go` (PrimMakeString)
**Effort:** Low

Three `make-*` primitives each independently extract optional fill arguments with slightly different patterns. Could share a helper.

---

## What's Working Well (Preserve)

- **Registration system** — Extension registration is clean, consistent, single source of truth
- **Error propagation** — Properly wrapped error sites, zero silent ignores
- **Public API** — `wile.Value` wrapper isolates internals, error types are structured, FFI is type-safe
- **Stack manipulation** — Clean, encapsulated, no duplication
- **Syntax utilities** — `SyntaxForEach`, `SyntaxList`, etc. are well-abstracted
- **Bootstrap initialization** — Centralized, correct ordering
- **BoolToBoolean adoption** — Production code fully converted
- **Constructor patterns** — All types use consistent `NewX()` pattern
- **SchemeString output** — R7RS-compliant across all types
- **Validator error accumulation** — Centralized via ValidationResult
- **Two-layer error convention** — Sentinel + wrap pattern enforced across ~80 call sites
- **fmt.Errorf eliminated** — No `fmt.Errorf` in production code
- **BigInteger Hashable** — Now implements `HashCode()`
- **Bare sentinels eliminated** — All `NewForeignError` calls now use sentinels (PR #217)
- **Test error idioms** — `errors.Is(err, io.EOF)` consistent across test and production code
- **Optional position parsing** — `helpers.ParseSubrange` consolidates `[start [end]]` extraction
