# Staff Engineer Technical Debt Assessment

**Assessment Date**: 2026-02-14 (updated)
**Scope**: Recently changed files (last 10 commits)
**Files Analyzed**: 35 files across `values/`, `registry/core/`, `machine/`, `internal/extensions/io/`, `internal/parser/`

---

## Executive Summary

The recently changed files reflect a mature codebase with strong architectural discipline (overflow-safe arithmetic, hygiene-aware macros, R7RS compliance). The dominant debt is **structural duplication** in the numeric tower, where a well-designed abstraction (the Number interface) is undermined by 28 hand-rolled type switches. This is the classic "missing abstraction" smell — the lattice model exists conceptually but isn't reified into data.

Completed items have been removed. See git history for the original document. Remaining debt is LOW priority (numeric tower deferred, EqualTo symmetry deferred, reflection-based equality acceptable).

---

## Prioritized Findings

### [Priority: High] — Numeric Tower Type-Switch Explosion (Deferred)

**Where**: `values/{integer,rational,complex,big_integer,big_float,big_complex}.go` lines 169-300 (Add/Subtract/Multiply/Divide methods)

**What**: Each of the 7 numeric types implements 4 arithmetic operations with nearly identical type-switch dispatch over all 7 types. This is ~28 methods containing ~200 lines each of duplicated logic. The pattern is:
```go
switch v := o.(type) {
case *Integer: /* operation */
case *BigInteger: /* operation */
case *Float: /* operation */
// ... 4 more cases
}
```

**Why it matters**:
- **Maintenance burden scales quadratically**: Adding a new numeric type (e.g., `Decimal`) requires touching 7 files × 4 operations = 28 methods
- **Bug fix propagation**: Fixing a promotion rule (e.g., exact zero domination in multiplication) requires identical edits in 7 places
- **Test coverage explosion**: Numeric tower coverage tests already enumerate 49 type combinations (`values/numeric_tower_coverage_test.go`)

**Suggested fix**: Extract a lattice-based dispatch table mapping `(TypeA, TypeB) → promoter func`. Each type's arithmetic methods become lookups + delegation.

**Effort**: L (3-5 days for design + implementation + comprehensive testing)

---

### [Priority: Medium] — EqualTo Symmetry Requires N×N Awareness (Deferred)

**Where**: `values/integer.go:470-480`, `rational.go:402-412`, `big_integer.go` (similar)

**What**: Each numeric type's `EqualTo` method must handle comparisons with related types:
```go
func (p *Integer) EqualTo(v Value) bool {
    switch other := v.(type) {
    case *Integer: /* ... */
    case *BigInteger: /* symmetry for BigInteger.EqualTo(Integer) */
    case *Rational: /* symmetry for Rational.EqualTo(Integer) */
    }
}
```

**Why it matters**:
- **Symmetry breakage risk**: If `A.EqualTo(B)` is implemented but `B.EqualTo(A)` is not, equality is asymmetric
- **Extension friction**: Adding `Decimal` requires updating Integer, Rational, and Float to recognize it
- **No compile-time enforcement**: Go doesn't enforce symmetry; relies on manual cross-referencing

**Suggested fix**: Centralize equality in a type-agnostic dispatcher using `Number.Compare()`. Each type only implements `Compare` (already exists); equality becomes `a.Compare(b) == 0`.

**Effort**: M (1-2 days for refactor + test verification)

---

## Assessment Methodology

**Files Analyzed**: 35 files from `git diff --name-only HEAD~10`
**Focus Areas**:
- Structural debt (missing abstractions, divergent patterns)
- Consistency debt (naming drift, convention violations)
- Testing debt (missing coverage, brittle tests)
- Evolution risk (hardcoded assumptions, scaling walls, extension friction)

**Priority Criteria**:
- **High**: Actively blocking features, spreading, or causing bugs
- **Medium**: Slowing development but not breaking anything today
- **Low**: Aesthetic or minor — fix opportunistically

---

# Full Codebase Technical Debt Assessment

**Assessment Date**: 2026-02-14 (updated)
**Scope**: Entire codebase (605 Go source files, ~57K lines)
**Methodology**: Systematic review across all packages for structural debt, consistency issues, testing gaps, and evolution risks

---

## Executive Summary

The Wile codebase is **architecturally sound with recent correctness improvements** (10 HIGH, 2 MEDIUM bugs fixed in last architectural review). The core packages (values, machine, environment, syntax) have 85-93% test coverage and follow consistent error handling conventions (~772 wrapped errors, ~2 bare errors remaining as panic-recovery wrappers). The macro system's three-layer architecture (pattern matching VM + syntax adapter + hygienic layer) is well-factored and documented.

Completed items have been removed. See git history for the original document. Remaining debt: numeric tower type-switch explosion (deferred indefinitely), EqualTo symmetry (deferred), numeric type switch incompleteness (deferred), reflection-based operation equality (accepted trade-off).

---

## LOW PRIORITY

### [Priority: Low] — Numeric Type Switch Incompleteness: Interfaces vs Concrete Types (Deferred)

**Where**: Multiple files with type switches on numbers (e.g., `registry/core/prim_arithmetic.go`, `values/numeric_tower.go`)

**What**: Some type switches use `case values.ComplexNumber:` (interface, matches both `*Complex` and `*BigComplex`) while others use `case *values.Complex:` / `case *values.BigComplex:` (concrete types, must list both). The recent H6 fix (`real?` predicate) showed how using concrete types missed `*Complex`. The inconsistency is a latent bug pattern.

**Why it matters**: When adding a new numeric type (e.g., `*Decimal` for R7RS-large), every type switch that uses concrete types must be updated. Type switches using interfaces automatically handle the new type if it implements the interface.

**Suggested fix**: Lint rule: type switches on numeric values must use interface types (`values.Number`, `values.RealNumber`, `values.ComplexNumber`) unless concrete type dispatch is semantically required. Fix existing switches to use interfaces where possible.

**Effort**: S (1-2 days to audit and fix)

---

### [Priority: Low] — VM Operation Equality: Reflection-Based Comparison Instead of Code Generation

**Where**: `machine/operation_helpers.go:10-40` (`sameType`, `fieldMatches`)

**What**: All operation `EqualTo` methods use reflection helpers to compare struct fields generically. This is slower than direct field comparison and harder to debug (reflection panics are opaque). The helpers were added to avoid hand-writing equality checks for ~32 operation types, but the tradeoff is performance and debuggability.

**Why it matters**: Operation equality is used in test assertions and potentially in optimization passes (detecting redundant operations). The reflection overhead is small but unnecessary—operations are compared frequently during compilation.

**Suggested fix**: Either accept the reflection cost (it's negligible for compilation workloads) or generate `EqualTo` methods via `go generate` using AST inspection. Do NOT hand-write ~32 equality methods.

**Effort**: S (if accepting status quo) / M (if pursuing code generation)

---

## Top Priority (Recommended Action)

All HIGH and MEDIUM items are resolved. Remaining items are LOW priority with no immediate action needed.

---

## Codebase Metrics

| Metric | Value |
|--------|-------|
| Total Go source files | ~615 |
| Total test files | ~302 |
| struct types | 309 |
| Foreign functions (primitives) | 505 |
| Error wrapping sites | ~772 |
| Bare errors (fmt.Errorf/errors.New) | ~2 (panic-recovery wrappers only) |
| context.TODO() uses | 4 production, ~408 tests (see [#233](https://github.com/aalpar/wile/issues/233)) |
| TODO/FIXME comments | 18 (12 production, 6 test) — original count of 507 included `context.TODO()` calls |
| Overall test coverage | 85.3% (root), 85-93% (core packages) |

| Package | Coverage | Notes |
|---------|----------|-------|
| `registry/helpers` | 86.7% | Fixed (commit `c20df47`) |
| `cmd/scheme` | 75.0% | User-facing CLI (was 9.9%, PR #228) |
| `internal/extensions/io` | 91.1% | I/O boundary (was 55.2%, PR #225) |
| `internal/extensions/math` | 68.0% | Complex arithmetic |
| `internal/extensions/eval` | 73.4% | Meta-evaluation |
| `values` | ~85% | Core numeric tower |
| `machine` | 85.0% | VM and compiler |
| `environment` | 90.4% | Symbol resolution |
| `registry/core` | 88.5% | Standard library |
| `internal/extensions/gointerop` | 96.4% | Go FFI |
| `internal/extensions/system` | 100.0% | System interface |

