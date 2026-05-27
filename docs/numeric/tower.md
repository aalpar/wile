# Numeric Tower Architecture

This document describes the numeric tower implementation in `values/numeric_tower.go`.

**Status:** Stable (2026-02-05)

---

## Design Philosophy: Algebraic Bias vs. Machine Arithmetic

Scheme's numeric tower is biased toward **algebra** — symbolic manipulation and exact representation. The tower is designed so that exact operations on exact inputs produce exact results (R7RS §6.2.2), and types preserve mathematical identity where possible. In an ideal algebraic system, `(* pi 1)` yields `pi` (not a float64 approximation), and `(log (exp 1))` yields exactly `1` (not `0.9999999999999998`).

Wile does not implement symbolic evaluation. It maps R7RS numerics onto Go's concrete types, which creates a pragmatic boundary: operations are only as precise as their runtime representation allows.

### Machine-Type Optimization

Wile's arithmetic is optimized for the common case where values stay within machine-type domains:

| Domain | Condition | Guarantee |
|--------|-----------|-----------|
| `int64` | Integer operands with results fitting int64 | Exact, hardware-speed arithmetic |
| `float64` | Float operands, or Integer operands < 2^53 promoted to Float | IEEE 754 precision, hardware speed |
| `complex128` | Complex operands, or Float/Integer promoted to Complex | Two float64 components, hardware speed |

When operations remain within these domains, results are both fast and as precise as the representation permits.

### IEEE 754 Semantic Uniformity

All inexact floating-point types in Wile — both machine-width (`Float`, `Complex`) and arbitrary-precision (`BigFloat`, `BigComplex`) — follow IEEE 754 semantics for special values (Inf, NaN). This is a deliberate design decision:

- **`values.BigFloat` and `values.BigComplex` MUST represent both Inf and NaN**, regardless of what Go's `math/big.Float` supports natively. Go's `big.Float` supports Inf (`big.Float.SetInf`) but has no NaN representation; Wile extends beyond this with internal state to track NaN.
- **IEEE 754 is the single reference spec** for all Inf/NaN behavior. When someone asks "what does `(+ +inf.0 x)` do?", the answer is "IEEE 754" regardless of whether `x` is `Float` or `BigFloat`.
- **Code paths stay uniform**: `BigFloat.Add` handles Inf/NaN the same way `float64` addition does. No branching on "which numeric type am I?" to get special-value semantics right.
- **Operations stay in their domain**: `float64 × float64 → float64`. `BigFloat × BigFloat → BigFloat`. Inf/NaN is never a reason to switch domains or demote types.

This eliminates the need for Inf/NaN guard paths in the dispatch table — the promotion lattice works correctly for all values, including special values.

### Promotion Beyond Machine Types

Certain operations promote values to arbitrary-precision types (`BigInteger`, `BigFloat`, `Rational`, `BigComplex`). Once a value enters the Big* domain, subsequent operations produce Big* results — there is no automatic demotion back to machine types during computation (only `Simplify` demotes after the fact).

This promotion is correct per R7RS but has practical consequences:
- Big* arithmetic is significantly slower (heap-allocated, no hardware acceleration)
- Mixed operations (e.g., `Float × BigComplex`) promote to the lattice LUB as usual

### Hot-Loop Allocation Reduction (`BigInteger` only)

For Go-side callers operating on `*BigInteger` in tight loops — e.g., counting-semiring path queries on DAGs — `values/numeric_scratch.go` provides unexported in-place arithmetic helpers (`addBigIntInPlace`, `subBigIntInPlace`, `mulBigIntInPlace`, `negateBigIntInPlace`). These reuse the destination's existing `[]Word` backing rather than allocating a fresh `*BigInteger` + `*big.Int` + `[]Word` per op (the path through `(*BigInteger).Add`).

The public `(*BigInteger).Add` etc. remain immutable per R7RS Number semantics; the in-place API is for library-internal Go callers only. The motivating consumer is `algebra/graph.CountPathsInDAG`, which the `(wile algebra graph)` library dispatches to when a semiring declares `(carrier . big-int)`. See `values/CLAUDE.md` §"In-Place Arithmetic on BigInteger" for the helpers' contract (aliasing, storage reuse) and microbench numbers.

The fast path applies only to `*BigInteger`. Other carriers — `*BigFloat`, `*Rational`, `*BigComplex` — have similar shapes but separate plans (see `plans/2026-05-24-bignum-allocation-reduction.md` §"Out of scope" for the scoping rationale).

### Out of Scope

The following algebraic optimizations are acknowledged but not planned for the current version:

- **Symbolic identity preservation**: `(* pi 1) → pi`, `(+ x 0) → x` (requires symbolic evaluation engine)
- **Algebraic simplification**: `(log (exp x)) → x`, `(sqrt (* x x)) → (abs x)` (requires term rewriting)
- **Singleton transcendental constants**: Representing `e`, `π`, etc. as singleton values so that operations like `(log e) → 1` produce exact results instead of floating-point approximations. This is philosophically aligned with Scheme's algebraic bias but crosses into Computer Algebra System (CAS) territory. No mainstream Scheme implementation (Guile, Racket, Chez, Chibi, MIT Scheme) does this — all represent `e` and `π` as inexact flonums. R7RS §6.2.6 explicitly excludes `log`, `sin`, `cos`, `exp` from the list of exactness-preserving operations. Scope creep is the main risk: each recognized identity (`(log e) → 1`, `(sin π) → 0`, `(log (* e e)) → 2`) is a special case, and a complete solution requires term rewriting. If pursued, this would be an extension (e.g., `(scheme symbolic)` library), not a change to the core numeric tower.
- **Machine-type constraint flags**: Engine-level flags to keep ALL computations within `int64`, `float64`, or `complex128` (desired feature, not yet a priority). This would prevent unexpected promotion to Big* types at the cost of raising implementation-restriction errors when results exceed machine-type range.

These are valid future directions but require significant design work beyond the current numeric tower architecture.

---

## Overview

The numeric tower uses **pre-built dispatch tables** indexed by `NumericKind`. Each numeric type's `Add`, `Subtract`, `Multiply`, `Divide`, and `Compare` methods look up the incoming operand's `Kind()` in a per-op table and invoke the matching closure. See the next subsection for the table inventory and call path.

### Dispatch Table Architecture

Tables are populated at `init()` time by generators in `values/promotion.go` — `makeArithmeticDispatch`, `makeLessThanDispatch`, `makeCompareDispatch`. Six of the seven numeric types carry six tables each (`Add`, `Subtract`, `Multiply`, `Divide`, `LessThan`, `Compare`), all pre-indexed by `NumericKind`. `BigComplex` carries five: it has no `LessThan` dispatch table because `LessThan` delegates to `Compare` (see `values/big_complex.go:148`). Total: **41 tables, 294 closures**. The fast-path call is `integerAdd[o.Kind()](p, o)` rather than a cascading type switch.

**Call path:** `Integer.Add(o)` → fast path for same type (`*Integer`), otherwise `integerAdd[o.Kind()](p, o)`.

**IEEE 754 guard:** When a `Float` holds Inf or NaN and the lattice LUB is `BigFloat`/`BigComplex`, the dispatch closures short-circuit to `float64`/`complex128` arithmetic. This logic is centralized in `makeArithmeticDispatch` (see `values/promotion.go` → `isSpecialFloat`), not replicated per type.

### Why This Instead of a Unified Tower

A unified tower dispatch (`TowerAdd`, etc.) was prototyped but **abandoned** because:

1. **Exact complex bug**: Linear promotion (Integer → BigInteger → Rational → Float → Complex) loses exactness when combining exact reals with complex numbers
2. **Battle-tested code**: The dispatch-table approach has been tested across all 49 type combinations
3. **Explicit cases**: Each promotion path is an explicit, generator-populated table entry — debuggable by reading `values/promotion.go`

---

## Current API

### Utility Functions

```go
// Simplify reduces a number to simpler type when possible
func Simplify(n Number) Number

// Exactness classification
func ExactnessOf(n Number) Exactness        // Returns Exact or Inexact
```

### Exactness Type

```go
type Exactness int

const (
    Exact Exactness = iota
    Inexact
)
```

**Deleted (2026-02-05):** `NumericRank`, `Rank`, `PromoteBoth`, `CommonRank`, `BinaryOp`, `TowerAdd`, `TowerSubtract`, `TowerMultiply`, `TowerDivide`, `TowerCompare`. `Promote` was retained (`values/promotion.go:303`) for use by the dispatch-table generators.

---

## Type Promotion (Lattice Model)

Direct dispatch implements a **lattice** with two dimensions:

```
                    BigComplex
                   ↗    ↑    ↖
            Complex   BigFloat   (exact BigComplex path)
               ↑    ↗    ↑         ↑
             Float    Rational ────┘
               ↑        ↑
            Integer → BigInteger
```

### Result Type Matrix

| A ↓ / B → | Integer | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
|-----------|---------|------------|----------|-------|----------|---------|------------|
| **Integer** | Integer¹ | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
| **BigInteger** | BigInteger | BigInteger | Rational | Float | BigFloat | Complex | BigComplex |
| **Rational** | Rational | Rational | Rational | Float | BigFloat | Complex | BigComplex |
| **Float** | Float | Float | Float | Float | BigFloat | Complex | BigComplex |
| **BigFloat** | BigFloat | BigFloat | BigFloat | BigFloat | BigFloat | BigComplex | BigComplex |
| **Complex** | Complex | Complex | Complex | Complex | BigComplex | Complex | BigComplex |
| **BigComplex** | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex | BigComplex |

¹ Integer + Integer may overflow to BigInteger

### Exactness Preservation

| A ↓ / B → | Exact | Inexact |
|-----------|-------|---------|
| **Exact** | Exact | Inexact |
| **Inexact** | Inexact | Inexact |

Where:
- **Exact**: Integer, BigInteger, Rational, BigComplex (with exact parts)
- **Inexact**: Float, BigFloat, Complex, BigComplex (with inexact parts)

---

## Simplification Rules

`Simplify` reduces numbers to simpler types when no information is lost:

| Input | Simplification |
|-------|---------------|
| BigComplex with zero imaginary | → real part (recursive) |
| Complex with zero imaginary | → Float → possibly Integer |
| BigFloat that is an integer | → BigInteger → possibly Integer |
| Float that is a whole number | → Integer |
| Rational with denominator 1 | → BigInteger → possibly Integer |
| BigInteger that fits int64 | → Integer |

---

## Error Handling

All types use consistent panic-based error handling for invalid inputs:

- Unknown types: `panic(werr.WrapForeignErrorf(werr.ErrNotANumber, "context: ..."))`
- Division by zero: `panic(werr.WrapForeignErrorf(werr.ErrDivisionByZero, "context: ..."))`

No bare sentinel panics remain. All panics wrap the sentinel with location context, enforced by the `noBareSentinelPanic` ruleguard rule.

All 49 type combinations (7×7) are handled without panics for valid operations.

---

## Conversion to Fixed-Precision Go Types

Conversion of a Scheme numeric value to a fixed-precision Go type
(`float64`, `complex128`) reports its accuracy via Go's `big.Accuracy`
enum (Below / Exact / Above). Wile surfaces this signal at three
layers:

| Layer | API | Returns on lossy conversion |
|-------|-----|-----------------------------|
| Go helper | `values.ToFloat64WithAccuracy(n)` | `(float64, big.Accuracy, isReal bool, error)` — accuracy field is the signal |
| Go helper (strict) | `values.ToFloat64Lossless(n)` | `werr.ErrLossyConversion` (wrapped, names direction) |
| Go helper | `values.ToComplex128WithAccuracy(n)` | `Complex128Result{Value, RealAcc, ImagAcc}` — per-component accuracy |
| Go helper (strict) | `values.ToComplex128Lossless(n)` | `werr.ErrLossyConversion` (wrapped) if either component non-Exact |
| FFI converter | `reflect.Float64` / `reflect.Complex128` param | strict (default): errors with `werr.ErrLossyConversion`; lossy: silently truncates if `WithLossyConversionsAllowed()` set on the engine |
| Scheme primitive | `(inexact-accuracy n)`, `(inexact-lossless? n)`, `(inexact-with-accuracy n)`, `(complex-inexact-with-accuracy n)` | `'below` / `'exact` / `'above` symbols (Wile-specific extensions in the math extension) |

### Strict-by-default discipline

The default ("strict") path **errors loudly** at the float64 /
complex128 boundary on any precision loss. Embedders that need the
legacy silent-truncation behavior opt in via
`wile.WithLossyConversionsAllowed()` on the engine.

This contrasts with R7RS-mandated `(exact->inexact)`, which is itself
unchanged — `(exact->inexact (expt 10 500))` continues to saturate to
`+inf.0` per R7RS §6.2.6. The new `inexact-*` primitives **expose the
saturation direction** rather than gate it.

### Accuracy symbol vocabulary

The Scheme symbols paraphrase Go's `big.Accuracy` directly:

- `'below` — the float64 representation is **less than** the true value
  (true value rounded down).
- `'exact` — float64 represents the value with no information loss.
- `'above` — the float64 representation is **greater than** the true
  value (true value rounded up).

For complex inputs, the accuracy is per-component: `(values real-acc
imag-acc)` and the symbols apply to each component independently.

### See also

- `werr.ErrLossyConversion` — sentinel for strict-path FFI / helper
  rejection.
- `values.BigAccuracyToSymbol(acc) *Symbol` — Go-side projection
  used by every primitive.
- `extensions/math/CLAUDE.local.md` — primitive inventory and design
  notes.
- `plans/2026-05-14-numeric-loss-signals-design.md` — design rationale,
  Q-1 through Q-6 resolutions.

---

## Testing

Coverage tests are in:

- `numeric_tower_coverage_test.go` — 245-case coverage matrix (7×7×5 operations)
- `numeric_lattice_test.go` — Lattice-based promotion model validation

Run tests:
```bash
go test -v ./values/ -run "TestNumericTower|TestLattice"
```

---

## References

- R7RS §6.2.1 — Numerical types (tower definition)
- R7RS §6.2.2 — Exactness (contagion rules)
- R7RS §6.2.3 — Implementation restrictions
- `values/CLAUDE.md` — Package documentation
- Design rationale: Unified tower dispatch was prototyped and abandoned (see "Why Direct Dispatch" above)
