# Numeric Tower Architecture

This document describes the numeric tower implementation in `pkg/values/`: the promotion and dispatch machinery in `pkg/values/promotion.go`, the cross-kind utilities (`Simplify`, `ExactnessOf`, `NumericEquals`) in `pkg/values/numeric_tower.go`, and one file per numeric type.

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

Certain operations promote values to arbitrary-precision types (`BigInteger`, `BigFloat`, `Rational`, `BigComplex`). Once a value enters the Big* domain, subsequent operations produce Big* results — there is no automatic demotion back to machine types during computation. `Simplify` demotes only where a producer calls it: parse-time literal normalization, and the complex/rational reductions (`canonicalRational` in `pkg/values/rational.go`, `maybeSimplify` in `pkg/values/big_complex.go`). The `+`/`-`/`*` results of `pkg/values/integer.go`'s overflow helpers flow through the dispatch tables unsimplified, so a `BigInteger` result that fits in `int64` stays a `BigInteger`.

An **exact** operand meeting an **inexact** one does NOT enter the Big* domain. It is absorbed into the inexact operand's representation — R7RS §6.2.2 exactness contagion:

| Operation | Result | Note |
|-----------|--------|------|
| `(+ 1.5 2)` | `Float` 3.5 | the exact `2` is rounded into float64 |
| `(+ 1.5 (expt 2 2000))` | `Float` `+inf.0` | the exact operand overflows; Chez agrees |
| `(+ 1.5 #m2)` | `BigFloat` 3.5 | precision ASKED for is preserved |
| `(exact->inexact 1/2)` | `Float` 0.5 | |

This is deliberately **lossy**, and it is what "inexact" means. It used to promote exact × `Float` to `BigFloat` "to preserve precision", on the theory that `Simplify` would demote afterwards. Per-op demotion was never wired, so ordinary float arithmetic minted 256-bit bignums that never came back down; `(+ 1.5 2)` was a `*BigFloat`. Removing the promotion made mixed float/integer arithmetic ~40% faster.

**Complex is not exempt.** Exact × `Complex` → `Complex`, exactly as exact × `Float` → `Float`. It has to be: the promotion table is a join-semilattice, and given exact ⊔ `Float` = `Float` and `Float` ⊔ `Complex` = `Complex`, associativity *forces* exact ⊔ `Complex` = `Complex`. Any two of those three entries determine the third.

Exact × `Complex` used to escalate to `BigComplex`, and the reason was real: an exact operand rounded into `complex128` acquires a manufactured `+0.0` imaginary part, which is not an *exact* `0`, so the exact-zero rules that give `(/ 10 2.0+0.0i)` its `-0.0i` sign stop applying. But escalating bought that correctness with a **broken lattice**. With exact ⊔ `Float` = `Float` on one side and exact ⊔ `Complex` = `BigComplex` on the other, the join stopped being associative on 12 of its 343 triples, and the result *kind* is observable through `eqv?`/`equal?` (R7RS §6.1 makes representation observable for inexacts). So:

```scheme
(+ 1 1.5 2.0+0.0i)                                ; => 4.5+0.0i   (a Complex)
(+ 2.0+0.0i 1 1.5)                                ; => 4.5+0.0i   (a BigComplex)
(=      (+ 1 1.5 2.0+0.0i) (+ 2.0+0.0i 1 1.5))    ; => #t
(eqv?   (+ 1 1.5 2.0+0.0i) (+ 2.0+0.0i 1 1.5))    ; => #f   ← same value, different fold order
(equal? (* 1.0+2.0i 1) 1.0+2.0i)                  ; => #f   ← multiply by exact 1
```

**The exact zero is protected at the operation, not in the table.** A real operand has *no* imaginary component, so `real ⊕ complex` is computed part-wise and the component is never manufactured in the first place — `(/ 10 2.0+0.0i)` is still `5.0-0.0i`. `Float` has always worked this way (`Float` ⊔ `Complex` has always been `Complex`); the exact kinds now share that path. See the `real ⊕ complex` helpers in `pkg/values/complex.go` and `initPromotionTable`'s Zone 3 in `pkg/values/promotion.go`.

The rule, stated once: **contagion is a promotion question and the table owns it; the exact zero is an operation question and `complex.go` owns it.** They were tangled together, and the tangle cost the semilattice.

Big* arithmetic remains significantly slower (heap-allocated, no hardware acceleration), so keeping ordinary float arithmetic out of it matters.

### Arithmetic Promotes; Comparison Does Not

Contagion is a property of ARITHMETIC, not of comparison. Arithmetic brings two operands into a common kind via `values.promotionTable`. **Comparison does not promote at all.**

Rounding an operand is free when the result is already inexact, and fatal when the result is a boolean — the rounding is what *decides* the boolean:

```scheme
(= (- (expt 2 100) 1) (exact->inexact (expt 2 100)))   ; => #f
(< (- (expt 2 100) 1) (exact->inexact (expt 2 100)))   ; => #t
```

Both operands round to the same `float64`. A lossy comparison would call them equal; they are not, and Chez says so too.

`values.CompareNumbers` (`pkg/values/compare.go`) is the single authority behind `=`, `<`, `>`, `<=` and `>=`. It returns one of four verdicts — `OrderLess`, `OrderEqual`, `OrderGreater`, `OrderUnordered` — and its contract is that it **never rounds an operand**:

| Operands | How a common domain is reached |
|----------|--------------------------------|
| both exact | compared exactly |
| mixed exact/inexact, both finite | the **inexact** operand is lifted to its exact rational |
| either infinite | compared by sign class; equal infinities of the same sign are `OrderEqual` |
| either NaN | `OrderUnordered`; all five predicates are `#f` |
| complex | equality is component-wise; the ordering predicates raise on a non-real operand |

The lifting direction is the whole point. Every finite `float64` and every finite `big.Float` **is** a rational, so lifting the inexact operand always exists; lifting the exact operand into a float never does. R7RS §6.2.6 names the consequence of getting this backwards: "The implementation approach of converting all arguments to inexact numbers if any argument is inexact is not transitive."

There used to be a second kind table, `comparisonTable`, sending exact × `Float` to `BigFloat` and exact × `Complex` to `BigComplex` and calling that the lossless lattice. It was not lossless: `DefaultBigFloatPrecision` is 256, so an exact operand needing 301 significant bits was rounded on the way in and trichotomy failed outright. The table is deleted.

`PromotionResultKind(a, b)` is a **test-only** accessor, declared in `pkg/values/export_test.go` and not part of the public API. It was exported from `promotion.go` once, alongside a `ComparisonResultKind` that read the deleted table; the doc comment there records why they were demoted (a raw index panic on an out-of-range `NumericKind`, and two same-typed functions whose confusion silently rounds an operand). Production code indexes the table directly.

### Hot-Loop Allocation Reduction (`BigInteger` only)

For Go-side callers operating on `*BigInteger` in tight loops — e.g., counting-semiring path queries on DAGs — `pkg/values/numeric_scratch.go` provides unexported in-place arithmetic helpers (`addBigIntInPlace`, `subBigIntInPlace`, `mulBigIntInPlace`, `negateBigIntInPlace`). These reuse the destination's existing `[]Word` backing rather than allocating a fresh `*BigInteger` + `*big.Int` + `[]Word` per op (the path through `(*BigInteger).Add`).

The public `(*BigInteger).Add` etc. remain immutable per R7RS Number semantics; the in-place API is for library-internal Go callers only. The motivating consumer is `extensions/algebra/graph.CountPathsInDAG`, which the `(wile algebra graph)` library dispatches to when a semiring declares `(carrier . big-int)`. The helpers' contract (aliasing rules, storage reuse) is documented on the declarations in `pkg/values/numeric_scratch.go`.

The fast path applies only to `*BigInteger`. The other carriers — `*BigFloat`, `*Rational`, `*BigComplex` — have similar shapes but no in-place helpers: they are out of scope for the counting-semiring workload that motivated these.

### Out of Scope

The following algebraic optimizations are acknowledged but not planned for the current version:

- **Symbolic identity preservation**: `(* pi 1) → pi`, `(+ x 0) → x` (requires symbolic evaluation engine)
- **Algebraic simplification**: `(log (exp x)) → x`, `(sqrt (* x x)) → (abs x)` (requires term rewriting)
- **Singleton transcendental constants**: Representing `e`, `π`, etc. as singleton values so that operations like `(log e) → 1` produce exact results instead of floating-point approximations. This is philosophically aligned with Scheme's algebraic bias but crosses into Computer Algebra System (CAS) territory. No mainstream Scheme implementation (Guile, Racket, Chez, Chibi, MIT Scheme) does this — all represent `e` and `π` as inexact flonums. R7RS §6.2.6 explicitly excludes `log`, `sin`, `cos`, `exp` from the list of exactness-preserving operations. Scope creep is the main risk: each recognized identity (`(log e) → 1`, `(sin π) → 0`, `(log (* e e)) → 2`) is a special case, and a complete solution requires term rewriting. If pursued, this would be an extension (e.g., `(scheme symbolic)` library), not a change to the core numeric tower.
- **Machine-type constraint flags**: Engine-level flags to keep ALL computations within `int64`, `float64`, or `complex128` (desired feature, not yet a priority). This would prevent unexpected promotion to Big* types at the cost of raising implementation-restriction errors when results exceed machine-type range.

These are valid future directions but require significant design work beyond the current numeric tower architecture.

---

## Overview

The numeric tower uses **pre-built dispatch tables** indexed by `NumericKind`. Each numeric type's `Add`, `Subtract`, `Multiply`, `Divide`, and `LessThan` methods look up the incoming operand's `Kind()` in a per-op table and invoke the matching closure. See the next subsection for the table inventory and call path.

### Dispatch Table Architecture

Tables are populated at `init()` time by generators in `pkg/values/promotion.go` — `makeArithmeticDispatch` and `makeLessThanDispatch`. All seven numeric types carry five tables each (`Add`, `Subtract`, `Multiply`, `Divide`, `LessThan`), pre-indexed by `NumericKind`. Total: **35 tables, 245 closures**.

`LessThan` is the tower's only ordering primitive. A `Compare(Number) int` sat alongside it until it was removed: it answered a four-state question (less, equal, greater, unordered) in a three-state return, so a NaN operand got `0` and read as "equal". Equality is `NumericEquals` (R7RS `=`) or `EqvNumber` (`eqv?`), never the absence of ordering. See the `Number` doc comment in `pkg/values/values.go`. The fast-path call is `integerAdd[o.Kind()](p, o)` rather than a cascading type switch.

**Call path:** `Integer.Add(o)` → fast path for same type (`*Integer`), otherwise `integerAdd[o.Kind()](p, o)`.

**IEEE 754 guard:** When a `Float` holds Inf or NaN and the lattice LUB is `BigFloat`/`BigComplex`, the dispatch closures short-circuit to `float64`/`complex128` arithmetic. This logic is centralized in `makeArithmeticDispatch` (see `pkg/values/promotion.go` → `isSpecialFloat`), not replicated per type. The guard changes the result *kind*, not only the arithmetic: the `BigFloat` LUB returns a `*Float`, while the `BigComplex` LUB returns a `*BigComplex` so the operand's imaginary part survives. The `LessThan` dispatch deliberately carries no such guard.

### Why This Instead of a Unified Tower

A unified tower dispatch (`TowerAdd`, etc.) was prototyped but **abandoned** because:

1. **Exact complex bug**: Linear promotion (Integer → BigInteger → Rational → Float → Complex) loses exactness when combining exact reals with complex numbers
2. **Battle-tested code**: The dispatch-table approach has been tested across all 49 type combinations
3. **Explicit cases**: Each promotion path is an explicit, generator-populated table entry — debuggable by reading `pkg/values/promotion.go`

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

**Deleted (2026-02-05):** `NumericRank`, `Rank`, `PromoteBoth`, `CommonRank`, `BinaryOp`, `TowerAdd`, `TowerSubtract`, `TowerMultiply`, `TowerDivide`, `TowerCompare`. `Promote` was retained (`pkg/values/promotion.go` → `Promote`) for use by the dispatch-table generators.

---

## Which Type a Literal Makes

The reader picks a type from the literal's notation, before any promotion
happens. Two independent axes decide it.

**Prefixes.** `#b`/`#o`/`#d`/`#x` (radix) and `#e`/`#i` (exactness) form a
two-slot product — either order, at most one from each slot. `#z` and `#m` are
not a third slot; they read one complete datum and widen it, so they inherit
whatever the inner datum's prefixes decided.

| Notation | Type | Note |
|----------|------|------|
| `123` | `Integer` | promotes to `BigInteger` on int64 overflow |
| `1/2` | `Rational` | |
| `1.5`, `1e3` | `Float` | promotes to `BigFloat` past float64 range |
| `#x1.8` | `Float` | radix-prefixed fraction: 1.5. Extension, see `docs/reference/r7rs-differences.md` |
| `#z…` | `BigInteger` | datum must denote an exact integer |
| `#m…` | `BigFloat` | datum must be real; precision is the value's |
| `1+2i` | `BigComplex` if both parts exact, else `Complex` | |

**Exponent markers.** R7RS §6.2.5 makes `s`/`f`/`d`/`l` an optional precision
request, and lets an implementation with fewer than four inexact representations
map the four size specifications onto what it has. Wile has two:

| Marker | Requests | Type |
|--------|----------|------|
| `e` | default precision | `Float` |
| `s` | short | `Float` |
| `f` | single | `Float` |
| `d` | double | `Float` |
| `l` | long | `BigFloat` |

`l` is the only marker that selects a representation distinct from the default,
so it is the only one that changes the type. It is symmetric on output: a
`BigFloat` writes with `l` (`1e1000` renders as `1l+1000`), because writing `e`
would claim default precision and the value would read back as a `Float`.

Two consequences worth stating, since they look like inconsistencies and are not:

- **Precision is the value's, not the literal's, once an introducer is involved.**
  `1.2345678901234567890123456789l0` is one token and keeps every digit;
  `#m#d1.2345678901234567890123456789` widens an already-rounded float64.
- **Markers are decimal-only.** `#x1e2` is 482, because `e` is a hex digit.

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

`Simplify` reduces numbers to simpler types when no information is lost. It descends **within** an exactness class, never across one: exactness, not magnitude, licenses a descent.

| Input | Simplification |
|-------|---------------|
| BigComplex with an **exact** zero imaginary | → real part (recursive; the one cross-kind case, handled in `Simplify` itself) |
| Rational with denominator 1 | → BigInteger → possibly Integer |
| BigInteger that fits int64 | → Integer |
| Integer, Float, BigFloat, Complex | identity |

The inexact tier does not descend. `Float` is the bottom of it, and demoting a whole-valued `Float` or an integral `BigFloat` to an exact `Integer` (which earlier versions did) crosses the exactness class and would make `(exact? 2.0)` answer `#t`. A `*Complex` never descends either: its parts are `float64`, so a `0.0` imaginary part is always an *inexact* zero, and R7RS §6.2.6 says the component is still there (`(real? -2.5+0.0i)` is `#f`). See the `SimplifyDown` functions registered per kind in `pkg/values/numeric_registry.go`.

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
- `extensions/math/` — the primitives that raise these signals.

---

## Testing

Coverage tests are in:

- `pkg/values/numeric_tower_coverage_test.go` — 245-case coverage matrix (7×7×5 operations)
- `pkg/values/numeric_lattice_test.go` — Lattice-based promotion model validation
- `pkg/values/promotion_test.go` — the two tables' semilattice laws, and the exact set of pairs on which they are permitted to diverge
- `pkg/values/numeric_dispatch_test.go` — `TestAllDispatchEntriesPopulated`: no nil entry in any of the 35 tables

Run tests:
```bash
go test -v ./pkg/values/ -run "TestNumericTower|TestLattice"
```

---

## References

- R7RS §6.2.1 — Numerical types (tower definition)
- R7RS §6.2.2 — Exactness (contagion rules)
- R7RS §6.2.3 — Implementation restrictions
- `pkg/values/` — package sources: `numeric_kind.go` (the `NumericKind` enum and the "adding a new numeric type" checklist), `promotion.go` (tables and dispatch generators), `numeric_registry.go` (per-kind `NumericTypeSpec`), `numeric_scratch.go` (in-place helpers)
- Design rationale: Unified tower dispatch was prototyped and abandoned (see "Why This Instead of a Unified Tower" above)
