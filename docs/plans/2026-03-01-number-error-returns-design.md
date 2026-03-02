# Number Interface Error Returns

**Goal:** Change `Number.Divide` and `Number.ToExact` from panicking to
returning `(Number, error)` so `applyForeign` can drop its `defer/recover`.
This unblocks direct-call optimizations for primitive functions.

## Background

`applyForeign` wraps every foreign function call in `defer/recover` to catch
panics from `values.Number` arithmetic methods. Division by exact zero and
non-finite→exact conversion are the two user-triggerable panic sites. The
recovery costs ~2ns per call on M4 Max, but more importantly it blocks a
class of compiler optimizations (direct primitive calls without
`SaveContinuation`) that would eliminate ~40ns per primitive call in hot loops.

## Interface Change

```go
type Number interface {
    // Changed — return error instead of panicking:
    Divide(Number) (Number, error)
    ToExact() (Number, error)

    // Unchanged:
    Add(Number) Number
    Subtract(Number) Number
    Multiply(Number) Number
    Negate() Number
    Abs() Number
    ToInexact() Number
    // ... predicates, comparisons unchanged ...
}
```

Only `Divide` and `ToExact` change. All other methods stay as `→ Number`.

**Rationale:** `ErrDivisionByZero` and `ErrExactnessConversion` are
user-triggerable. Promotion panics (`ErrNotANumber`) are assertion guards
for impossible type combinations — those should crash on bugs, not be
handled as expected errors.

## Implementation Changes

### 1. Type Implementations (7 types × Divide + relevant ToExact)

Each `Divide` changes `panic(werr.ErrDivisionByZero)` →
`return nil, werr.ErrDivisionByZero`. Each internal dispatch path returns
`(result, nil)`.

Types: Integer, Float, Rational, Complex, BigInteger, BigFloat, BigComplex.

`ToExact` changes in Float (non-finite guard) and BigFloat (non-finite guard),
plus `numeric_tower.go` which dispatches ToExact for cross-type promotion.

### 2. Promotion Dispatch (values/promotion.go)

The divide-specific dispatch entries (`makeDivideDispatch` or inline dispatch
within each type's `Divide` method) change return type. Add/subtract/multiply
dispatch stays unchanged.

### 3. Fold Helpers (registry/helpers/numeric.go)

```go
// NumericFoldVariadic and NumericFoldWithFirst:
// binOp changes:
//   Before: func(acc, val Number) Number
//   After:  func(acc, val Number) (Number, error)
//
// unaryOp in NumericFoldWithFirst changes:
//   Before: func(val Number) Number
//   After:  func(val Number) (Number, error)
```

Callers:
- `PrimDiv`: `func(a, b Number) (Number, error) { return a.Divide(b) }`
- `PrimSub`: `func(a, b Number) (Number, error) { return a.Subtract(b), nil }`
- `PrimAdd`: `func(a, b Number) (Number, error) { return a.Add(b), nil }`
- `PrimMul`: `func(a, b Number) (Number, error) { return a.Multiply(b), nil }`

### 4. Remove defer/recover from applyForeign (machine/machine_context.go)

Delete the `defer func() { r := recover(); ... }()` block. Named return
values become regular returns. `goErrorToSchemeException` conversion still
applies for non-abort/non-escape errors from the normal error return path.

### 5. Safety net in Run() (machine/machine_context.go)

Add a single `defer/recover` at `Run()` scope as a top-level safety net for
assertion-violation panics (promotion `ErrNotANumber`, etc.). This is cold —
entered once per top-level evaluation, not per function call.

```go
func (p *MachineContext) Run() (runErr error) {
    defer func() {
        r := recover()
        if r == nil { return }
        runErr = convertPanicToError(p, r)
    }()
    // ... dispatch loop ...
}
```

### 6. numericEquals (registry/core/prim_arithmetic.go)

Line `return a.Subtract(b).IsZero()` stays — Subtract doesn't change
signature. But the general path for `=` uses Subtract which can't fail.

### 7. Extension math primitives (extensions/math/)

Any call to `.Divide()` or `.ToExact()` gains error handling.

## What Doesn't Change

- `Add`, `Subtract`, `Multiply`, `Negate`, `Abs`, `ToInexact` signatures
- BigComplex internal arithmetic (uses Add/Sub/Mul on parts)
- `LessThan`, `Compare`, all predicates
- Promotion `Promote()` — keeps panic for impossible type combos (caught by
  Run safety net)

## Blast Radius

| Package | Files | Call Sites |
|---------|-------|------------|
| values/ | ~10 | ~15 |
| registry/helpers/ | 1 | ~4 |
| registry/core/ | 1-2 | ~4 |
| extensions/math/ | 1-2 | ~3 |
| machine/ | 1 | ~2 |
| **Total** | ~15-17 | ~28-30 |

## Expected Performance Impact

- Removes ~2ns defer/recover from every `applyForeign` call (441 calls in
  fib(10) = ~0.9µs saved directly)
- Unblocks future direct-call optimization: eliminating `SaveContinuation`
  for known-primitive calls saves ~40-60ns per call. For fib(10), 353
  eliminable primitive calls × ~50ns = ~17µs potential savings (35% of
  49µs baseline)
- Run() safety net: one `defer` amortized across entire evaluation — negligible

## Testing

- Existing tests for division-by-zero and exactness-conversion errors
  continue to pass (error paths unchanged, just mechanism changes from
  panic to return)
- Benchmark regression check: `BenchmarkRun/Fibonacci`,
  `BenchmarkApplyForeign`, `BenchmarkDeferRecoverFib`
