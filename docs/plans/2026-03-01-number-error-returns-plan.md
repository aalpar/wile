# Number Interface Error Returns — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Change `Number.Divide` and `Number.ToExact` from panicking to returning `(Number, error)` so `applyForeign` can drop its `defer/recover`. Zero panics downstream of `Run()`.

**Architecture:** `Divide` and `ToExact` return `(Number, error)`. Divide dispatch tables return `(Number, error)` via a new `makeDivideDispatch` (separate from `makeArithmeticDispatch`). Add/Sub/Mul dispatch tables stay `→ Number`. Promotion table completeness validated at init time so nil-promoter panics are impossible at runtime. No `defer/recover` anywhere downstream of `Run()`.

**Tech Stack:** Go, no new dependencies.

**Design doc:** `docs/plans/2026-03-01-number-error-returns-design.md`

---

## Constraint: Zero Panics Downstream of Run()

No `panic()` call may be reachable from `MachineContext.Run()`, any opcode, any `ForeignFunction`, or any `Number` method called from those paths. This means:

1. Divide dispatch tables must return `(Number, error)` — no `panic(err)` in closures
2. No `defer/recover` safety net in `Run()` — it's not needed if nothing panics
3. Promotion table validated complete at `init()` — nil promoter calls are impossible
4. BigComplex/BigFloat internal `.Divide()` calls propagate errors through dispatch tables

---

## Task Ordering

All changes in `values/` must compile atomically (interface + all implementations + dispatch).
Tasks 3+4 (fold helpers + primitives) also compile atomically.

```
Task 1: Divide interface + dispatch tables + all 7 implementations (atomic)
Task 2: ToExact interface + all 7 implementations + helpers (atomic)
Task 3: Init-time promotion table validation
Task 4: Fold helpers + primitive callers (atomic)
Task 5: Remove defer/recover from applyForeign
Task 6: Full test suite + benchmark regression
```

---

### Task 1: Divide — Interface, Dispatch Tables, All Implementations

This task changes everything needed for `Divide(Number) (Number, error)` to compile. It must be done atomically because the interface, dispatch tables, and implementations are mutually dependent.

**Files:**
- Modify: `values/values.go:315` — interface Divide signature
- Modify: `values/promotion.go:459-472` — replace `makeDivideDispatch` with error-returning version
- Modify: `values/integer.go` — Integer.Divide + dispatch table variable type
- Modify: `values/float.go` — Float.Divide + dispatch table variable type
- Modify: `values/rational.go` — Rational.Divide + dispatch table variable type
- Modify: `values/complex.go` — Complex.Divide + dispatch table variable type
- Modify: `values/big_integer.go` — BigInteger.Divide + dispatch table variable type
- Modify: `values/big_float.go` — BigFloat.Divide + dispatch table variable + init closure
- Modify: `values/big_complex.go` — BigComplex.Divide + dispatch table variable + init closure (4 internal .Divide calls)

**Step 1: Change interface**

`values/values.go:315` — change and update doc comment (remove the sentence about recovering panics at the boundary, lines 305-308):
```go
Divide(Number) (Number, error)
```

**Step 2: Write `makeDivideDispatch` as error-returning dispatch generator**

Replace the current `makeDivideDispatch` in `values/promotion.go:459-472`. The current version delegates to `makeArithmeticDispatch` which returns `func(T, Number) Number`. The new version is a standalone function that returns `func(T, Number) (Number, error)`.

The structure mirrors `makeArithmeticDispatch` exactly — same promotion table lookup, same IEEE 754 special-value guards — but all closures return `(Number, error)` and the `applyOp` callback returns `(Number, error)`.

```go
// makeDivideDispatch generates a dispatch table for the Divide operation.
// Unlike makeArithmeticDispatch, dispatch entries return (Number, error) so
// division-by-zero errors propagate without panicking.
func makeDivideDispatch[T Number](
	srcKind NumericKind,
	sameTypeOp func(T, Number) (Number, error),
) [numKinds]func(T, Number) (Number, error) {
	ensurePromotionInit()
	var table [numKinds]func(T, Number) (Number, error)
	table[srcKind] = sameTypeOp
	for dstKind := range numKinds {
		if dstKind == srcKind {
			continue
		}
		lubKind := promotionTable[srcKind][dstKind]
		promSrc := promoter[srcKind][lubKind]
		promDst := promoter[dstKind][lubKind]

		lubNeedsGuard := lubKind != KindFloat && lubKind != KindComplex

		switch {
		case srcKind == KindFloat && lubNeedsGuard:
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) (Number, error) {
				if isSpecialFloat(any(p).(*Float)) {
					if lubIsComplex {
						z := NumberToComplex128(p) / NumberToComplex128(o)
						return NewBigComplexFromBigFloats(
							NewBigFloatFromFloat64(real(z)),
							NewBigFloatFromFloat64(imag(z)),
						), nil
					}
					return NewFloat(NumberToFloat64(p) / NumberToFloat64(o)), nil
				}
				return promSrc(p).Divide(promDst(o))
			}
		case dstKind == KindFloat && lubNeedsGuard:
			lubIsComplex := lubKind == KindBigComplex
			table[dstKind] = func(p T, o Number) (Number, error) {
				if isSpecialFloat(o.(*Float)) {
					if lubIsComplex {
						z := NumberToComplex128(p) / NumberToComplex128(o)
						return NewBigComplexFromBigFloats(
							NewBigFloatFromFloat64(real(z)),
							NewBigFloatFromFloat64(imag(z)),
						), nil
					}
					return NewFloat(NumberToFloat64(p) / NumberToFloat64(o)), nil
				}
				return promSrc(p).Divide(promDst(o))
			}
		default:
			table[dstKind] = func(p T, o Number) (Number, error) {
				return promSrc(p).Divide(promDst(o))
			}
		}
	}
	return table
}
```

Key difference from `makeArithmeticDispatch`: the recursive `applyOp` callback is replaced by direct `promSrc(p).Divide(promDst(o))` calls which now naturally return `(Number, error)`. No `panic()` anywhere.

**Step 3: Change all 7 dispatch table variable types**

Each type has a dispatch table variable. All change type:

```go
// Before (in each type file):
var integerDivide [numKinds]func(*Integer, Number) Number

// After:
var integerDivide [numKinds]func(*Integer, Number) (Number, error)
```

Apply to: `integerDivide`, `floatDivide`, `rationalDivide`, `complexDivide`, `bigIntegerDivide`, `bigFloatDivide`, `bigComplexDivide`.

**Step 4: Change all 7 Divide receiver methods**

Pattern for 5 simple types (Integer, Float, Rational, BigInteger, Complex):
```go
func (p *T) Divide(o Number) (Number, error) {
    if o.IsZero() && o.IsExact() {
        return nil, werr.ErrDivisionByZero  // was: panic(werr.ErrDivisionByZero)
    }
    // Same-type fast path (if present):
    v, ok := o.(*T)
    if ok {
        // ... same-type logic ...
        return result, nil
    }
    return tDivide[o.Kind()](p, o)  // dispatch now returns (Number, error) directly
}
```

Every `return result` → `return result, nil`. The dispatch call returns `(Number, error)` directly — no wrapping needed.

**Step 5: Change all 7 dispatch init closures**

The `sameTypeOp` closures passed to `makeDivideDispatch` change signature. For most types, the init closure is a same-type handler:

```go
// Before:
integerDivide = makeDivideDispatch(KindInteger, func(p *Integer, o Number) Number {
    // ... same-type division ...
    return result
})

// After:
integerDivide = makeDivideDispatch(KindInteger, func(p *Integer, o Number) (Number, error) {
    // ... same-type division ...
    return result, nil
})
```

**BigFloat** — its init closure calls `p.Divide(o)` recursively:
```go
// Before:
bigFloatDivide = makeDivideDispatch(KindBigFloat, func(p *BigFloat, o Number) Number {
    return p.Divide(o)
})

// After:
bigFloatDivide = makeDivideDispatch(KindBigFloat, func(p *BigFloat, o Number) (Number, error) {
    return p.Divide(o)
})
```
Natural — `p.Divide(o)` already returns `(Number, error)`.

**BigComplex** — its init closure has 4 internal `.Divide()` calls. These now propagate errors:
```go
bigComplexDivide = makeDivideDispatch(KindBigComplex, func(p *BigComplex, o Number) (Number, error) {
    v := o.(*BigComplex)
    if v.imag.IsZero() {
        newReal, err := p.real.Divide(v.real)
        if err != nil {
            return nil, err
        }
        newImag, err := p.imag.Divide(v.real)
        if err != nil {
            return nil, err
        }
        return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag)), nil
    }
    // General case: arithmetic on parts uses Add/Sub/Mul (no error)
    ac := p.real.Multiply(v.real)
    bd := p.imag.Multiply(v.imag)
    bc := p.imag.Multiply(v.real)
    ad := p.real.Multiply(v.imag)
    cc := v.real.Multiply(v.real)
    dd := v.imag.Multiply(v.imag)

    numerReal := ac.Add(bd)
    numerImag := bc.Subtract(ad)
    denom := cc.Add(dd)

    newReal, err := toBigFloat(numerReal).Divide(toBigFloat(denom))
    if err != nil {
        return nil, err
    }
    newImag, err := toBigFloat(numerImag).Divide(toBigFloat(denom))
    if err != nil {
        return nil, err
    }
    return maybeSimplify(promoteToBigComplexPart(newReal), promoteToBigComplexPart(newImag)), nil
})
```

Zero `panic()` calls. Errors propagate naturally.

**Step 6: Verify compilation**

Run: `go build ./values/...`
Expected: PASS

**Step 7: Run values tests**

Run: `go test ./values/...`
Expected: PASS

**Step 8: Commit**

```
refactor(values): change Number.Divide to return (Number, error)

Replace panic(ErrDivisionByZero) with error return in all 7 numeric
types. Divide dispatch tables now return (Number, error) via a new
standalone makeDivideDispatch that propagates errors through promotion
without panicking. BigComplex internal .Divide() calls propagate
errors naturally.
```

---

### Task 2: ToExact — Interface, All Implementations, Helpers

**Files:**
- Modify: `values/values.go:318` — interface ToExact signature
- Modify: `values/numeric_tower.go:46-61` — `floatToExact`
- Modify: `values/float.go` — Float.ToExact
- Modify: `values/big_float.go` — BigFloat.ToExact
- Modify: `values/complex.go` — Complex.ToExact
- Modify: `values/big_complex.go` — BigComplex.ToExact + `toExactPart`
- Modify: `values/integer.go` — Integer.ToExact (trivial)
- Modify: `values/big_integer.go` — BigInteger.ToExact (trivial)
- Modify: `values/rational.go` — Rational.ToExact (trivial)

**Step 1: Change interface**

`values/values.go:318`:
```go
ToExact() (Number, error)
```

**Step 2: Change `floatToExact`**

`values/numeric_tower.go:46-61` — panic → error return:
```go
func floatToExact(f float64) (Number, error) {
    r := new(big.Rat).SetFloat64(f)
    if r == nil {
        return nil, werr.WrapForeignErrorf(werr.ErrExactnessConversion,
            "cannot convert non-finite float to exact")
    }
    if r.IsInt() {
        num := r.Num()
        if num.IsInt64() {
            return NewBigIntegerFromInt64(num.Int64()), nil
        }
        return NewBigInteger(new(big.Int).Set(num)), nil
    }
    return NewRationalFromRat(r), nil
}
```

**Step 3: Types that can error**

- **Float.ToExact**: `return floatToExact(p.Value)` — signature matches naturally
- **BigFloat.ToExact**: `panic(...)` → `return nil, werr.WrapForeignErrorf(...)`, all other returns add `, nil`
- **Complex.ToExact**: two `floatToExact` calls with error checks
- **BigComplex.ToExact**: calls `toExactPart` which returns error

**Step 4: Types that never error**

- **Integer.ToExact**: `return p` → `return p, nil`
- **BigInteger.ToExact**: `return p` → `return p, nil`
- **Rational.ToExact**: `return p` → `return p, nil`

**Step 5: Change `toExactPart`**

`values/big_complex.go` — change signature, propagate error from `v.ToExact()`:
```go
func toExactPart(n Number) (Number, error) {
    // ... cases return (result, nil) ...
    // BigFloat case:
    exact, err := v.ToExact()
    if err != nil {
        return nil, err
    }
    q := Simplify(exact)
    // ...
}
```

**Step 6: Verify and test**

Run: `go build ./values/... && go test ./values/...`
Expected: PASS

**Step 7: Commit**

```
refactor(values): change Number.ToExact to return (Number, error)

Replace panic(ErrExactnessConversion) with error return in Float,
BigFloat, Complex, BigComplex. Trivial types return (self, nil).
```

---

### Task 3: Init-Time Promotion Table Validation

This ensures no nil-promoter panic is reachable at runtime from Add/Sub/Mul dispatch tables. The validation runs at program startup — if a promoter is missing, the program crashes at init (development time), not at runtime (user time).

**Files:**
- Modify: `values/promotion.go` — add validation in `initPromoters()` or a new `init()` function

**Step 1: Add validation**

At the end of `initPromoters()` (or in a separate `init()` that runs after promotion initialization), validate that every promoter reachable from the promotion table is non-nil:

```go
// validatePromotionTable ensures every cross-type promotion path has a
// non-nil promoter function. This is a build-time assertion — if it fails,
// a type was added to the numeric tower without completing its promotion
// entries. Panics at program startup, never at runtime.
func validatePromotionTable() {
    for src := range numKinds {
        for dst := range numKinds {
            if src == dst {
                continue
            }
            lub := promotionTable[src][dst]
            if promoter[src][lub] == nil {
                panic(fmt.Sprintf(
                    "incomplete promotion table: promoter[%d][%d] is nil (src=%d, dst=%d, lub=%d)",
                    src, lub, src, dst, lub))
            }
            if promoter[dst][lub] == nil {
                panic(fmt.Sprintf(
                    "incomplete promotion table: promoter[%d][%d] is nil (src=%d, dst=%d, lub=%d)",
                    dst, lub, src, dst, lub))
            }
        }
    }
}
```

Call `validatePromotionTable()` at the end of `initPromoters()` (inside the `sync.Once`).

**Step 2: Verify tests still pass**

Run: `go test ./values/...`
Expected: PASS (validation succeeds — the table is complete)

**Step 3: Commit**

```
refactor(values): validate promotion table completeness at init time

Add init-time assertion that every cross-type promotion path has a
non-nil promoter function. This guarantees that Add/Sub/Mul dispatch
table closures can never hit a nil-function panic at runtime. Failures
crash at program startup with a descriptive message.
```

---

### Task 4: Fold Helpers + Primitive Callers

Must compile atomically — fold helpers change callback signatures, primitives provide the new-signature callbacks.

**Files:**
- Modify: `registry/helpers/numeric.go` — `NumericFoldVariadic`, `NumericFoldWithFirst`
- Modify: `registry/core/prim_arithmetic.go` — PrimAdd, PrimSub, PrimMul, PrimDiv, PrimExact

**Step 1: Change `NumericFoldVariadic` binOp signature**

```go
binOp func(acc, val values.Number) (values.Number, error)
```

Update all internal `binOp(acc, v)` call sites to handle the returned error.

**Step 2: Change `NumericFoldWithFirst` signatures**

```go
unaryOp func(val values.Number) (values.Number, error)
binOp   func(acc, val values.Number) (values.Number, error)
```

Update all internal call sites.

**Step 3: Update primitive callers**

- **PrimAdd**: `func(acc, val Number) (Number, error) { return acc.Add(val), nil }`
- **PrimSub**: unary wraps Subtract with nil error, binary wraps Subtract with nil error
- **PrimMul**: `func(acc, val Number) (Number, error) { return acc.Multiply(val), nil }`
- **PrimDiv**: `func(val Number) (Number, error) { return values.NewInteger(1).Divide(val) }` — natural
- **PrimExact**: `result, err := n.ToExact()` with error check

**Step 4: Verify and test**

Run: `go build ./... && go test ./values/... ./registry/...`
Expected: PASS

**Step 5: Commit**

```
refactor(registry): update fold helpers and primitives for error returns

NumericFoldVariadic and NumericFoldWithFirst callbacks now return
(Number, error). PrimDiv and PrimExact propagate errors naturally.
PrimAdd/Sub/Mul wrap non-erroring operations with nil error.
```

---

### Task 5: Remove defer/recover from applyForeign

**Files:**
- Modify: `machine/machine_context.go` — `applyForeign`

**Step 1: Remove defer/recover**

Change signature from named returns to regular returns:
```go
// Before:
func (p *MachineContext) applyForeign(fcls *ForeignClosure, vs ...values.Value) (rmc *MachineContext, rerr error) {

// After:
func (p *MachineContext) applyForeign(fcls *ForeignClosure, vs ...values.Value) (*MachineContext, error) {
```

Delete the entire `defer func() { r := recover(); ... }()` block.

No safety net in `Run()`. Nothing downstream of `Run()` panics.

**Step 2: Run full test suite**

Run: `make test`
Expected: PASS

**Step 3: Run lint**

Run: `make lint`
Expected: PASS

**Step 4: Commit**

```
perf(machine): remove defer/recover from applyForeign

Number.Divide and Number.ToExact now return errors through normal
paths. Promotion table completeness is validated at init time. No
panic is reachable downstream of Run(), so no recovery is needed.

This unblocks direct-call optimizations for known-primitive calls.
```

---

### Task 6: Benchmark Regression Check

**Step 1:** Run fib benchmark:
`go test -bench='BenchmarkRun/Fibonacci' -benchmem -count=5 .`
Expected: ~49µs/op or slightly better.

**Step 2:** Run applyForeign microbenchmark:
`go test -bench='BenchmarkApplyForeign$' -benchmem -count=5 ./machine/`
Expected: Improvement from ~8.5ns to ~6-7ns.

**Step 3:** Run DeferRecoverFib benchmark:
`go test -bench='BenchmarkDeferRecoverFib$' -benchmem -count=5 ./machine/`
Expected: Should improve (no defer/recover).

**Step 4:** Full suite:
`make lint && make test`
Expected: PASS

---

## Summary

| Task | What | Key Change |
|------|------|------------|
| 1 | Divide interface + dispatch + 7 types | Error-returning dispatch tables via new `makeDivideDispatch` |
| 2 | ToExact interface + 7 types + helpers | `floatToExact`/`toExactPart` return error |
| 3 | Promotion table validation | Init-time assertion: no nil promoters |
| 4 | Fold helpers + primitives | Callback signatures return `(Number, error)` |
| 5 | Remove defer/recover from applyForeign | No safety net needed — nothing panics |
| 6 | Benchmark regression | Verify improvement |
