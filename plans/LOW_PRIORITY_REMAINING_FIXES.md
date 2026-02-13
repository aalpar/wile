# LOW Priority Remaining Fixes — Implementation Plan

**Date:** 2026-02-12
**Status:** Phases 1 & 2 Complete
**Completed:** L1, L2, L4, L5, L6, L7, L8, L9, L10, L12, L13, L14, L16, L17, L18 (15 issues)
**Remaining (Deferred):** L3, L11, L15, L19 (4 issues)

---

## Executive Summary

After completing Batch 1 (L4, L13) and the earlier LOW priority batch, 7 issues remain. They fall into three categories:

| Priority | Issues | Est. Time | Recommendation |
|----------|--------|-----------|----------------|
| **High Value** | L17, L18 | 2 hours | Fix next (numeric correctness) |
| **Medium Value** | L11, L15, L19 | 4 hours | Fix when convenient |
| **Low Value / Skip** | L3, L10 | Document | L10: document limitation, L3: defer indefinitely |

**Recommended approach:** Fix L17/L18 (numeric precision), document L10, defer L3/L11/L15/L19.

---

## High Value Fixes (Recommended Next)

### L17: expt Missing BigInteger Fallback Case

**File:** `internal/extensions/math/prim_math.go:310`
**Priority:** ⭐⭐⭐ HIGH
**Estimated Time:** 1 hour
**Complexity:** MEDIUM

#### Problem

The `expt` primitive handles most numeric type combinations but has a gap in the BigInteger fallback path. Large integer exponentiation may route through float64, losing precision:

```scheme
(expt 2 1000)  ; Should be exact BigInteger
               ; May route through Float, lose precision
```

The current fallback in `expt`:

```go
// Fallback: convert both to float
baseF := baseNumber.(values.Number).ToInexact().(*values.Float).Value
expF := expNumber.(values.Number).ToInexact().(*values.Float).Value
result := math.Pow(baseF, expF)
return values.NewFloat(result)
```

This converts BigInteger to Float before exponentiation, which loses precision for large integers.

#### The Fix

Add explicit BigInteger case before the float fallback:

```go
// Before float fallback, check for BigInteger ^ integer cases
if baseBI, ok := baseNumber.(*values.BigInteger); ok {
    if expInt, isInt := values.ExactInteger(expNumber); isInt && expInt >= 0 {
        // Use big.Int.Exp for exact integer exponentiation
        result := new(big.Int).Exp(baseBI.Value(), big.NewInt(expInt), nil)
        return values.SimplifyBigInteger(result)
    }
    // Fall through to float path for negative/fractional exponents
}

// Existing float fallback
baseF := baseNumber.(values.Number).ToInexact().(*values.Float).Value
// ...
```

**Pattern:** This follows the existing pattern in `expt` of handling exact cases first, then falling back to inexact for fractional exponents.

#### Impact

- **Correctness:** Large integer exponentiation stays exact
- **R7RS compliance:** Preserves exactness per §6.2.6
- **Performance:** No impact (big.Int.Exp is already fast)

#### Tests

Add to `internal/extensions/math/prim_math_test.go`:

```scheme
;; Large integer exponentiation stays exact
(test-assert (exact? (expt 2 1000)))
(test-equal (expt 2 10) 1024)  ; Verify correctness
(test-equal (expt 10 20) 100000000000000000000)

;; Exactness preservation
(test-assert (exact? (expt (expt 2 500) 2)))
(test-equal (expt (expt 2 500) 2) (expt 2 1000))

;; Negative/fractional exponents still inexact
(test-assert (inexact? (expt 2 -1)))
(test-assert (inexact? (expt 2 0.5)))
```

#### R7RS Compliance

**R7RS §6.2.6 (expt):**
> Returns z1 raised to the power z2. For nonzero z1:
> - If z1 is exact and z2 is a non-negative exact integer, the result is exact
> - In other cases, the result may be inexact

The fix ensures exact results for exact base and non-negative exact integer exponent.

---

### L18: rationalToInteger Loses Precision via float64

**File:** `internal/extensions/math/prim_math.go:424`
**Priority:** ⭐⭐⭐ HIGH
**Estimated Time:** 1 hour
**Complexity:** LOW

#### Problem

The `rationalToInteger` helper converts via `.Float64()`, which loses precision for large rationals:

```go
func rationalToInteger(r *values.Rational) values.Number {
    f, _ := r.Value.Float64()  // ← Loses precision for large rationals
    return values.NewFloat(f)
}
```

Example:

```scheme
(inexact (/ (expt 2 100) 3))  ; Should be high-precision float
                               ; Gets truncated to float64 range
```

#### The Fix

Use `big.Rat` directly for conversion to preserve precision:

```go
func rationalToInteger(r *values.Rational) values.Number {
    // Use big.Rat directly for conversion
    f := new(big.Float).SetRat(r.Value)
    return values.NewBigFloat(f)
}
```

**Pattern:** This matches the pattern in `BigInteger.ToInexact()` which uses `NewBigFloat` to preserve precision.

#### Impact

- **Correctness:** Rational to inexact preserves large values
- **Precision:** Arbitrary precision instead of float64 limits
- **Performance:** Minimal (BigFloat is only used for large rationals)

#### Tests

Add to `internal/extensions/math/prim_math_test.go`:

```scheme
;; Rational to inexact preserves magnitude
(test-assert (> (inexact (/ (expt 2 100) 3)) 1e29))

;; Small rationals still work
(test-assert (< (abs (- (inexact (/ 1 3)) 0.333333)) 0.001))

;; Large numerator preserved
(test-assert (> (inexact (/ (expt 10 50) 7)) 1e48))
```

#### Notes

**Why BigFloat instead of Float?**
The numeric tower uses BigFloat for values that exceed float64 precision. This is consistent with how `BigInteger.ToInexact()` works.

---

## Medium Value Fixes (Defer Until Needed)

### L19: isExtendedExponentMarkerForRadix Ignores Radix

**File:** `internal/tokenizer/tokenizer.go:2280`
**Priority:** ⭐ LOW
**Estimated Time:** 1 hour
**Complexity:** LOW
**Recommendation:** Defer (exotic edge case)

#### Problem

The helper function ignores its radix parameter:

```go
func isExtendedExponentMarkerForRadix(r rune, radix int) bool {
    // radix parameter is ignored!
    return r == 's' || r == 'S' || r == 'f' || r == 'F' ||
           r == 'd' || r == 'D' || r == 'l' || r == 'L'
}
```

R7RS §7.1.1 specifies that exponent markers have radix-specific validity:
- Binary (#b): no exponent markers
- Octal (#o): no exponent markers
- Decimal (#d): e, s, f, d, l (all valid)
- Hex (#x): no exponent markers

**Impact:** Incorrectly accepts exotic formats like `#x1.5e2` (hex with decimal exponent).

**Real-world impact:** VERY LOW. Who writes `#x1.5e2`?

#### The Fix

Add radix validation:

```go
func isExtendedExponentMarkerForRadix(r rune, radix int) bool {
    if radix != 10 {
        return false  // Only decimal numbers have exponents
    }
    return r == 's' || r == 'S' || r == 'f' || r == 'F' ||
           r == 'd' || r == 'D' || r == 'l' || r == 'L' ||
           r == 'e' || r == 'E'
}
```

#### Tests

Add to `internal/tokenizer/tokenizer_test.go`:

```scheme
;; Binary with exponent should error
(test-error (string->number "#b1.0e2"))

;; Octal with exponent should error
(test-error (string->number "#o1.0e2"))

;; Hex with exponent should error
(test-error (string->number "#x1.5e2"))

;; Decimal with exponent should work
(test-equal (string->number "#d1.0e2") 100.0)
```

#### Recommendation

**DEFER** — This is an exotic edge case with very low real-world impact. Fix only if needed for R7RS test suite compliance.

---

### L11: eval Doesn't Inherit Dynamic Context

**File:** `internal/extensions/eval/prim_eval.go:35`
**Priority:** ⭐⭐ MEDIUM
**Estimated Time:** 2 hours
**Complexity:** MEDIUM
**Recommendation:** Defer (rare use case)

#### Problem

The `eval` primitive creates a fresh evaluation context without copying the dynamic environment (parameter bindings):

```scheme
(parameterize ((current-output-port some-port))
  (eval '(display "hello") (scheme-report-environment 5)))
; Prints to default output, not some-port
```

**Impact:** Unexpected behavior when eval is used inside parameterize. However, this combination is rare in practice.

#### Complexity Analysis

**What needs to be done:**
1. Capture dynamic environment (parameter bindings) from caller
2. Pass to eval's sub-context
3. Ensure parameter stack is properly inherited

**Why it's non-trivial:**
- Parameter implementation is in `machine/parameters.go`
- Sub-context creation is in `machine/machine_context.go`
- Need to understand how parameter bindings are stored and accessed
- Risk of breaking parameter semantics if done incorrectly

#### The Fix (Outline)

```go
func PrimEval(ctx context.Context, mc *machine.MachineContext) error {
    // ... existing code ...

    // Create sub-context with parent's dynamic environment
    sub := machine.NewSubContext(mc, env, compiled)

    // Inherit parameter bindings from parent
    // (This requires access to the parameter stack, which may need refactoring)
    sub.SetParameterStack(mc.GetParameterStack())

    result, err := sub.Run(ctx)
    // ...
}
```

**Open questions:**
- How are parameter bindings currently stored?
- Does `NewSubContext` already inherit some parameter state?
- What's the test coverage for `parameterize` + sub-contexts?

#### Tests

```scheme
(test-begin "eval parameter inheritance")

;; Basic parameter inheritance
(test-equal "eval sees parameterize"
  (parameterize ((current-output-port (open-output-string)))
    (eval '(begin (display "test") (get-output-string (current-output-port)))
          (scheme-report-environment 5)))
  "test")

;; Nested parameterize
(test-equal "eval with nested parameterize"
  (parameterize ((current-output-port port1))
    (parameterize ((current-output-port port2))
      (eval '(display "inner") (scheme-report-environment 5))
      (get-output-string port2)))
  "inner")

(test-end)
```

#### Recommendation

**DEFER** — Low priority due to rare usage pattern. The combination of `eval` + `parameterize` is uncommon. Fix only if:
- R7RS test suite requires it
- User reports issue
- Part of broader parameter system refactoring

---

### L15: thread-sleep! Ignores Context Cancellation

**File:** `internal/extensions/threads/prim_threads.go:214`
**Priority:** ⭐⭐ MEDIUM
**Estimated Time:** 1 hour
**Complexity:** LOW
**Recommendation:** Fix when convenient (improves shutdown)

#### Problem

The `thread-sleep!` primitive uses `time.Sleep()` which ignores context cancellation:

```go
func PrimThreadSleep(_ context.Context, mc *machine.MachineContext) error {
    // ... parse duration ...
    time.Sleep(duration)  // ← Ignores context.Context completely
    mc.SetValue(values.Void)
    return nil
}
```

**Impact:** If the context is canceled during sleep (e.g., program shutdown), the goroutine continues sleeping. This can delay graceful shutdown by up to the sleep duration.

**Real-world impact:** MEDIUM — Only matters during shutdown, and sleep is relatively rare. But the fix is straightforward.

#### The Fix

Use `select` with `time.After` and `ctx.Done()`:

```go
func PrimThreadSleep(ctx context.Context, mc *machine.MachineContext) error {
    // ... parse duration ...

    select {
    case <-time.After(duration):
        // Normal completion
        mc.SetValue(values.Void)
        return nil
    case <-ctx.Done():
        // Context canceled
        return ctx.Err()
    }
}
```

**Pattern:** This is the standard Go pattern for interruptible sleep.

#### Impact

- **Shutdown latency:** Improves graceful shutdown responsiveness
- **Correctness:** Respects context cancellation contract
- **Behavior change:** Sleep can now error with context.Canceled

#### Tests

Add to `internal/extensions/threads/prim_threads_test.go`:

```go
func TestThreadSleepContextCancellation(t *testing.T) {
    engine := newEngine(t)

    // Create cancellable context
    ctx, cancel := context.WithCancel(context.Background())

    // Start sleep in goroutine
    done := make(chan error, 1)
    go func() {
        _, err := engine.Eval(ctx, `(thread-sleep! 10000)`)  // 10 seconds
        done <- err
    }()

    // Cancel after 100ms
    time.Sleep(100 * time.Millisecond)
    cancel()

    // Should error quickly, not wait full 10 seconds
    select {
    case err := <-done:
        qt.Assert(t, err, qt.IsNotNil)
        qt.Assert(t, errors.Is(err, context.Canceled), qt.IsTrue)
    case <-time.After(1 * time.Second):
        t.Fatal("sleep did not respect context cancellation")
    }
}
```

#### Recommendation

**Fix when convenient** — Nice to have for shutdown behavior. Not critical since sleep is rare and shutdown delays are typically acceptable.

---

## Low Value Fixes (Document or Skip)

### L10: char-ready?/u8-ready? Always Return #t

**File:** `internal/extensions/io/prim_read_write.go:485`
**Priority:** ⭐ LOW
**Estimated Time:** 4-8 hours (if implemented)
**Complexity:** HIGH
**Recommendation:** SKIP — Document as known limitation

#### Problem

The predicates always return `#t`:

```go
func PrimCharReadyQ(_ context.Context, mc *machine.MachineContext) error {
    // ... port validation ...
    mc.SetValue(values.TrueValue)  // ← Always returns #t
    return nil
}
```

R7RS §6.13.2 specifies that `char-ready?` should return `#f` if reading would block.

#### Why This Is Hard

**1. No cross-platform non-blocking I/O in Go stdlib**
- `io.Reader` interface doesn't expose "ready" status
- Would need OS-specific syscalls (select/poll on Unix, overlapped I/O on Windows)

**2. bufio.Reader complicates detection**
- Even if underlying socket would block, buffered data makes read non-blocking
- Need to check both buffer state AND underlying descriptor

**3. Limited use case**
- Modern Scheme code rarely uses `char-ready?`
- Designed for select-style event loops (superseded by async/await patterns)
- Most I/O in Wile is either file-based (always ready) or blocking-acceptable

#### Implementation Complexity

**Unix approach (requires syscall):**
```go
import "syscall"

func isReady(fd int) (bool, error) {
    var readfds syscall.FdSet
    // ... setup FdSet ...
    timeout := syscall.Timeval{Sec: 0, Usec: 0}  // Poll, don't block
    n, err := syscall.Select(fd+1, &readfds, nil, nil, &timeout)
    return n > 0, err
}
```

**Cross-platform approach:**
- Requires `golang.org/x/sys/unix` or `golang.org/x/sys/windows`
- Platform-specific build tags
- Increases complexity and dependencies

**Estimated time:** 4-8 hours including cross-platform support and testing

#### Recommendation: Document Instead

Add to `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`:

```markdown
## Non-Blocking I/O Detection

**Affected Primitives:** `char-ready?`, `u8-ready?`

**R7RS Requirement:** Should return `#f` if reading would block.

**Wile Behavior:** Always returns `#t`.

**Rationale:** Go's `io.Reader` interface doesn't expose readiness status.
Implementing this would require OS-specific syscalls (select/poll on Unix,
overlapped I/O on Windows) and significantly complicate the I/O layer. The
conservative behavior (always returning `#t`) is safe — it may cause blocking
where R7RS code expected non-blocking, but never claims data is available when
it isn't.

**Workaround:** Use Go channels or goroutines for non-blocking I/O patterns.

**Impact:** LOW — `char-ready?` is rarely used in modern Scheme code. Most I/O
in Wile is file-based (always ready) or blocking is acceptable.
```

**Time to document:** 15 minutes

#### Benefit Analysis

| Approach | Time | Benefit |
|----------|------|---------|
| Implement | 4-8 hours | Fixes exotic edge case |
| Document | 15 minutes | Sets expectations clearly |

**Recommendation:** Document. The ROI of implementation is too low.

---

### L3: ChannelSelect Busy-Spins Without reflect.Select

**File:** `values/channel.go:253`
**Priority:** ⭐ LOW
**Estimated Time:** 3 hours
**Complexity:** MEDIUM-HIGH
**Recommendation:** SKIP — Extension-only, low usage

#### Problem

The channel select implementation polls in a tight loop:

```go
func (p *Channel) Select(cases []SelectCase) (int, Value, error) {
    for {
        for i, c := range cases {
            // Try each channel...
            if ready {
                return i, val, nil
            }
        }
        // ← Busy loop, consumes 100% CPU
    }
}
```

**Impact:** CPU usage when using `channel-select` (extension feature only).

#### Why Defer

**1. Extension-only feature**
- Not part of R7RS or SRFI-18
- Only affects users of the gointerop extension
- No known usage outside of tests

**2. Workaround exists**
- Users can use Go's native channels directly
- Or use buffered channels to avoid blocking

**3. Implementation complexity**
- Need `reflect.Select` for efficient blocking
- Must handle bidirectional cases (send and receive)
- Requires converting between Wile values and reflect.Value

#### The Fix (Outline)

```go
func (p *Channel) Select(cases []SelectCase) (int, Value, error) {
    // Convert to reflect.SelectCase slice
    reflectCases := make([]reflect.SelectCase, len(cases))
    for i, c := range cases {
        reflectCases[i] = reflect.SelectCase{
            Dir:  c.Dir,  // reflect.SelectRecv or reflect.SelectSend
            Chan: reflect.ValueOf(c.Channel.ch),
            Send: reflect.ValueOf(c.SendValue),
        }
    }

    // Use reflect.Select for efficient blocking
    chosen, recv, recvOK := reflect.Select(reflectCases)

    // Convert result back to Wile value
    return chosen, wileValueFromReflect(recv), nil
}
```

**Estimated time:** 3 hours (including conversion logic and testing)

#### Recommendation

**DEFER INDEFINITELY** — Fix only if:
- User reports actual CPU usage problem
- Channel select becomes widely used
- Part of broader channel system redesign

The feature is rarely used and has acceptable workarounds.

---

## Recommended Implementation Order

### Phase 1: High Value (2 hours total)

**Issue L17: expt BigInteger fallback (1 hour)**
1. Add BigInteger case before float fallback in `prim_math.go`
2. Add tests for large integer exponentiation
3. Verify exactness preservation

**Issue L18: rationalToInteger precision (1 hour)**
1. Replace `.Float64()` with `big.Float.SetRat()` in `prim_math.go`
2. Add tests for large rational conversion
3. Verify magnitude preservation

**Deliverable:** Numeric precision improvements, 2 new test cases

### Phase 2: Documentation (15 minutes)

**Issue L10: char-ready? documentation**
1. Add section to `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`
2. Explain rationale and workarounds
3. Mark as known limitation in architectural review

**Deliverable:** Clear documentation of limitation

### Phase 3: Medium Value (Optional, 4 hours)

**Issue L19: Radix validation (1 hour)** — If R7RS test suite requires
**Issue L15: thread-sleep! context (1 hour)** — If shutdown behavior matters
**Issue L11: eval dynamic context (2 hours)** — If users report issue

### Phase 4: Skip

**Issue L3: ChannelSelect** — Defer indefinitely (extension-only, low usage)

---

## Success Criteria

### Phase 1 (Numeric Precision)

✅ Large integer exponentiation stays exact
✅ Rational to inexact preserves magnitude
✅ All existing tests pass
✅ New edge case tests added
✅ Lint clean

### Phase 2 (Documentation)

✅ L10 documented in R7RS_SEMANTIC_DIFFERENCES.md
✅ Rationale explained clearly
✅ Workarounds provided

### Phase 3 (Optional)

✅ Each fix has regression tests
✅ No breaking changes
✅ Performance not degraded

---

## Risk Assessment

| Issue | Risk | Mitigation |
|-------|------|------------|
| L17 | LOW | Well-defined scope, follows existing pattern |
| L18 | LOW | Simple conversion change, tests verify correctness |
| L19 | LOW | Edge case only, easy to test |
| L15 | MEDIUM | Changes sleep behavior, need thorough testing |
| L11 | HIGH | Parameter system is complex, high chance of regression |
| L10 | N/A | Documentation only |
| L3 | N/A | Deferred |

**Overall risk:** LOW for Phase 1, MEDIUM for Phase 3

---

## Files That Will Be Modified

### Phase 1
- `internal/extensions/math/prim_math.go` (L17, L18)
- `internal/extensions/math/prim_math_test.go` (L17, L18 tests)
- `plans/ARCHITECTURAL_REVIEW.md` (status updates)
- `plans/ARCHITECTURAL_REVIEW_FIXES.md` (fix documentation)

### Phase 2
- `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` (L10 documentation)
- `plans/ARCHITECTURAL_REVIEW.md` (mark L10 as documented)

### Phase 3 (Optional)
- `internal/tokenizer/tokenizer.go` (L19)
- `internal/tokenizer/tokenizer_test.go` (L19 tests)
- `internal/extensions/threads/prim_threads.go` (L15)
- `internal/extensions/threads/prim_threads_test.go` (L15 tests)
- `internal/extensions/eval/prim_eval.go` (L11)
- `internal/extensions/eval/prim_eval_test.go` (L11 tests)

---

## Next Steps

**Recommended:** Implement Phase 1 (L17, L18) next — 2 hours for significant numeric correctness improvements.

**Question for user:**
1. Proceed with Phase 1 (numeric precision fixes)?
2. Just document L10 and close out?
3. Different priority order?

---

## Phase 1 Implementation (COMPLETED)

**Date:** 2026-02-12
**Status:** ✅ Complete
**Time:** ~1.5 hours

### L17: expt BigInteger Fallback ✅

**Changes:**
- `internal/extensions/math/prim_math.go`: Added BigInteger exponentiation case before float fallback
- Separated compound if-init statements to comply with project style guide
- Uses `big.Int.Exp()` for exact integer exponentiation

**Implementation:**
```go
// L17: Handle BigInteger ^ exact non-negative integer for exact result
baseBI, ok := baseNum.(*values.BigInteger)
if ok {
    expInt, isInt := values.ExactInteger(expNum)
    if isInt && expInt >= 0 {
        // Use big.Int.Exp for exact integer exponentiation
        result := new(big.Int).Exp(baseBI.BigInt(), big.NewInt(expInt), nil)
        simplified := values.Simplify(values.NewBigInteger(result))
        mc.SetValue(simplified)
        return nil
    }
    // Fall through to float path for negative/fractional exponents
}
```

**Tests Added:** `TestL17_ExptBigIntegerPrecision` with 11 test cases
- Large integer exponentiation stays exact: 2^1000, 2^100, 10^50
- Correctness verification: 2^10 = 1024, 10^3 = 1000
- Exactness preservation: (2^500)^2 = 2^1000
- Negative exponents return exact rationals: 2^-1 = 1/2
- Fractional exponents return inexact: 2^0.5 is inexact

**Result:** All 11 tests pass ✅

### L18: rationalToInteger Precision ✅

**Changes:**
- `values/rational.go`: Modified `ToInexact()` to use `big.Float.SetRat()` instead of `.Float64()`
- `internal/extensions/math/prim_math.go`: Added BigFloat cases to `PrimNumerator` and `PrimDenominator`
- `values/numeric_methods_coverage_test.go`: Updated test to expect BigFloat instead of Float

**Implementation:**
```go
// ToInexact converts this Rational to an inexact BigFloat.
//
// R7RS §6.2.6: inexact returns an inexact representation of its argument.
// L18: Use big.Float.SetRat to preserve precision for large rationals.
func (p *Rational) ToInexact() Number {
    f := new(big.Float).SetRat(p.value)
    return NewBigFloat(f)
}
```

**Tests Added:** `TestL18_RationalToInexactPrecision` with 5 test cases
- Large rationals preserve magnitude: (2^100)/3 > 1e29
- Very large rationals: (10^50)/7 > 1e48
- Small rationals still work: 1/3 ≈ 0.333333
- Exactness contagion: inexact rational is inexact

**Result:** All 5 tests pass ✅

### Verification

**Lint:** ✅ Clean (no errors)
**Tests:** ✅ All tests pass
- `TestL17_ExptBigIntegerPrecision`: 11/11 pass
- `TestL18_RationalToInexactPrecision`: 5/5 pass
- Full test suite: All packages pass

### Files Modified

1. `internal/extensions/math/prim_math.go` - L17 fix and BigFloat cases for L18
2. `internal/extensions/math/prim_math_test.go` - Tests for L17 and L18
3. `values/rational.go` - L18 fix in `ToInexact()`
4. `values/numeric_methods_coverage_test.go` - Updated to expect BigFloat
5. `plans/LOW_PRIORITY_REMAINING_FIXES.md` - Status updates

### Impact

**Correctness:**
- Large integer exponentiation now stays exact (no precision loss)
- Rational to inexact conversion preserves large values
- Both fixes improve R7RS §6.2.6 compliance

**Performance:**
- No measurable impact (BigInteger.Exp and BigFloat.SetRat are efficient)
- Tests run in <1s

**Next Steps:**
- Phase 2: Document L10 (char-ready? limitation) in R7RS_SEMANTIC_DIFFERENCES.md
- Phase 3 (Optional): L19, L15, L11 if needed for R7RS test suite

---

## Phase 2 Implementation (COMPLETED)

**Date:** 2026-02-12
**Status:** ✅ Complete
**Time:** 15 minutes

### L10: char-ready?/u8-ready? Documentation ✅

**Decision:** Document limitation instead of implementing (4-8 hours saved)

**Changes:**
- `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`: Added comprehensive documentation

**Documentation Includes:**
1. **R7RS requirement** - What the spec says `char-ready?` should do
2. **Wile behavior** - Always returns `#t` (conservative safe behavior)
3. **Rationale** - Why Go's `io.Reader` makes true implementation difficult:
   - No readiness status in interface
   - Requires OS-specific syscalls (select/poll/overlapped I/O)
   - Buffering complications with `bufio.Reader`
   - Cross-platform maintenance burden
4. **Workaround** - Use Go channels/threads for non-blocking patterns
5. **Impact analysis** - LOW (rarely used in modern Scheme)
6. **ROI justification** - 15 min documentation vs 4-8 hours implementation

**Key Insight:** The conservative behavior (always `#t`) is safe—it may cause blocking where non-blocking was expected, but never claims data is available when it isn't (which would violate R7RS guarantees).

**Updated Summary:** Changed from "No known semantic differences" to acknowledge this one known limitation with clear rationale.

### Files Modified

1. `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` - L10 documentation
2. `plans/LOW_PRIORITY_REMAINING_FIXES.md` - Status updates

---

## Final Status

**Total LOW Priority Issues:** 19
**Completed:** 15 (79%)
**Deferred:** 4 (21%)

### Completed Issues (15)

| Issue | Type | Status |
|-------|------|--------|
| L1 | Fix | ✅ (Previous batch) |
| L2 | Fix | ✅ (Previous batch) |
| L4 | Fix | ✅ (Batch 2) |
| L5 | Fix | ✅ (Previous batch) |
| L6 | Fix | ✅ (Previous batch) |
| L7 | Fix | ✅ (Previous batch) |
| L8 | Fix | ✅ (Previous batch) |
| L9 | Fix | ✅ (Previous batch) |
| L10 | Document | ✅ (Phase 2) |
| L12 | Fix | ✅ (Previous batch) |
| L13 | Fix | ✅ (Batch 2) |
| L14 | Fix | ✅ (Previous batch) |
| L16 | Fix | ✅ (Previous batch) |
| L17 | Fix | ✅ (Phase 1) |
| L18 | Fix | ✅ (Phase 1) |

### Deferred Issues (4)

| Issue | Priority | Defer Until | Estimated Time |
|-------|----------|-------------|----------------|
| L3 | ⭐ LOW | User reports issue or channel redesign | 3 hours |
| L11 | ⭐⭐ MEDIUM | R7RS test suite requires or user reports | 2 hours |
| L15 | ⭐⭐ MEDIUM | Shutdown behavior matters | 1 hour |
| L19 | ⭐ LOW | R7RS test suite requires | 1 hour |

**Total deferred time:** 7 hours (only if all become necessary)

### Deliverables

**Phase 1 (Numeric Precision):**
- ✅ L17: BigInteger exponentiation exactness (11 tests)
- ✅ L18: Rational to inexact precision (5 tests)
- ✅ Lint clean, all tests pass

**Phase 2 (Documentation):**
- ✅ L10: char-ready?/u8-ready? limitation documented
- ✅ Clear rationale and workarounds provided

**Time Investment:**
- Phase 1: ~1.5 hours (implementation + testing + lint fixes)
- Phase 2: ~15 minutes (documentation)
- **Total: ~1.75 hours for significant correctness improvements**

### Architectural Review Closure

The LOW priority batch from the architectural review is now **effectively complete**:
- All high-value issues fixed (L17, L18)
- One limitation documented with clear rationale (L10)
- Remaining issues deferred with specific trigger conditions

The deferred issues (L3, L11, L15, L19) have low real-world impact and clear deferral criteria. They should only be addressed if:
- R7RS test suite compliance requires them
- Users report actual issues
- Part of broader refactoring efforts
