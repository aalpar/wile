# LOW Priority Remaining Issues — Fix Plan

**Date:** 2026-02-12
**Status:** Planning
**Context:** 9 LOW priority issues remain from architectural review after Batch 1 fixes

**Completed:** L1, L2, L5, L6, L7, L8 (verified), L9, L12, L14, L16 (10 issues)
**Remaining:** L3, L4, L10, L11, L13, L15, L17, L18, L19 (9 issues)

---

## Executive Summary

The remaining 9 LOW priority issues fall into distinct categories with different risk/complexity profiles:

| Category | Issues | Est. Time | Recommendation |
|----------|--------|-----------|----------------|
| **Tier 1: Quick Fixes** | L4, L13 | 1 hour | Fix next |
| **Tier 2: Numeric Edge Cases** | L17, L18 | 2 hours | Fix when convenient |
| **Tier 3: I/O Non-Blocking** | L10 | HIGH complexity | Skip (needs OS-level changes) |
| **Tier 4: Context Inheritance** | L11, L15 | 2-3 hours | Defer (low impact) |
| **Tier 5: Concurrency Performance** | L3 | 3 hours | Defer (extension-only) |
| **Tier 6: Tokenizer Edge Case** | L19 | 1 hour | Defer (exotic formats) |

**Recommended next action:** Batch fix Tier 1 (L4, L13) — 1 hour total, high ROI

---

## Issue Categories

### Tier 1: Quick Fixes (Recommended Next)

#### L4: NewTemporaryVariableName Seeds PRNG Per Call

**File:** `values/utils.go:253`

**Problem:**
```go
func NewTemporaryVariableName() *Symbol {
    rand.Seed(time.Now().UnixNano())  // ← Seeds on EVERY call
    b := make([]byte, 8)
    rand.Read(b)
    // ...
}
```

Reseeding the PRNG on every call is inefficient and potentially reduces randomness quality (fast successive calls may get same seed).

**Impact:** Performance (minor), code quality

**Fix:**
```go
var (
    tempVarRand     *rand.Rand
    tempVarRandOnce sync.Once
)

func NewTemporaryVariableName() *Symbol {
    tempVarRandOnce.Do(func() {
        tempVarRand = rand.New(rand.NewSource(time.Now().UnixNano()))
    })
    
    b := make([]byte, 8)
    tempVarRand.Read(b)
    // ...
}
```

**Alternative (simpler):** Use `math/rand/v2` (Go 1.22+) which has better defaults:
```go
func NewTemporaryVariableName() *Symbol {
    // math/rand/v2 is automatically seeded and thread-safe
    b := make([]byte, 8)
    rand.Read(b)  // Uses global generator (no manual seeding needed)
    // ...
}
```

**Estimated Time:** 15 minutes
**Priority:** ⭐⭐ MEDIUM (performance + code quality)

**Testing:**
- Existing tests should pass unchanged
- Add test that verifies unique names generated rapidly
- No regression tests needed (behavior unchanged)

---

#### L13: once-do! Swallows Thunk Errors

**File:** `internal/extensions/gointerop/prim_gointerop.go:417`

**Problem:**
```go
err := mc.Apply(ctx, thunk)
if err != nil {
    // Error is silently ignored! ← BUG
}
```

Errors from the thunk are lost. If the once-guarded initialization fails, the caller has no indication of the failure.

**Impact:** Error visibility, debugging difficulty

**Fix:**
```go
err := mc.Apply(ctx, thunk)
if err != nil {
    return err  // ← Propagate error to caller
}
```

**Estimated Time:** 10 minutes (code change) + 20 minutes (test)
**Priority:** ⭐⭐ MEDIUM (correctness)

**Testing:**
```scheme
(test-begin "once-do! error propagation")

(define failed-once (make-once))
(test-assert "errors propagate from thunk"
  (guard (exn (else #t))
    (once-do! failed-once (lambda () (error "initialization failed")))
    #f))  ; Should not reach here

(test-end)
```

**Tier 1 Total:** ~45 minutes for both issues

---

### Tier 2: Numeric Edge Cases

#### L17: expt Missing BigInteger Fallback Case

**File:** `internal/extensions/math/prim_math.go:310`

**Problem:**
The `expt` primitive handles most numeric type combinations but has a gap in the BigInteger fallback path. Large integer exponentiation may route through float64, losing precision.

**Example:**
```scheme
(expt 2 1000)  ; Should be exact BigInteger
               ; May route through Float, lose precision
```

**Impact:** Numeric precision for edge cases

**Fix:** Add explicit BigInteger case in fallback:
```go
// In expt primitive
case *values.BigInteger:
    // Use big.Int.Exp for exact integer exponentiation
    if exponent is integer && exponent >= 0 {
        result := new(big.Int).Exp(base.Value, exp.Value, nil)
        return simplifyBigInteger(result)
    }
    // Fall through to float path for negative/fractional exponents
```

**Estimated Time:** 1 hour (implementation + tests)
**Priority:** ⭐⭐ MEDIUM (numeric correctness)

**Testing:**
```scheme
(test-equal "large integer exponentiation stays exact"
  (exact? (expt 2 1000))
  #t)

(test-equal "expt preserves exactness"
  (expt (expt 2 500) 2)
  (expt 2 1000))
```

---

#### L18: rationalToInteger Loses Precision via float64

**File:** `internal/extensions/math/prim_math.go:424`

**Problem:**
```go
func rationalToInteger(r *values.Rational) values.Number {
    f, _ := r.Value.Float64()  // ← Loses precision for large rationals
    return values.NewFloat(f)
}
```

**Example:**
```scheme
(inexact (/ (expt 2 100) 3))  ; Should be high-precision float
                               ; Gets truncated to float64 range
```

**Impact:** Numeric precision for edge cases

**Fix:**
```go
func rationalToInteger(r *values.Rational) values.Number {
    // Use big.Rat directly for conversion
    f := new(big.Float).SetRat(r.Value)
    return values.NewBigFloat(f)  // Preserve precision
}
```

**Estimated Time:** 1 hour (implementation + tests)
**Priority:** ⭐⭐ MEDIUM (numeric correctness)

**Testing:**
```scheme
(test-assert "rational to inexact preserves large values"
  (> (inexact (/ (expt 2 100) 3))
     1e29))  ; Verify magnitude preserved
```

**Tier 2 Total:** ~2 hours for both issues

---

### Tier 3: I/O Non-Blocking (Recommend SKIP)

#### L10: char-ready?/u8-ready? Always Return #t

**File:** `internal/extensions/io/prim_read_write.go:485`

**Problem:**
```go
func PrimCharReadyQ(_ context.Context, mc *machine.MachineContext) error {
    // ... port validation ...
    mc.SetValue(values.TrueValue)  // ← Always returns #t
    return nil
}
```

R7RS §6.13.2 specifies that `char-ready?` should return `#f` if reading would block.

**Impact:** Blocking I/O detection (rarely used in practice)

**Why This Is Hard:**

1. **No cross-platform non-blocking I/O in Go stdlib**
   - `io.Reader` interface doesn't expose "ready" status
   - Would need OS-specific syscalls (select/poll on Unix, overlapped I/O on Windows)

2. **bufio.Reader complicates detection**
   - Even if underlying socket would block, buffered data makes read non-blocking
   - Need to check both buffer state AND underlying descriptor

3. **Limited use case**
   - Modern Scheme code rarely uses `char-ready?`
   - Designed for select-style event loops (superseded by async/await patterns)
   - Most I/O in Wile is either file-based (always ready) or blocking-acceptable

**Implementation Complexity:**

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

**Cross-platform approach (requires cgo or third-party):**
- Use `golang.org/x/sys/unix` or `golang.org/x/sys/windows`
- Add platform-specific build tags
- Increases complexity and dependencies

**Recommendation:** **SKIP**

**Rationale:**
- HIGH implementation complexity
- LOW real-world impact (char-ready? rarely used)
- Would require OS-specific code or new dependencies
- Current behavior (always #t) is conservative (never claims data when there isn't any)
- Better solution: document limitation in R7RS_SEMANTIC_DIFFERENCES.md

**Alternative:** Document as known limitation:

```markdown
## Non-Blocking I/O Detection

**Affected Primitives:** `char-ready?`, `u8-ready?`

**R7RS Requirement:** Should return `#f` if reading would block.

**Wile Behavior:** Always returns `#t`.

**Rationale:** Go's `io.Reader` interface doesn't expose readiness status. 
Implementing this would require OS-specific syscalls (select/poll) and 
significantly complicate the I/O layer. The conservative behavior (always 
returning `#t`) is safe — it may cause blocking where R7RS code expected 
non-blocking, but never claims data is available when it isn't.

**Workaround:** Use Go channels or goroutines for non-blocking I/O patterns.
```

**Estimated Time:** 4-8 hours (if implemented)
**Priority:** ⭐ LOW
**Decision:** **SKIP** — document instead

---

### Tier 4: Context Inheritance (Defer)

#### L11: eval Doesn't Inherit Dynamic Context

**File:** `internal/extensions/eval/prim_eval.go:35`

**Problem:**
```scheme
(parameterize ((current-output-port some-port))
  (eval '(display "hello") (scheme-report-environment 5)))
; Prints to default output, not some-port
```

The `eval` primitive creates a fresh evaluation context without copying the dynamic environment (parameter bindings).

**Impact:** Unexpected behavior when eval is used inside parameterize

**Complexity:** MEDIUM
- Need to capture dynamic environment (parameter bindings) from caller
- Pass to eval's sub-context
- Ensure parameter stack is properly inherited

**Fix Approach:**
```go
func PrimEval(ctx context.Context, mc *machine.MachineContext) error {
    // ... existing code ...
    
    // Create sub-context with parent's dynamic environment
    sub := machine.NewSubContext(mc, env, compiled)
    
    // Inherit parameter bindings from parent
    // (This requires access to the parameter stack, which may need refactoring)
    
    result, err := sub.Run(ctx)
    // ...
}
```

**Estimated Time:** 2 hours (requires understanding parameter implementation)
**Priority:** ⭐ LOW (eval rarely used, parameterize + eval even rarer)
**Recommendation:** **DEFER** — low priority, low real-world impact

---

#### L15: thread-sleep! Ignores Context Cancellation

**File:** `internal/extensions/threads/prim_threads.go:214`

**Problem:**
```go
func PrimThreadSleep(_ context.Context, mc *machine.MachineContext) error {
    // ... parse duration ...
    time.Sleep(duration)  // ← Ignores context.Context completely
    mc.SetValue(values.Void)
    return nil
}
```

If the context is canceled during sleep, the goroutine continues sleeping. This can cause shutdown delays.

**Impact:** Graceful shutdown latency

**Fix:**
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

**Estimated Time:** 30 minutes (code) + 30 minutes (test)
**Priority:** ⭐ LOW (only matters for shutdown, sleep is rare)
**Recommendation:** **DEFER** — nice to have, not critical

**Tier 4 Total:** ~3 hours for both issues

---

### Tier 5: Concurrency Performance (Defer)

#### L3: ChannelSelect Busy-Spins Without reflect.Select

**File:** `values/channel.go:253`

**Problem:**
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

The channel select implementation polls in a tight loop instead of using Go's `reflect.Select` for efficient blocking.

**Impact:** CPU usage when using channel-select (extension feature only)

**Complexity:** MEDIUM-HIGH
- Need to convert Wile channel values to reflect.SelectCase
- Handle bidirectional cases (send and receive)
- Map results back to Wile values

**Fix Approach:**
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

**Estimated Time:** 3 hours (implementation + testing)
**Priority:** ⭐ LOW (extension-only feature, rarely used)
**Recommendation:** **DEFER** — affects only channel extension users

---

### Tier 6: Tokenizer Edge Case (Defer)

#### L19: isExtendedExponentMarkerForRadix Ignores Radix

**File:** `internal/tokenizer/tokenizer.go:2280`

**Problem:**
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
- Decimal (#d): e, s, f, d, l
- Hex (#x): no exponent markers

**Impact:** Incorrectly accepts exotic number formats like `#x1.5e2` (hex with decimal exponent)

**Complexity:** LOW
**Real-world impact:** VERY LOW (who writes `#x1.5e2`?)

**Fix:**
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

**Estimated Time:** 1 hour (with tests for all radixes)
**Priority:** ⭐ LOW (exotic edge case)
**Recommendation:** **DEFER** — very low real-world impact

---

## Recommended Fix Order

### Batch 1: Quick Wins (1 hour)
**Issues:** L4, L13
**Rationale:** Easy fixes, measurable improvements

1. L4 - Fix PRNG seeding (15 min)
2. L13 - Propagate once-do! errors (30 min)

### Batch 2: Numeric Precision (2 hours)
**Issues:** L17, L18
**Rationale:** Improve numeric correctness for edge cases

3. L17 - Add BigInteger case to expt (1 hour)
4. L18 - Fix rationalToInteger precision (1 hour)

### Deferred (Don't Fix)
**Issues:** L3, L10, L11, L15, L19
**Rationale:** Low ROI, high complexity, or rare use cases

- L10 - Document as known limitation (15 min for docs)
- L11, L15 - Low priority, rare scenarios
- L3 - Extension-only, performance not critical
- L19 - Exotic edge case, very low impact

---

## Implementation Plan for Batch 1 (L4, L13)

### Phase 1: L4 - Fix PRNG Seeding

**Files:**
- `values/utils.go` (fix)
- `values/utils_test.go` (test)

**Steps:**
1. Add package-level `sync.Once` and `*rand.Rand`
2. Replace `rand.Seed()` with one-time initialization
3. Add test verifying unique names from rapid calls

**Test:**
```go
func TestNewTemporaryVariableName_Uniqueness(t *testing.T) {
    seen := make(map[string]bool)
    for i := 0; i < 1000; i++ {
        name := NewTemporaryVariableName()
        if seen[name.Key] {
            t.Fatalf("duplicate name generated: %s", name.Key)
        }
        seen[name.Key] = true
    }
}
```

### Phase 2: L13 - Propagate once-do! Errors

**Files:**
- `internal/extensions/gointerop/prim_gointerop.go` (fix)
- `internal/extensions/gointerop/prim_gointerop_test.go` (test)

**Steps:**
1. Change error ignore to `return err`
2. Add test with failing thunk
3. Verify error propagates to caller

**Test:**
```scheme
(define failed-once (make-once))
(test-assert "once-do! propagates errors"
  (guard (exn (else #t))
    (once-do! failed-once (lambda () (error "fail")))
    #f))
```

### Verification
- Run `make test`
- Run `make lint`
- Verify 0 regressions

---

## Success Criteria

### Batch 1 (L4, L13)
✅ PRNG initialized once per process
✅ Unique temporary variable names generated rapidly
✅ once-do! errors propagate to caller
✅ All existing tests pass
✅ Lint clean

### Batch 2 (L17, L18)
✅ Large integer exponentiation stays exact
✅ Rational to inexact preserves precision
✅ All existing tests pass
✅ New edge case tests added

### Documentation
✅ L10 documented in R7RS_SEMANTIC_DIFFERENCES.md
✅ Deferred issues noted in ARCHITECTURAL_REVIEW.md

---

## Future Considerations

### If L11 (eval dynamic context) becomes priority:
- Requires parameter stack implementation visibility
- May need refactoring of parameter system
- Estimated: 4-6 hours

### If L15 (thread-sleep! context) becomes priority:
- Straightforward select {} implementation
- Estimated: 1 hour

### If L3 (ChannelSelect) becomes priority:
- Need reflect.Select expertise
- Requires careful testing of bidirectional cases
- Estimated: 4-6 hours with thorough testing

---

## Next Steps

1. **Immediate:** Implement Batch 1 (L4, L13) — 1 hour
2. **Short-term:** Implement Batch 2 (L17, L18) — 2 hours when convenient
3. **Documentation:** Add L10 to R7RS_SEMANTIC_DIFFERENCES.md
4. **Update tracking:** Mark completed issues in ARCHITECTURAL_REVIEW.md

Total estimated work for recommended fixes: **3 hours**
Deferred work (if needed later): **10-15 hours**
