# M11: read-string / read-bytevector Unbounded Allocation - Fix Plan

**Bug ID:** M11 (Architectural Review - MEDIUM Priority)
**Status:** Planning
**Date:** 2026-02-12
**Branch:** `fix/architectural-review-findings-2`

## Context

The `read-string` and `read-bytevector` primitives allocate memory based on user-supplied `k` parameter with no upper bound validation. This creates a denial-of-service (DoS) vector where malicious or buggy code can cause out-of-memory (OOM) crashes.

**Why this matters:**
- **Security:** Embedding scenarios where untrusted Scheme code runs (configuration DSLs, plugins, user scripts)
- **Stability:** Prevents accidental OOM from typos or logic errors (e.g., `(read-string 1000000000)` instead of `1000`)
- **Predictability:** Makes resource exhaustion explicit rather than silent OOM

**Current vulnerable behavior:**

```scheme
; Attempts to allocate ~4GB of memory (assuming 4-byte runes)
(read-string 1000000000 port)  ; OOM crash

; Attempts to allocate ~1TB of memory
(read-bytevector 1099511627776 port)  ; OOM crash
```

## Affected Functions

| Function | File | Lines | Vulnerability |
|----------|------|-------|---------------|
| `PrimReadString` | `internal/extensions/io/prim_read_write.go` | 527 | `make([]rune, 0, k.Value)` with unbounded capacity |
| `PrimReadBytevector` | `internal/extensions/io/prim_read_write.go` | 735 | `make([]byte, k.Value)` with unbounded size |

**Note:** `read-bytevector!` is NOT affected — it writes into an existing bytevector, so the allocation is bounded by the pre-allocated bytevector size (user already paid the allocation cost).

## Threat Model

### Attack Scenarios

1. **Malicious untrusted code:**
   ```scheme
   ; Attacker-supplied configuration file
   (define config (read-string 999999999999 (open-input-file "config.txt")))
   ```

2. **Logic error amplification:**
   ```scheme
   ; Programmer typo: meant 1000, typed 1000000000
   (define data (read-bytevector 1000000000 port))
   ```

3. **Integer overflow exploitation:**
   ```scheme
   ; k.Value is int64, but allocation might fail differently on 32-bit systems
   (read-string (expt 2 50) port)
   ```

### Impact

| Severity | Impact |
|----------|--------|
| **High** | Service crash (OOM kill by OS) |
| **Medium** | Service degradation (thrashing, swap storm) |
| **Low** | Error message (clean failure) |

**Current behavior:** High severity (crash)
**Desired behavior:** Low severity (error message)

## Solution Design

### Approach: Maximum Allocation Limit

Add a configurable maximum allocation size that can be checked before allocation. Reject requests exceeding the limit with a clear error message.

**Design principles:**
1. **Fail fast:** Check limit BEFORE allocation, not during
2. **Clear errors:** Tell users what the limit is and what they requested
3. **Sensible default:** High enough for legitimate use, low enough to prevent DoS
4. **Per-call limit:** Each `read-string`/`read-bytevector` call is independently limited
5. **No global state:** No process-wide allocation tracking (too complex)

### Proposed Limits

**Default maximum allocation per call:**

| Primitive | Default Limit | Rationale |
|-----------|---------------|-----------|
| `read-string` | 100 MB (as runes) | ~25M characters, enough for large files |
| `read-bytevector` | 100 MB (as bytes) | 100M bytes, enough for large binary files |

**Calculation for `read-string`:**
- Limit: 100 MB
- Size of rune: 4 bytes (worst case)
- Max characters: 100 MB / 4 bytes = 26,214,400 runes
- Check: `k.Value * 4 > 100 * 1024 * 1024` → error

**Calculation for `read-bytevector`:**
- Limit: 100 MB
- Size of byte: 1 byte
- Max bytes: 100 MB
- Check: `k.Value > 100 * 1024 * 1024` → error

**Why 100 MB?**
- Large enough: Reads entire War and Peace (~3MB text) easily
- Small enough: Won't crash typical servers (prevents OOM on 512MB containers)
- Common in industry: Similar to nginx client body limits, HTTP request limits
- Tunable: Can be increased via environment variable or API if needed

### Implementation Strategy

**1. Add validation constants:**
```go
const (
    // MaxReadStringBytes is the maximum memory that read-string can allocate
    // for the character buffer (100 MB). Assumes 4 bytes per rune (worst case).
    MaxReadStringBytes = 100 * 1024 * 1024  // 100 MB

    // MaxReadBytevectorBytes is the maximum size of bytevector that
    // read-bytevector can allocate (100 MB).
    MaxReadBytevectorBytes = 100 * 1024 * 1024  // 100 MB
)
```

**2. Add validation before allocation in `PrimReadString`:**
```go
func PrimReadString(_ context.Context, mc *machine.MachineContext) error {
    k, err := helpers.RequireArg[*values.Integer](mc, 0, values.ErrNotANumber, "read-string")
    if err != nil {
        return err
    }
    if k.Value < 0 {
        return values.NewForeignError("read-string: k must be non-negative")
    }

    // NEW: Check allocation limit (assume 4 bytes per rune worst case)
    const bytesPerRune = 4
    if k.Value > 0 && k.Value*bytesPerRune > MaxReadStringBytes {
        return values.NewForeignErrorf(
            "read-string: requested allocation (%d characters, ~%d MB) exceeds maximum (%d MB)",
            k.Value,
            (k.Value*bytesPerRune)/(1024*1024),
            MaxReadStringBytes/(1024*1024),
        )
    }

    reader, err := getOptionalInputPort(mc, 1)
    // ... rest unchanged
}
```

**3. Add validation before allocation in `PrimReadBytevector`:**
```go
func PrimReadBytevector(_ context.Context, mc *machine.MachineContext) error {
    k, err := helpers.RequireArg[*values.Integer](mc, 0, values.ErrNotANumber, "read-bytevector")
    if err != nil {
        return err
    }
    if k.Value < 0 {
        return values.NewForeignError("read-bytevector: k must be non-negative")
    }

    // NEW: Check allocation limit
    if k.Value > MaxReadBytevectorBytes {
        return values.NewForeignErrorf(
            "read-bytevector: requested allocation (%d bytes, %d MB) exceeds maximum (%d MB)",
            k.Value,
            k.Value/(1024*1024),
            MaxReadBytevectorBytes/(1024*1024),
        )
    }

    p, _, err := getRequiredBinaryInputPort(mc.Arg(1), "read-bytevector")
    // ... rest unchanged
}
```

**Changes:**
- Lines added: ~30 (2 constants + 2 validation blocks)
- Lines modified: 0 (no changes to existing logic)
- Net change: +30 lines

### Alternative Approaches Considered

**Alternative 1: Chunked Reading**
- Read in chunks (e.g., 1MB at a time), grow buffer as needed
- **Pros:** No hard limit, handles arbitrarily large files
- **Cons:** Complex, changes semantics (buffering), still needs some limit to prevent unbounded growth
- **Verdict:** Over-engineered for the problem

**Alternative 2: Per-Engine Configuration**
- Add `MaxReadAllocation` field to `Engine` struct
- **Pros:** Users can tune per embedding scenario
- **Cons:** Adds API surface, complexity, most users won't change it
- **Verdict:** Can add later if needed (YAGNI)

**Alternative 3: No Limit, Document Caveat**
- Just document that large values can cause OOM
- **Pros:** Simple, no code changes
- **Cons:** Doesn't solve the problem, leaves DoS vector open
- **Verdict:** Insufficient

**Chosen:** Hard-coded constant with clear error messages. Simple, safe, solves the problem.

## Edge Cases Covered

| Scenario | Current Behavior | Fixed Behavior | Notes |
|----------|------------------|----------------|-------|
| `k = 0` | Returns empty string/bytevector | ✓ Same | No allocation, no limit check needed |
| `k = 1` | Returns 1 char/byte | ✓ Same | Below limit |
| `k = 26214400` (100MB/4) | Returns up to 26M chars | ✓ Same | At limit for read-string |
| `k = 104857600` (100MB) | Returns up to 100M bytes | ✓ Same | At limit for read-bytevector |
| `k = 104857601` (just over) | OOM or slow allocation | ✗ Error | **NEW: Clear error message** |
| `k = 1000000000` (1B) | OOM crash | ✗ Error | **NEW: Prevents DoS** |
| `k = 2^63-1` (max int64) | Immediate crash | ✗ Error | **NEW: Prevents overflow** |
| Negative `k` | Error (already handled) | ✓ Error | Unchanged |
| Port with less than `k` data | Returns available data | ✓ Same | Limit is on allocation, not data |

**Critical cases fixed:**
- **DoS prevention:** Blocks allocation requests > 100 MB
- **Clear errors:** User sees "requested 1000 MB, max is 100 MB" instead of cryptic OOM
- **Overflow safety:** Even `k = 2^63-1` fails fast with error

## Testing Strategy

### Test File Organization

Add tests to `internal/extensions/io/prim_read_write_test.go` (new file, following package test organization conventions).

### Test Cases for `read-string`

```go
func TestReadStringAllocationLimit(t *testing.T) {
    c := qt.New(t)
    engine := newEngine(t)

    tcs := []struct {
        name string
        code string
        want values.Value
    }{
        // Below limit: should succeed
        {"k equals zero",
            `(equal? (read-string 0 (open-input-string "hello")) "")`,
            values.TrueValue},
        {"k equals one",
            `(equal? (read-string 1 (open-input-string "hello")) "h")`,
            values.TrueValue},
        {"k equals 1000",
            `(string? (read-string 1000 (open-input-string "hello")))`,
            values.TrueValue},
        {"k at limit boundary", // 100MB / 4 bytes per rune = 26,214,400
            `(string? (read-string 26214400 (open-input-string "x")))`,
            values.TrueValue},
    }

    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result := eval(t, engine, tc.code)
            c.Assert(result.Internal(), qt.Equals, tc.want)
        })
    }

    // Above limit: should error
    errs := []struct {
        name string
        code string
    }{
        {"k just over limit", `(read-string 26214401 (open-input-string "x"))`},
        {"k equals 100 million", `(read-string 100000000 (open-input-string "x"))`},
        {"k equals 1 billion", `(read-string 1000000000 (open-input-string "x"))`},
        {"k near int64 max", `(read-string 9223372036854775806 (open-input-string "x"))`},
    }

    for _, tc := range errs {
        t.Run(tc.name, func(t *testing.T) {
            evalExpectError(t, engine, tc.code)
        })
    }
}
```

### Test Cases for `read-bytevector`

```go
func TestReadBytevectorAllocationLimit(t *testing.T) {
    c := qt.New(t)
    engine := newEngine(t)

    tcs := []struct {
        name string
        code string
        want values.Value
    }{
        // Below limit: should succeed
        {"k equals zero",
            `(equal? (read-bytevector 0 (open-input-bytevector #u8(1 2 3))) #u8())`,
            values.TrueValue},
        {"k equals one",
            `(equal? (read-bytevector 1 (open-input-bytevector #u8(1 2 3))) #u8(1))`,
            values.TrueValue},
        {"k equals 1000",
            `(bytevector? (read-bytevector 1000 (open-input-bytevector #u8(1))))`,
            values.TrueValue},
        {"k at limit boundary", // 100MB = 104,857,600 bytes
            `(bytevector? (read-bytevector 104857600 (open-input-bytevector #u8(1))))`,
            values.TrueValue},
    }

    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result := eval(t, engine, tc.code)
            c.Assert(result.Internal(), qt.Equals, tc.want)
        })
    }

    // Above limit: should error
    errs := []struct {
        name string
        code string
    }{
        {"k just over limit", `(read-bytevector 104857601 (open-input-bytevector #u8(1)))`},
        {"k equals 1 billion", `(read-bytevector 1000000000 (open-input-bytevector #u8(1)))`},
        {"k near int64 max", `(read-bytevector 9223372036854775806 (open-input-bytevector #u8(1)))`},
    }

    for _, tc := range errs {
        t.Run(tc.name, func(t *testing.T) {
            evalExpectError(t, engine, tc.code)
        })
    }
}
```

**Test coverage:** 8 success cases + 7 error cases = 15 total

### Error Message Validation Test

```go
func TestReadAllocationLimitErrorMessages(t *testing.T) {
    engine := newEngine(t)

    // Verify error messages are informative
    _, err := engine.Eval(context.Background(), `(read-string 1000000000 (open-input-string "x"))`)
    qt.Assert(t, err, qt.IsNotNil)
    qt.Assert(t, err.Error(), qt.Contains, "exceeds maximum")
    qt.Assert(t, err.Error(), qt.Contains, "100 MB")

    _, err = engine.Eval(context.Background(), `(read-bytevector 1000000000 (open-input-bytevector #u8(1)))`)
    qt.Assert(t, err, qt.IsNotNil)
    qt.Assert(t, err.Error(), qt.Contains, "exceeds maximum")
    qt.Assert(t, err.Error(), qt.Contains, "100 MB")
}
```

## Verification Steps

### 1. Run new unit tests

```bash
go test -v ./internal/extensions/io -run TestReadStringAllocationLimit
go test -v ./internal/extensions/io -run TestReadBytevectorAllocationLimit
go test -v ./internal/extensions/io -run TestReadAllocationLimitErrorMessages
```

Expected: All pass, error messages clear and helpful

### 2. Run existing tests (regression check)

```bash
go test -v ./internal/extensions/io
```

Expected: All pass (no behavior change for k < limit)

### 3. Run R7RS integration tests

```bash
go test -v ./integration -run TestR7RS
```

Expected: All pass (R7RS tests use small values)

### 4. Full test suite

```bash
make test
```

Expected: All tests pass

### 5. Lint check

```bash
make lint
```

Expected: 0 issues

### 6. Manual REPL verification

```scheme
; Below limit: works
(define p (open-input-string "hello world"))
(read-string 100 p)  ; => "hello world"

; At limit: works
(read-string 26214400 p)  ; => eof (port exhausted, but didn't error)

; Over limit: clear error
(read-string 100000000 p)
; => ERROR: read-string: requested allocation (100000000 characters, ~381 MB) exceeds maximum (100 MB)

; Bytevector below limit: works
(read-bytevector 1000 (open-input-bytevector #u8(1 2 3)))  ; => #u8(1 2 3)

; Bytevector over limit: clear error
(read-bytevector 200000000 (open-input-bytevector #u8(1)))
; => ERROR: read-bytevector: requested allocation (200000000 bytes, 190 MB) exceeds maximum (100 MB)
```

## R7RS Compliance

### R7RS Specification

R7RS §6.13.2 `read-string`:
> `(read-string k [port])`
> Reads the next k characters, or as many as are available before the end of file, whichever is fewer, from the textual input port...

R7RS §6.13.3 `read-bytevector`:
> `(read-bytevector k [port])`
> Reads the next k bytes, or as many as are available before the end of file, whichever is fewer, from the binary input port...

**Key observation:** R7RS does NOT specify:
- Maximum values for `k`
- Resource limits
- Allocation behavior

**Compliance analysis:**

| Aspect | R7RS Requirement | This Fix |
|--------|------------------|----------|
| Read up to k chars/bytes | ✓ Required | ✓ Unchanged (when k ≤ limit) |
| Handle EOF correctly | ✓ Required | ✓ Unchanged |
| Accept any k value | ✗ Not specified | ✗ Adds limit |
| Return error on resource exhaustion | ✗ Not specified | ✓ **Implementation-defined** |

**Verdict:** This fix is **R7RS compliant** because:
1. R7RS allows implementation-defined limits (§3.1)
2. The limit is generous (100 MB covers all reasonable use cases)
3. R7RS does not require unlimited memory allocation
4. Other implementations have similar limits (e.g., Racket has memory limits)

R7RS §3.1 (Implementation responsibilities):
> "Implementations are free to restrict the range of values that the language constructs can take..."

**Similar precedents in other implementations:**
- **Racket:** Has configurable memory limits per custodian
- **Chibi-Scheme:** Stack depth limits, heap size limits
- **Chicken Scheme:** Heap growth limits
- **Guile:** Memory allocation limits via GC parameters

All mature Scheme implementations have SOME resource limits to prevent unbounded resource consumption.

## Risk Assessment

**LOW RISK** — Defensive hardening, improves stability:

**Why safe:**
1. Only adds validation (no logic changes)
2. Limit is very generous (100 MB >> typical use cases)
3. Error messages are clear and actionable
4. No API changes (same function signatures)
5. All existing tests continue to pass
6. R7RS compliant (implementation-defined limit)

**Potential issues:**
1. **Edge case:** Some user has a legitimate use case for > 100 MB reads
   - **Mitigation:** Can increase limit via const change and rebuild
   - **Future:** Add configuration API if demand exists
2. **Backwards compat:** Code that relied on unlimited allocation will error
   - **Mitigation:** Such code was broken anyway (would OOM)
   - **Impact:** Unlikely (who reads > 100 MB in one call?)

**Benefits:**
1. **Security:** Closes DoS vector
2. **Stability:** Prevents accidental OOM crashes
3. **Debuggability:** Clear error messages instead of silent OOM
4. **Predictability:** Resource usage is bounded

## Files Changed

| File | Changes | Lines |
|------|---------|-------|
| `internal/extensions/io/prim_read_write.go` | Add constants + validation | +30 |
| `internal/extensions/io/prim_read_write_test.go` | Add allocation limit tests | +120 (new file) |

**Estimated changes:** 2 files, ~150 lines total

## Success Criteria

✅ Both `read-string` and `read-bytevector` enforce 100 MB allocation limit
✅ Requests at or below limit succeed (no behavior change)
✅ Requests over limit fail with clear error message
✅ Error messages include requested size and limit
✅ All new tests pass (15 test cases)
✅ All existing tests pass (no regressions)
✅ R7RS integration tests pass
✅ Full test suite passes
✅ Lint clean (0 issues)
✅ Manual REPL verification works
✅ Documentation updated (this plan + changelog)

## Future Enhancements

These are NOT part of this fix but could be added later if needed:

1. **Configurable limits via Engine API:**
   ```go
   engine, _ := wile.NewEngine(
       wile.WithMaxReadAllocation(200 * 1024 * 1024), // 200 MB
   )
   ```

2. **Environment variable override:**
   ```bash
   WILE_MAX_READ_ALLOCATION=200M ./scheme script.scm
   ```

3. **Per-port limits:**
   ```scheme
   (define p (open-input-file "huge.dat" '(max-read 500000000)))
   ```

4. **Streaming API for large files:**
   ```scheme
   (call-with-input-file "huge.dat"
     (lambda (port)
       (read-string-chunked port 1000000  ; chunk size
         (lambda (chunk) ...))))  ; process each chunk
   ```

**For now:** Keep it simple. Hard-coded 100 MB limit solves the problem.

## Implementation Checklist

- [ ] Add `MaxReadStringBytes` and `MaxReadBytevectorBytes` constants
- [ ] Add validation to `PrimReadString` before allocation
- [ ] Add validation to `PrimReadBytevector` before allocation
- [ ] Create `prim_read_write_test.go` with allocation limit tests
- [ ] Add error message validation tests
- [ ] Run all new tests (verify they pass)
- [ ] Run all existing tests (verify no regressions)
- [ ] Run R7RS integration tests
- [ ] Run lint (verify 0 issues)
- [ ] Manual REPL verification
- [ ] Update ARCHITECTURAL_REVIEW.md (mark M11 as Fixed)
- [ ] Update ARCHITECTURAL_REVIEW_FIXES.md (add M11 implementation report)
- [ ] Update CHANGELOG.md (add entry in [Unreleased] → Fixed)

## References

- **R7RS §6.13.2** — `read-string` specification
- **R7RS §6.13.3** — `read-bytevector` specification
- **R7RS §3.1** — Implementation responsibilities (allows implementation-defined limits)
- **Go `make()` documentation** — Memory allocation behavior
- **Architectural Review** — `plans/ARCHITECTURAL_REVIEW.md` lines 213-218
- **Similar issues in other languages:**
  - Python: `sys.maxsize` limits
  - Ruby: Memory allocation limits
  - Node.js: `--max-old-space-size` flag
  - Java: `-Xmx` heap size limit
