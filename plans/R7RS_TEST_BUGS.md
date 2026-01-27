# R7RS Test Suite Bugs

This document tracks **open** bugs discovered when running `r7rs-tests.scm` against Wile.

**Test Source:** `r7rs-tests.scm` (based on Chibi Scheme's R7RS test suite)

---

## Summary

| Priority | Category | Count |
|----------|----------|-------|
| Low | Minor issues | 3 |

---

## Open Bugs

### Bug #1: Parser error "unknown token type: ):"

**Priority:** Low

**Current behavior:**
```
unknown token type: ):
```

This error appears at the end of the r7rs-tests.scm run, after most tests complete.

**R7RS Reference:** §7.1.1 - `)` and `:` are distinct token types and should be parsed separately.

**Suspected cause:** Tokenizer state corruption or incorrect token boundary detection. The `)` and `:` characters are being combined into an invalid token.

**Investigation needed:**
- Check if this occurs at a specific location in the test file
- Examine tokenizer state machine transitions
- May be related to reading from a port that has partially consumed input

---

### Bug #2: Floating point precision differences

**Priority:** Low

**Current behavior:**
```scheme
;; Various tests show precision differences
;; Example: 9.728 vs 9.728000255822641
```

**Notes:** This may be acceptable per R7RS which allows implementation-defined precision. However, the differences are larger than expected for IEEE 754 double precision.

**Investigation needed:**
- Compare with other Scheme implementations
- Review float-to-string conversion

---

### Bug #3: Unicode `string-ci<?` failures

**Priority:** Low

**Current behavior:**
Various case-insensitive string comparisons fail for Unicode characters.

**R7RS Reference:** §6.7 - Case-insensitive string comparisons should use Unicode case folding.

**Investigation needed:**
- Review Unicode case folding implementation
- May require ICU or equivalent library for full conformance

---

## Test Suite Discrepancies (Not Wile Bugs)

These are tests in r7rs-tests.scm that have incorrect expected values.

### Discrepancy #1: `sqrt(-1.0-0.0i)` expected value

**Test (line 1017):**
```scheme
(test 0.0+1.0i (sqrt -1.0-0.0i))
```

**Wile returns:** `0-1i`
**Test expects:** `0+1i`

**Analysis:** Wile is **correct** per IEEE 754/C99. The Go `cmplx.Sqrt` documentation states: "imag(r) has the same sign as imag(x)". Since `-0.0i` has a negative sign, the result's imaginary part should be negative.

**R7RS §6.2.6:** "Implementations that use IEEE binary floating-point arithmetic should follow the relevant standards for these procedures."

The test suite has the wrong expected value for this edge case involving negative zero.

---

## Testing Commands

```bash
# Run full test suite
./dist/scheme --file r7rs-tests.scm

# Run with timeout to detect hangs (useful for debugging)
timeout 60 ./dist/scheme --file r7rs-tests.scm

# Run subset of tests (first N lines)
head -500 r7rs-tests.scm > /tmp/partial-tests.scm
./dist/scheme --file /tmp/partial-tests.scm
```

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)
- [R7RS Corrected HTML](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)
- Test suite: `r7rs-tests.scm` (Chibi Scheme R7RS tests)
