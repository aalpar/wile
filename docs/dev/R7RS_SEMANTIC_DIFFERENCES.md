# R7RS Semantic Differences

This document catalogs differences between the current implementation and the R7RS-small specification. These are semantic differences where the implementation produces results but may not match R7RS behavior for certain inputs.

**Reference:** [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)

**Last Updated:** 2026-01-23

---

## Summary

| Procedure | R7RS Requirement | Current Implementation | Status |
|-----------|------------------|------------------------|--------|
| `string-upcase` | Unicode full uppercasing | `strings.ToUpper()` | ⚠️ Difference |
| `string-downcase` | Unicode full lowercasing | `strings.ToLower()` | ⚠️ Difference |

### Fixed Issues (No Longer Different)

The following were previously listed as differences but have been fixed:

| Procedure | Status | Fix Location |
|-----------|--------|--------------|
| `char-foldcase` | ✅ Fixed | Uses `simpleCaseFold()` in `extensions/all/prim_all.go` |
| `string-foldcase` | ✅ Fixed | Uses `golang.org/x/text/cases.Fold()` |
| `char-ci=?` et al. | ✅ Fixed | Uses simple case folding |
| `string-ci=?` et al. | ✅ Fixed | Uses case folding |
| `digit-value` | ✅ Fixed | Handles all Unicode Nd digits |
| `read-error?` | ✅ Fixed | Implemented in `extensions/exceptions/prim_exceptions.go` |
| `file-error?` | ✅ Fixed | Implemented in `extensions/exceptions/prim_exceptions.go` |

---

## Remaining Differences

### 1. `string-upcase`

**File:** `go/extensions/all/prim_all.go`

**R7RS Specification (Section 6.7 "Strings"):**
> "These procedures apply the Unicode full uppercasing algorithm to their arguments and return the result."

**Current Implementation:**
```go
strings.ToUpper(str.Value)
```

**Difference:**
- Uses Go's `strings.ToUpper()` which performs simple uppercasing
- R7RS requires Unicode full uppercasing which can change string length

**Key Example - German Sharp S:**
```
Input:    "straße"  (6 characters)
R7RS:     "STRASSE" (7 characters) - ß → SS
Current:  "STRAßE"  (6 characters) - ß unchanged
```

**Impact:** Low - affects primarily German text with ß.

**Fix:** Use `golang.org/x/text/cases.Upper(language.Und)`.

---

### 2. `string-downcase`

**File:** `go/extensions/all/prim_all.go`

**R7RS Specification (Section 6.7 "Strings"):**
> "These procedures apply the Unicode full lowercasing algorithm to their arguments and return the result."

**Current Implementation:**
```go
strings.ToLower(str.Value)
```

**Difference:**
- Uses Go's `strings.ToLower()` which performs simple lowercasing
- R7RS requires Unicode full lowercasing

**Impact:** Lower than uppercasing since fewer characters expand during lowercasing.

**Fix:** Use `golang.org/x/text/cases.Lower(language.Und)`.

---

## Conformant Features

The following features are fully conformant with R7RS:

### Case Folding (R7RS §6.6, §6.7)

| Procedure | Implementation | Notes |
|-----------|----------------|-------|
| `char-foldcase` | `simpleCaseFold()` | Unicode simple case folding |
| `string-foldcase` | `cases.Fold()` | Unicode full case folding (ß → "ss") |
| `char-ci=?` et al. | Via `simpleCaseFold()` | Correct per R7RS |
| `string-ci=?` et al. | Via case folding | Correct per R7RS |

The implementation correctly distinguishes between:
- **Simple case folding** (char-foldcase): One-to-one character mapping
- **Full case folding** (string-foldcase): Can expand characters (ß → "ss")

### Unicode Digit Value (R7RS §6.6)

`digit-value` correctly handles all Unicode decimal digits (category Nd):

| Script | Range | Example | Status |
|--------|-------|---------|--------|
| ASCII | U+0030-0039 | 0-9 | ✅ |
| Arabic-Indic | U+0660-0669 | ٠١٢٣٤٥٦٧٨٩ | ✅ |
| Extended Arabic-Indic | U+06F0-06F9 | ۰۱۲۳۴۵۶۷۸۹ | ✅ |
| Devanagari | U+0966-096F | ०१२३४५६७८९ | ✅ |
| Bengali | U+09E6-09EF | ০১২৩৪৫৬৭৮৯ | ✅ |
| Thai | U+0E50-0E59 | ๐๑๒๓๔๕๖๗๘๙ | ✅ |
| Fullwidth | U+FF10-FF19 | ０１２３４５６７８９ | ✅ |

### Exceptions (R7RS §6.11)

| Procedure | Status | Notes |
|-----------|--------|-------|
| `with-exception-handler` | ✅ Conformant | Installs handler, propagates to parent on re-raise |
| `raise` | ✅ Conformant | Non-continuable; handler must not return |
| `raise-continuable` | ✅ Conformant | Handler return value becomes result |
| `error` | ✅ Conformant | Creates error object, raises non-continuable |
| `error-object?` | ✅ Conformant | Type predicate |
| `error-object-message` | ✅ Conformant | Extracts message string |
| `error-object-irritants` | ✅ Conformant | Extracts irritants list |
| `guard` | ✅ Conformant | Exception handling syntax with cond-like clauses |
| `read-error?` | ✅ Conformant | Predicate for read errors |
| `file-error?` | ✅ Conformant | Predicate for file errors |

### Promises (R7RS §4.2.5)

| Procedure/Syntax | Status | Notes |
|------------------|--------|-------|
| `delay` | ✅ Conformant | Creates promise with delayed expression |
| `force` | ✅ Conformant | Forces promise, memoizes result, iterative forcing |
| `delay-force` | ✅ Conformant | Lazy promise for tail-recursive algorithms |
| `make-promise` | ✅ Conformant | Wraps value in already-forced promise; returns promise unchanged |
| `promise?` | ✅ Conformant | Type predicate |

The implementation correctly handles:
- **Iterative forcing:** `(force (delay (delay (delay 42))))` returns 42
- **Memoization:** Thunk evaluated only once
- **`make-promise` identity:** Returns promise argument unchanged
- **`delay-force` tail recursion:** No stack growth for lazy algorithms

---

## Recommendations for Full R7RS Conformance

### Fix `string-upcase` and `string-downcase`

```go
import (
    "golang.org/x/text/cases"
    "golang.org/x/text/language"
)

// For uppercasing (string-upcase)
upper := cases.Upper(language.Und)
result := upper.String(input)

// For lowercasing (string-downcase)
lower := cases.Lower(language.Und)
result := lower.String(input)
```

This is the same approach used for `string-foldcase`, which already uses `cases.Fold()`.

### Trade-offs

| Approach | Pros | Cons |
|----------|------|------|
| Use `x/text` (recommended) | Full conformance, well-tested | Already a dependency |
| Current implementation | Simpler | Not fully R7RS compliant |

Since `golang.org/x/text/cases` is already imported for `string-foldcase`, adding full case mapping for `string-upcase` and `string-downcase` adds no new dependencies.

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf) - Sections 6.6, 6.7, 6.11
- [Unicode CaseFolding.txt](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
- [Unicode SpecialCasing.txt](https://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt)
- [Go x/text/cases package](https://pkg.go.dev/golang.org/x/text/cases)
