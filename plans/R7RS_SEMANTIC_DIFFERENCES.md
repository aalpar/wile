# R7RS Semantic Differences

This document catalogs differences between the current implementation and the R7RS-small specification for character and string procedures. These are semantic differences where the implementation produces results but may not match R7RS behavior for certain inputs.

**Reference:** [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)

---

## Summary

| Procedure | R7RS Requirement | Current Implementation | Impact |
|-----------|------------------|------------------------|--------|
| `char-foldcase` | Unicode SimpleCaseFolding | `unicode.ToLower()` | Low |
| `string-foldcase` | Unicode CaseFolding | `strings.ToLower()` | Low |
| `char-ci=?` et al. | Compare via SimpleCaseFolding | Compare via `unicode.ToLower()` | Low |
| `string-ci=?` | Compare via CaseFolding | `strings.EqualFold()` | Low |
| `string-ci<?` et al. | Compare via CaseFolding | Compare via `strings.ToLower()` | Low |
| `digit-value` | All Unicode decimal digits | ASCII 0-9 only | Medium |
| `string-upcase` | Unicode full uppercasing | `strings.ToUpper()` | Low |
| `string-downcase` | Unicode full lowercasing | `strings.ToLower()` | Low |

---

## Detailed Analysis

### 1. `char-foldcase`

**File:** `prim_char_foldcase.go`

**R7RS Specification (Section 6.6 "Characters"):**
> "The char-foldcase procedure applies the Unicode simple case-folding algorithm to its argument and returns the result. Note that language-sensitive folding is not used."

**Current Implementation:**
```go
unicode.ToLower(ch.Value)
```

**Difference:**
- Uses Go's `unicode.ToLower()` which performs Unicode lowercasing
- R7RS requires Unicode SimpleCaseFolding (defined in Unicode CaseFolding.txt with status 'C' or 'S')

**Affected Characters:**
Most characters are unaffected. Differences occur primarily with:
- Certain Greek characters (e.g., final sigma handling)
- Some archaic or specialized Unicode characters

**Reference:** [Unicode CaseFolding.txt](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)

---

### 2. `string-foldcase`

**File:** `prim_string_foldcase.go`

**R7RS Specification (Section 6.7 "Strings"):**
> "The string-foldcase procedure applies the Unicode full case-folding algorithm to its argument and returns the result."

**Current Implementation:**
```go
strings.ToLower(str.Value)
```

**Difference:**
- Uses Go's `strings.ToLower()` which performs simple lowercasing
- R7RS requires Unicode full case-folding which can change string length

**Key Example - German Sharp S (ß):**
```
Input:    "straße"  (6 characters)
R7RS:     "strasse" (7 characters) - ß folds to "ss"
Current:  "straße"  (6 characters) - ß unchanged by ToLower
```

**Other Affected Characters:**
- U+0130 İ (Latin Capital Letter I With Dot Above) → "i\u0307" in full folding
- Various ligatures and special characters

**Fix:** Use `golang.org/x/text/cases` package with `cases.Fold(language.Und)`

---

### 3. Case-Insensitive Character Comparisons

**Files:** `prim_char_ci_eq.go`, `prim_char_ci_lt.go`, `prim_char_ci_gt.go`, `prim_char_ci_le.go`, `prim_char_ci_ge.go`

**R7RS Specification (Section 6.6 "Characters"):**
> "These procedures are similar to char=? et cetera, but they treat upper case and lower case letters as the same. For example, (char-ci=? #\A #\a) returns #t."
>
> "Specifically, these procedures behave as if char-foldcase were applied to their arguments before comparing them."

**Current Implementation:**
```go
unicode.ToLower(a) == unicode.ToLower(b)
```

**Difference:**
Same as `char-foldcase` - uses lowercasing instead of SimpleCaseFolding.

**Impact:** Minimal for typical use cases. May differ for exotic Unicode characters.

---

### 4. Case-Insensitive String Comparisons

**Files:** `prim_string_ci_eq.go`, `prim_string_ci_lt.go`, `prim_string_ci_gt.go`, `prim_string_ci_le.go`, `prim_string_ci_ge.go`

**R7RS Specification (Section 6.7 "Strings"):**
> "These procedures are the case-insensitive versions of string=?, string<?, etc. They behave as if string-foldcase were applied to their arguments before comparing them."

**Current Implementation:**

For `string-ci=?`:
```go
strings.EqualFold(a, b)
```

For `string-ci<?`, `string-ci>?`, etc.:
```go
strings.ToLower(a) < strings.ToLower(b)  // (comparison varies)
```

**Difference:**
- `strings.EqualFold()` uses Unicode simple case-folding, which is close but not identical to R7RS full case-folding
- Ordering comparisons use `strings.ToLower()` which differs from case-folding

**Key Example:**
```scheme
; R7RS behavior (with full case-folding):
(string-ci=? "straße" "STRASSE")  ; Should return #t

; Current behavior (with simple folding/lowering):
(string-ci=? "straße" "STRASSE")  ; Returns #f (ß ≠ ss)
```

---

### 5. `digit-value`

**File:** `prim_digit_value.go`

**R7RS Specification (Section 6.6 "Characters"):**
> "This procedure returns the numeric value (0 to 9) of its argument if it is a numeric digit (that is, if char-numeric? returns #t), or #f on any other character."
>
> Note: `char-numeric?` is defined to return #t for Unicode category Nd (Decimal Number).

**Current Implementation:**
```go
if ch.Value >= '0' && ch.Value <= '9' {
    mc.SetValue(values.NewInteger(int64(ch.Value - '0')))
} else {
    mc.SetValue(values.FalseValue)
}
```

**Difference:**
- Only handles ASCII digits 0-9 (U+0030 to U+0039)
- R7RS requires handling ALL Unicode decimal digits (category Nd)

**Affected Unicode Digit Ranges:**

| Script | Range | Example | Digit Value |
|--------|-------|---------|-------------|
| ASCII | U+0030-0039 | 0-9 | ✅ Supported |
| Arabic-Indic | U+0660-0669 | ٠١٢٣٤٥٦٧٨٩ | ❌ Returns #f |
| Extended Arabic-Indic | U+06F0-06F9 | ۰۱۲۳۴۵۶۷۸۹ | ❌ Returns #f |
| Devanagari | U+0966-096F | ०१२३४५६७८९ | ❌ Returns #f |
| Bengali | U+09E6-09EF | ০১২৩৪৫৬৭৮৯ | ❌ Returns #f |
| Thai | U+0E50-0E59 | ๐๑๒๓๔๕๖๗๘๙ | ❌ Returns #f |
| Fullwidth | U+FF10-FF19 | ０１２３４５６７８９ | ❌ Returns #f |

**Fix:**
```go
import "unicode"

if unicode.IsDigit(ch.Value) {
    // Calculate digit value from Unicode properties
    // Each decimal digit block starts at value 0
    // Use unicode.Nd category to find the base
}
```

**Impact:** Medium - affects internationalized applications using non-ASCII numerals.

---

### 6. `string-upcase`

**File:** `prim_string_upcase.go`

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
Current:  "STRAßE"  (6 characters) - ß unchanged (Go 1.x behavior)
```

Note: As of Go 1.x, `strings.ToUpper()` does not expand ß to SS. This may change in future Go versions.

---

### 7. `string-downcase`

**File:** `prim_string_downcase.go`

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

---

## Recommendations for Full R7RS Conformance

### Option 1: Use `golang.org/x/text/cases` Package

```go
import (
    "golang.org/x/text/cases"
    "golang.org/x/text/language"
)

// For case-folding (char-foldcase, string-foldcase, case-insensitive comparisons)
folder := cases.Fold()
folded := folder.String(input)

// For uppercasing (string-upcase)
upper := cases.Upper(language.Und)
result := upper.String(input)

// For lowercasing (string-downcase)
lower := cases.Lower(language.Und)
result := lower.String(input)
```

### Option 2: Implement Unicode Algorithms Directly

For `digit-value`, use Unicode properties:
```go
import "unicode"

func digitValue(r rune) (int, bool) {
    if !unicode.Is(unicode.Nd, r) {
        return 0, false
    }
    // Each Nd block starts with digit 0
    // Find the block base and calculate offset
    // ...
}
```

### Trade-offs

| Approach | Pros | Cons |
|----------|------|------|
| Add `x/text` dependency | Full conformance, well-tested | Additional dependency |
| Current implementation | No dependencies, simpler | Not fully R7RS compliant |
| Custom implementation | No dependencies, full control | More code to maintain |

---

## Testing R7RS Conformance

Add tests for edge cases:

```scheme
; Case folding tests
(char-foldcase #\ß)           ; Should return #\ß (simple folding)
(string-foldcase "ß")         ; Should return "ss" (full folding)
(string-ci=? "STRASSE" "straße") ; Should return #t

; Digit value tests
(digit-value #\٥)             ; Arabic-Indic 5, should return 5
(digit-value #\५)             ; Devanagari 5, should return 5
(digit-value #\๕)             ; Thai 5, should return 5

; Uppercasing tests
(string-upcase "straße")      ; Should return "STRASSE"
```

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf) - Sections 6.6, 6.7
- [Unicode CaseFolding.txt](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
- [Unicode SpecialCasing.txt](https://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt)
- [UAX #29: Unicode Text Segmentation](https://unicode.org/reports/tr29/)
- [Go x/text/cases package](https://pkg.go.dev/golang.org/x/text/cases)
