# Primitive Unit Tests Implementation Plan

This document outlines the plan for implementing comprehensive unit tests for all 219 primitives missing dedicated test files.

## Executive Summary

- **Total primitives needing tests:** 219
- **Estimated test files to create:** 180+ (some primitives can be grouped)
- **Estimated lines of test code:** 25,000-35,000
- **Recommended approach:** Phased implementation by category, prioritizing core primitives

---

## Type Coverage Requirements

Every numeric primitive must be tested with ALL applicable types from the Scheme numeric tower:

| Type | Go Type | Constructor | Example Scheme Literal | Notes |
|------|---------|-------------|------------------------|-------|
| Integer | `*values.Integer` | `values.NewInteger(42)` | `42` | Exact |
| BigInteger | `*values.BigInteger` | `values.NewBigIntegerFromString("123456789012345678901234567890", 10)` | `#z123456789012345678901234567890` | Exact, arbitrary precision |
| Float | `*values.Float` | `values.NewFloat(3.14)` | `3.14` | Inexact, IEEE 754 |
| BigFloat | `*values.BigFloat` | `values.NewBigFloatFromString("3.14159265358979323846")` | `#m3.14159265358979323846` | Inexact, arbitrary precision, no Inf/NaN |
| Rational | `*values.Rational` | `values.NewRational(3, 4)` | `3/4` | Exact |
| Complex | `*values.Complex` | `values.NewComplexFromParts(1.0, 2.0)` | `1+2i` | Inexact |

### Type Promotion Rules to Test

```
Integer + Integer → Integer (or BigInteger on overflow)
Integer + Float → Float
Integer + BigFloat → BigFloat
Integer + Rational → Rational
Integer + Complex → Complex
Float + Rational → Float
Float + BigFloat → BigFloat
BigFloat + Rational → BigFloat
Any + Complex → Complex
```

### Exactness Contagion in min/max

When using `min` or `max`, if any argument is inexact (Float, BigFloat, Complex), the result is inexact:
```
(max 3 #m2.5) → 3.0 (Float, because BigFloat is inexact)
(min 2 #m3.5) → 2.0 (Float, because BigFloat is inexact)
```

### Special Values to Test

- `+inf.0`, `-inf.0` (positive/negative infinity)
- `+nan.0` (not a number)
- `0`, `-0` (signed zeros for floats)
- Very large integers (test BigInteger promotion)
- Very small rationals (precision)

---

## Test Pattern Templates

### Template 1: Basic Scheme Code Tests

```go
// prim_xxx_test.go
package primitives_test

import (
    "testing"
    "wile/values"
    qt "github.com/frankban/quicktest"
)

func TestXxx(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        // Happy path - basic functionality
        {"basic case", `(xxx 1 2)`, values.NewInteger(3)},

        // Type coverage - all numeric types
        {"integer args", `(xxx 10 20)`, values.NewInteger(30)},
        {"float args", `(xxx 1.5 2.5)`, values.NewFloat(4.0)},
        {"bigfloat args", `(xxx #m1.5 #m2.5)`, values.NewBigFloatFromString("4.0")},
        {"rational args", `(xxx 1/2 1/4)`, values.NewRational(3, 4)},
        {"complex args", `(xxx 1+2i 3+4i)`, values.NewComplexFromParts(4.0, 6.0)},
        {"big integer args", `(xxx #z1000000000000000000 #z1)`, /* expected */},

        // Mixed types
        {"integer + float", `(xxx 1 2.0)`, values.NewFloat(3.0)},
        {"integer + bigfloat", `(xxx 1 #m2.0)`, values.NewBigFloatFromString("3.0")},
        {"integer + rational", `(xxx 1 1/2)`, values.NewRational(3, 2)},

        // Edge cases
        {"zero args", `(xxx)`, /* identity element if applicable */},
        {"single arg", `(xxx 5)`, values.NewInteger(5)},
        {"empty list input", `(xxx '())`, /* expected */},

        // Special values
        {"positive infinity", `(xxx +inf.0 1)`, /* expected */},
        {"negative infinity", `(xxx -inf.0 1)`, /* expected */},
        {"nan propagation", `(xxx +nan.0 1)`, /* expected */},

        // Boundary conditions
        {"max int", `(xxx 9223372036854775807 0)`, /* expected */},
        {"min int", `(xxx -9223372036854775808 0)`, /* expected */},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, values.SchemeEquals, tc.out)
        })
    }
}
```

### Template 2: Error Condition Tests

```go
func TestXxx_Errors(t *testing.T) {
    tcs := []struct {
        name string
        code string
    }{
        {"wrong type - string", `(xxx "hello")`},
        {"wrong type - symbol", `(xxx 'foo)`},
        {"wrong arity - too few", `(xxx)`},
        {"wrong arity - too many", `(xxx 1 2 3 4 5)`},
        {"out of bounds", `(xxx -1)`},
        {"division by zero", `(xxx 1 0)`},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            _, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNotNil)
        })
    }
}
```

### Template 3: Predicate Tests

```go
func TestXxxPredicate(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        // True cases
        {"true for valid type", `(xxx? 42)`, values.TrueValue},

        // False cases
        {"false for wrong type", `(xxx? "hello")`, values.FalseValue},
        {"false for empty list", `(xxx? '())`, values.FalseValue},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, values.SchemeEquals, tc.out)
        })
    }
}
```

### Template 4: Comparison Chain Tests

```go
func TestNumericComparison(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        // Two arguments
        {"2 < 3", `(< 2 3)`, values.TrueValue},
        {"3 < 2", `(< 3 2)`, values.FalseValue},

        // Chain (transitive)
        {"1 < 2 < 3", `(< 1 2 3)`, values.TrueValue},
        {"1 < 3 < 2", `(< 1 3 2)`, values.FalseValue},

        // Mixed types in chain
        {"1 < 1.5 < 2", `(< 1 1.5 2)`, values.TrueValue},
        {"1 < 3/2 < 2", `(< 1 3/2 2)`, values.TrueValue},

        // Edge: single argument (always true)
        {"single arg", `(< 5)`, values.TrueValue},

        // NaN behavior (always false)
        {"nan comparison", `(< 1 +nan.0)`, values.FalseValue},
    }
    // ...
}
```

---

## Implementation Phases

### Phase 1: Core Arithmetic (Priority: Critical)
**Estimated time: 2-3 days**
**Files: 17**

These are the most fundamental primitives used everywhere.

| Primitive | File | Test Cases | Notes |
|-----------|------|------------|-------|
| `+` | `prim_add_test.go` | 25+ | Zero args returns 0, all type combinations |
| `-` | `prim_sub_test.go` | 20+ | Unary negation, binary, variadic |
| `*` | `prim_mul_test.go` | 25+ | Zero args returns 1, overflow to BigInteger |
| `/` | `prim_div_test.go` | 20+ | Unary reciprocal, division by zero |
| `quotient` | `prim_quotient_test.go` | 15+ | Integer division, sign handling |
| `remainder` | `prim_remainder_test.go` | 15+ | Sign follows dividend |
| `modulo` | `prim_modulo_test.go` | 15+ | Sign follows divisor |
| `gcd` | `prim_gcd_test.go` | 15+ | Zero handling, negative numbers |
| `lcm` | `prim_lcm_test.go` | 15+ | Zero in args returns 0 |
| `expt` | `prim_expt_test.go` | 20+ | Integer/fractional powers, negative bases |
| `sqrt` | `prim_sqrt_test.go` | 15+ | Perfect squares, negative→complex |
| `square` | `prim_square_test.go` | 10+ | All numeric types |
| `abs` | (may exist) | 10+ | All numeric types |
| `max` | `prim_max_test.go` | 18+ | All 6 types, mixed types, NaN handling, exactness contagion with BigFloat |
| `min` | `prim_min_test.go` | 18+ | All 6 types, mixed types, NaN handling, exactness contagion with BigFloat |
| `exact-integer-sqrt` | `prim_exact_integer_sqrt_test.go` | 10+ | Returns two values |
| `rationalize` | `prim_rationalize_test.go` | 10+ | Tolerance parameter |

**Verification:** After Phase 1, run `make test` and verify all arithmetic operations work correctly with mixed types.

---

### Phase 2: Numeric Predicates & Comparisons (Priority: Critical)
**Estimated time: 2 days**
**Files: 20**

| Primitive | File | Test Cases | Notes |
|-----------|------|------------|-------|
| `zero?` | `prim_zero_q_test.go` | 12+ | All 6 numeric types, 0.0 vs -0.0 |
| `positive?` | `prim_positive_q_test.go` | 12+ | All 6 types, edge: +0.0 |
| `negative?` | `prim_negative_q_test.go` | 12+ | All 6 types, edge: -0.0 |
| `odd?` | `prim_odd_q_test.go` | 12+ | Integer, BigInteger, integral Float/BigFloat |
| `even?` | `prim_even_q_test.go` | 12+ | Integer, BigInteger, integral Float/BigFloat |
| `exact?` | `prim_exact_q_test.go` | 12+ | Integer/BigInteger/Rational=true, Float/BigFloat/Complex=false |
| `inexact?` | `prim_inexact_q_test.go` | 12+ | Float/BigFloat/Complex=true |
| `exact-integer?` | `prim_exact_integer_q_test.go` | 12+ | Integer/BigInteger only |
| `finite?` | `prim_finite_q_test.go` | 12+ | All exact types + BigFloat=true, Float inf/nan=false |
| `infinite?` | `prim_infinite_q_test.go` | 10+ | +inf.0/-inf.0=true, BigFloat=false (no inf support) |
| `nan?` | `prim_nan_q_test.go` | 10+ | +nan.0=true, BigFloat=false (no nan support) |
| `integer?` | `prim_integer_q_test.go` | 14+ | 3.0 is integer, #m4.0 is integer |
| `rational?` | `prim_rational_q_test.go` | 12+ | All reals except inf/nan, BigFloat=true |
| `real?` | `prim_real_q_test.go` | 12+ | All except Complex with imag≠0 |
| `=` | `prim_num_eq_test.go` | 22+ | NaN≠NaN, chain, mixed types including BigFloat |
| `<` | `prim_num_lt_test.go` | 18+ | Chain, mixed types including BigFloat |
| `>` | `prim_num_gt_test.go` | 18+ | Chain, mixed types including BigFloat |
| `<=` | `prim_num_le_test.go` | 18+ | Chain, NaN handling |
| `>=` | `prim_num_ge_test.go` | 18+ | Chain, NaN handling |

---

### Phase 3: List Operations (Priority: High)
**Estimated time: 2 days**
**Files: 22**

| Primitive | File | Test Cases | Notes |
|-----------|------|------------|-------|
| `car` | `prim_car_test.go` | 10+ | Error on non-pair |
| `cdr` | `prim_cdr_test.go` | 10+ | Improper lists |
| `cons` | `prim_cons_test.go` | 10+ | Building lists, pairs |
| `set-car!` | `prim_set_car_test.go` | 8+ | Mutation |
| `set-cdr!` | `prim_set_cdr_test.go` | 8+ | |
| `null?` | `prim_null_q_test.go` | 8+ | Only '() is true |
| `pair?` | `prim_pair_q_test.go` | 10+ | '() is false |
| `list?` | `prim_list_q_test.go` | 12+ | Improper=false, circular=false |
| `length` | `prim_length_test.go` | 10+ | Error on improper |
| `append` | `prim_append_test.go` | 15+ | Zero args, improper final |
| `reverse` | `prim_reverse_test.go` | 10+ | |
| `list-ref` | `prim_list_ref_test.go` | 12+ | Bounds checking |
| `list-set!` | `prim_list_set_test.go` | 10+ | |
| `list-tail` | `prim_list_tail_test.go` | 10+ | k=0, k=length |
| `make-list` | `prim_make_list_test.go` | 10+ | With/without fill |
| `memq` | `prim_memq_test.go` | 12+ | Uses eq? |
| `memv` | `prim_memv_test.go` | 12+ | Uses eqv? |
| `member` | `prim_member_test.go` | 12+ | Uses equal? |
| `assq` | `prim_assq_test.go` | 12+ | |
| `assv` | `prim_assv_test.go` | 12+ | |
| `assoc` | `prim_assoc_test.go` | 12+ | |

---

### Phase 4: String & Character Operations (Priority: High)
**Estimated time: 2-3 days**
**Files: 32**

#### R7RS Conformance TODO List

The following items must be completed to achieve R7RS conformance for string and character operations.

**Legend:** ✅ = tests added, implementation done | 🧪 = tests added, implementation pending | ⬜ = no tests yet

**Non-Variadic Procedures That Must Be Made Variadic (R7RS requires 2+ arguments):**
- ✅ `char-ci=?` - variadic implementation complete (`prim_char_ci_variadic.go`)
- ✅ `char-ci<?` - variadic implementation complete (`prim_char_ci_variadic.go`)
- ✅ `char-ci>?` - variadic implementation complete (`prim_char_ci_variadic.go`)
- ✅ `char-ci<=?` - variadic implementation complete (`prim_char_ci_variadic.go`)
- ✅ `char-ci>=?` - variadic implementation complete (`prim_char_ci_variadic.go`)
- ✅ `string-ci=?` - variadic implementation complete (`prim_string_ci_variadic.go`)
- ✅ `string-ci<?` - variadic implementation complete (`prim_string_ci_variadic.go`)
- ✅ `string-ci>?` - variadic implementation complete (`prim_string_ci_variadic.go`)
- ✅ `string-ci<=?` - variadic implementation complete (`prim_string_ci_variadic.go`)
- ✅ `string-ci>=?` - variadic implementation complete (`prim_string_ci_variadic.go`)

**Missing R7RS Base Procedures:**
- ✅ `string-set!` - `(string-set! string k char)` - mutate character at position
- ✅ `string-fill!` - `(string-fill! string fill [start [end]])` - fill region with character
- ✅ `string-copy!` - `(string-copy! to at from [start [end]])` - copy between strings
- ✅ `string-map` - `(string-map proc string1 string2 ...)` - map over strings
- ✅ `string-for-each` - `(string-for-each proc string1 string2 ...)` - iterate over strings

**Missing Optional Arguments:**
- ✅ `string-copy` - add optional `start` and `end` arguments: `(string-copy string [start [end]])`
- ✅ `string->list` - add optional `start` and `end` arguments: `(string->list string [start [end]])`

**Semantic Fixes:**
- ⬜ `char-foldcase` - use Unicode SimpleCaseFolding algorithm, not just `unicode.ToLower()`
- ⬜ `string-foldcase` - use Unicode case-folding algorithm, not just `strings.ToLower()`
- ⬜ `digit-value` - handle all Unicode decimal digits (Arabic-Indic, Devanagari, etc.), not just ASCII 0-9

**Additional Tests Added:**
- ✅ Unicode string operations (`TestStringUnicode`) - Chinese, Greek, emoji, accented chars
- ✅ Unicode character operations (`TestCharUnicode`) - Greek letters, char-upcase/downcase
- ✅ String error conditions (`TestStringErrors`) - 45 error cases including:
  - string-length, string-ref, substring, make-string, string-copy errors
  - string constructor and string->list/list->string errors
  - string-upcase, string-downcase, string-foldcase errors
  - All string comparison operators (string=?, string<?, string>?, string<=?, string>=?)
  - All string-ci comparison operators (string-ci=?, string-ci<?, string-ci>?, string-ci<=?, string-ci>=?)
  - string->symbol, symbol->string, number->string errors
- ✅ Character comparison error tests (`TestCharCompareErrors`) - 12 error cases for char=?, char<?, char>?, char<=?, char>=?
- ✅ Character CI comparison error tests (`TestCharCICompareErrors`) - 10 error cases for char-ci comparisons
- ✅ Character predicate error tests (`TestCharPredicateErrors`) - 10 error cases for char-alphabetic?, char-numeric?, etc.
- ✅ Character conversion error tests (`TestCharConversionErrors`) - 14 error cases for char->integer, integer->char, char-upcase, etc.
- ✅ String comparison tests (`TestStringEqualScheme`, `TestStringLessThanScheme`, etc.) - 50+ tests for string=?, string<?, string>?, string<=?, string>=?
- ✅ Variadic char-ci comparison tests (`TestCharCICompareVariadic`) - tests for 3+ argument comparisons
- ✅ Variadic string-ci comparison tests (`TestStringCICompareVariadic`) - tests for 3+ argument comparisons
- ✅ String mutation tests (`TestStringSet`, `TestStringSetErrors`) - 9 tests for string-set!
- ✅ String fill tests (`TestStringFill`, `TestStringFillErrors`) - 10 tests for string-fill!
- ✅ String copy-to tests (`TestStringCopyTo`, `TestStringCopyToErrors`) - 10 tests for string-copy!
- ✅ String map tests (`TestStringMap`, `TestStringMapErrors`) - 7 tests for string-map
- ✅ String for-each tests (`TestStringForEach`, `TestStringForEachErrors`) - 5 tests for string-for-each
- ✅ String copy with optional args tests (`TestStringCopy`, `TestStringCopyErrors`) - 14 tests for string-copy with start/end
- ✅ String->list with optional args tests (`TestStringToListOptional`, `TestStringToListErrors`) - 14 tests for string->list with start/end

#### String Operations (20 files)
| Primitive | Test Cases | Notes |
|-----------|------------|-------|
| `string-length` | 10+ | Unicode characters |
| `string-ref` | 12+ | Bounds checking |
| `substring` | 15+ | Various index combinations |
| `string-append` | 12+ | Zero args, many args |
| `string->list` | 12+ | With indices |
| `list->string` | 10+ | |
| `string-upcase` | 8+ | Unicode |
| `string-downcase` | 8+ | |
| `string->symbol` | 8+ | |
| `symbol->string` | 8+ | |
| `string=?` | 10+ | Empty strings |
| `string<?` | 10+ | Lexicographic |
| `string>?` | 10+ | |
| `string<=?` | 10+ | |
| `string>=?` | 10+ | |
| `string-ci=?` | 10+ | Case-insensitive |
| `string-ci<?` | 10+ | |
| `string-ci>?` | 10+ | |
| `string-ci<=?` | 10+ | |
| `string-ci>=?` | 10+ | |

#### Character Operations (12 files)
| Primitive | Test Cases | Notes |
|-----------|------------|-------|
| `char->integer` | 8+ | ASCII, Unicode |
| `integer->char` | 10+ | Valid/invalid code points |
| `char-upcase` | 8+ | |
| `char-downcase` | 8+ | |
| `char-foldcase` | 8+ | |
| `digit-value` | 10+ | 0-9 return value, others #f |
| `char=?` | 8+ | Chain |
| `char<?` | 8+ | |
| `char>?` | 8+ | |
| `char<=?` | 8+ | |
| `char>=?` | 8+ | |

---

### Phase 5: Numeric Conversion & Complex Numbers (Priority: Medium) ✅ COMPLETE
**Estimated time: 1-2 days**
**Files: 12**
**Status: Complete - 155+ test cases across existing and new test files**

Tests are distributed across:
- `prim_phase5_test.go` - 155 comprehensive tests for all Phase 5 primitives
- `prim_numeric_conversion_test.go` - existing tests for numerator, denominator, number->string, string->number
- `prim_numeric_extra_test.go` - existing tests for exact, inexact, exact-integer-sqrt, rationalize
- `prim_complex_test.go` - existing tests for real-part, imag-part, magnitude, make-rectangular
- `prim_complex_extra_test.go` - existing tests for angle, make-polar, round-trip tests

| Primitive | File | Test Cases |
|-----------|------|------------|
| `exact` | `prim_phase5_test.go` | 16 ✅ |
| `inexact` | `prim_phase5_test.go` | 11 ✅ |
| `numerator` | `prim_phase5_test.go` | 12 ✅ |
| `denominator` | `prim_phase5_test.go` | 12 ✅ |
| `number->string` | `prim_phase5_test.go` | 18 ✅ |
| `string->number` | `prim_phase5_test.go` | 14 ✅ |
| `make-rectangular` | `prim_phase5_test.go` | 10 ✅ |
| `make-polar` | `prim_complex_extra_test.go` | 11 ✅ |
| `real-part` | `prim_phase5_test.go` | 9 ✅ |
| `imag-part` | `prim_phase5_test.go` | 9 ✅ |
| `magnitude` | `prim_phase5_test.go` | 14 ✅ |
| `angle` | `prim_complex_extra_test.go` | 14 ✅ |
| `exact?` | `prim_phase5_test.go` | 8 ✅ |
| `inexact?` | `prim_phase5_test.go` | 7 ✅ |
| `exact-integer?` | `prim_phase5_test.go` | 8 ✅ |

---

### Phase 6: Transcendental Functions (Priority: Medium) ✅ COMPLETE
**Estimated time: 1-2 days**
**Files: 1 (consolidated in prim_trig_test.go)**
**Status: Complete - 150+ test cases covering all transcendental functions**

Tests are consolidated in `prim_trig_test.go` following the project's thematic consolidation pattern:

| Primitive | Test Function | Test Cases |
|-----------|---------------|------------|
| `exp` | `TestExp`, `TestExpExtended`, `TestExpErrors` | 15+ ✅ |
| `log` | `TestLog`, `TestLogExtended`, `TestLogErrors` | 20+ ✅ |
| `sin` | `TestSin`, `TestSinExtended`, `TestSinErrors` | 15+ ✅ |
| `cos` | `TestCos`, `TestCosExtended`, `TestCosErrors` | 15+ ✅ |
| `tan` | `TestTan`, `TestTanExtended`, `TestTanErrors` | 12+ ✅ |
| `asin` | `TestAsin`, `TestAsinExtended`, `TestAsinErrors` | 15+ ✅ |
| `acos` | `TestAcos`, `TestAcosExtended`, `TestAcosErrors` | 15+ ✅ |
| `atan` | `TestAtan`, `TestAtanExtended`, `TestAtanErrors` | 20+ ✅ |
| Identities | `TestTranscendentalIdentities` | 7 identity tests ✅ |

**Coverage includes:**
- All supported numeric types (Integer, Float, Rational)
- Special values (+inf.0, -inf.0, +nan.0)
- Error conditions (wrong types: string, symbol, list, boolean)
- Mathematical identities (sin²+cos²=1, tan=sin/cos, exp(log(x))=x, etc.)
- Domain boundaries (asin/acos outside [-1,1])
- Two-arg atan (atan2) with all quadrants

---

### Phase 7: Division Operations (Priority: Medium) ✅ COMPLETE
**Estimated time: 1 day**
**Files: 1 (consolidated in prim_division_test.go)**
**Status: Complete - 150+ test cases covering all division operations**

Tests are consolidated in `prim_division_test.go` following the project's thematic consolidation pattern:

| Primitive | Test Function | Test Cases |
|-----------|---------------|------------|
| `floor/` | `TestFloorDivComprehensive`, `TestFloorDivIdentity` | 23+ ✅ |
| `floor-quotient` | `TestFloorQuotientComprehensive` | 17+ ✅ |
| `floor-remainder` | `TestFloorRemainderComprehensive`, `TestFloorDivRemainderSign` | 20+ ✅ |
| `truncate/` | `TestTruncateDivComprehensive`, `TestTruncateDivIdentity` | 23+ ✅ |
| `truncate-quotient` | `TestTruncateQuotientComprehensive` | 17+ ✅ |
| `truncate-remainder` | `TestTruncateRemainderComprehensive`, `TestTruncateDivRemainderSign` | 20+ ✅ |
| Comparison | `TestFloorVsTruncateDifference` | 5 ✅ |
| Errors | `TestDivisionErrors` | 36+ ✅ |
| Compatibility | `TestModuloEqualsFloorRemainder`, `TestRemainderEqualsTruncateRemainder`, `TestQuotientEqualsTruncateQuotient` | 15 ✅ |

**Coverage includes:**
- All sign combinations (++, +-, -+, --)
- Mathematical identities (n = d * q + r)
- Remainder sign conventions (floor: follows divisor, truncate: follows dividend)
- Edge cases (zero dividend, divide by 1/-1, large numbers)
- Error conditions (division by zero, wrong types)
- Compatibility tests (modulo = floor-remainder, remainder = truncate-remainder, quotient = truncate-quotient)

---

### Phase 8: Equality & Control Flow (Priority: Medium)
**Estimated time: 2 days**
**Files: 11**

| Primitive | File | Key Test Cases |
|-----------|------|----------------|
| `eq?` | `prim_eq_q_test.go` | Symbols same, small ints cached |
| `eqv?` | `prim_eqv_q_test.go` | Numbers, chars, booleans |
| `equal?` | `prim_equal_q_test.go` | Deep comparison |
| `apply` | `prim_apply_test.go` | With list, multiple args + list |
| `map` | `prim_map_test.go` | Single list, multiple lists |
| `for-each` | `prim_for_each_test.go` | Side effects, return void |
| `call-with-values` | `prim_call_with_values_test.go` | Producer/consumer |
| `values` | `prim_values_test.go` | Zero, one, multiple |
| `dynamic-wind` | `prim_dynamic_wind_test.go` | Before/after with continuations |
| `not` | `prim_not_test.go` | #f→#t, everything else→#f |

---

### Phase 9: I/O Operations (Priority: Medium-Low)
**Estimated time: 3-4 days**
**Files: 34**

These require more setup (temp files, string ports) but are less frequently modified.

#### Port Creation & Management
- `open-input-file`, `open-output-file`
- `open-binary-input-file`, `open-binary-output-file`
- `open-input-string`, `open-output-string`
- `open-input-bytevector`, `open-output-bytevector`
- `close-port`
- Port predicates: `input-port?`, `output-port?`, `port?`, `*-port-open?`
- Current ports: `current-input-port`, `current-output-port`

#### Higher-order Port Functions
- `call-with-input-file`, `call-with-output-file`
- `with-input-from-file`, `with-output-to-file`
- `get-output-string`, `get-output-bytevector`

#### Read/Write
- `read`, `read-syntax`, `read-token`
- `write`, `write-simple`, `write-shared`
- `display`, `write-char`, `newline`
- `eof-object`, `eof-object?`

**Test Strategy for I/O:**
```go
func TestOpenInputString(t *testing.T) {
    // Use string ports to avoid file system
    code := `(let ((p (open-input-string "hello")))
               (read-char p))`
    result, err := runSchemeCode(t, code)
    qt.Assert(t, err, qt.IsNil)
    qt.Assert(t, result, values.SchemeEquals, values.NewCharacter('h'))
}
```

---

### Phase 10: Exception Handling & Promises (Priority: Medium-Low)
**Estimated time: 1-2 days**
**Files: 11**

| Category | Primitives |
|----------|------------|
| Exceptions | `with-exception-handler`, `raise`, `raise-continuable`, `error`, `error-object?`, `error-object-message`, `error-object-irritants` |
| Promises | `make-promise`, `make-lazy-promise`, `force` (memoization) |
| Parameters | `make-parameter` (with/without converter) |

---

### Phase 11: Environment, Eval & Syntax (Priority: Lower)
**Estimated time: 2-3 days**
**Files: 17**

| Category | Primitives |
|----------|------------|
| Eval | `eval`, `environment`, `interaction-environment`, `scheme-report-environment`, `null-environment`, `load` |
| Syntax | `datum->syntax`, `syntax->datum`, `identifier?`, `bound-identifier=?`, `free-identifier=?`, `syntax-local-value`, `syntax-local-introduce`, `syntax-local-identifier-as-binding`, `make-compile-time-value` |
| Expansion | `expand`, `expand-once`, `compile` |

---

### Phase 12: System & Concurrency (Priority: Lower)
**Estimated time: 2 days**
**Files: 15**

| Category | Primitives |
|----------|------------|
| File System | `file-exists?`, `delete-file` |
| Process | `command-line`, `exit`, `emergency-exit`, `get-environment-variable`, `get-environment-variables`, `features` |
| Time | `current-second`, `current-jiffy`, `jiffies-per-second` |
| Threading | `thread`, `mutex`, `condvar` (SRFI-18) |
| Go Concurrency | `channel`, `sync` (WaitGroup, RWMutex, Once, Atomic) |

---

## Verification Strategy

### After Each Phase

1. **Run all tests:**
   ```bash
   cd go && make test
   ```

2. **Check coverage:**
   ```bash
   cd go && go test -cover ./runtime/primitives/...
   ```

3. **Run specific category:**
   ```bash
   cd go && go test -v -run "TestAdd|TestSub|TestMul" ./runtime/primitives/...
   ```

### Coverage Goals

| Phase | Target Coverage |
|-------|-----------------|
| After Phase 1-3 | 60% |
| After Phase 4-6 | 75% |
| After Phase 7-9 | 85% |
| After Phase 10-12 | 95%+ |

---

## Helper Functions to Add

Before starting implementation, add these helpers to `test_helpers_test.go`:

```go
// runSchemeCodeExpectError runs code and expects an error
func runSchemeCodeExpectError(t *testing.T, code string) error {
    t.Helper()
    _, err := runSchemeCode(t, code)
    if err == nil {
        t.Errorf("expected error but got none for: %s", code)
    }
    return err
}

// runSchemeCodeExpectTrue is a shorthand for boolean true result
func runSchemeCodeExpectTrue(t *testing.T, code string) {
    t.Helper()
    result, err := runSchemeCode(t, code)
    qt.Assert(t, err, qt.IsNil)
    qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

// runSchemeCodeExpectFalse is a shorthand for boolean false result
func runSchemeCodeExpectFalse(t *testing.T, code string) {
    t.Helper()
    result, err := runSchemeCode(t, code)
    qt.Assert(t, err, qt.IsNil)
    qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

// floatEquals checks if two floats are approximately equal
func floatEquals(expected float64) qt.Checker {
    return qt.CmpEquals(cmp.Comparer(func(a, b values.Value) bool {
        af, aok := a.(*values.Float)
        bf, bok := b.(*values.Float)
        if !aok || !bok {
            return false
        }
        return math.Abs(af.Datum()-bf.Datum()) < 1e-10
    }))
}
```

---

## File Naming Convention

Each test file should follow this pattern:
```
prim_{scheme_name_with_underscores}_test.go
```

Examples:
- `+` → `prim_add_test.go`
- `string->number` → `prim_string_to_number_test.go`
- `call/cc` → `prim_call_cc_test.go`
- `char=?` → `prim_char_eq_test.go`
- `list?` → `prim_list_q_test.go`

---

## Minimum Test Cases Per Primitive

| Primitive Type | Minimum Tests |
|----------------|---------------|
| Numeric binary op | 25 (6 types × 2 args + mixed + edge) |
| Numeric unary op | 12 (6 types + edge) |
| Predicate | 12 (true cases + false cases for all types) |
| Comparison chain | 18 (2-arg + chain + mixed types + edge) |
| List operation | 12 (empty + single + multi + error) |
| String operation | 12 (empty + ASCII + Unicode + error) |
| I/O operation | 8 (basic + error + cleanup) |

**Note:** The 6 numeric types are: Integer, BigInteger, Float, BigFloat, Rational, Complex

---

## Estimated Total Effort

| Phase | Files | Est. Lines | Est. Time |
|-------|-------|------------|-----------|
| 1. Core Arithmetic | 17 | 3,000 | 2-3 days |
| 2. Predicates & Comparisons | 20 | 2,500 | 2 days |
| 3. List Operations | 22 | 2,800 | 2 days |
| 4. String & Character | 32 | 3,500 | 2-3 days |
| 5. Numeric Conversion & Complex | 12 | 1,500 | 1-2 days |
| 6. Transcendental | 8 | 1,000 | 1-2 days |
| 7. Division | 6 | 800 | 1 day |
| 8. Equality & Control | 11 | 1,500 | 2 days |
| 9. I/O | 34 | 4,000 | 3-4 days |
| 10. Exceptions & Promises | 11 | 1,200 | 1-2 days |
| 11. Eval & Syntax | 17 | 2,000 | 2-3 days |
| 12. System & Concurrency | 15 | 1,500 | 2 days |
| **TOTAL** | **~180** | **~25,000** | **~22-30 days** |

---

## Priority Order for Implementation

If time is limited, implement in this order:

1. **Critical (must have):** Phases 1-3 (arithmetic, predicates, lists)
2. **High (should have):** Phases 4-5 (strings, conversion)
3. **Medium (nice to have):** Phases 6-8 (transcendental, control)
4. **Lower (when time permits):** Phases 9-12 (I/O, system)

---

## Success Criteria

1. Every `prim_*.go` file has a corresponding `prim_*_test.go` file
2. All tests pass: `make test` returns 0
3. Coverage > 90%: `go test -cover` shows 90%+ for primitives package
4. All numeric types tested: Integer, BigInteger, Float, BigFloat, Rational, Complex
5. Edge cases covered: empty inputs, boundary values, error conditions
6. No regressions: existing functionality continues to work
