# Tokenizer Number Parsing Consolidation Plan

**Status:** PLANNED — Analysis complete, implementation not started

## Executive Summary

The tokenizer's number parsing (~700 lines, lines 1080-1800) contains significant repetition due to Scheme's complex numeric literal grammar. This plan identifies "quasi-sub-tokens" - reusable number components - and proposes consolidation through a sub-tokenizer architecture.

**Estimated savings: ~400-600 bytes (200-300 lines)**
**Risk: MEDIUM-HIGH (tokenizer is critical path)**

---

## Analysis: Scheme Number Grammar

Scheme numbers have a recursive structure with reusable components:

```
<number>      ::= <prefix>? <complex>
<complex>     ::= <real> | <real> @ <real> | <real> [+-] <ureal>? i
<real>        ::= [+-]? <ureal> | <infnan>
<ureal>       ::= <uinteger> | <uinteger> / <uinteger> | <decimal>
<decimal>     ::= <uinteger> <suffix>? | . <digit>+ <suffix>? | <uinteger> . <digit>* <suffix>?
<suffix>      ::= <exp-marker> [+-]? <digit>+
<infnan>      ::= [+-] inf.0 | [+-] nan.0
```

These sub-components appear repeatedly in different contexts.

---

## Identified Quasi-Sub-Tokens

### 1. Digit Sequence (`[0-9]+` for radix r)

**Already consolidated**: `readUnsignedBaseNNumber(r, maxn)`

**Usage count**: ~15 direct calls

**Status**: Good - no change needed

### 2. Optional Sign (`[+-]?`)

**Current pattern** (appears ~12 times):
```go
if isExplicitSign(p.curr()) {
    p.next()
    if p.err != nil {
        return
    }
}
```

**Locations**:
- `readBigNum:1126-1131` (6 lines)
- `mayReadExponent:1554-1560` (7 lines)
- `mayReadSignedImaginaryPart:1614-1619` (6 lines)
- `mayReadPolarPart:1684-1689` (6 lines)
- `readBaseNInteger:1734-1741` (8 lines)
- And 7+ more instances

**Proposed consolidation**:
```go
// mayConsumeSign advances past an optional sign character.
// Returns the sign rune ('+', '-', or 0 if none).
func (p *Tokenizer) mayConsumeSign() rune {
    if !isExplicitSign(p.curr()) {
        return 0
    }
    sign := p.curr()
    p.next()
    return sign
}
```

**Savings**: ~50 lines (~500 bytes)

### 3. Decimal Fraction (`. <digit>*`)

**Current pattern** (appears ~6 times):
```go
if isDot(p.curr()) {
    p.next()
    if p.err != nil {
        return
    }
    if isDigit(r, p.curr()) {
        p.readUnsignedBaseNNumber(r, 0)
        if p.err != nil {
            return
        }
    }
}
```

**Locations**:
- `readDecimalFractionWithExponent:1180-1194` (15 lines)
- `mayReadUnsignedFractionalRealNumberOrRationalRealNumber:1489-1514` (26 lines)
- `mayReadSignedImaginaryPart:1641-1661` (21 lines)
- `mayReadPolarPart:1693-1716` (24 lines)
- `readBigNum:1137-1146` (10 lines)

**Proposed consolidation**:
```go
// DecimalFractionResult holds the result of parsing a decimal fraction.
type DecimalFractionResult struct {
    HasDot     bool
    HasDigits  bool
}

// mayConsumeDecimalFraction parses an optional decimal point and following digits.
// Returns whether a dot was consumed and whether digits followed it.
func (p *Tokenizer) mayConsumeDecimalFraction(r int) DecimalFractionResult {
    result := DecimalFractionResult{}
    if !isDot(p.curr()) {
        return result
    }
    result.HasDot = true
    p.next()
    if p.err != nil {
        return result
    }
    if isDigit(r, p.curr()) {
        result.HasDigits = true
        p.readUnsignedBaseNNumber(r, 0)
    }
    return result
}
```

**Savings**: ~60 lines (~600 bytes)

### 4. Exponent Suffix (`[eEsSfFdDlL] [+-]? <digit>+`)

**Already consolidated**: `mayReadExponent(r)` (34 lines)

**Usage count**: ~8 direct calls

**Status**: Good - but internal duplication with error handling

### 5. Special Numbers (inf.0, nan.0)

**Current state**: Three overlapping functions:
- `readSpecialNumber(s, r, mismatchErr, onMismatch)` - generic helper (24 lines)
- `readInf(s, r)` - wrapper (3 lines)
- `readNan(s, r)` - wrapper (7 lines)
- `scanForImaginaryNumberSpecials(r, txt)` - **duplicate logic** (34 lines)

**The duplication**:
```go
// scanForImaginaryNumberSpecials duplicates readSpecialNumber logic:
func (p *Tokenizer) scanForImaginaryNumberSpecials(r int, txt string) {
    n := p.scan([]byte(txt))           // same as readSpecialNumber
    if p.err != nil { return }
    if n != 0 { ... error ... }        // same pattern
    if !isDot(p.curr()) { ... }        // same check
    p.next()
    if !isDigit(r, p.curr()) { ... }   // same check
    p.readUnsignedBaseNNumber(r, 0)    // same call
    if !isImaginary(p.curr()) { return }  // DIFFERENT: imaginary check
    p.next()
}
```

**Proposed consolidation**:
```go
// SpecialNumberResult holds the result of parsing inf.0 or nan.0.
type SpecialNumberResult struct {
    Matched    bool
    Imaginary  bool  // true if followed by 'i'
}

// mayConsumeSpecialNumber parses inf.0 or nan.0, optionally followed by 'i'.
func (p *Tokenizer) mayConsumeSpecialNumber(keyword string, r int) SpecialNumberResult {
    result := SpecialNumberResult{}
    n := p.scan([]byte(keyword))
    if p.err != nil || n != 0 {
        return result
    }
    if !isDot(p.curr()) {
        return result
    }
    p.next()
    if p.err != nil || !isDigit(r, p.curr()) {
        return result
    }
    p.readUnsignedBaseNNumber(r, 0)
    result.Matched = true
    if p.err == nil && isImaginary(p.curr()) {
        result.Imaginary = true
        p.next()
    }
    return result
}
```

Then simplify callers:
```go
func (p *Tokenizer) readInf(s string, r int) {
    result := p.mayConsumeSpecialNumber(s, r)
    if !result.Matched {
        p.err = NewTokenizerError(MessageExpectingInf, p.tokenStart, p.tokenEnd)
    }
}
```

**Savings**: ~40 lines (~400 bytes)

### 6. Imaginary Suffix (`i`)

**Current pattern** (appears ~10 times):
```go
if isImaginary(p.curr()) {
    if signed {
        p.state = TokenizerStateSignedImaginary
    } else {
        p.state = TokenizerStateUnsignedImaginary
    }
    p.next()
}
```

**Locations**:
- `readIntegerAndFraction:1335-1341` (7 lines)
- `readSignedDecimalFractionOrExponentWithImaginary:1289-1292` (4 lines)
- `readImaginaryOrSignedInfinity:1232-1237` (6 lines)
- `mayReadSignedImaginaryPart:1663-1666` (4 lines)
- And 5+ more

**Proposed consolidation**:
```go
// mayConsumeImaginary checks for and consumes trailing 'i'.
// Updates state to appropriate imaginary state if found.
func (p *Tokenizer) mayConsumeImaginary(signed bool) bool {
    if !isImaginary(p.curr()) {
        return false
    }
    if signed {
        p.state = TokenizerStateSignedImaginary
    } else {
        p.state = TokenizerStateUnsignedImaginary
    }
    p.next()
    return true
}
```

**Savings**: ~30 lines (~300 bytes)

### 7. Complex Number Suffix Dispatch

**Current pattern** (appears ~4 times):
```go
switch {
case isImaginary(p.curr()):
    // handle pure imaginary
case isExplicitSign(p.curr()):
    // handle complex with signed imaginary part
case isComplexPolar(p.curr()):
    // handle polar form
}
```

**Locations**:
- `readIntegerAndFraction:1334-1366` (33 lines)
- `readSignedDecimalFractionOrExponentWithImaginary:1288-1299` (12 lines)
- `readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber:1446-1464` (19 lines)

**Proposed consolidation**:
```go
// ComplexSuffixType indicates what kind of complex suffix was found.
type ComplexSuffixType int

const (
    ComplexSuffixNone ComplexSuffixType = iota
    ComplexSuffixImaginary      // ends with 'i'
    ComplexSuffixRectangular    // has +/- followed by imaginary part
    ComplexSuffixPolar          // has @ followed by angle
)

// mayConsumeComplexSuffix handles the suffix of a real number that might
// make it part of a complex number.
func (p *Tokenizer) mayConsumeComplexSuffix(signed bool, r int) ComplexSuffixType {
    switch {
    case isImaginary(p.curr()):
        p.mayConsumeImaginary(signed)
        return ComplexSuffixImaginary
    case isExplicitSign(p.curr()):
        if signed {
            p.state = TokenizerStateSignedComplex
        } else {
            p.state = TokenizerStateUnsignedComplex
        }
        p.mayReadSignedImaginaryPart(signed, r)
        return ComplexSuffixRectangular
    case isComplexPolar(p.curr()):
        if signed {
            p.state = TokenizerStateSignedComplexPolar
        } else {
            p.state = TokenizerStateUnsignedComplexPolar
        }
        p.mayReadPolarPart(r)
        return ComplexSuffixPolar
    }
    return ComplexSuffixNone
}
```

**Savings**: ~40 lines (~400 bytes)

### 8. Error-Check-And-Return Pattern

**Current pattern** (appears ~50+ times):
```go
p.next()
if p.err != nil {
    return
}
```

**Proposed consolidation** - Two approaches:

**Approach A: Helper method**
```go
// advanceOrReturn advances to the next character.
// Returns true if an error occurred (caller should return).
func (p *Tokenizer) advanceOrReturn() bool {
    p.next()
    return p.err != nil
}

// Usage:
if p.advanceOrReturn() { return }
```

**Approach B: Combine with consumption**
Already done implicitly in the other helpers (`mayConsumeSign`, `mayConsumeDecimalFraction`, etc.)

**Savings**: ~50 lines (~500 bytes) with approach A

---

## State Machine Consolidation

### Current State Explosion

The tokenizer has 40+ numeric states with parallel signed/unsigned variants:
- `TokenizerStateSignedInteger` / `TokenizerStateUnsignedInteger`
- `TokenizerStateSignedDecimalFraction` / `TokenizerStateUnsignedDecimalFraction`
- `TokenizerStateSignedImaginary` / `TokenizerStateUnsignedImaginary`
- etc.

**Current pattern**:
```go
if signed {
    p.state = TokenizerStateSignedDecimalFraction
} else {
    p.state = TokenizerStateUnsignedDecimalFraction
}
```

**Proposed: State lookup table**
```go
type signedStatePair struct {
    signed   TokenizerState
    unsigned TokenizerState
}

var decimalFractionStates = signedStatePair{
    signed:   TokenizerStateSignedDecimalFraction,
    unsigned: TokenizerStateUnsignedDecimalFraction,
}

var imaginaryStates = signedStatePair{
    signed:   TokenizerStateSignedImaginary,
    unsigned: TokenizerStateUnsignedImaginary,
}

// setState sets the appropriate signed or unsigned state.
func (p *Tokenizer) setState(pair signedStatePair, signed bool) {
    if signed {
        p.state = pair.signed
    } else {
        p.state = pair.unsigned
    }
}

// Usage:
p.setState(decimalFractionStates, signed)
```

**Savings**: ~30 lines (~300 bytes)

---

## Proposed Sub-Tokenizer Architecture

### High-Level Design

Create a `NumberSubTokenizer` that encapsulates number parsing:

```go
// NumberSubTokenizer handles Scheme numeric literal parsing.
type NumberSubTokenizer struct {
    tok      *Tokenizer
    radix    int
    signed   bool
    exactness Exactness  // unknown, exact, inexact
}

// NumberPart represents a parsed component of a number.
type NumberPart struct {
    Type       NumberPartType
    IntPart    string
    FracPart   string
    ExpSign    rune
    ExpPart    string
    Imaginary  bool
}

type NumberPartType int

const (
    NumberPartInteger NumberPartType = iota
    NumberPartDecimal
    NumberPartRational
    NumberPartInf
    NumberPartNan
)

// ParseReal parses a real number component.
func (p *NumberSubTokenizer) ParseReal() (*NumberPart, error) {
    // Unified logic for parsing real numbers
    // Uses the quasi-sub-token helpers
}

// ParseComplex parses a complex number.
func (p *NumberSubTokenizer) ParseComplex() (*NumberPart, *NumberPart, error) {
    // Parses real part, then optional imaginary part
}
```

### Benefits

1. **Single entry point** for number parsing instead of 15+ functions
2. **Clearer grammar mapping** - each sub-tokenizer method corresponds to a grammar production
3. **Easier testing** - can unit test number parsing in isolation
4. **Better error messages** - context is preserved in the sub-tokenizer

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| Behavioral changes | HIGH | Extensive test coverage before/after |
| Performance regression | MEDIUM | Benchmark critical paths |
| Increased complexity | LOW | Actually reduces complexity |

---

## Summary: Consolidation Opportunities

| Component | Current Lines | Proposed Lines | Savings |
|-----------|---------------|----------------|---------|
| Optional sign handling | ~72 (12×6) | ~15 | ~57 |
| Decimal fraction parsing | ~96 (6×16) | ~25 | ~71 |
| Special number (inf/nan) | ~68 | ~35 | ~33 |
| Imaginary suffix | ~50 (10×5) | ~15 | ~35 |
| Complex suffix dispatch | ~64 (4×16) | ~30 | ~34 |
| Error-check-return | ~100 (50×2) | ~50 | ~50 |
| Signed/unsigned states | ~30 (10×3) | ~15 | ~15 |
| **Total** | **~480** | **~185** | **~295** |

**Estimated savings: ~295 lines (~3,000 bytes)**

---

## Implementation Phases

### Phase 1: Low-Risk Helpers (1-2 days)

1. Add `mayConsumeSign()` helper
2. Add `mayConsumeDecimalFraction()` helper
3. Add `mayConsumeImaginary()` helper
4. Migrate callers incrementally

**Risk**: LOW - additive changes, no behavioral modification

### Phase 2: Special Number Consolidation (1 day)

1. Refactor `scanForImaginaryNumberSpecials` to use `readSpecialNumber`
2. Add imaginary tracking to special number parsing

**Risk**: MEDIUM - modifies existing logic

### Phase 3: Complex Suffix Unification (1-2 days)

1. Create `mayConsumeComplexSuffix()`
2. Consolidate `readIntegerAndFraction` suffix handling
3. Consolidate `readSignedDecimalFractionOrExponentWithImaginary` suffix handling

**Risk**: MEDIUM - touches core number parsing

### Phase 4: State Management (1 day)

1. Create state pair lookup table
2. Add `setState()` helper
3. Migrate signed/unsigned branching

**Risk**: LOW - mechanical refactoring

### Phase 5: Sub-Tokenizer Architecture (Optional, 3-5 days)

1. Design `NumberSubTokenizer` interface
2. Implement incrementally
3. Extensive testing

**Risk**: HIGH - major architectural change

---

## Verification Strategy

1. **Existing tests**: All tokenizer tests must pass
2. **Fuzzing**: Generate random numeric literals to compare before/after
3. **Edge cases**: Ensure special cases work:
   - `+inf.0`, `-inf.0`, `+nan.0`, `-nan.0`
   - `+i`, `-i`, `3+4i`, `3-4i`
   - `1@1.5708` (polar)
   - `#b101`, `#o777`, `#xDEAD` (radix)
   - `#e1.5`, `#i3` (exactness)
   - `.5`, `1.`, `1.0`, `1e10` (decimal variants)
   - `1/2`, `+3/4`, `-5/6` (rationals)

---

## Files to Modify

| File | Change |
|------|--------|
| `go/tokenizer/tokenizer.go` | Add helpers, refactor number parsing (~1800-line file) |
| `go/tokenizer/number_helpers.go` | New file for quasi-sub-token helpers (~100 lines) |

---

## Comparison with TODO.md

The existing TODO.md (lines 485-494) already identifies some of these:

```
- [ ] `readRadixPrefix` — consolidate #b/#o/#d/#x handling
- [ ] `readBooleanLiteral` — consolidate #t/#true and #f/#false
- [ ] `readDecimalFractionWithExponent` — extract decimal+exponent pattern
- [ ] `readImaginarySuffix` — consolidate imaginary number suffixes
- [ ] `readExplicitSignNumber` — consolidate +/- number handling
- [ ] `readInfNan` — consolidate inf.0/nan.0 parsing
```

This plan provides **concrete implementations** for these TODOs and adds additional opportunities not previously identified:
- Complex suffix dispatch consolidation
- State pair table
- Error-check-return pattern
- Sub-tokenizer architecture option
