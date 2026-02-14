# Tokenizer Redundancy Refactoring Plan

**Status:** COMPLETE — All phases (1-8) implemented
**File:** `internal/tokenizer/tokenizer.go` (2,158 lines, down from 2,287)
**Prior work:** `TOKENIZER_CONSOLIDATION_PLAN.md` (~174 lines saved)
**Phase 1 savings realized:** ~108 lines (dead code, trivial wrappers, duplicate predicates)

## Problem

The tokenizer's number parsing (~700 lines, lines 1080-1780) contains repeated micro-patterns driven by Scheme's complex numeric grammar. After Phase 1 cleanup, the remaining redundancy is both micro-pattern (Phases 4-6) and structural (no `readUreal` primitive).

## Remaining Redundancy Map

```
Category                          Instances   Est. Lines Saved   Risk
─────────────────────────────────────────────────────────────────────
A. Dead code / trivial wrappers      COMPLETE                    —
B. Duplicated validation             COMPLETE                    —
C. readDigitsAndHash micro-pattern   COMPLETE                    —
D. Imaginary suffix dispatch          COMPLETE                    —
E. Complex suffix dispatch (signed)  COMPLETE                    —
F. Optional sign consumption         COMPLETE                    —
G. Inlined readDiv                   COMPLETE                    —
H. Structural: no readUreal           4            ~80           HIGH
I. Dot-subsequent triplication       COMPLETE                    —
─────────────────────────────────────────────────────────────────────
Remaining estimated savings:                      ~150 lines
```

---

## Phase 1: Dead Code and Trivial Wrappers COMPLETE

All items (1a-1h) were implemented across prior branches:
- `readIntraExtendedToken`, `readIntraStringEscape` — inlined
- `escape()`, `this()`, `span()`, `isEOF()`, `digit()`, `digs` — deleted
- `isSymbolInitial`, `isIdentifierInitial` — unified to `isInitial`
- `isExtendedExponentMarkerForRadix` — inlined

`NewTokenizerWithComments` remains as a no-op wrapper (deferred — active API surface, 25 callers).

---

## Phase 2: Duplicated Validation COMPLETE

`validateCodePoint` extracted (line 575). Both call sites (`readHexEscapeToken`, `readCharacterMnemonicOrCharacterEscapeOrCharacterHexEscape`) use it.

---

## Phase 3: readDigitsAndHash COMPLETE

`readDigitsAndHash(r int)` extracted (line 1786). All 10 call sites updated.

---

## Phase 4: Imaginary Suffix Helper COMPLETE

### 4a. `mayConsumeImaginarySuffix` — absorb trailing `i`

The pattern "check for `i`, consume" appears in two shapes across 11 sites. ~6 are clean extraction candidates:

**Shape A — optional trailing `i` (3 clean sites):**
```go
if isImaginary(p.curr()) {
    p.next()
}
```
Lines 1343, 1605, 1615

**Shape B — negative guard with state-set + consume (3 clean sites):**
```go
if !isImaginary(p.curr()) {
    return
}
p.state = TokenizerState...
p.next()
```
Lines 1226-1230 (`readSignedNan`), 1267 (`readSignedDecimalFractionOrExponentWithImaginary`), 1666-1670 (`mayReadSignedImaginaryPart`)

**NOT clean candidates** (different control flow — leave as-is):
- Lines 1202, 1209: negative check with early return, state already set before check
- Line 1314: signed/unsigned dispatch (if/else on `signed` flag)
- Line 1354: error on mismatch (`MessageExpectingImaginary`)

**Action:** Create `mayConsumeImaginarySuffix() bool`:
```go
func (p *Tokenizer) mayConsumeImaginarySuffix() bool {
    if !isImaginary(p.curr()) {
        return false
    }
    p.next()
    return true
}
```

Shape A sites: replace 3-line `if` block with `p.mayConsumeImaginarySuffix()`.
Shape B sites: use `if !p.mayConsumeImaginarySuffix() { return }` then set state.

Note: the original code sets `p.state` *between* detecting `i` and consuming it. The helper reorders this (consume inside helper, state set by caller after). Semantically safe — `p.state` is only read after `term()`.

**Savings:** ~20 lines

---

## Phase 5: Signed Complex Suffix Dispatch COMPLETE

### 5a. Extract signed-only complex suffix routing

After reading a signed real number, dispatch on the next character is identical in two functions:

**Site 1:** `readSignedDecimalFractionOrExponentWithImaginary` (lines 1267-1279)
**Site 2:** `readIntegerAndFraction` signed branches (lines 1314-1322, interleaved with unsigned)

Both do:
```go
switch {
case isImaginary(p.curr()):
    p.state = TokenizerStateSignedImaginary; p.next()
case isExplicitSign(p.curr()):
    p.state = TokenizerStateSignedComplex
    p.mayReadSignedImaginaryPart(true, r)
case isComplexPolar(p.curr()):
    p.state = TokenizerStateSignedComplexPolar
    p.mayReadPolarPart(r)
}
```

**Action:** Extract `readSignedComplexSuffix(r int)`:
```go
func (p *Tokenizer) readSignedComplexSuffix(r int) {
    switch {
    case isImaginary(p.curr()):
        p.state = TokenizerStateSignedImaginary
        p.next()
    case isExplicitSign(p.curr()):
        p.state = TokenizerStateSignedComplex
        p.mayReadSignedImaginaryPart(true, r)
    case isComplexPolar(p.curr()):
        p.state = TokenizerStateSignedComplexPolar
        p.mayReadPolarPart(r)
    }
}
```

The unsigned complex path in `readIntegerAndFraction` (lines 1325-1360) has bespoke `i`/`inf.0i` disambiguation and stays inline.

In `readSignedDecimalFractionOrExponentWithImaginary`, the entire tail switch becomes `p.readSignedComplexSuffix(r)`.
In `readIntegerAndFraction`, the `if signed { ... }` branches collapse into `p.readSignedComplexSuffix(r)`.

**Savings:** ~20 lines

---

## Phase 6: Optional Sign Consumption COMPLETE

### 6a. `mayConsumeSign` — absorb optional `[+-]?`

The clean pattern (5 verified sites):
```go
if isExplicitSign(p.curr()) {
    p.next()
    if p.err != nil {
        return
    }
}
```

| Line | Function |
|------|----------|
| 1092 | `readBigNum` (leading sign) |
| 1122 | `readBigNum` (exponent sign) |
| 1558 | `mayReadExponent` |
| 1583 | `mayReadSignedImaginaryPart` |
| 1687 | `mayReadPolarPart` |

**NOT clean candidates** (need sign value or interleave state-setting):
- `readBaseNInteger` (line 1736): captures sign character and increments counter
- `readImaginaryOrSignedInfinity` (line 1191): sets state between check and next
- `readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber` (line 1439): sign consumption is the root of a decision tree

**Action:** Create `mayConsumeSign()`:
```go
func (p *Tokenizer) mayConsumeSign() {
    if isExplicitSign(p.curr()) {
        p.next()
    }
}
```

Callers still check `p.err` after the call. Reduces 4-line blocks to 2-line blocks.

**Savings:** ~10 lines

---

## Phase 7: Inlined readDiv Fix COMPLETE

### 7a. Replace hand-unrolled `readDiv` in `mayReadSignedImaginaryPart`

Lines 1636-1649 in `mayReadSignedImaginaryPart`:
```go
case p.curr() == '/':
    p.next()
    if p.err != nil { return }
    if !isDigit(r, p.curr()) {
        p.err = NewTokenizerError(MessageExpectingNumber, ...)
        return
    }
    p.readDigitsAndHash(r)
```

This is exactly `readDiv(r)` (lines 1135-1145). The pattern was hand-unrolled.

**Action:** Replace with `p.readDiv(r)`.

**Savings:** ~5 lines

---

## Structural Analysis: The Missing `readUreal` Primitive

### The core problem

The R7RS grammar has a clean decomposition:

```
<complex> → <real> [ @<real> | ±<ureal>i | ±i | ±inf.0i | ±nan.0i ]
<real>    → [±] <ureal> | ±inf.0 | ±nan.0
<ureal>   → <uinteger> [/<uinteger>] | <decimal>
<decimal> → <uinteger><suffix> | .<digit>+<suffix> | <digit>+.<digit>*<suffix>
```

The tokenizer doesn't map functions to these productions. The concept of "read a real number" is duplicated across four functions:

| Function | What it reads | Lines |
|----------|--------------|-------|
| `readIntegerAndFraction` | Full real + complex suffixes | 1278-1365 |
| `mayReadUnsigned...RationalRealNumber` | Real after sign consumed (unsigned context) | 1480-1532 |
| `mayReadSignedImaginaryPart` | Real coefficient before mandatory `i` | 1574-1662 |
| `mayReadPolarPart` | Real angle after `@` | 1668-1722 |

Each reads `[sign] digits [.digits] [/digits] [e[sign]digits]` with different state-setting, different continuation logic, and different error handling.

### Call graph showing duplication

```
readUnsignedFractional...
├─ readIntegerAndFraction(signed, r)
│  ├─ readDigitsAndHash           ← digits
│  ├─ readDecimalFractionWithExp  ← .digits
│  ├─ readDiv                     ← /digits
│  ├─ mayReadExponent             ← e[±]digits
│  └─ [complex suffix dispatch]   ← i / ± / @
│
├─ readSignedDecimalFraction...WithImaginary(r)
│  ├─ readDigitsAndHash           ← digits
│  ├─ mayReadExponent             ← e[±]digits
│  └─ [complex suffix dispatch]   ← i / ± / @  ← SAME as above (signed only)
│
└─ readConsOrDecimalFractionWithExponent(r)
   ├─ readDigitsAndHash           ← digits
   └─ mayReadExponent             ← e[±]digits

mayReadUnsigned...RationalRealNumber
├─ readDigitsAndHash              ← digits (SAME)
├─ readDecimalFractionWithExp     ← .digits (SAME)
├─ readDiv                        ← /digits (SAME)
└─ mayReadExponent                ← e[±]digits (SAME)

mayReadSignedImaginaryPart
├─ [optional sign]
├─ readDigitsAndHash              ← digits (SAME)
├─ readOptionalDecimalPart        ← .digits (SAME)
├─ readDiv-inline                 ← /digits (SAME pattern, hand-unrolled)
├─ mayReadExponent                ← e[±]digits (SAME)
└─ [mandatory i]

mayReadPolarPart
├─ [optional sign]
├─ readDigitsAndHash              ← digits (SAME)
├─ readOptionalDecimalPart        ← .digits (SAME)
└─ mayReadExponent                ← e[±]digits (SAME)
```

The "digits then decimal/rational/exponent" sequence appears **4 times**. The "complex suffix dispatch" appears **2 times** (identical for signed case).

### Extraction approaches

**A. Callback-based:** `readUreal` takes a struct of state assignments (which `TokenizerState` for integer/decimal/rational/scientific). Each caller passes its own state map. Saves ~80 lines but adds indirection.

**B. Read-then-classify:** `readUreal` reads without setting state. Caller inspects what was consumed. Requires `readUreal` to communicate what it parsed — messy for a tokenizer that doesn't backtrack.

**C. Incremental (recommended):** Deduplicate `mayReadSignedImaginaryPart` and `mayReadPolarPart` by replacing their digit/fraction/exponent logic with calls to existing helpers (`readDecimalFractionWithExponent`, `readDiv`, `mayReadExponent`). Low-risk, saves ~30-40 lines. Defer full `readUreal` extraction.

### Decision

The full `readUreal` extraction fights against the tokenizer's "set state as you go" model — every call site interleaves state assignments differently. Approach C (incremental cleanup of the worst offenders) is the pragmatic choice. The `readUreal` extraction is documented debt, justified only if a new number format is added.

---

## Phase 8: Dot-Subsequent Triplication COMPLETE

The pattern "is the character after `.` a dot-subsequent → symbol, else digit → decimal fraction" appears in three places with accidental differences:

| Location | Line | Behavior on dot-subsequent |
|----------|------|---------------------------|
| `readSignedDecimalFraction...` | 1238 | Sets Symbol, reads full symbol |
| `readConsOrDecimalFractionWithExponent` | 1373 | Sets Symbol, reads subsequent chars inline |
| `mayReadUnsigned...RationalRealNumber` | 1499 | TODO comment, reads full symbol |

These evolved separately but do the same thing. Unifiable if Phases 4-7 are already touching these functions.

**Savings:** ~15 lines

---

## Implementation Order

```
Phase 7 (Inlined readDiv)          → LOW risk, trivial single-site fix
Phase 6 (mayConsumeSign)           → MED risk, 5 clean sites, no dependencies
Phase 4 (mayConsumeImaginary)      → MED risk, 6 clean sites
Phase 5 (readSignedComplexSuffix)  → MED risk, can use Phase 4 helper
Phase 8 (Dot-subsequent)           → MED risk, opportunistic with Phase 5
```

Dependencies: Phase 5 benefits from Phase 4 (`readSignedComplexSuffix` can use `mayConsumeImaginarySuffix` in its imaginary case). Phase 7 is independent. All other phases are independent.

Each phase is independently committable and testable.

## Verification

All existing tokenizer tests must pass after each phase. Key edge cases:
- `+inf.0`, `-inf.0`, `+nan.0`, `-nan.0`
- `+i`, `-i`, `3+4i`, `1+inf.0i`, `1-nan.0i`
- `1@1.5708` (polar)
- `#b101`, `#o77`, `#xFF`, `#d42`
- `#e1.5`, `#i3`
- `.5`, `1.5`, `1/2`
- `1##`, `1.##e2` (hash digits)
- `...`, `.foo` (dot-initial symbols)

```bash
go test -v -count=1 ./internal/tokenizer/...
```

## Non-Targets (Intentionally Not Consolidated)

| Pattern | Why Keep |
|---------|----------|
| Signed vs unsigned state assignment | Semantic distinction — R7RS tracks signedness |
| Unsigned complex path in `readIntegerAndFraction` | Bespoke `i`/`inf.0i` disambiguation, doesn't match signed pattern |
| `readBigNum` separate from regular number parsing | Different grammar (arbitrary precision, no hash digits) |
| `readString` vs `readExtendedSymbol` | Different termination chars — already share `readEscapeSequence` |
| `NewTokenizerWithComments` | No-op wrapper but active API surface (25 callers) — separate decision |
| `readDiv` called from `mayReadPolarPart` for `.` | Misleading name but correct behavior; renaming is cosmetic |
| Mnemonic/directive iteration loops | Only 2 instances; too few to justify extraction |
| `readSpecialNumber` + optional imaginary `i` | 3 sites with variation — below extraction threshold |
| Full `readUreal` extraction | Fights "set state as you go" model; justified only if adding new number format |

## Summary

```
                            Lines    Cumulative
Prior consolidation          ~174        ~174
Phase 1 (complete)           ~108        ~282
Phases 2-3 (complete)         ~22        ~304
Phases 4-6 (complete)         ~11        ~315
Phases 7-8 (complete)         ~10        ~325
                                     ─────────
Original file size:         2,287
Final file size:            2,158
Total reduction:            129 lines (~5.6%)
```

Phases 4-8 saved fewer gross lines than the original estimates (~21 vs ~70) because each new helper adds its own function signature, docstring, and body. The value is readability: five new named helpers (`mayConsumeImaginarySuffix`, `readSignedComplexSuffix`, `mayConsumeSign`, `readDotSubsequentSymbol`, `readDiv` reuse) replace scattered micro-patterns. The structural `readUreal` extraction (~80 lines, HIGH risk) remains as documented debt — justified only if a new number format is added.
