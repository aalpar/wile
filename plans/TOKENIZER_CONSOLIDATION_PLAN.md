# Tokenizer Number Parsing Consolidation Plan

**Status:** PARTIALLY COMPLETE — Core consolidation done, further extraction deferred

**Risk: MEDIUM-HIGH** (tokenizer is critical path)

## Problem

The tokenizer's number parsing (~700 lines, lines 1080-1800) contains significant repetition due to Scheme's complex numeric literal grammar. Reusable components ("quasi-sub-tokens") appear repeatedly in different contexts.

## Completed Work

| Change | PR/Branch | Lines Saved |
|--------|-----------|-------------|
| `readDelimited` — unified string/symbol scanning | PR #230 (`refactor/tokenizer-reader-consolidation`) | ~60 |
| Predicate cleanup — `isSymbolInitial`/`isIdentifierInitial` → `isInitial`, `for`→`if` fix | `refactor/tokenizer-predicate-cleanup` | ~17 |
| `signedState` helper — 5 if/else blocks in `readIntegerAndFraction` | `refactor/tokenizer-signed-state-helper` | ~12 |
| Delete `scanForImaginaryNumberSpecials` — replaced with `readSpecialNumber` + imaginary check | `refactor/tokenizer-number-parsing-consolidation` | ~50 |
| `readOptionalDecimalPart` — unified decimal fraction pattern from 3 sites | `refactor/tokenizer-number-parsing-consolidation` | ~35 |

**Total realized savings: ~174 lines**

## Remaining Quasi-Sub-Tokens (Deferred)

| Component | Current Pattern | Instances | Proposed Helper | Savings |
|-----------|----------------|-----------|-----------------|---------|
| Optional sign `[+-]?` | `if isExplicitSign(p.curr()) { p.next() ... }` | ~12 | `mayConsumeSign() rune` | ~50 lines |
| Imaginary suffix `i` | `if isImaginary(p.curr()) { setState... p.next() }` | ~10 | `mayConsumeImaginary(signed) bool` | ~30 lines |
| Complex suffix dispatch | `switch { case isImaginary ... case isExplicitSign ... case isComplexPolar ... }` | ~4 | `mayConsumeComplexSuffix(signed, r) ComplexSuffixType` | ~40 lines |
| Sub-tokenizer architecture (optional, major change) | — | — | — | HIGH risk |

**Estimated remaining savings: ~120 lines** (diminishing returns — each extraction touches more call sites with less identical code)

## Files Modified

| File | Change |
|------|--------|
| `internal/tokenizer/tokenizer.go` | All changes — helpers added inline, no new files needed |

## Verification

All existing tokenizer tests must pass. Edge cases: `+inf.0`, `-nan.0`, `+i`, `3+4i`, `1@1.5708`, `#b101`, `#e1.5`, `.5`, `1/2`.
