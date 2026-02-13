# Tokenizer Number Parsing Consolidation Plan

**Status:** PLANNED — Analysis complete, implementation not started

**Risk: MEDIUM-HIGH** (tokenizer is critical path)

## Problem

The tokenizer's number parsing (~700 lines, lines 1080-1800) contains significant repetition due to Scheme's complex numeric literal grammar. Reusable components ("quasi-sub-tokens") appear repeatedly in different contexts.

## Identified Quasi-Sub-Tokens

| Component | Current Pattern | Instances | Proposed Helper | Savings |
|-----------|----------------|-----------|-----------------|---------|
| Optional sign `[+-]?` | `if isExplicitSign(p.curr()) { p.next() ... }` | ~12 | `mayConsumeSign() rune` | ~50 lines |
| Decimal fraction `. <digit>*` | `if isDot(p.curr()) { p.next() ... readUnsignedBaseNNumber ... }` | ~6 | `mayConsumeDecimalFraction(r) DecimalFractionResult` | ~60 lines |
| Special numbers (inf.0, nan.0) | 3 overlapping functions + `scanForImaginaryNumberSpecials` (duplicate) | 4 | `mayConsumeSpecialNumber(keyword, r) SpecialNumberResult` | ~40 lines |
| Imaginary suffix `i` | `if isImaginary(p.curr()) { setState... p.next() }` | ~10 | `mayConsumeImaginary(signed) bool` | ~30 lines |
| Complex suffix dispatch | `switch { case isImaginary ... case isExplicitSign ... case isComplexPolar ... }` | ~4 | `mayConsumeComplexSuffix(signed, r) ComplexSuffixType` | ~40 lines |
| Error-check-return | `p.next(); if p.err != nil { return }` | ~50 | Absorbed into above helpers | ~50 lines |
| Signed/unsigned state pairs | `if signed { p.state = Signed* } else { p.state = Unsigned* }` | ~10 | `setState(pair, signed)` with lookup table | ~15 lines |

**Estimated total savings: ~295 lines**

## Implementation Phases

| Phase | Description | Risk |
|-------|-------------|------|
| 1 | Low-risk helpers: `mayConsumeSign`, `mayConsumeDecimalFraction`, `mayConsumeImaginary` | LOW |
| 2 | Special number consolidation: merge `scanForImaginaryNumberSpecials` into `readSpecialNumber` | MEDIUM |
| 3 | Complex suffix unification: `mayConsumeComplexSuffix` | MEDIUM |
| 4 | State management: signed/unsigned state pair table | LOW |
| 5 | Sub-tokenizer architecture (optional, major change) | HIGH |

## Files to Modify

| File | Change |
|------|--------|
| `internal/tokenizer/tokenizer.go` | Add helpers, refactor number parsing |
| `internal/tokenizer/number_helpers.go` | New — quasi-sub-token helpers (~100 lines) |

## Verification

All existing tokenizer tests must pass. Edge cases: `+inf.0`, `-nan.0`, `+i`, `3+4i`, `1@1.5708`, `#b101`, `#e1.5`, `.5`, `1/2`.
