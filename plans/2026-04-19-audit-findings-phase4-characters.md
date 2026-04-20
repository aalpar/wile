# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Characters

**Status**: Complete. 2 code findings + test correction, all resolved.
**Category**: R7RS §6.6 Characters (~20 primitives in `registry/core/characters.go` + `internal/extensions/all/prim_characters.go`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C).
**Prior categories**: bytevectors (2), strings (2+1), ports (0+2), lists (1+1).

## Scope

| File | Primitives |
|---|---|
| `registry/core/characters.go` | `char->integer`, `integer->char`, `char=?`/`<?`/`>?`/`<=?`/`>=?` |
| `internal/extensions/all/prim_characters.go` | `char-ci=?` family, `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`, `char-lower-case?`, `char-upcase`, `char-downcase`, `char-foldcase`, `digit-value` |

## Finding F.1 — `char-alphabetic?` missing Nl category

**Severity:** medium (R7RS spec deviation affecting real Unicode input). **Status:** fixed.

R7RS §6.6 explicitly enumerates the categories that count as alphabetic:

> Alphabetic: Lu, Ll, Lt, Lm, Lo, Nl

Old impl:

```go
var PrimCharAlphabeticQ = helpers.MakeCharPredicate("char-alphabetic?", unicode.IsLetter)
```

`unicode.IsLetter` covers Lu, Ll, Lt, Lm, Lo — **but not Nl** (Letter Number). Result: `(char-alphabetic? Ⅰ)` wrongly returned `#f` for U+2160 ROMAN NUMERAL ONE and other letter numerals.

### Fix

```go
var PrimCharAlphabeticQ = helpers.MakeCharPredicate("char-alphabetic?", func(r rune) bool {
    return unicode.IsLetter(r) || unicode.In(r, unicode.Nl)
})
```

Docstring updated from "Unicode letter (Lu, Ll, Lt, Lm, Lo categories)" to cite R7RS §6.6 explicitly with the Nl addition.

### Why the existing `TestCharAlphabeticUnicode` did not catch this

The test docstring cited the Unicode "Alphabetic" property, which coincidentally includes Nl. But the test cases only covered Lu, Ll, Lt, and Lo characters — no Nl examples (Roman numerals, letter-like numerics). The bug was an absent-case gap, not a wrong-case gap.

## Finding F.2 — `char-numeric?` missing Nl and No categories

**Severity:** medium (R7RS spec deviation). **Status:** fixed.

R7RS §6.6:

> Numeric: Nd, Nl, No

Old impl:

```go
var PrimCharNumericQ = helpers.MakeCharPredicate("char-numeric?", unicode.IsDigit)
```

`unicode.IsDigit` covers only Nd (decimal digits). Results:

- `(char-numeric? Ⅰ)` wrongly returned `#f` for U+2160 ROMAN NUMERAL ONE (Nl)
- `(char-numeric? ½)` wrongly returned `#f` for U+00BD VULGAR FRACTION ONE HALF (No)
- `(char-numeric? ²)` wrongly returned `#f` for U+00B2 SUPERSCRIPT TWO (No)

### Fix

```go
var PrimCharNumericQ = helpers.MakeCharPredicate("char-numeric?", unicode.IsNumber)
```

Go's `unicode.IsNumber` covers categories Nd | Nl | No — exactly the R7RS set.

Docstring updated from "Unicode decimal digit" to cite R7RS §6.6 (Nd, Nl, No).

### Incorrect existing test

`TestCharNumericUnicode` (`prim_char_extra_test.go:693`) enshrined the wrong behavior with two cases and a misleading docstring:

- Docstring claimed R7RS §6.6 wants "Numeric_Type=Decimal" — wrong. That's a Unicode UCD property, not the R7RS §6.6 category list.
- `"Roman numeral V is not numeric (Nl category, not Nd)"` — expected `#f`. This is non-R7RS.
- `"superscript 2 is not numeric (No category, not Nd)"` — expected `#f`. Non-R7RS.

Per root CLAUDE.md:

> Tests that conform to R7RS must not be removed or reverted. If a test fails but correctly reflects R7RS behavior, the implementation must be fixed — not the test.

These tests did **not** conform to R7RS — they documented a non-spec interpretation. Updated the docstring and flipped the two enshrined-wrong expectations to `#t`. Turkish dotted/dotless `I` cases correctly stayed `#f` (those are Lu/Ll, not N*).

## Not-findings (positive verification)

### Parallel-case lens between char and string

- `char-foldcase` uses simple (1:1) Unicode mapping via `simpleCaseFold`.
- `string-foldcase` uses full Unicode case folding via `cases.Fold()` (can expand — e.g., `ß → ss`).

**Correct per R7RS.** §6.6 on `char-foldcase`:

> Note that the Unicode tables map characters to characters; a char always has a well-defined simple fold.

§6.7 on `string-foldcase`:

> Applies the full case folding algorithm... In contrast to char-foldcase, the result string can be different from the input string in both lengths and contents.

The asymmetry is required by the spec. Wile is correct.

### `integer->char` range validation

Rejects all three out-of-range cases per R7RS and Unicode:

- Negative inputs
- Values above `0x10FFFF` (outside Unicode scalar value range)
- Surrogate halves `0xD800..0xDFFF` (not legal Unicode scalar values)

Boundary: `0xD7FF` accepted, `0xE000` accepted. Correct.

### Sharp-S fold and Kelvin sign fold

- `(char-foldcase ẞ)` → `ß` (U+1E9E → U+00DF) — handled by explicit `simpleCaseFold` case matching Unicode CaseFolding.txt.
- `(char-foldcase ℵ)` — skipped; K-series Kelvin-sign fold (U+212A → U+006B) handled by `unicode.ToLower` default path. (Note: the `case 'K':` arm in `simpleCaseFold` is dead code with a misleading comment — the literal `'K'` is ASCII U+004B, not U+212A. Both route to `unicode.ToLower` via the default path; no semantic bug, just stale code. Not a finding this session.)

### `digit-value` across scripts

Correctly returns 0–9 for Arabic-Indic, Devanagari, Thai digit families. Returns `#f` for non-digits. Correct per R7RS.

### `char-whitespace?` covers NEL and NBSP

U+0085 (NEL) and U+00A0 (NBSP) both return `#t`. Go's `unicode.IsSpace` matches R7RS §6.6's "Zs, Zl, Zp + TAB LF VT FF CR NEL" requirement.

### ParamTypes clean

All character primitives declare correct user-facing types. No B.1-class `TypeByte`-equivalent leaks (would be `TypeCharacter` leaks of internal representation) — the `*values.Character` type is the correct Scheme-facing type.

## Phase 4 scoreboard after 5 categories

| Category | Code findings | Doc findings |
|---|---|---|
| bytevectors | 2 | 0 |
| strings | 2 | 1 |
| ports | 0 | 2 |
| lists | 1 | 1 |
| characters | 2 | 0 |

Characters produced 2 code findings — highest since bytevectors. The finding class is different from everything prior: **both findings are "reasonable-looking Go stdlib choices that don't match R7RS's exact category list."** This is the first category where the errors were at the spec-interpretation layer rather than registration/annotation.

Hypothesis: categories with **explicit R7RS category enumerations** (whitespace has them, alphabetic/numeric have them, case-mapping has them) are denser in this class of bug than categories with behavior-defined spec text (like list ops). Numbers (next/last session) will be the hardest — R7RS §6.2 enumerates dozens of exactness / contagion rules that are easy to get subtly wrong.

## Next sessions

- **numbers** (R7RS §6.2) — largest surface. Dense test coverage already exists; the audit value is verifying edge cases in exactness contagion, NaN/inf semantics, and primitive annotation lies.
- **control** (R7RS §6.10) — call/cc, values, dynamic-wind. Tricky semantics; primarily verified via Phase 1 and extensive existing tests.
- **exceptions** (R7RS §6.11) — covered by Phase 1 A.2.
- **records / promises** — SRFI-9 & R7RS §4.2.5. Bonus category beyond §6.
