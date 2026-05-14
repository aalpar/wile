# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Numbers

**Status**: Complete. 1 code finding (H.1), resolved.
**Category**: R7RS §6.2 Numbers (~50 primitives across `registry/core/arithmetic.go`, `registry/core/predicates.go`, `extensions/math/*`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C).
**Prior categories**: bytevectors (2), strings (2+1), ports (0+2), lists (1+1), characters (2), exceptions (0+1), control (0), records/promises (0).

**This is the Phase 4 finale.** All 9 R7RS-small categories audited.

## Scope

| File | Primitives |
|---|---|
| `arithmetic.go` | `+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `abs`, `min`, `max`, `quotient`, `remainder`, `modulo`, `gcd`, `lcm`, `exact`, `inexact`, `exact->inexact`, `inexact->exact` |
| `predicates.go` | `number?`, `complex?`, `real?`, `rational?`, `integer?`, `exact?`, `inexact?`, `exact-integer?`, `zero?`, `positive?`, `negative?`, `odd?`, `even?` |
| `extensions/math/prim_transcendental.go` | `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt`, `expt` |
| `extensions/math/prim_rounding.go` | `floor`, `ceiling`, `truncate`, `round`, `floor/`, `floor-quotient`, `floor-remainder`, `truncate/`, `truncate-quotient`, `truncate-remainder` |
| `extensions/math/prim_complex.go` | `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle` |
| `extensions/math/prim_rational.go` | `numerator`, `denominator`, `rationalize`, `exact-integer-sqrt`, `finite?`, `infinite?`, `nan?` |
| `extensions/math/prim_conversion.go` | `number->string`, `string->number` |
| `bootstrap_procedures.scm` | `zero?`, `positive?`, `negative?`, `square`, `exact-integer?` (Scheme, out of Go scope) |

## Finding H.1 — `quotient/remainder/modulo/gcd/lcm` declare `TypeInteger` but accept real-valued inputs

**Severity:** medium (Phase-2 time bomb). **Status:** fixed.

The registration file `registry/core/arithmetic.go:73` carried an explicit `TODO(Phase 4)` capturing this:

> TODO(Phase 4): quotient/remainder/modulo/gcd/lcm contracts declare TypeInteger, but implementations accept inexact integers (e.g., 7.0) via helpers.ExtractInteger. Before enabling runtime enforcement, widen to TypeNumber or introduce TypeIntegerValue.

### Reproducer

```scheme
(quotient 7.0 3.0)   ; => 2.0   (impl succeeds)
(remainder 7.0 3.0)  ; => 1.0
(modulo 7.0 3.0)     ; => 1.0
(gcd 12.0 8.0)       ; => 4.0
(lcm 4.0 6.0)        ; => 12.0
```

But `TypeInteger` in wile's vocabulary maps to `*values.Integer | *values.BigInteger` only (`values/value_type.go:190–200`). Once Extension Contracts Phase 2 wires `ParamTypes → SetValidator`, every call above is wrongly rejected.

### Impl's actual domain

`helpers.ExtractInteger` (`registry/helpers/integer.go:287`) accepts:

- `*values.Integer` (exact `int64`)
- `*values.BigInteger` (arbitrary-precision exact)
- `*values.Float` **if integer-valued** (e.g., `7.0` succeeds, `7.5` raises)

Rejects:

- `*values.BigFloat` (even integer-valued)
- Non-integer-valued `*values.Rational` (e.g., `7/3`; `6/2` auto-simplifies to `3` at eval time so passes)
- `*values.Complex` / `*values.BigComplex`

### Fix

Widened `ParamTypes` on `quotient`, `remainder`, `modulo`, `gcd`, `lcm` from `TypeInteger` to `TypeReal`. Same widening on their `ReturnType` since integer-valued Float input produces Float output.

Rationale for `TypeReal` over the TODO's suggested `TypeNumber`:

- `TypeReal` rejects Complex at the type level; impl's `ExtractInteger` rejects Complex at runtime. Same set.
- `TypeNumber` accepts Complex; would push Complex rejection to impl (one more runtime check for no benefit).
- Consistent with `abs`, `floor`, `ceiling`, `truncate`, `round` which also declare `TypeReal` (same domain shape).

`BigFloat` and non-integer `Rational` are still caught at impl level — that's a widening precision gap, but every `TypeConstraint`-based annotation has this property. The alternative (a new `TypeIntegerValue` that matches R7RS's `integer?` predicate) requires vocabulary extension and is deferred per `plans/2026-04-19-axis-b-inventory.md:566` §6 Option A.

## Not-findings (positive verification)

### Exactness contagion — correct

```scheme
(/ 1 3)     ; => 1/3            (exact + exact = exact rational)
(/ 7 3.0)   ; => 2.333...       (exact + inexact = inexact)
(max 1 2.0 3) ; => 3.0          (any inexact → inexact per R7RS §6.2.2)
(abs -1/2)  ; => 1/2            (exactness preserved)
(abs -1.5)  ; => 1.5
```

### Special values — correct

```scheme
(/ 1 0)     ; raises — exact division by zero is error per R7RS
(/ 1 0.0)   ; => +inf.0          (IEEE 754)
(finite? +inf.0)  ; => #f
(nan? +nan.0)     ; => #t
```

### Complex numbers — correct

- `(square 3+4i) → -7+24i` (complex closure under multiplication)
- `(magnitude 3+4i) → 5.0` (sqrt is inexact; R7RS permits)
- `(imag-part 5) → 0` (exact zero for real input)

### `gcd`/`lcm` identity elements — correct per R7RS §6.2.6

- `(gcd) → 0`
- `(lcm) → 1`

### Variadic arity — correct

- `(+)` → 0, `(*)` → 1 (identities per R7RS)
- `(-)` errors, `(/ )` errors (R7RS requires at least 1 arg)
- `(- 5)` → -5 (unary negation)
- `(/ 2)` → 1/2 (unary reciprocal)

### `exact-integer-sqrt` — correct multi-value return

```scheme
(call-with-values (lambda () (exact-integer-sqrt 10)) list)  ; => (3 1)
(exact-integer-sqrt -1)   ; raises (non-negative required)
(exact-integer-sqrt 1.0)  ; raises (TypeExactInteger enforced)
```

### `number->string` / `string->number` — correct

- Radix parameter is `TypeInteger` (2/8/10/16 enforced at impl level)
- `string->number` returns `TypeAny` because the union is `Number | #f`

### `floor-quotient` extended to reals (not a finding)

`(floor-quotient 7.5 3) → 2.0` works. R7RS §6.2.6 defines floor-quotient only for integers, but wile's ParamTypes declare `TypeReal` and the impl does real-valued floor-of-division. This is an R7RS-compatible extension: all R7RS-conforming programs work as specified, and non-integer inputs get a natural generalization.

## Phase 4 final scoreboard (all 9 categories)

| # | Category | Code | Doc | Cross |
|---|---|---|---|---|
| 1 | bytevectors | 2 | 0 | — |
| 2 | strings | 2 | 1 | — |
| 3 | ports | 0 | 2 | — |
| 4 | lists | 1 | 1 | — |
| 5 | characters | 2 | 0 | — |
| 6 | exceptions | 0 | 0 | 1 |
| 7 | control | 0 | 0 | — |
| 8 | records/promises | 0 | 0 | — |
| 9 | numbers | 1 | 0 | — |

**Final total: 8 code findings + 4 doc findings + 1 cross-category, across 9 categories.**

### Finding class distribution

| Class | Occurrences | Pattern |
|---|---|---|
| B.1 | 4 (bytevectors: `TypeByte`×3, numbers: `TypeInteger`×5 ≈ 2 distinct fixes) | Internal-type leak to user-facing API |
| B.5 | 1 (strings: `EqualFold` stale CLAUDE.md) | Docstring lie |
| C.1 | 2 (strings: `string-copy!`, `string-fill!`) | ParamCount < R7RS min arity |
| D.1/D.2 | 2 (ports CLAUDE.md stale) | Internal doc drift |
| E.1 | 1 (lists: `append` TypeList too strict) | Variadic rest type too narrow |
| E.2 | 1 (lists: pair/vector literal mutability) | Asymmetric by type (strings immutable, pairs/vectors not) |
| F.1/F.2 | 2 (characters: `char-alphabetic?`, `char-numeric?`) | Spec-enumerated Unicode categories missed |
| G.1 | 1 (exceptions→parser: mid-parse EOF) | Asymmetric reader behavior |
| H.1 | 1 (numbers: `TypeInteger` → `TypeReal` for 5 primitives) | Annotation narrower than impl domain |

### Hypothesis: **PR cleanup history predicts drift**

Four categories zeroed code findings. All four had dedicated recent cleanup PRs:

- **ports** — Phase 1 B.4 file-resolver extraction, recent embedded-stdlib work
- **exceptions** — Phase 1 A.2 error-type-identity
- **control** — PR #418 UNIFY-ESCAPE-MECHANISMS
- **records/promises** — PR #566 OpaqueValue

Categories with highest finding density had mixed legacy/recent code (bytevectors, characters, strings). Audit effectively **maps the post-cleanup drift-entropy gradient**.

### Meta-finding: what the full audit revealed about the type vocabulary

Every code finding except F.1/F.2 (Unicode category interpretation) was about **type-annotation precision**:

- B.1: user-facing API declares internal Go type (TypeByte)
- C.1: ParamCount doesn't match R7RS minimum arity
- E.1: TypeList for variadic rest where R7RS allows any
- H.1: TypeInteger where impl accepts integer-valued reals

The vocabulary gaps this audit surfaced match exactly what `plans/2026-04-19-axis-b-inventory.md:566` §6 predicted:

- **TypeMaybe(T)** would eliminate the "declared TypeAny because union of T | #f is unrepresentable" cases (12 primitives identified in axis-b).
- **TypeIntegerValue** (matching R7RS `integer?` predicate) would eliminate H.1's over-widening-to-TypeReal compromise.
- **TypeRecordTypeDescriptor** would close the `addRecords` TODO.

These are the three vocabulary extensions the audit empirically justifies, in rough priority order. Followup plan: new `plans/2026-04-20-type-constraint-vocabulary-extensions.md` (or similar) can cite this audit as evidence.

## Closing

Phase 4 finished. The audit plan's original scoping question (`plans/2026-04-19-primitive-annotation-audit.md` §7: "the audit measures how much of the primitive surface is describable in the current TypeConstraint vocabulary, and where are the gaps") is now answered empirically across 9 categories. The strategic output feeding Extension Contracts Phase 2 is:

1. **328 Single-bucket primitives** (from axis-b) + the 8 fixed in this Phase 4 + the 5 now consistently declared as `TypeReal` (H.1) form a validated baseline that can safely carry compile-time type checking.
2. Three `TypeConstraint` vocabulary extensions (`TypeMaybe`, `TypeIntegerValue`, `TypeRecordTypeDescriptor`) are justified by concrete customer counts from this audit.
3. Four R7RS deviations documented in `docs/reference/r7rs-differences.md` (char-ready?/u8-ready?, parameterize marks, set-current-directory!, literal mutability) plus the new #5 (mid-parse EOF) form the honest conformance statement.

No further Phase-4-axis-C work is scheduled.
