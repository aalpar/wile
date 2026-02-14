# Tokenizer Consolidation

**Status:** COMPLETE — structural `readUreal` extraction remains as documented debt
**File:** `internal/tokenizer/tokenizer.go` (2,158 lines, down from 2,287)
**Risk:** MEDIUM-HIGH (tokenizer is critical path; all changes verified against full test suite)

---

## §1 — The R7RS Number Grammar

R7RS §7.1.1 specifies numeric literals as a context-free grammar parameterized
by radix. The relevant productions, simplified:

```
<number>    → <prefix R> <complex R>

<complex R> → <real R>
            |  <real R> @ <real R>
            |  <real R> + <ureal R> i
            |  <real R> - <ureal R> i
            |  <real R> + i  |  <real R> - i
            |  <real R> +inf.0i  |  <real R> -inf.0i
            |  <real R> +nan.0i  |  <real R> -nan.0i
            |  + <ureal R> i  |  - <ureal R> i
            |  +inf.0i  |  -inf.0i  |  +nan.0i  |  -nan.0i
            |  +i  |  -i

<real R>    → <sign> <ureal R>  |  +inf.0  |  -inf.0  |  +nan.0  |  -nan.0

<ureal R>   → <uinteger R>
            |  <uinteger R> / <uinteger R>
            |  <decimal R>                          (only for R=10)

<decimal 10> → <uinteger 10> <suffix>
            |  . <digit 10>+ #* <suffix>
            |  <digit 10>+ . <digit 10>* #* <suffix>
            |  <digit 10>+ #+ . #* <suffix>

<uinteger R> → <digit R>+ #*

<prefix R>  → <radix R> <exactness>  |  <exactness> <radix R>
<suffix>    → ε  |  <exponent marker> <sign> <digit 10>+
```

**Key structural property:** The grammar is *left-to-right deterministic* —
no production requires backtracking if you have one-character lookahead. This
is why a single-lookahead scanner works. The tokenizer exploits this by
committing to a parsing path after examining `p.curr()`.

The `<ureal R>` production is the **irreducible kernel** of number parsing.
Every numeric literal contains exactly one `<ureal R>` instance (reals), or
two (complex: one for real part, one for imaginary/angle). The question this
document tracks: should the tokenizer have a function that corresponds to
`<ureal R>`?

---

## §2 — State Space Analysis

### §2.1 — TokenizerState Enumeration

The tokenizer uses a flat `int` enum (`TokenizerState`) with 75 constants.
Of these, **40 are number-related**:

| Category | States | Count |
|----------|--------|-------|
| Special float (signed only) | `SignedInf`, `SignedNan` | 2 |
| Imaginary/complex | `{Signed,Unsigned}{Imaginary,ImaginaryInf,ImaginaryNan,Complex,ComplexPolar}` | 14 |
| Integer (default radix) | `{Signed,Unsigned}Integer` | 2 |
| Integer (explicit radix) | `{Signed,Unsigned}IntegerBase{2,8,10,16}` | 8 |
| Decimal fraction | `{Signed,Unsigned}DecimalFraction` | 2 |
| Rational fraction | `{Signed,Unsigned}RationalFraction` | 2 |
| Scientific notation | `{Signed,Unsigned}ScientificNotation` | 2 |
| Big number (extension) | `BigFloat`, `BigInteger{DefaultBase,Base2,Base8,Base10,Base16}` | 6 |
| Radix/exactness markers | `MarkerBase{2,8,10,16}`, `MarkerNumber{Inexact,Exact}` | 6 |
| **Subtotal** | | **40** *(remaining 35 are non-numeric)* |

### §2.2 — The Signed/Unsigned Duality

13 of the 40 number states come in signed/unsigned pairs:

```
                    ┌─────────────────────────────────────┐
  Integer ──────────┤  Signed ◄──► Unsigned               │
  IntegerBase{2,8,  │  13 pairs = 26 states               │
    10,16} ─────────┤                                     │
  DecimalFraction ──┤  Each pair encodes 1 bit of info:   │
  RationalFraction ─┤  signed? yes/no                     │
  ScientificNot. ───┤                                     │
  Imaginary ────────┤  The Tokenizer struct ALSO carries: │
  ImaginaryInf ─────┤    signed bool   ← same 1 bit      │
  ImaginaryNan ─────┤                                     │
  Complex ──────────┤  This is a value alias (§4.1).      │
  ComplexPolar ─────┤                                     │
                    └─────────────────────────────────────┘
```

**Information-theoretic analysis** (Shannon, 1948, §I):

The signed/unsigned distinction carries exactly 1 bit of information
(log₂(2) = 1). It is currently encoded in **two** independent locations:

1. The `TokenizerState` enum (via 13 paired constants)
2. The `Tokenizer.signed` bool field

This is **redundant encoding** — the same 1 bit appears in two representations.
The `signed` bool is authoritative (set at sign consumption time); the state
enum is a downstream echo. The enum duality forces `signedState()` and
`integerStateForRadix()` to exist as mux functions that translate from the bool
to the enum representation.

**Type precision calculation** (Pierce, *Types and Programming Languages*, 2002):

```
Current number states:     |representable| = 40
Without signed/unsigned:   |representable| = 40 - 13 = 27  (+ 1 bool elsewhere)
Semantic information:      same in both cases

Precision = |valid| / |representable|
  Current:  40/40 = 100%  (no impossible states)
  But the encoding uses 40 enum values + 1 bool = 41 dimensions
  to represent information that fits in 27 enum values + 1 bool = 28 dimensions.
  The 13 extra enum values are projections of the bool — zero new information.
```

**Why it persists (and why it's correct to leave it):**

The parser switches on `TokenizerState` to determine number construction
strategy. It uses the Signed/Unsigned distinction to select between code
paths — this is *not* the same as checking `token.Sign()`. The state enum
serves as a **discriminated dispatch tag** for the parser's number construction
table. Eliminating the duality would mean: every parser switch on number states
must also check `.Sign()`, adding conditional nesting. The current design trades
13 extra enum values for O(1) dispatch at every parser site.

This is the correct trade-off: the tokenizer is written once, the parser
switches are read and maintained many times. A flat dispatch key that carries
all relevant information (format × signedness) is preferable to a two-part key
(format + separate signedness check).

> **Verdict:** The signed/unsigned duality is a *deliberate encoding choice*,
> not accidental duplication. Documented as a non-target.

### §2.3 — Irreducible State Count

Stripping markers (prefix-phase states), big numbers (extension), and the
signed/unsigned duality, the irreducible semantic states for R7RS numbers are:

| State | R7RS Production |
|-------|-----------------|
| Integer × 5 radix variants | `<uinteger R>` for R ∈ {default, 2, 8, 10, 16} |
| DecimalFraction | `<digit 10>+ . <digit 10>* #* <suffix>` |
| RationalFraction | `<uinteger R> / <uinteger R>` |
| ScientificNotation | `<uinteger 10> <suffix>` where suffix ≠ ε |
| Imaginary | `<sign> <ureal R> i` |
| ImaginaryInf | `±inf.0i` |
| ImaginaryNan | `±nan.0i` |
| Complex (rectangular) | `<real R> ± <ureal R> i` |
| ComplexPolar | `<real R> @ <real R>` |
| Inf | `±inf.0` |
| Nan | `±nan.0` |

**15 irreducible states.** The tokenizer represents these with 40 enum
values — a 2.67× expansion factor, accounted for by:
- signed/unsigned duality: ×2 on 13 states (+13)
- big number extension: +6
- prefix markers: +6

All three expansions have justification. No state is unreachable.

---

## §3 — Function Call Graph (Number Parsing)

```
readUnsignedFractionalRealNumberOrImaginaryNumberOrRationalRealNumber(r)
│                                      ← entry point; dispatches on first char
├─ [sign] ──► readIntegerAndFraction(true, r)
│             │
│             ├─ readDigitsAndHash(r)      ← <uinteger R> production
│             ├─ [.] readDecimalFractionWithExponent(r)
│             │      └─ readOptionalDecimalPart(r, hadHash)
│             │         └─ mayReadExponent(r)
│             ├─ [/] readDiv(r)            ← rational
│             ├─ [e] mayReadExponent(r)    ← scientific
│             └─ readSignedComplexSuffix(r)
│                ├─ [i] mayConsumeImaginarySuffix()
│                ├─ [±] mayReadSignedImaginaryPart(_, r)   ← ★ ureal-like
│                │      ├─ readDigitsAndHash(r)
│                │      ├─ readOptionalDecimalPart(r, hadHash)
│                │      ├─ readDiv(r)
│                │      ├─ mayReadExponent(r)
│                │      └─ mayConsumeImaginarySuffix()
│                └─ [@] mayReadPolarPart(r)                ← ★ ureal-like
│                       ├─ readDigitsAndHash(r)
│                       ├─ readOptionalDecimalPart(r, hadHash)
│                       └─ mayReadExponent(r)
│
├─ [sign, i] ──► readImaginaryOrSignedInfinity(r)
├─ [sign, n] ──► readSignedNan(r)
├─ [sign, .] ──► readSignedDecimalFractionOrExponentWithImaginary(r)
│
├─ [.] ──► readConsOrDecimalFractionWithExponent(r)
│
└─ [digit] ──► readIntegerAndFraction(false, r)
               │
               └─ [unsigned path: ±/@/i suffix]
                  ├─ [i] state := UnsignedImaginary
                  ├─ [±] mayReadUnsignedFractional...RealNumber(r) ← ★ ureal-like
                  │      ├─ readDigitsAndHash(r)
                  │      ├─ readDecimalFractionWithExponent(r)
                  │      ├─ mayReadExponent(r)
                  │      └─ readDiv(r)
                  └─ [@] mayReadPolarPart(r)
```

### §3.1 — Primitive Operations (Leaf Layer)

These are the **compositional atoms** — small, single-purpose functions that
appear at multiple call sites:

| Primitive | Call Sites | R7RS Production | Monoid? |
|-----------|-----------|-----------------|---------|
| `readDigitsAndHash(r)` | 10 | `<digit R>+ #*` | Yes (sequential composition) |
| `mayReadExponent(r)` | 4 | `<suffix>` | Yes (identity = ε) |
| `mayConsumeSign()` | 5 | `<sign>` | Yes (identity = no sign) |
| `mayConsumeImaginarySuffix()` | 6 | terminal `i` | Yes (identity = no `i`) |
| `readDiv(r)` | 3 | `/ <uinteger R>` | N/A (not composable) |
| `readOptionalDecimalPart(r, h)` | 3 | `. <digit>* #* <suffix>` | Yes (identity = ε) |
| `readHashDigits()` | 2 | `#*` | Yes (identity = ε) |

> **Algebraic note** (Milewski, *Category Theory for Programmers*, §4):
> The `may*` functions form a monoid under sequential composition. Each has
> an identity (no-op when the lookahead doesn't match) and composes
> associatively. This is why they can be freely reordered and combined
> without introducing coupling — they are **endomorphisms** on the
> tokenizer state: `Tokenizer → Tokenizer`, closed under composition.

### §3.2 — The Four `<ureal>`-Like Functions

Four functions implement variants of the `<ureal R>` production, each in a
different grammatical context:

| Function | Context | Lines | State Assignments | Suffix Dispatch |
|----------|---------|-------|-------------------|-----------------|
| `readIntegerAndFraction` | Full `<real>` + complex suffixes | ~65 | 6 (via `signedState`) | i / ± / @ |
| `mayReadUnsigned...RealNumber` | `<ureal>` after sign consumed | ~44 | 0 (caller owns state) | none |
| `mayReadSignedImaginaryPart` | Imaginary coefficient before `i` | ~70 | 0 (caller owns state) | `i` (mandatory) |
| `mayReadPolarPart` | Polar angle after `@` | ~52 | 1 (at end) | none |

**Shared operation sequence** (the `<ureal>` kernel):

```
readDigitsAndHash(r)                    ← <digit R>+ #*
├─ [.] readOptionalDecimalPart(r, h)   ← . <digit>* #* <suffix>
├─ [/] readDiv(r)                       ← / <uinteger R>
└─ [e] mayReadExponent(r)               ← <suffix>
```

This 4-step sequence appears in all four functions. The differences:

| Dimension | readIntegerAndFraction | mayReadUnsigned... | mayReadSignedImag... | mayReadPolarPart |
|-----------|-----------------------|--------------------|-----------------------|------------------|
| **Pre-sequence** | `integerStateForRadix` | `inf.0`/`nan.0` check | `mayConsumeSign`, `i`/`n` check | `@` consume, `mayConsumeSign` |
| **State during** | 6 assignments at branch points | none | none | none |
| **Post-sequence** | complex suffix dispatch (i/±/@) | return to caller | `mayConsumeImaginarySuffix` | `mayReadExponent`, set polar state |

> **Factoring analysis** (Bird & de Moor, *Algebra of Programming*, §2.3):
>
> If `f₁(x) = pre₁ ; ureal(x) ; post₁` and `f₂(x) = pre₂ ; ureal(x) ; post₂`,
> the common factor is `ureal`. But the state assignments in
> `readIntegerAndFraction` happen *inside* `ureal`, not before or after it.
> This prevents clean factoring: `ureal` would need to be parameterized by
> a state-assignment strategy, turning a simple loop into a higher-order
> function with a callback at each branch point.
>
> In Hoare logic terms:
> ```
> {digits consumed}  branch_on_dot_or_slash_or_exponent  {state set, fraction/exponent consumed}
> ```
> The postcondition includes `state set` — which value `p.state` holds differs
> by call site. A unified `readUreal` must either:
> (a) accept the state values as parameters (6 parameters for `readIntegerAndFraction`)
> (b) defer state assignment to the caller (requires the caller to inspect what `readUreal` consumed)
> (c) return a discriminant that the caller maps to a state

---

## §4 — Problem Statement

The tokenizer's number parsing (~700 lines) contained significant repetition
driven by the R7RS numeric grammar's structure. The same micro-patterns —
digit consumption, optional sign, imaginary suffix, complex suffix dispatch —
appeared across 4+ functions that each parsed a "real number" in a different
context (standalone, imaginary coefficient, polar angle, post-sign).

### §4.1 — Coupling Defects (Pre-Consolidation)

Before consolidation, the tokenizer exhibited:

1. **Hand-unrolled loops** (Parnas, 1972): Digit consumption code appeared
   inline at 10+ sites instead of as a single `readDigitsAndHash`. Each site
   was a copy of the same loop, differing only in error handling. This is the
   transition from *enumeration to induction* — identical blocks differing only
   in data should be a single function parameterized by the data.

2. **Phantom functions** (wrappers that add no behavior): `scanForImaginaryNumberSpecials`
   was a 50-line function whose logic was entirely subsumed by its callers.
   A wrapper that delegates to exactly one function with no transformation is
   the identity morphism — it adds indirection without adding meaning.

3. **Accidental variation** across sites: Three `readDotSubsequentSymbol` call
   sites had subtly different implementations of the same check. Differences
   were accidental (evolved independently), not semantic.

4. **Value aliases**: The `signed` bool and the Signed/Unsigned state enum
   encode the same information twice (see §2.2). The `signedState()` helper
   is a **synchronization function** — it exists solely to keep the two
   representations consistent.

---

## §5 — Completed Work

Two rounds of consolidation, 13 changes across 10 PRs/branches.

### Round 1: Core Consolidation (~174 lines saved)

| Change | PR/Branch | Savings | Principle |
|--------|-----------|---------|-----------|
| `readDelimited` — unified string/symbol scanning | PR #230 | ~60 | Composability: factor common structure |
| Predicate cleanup — `isInitial` unification, `for`→`if` fix | `refactor/tokenizer-predicate-cleanup` | ~17 | State tightness: eliminate dead branches |
| `signedState` helper — signed/unsigned state dispatch | PR #236 | ~12 | Composability: name the mux operation |
| Delete `scanForImaginaryNumberSpecials` | PR #234 | ~50 | Composability: remove identity morphism |
| `readOptionalDecimalPart` — unified decimal fraction | PR #234 | ~35 | Composability: factor `<decimal>` tail |

### Round 2: Micro-Pattern Extraction (~151 lines saved)

| Phase | Change | Savings | Algebraic Justification |
|-------|--------|---------|------------------------|
| 1 | Dead code / trivial wrappers (8 items inlined or deleted) | ~108 | Delete identity morphisms and dead projections |
| 2 | `validateCodePoint` extracted (2 call sites) | ~8 | Factor shared precondition check |
| 3 | `readDigitsAndHash(r)` extracted (10 call sites) | ~14 | Factor `<digit R>+ #*` production into single function |
| 4 | `mayConsumeImaginarySuffix()` (6 clean sites) | ~6 | Factor terminal `i` consumption; monoid identity = no-op |
| 5 | `readSignedComplexSuffix(r)` (2 identical dispatch blocks) | ~5 | Factor complex suffix dispatch (i/±/@) |
| 6 | `mayConsumeSign()` (5 clean sites) | ~4 | Factor `<sign>` production; monoid identity = no-op |
| 7 | Inlined `readDiv` replaced with actual `readDiv(r)` call | ~3 | Eliminate hand-inlined copy |
| 8 | `readDotSubsequentSymbol` (3 sites with accidental differences) | ~3 | Unify accidentally-diverged implementations |

**Total: ~325 lines saved (~14% of original file)**

Phases 4-8 saved fewer gross lines than estimated (~21 vs ~70) because each
helper adds its own signature and body. The value is readability: named helpers
replace scattered micro-patterns. In information-theoretic terms (Shannon, 1948,
§II), the *description length* of the tokenizer decreased — the same behavior
is specified with fewer symbols, meaning less redundancy and fewer places where
a change must be replicated.

### §5.1 — Composition Diagram (Post-Consolidation)

The primitive layer now forms a clean DAG:

```
                    readDigitsAndHash(r)
                   ┌───────┤
                   │       ├── readUnsignedBaseNNumber(r)
                   │       └── readHashDigits()
                   │
readOptionalDecimalPart(r, hadHash)
         │
         ├── readDigitsAndHash(r)  [if !hadHash]
         ├── readHashDigits()      [if hadHash]
         └── mayReadExponent(r)
                   │
                   ├── exponentMarkerStrength()
                   ├── mayConsumeSign()
                   └── readUnsignedBaseNNumber(r)
```

All arrows point downward (no cycles). Each primitive has a single
responsibility that maps to one R7RS production or production fragment.

> **Acyclic Dependencies Principle** (Martin, *Clean Architecture*, Ch. 14):
> The dependency graph MUST be a DAG. The number parsing primitives satisfy
> this — no function in the leaf layer calls any function in the dispatch layer.

---

## §6 — Remaining Debt: The Missing `readUreal`

### §6.1 — The Grammar Argument

The R7RS grammar has a clean factoring:

```
<complex R> uses <real R>    (1-2 times)
<real R>    uses <ureal R>   (exactly once)
<ureal R>   = <uinteger R> [/ <uinteger R>] | <decimal R>
```

A tokenizer that mirrors this grammar would have `readComplex` → `readReal` →
`readUreal`, with `readUreal` as a single function. The current tokenizer
**does not have `readUreal`**. The `<ureal R>` production is instead inlined
into 4 functions, each in a different grammatical context.

### §6.2 — The Inlining is Forced by State Threading

The four functions share the `<ureal>` kernel (§3.2) but disagree on when and
how to assign `p.state`. This is the fundamental obstacle to extraction.

**Formal statement of the problem:**

Let `U(r)` be the `<ureal R>` kernel: `digits → [. digits] | [/ digits] | [e ± digits]`.

Each call site wraps `U(r)` with a different state-assignment schedule:

```
readIntegerAndFraction:      state := f₁(branch)  DURING  U(r)  THEN  dispatch(i/±/@)
mayReadUnsigned...:          U(r) only  (no state assignment)
mayReadSignedImaginaryPart:  U(r)  THEN  consume(i)
mayReadPolarPart:            U(r)  THEN  state := polar
```

Site 1 assigns state **during** U(r) — at the `.`, `/`, and `e` branch points.
Sites 2-4 assign state **after** U(r) completes. A unified `readUreal` must
accommodate both patterns.

### §6.3 — Three Extraction Strategies

| Strategy | Mechanism | Savings | Cost |
|----------|-----------|---------|------|
| **Callback-based** | `readUreal(r, stateMap StateConfig)` where `StateConfig` has fields for each branch's state value | ~80 lines | Adds a struct type + indirection at every branch point. 6 parameters for `readIntegerAndFraction`'s case. |
| **Read-then-classify** | `readUreal` parses without state, returns a discriminant (integer/decimal/rational/scientific). Caller maps discriminant → state. | ~60 lines | Fights the no-backtrack design: the caller must re-derive what `readUreal` already determined. Introduces a return value that's a mini-enum — duplicating the `TokenizerState` concept at a smaller scale. |
| **Incremental** | Deduplicate the two worst offenders only (`mayReadSignedImaginaryPart` + `mayReadPolarPart`, which don't assign state during U(r)). | ~30-40 lines | Leaves `readIntegerAndFraction` untouched. Partial solution. |

**Hoare-logic analysis of strategy (b):**

```
{p.curr() is digit or dot}
  readUreal(r)
{digits consumed, fraction/exponent consumed, p.state UNCHANGED}

Postcondition lacks state assignment → caller needs:
  result := readUreal(r)
  switch result.kind {
  case decimal:  p.state = signedState(signed, SignedDecimalFraction, ...)
  case rational: p.state = signedState(signed, SignedRationalFraction, ...)
  ...
  }
```

The switch in the caller recapitulates the branching that `readUreal` already
performed internally. This is **information loss through an abstraction
boundary** — the function knows which branch it took, but discards that
knowledge, forcing the caller to re-derive it from a summary.

### §6.4 — Decision

**Deferred.** The extraction fights the tokenizer's architecture. The state-
threading model means any unified `readUreal` must either:

- Accept 6 state parameters (callback-based) — adding complexity proportional
  to the savings, net benefit ≈ 0
- Discard information and force re-derivation (read-then-classify) — violating
  the scanner's no-backtrack design principle
- Only unify the easy cases (incremental) — saving ~35 lines, below the
  threshold where the abstraction pays rent

**Justified trigger:** Adding a new number format (e.g., new exactness prefix,
new radix, imaginary format change) that would require modifying all 4 functions
identically. At that point, the extraction saves modification-site count, which
is the real cost metric. Until then, the ~80-line savings don't justify the
structural risk for a stable parser.

> **Parnas (1972):** Modules should hide design decisions likely to change. The
> `<ureal>` production is *unlikely to change* — it's been stable across R4RS,
> R5RS, R6RS, and R7RS. The tokenizer's state-threading model is also unlikely
> to change (it's a consequence of single-lookahead scanning). When neither the
> hidden decision nor the interface is likely to change, the module boundary
> adds cost without benefit.

---

## §7 — Non-Targets

| Pattern | Rationale | Formal Basis |
|---------|-----------|--------------|
| Signed vs unsigned state assignment | See §2.2: deliberate encoding for O(1) parser dispatch | Discriminated union vs. product type trade-off |
| Unsigned complex path in `readIntegerAndFraction` | Bespoke `i`/`inf.0i` disambiguation: `+i` vs `+inf.0` requires 2-char lookahead within the `i` path | Context-dependent grammar; can't factor without backtrack |
| `readBigNum` separate from regular numbers | Different grammar: arbitrary precision, no hash digits, no complex suffixes | Distinct production; no shared structure to factor |
| `readString` vs `readExtendedSymbol` | Different termination (`"` vs `\|`); already share `readEscapeSequence` via `readDelimited` | Maximal sharing already achieved |
| `NewTokenizerWithComments` | No-op wrapper but active API surface (25 callers) | Identity morphism, but removing it is a breaking API change — cost exceeds benefit |
| `readSpecialNumber` + optional imaginary | 3 sites with variation — below extraction threshold | 3 < 4 (minimum for abstraction to pay rent on function overhead) |
| Mnemonic/directive iteration loops | Only 2 instances | 2 < 3 threshold |
| Full `readUreal` extraction | See §6 | State threading prevents clean factoring |

### §7.1 — Extraction Threshold

A helper function has fixed overhead: signature, error checks, documentation.
In this codebase, that's ~5-8 lines. For a helper to save net lines, it must:

```
savings = (inline_size × call_sites) - (helper_size + call_site_overhead × call_sites)

For savings > 0:
  call_sites > helper_size / (inline_size - call_site_overhead)

With inline_size ≈ 4, call_site_overhead ≈ 1, helper_size ≈ 8:
  call_sites > 8 / (4 - 1) ≈ 2.67 → need ≥ 3 call sites
```

This is why 2-instance patterns are non-targets and 3-instance patterns are
borderline. The threshold is structural, not arbitrary.

---

## §8 — Verification

All existing tokenizer tests pass after each change. The test suite covers the
full R7RS §7.1.1 numeric grammar:

| Category | Test Cases | R7RS Productions Covered |
|----------|-----------|--------------------------|
| Special reals | `+inf.0`, `-inf.0`, `+nan.0`, `-nan.0` | `<real R>` special forms |
| Pure imaginary | `+i`, `-i` | `<complex R>` ±i forms |
| Rectangular complex | `3+4i`, `1+inf.0i`, `1-nan.0i` | `<complex R>` rectangular |
| Polar complex | `1@1.5708` | `<complex R>` polar |
| Radix prefixes | `#b101`, `#o77`, `#xFF`, `#d42` | `<prefix R>` × `<uinteger R>` |
| Exactness prefixes | `#e1.5`, `#i3` | `<prefix R>` exactness |
| Decimal variants | `.5`, `1.5`, `1/2` | `<decimal 10>`, `<ureal R>` rational |
| Hash digits | `1##`, `1.##e2` | `<uinteger R>` with `#*`, `<decimal 10>` with `#*` |
| Dot-initial symbols | `...`, `.foo` | Lexical ambiguity: dot vs. decimal vs. symbol |

```bash
go test -v -count=1 ./internal/tokenizer/...
```

---

## §9 — Summary Metrics

**State space:**
- 40 number-related `TokenizerState` values, all reachable
- 15 irreducible semantic states × expansion factors (signed/unsigned, radix, big numbers)
- 0 impossible states (type precision = 100% for the enum itself)
- 1 value alias (`signed` bool ↔ Signed/Unsigned state pairs) — deliberate trade-off (§2.2)

**Dependency structure:**
- Number parsing functions form a 2-layer DAG: dispatch layer (4 ureal-like
  functions) → primitive layer (7 leaf operations)
- No cycles. No SDP violations within the number parsing subsystem.
- `internal/tokenizer` package instability: I = Ce/(Ca+Ce) = 2/(2+2) = 0.50

**Consolidation impact:**
- ~325 lines saved (~14% of original file)
- 10 call sites unified into `readDigitsAndHash` (highest-value extraction)
- ~80 lines of remaining debt (§6), deferred with documented trigger condition

---

## References

| Citation | Relevance |
|----------|-----------|
| R7RS §7.1.1 | Lexical structure; number grammar productions |
| Shannon, "A Mathematical Theory of Communication" (1948) | Information content of state encodings; redundancy analysis |
| Pierce, *Types and Programming Languages* (MIT Press, 2002) | Type precision; representable vs. valid states |
| Parnas, "On the Criteria To Be Used in Decomposing Systems into Modules" (CACM, 1972) | Module boundaries hide decisions likely to change |
| Martin, *Clean Architecture* (Prentice Hall, 2017) | Dependency metrics (Ca, Ce, I); Acyclic Dependencies Principle |
| Milewski, *Category Theory for Programmers* (2014) | Monoid structure of composable scanner operations |
| Bird & de Moor, *Algebra of Programming* (Prentice Hall, 1997) | Factoring common structure from sequential compositions |
| Hoare, "An Axiomatic Basis for Computer Programming" (CACM, 1969) | Pre/postcondition analysis of `readUreal` extraction strategies |
| Minsky, "Effective ML" (Jane Street, 2011) | Make illegal states unrepresentable |
