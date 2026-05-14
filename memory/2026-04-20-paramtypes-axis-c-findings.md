# Phase 5.E — ParamTypes Axis-C R7RS Sweep

**Parent**: `plans/2026-04-20-paramtypes-audit-design.md` §6.
**Purpose**: Compare impl accepted-domain (from 5.B inventory) against R7RS-small parameter-type specifications, category by category. Mirrors Phase 4 (axis-C over `ReturnType`) for the input side.
**Expected yield**: low, per design doc §6. The question is whether Wile's impl accepts a non-R7RS param domain (broader OR narrower) for any primitive whose name matches an R7RS-small primitive.

**Result**: 1 category with a pre-existing documented deviation, 0 new findings. The four-axis framework closes cleanly.

---

## 1. Method

1. For each R7RS-small category (§6.2–§6.13), cross-reference the primitives in `plans/2026-04-20-paramtypes-inventory.md` against R7RS param-type specifications.
2. For each matched primitive, compare:
   - The impl's accepted domain (scanned column of the inventory).
   - The R7RS specification's param-type clause.
3. Flag any divergence as a finding.

**What this sweep cannot catch** (deferred to runtime testing / property-based fuzz):

- *Within-type* constraints that no `ValueType` expresses (e.g., "exact non-negative integer" narrower than `TypeExactInteger`; "character that is a digit" narrower than `TypeCharacter`). The declaration layer operates on ValueTypes; sub-type domain invariants are enforced at runtime or not at all. Documenting each as a "finding" would produce dozens of entries with no actionable follow-up.
- *Variadic arity constraints* beyond count (e.g., "at least one argument must be a string"). Inventory §Variadic-rest tracks these as a separate concern.

---

## 2. Category sweep

Each row: R7RS section → representative primitives → any param-domain divergence.

| R7RS § | Category | Primitives (rep.) | Divergence? |
|---|---|---|---|
| §6.1 | Equality | `eq?`, `eqv?`, `equal?` | None — accept any, as R7RS specifies. |
| §6.2.6 | Integer division (quotient/remainder/modulo/gcd/lcm) | `quotient`, `remainder`, `modulo`, `gcd`, `lcm` | **Documented (Phase 4 H.1).** R7RS requires integer; impl accepts real-valued via `ExtractInteger`. Not a new finding; fix deferred pending `TypeIntegerValue` or widened declarations. |
| §6.2.6 | Numeric comparison | `=`, `<`, `>`, `<=`, `>=` | None — R7RS defines for reals; impl accepts reals (rejects complex via `isRealNumber` post-gate filter — same FP class as §1 of the 5.C sidecar). |
| §6.2.6 | Arithmetic | `+`, `-`, `*`, `/` | None — R7RS defines for numbers (including complex); impl accepts `Number`. |
| §6.2.6 | Numeric predicates | `number?`, `real?`, `integer?`, `zero?`, `positive?`, `negative?`, `odd?`, `even?` | None — R7RS defines as total predicates over any value; impl follows (see 5.C §2.B.2 caveat on not tightening predicate domains). |
| §6.2.6 | `abs` | `abs` | None — R7RS defines for reals; impl rejects complex via post-gate filter. Declared `TypeReal` is sound (5.C §1). |
| §6.2.6 | `make-rectangular`, `make-polar` | (ext math) | None — R7RS defines for reals; impl rejects complex via post-gate filter. Declared `TypeReal` is sound. |
| §6.2.6 | `exact`, `inexact`, `exact->inexact`, `inexact->exact` | (ext math) | None — R7RS defines for numbers; impl accepts `Number`. |
| §6.2.6 | `floor`, `ceiling`, `truncate`, `round` | (ext math) | None — R7RS defines for real numbers; impl accepts real via `extractReal`. |
| §6.2.7 | `number->string`, `string->number` | (ext math) | None — domain correct; radix subrange enforced at runtime. |
| §6.3 | Booleans | `not`, `boolean?`, `boolean=?` | None — R7RS total predicates. |
| §6.4 | Pairs/lists | `car`, `cdr`, `set-car!`, `set-cdr!`, `length`, `list-ref`, `list-tail`, `reverse`, `append`, `memq/memv/member`, `assq/assv/assoc` | None — param types match. `list-ref`/`list-tail` k param is declared `TypeExactInteger`; R7RS requires exact non-negative integer — the "non-negative" invariant is enforced at runtime (range check), not expressible in the current ValueType enum. Same class for `make-list`, `make-string`, `make-vector`, `make-bytevector`, `substring`, `string-copy!`, `vector-copy!`, `bytevector-copy!`. **Not flagged** — `TypeNonNegativeInteger` would need to be a new ValueType; tracked as future vocabulary work (5.C §2.A). |
| §6.5 | Symbols | `symbol?`, `symbol->string`, `string->symbol`, `symbol=?` | None. |
| §6.6 | Characters | `char?`, `char->integer`, `integer->char`, comparisons, case predicates, case mappers | None — `integer->char` k param R7RS requires "exact integer that is a Unicode scalar value"; codepoint-range check is at runtime (out of declaration scope). |
| §6.7 | Strings | `make-string`, `string`, `string-length`, `string-ref`, `string-set!`, `substring`, `string-copy`, `string->list`, `list->string`, comparisons | None — same non-negative-integer index class as §6.4. |
| §6.8 | Vectors | `vector`, `vector-length`, `vector-ref`, `vector-set!`, `vector-fill!`, `vector->list`, `list->vector`, `vector-copy` | None — same class. |
| §6.9 | Bytevectors | `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!`, `bytevector-copy`, `utf8->string`, `string->utf8` | None — `bytevector-u8-set!` byte param declared `TypeAny`, scanned as such. R7RS requires "exact integer in [0, 255]"; byte-range check is at runtime. Same class as index sub-domains. |
| §6.10 | Control | `procedure?`, `apply`, `call-with-current-continuation`, `values`, `call-with-values`, `dynamic-wind` | None — `apply` last-arg-must-be-list is expressed through variadic convention, not ValueTypes; runtime error on non-list. |
| §6.11 | Exceptions | `with-exception-handler`, `raise`, `raise-continuable`, `error`, `error-object?`, `error-object-message`, `error-object-irritants`, `read-error?`, `file-error?` | None in the axis-C sense. Error-object accessor primitives declare `TypeAny` — that's a vocabulary gap (5.C §2.A), not an R7RS mismatch. |
| §6.13.1 | Ports | `input-port?`, `output-port?`, `port?`, `eof-object?`, `close-port`, `close-input-port`, `close-output-port` | None. |
| §6.13.2 | Textual I/O | `read`, `write`, `display`, `newline`, `read-char`, `peek-char`, `write-char`, `read-line`, `write-string` | None — port-direction/textuality expressed via ValueType (sound). |
| §6.13.3 | Binary I/O | `read-u8`, `peek-u8`, `write-u8`, `read-bytevector`, `write-bytevector`, `get-output-bytevector` | None after 5.D (`get-output-bytevector` narrowed to `TypeBinaryOutputPort`). |

---

## 3. Consolidated findings

### 3.1 Re-affirmations (no new work)

- **Phase 4 H.1** (integer division coercion): `quotient`, `remainder`, `modulo`, `gcd`, `lcm` accept real-valued inputs like `7.0`. R7RS-nonconforming at the accepted-domain level. Recorded in `plans/2026-04-19-audit-findings-phase4-numbers.md`; the fix is declaration-widening (to `TypeReal` or a new `TypeIntegerValue`) plus impl narrowing at runtime. Deferred.
- **Sub-domain constraints** that R7RS specifies but no `ValueType` expresses (exact-non-negative-integer, exact-integer-in-range, Unicode-codepoint-integer, byte-range-integer) — ~25 primitives. Runtime-enforced; declaration-layer correct for what it can express. Candidates for a future refinement-type extension (e.g., `TypeSubrange(T, [lo, hi])`); not a separate type-vocabulary gap because the base type (`TypeExactInteger`, `TypeInteger`) is already correct.

### 3.2 No new findings

The 5.B inventory already surfaced every axis-C issue expressible within the current ValueType vocabulary. The axis-C sweep is closure — it confirms that the impl/axis-B matches do not hide non-R7RS acceptance patterns at the declaration level.

This is the design-doc-predicted outcome: *"Expected yield: low (R7RS has few unusual parameter-type requirements Wile's impl would violate)."*

---

## 4. The four-axis framework — closure

With 5.E complete, the four-axis audit framework is closed:

| Axis | Scope | Status | Sidecar |
|---|---|---|---|
| A | Docs ↔ `ReturnType` | Complete (Phase 1) | harness clean |
| B | `ReturnType` ↔ impl | Complete (Phase 3.D + 5 tightenings, PR #675) | `plans/2026-04-20-axis-b-annotation-bugs.md` |
| A' | Docs ↔ `ParamTypes` | Not separately swept — docs do not declare param types; ParamNames documentation is orthogonal. Covered implicitly by axis-B of ParamTypes. | — |
| B' | `ParamTypes` ↔ impl (per-slot) | Complete (5.B analyzer + 5.C sidecar + 5.D partial narrowing) | `plans/2026-04-20-paramtypes-annotation-bugs.md` |
| C | `ReturnType` ↔ R7RS | Complete (Phase 4, 9 categories) | `plans/2026-04-19-audit-findings-phase4-*.md` |
| C' | `ParamTypes` ↔ R7RS | Complete (this document) | `plans/2026-04-20-paramtypes-axis-c-findings.md` |

The audit is now a complete record of the primitive annotation layer's correctness. The remaining deltas between declared and actual accepted domains map entirely to **TypeConstraint vocabulary gaps** — not to annotation errors.

---

## 5. Follow-up decisions — unblocked by this closure

The combined evidence now supports a vocabulary-extension design conversation:

- ~28 return-side opaque-type gaps (axis-B Category C).
- ~85 param-side opaque-type gaps (axis-B' / 5.C §2.A).
- ~25 refinement-type candidates (axis-C' sub-domain constraints, §3.1 above).
- 13 union-bucket primitives (5.C §3) — **negative** evidence against `TypeUnion` introduction.

Design decision to be made in a separate plan: extend the ValueType enum with scalar entries per category vs. introduce a parametric `TypeOpaque[Go-type]` vs. introduce both plus `TypeSubrange` for refinement types. Cost profiles differ per §7.3 of the design doc.

**Status**: audit artifacts are complete. Vocabulary-extension design is the next piece of forward work, separately scoped.

---

## 6. Cross-references

- Phase 5 design: `plans/2026-04-20-paramtypes-audit-design.md` §6.
- Phase 5.A/B inventory: `plans/2026-04-20-paramtypes-inventory.md`.
- Phase 5.C sidecar: `plans/2026-04-20-paramtypes-annotation-bugs.md`.
- Phase 5.D narrowing: commit on `audit/paramtypes-sidecar` (`get-output-bytevector`).
- Axis-B Category C: `plans/2026-04-20-axis-b-annotation-bugs.md` §4.
- Phase 4 axis-C per-category findings: `plans/2026-04-19-audit-findings-phase4-*.md`.
- Extension Contracts Phase 2+: `plans/2026-03-26-extension-contracts-impl.md`.
