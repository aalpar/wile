# Phase 5.C — ParamTypes Annotation Bug Sidecar

**Parent**: `plans/2026-04-20-paramtypes-audit-design.md` §4.
**Source**: `plans/2026-04-20-paramtypes-inventory.md` (484 primitives, 476 declared slots).
**Purpose**: Prioritized bug list for Phase 5.D cleanup PR. Classifies inventory findings into:

1. **Real bugs**, ordered by severity under Extension Contracts Phase-2 runtime enforcement.
2. **Analyzer false positives** with evidence, so they're not re-reported.
3. **Non-bugs requiring type-system extension**, cross-referenced with Phase 3 Category C.

---

## Headline

| Bucket | Count | Actionable in 5.D | Blocked on TypeConstraint vocabulary |
|---|---|---|---|
| Declared-too-narrow | 3 | **0** (all FP) | 0 |
| Declared-too-wide | 107 | **0–1** (see §2.B) | ~85 (TypeConstraint gaps) + ~21 (analyzer artifacts/semantic) |
| Union | 13 | **0** | 13 (TypeUnion extension) |
| Unguarded | 79 | **0** (analyzer limitation) | 0 |
| Variadic-rest | 93 | (separate track, see §6) | — |

**Net for 5.D**: zero mechanical fixes that *fully* narrow a declared type to its impl's accepted domain using types in the current 28-entry enum. One candidate (`get-output-bytevector`) offers a *partial* narrowing (`TypeOutputPort` → `TypeBinaryOutputPort`) that catches a class of misuse earlier without making the declaration exact. Every other candidate turned out on closer inspection to be one of:

- A predicate over all values (R7RS domain is `TypeAny`) — keep `TypeAny`.
- An analyzer mis-labeling of `values.Tuple` assertions as "pair" (the impl accepts `Pair ∪ EmptyList` = `TypeList`; declared `TypeList` is correct) — fix is in the analyzer, not the primitive.
- A TypeConstraint vocabulary gap — the required Go type has no enum entry.

The design doc extrapolated 30–80 fixes from Phase 4 H.1's arithmetic cluster, but that class (declared-too-narrow under a coercing extractor) did not generalize — the declared-too-narrow bucket is empty, and 80%+ of declared-too-wide findings point at *TypeConstraint vocabulary* gaps (Box, Promise, Record, SyntaxSymbol, and all concurrency types lack enum entries), not at incorrect annotations.

The load-bearing-under-Phase-2 category (declared-too-narrow) is empty. The audit's primary safety concern — declared types rejecting valid calls — does not manifest in the current codebase.

**Revised framing**: Phase 5.D as a "mechanical cleanup PR" is largely obviated. The audit confirms the annotation system is consistent with its design *within its current vocabulary*. The bulk of the inventory signal feeds the *TypeConstraint vocabulary extension* decision (design doc §7.3, currently deferred), not a mechanical cleanup.

---

## 1. Declared-too-narrow — all false positives (3 entries, 0 bugs)

The analyzer sees only the outer type gate (`RequireArg[Number]` or type-assertion to `Number`); it does not trace post-gate in-body filters. All three entries use a `RequireArg[Number]`-or-equivalent gate followed by an `isRealNumber`-style rejection that narrows the effective accepted domain to real. **Declared `TypeReal` matches the effective domain. No fix needed.**

| Primitive | Slot | Gate | Post-gate filter | Verdict |
|---|---|---|---|---|
| `abs` | 0 | `RequireArg[values.Number]` at `registry/core/prim_arithmetic.go:177` | `if isComplex { return ErrNotAReal }` at `:183` | FP — declared `TypeReal` is sound |
| `make-polar` | 0 | `r.(values.Number)` assertion at `extensions/math/prim_complex.go:145` | `if !isRealNumber(r) { return ErrNotANumber }` at `:146` | FP — declared `TypeReal` is sound |
| `make-polar` | 1 | `theta.(values.Number)` at `:150` | `if !isRealNumber(theta)` at `:151` | FP — declared `TypeReal` is sound |

**Follow-up (optional, analyzer-only)**: if the v1 analyzer is ever extended to handle post-gate filters (a small interprocedural SSA pass over the first dozen lines of each primitive body), these three entries will move to Single-strict / Single-coercing automatically. No action in 5.D.

---

## 2. Declared-too-wide — 107 entries, 4 sub-buckets

### 2.A TypeConstraint vocabulary gaps — not actionable in 5.D (~85 entries)

Primitives declared `TypeAny` because the type they require has no `ValueType` enum entry in `values/value_type.go`. The enum currently holds 28 entries (see `value_type.go:52–81`); notably **missing** are the following types that do exist as Go types in `values/` but have no `TypeConstraint` spelling:

**Missing enum entries (by cluster):**

| Go type(s) | Scheme concept | Affected primitives |
|---|---|---|
| `*values.Box` | box | `set-box!` slot 0, `unbox` slot 0 |
| `*values.Promise` | promise | `make-promise` slot 0 |
| `*values.Record`, `*values.RecordType` | record, record-type | `record-predicate`, `record-type`, `make-promise` |
| `*values.OpaqueValue` | opaque | `opaque-tag` slot 0 |
| `*values.NativeError` | error-object | `error-object-*` (5), `error-object?`, `read-error?`, `file-error?` |
| `*syntax.SyntaxSymbol`, `syntax.SyntaxValue` | syntax object, identifier | `bound-identifier=?` ×2, `free-identifier=?` ×2, `identifier?`, `datum->syntax` slot 0, `syntax->datum`, `compile` |
| `*machine.PromptTag` | continuation-prompt-tag | `abort-current-continuation`, `continuation-prompt-available?` |
| `*machine.ErrorContext` | error-context | `error-context-marks`, `error-context-source`, `error-context-stack-trace` |
| `*values.AtomicBox` | atomic | `atomic-compare-and-swap!`, `atomic-load`, `atomic-store!`, `atomic-swap!` |
| `*values.Channel` | channel | 8 primitives |
| `*values.Once` | once | `once-do!`, `once-done?` |
| `*values.RWMutex` | rw-mutex | 6 primitives |
| `*values.WaitGroup` | wait-group | 3 primitives |
| `*values.Mutex` | mutex | 6 primitives |
| `*values.ConditionVariable` | condition-variable | 5 primitives |
| `*values.Thread` | thread | 6 primitives |
| `*values.Time` | time | `time->seconds` |
| `*values.Process` | process | 5 primitives, `process?` |

**This cluster corresponds to** Phase 3 Category C (`plans/2026-04-20-axis-b-annotation-bugs.md` §4), now with param-side evidence. Axis-B's 28 scalar return-type gaps and axis-A's ~85 `TypeAny`-declared opaque-type param gaps are **the same underlying TypeConstraint vocabulary deficit**, viewed from different ends of the primitive.

**Action for 5.D**: none. Cross-reference this section from the axis-B sidecar and from `plans/CLAUDE.md`. Vocabulary extension is §7.3 deferred work and should be driven by a separate design decision.

### 2.B Candidate tightenings — 1 partial, all others rejected on inspection

Verified candidates whose required type *appears* to have a `ValueType` enum entry, with per-entry findings:

| Primitive | Slot | Current | Impl's actual gate | Verdict |
|---|---|---|---|---|
| `pair?` | 0 | `TypeAny` | Type-switch; returns `#f` on non-pair | **Keep `TypeAny`** — predicate domain is all values per R7RS. |
| `generate-temporaries` | 0 | `TypeAny` | `arg.(values.Tuple)` at `registry/core/prim_syntax.go:131` | **Keep or → `TypeList`**. Accepts `Tuple = Pair ∪ EmptyList`; analyzer mis-labeled as "pair". Tightening to `TypeList` is semantically correct. |
| `continuation-mark-set->list*` | 1 | `TypeList` | `keyListVal.(values.Tuple)` at `registry/core/prim_cont_marks.go:88`, iterates with EmptyList terminator | **Keep `TypeList`** — already correct; analyzer mis-labeled. |
| `get-output-bytevector` | 0 | `TypeOutputPort` | `RequireArg[values.ByteVectorExtractor]` at `internal/extensions/io/prim_ports.go:193` | **Partial tightening to `TypeBinaryOutputPort`** — narrower than current but still a superset of the impl's accepted domain (see §2.B.1). |

#### 2.B.1 `get-output-bytevector` — partial narrowing, not a complete fix

The impl requires `values.ByteVectorExtractor`, satisfied by exactly two port types: `ByteVectorBufferedOutputPort` and `ByteVectorInputOutputPort` (verified via `var _ ByteVectorExtractor = ...` assertions in `values/byte_vector_buffered_output_port.go:26` and `values/byte_vector_input_output_port.go:27`).

`TypeBinaryOutputPort` checks for the `values.BinaryWriter` interface (`values/value_type.go:229`), satisfied by **four** port types: the two above plus `BinaryOutputPort` (file-backed) and `ByteVectorOutputPort` (file-backed with byte-vector name). The latter two will pass the `TypeBinaryOutputPort` contract but still fail with a type mismatch when the impl fails the `ByteVectorExtractor` assertion.

**Tightening status**:
- Catches misuse like `(get-output-bytevector some-textual-output-port)` at the contract boundary under Phase-2 enforcement.
- Does not catch `(get-output-bytevector (open-binary-output-file ...))`, which still fails at runtime with the same error as today.
- No runtime cost. Declaration gets strictly more precise without regressing any valid call.

**Decision**: include in 5.D as an opportunistic partial narrowing. Frame it as "declaration moved one step down the port hierarchy, not to the exact accepted domain."

#### 2.B.2 Predicate domain caveat (reference)

R7RS predicates accept any value and return `#f` for non-matching types (`(pair? 5) → #f`). Tightening `pair?` to `TypePair` would reject non-pair inputs at the Phase-2 contract boundary, which violates R7RS. **Exclude all predicates from cleanup.** Affected predicates listed as `TypeAny → specific-type` in the inventory: `pair?`, `error-object?`, `read-error?`, `file-error?`, `process?`, `integer?`, `rational?`, `real?`, `binary-port?`, `textual-port?`.

#### 2.B.3 Tuple-vs-Pair analyzer artifact

`values.Tuple` is the Scheme-level `TypeList` (see `values/value_type.go:68`: "Tuple interface"), satisfied by both `*Pair` and the `emptyListType{}` singleton. The analyzer reports the scanned type as "pair" whenever it sees a `values.Tuple` assertion, because its mapping table assumes the assertion is on a `*Pair`. This mis-labeling accounts for:

- `generate-temporaries` slot 0 (confirmed — impl uses `values.Tuple`)
- `continuation-mark-set->list*` slot 1 (confirmed — impl uses `values.Tuple`)
- All six §2.D list-vs-pair entries (`length`, `reverse`, `list->string`, `list-copy`, `list-ref`, `list-tail` — all use `values.IsEmptyList` early-return + `values.Tuple` assertion)

**Follow-up (analyzer, not primitives)**: update `audit/wile-axis-b-params.scm` to distinguish `values.Tuple` from `*values.Pair` in its extractor table. Expected effect: 8 rows move from Declared-too-wide to Single-strict, no primitive edits.

### 2.C Analyzer artifacts — optional normalization (~10 entries)

Declared `TypeExactInteger`, scanned as `integer`. `values/value_type.go:61` comments `TypeExactInteger` as an alias for `TypeInteger`, and `value_type.go:201` explicitly sets `checks[TypeExactInteger] = checks[TypeInteger]`. The analyzer reports these as different because it compares string labels, but the runtime behavior is identical.

| Primitives (slot) |
|---|
| `bytevector-copy!` (1), `integer->char` (0), `make-bytevector` (0), `make-list` (0), `make-string` (0), `make-vector` (0), `string-copy!` (1), `substring` (1, 2), `vector-copy!` (1), `exact-integer-sqrt` (0) |

**Recommendation**: update the analyzer to treat `TypeExactInteger == TypeInteger` symmetrically rather than changing 10+ primitive declarations. No runtime consequence either way; the analyzer fix is one line.

### 2.D Semantic list-vs-pair — 6 entries, case-by-case verification

Primitives declared `TypeList` where the impl type-asserts to `TypePair` (rejecting `EmptyList`). `TypeList = Pair | EmptyList` per `values/value_type.go:68` ("Tuple interface"). Tightening to `TypePair` would break `(length '())`, `(reverse '())`, etc., if the impl silently handles `EmptyList` through a short-circuit before the assertion.

| Primitive | Slot | Source | Known to handle `'()` |
|---|---|---|---|
| `length` | 0 | `registry/core/prim_lists.go:165` | Yes — `(length '()) → 0` per R7RS |
| `list->string` | 0 | `registry/core/prim_strings.go:155` | Yes — `(list->string '()) → ""` |
| `list-copy` | 0 | `registry/core/prim_lists.go:302` | Yes — R7RS returns `()` |
| `list-ref` | 0 | `registry/core/prim_lists.go:193` | No — indexing into `'()` is an error anyway |
| `list-tail` | 0 | `registry/core/prim_lists.go:260` | Partial — `(list-tail '() 0) → '()` |
| `reverse` | 0 | `registry/core/prim_lists.go:138` | Yes — `(reverse '()) → '()` |

**Action for 5.D**: none. Declared `TypeList` is correct. Follow-up in the analyzer: recognize early-return / short-circuit on `EmptyList` before a type assertion, same class of fix as the post-gate-filter analysis called out in §1.

---

## 3. Union bucket — 13 entries, feeds TypeUnion extension evidence

Primitives where the impl type-switches over a non-singleton set of types, no existing `ValueType` spans the union. All concentrated in math/threads extensions.

| Primitive | Slot | Declared | Scanned union | Source |
|---|---|---|---|---|
| `angle` | 0 | `number` | `{complex, integer, flonum, rational}` | `extensions/math/prim_complex.go:231` |
| `denominator` | 0 | `real` | `{integer, rational, flonum}` | `extensions/math/prim_rational.go:64` |
| `imag-part` | 0 | `number` | `{complex, integer, rational, flonum}` | `extensions/math/prim_complex.go:181` |
| `magnitude` | 0 | `number` | `{complex, integer, flonum, rational}` | `extensions/math/prim_complex.go:200` |
| `make-rectangular` | 0, 1 | `real` | `{number, flonum}` | `extensions/math/prim_complex.go:29` |
| `number->string` | 0 | `number` | `{integer, flonum, rational, complex}` | `extensions/math/prim_conversion.go:47` |
| `numerator` | 0 | `real` | `{integer, rational, flonum}` | `extensions/math/prim_rational.go:27` |
| `rationalize` | 0, 1 | `real` | `{integer, rational, flonum}` | `extensions/math/prim_rational.go:101` |
| `real-part` | 0 | `number` | `{complex, integer, flonum, rational}` | `extensions/math/prim_complex.go:164` |
| `seconds->time` | 0 | `number` | `{integer, flonum}` | `extensions/threads/prim_threads.go:631` |
| `sqrt` | 0 | `number` | `{integer, flonum, rational, complex}` | `extensions/math/prim_transcendental.go:117` |

**Observation**: in every case, the declared annotation (`TypeNumber` or `TypeReal`) is the *correct* R7RS type for the primitive's domain. The "union" label is an analyzer artifact of the impl's branch-per-numeric-type dispatch. A `TypeUnion(Integer, Float, Rational)` declaration would be *equivalent in semantics* to declaring `TypeReal` — no information gain, significant complexity cost for every `TypeConstraint` consumer (design doc §7.3).

**Action for 5.D**: none. Action for Phase 5.E or later: document this cluster as **negative evidence** against `TypeUnion` introduction. The numeric tower's branch dispatch is an implementation detail; the declared type captures the domain correctly.

---

## 4. Unguarded bucket — 79 entries, analyzer false-positive class

No type check detected by SSA scan. 79/476 = 16.6%, below the §3.4 kill criterion (30%).

### Sub-class breakdown

| Sub-class | Count (est.) | Example primitives | Why analyzer misses |
|---|---|---|---|
| Helper delegation | ~40 | `modulo`, `quotient`, `remainder` (→`integerDivisionOp`); `odd?`, `even?` (→`helpers.NumericPredicate`); `syntax-*` location accessors (→`requireSyntaxValue`) | Gate happens inside a local helper, one level down |
| Opaque-rest or continuation primitives | ~20 | `call-with-*`, `call/cc`, `apply`, `with-timeout` | Dispatch via apply machinery; argument validation happens in VM, not the primitive |
| Intentionally permissive | ~10 | `raise`, `raise-continuable`, `force`, `make-parameter`, `syntax-local-introduce` | Truly accept `TypeAny` by design |
| Others | ~9 | `atan`, `log`, `null-environment`, `scheme-report-environment`, `make-opaque-record-type`, `make-record-type`, `record-accessor`, `record-constructor`, `record-modifier` | Various — mostly helper delegation |

The analyzer design (§3.4) anticipated this: "*follow one level of local helper calls*" is marked as a known risk; v1 did not implement interprocedural extraction.

**Action for 5.D**: none. Action for a follow-up analyzer revision: extend `audit/wile-axis-b-params.scm` to inline one level of local package-private helper calls when they immediately return to the caller with a type guard. Expected yield: move ~40 primitives from Unguarded to Single-strict/Single-coercing, dropping Unguarded to ~8% and raising audit confidence.

---

## 5. Variadic-rest — 93 entries, separate track

Rest-slot analysis is per-element. Inventory §Variadic-rest aggregates stats but does not produce a bug list because:

- Most variadic primitives accept `TypeAny` rest-elements by contract (`+`, `*`, `list`, `vector`, `apply`, `error`).
- Others type-switch at runtime per element (`error-object-irritants`, `string-append`).

**Action for 5.D**: none. Follow-up: if Phase 2+ introduces rest-type validation as a separate `ForeignClosure.ValidateRest` hook, this bucket reconsolidates into the primary classification.

---

## 6. Priority-ordered actionable list for Phase 5.D

### Tier 1 — one opportunistic partial narrowing

Only one primitive-level edit is well-motivated:

- `get-output-bytevector` slot 0: `TypeOutputPort` → `TypeBinaryOutputPort`. Partial narrowing; see §2.B.1 for the acknowledged gap (`BinaryWriter` ⊋ `ByteVectorExtractor`). Inclusion is optional.

The Phase 4 H.1 arithmetic cluster that motivated the "30–80 mechanical fixes" estimate in design §5 did not replicate. Extrapolation from a single category misled the estimate. The actual finding: the annotation system is consistent with its design within the vocabulary it has; the actionable pressure is on the vocabulary itself, not on individual annotations.

### Tier 2 — analyzer refinements (highest-value follow-up work)

Change scope is the analyzer script (`audit/wile-axis-b-params.scm`), not the primitives. Each refinement removes a false-positive class that currently inflates bucket counts:

| Refinement | Removes from | Est. FPs eliminated |
|---|---|---|
| Treat `TypeExactInteger == TypeInteger` symmetrically | §2.C | ~10 |
| Distinguish `values.Tuple` from `*values.Pair` in extractor table | §2.B.3, §2.D | ~8 |
| Inline one level of local helper calls | §4 unguarded | ~40 |
| Trace post-gate filters (rejection-style `if isComplex { return err }`) | §1, some §2.A | ~3 |

Together these drop the actionable-looking noise from ~60 entries to ~9, substantially raising audit confidence without touching the registry.

### Tier 3 — documentation

- Update `plans/CLAUDE.md` Phase 5 row: replace "Phase 5.B shipped …" with "5.C sidecar complete; 1 opportunistic partial tightening, 85 param-side TypeConstraint gaps cross-referenced with axis-B Category C; primary follow-up is analyzer refinement per §Tier 2".
- Add back-reference from `plans/2026-04-20-axis-b-annotation-bugs.md` §4 to this sidecar's §2.A (same TypeConstraint deficit from both angles).
- Update `TODO.md` Tier 1 audit item to reflect 5.C complete and 5.D's reduced scope.

### Explicitly deferred

- TypeConstraint vocabulary gaps (§2.A, ~85 entries): requires design decision on extension scope (scalar enum entries vs. `TypeOpaque(name)` parametric).
- Union bucket (§3): awaiting evidence for `TypeUnion` extension. Current annotations are correct.
- Phase 5.D as previously scoped: the "unified PR, one commit per coercion family" from design §7.2 was dimensioned for 30–80 fixes. With 1 fix, that scaffolding is unnecessary; a single-commit PR is appropriate.

---

## 7. Manifest regeneration

After Tier 1 edits (once verification complete):

```
cd wile
WILE_AUDIT_UPDATE=1 go test ./registry/... -run TestBuildAxisBManifest
wile-goast -f audit/wile-axis-b-params.scm > plans/paramtypes-raw.scm
# regenerate plans/2026-04-20-paramtypes-inventory.md from raw
```

Expected delta: 1–3 rows move from Declared-too-wide to Single-strict. No other buckets change.

---

## 8. Cross-references

- Phase 5 design: `plans/2026-04-20-paramtypes-audit-design.md` §4 (this file satisfies §4).
- Phase 5 inventory: `plans/2026-04-20-paramtypes-inventory.md` (input data).
- Phase 3 Category C (scalar TypeConstraint gaps, return-side view): `plans/2026-04-20-axis-b-annotation-bugs.md` §4.
- TypeConstraint enum: `values/value_type.go:52–81` (28 entries, enumerated).
- Analyzer: `audit/wile-axis-b-params.scm`.
