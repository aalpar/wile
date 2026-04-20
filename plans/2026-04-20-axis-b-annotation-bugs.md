# Axis-B Annotation Bug Sidecar (Phase 3.D)

**Parent plan**: `plans/2026-04-19-primitive-annotation-audit.md` §3 Phase 3.
**Analyzer design**: `plans/2026-04-19-axis-b-analyzer-design.md` §5, §7.4.
**Inventory source**: `plans/axis-b-raw.scm` (500 primitive records).
**Status**: Bug list extracted; mechanical cleanup PR pending (tracked as Phase 3.D in §8 of the analyzer design).

---

## 1. Method

Filter `plans/axis-b-raw.scm` for records where:

1. `(bucket Single)` — all impl return paths narrow to a single Go type.
2. Declared `ReturnType` does not equal the narrowed singleton after normalization.

Normalization rules:

- `TypeInteger` and `TypeExactInteger` share one check function (`values/value_type.go:201`) — treat them as equivalent.
- Concrete Go type names produced by the narrower map to their `TypeConstraint` vocabulary equivalents:
  - `values.voidType` → `void`
  - `values.eofType` → `eof-object`
  - `*machine.ForeignClosure` → `procedure`
  - `*machine.MachineContext` → `any` (narrower could not inspect through an inline-primitive boundary)
  - `[]values.Value` → `any` (multi-value return; no single-value `TypeConstraint`)
  - `*values.ByteVectorBufferedOutputPort` → `binary-output-port`
  - Other Go types (`*machine.PromptTag`, `*values.Thread`, etc.) have no existing `TypeConstraint` and are surfaced as type-system gaps, not annotation bugs.

Raw mismatch count (pre-normalization): 41. After normalization, partitioned into four categories below.

---

## 2. Category A — Mechanical tightening (actionable)

**5 primitives.** Declared type is wider than the narrowed singleton, the narrowing is verified against every impl branch, and a more specific `ValueType` constant already exists.

| Primitive | File:line | Current | Proposed |
|---|---|---|---|
| `make-polar` | `extensions/math/register.go:175` | `TypeNumber` | `TypeComplex` |
| `angle` | `extensions/math/register.go:191` | `TypeReal` | `TypeFlonum` |
| `thread-name` | `extensions/threads/register.go:53` | `TypeAny` | `TypeString` |
| `mutex-name` | `extensions/threads/register.go:94` | `TypeAny` | `TypeString` |
| `condition-variable-name` | `extensions/threads/register.go:128` | `TypeAny` | `TypeString` |

**Verification** (against impls):

- `make-polar` (`prim_complex.go:142`): single path — `values.NewComplexFromParts(...)` yields `*Complex` (satisfies `TypeComplex`).
- `angle` (`prim_complex.go:231`): seven case arms all call `values.NewFloat(...)` or `values.NewBigFloatFromFloat64(...)`; both types satisfy `TypeFlonum` (`value_type.go:202–212`).
- `thread-name`, `mutex-name`, `condition-variable-name`: all three impls call `values.NewString(Name())` unconditionally (after arg type-check). No `#f` path. Note: existing docstring comments say "or #f if unnamed" — stale prose, independent axis-A issue surfaced in passing.

**Known stale docstrings** (out of scope for this PR): `extensions/threads/register.go:54`, `:94`, `:128` claim "or #f if unnamed". Retain as future docstring-cleanup work.

Expected effect: zero runtime behavior change; extension-contract Phase 2 gains five sound-narrow annotations.

---

## 3. Category B — Declared type missing (not actionable)

**1 primitive.**

| Primitive | File:line | Declared | Reason |
|---|---|---|---|
| `exact-integer-sqrt` | `extensions/math/prim_rational.go:226` | empty string (unset) | Returns two values via `mc.SetValues(s, r)`. Intentionally left unset per `extensions/math/register.go:163`. Not a bug — there is no single-value `TypeConstraint` for multi-value returns. |

**Disposition**: Leave as-is. Document the multi-value-return convention as input to a future `TypeConstraint` extension (analogous to `TypeMaybe`/`TypeUnion` in parent plan §6, but orthogonal).

---

## 4. Category C — Type-system gaps (deferred)

**28 primitives.** The narrower identified a concrete Go type that has **no corresponding `ValueType` constant**. Tightening requires adding to the `TypeConstraint` vocabulary — out of scope for this audit per parent plan §6 decision (2026-04-19).

These are the strategic signal: they tell the future `TypeConstraint` extension plan which additions are actually justified by real primitives.

| Narrowed type | Count | Primitives |
|---|---|---|
| `*machine.PromptTag` | 2 | `make-continuation-prompt-tag`, `default-continuation-prompt-tag` |
| `*machine.ContinuationMarkSet` | 1 | `continuation-marks` |
| `*values.Box` (narrowed as `box`) | 1 | `box` |
| `values.eofType` | 1 | `eof-object` |
| `*environment.Namespace` | 6 | `interaction-environment`, `scheme-report-environment`, `null-environment`, `environment`, `make-namespace`, `namespace-derive` |
| `*values.CompileTimeValue` | 1 | `make-compile-time-value` |
| `*values.Thread` | 1 | `thread-start!` |
| `*values.Mutex` | 1 | `make-mutex` |
| `*values.ConditionVariable` | 1 | `make-condition-variable` |
| `*values.Time` | 2 | `current-time`, `seconds->time` |
| `*values.Channel` | 1 | `make-channel` |
| `*values.WaitGroup` | 1 | `make-wait-group` |
| `*values.RWMutex` | 1 | `make-rw-mutex` |
| `*values.Once` | 1 | `make-once` |
| `*values.AtomicBox` | 1 | `make-atomic` |
| `*values.RecordType` (narrowed as `record-type`) | 3 | `make-record-type`, `make-opaque-record-type`, `record-type` |
| `*values.Promise` (narrowed as `promise`) | 2 | `make-promise`, `%make-lazy-promise` |
| `*values.Process` | 1 | `process-spawn` |

**Concentration:** 18 of 28 entries map to five gap categories — prompt/mark plumbing (3), namespace (6), concurrency primitives (7), records (3), promises (2). That's the evidence for which `TypeConstraint` extensions to ship first if/when a future plan opens.

**Disposition:** No annotation change. Preserve as input to future Extension Contracts work (see TODO.md "Extension API contracts Phase 2+").

---

## 5. Category D — Narrower limitation (not actionable)

**7 primitives.** The narrower missed a `SetValue` sink — either an inline-primitive boundary (`MachineContext` escape) or a helper-internal `SetValue` — so its narrowed set is strictly smaller than the impl's true codomain.

| Primitive | File:line | Declared | Narrower output | Rationale |
|---|---|---|---|---|
| `make-parameter` | `registry/core/prim_parameters.go:32` | `TypeProcedure` | `*MachineContext` | Returns `*Parameter`, which implements `Callable` → `TypeProcedure` is correct. |
| `continuation-prompt-available?` | `registry/core/prim_prompt.go:194` | `TypeBoolean` | `*MachineContext` | Predicate always returns `TrueValue`/`FalseValue`. |
| `once-do!` | `extensions/gointerop/prim_gointerop.go:405` | `TypeBoolean` | `*MachineContext` | Returns boolean indicating whether the thunk ran. |
| `syntax-line` | `registry/core/prim_syntax_loc.go:66` | `TypeAny` | `integer` | Narrower saw only the `NewInteger` path; missed `values.FalseValue` set inside `requireSourceContext` helper (line 44). Impl truly returns integer-or-`#f`. |
| `syntax-column` | `registry/core/prim_syntax_loc.go:78` | `TypeAny` | `integer` | Same helper-indirect `#f` sink as above. |
| `syntax-position` | `registry/core/prim_syntax_loc.go:90` | `TypeAny` | `integer` | Same. |
| `syntax-span` | `registry/core/prim_syntax_loc.go:102` | `TypeAny` | `integer` | Same. |

**Disposition:** No change. These are analyzer-capability limitations, not annotation bugs.

**Analyzer improvement suggestion:** The helper-indirect sink pattern (`foo() { if x { mc.SetValue(Y); return (nil, nil) }; ... return (Z, nil) }` where the caller conditionalizes on the first return) is a real pattern in the codebase — four cases here alone. A future analyzer pass that follows sink-setting through single-level helper calls would reclassify these.

**Disposition:** No change. These are analyzer-capability limitations, not annotation bugs. If the analyzer later gains the ability to follow inline-primitive boundaries, rerunning will reclassify them into `Single`.

---

## 6. Totals

| Category | Count | Action |
|---|---|---|
| A — mechanical tightening | 5 | One PR with 5 one-line changes (tracked as Phase 3.D cleanup). |
| B — missing declaration (multi-value return) | 1 | No action; conventional gap. |
| C — type-system gap | 28 | No action; feeds future `TypeConstraint` extension plan. |
| D — narrower limitation | 7 | No action; analyzer capability gap. |
| **Total** | **41** | |

**Phase 1 regression guarantee**: All Category-A changes preserve behavior (no value-type change at runtime, only annotation). Re-running `audit_annotations_test.go` after the cleanup must continue to report `prims=475 with-examples=251 examples=403 self-call=328 verified=328`.

---

## 7. Next steps

- [ ] Phase 3.D cleanup PR: apply the five Category-A `ReturnType` changes. Two commits (math; threads). Re-run `audit_annotations_test.go` after each commit.
- [ ] Parent audit plan: close Phase 3 after the cleanup PR lands.
- [ ] Phase 5: `ParamTypes` audit (see TODO.md "Audit PrimitiveSpec ReturnType and ParamTypes annotations" — only `ReturnType` is complete).
