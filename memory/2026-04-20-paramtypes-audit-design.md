# Primitive Annotation Audit — Phase 5: ParamTypes Audit Design

**Status**: ✅ **COMPLETE** — Phase 5.A (PR #678), 5.B (PR #679), 5.C (PR #680),
5.D (one partial narrowing: `get-output-bytevector` → `TypeBinaryOutputPort`),
5.E (`plans/2026-04-20-paramtypes-axis-c-findings.md` R7RS sweep) all shipped.
See `plans/CLAUDE.md` "Extension Contracts (Phase 2+)" row for full status table.
Vocabulary-extension follow-up at `plans/2026-04-21-type-constraint-extension-design.md`.
**Parent**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 5 extends the four-axis framework to `ParamTypes`.
**Prior phases**:
- Phase 1 (axis A, docs ↔ `ReturnType`): complete — harness reports 0 findings.
- Phase 3 (axis B, `ReturnType` ↔ impl): complete — inventory + sidecar (`plans/2026-04-19-axis-b-inventory.md`, `plans/2026-04-20-axis-b-annotation-bugs.md`).
- Phase 3.D (cleanup): complete — PR #675 tightened 5 declared `ReturnType`s to match narrowed singletons.
- Phase 4 (axis C, `ReturnType` ↔ R7RS): complete — all 9 R7RS-small categories swept.

**This phase**: audit the *input* side of `PrimitiveSpec`, mirroring the four-axis framework but with a fundamentally different analysis shape.

---

## 1. Why ParamTypes is not just "ReturnType backwards"

The Phase 3 analyzer asked: *what types flow into the result-writing sinks from this primitive?* A single question per primitive, a single narrowed set per answer, one `ReturnType` field.

`ParamTypes` is structurally different along three axes:

### 1.1 Arity

`PrimitiveSpec.ParamTypes` is `[]TypeConstraint` sized to `ParamCount`. A primitive with 3 parameters has 3 independent annotation slots. Variadic primitives (`IsVariadic: true`) add a special last slot — the **rest-type**, annotating the element type of the variadic tail.

Each slot is its own audit subject. `+` has one annotation slot (the variadic rest); `string-copy` has three (string, optional start, optional end); `vector-set!` has three (vector, integer, any).

### 1.2 Direction of evidence

Where `ReturnType` analysis walks *forward* from the impl toward sinks, `ParamTypes` analysis walks *backward* from argument extraction points:

```
mc.Arg(0)   ─┐                                ┌─ values.NewInteger(...)
             ├─ [analyzer reads flow outward] ─┤
mc.Arg(1)   ─┘                                └─ mc.SetValue(...)
```

`ParamTypes` asks: *what types does the impl require/accept at `mc.Arg(i)`?* The answer comes from type-switches, typed helper extractors, and the lack thereof — not from constructors.

### 1.3 Strict gates vs. coercing extractors

`ReturnType` narrowing only sees what the impl *produces*. `ParamTypes` narrowing must distinguish two fundamentally different impl patterns:

| Pattern | Example | Accepted domain |
|---|---|---|
| **Strict type gate** | `helpers.RequireArg[*values.String](mc, 0, ...)` | `*values.String` — rejects everything else |
| **Coercing extractor** | `helpers.ExtractInteger(mc.Arg(0))` | `*Integer \| *BigInteger \| *Float` (if integer-valued) — broader than `TypeInteger` |
| **Type switch** | `switch v := mc.Arg(0).(type) { case *A: …; case *B: … }` | Union of case types (possibly with explicit error default, possibly pass-through) |
| **Unguarded** | `mc.Arg(0)` used directly, no type check | `any` — impl tolerates any `Value` (often because a callee raises the type error downstream) |

Phase 4 axis-C finding H.1 (`quotient`/`remainder`/`modulo`/`gcd`/`lcm`) was exactly the coercing-extractor trap: declared `TypeInteger`, impl via `ExtractInteger` accepted `TypeReal`. The declared annotation understated the accepted domain. Under Extension Contracts Phase 2 runtime validation, every such primitive would wrongly reject legitimate programs.

This is more urgent than the Phase 3 cleanup found. Phase 3 findings were mostly declared-too-wide (imprecise but sound under Phase 2). Phase 5 findings will include a significant fraction of **declared-too-narrow** primitives — actively unsound under Phase 2 runtime enforcement.

---

## 2. The four axes applied to ParamTypes

| Axis | Pair | Catches | Technique |
|---|---|---|---|
| **A** | docstring prose ↔ `ParamTypes` slot | Parameter descriptions contradicting declared type (e.g., "N: a non-negative integer" but `TypeAny`) | Text scan + manual triage — most docstrings don't formalize parameter types, so axis A's yield is lower than for `ReturnType`. |
| **B** | `ParamTypes[i]` ↔ impl's extraction at `mc.Arg(i)` | Declared-too-narrow (rejected by Phase-2 but impl accepts broader); declared-too-wide (imprecise but sound); unguarded params declared as typed | SSA analysis of argument-extraction call sites. **Primary Phase 5 work.** |
| **C** | impl accepted domain ↔ R7RS / SRFI / Racket spec | Non-standard acceptance (e.g., Wile accepting `Integer` where R7RS requires `ExactInteger`); standard-rejection (Wile rejecting what R7RS accepts) | Human review, category by category — Phase 4-style. |
| **D** | `ParamTypes[i]` ↔ spec | Falls out of B+C. | — |

**Sequencing proposal**:

```
Phase 5.A — ParamTypes manifest generator                 ← extends audit_manifest_test.go
Phase 5.B — ParamTypes SSA analyzer                       ← extends wile-goast/audit/wile-axis-b.scm
Phase 5.C — Inventory + sidecar bug list                  ← parallel to 2026-04-19-axis-b-inventory.md
Phase 5.D — Mechanical cleanup PR                         ← parallel to PR #675
Phase 5.E — Axis C sweep (R7RS categories)                ← parallel to Phase 4
```

5.A is a small extension. 5.B is the main tooling work. 5.C–5.E are analogous to prior phases.

---

## 3. Phase 5.B — analyzer architecture

### 3.1 Manifest extension (5.A)

Extend `audit_manifest_test.go` to emit ParamTypes data. Currently the manifest is a flat list of `(name declared-return-type go-function go-source)`; extend each entry to carry `param-types`:

```scheme
("vector-set!"
  "void"                                               ; declared-return-type (existing)
  ("vector" "exact-integer" "any")                     ; declared-param-types (NEW)
  "github.com/aalpar/wile/registry/core.PrimVectorSet"
  "registry/core/prim_vectors.go:35")
```

Variadic rest is rendered as `"...any"` (ellipsis prefix):

```scheme
("+"
  "number"
  ("...number")
  "...")
```

`TestBuildAxisBManifest` regenerates this in one pass; the `WILE_AXIS_B_UPDATE=1` escape hatch remains the same. The Phase 3 manifest format gains one slot per entry — backward-compatible because Scheme consumers that `cdr` past the return type already work by position, and the analyzer script can be updated atomically with the manifest format.

### 3.2 SSA analyzer (5.B core)

Extend `audit/wile-axis-b.scm` (currently only narrows return types) with a **per-argument narrowing pass**. For each primitive F and each parameter slot `i` in `0..ParamCount-1`:

1. Locate the SSA value corresponding to `mc.Arg(i)` inside F.
2. Walk forward through the def-use chain until one of four events:
   - **Strict gate**: the value flows into a generic type-parameterized helper like `helpers.RequireArg[*T]`. Record the type parameter as the narrowed accepted type. Confidence: `narrow`.
   - **Coercing extractor**: the value flows into a helper whose accepted domain is known to be wider than any single `ValueType` (e.g., `helpers.ExtractInteger`, `helpers.ToFloat64`, `helpers.ExtractReal`). Record the extractor's accepted domain (from a small, curated table). Confidence: `coercing`.
   - **Type switch**: the value is the scrutinee of a Go `switch v := x.(type)`. Record the case-arm types as a union; default-arm handling:
     - If default returns an error: the switch is closed; union = case types only.
     - If default falls through / assigns the value: union = case types + `any`.
     - Confidence: `narrow` if closed, `widened` if open.
   - **Unguarded**: the value reaches a primitive exit (success return, sink, or passes to a helper not in the extractor table) without any type check. Confidence: `unguarded`.

Use a curated **extractor table** (hand-maintained, small) mapping helper function names to their accepted domains. Starting set:

| Helper | Accepted domain |
|---|---|
| `helpers.RequireArg[*T]` | `T` (read from type parameter) |
| `helpers.OptionalArg[*T]` | `T \| absent` |
| `helpers.ExtractInteger` | `*Integer \| *BigInteger \| *Float (integer-valued)` |
| `helpers.ExtractReal` | `*Integer \| *BigInteger \| *Rational \| *Float \| *BigFloat` |
| `helpers.ToFloat64` | same as `ExtractReal` |
| `helpers.ToComplex128` | `ComplexNumber \| RealNumber` |
| `helpers.ParseSubrange` | start, end: `*Integer` or absent |

Table lives in the analyzer script, not in the wile source tree — it's narrowing metadata, not runtime data.

### 3.3 Bucketing

Mirrors Phase 3 buckets with adjustments for the per-slot analysis:

| Bucket | Pattern | Informs |
|---|---|---|
| **Single-strict** | One `RequireArg[*T]` gate, declared matches narrowed | Clean ✓. |
| **Single-coercing** | One coercing extractor, declared matches accepted domain | Clean ✓. |
| **Declared-too-narrow** | Declared `ValueType` is strictly narrower than the narrowed accepted domain | **Bug sidecar.** Under Phase-2 enforcement, valid calls would be rejected. High priority. |
| **Declared-too-wide** | Declared is wider than narrowed (e.g., `TypeAny` where impl strictly requires `TypeString`) | Lower-priority tightening; same shape as Phase 3 Category A. |
| **Union** | Type switch over multiple types, no existing `ValueType` covers the union | Type-system gap — feeds future `TypeUnion` evidence. |
| **Unguarded** | No type check; declared is `TypeAny` or missing | No-op. A high count signals "most primitives are runtime-typed via callees, not statically" — evidence for whether to invest in static enforcement at all. |
| **Variadic-rest** | Rest slot analysis (separate bucket because rest is per-element, not per-argument) | Aggregate rest-type statistics separately. |

### 3.4 Known risks

- **Extractor table completeness**: `grep` across the codebase shows ~15 distinct helper names. A curated table of 10–15 entries should cover most cases, but a long tail of ad-hoc type-switches will consume analysis time. Kill criterion: if >30% of slots land in `unguarded` due to missing extractor-table entries, stop and expand the table.
- **Optional argument handling**: primitives with optional arguments (see `registry/CLAUDE.md` "Optional Argument Patterns") parse the rest-list via different mechanisms (`OptionalArg[T]`, `ParseOptionalArg`, `ParseSubrange`). Each variant needs an extractor-table entry.
- **Inter-procedural flow**: some primitives hand `mc.Arg(i)` to a helper in the local package (`requireSyntaxValue`, `requireSourceContext`, etc.). The analyzer must follow one level of local helper calls; otherwise every syntax-loc primitive lands in `unguarded` (same false-positive class that hit Phase 3's `syntax-*` return-type analysis).

---

## 4. Phase 5.C — inventory + sidecar output

Two artifacts, parallel to Phase 3:

- `plans/2026-04-20-paramtypes-inventory.md` — human-readable distribution across the buckets above. Per-slot stats: total declared slots, % strict-matched, % coercing-matched, % declared-too-narrow, etc.
- `plans/2026-04-20-paramtypes-annotation-bugs.md` — sidecar bug list for Phase 5.D. Prioritized:
  1. **Declared-too-narrow** (unsound under Phase-2 enforcement)
  2. **Declared-too-wide** (imprecise but sound — lower priority)
  3. **Union gaps** — feeds `TypeUnion` / `TypeMaybe` extension plan (same as Phase 3 Category C)

---

## 5. Phase 5.D — cleanup PR

Mechanical edits based on Phase 5.C sidecar. Expected scope: higher than Phase 3.D's 5 changes, because:

- `ParamTypes` slots across 500 primitives ≈ 1000+ individual annotations (avg ~2 slots per primitive).
- Phase 4 H.1 found 5 declared-too-narrow primitives in a single category (arithmetic). Extrapolating, a full sweep may find 30–80 mechanical fixes.
- Coercing-extractor patterns are standardized (`ExtractInteger`, `ExtractReal`) so most bugs share a shape: `TypeInteger` → `TypeReal`, `TypeRational` → `TypeReal`, etc.

Commit structure: one commit per coercion family (integer, real, complex, ...). Manifest regeneration in a separate commit per the Phase 3.D pattern.

---

## 6. Phase 5.E — axis C R7RS sweep

Optional final stage. For each R7RS-small category, compare impl accepted-domain against R7RS parameter-type specification. Category-by-category per Phase 4. Expected yield: low (R7RS has few unusual parameter-type requirements Wile's impl would violate), but parallel-structure closure for the four-axis framework.

---

## 7. Open decisions

### 7.1 Rest-type annotation format — DECIDED (2026-04-20)

`PrimitiveSpec.ParamTypes` array terminates at `ParamCount`. There is no dedicated field for the variadic rest element type. The convention is:

```go
{ParamCount: 2, IsVariadic: true, ParamTypes: []TypeConstraint{TypeAny, TypeAny}}
//              ^^^^^^^^^^^^^^^^                                 ^^^^^^^^
// "2 params, last is variadic rest"                             "rest-element type
//                                                                (not rest-list type)"
```

**Decision**: formalize the existing convention with a doc comment on `PrimitiveSpec.ParamTypes`. No structural code change. Parallel to how `PrimitiveSpec.ReturnType: nil` means "unspecified" (also a convention, also previously undocumented in the type itself).

Applied in this design PR — see `registry/registry.go:31–38`. Phase 5 tooling relies on this as canonical going forward.

### 7.2 Scope of the cleanup PR — DECIDED (2026-04-20)

**Decision**: unified PR. One commit per coercion family (integer, real, complex, …) so the diff remains reviewable by category, but all changes land together. Larger than Phase 3.D; scope mismatch is acceptable given the load-bearing nature of declared-too-narrow fixes under Extension Contracts Phase 2.

### 7.3 Relationship to `TypeConstraint` vocabulary extension — DEFERRED (2026-04-20)

Phase 3 Category C (28 singleton gaps — `TypeThread`, `TypePromise`, `TypeBox`, …) and Phase 5's union bucket (primitives accepting `{T1, T2, …}` unions via type switches) both feed future `TypeConstraint` extension work — but with **different cost profiles**:

- **Category C**: scalar extensions (add new enum constants + `makeCheck[*T]` closures). Low per-entry cost, low evidence bar.
- **Phase 5 unions**: parametric extensions (introduce `TypeUnion(T1, T2, …)` or `TypeMaybe(T)`). Every `TypeConstraint` consumer must handle compound types. High per-extension cost, high evidence bar.

**Decision**: keep the two artifacts separate until Phase 5.C produces concrete cluster shapes. Cross-reference between them:

- Phase 5 inventory's union-bucket section cites Phase 3 Category C for scalar-gap context.
- `plans/2026-04-20-axis-b-annotation-bugs.md` §4 (Category C) already lists the 28 scalar gaps.

Revisit consolidation after Phase 5.C. Administrative merging before the data justifies it risks papering over the cost-and-evidence distinction that will actually drive the downstream extension decisions.

---

## 8. Deliverables

- `audit_manifest_test.go` — extended to emit ParamTypes.
- `wile-goast/audit/wile-axis-b.scm` or wile-side equivalent — extended with per-slot narrowing.
- `plans/2026-04-20-paramtypes-inventory.md` — primary Phase 5.C output.
- `plans/2026-04-20-paramtypes-annotation-bugs.md` — sidecar for 5.D.
- Phase 5.D cleanup PR (mechanical fixes).
- (Optional) Phase 5.E per-category sweep findings.

---

## 9. Non-goals

- Not extending `TypeConstraint` itself — same scope boundary as Phase 3 §6 decision (2026-04-19).
- Not auditing `ParamCount` vs. impl arity. A separate class of bug (mis-counted parameters) worth a follow-up but not this audit.
- Not auditing `IsVariadic` correctness. Same rationale.
- Not automating the extractor table. Curated by hand; update as the codebase grows new patterns.
- Not implementing a general SSA narrowing pass for ad-hoc Go type switches. Starter approach is the extractor table + direct-switch recognition. A full SSA narrowing pass is the eventual correct answer for primitives that bypass helpers (dozens in `prim_arithmetic.go`, `prim_predicates.go`) — deferred as a follow-up once Phase 5 reveals whether the starter coverage is adequate. Acknowledged 2026-04-20 review.
