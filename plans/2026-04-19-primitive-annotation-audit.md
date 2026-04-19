# Primitive Annotation Audit — Scoping Plan

**Status**: Phase 1 in progress. §6 decision: **Option A** — union-type vocabulary is out of audit scope. Phase 2 triage categorizes findings; vocabulary extensions become a separate downstream plan.

**Origin**: `TODO.md` Tier 1 — "Audit PrimitiveSpec `ReturnType` and `ParamTypes` annotations [High, L]".

**Scope**: 421 `ReturnType:` sites across 39 files: `registry/` + `extensions/` + `internal/extensions/` + `engine.go`. Matching `ParamTypes` entries.

---

## 1. Why this matters

Two lies discovered by manual review on PR #664:

- `procedure-arity` declared `ReturnType: TypePair` but returned pair / list / integer / `#f` across branches. Fixed in `registry/core/reflection.go:29` by widening to `TypeAny` with a comment explaining the union.
- `open-output-bytevector`'s one-arg form declared `TypeBinaryOutputPort` while passing through whatever `OutputPort` the caller supplied. The arm was dead — removed in commit `d098c54b`. Now correctly declared at `internal/extensions/io/register.go:232-235`.

Both were silent until someone read the code. The systematic question: **what other annotations lie, and which check would have caught each class?**

Tier 2 "Extension API contracts Phase 2+" will ship compile-time type checking that **consults these annotations**. The moment it ships, unsound annotations stop being documentation bugs and start becoming wrongly-rejected programs. The R7RS-compliance product claim (`CLAUDE.md` §Product Vision) then rests on evidence we haven't produced.

---

## 2. The four drift axes

`PrimitiveSpec` hosts three representations of the same primitive:

```
    Doc field (prose + examples "=> X")
         │
         │  drift axis A
         ▼
    ReturnType / ParamTypes (static annotation)
         │
         │  drift axis B
         ▼
    Impl function (Go code)
         │
         │  drift axis C  (vs published standard)
         ▼
    R7RS / SRFI / Racket spec
```

Each pairing catches a different class of bug:

| Axis | Pair | Catches | Check technique |
|------|------|---------|----------------|
| **A** | docstring examples ↔ annotation | Examples whose literal output contradicts declared type. Cheap and concrete: if `(foo 1) => (1 . #f)` but `ReturnType: TypeInteger`, someone is lying. | Parse `=> X` from `Doc`, evaluate `X`, check vs `ReturnType`. |
| **B** | annotation ↔ implementation | Dead branches, branches returning types not covered by the annotation, over-narrow annotations (the `procedure-arity` class). | Static analysis of Impl return paths, or fuzz harness. |
| **C** | implementation ↔ standard | Non-standard extensions masquerading as standard primitives (the `open-output-bytevector` class). | Human review against R7RS/SRFI/Racket, domain by domain. |
| **D** | annotation ↔ standard | Documented non-standard behavior as if it were standard. | Falls out of B+C together. |

Axis A is mechanizable today with existing infrastructure. Axis B needs tooling (wile-goast SSA pass or fuzz harness). Axis C is human-labor-bound.

---

## 3. Sequencing

```
Phase 1 — Axis A harness           ← starts here
Phase 2 — Triage Phase 1 output   (bugs / union exposures / nil-candidates / side-effect-only)
Phase 3 — Axis B tooling          (wile-goast belief over SSA, or a targeted fuzz harness)
Phase 4 — Axis C (by category)    (arithmetic, strings, vectors, ports, ...) — one category = one session
```

Phase 1 should finish in one session and produces the first concrete list of lying annotations. Phases 3 and 4 are gated on Phase 2's categorization.

---

## 4. Phase 1 design — docstring-example harness

### Input

`Registry.Primitives()` returns all `PrimitiveRegistration` with full `Spec` (see `registry/registry.go:266`). Build a registry with `AllExtensions()`, enumerate, and parse `Doc`.

### Extract

Examples in `Doc` follow a uniform shape:

```
Examples:
  (procedure-arity car)   => (1 . #f)
  (procedure-arity +)     => (0 . #t)
```

A regex `(?m)^\s*(\(.+?\))\s+=>\s+(.+?)\s*$` captures call / expected pairs. Multi-line expected values are rare; start by supporting single-line and report unrecognized shapes separately.

### Evaluate

For each `(call, expected)`:

- Construct a fresh `Engine` with `WithProfile(KitchenSink)` per example. This is the only isolation that preserves every phase's bindings — `NewSchemeReportNamespace` copies only the runtime phase (bootstrap macros like `delay`/`guard` and the compile-phase `quote` handling for dotted pairs are dropped, producing false-positive eval-errors), and `NewChildNamespace` starts empty. The cost (≈3s for ~334 examples) is an acceptable price for determinism.
- Evaluate `call` in that engine via `EvalMultipleWithSource`.
- Parse `expected` as a Scheme datum via the same pattern (`(quote expected)` in a fresh engine).
- Assertion 1 (soundness): `spec.ReturnType.Check(actual)` must succeed, or the annotation is wrong.
- Assertion 2 (doc correctness): `actual.EqualTo(expectedValue)` must hold, or the docstring example is stale.

### Exclude

Primitives whose examples have observable side effects or need setup:

- Port I/O that writes to file descriptors.
- Primitives with `;` comment-form examples (`procedure-source-location`'s `=> #f  ; foreign procedure`) — already human-flagged-uncertain.
- Thread / mutex / channel primitives — stateful.

Excluded primitives go onto a list for Phase 3 (axis B).

### Output

`audit_annotations_test.go` at repo root — a test (not a binary) so it runs under `make test`. Deliberately report-only in this phase: logs every finding via `t.Log` and never calls `t.Error`/`t.Fatal`. Findings are categorized (type-mismatch, value-mismatch, eval-error, expected-unparseable) with per-finding detail (primitive name, call, declared type, actual type, expected literal, actual value). Promotion of specific categories to hard failures is a separate downstream decision once Phase 2 triage shapes expectations.

### What Phase 1 will *not* catch

- Branches not exercised by any docstring example. Example: `procedure-arity`'s `+` and `car` examples only cover the pair case; the integer case (composable continuations) has no example, so the harness can't narrow the bug. Phase 3 addresses this.
- Primitives with no examples. Quick count expected: most arithmetic / list / vector primitives have examples; some ports and reflection primitives don't. Phase 1's output includes the coverage gap.

---

## 5. Success criteria for Phase 1

- Harness runs under `make test`.
- Report categorizes every one of the ~421 specs into: `verified`, `doc-error`, `annotation-error`, `no-examples`, `excluded-side-effect`, `unparseable-example`.
- Zero `annotation-error` findings in `registry/core/` — the core primitives are the load-bearing ones.

---

## 6. Open decision — union-type representation

**This is the real design question and the plan needs your call before Phase 2.**

The `TypeConstraint` system has 27 concrete `ValueType` constants but **no union constructor**. `values/value_type.go:52-80`. So when Phase 1 exposes a primitive whose sound narrow type is "pair-or-integer-or-`#f`" (the `procedure-arity` pattern), the fix options are:

| Option | Example | Trade-off |
|--------|---------|-----------|
| **Widen to `TypeAny`** | current fix for `procedure-arity` | Sound, zero code change, but throws away all type info — a Phase-2 compile-time checker learns nothing from these specs. |
| **Set `ReturnType: nil`** ("unspecified") | what the code treats as "no annotation" | Equivalent to `TypeAny` for Phase-2 checking; clearer *intent* ("we deliberately can't annotate this") vs. *sloppiness*. But `nil` currently also means "nobody bothered" — two meanings collide. |
| **Introduce `TypeUnion`** | `values.Union(TypePair, TypeInteger, TypeBoolean)` | Actually useful to Phase-2. Costs: new `TypeConstraint` implementation, `Name()` like `"pair|integer|boolean"`, `Check()` is short-circuit disjunction, and every consumer of `TypeConstraint` must handle it. Aligns with direction if we ever want real type info. |
| **Introduce `TypeMaybe(T)`** (special case) | `Maybe(TypePair)` meaning `pair \| #f` | Handles the common `T-or-#f` pattern cheaply. Doesn't solve the general union problem (`procedure-arity` returns 4 shapes). |

Phase 2's triage output will tell us how many specs need which option. If the answer is "five specs" we ship `TypeAny`. If the answer is "forty specs and most are `T \| #f`" we ship `TypeMaybe`. If the answer is "broad disjunctions everywhere" we ship `TypeUnion`.

**What this plan asks you to decide now**: whether Phase 2 should be allowed to **propose** a `TypeConstraint` extension, or whether the audit is strictly annotation-corrections with the existing type vocabulary. Bounding the scope up front affects what "done" means.

**Decision (2026-04-19)**: Option A — widen to `TypeAny` for all audit-discovered unions. Phase 2's output will still enumerate the cases where `TypeAny` was chosen because no narrower sound type was expressible; that enumeration feeds a **separate** future plan on extending `TypeConstraint` (union / maybe / etc). Scope of this audit is strictly annotation corrections.

---

## 7. Meta: what the audit is actually measuring

Not "are annotations right." That framing is too narrow.

The audit measures: **how much of the primitive surface is describable in the current `TypeConstraint` vocabulary, and where are the gaps?** The primitives that can't be honestly annotated are a specification of what the type system is missing. That spec is more valuable than the annotation corrections themselves, because it feeds Phase-2 design (extension contracts).

Frame Phase 2's triage output as two parallel deliverables:

1. A list of annotation corrections (the cheap win).
2. A list of type-system gaps (the strategic win — input to "Extension API contracts Phase 2+").

---

## 8. Deliverables

- `audit_annotations_test.go` (Phase 1).
- `plans/2026-04-19-audit-findings-phase1.md` — Phase 2 output, categorized.
- Follow-up plan files per Phase 4 category as needed.
- Decision record in this file's §6 once you pick the union-type direction.

---

## 9. Non-goals

- Fixing every non-core annotation in one PR. Categories ship separately.
- Touching Phase-2 compile-time checking. That's downstream.
- Retrofitting docstrings to cover uncovered branches. That's a documentation task, not an audit task.
