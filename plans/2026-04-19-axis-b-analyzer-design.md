# Primitive Annotation Audit — Axis B Analyzer Design (Phase 3)

**Status**: Design. Not yet implemented.
**Parent**: `plans/2026-04-19-primitive-annotation-audit.md` (Phase 3 of that plan).
**Phase 1 harness**: `audit_annotations_test.go` (runs under `make test`, reports only).
**Phase 1 findings**: `plans/2026-04-19-audit-findings-phase1.md` (harness reports zero findings after cleanup PRs #669, #670).

---

## 1. Framing — why Axis B, and what it measures

Phase 1 compared docstring examples to declared `ReturnType`. That catches **axis A** drift (documentation vs annotation) but has two structural blind spots:

- 224 of 475 primitives have no docstring examples at all (251 with examples, per the current harness run).
- 75 of the remaining 403 examples invoke the primitive indirectly through a wrapper (tests the wrapper, not the primitive); 328 are self-calls the harness verified.

Axis B closes those gaps by comparing the declared annotation to the **implementation** — the Go Impl function — directly. Unlike axis A, axis B doesn't depend on human-authored examples; it enumerates every return path.

**The primary goal of Phase 3 is not to fix annotations.** Per §6 of the parent audit plan (decision 2026-04-19), annotation widenings will all go to `TypeAny` until the `TypeConstraint` vocabulary is extended. Extending that vocabulary (adding `TypeMaybe`, `TypeUnion`, parametric types) is a separate future plan — and it needs evidence about which extensions are justified by real usage.

Phase 3 produces that evidence. It measures the distribution of return-type *shapes* across all 475 primitives and categorizes each primitive by the pattern its narrowed type set takes. The categorized inventory is the artifact Tier 2 "Extension API contracts Phase 2+" consumes to make the `TypeConstraint` design decision.

A secondary goal: outright annotation bugs discovered incidentally (primitives with a single narrow type that doesn't match the declared `ReturnType` — the `bytevector-u8-ref` class) are surfaced as a sidecar list for a separate mechanical cleanup PR.

---

## 2. Non-goals

- **Not fixing annotations.** Bugs surfaced by the analyzer go to a separate sidecar file, fixed in a separate follow-up PR (Phase 3.D below).
- **Not extending `TypeConstraint`.** The §6 decision stands: widenings use `TypeAny`. Introducing `TypeMaybe` or `TypeUnion` depends on this inventory but is separate work.
- **Not auditing `ParamTypes`.** Only `ReturnType`. `ParamTypes` is a different value-flow question — values flowing *into* foreign-function arguments — and is a separate future phase.
- **Not validating against R7RS / SRFI / Racket specs.** That is axis C, addressed category-by-category in Phase 4.
- **Not running under `make test`.** The analyzer uses SSA-build over the whole wile module and is expensive. It runs on demand.

---

## 3. Success criteria

1. Every one of the 475 primitives lands in exactly one of the seven buckets (§5).
2. Re-running the analyzer on an unchanged tree produces byte-identical output (reproducibility via determinism, not persistence).
3. The inventory document names each primitive with its Impl function location (`file:line`) and the concrete types the analyzer narrowed it to, so any claim is traceable to source.
4. Primitives in the **Single** bucket whose declared `ReturnType` does not match the narrowed singleton are surfaced as a separate sidecar bug list.
5. Primitives in the **Helper-widened** bucket (confidence `widened`) are flagged for manual review; they are not type-system gaps but analysis-tool gaps.

---

## 4. Scope

All 475 primitives registered under `AllExtensions()`. Spans:

- `registry/core/*.go`
- `extensions/*/*.go` (7 public extensions: files, math, process, system, threads, gointerop, introspection)
- `internal/extensions/*/*.go` (4 internal: io, eval, namespace, all)
- `engine.go` (any engine-level registrations)

Rationale for running on all 475, not just Phase-1 blind spots: the inventory measures *distribution*. Restricting to the ~299 blind-spot prims (224 no-example + 75 wrapped) would bias the distribution. Primitives whose examples Phase 1 verified still need SSA narrowing — examples cover only exercised branches, and axis A soundness doesn't imply the annotation covers every impl path.

---

## 5. Buckets

Each primitive lands in exactly one bucket. The bucket is determined by the shape of the narrowed return-type set and the confidence level.

| Bucket | Pattern | What it informs |
|---|---|---|
| **Single** | Narrowed set is a single concrete `values.ValueType` | No type-system gap. If declared type matches, clean ✓. If not, this is an annotation bug → sidecar. |
| **Maybe(T)** | Narrowed set = `{T, *Boolean:#f}` (or `{T, *EmptyList}`) for a single `T` | Justifies `TypeMaybe(T)` — the cheapest `TypeConstraint` extension. A high count here is strong evidence for shipping `TypeMaybe` first, standalone. |
| **Narrow union** | Narrowed set has 2–3 distinct types, none of which is the `#f` pattern | Justifies enumerated `TypeUnion` with small fixed arity, or ad-hoc combined types. |
| **Broad union** | Narrowed set has 4 or more distinct types | Justifies full variadic `TypeUnion` — expensive to add (every `TypeConstraint` consumer must handle it). Only worth shipping if the count is high. |
| **Polymorphic** | Return type depends on the input type (identity, `car`, `list-ref`, `apply`) | Needs parametric/dependent types — a major investment. Count here tells us whether that work has justifying mass. |
| **Helper-widened** | At least one return path reaches an untyped boundary — the analyzer couldn't narrow it | Not a type-system gap. Flag for manual review. If >30% of prims land here, the analyzer itself needs rework (see §8 kill criteria). |
| **Side-effecting** | Returns unit/unspecified (`#!unspecified`, `*EmptyList`) on all paths, or always panics / always errors | Group and skip. No annotation to improve. |

---

## 6. Analyzer architecture

### 6.1 Where it lives

- **Manifest generator**: wile repo. Reuses the runtime enumeration Phase 1 already performs.
- **SSA analyzer**: wile-goast repo, as a script at `cmd/wile-goast/scripts/wile-axis-b.scm` (the `go:embed`-ed script directory alongside existing `unify-detect.scm` and `goast-query.scm`). Invoked as `wile-goast --run wile-axis-b`. Infrastructure (SSA build, callgraph, Go type queries) lives in wile-goast. Note: because scripts are embedded into the binary, the B PR (analyzer) must land and wile-goast must be rebuilt locally before the C PR (inventory landing) can be generated.
- **Inventory output**: wile repo, under `plans/`.

### 6.2 Two-pass discovery

1. **Runtime pass (wile repo)**: walk `Registry.Primitives()` under `AllExtensions()`. For each primitive, record `(name, declared-ReturnType, go-function-name, go-source-location)`. The Go function name is resolved via `runtime.FuncForPC(reflect.ValueOf(spec.Impl).Pointer())` and source location via `Func.FileLine(pc)` — standard Go reflection technique. Output: `plans/axis-b-manifest.scm`, an S-expression list.
2. **SSA pass (wile-goast repo)**: load the wile module via `go-ssa-build`. For each Go function in the manifest, walk the SSA to determine its narrowed return-type set.

The manifest file is the contract between the two passes. It is committed so diffs across PRs reveal new / removed / renamed primitives as review signal.

### 6.3 The narrowing analysis

Primitives don't "return" `values.Value` in the Go sense. They modify `*MachineContext` via sink calls (`mc.SetValue(v)`, `mc.PushValue(v)`, and any callable-return paths). The analyzer's core query:

> For primitive function F, what is the set of concrete `values.Value` subtypes that flow into any result-writing sink reachable from F?

Mechanics:

1. Enumerate result-writing sinks once — a small fixed set discovered by reading the VM. This is the "taint target" set.
2. For each primitive function F, walk F's SSA. At every call to a sink, record the static type of the sink's value argument.
3. When that static type is `values.Value` (the interface), follow the value's def-use chain back: through SSA phis, through helper-function returns (inter-procedural call-graph walk), until we hit either:
   - a **concrete type constructor** (e.g., `values.NewInteger`, `&values.Pair{...}`, `values.TrueValue`) — record the concrete type
   - an **untyped boundary** — a function returning `values.Value` whose own narrowing produced `TypeAny`, or an `interface{}` parameter being forwarded, or a reflect-dispatch site we can't resolve statically. Record `TypeAny` for this path and set the primitive's confidence to `widened`.
4. Paths through `panic(...)` or `return err` without reaching a sink do not contribute — they cannot affect the primitive's return type.

### 6.4 Output shape (per primitive)

Raw S-expression emitted by the analyzer:

```scheme
(primitive
  (name "car")
  (impl
    (go-function "github.com/aalpar/wile/registry/core.primCar")
    (go-source "registry/core/lists.go:42"))
  (declared-return-type "TypePair")
  (narrowed-return-types ("TypePair"))
  (confidence narrow)
  (bucket Single))
```

`confidence` is one of:
- `narrow` — every reachable sink path terminated at a concrete constructor.
- `widened` — at least one path hit an untyped boundary.
- `no-paths` — no sink path was found from F. Surfaced as a detectable failure; those primitives need human review of the sink enumeration in §6.3.1.

### 6.5 Analyzer-level limits, explicit

- Does not attempt whole-program typing of every helper in wile. Walks only from primitive roots through the callgraph. Unreachable helpers are ignored.
- Does not cache across runs. Reproducibility comes from determinism of the SSA build on a given tree, not from persistence.
- Does not attempt to resolve method dispatch on `values.Value` methods generically (`x.SchemeString()` for `x: values.Value`) — methods aren't result-writing sinks, so it doesn't need to.

### 6.6 Known risks

- **Sink enumeration completeness**: §6.3.1 posits "a small fixed set" of sinks. If the real number is higher — 15+ distinct sink patterns, or a significant fraction of primitives use obscure result-writing paths (e.g., tail-call primitives, `call/cc`-family, direct continuation mutation) — the `no-paths` confidence count will be high. This is a detectable failure mode; see §8 kill criteria.
- **Extension helper registrations**: some extension packages register primitives via helper constructors (`helpers.MakePrim(...)`). The runtime pass must chase through those helpers so the recorded Go function is the actual primitive body, not the registration helper. Detectable: if a primitive's Go function name resolves to a helper rather than a body.
- **Variadic primitives** (`IsVariadic: true`) aren't different at the return-type level — same analysis applies.

---

## 7. Output

Four artifacts: one human-readable deliverable, one analyzer source, one structured data file, one bug sidecar.

### 7.1 `plans/2026-04-19-axis-b-inventory.md` — primary deliverable

Human-readable markdown. One section per bucket. Within each bucket, primitives listed alphabetically with compact rows:

```
### Maybe(T) — N prims

| Primitive | Narrowed types | Declared | Impl |
|---|---|---|---|
| `assv` | `*Pair`, `*Boolean:#f` | `TypeAny` | `registry/core/lists.go:203` |
| `memq` | `*Pair`, `*Boolean:#f` | `TypeAny` | `registry/core/lists.go:178` |
```

Each bucket section ends with a one-paragraph interpretation: "N primitives land here. Typical pattern: lookup-may-fail. Justifies `TypeMaybe(T)` if N ≥ threshold T." Thresholds are written in once the numbers are known (i.e., during Phase 3.C).

The final section is **"Type-system recommendations"** — three to five distilled bullets for Extension Contracts Phase 2+. This is the load-bearing paragraph. Everything above it is evidence.

### 7.2 `wile-goast/cmd/wile-goast/scripts/wile-axis-b.scm` — the analyzer

Reproducible source. Runs via `wile-goast --run wile-axis-b --arg manifest=PATH [--arg output=PATH]`. Emits both the raw per-primitive data (§7.3) and the inventory markdown (§7.1).

### 7.3 `plans/axis-b-raw.scm` — structured sidecar

The full per-primitive data from §6.4, one S-expression per primitive. Anyone can re-aggregate into different buckets or queries without re-running the SSA analysis. Committed alongside the inventory.

### 7.4 `plans/2026-04-19-axis-b-annotation-bugs.md` — bug sidecar

Primitives where the **Single** bucket's narrowed set is `{T}` but the declared `ReturnType` is something else. Each entry is actionable without further design: a one-line `ReturnType:` change. Input for a separate Phase 3.D PR. Keeping it out of the inventory prevents mechanical fixes from burying the strategic signal.

### 7.5 Deliberate non-integrations

- **No `make` target**, no `make test` integration. The analyzer is expensive and runs on demand.
- **No CI gate**. If later we want regression checking, we can add a checksum comparison of `axis-b-raw.scm` — but that is a follow-up, not part of Phase 3.

### 7.6 Regeneration discipline

When a primitive is added, modified, or removed, the author re-runs the analyzer and commits the updated inventory and raw sidecar. The PR references the analyzer's git hash. If the analyzer itself changes (e.g., a bug fix in narrowing), its PR diff is the inventory churn it causes — useful review signal.

---

## 8. Sequencing & delivery

Four PRs, two repos, clean dependency chain.

### 8.A — Manifest generator (wile repo)

New code that emits `plans/axis-b-manifest.scm`. Walks `Registry.Primitives()` under `AllExtensions()`. Uses the runtime techniques Phase 1 already uses. The generator runs under `make test` (cheap — no SSA) and the manifest file is committed; diffs act as review signal for primitive churn.

Scope: ~50–100 LOC.

### 8.B — Analyzer script (wile-goast repo)

`cmd/wile-goast/scripts/wile-axis-b.scm`. Consumes the manifest; runs the SSA pass described in §6; emits `axis-b-raw.scm` and the inventory markdown. Ships with a smoke-test that runs it against a hand-picked 5-primitive subset with known expected output, so the analyzer itself has regression coverage.

Scope: few hundred lines of Scheme.

### 8.C — Inventory landing (wile repo)

First real run of (8.B) over the full 475-prim manifest. Commits:

- `plans/2026-04-19-axis-b-inventory.md` (markdown)
- `plans/axis-b-raw.scm` (structured)
- `plans/2026-04-19-axis-b-annotation-bugs.md` (bug sidecar)

The PR description contains the distilled "Type-system recommendations" paragraph. This is the artifact Extension Contracts Phase 2+ opens.

Scope: mostly generated output + curation of the distilled conclusion.

### 8.D — Annotation-bug sweep (wile repo, follow-up, optional)

Mechanical: walk the bug sidecar, change each declared `ReturnType` to match the narrowed type. One commit per cluster (core, extensions, internal extensions). Re-runs the Phase 1 audit harness afterward to verify no regressions.

Scope: proportional to sidecar length — expect 10–30 one-line changes.

### Dependency chain

```
A (manifest) ─┐
              ├──→ C (inventory landing) ──→ D (bug sweep, optional)
B (analyzer)──┘
```

A and B can proceed in parallel once the manifest file format is agreed. B's smoke-test subset can be constructed from a hand-written manifest stub before A lands, so B is not strictly blocked on A. Because wile-goast scripts are `go:embed`-ed, C requires B's script to be merged and a rebuilt wile-goast binary installed locally before it can be generated.

### Kill criteria

Checkpoint in the 8.B PR description:
- If **>30%** of primitives land in **Helper-widened** (confidence `widened`), stop and revisit. The analyzer cannot narrow enough to produce useful buckets — either the sink enumeration is incomplete, or real wile code routes too many primitives through untyped helpers, or both.
- If the **no-paths** count is non-negligible (say, >5 primitives), stop and revisit §6.3.1. A primitive with zero sink paths means the analyzer missed a result-writing site; fix the sink set and re-run.

Either trigger converts Phase 3 back into a tooling-design question before continuing.

---

## 9. Deferred / out-of-scope

- Extending `TypeConstraint` with `TypeMaybe`, `TypeUnion`, or parametric types. Gated on this inventory's findings; separate future plan.
- `ParamTypes` analysis. Separate axis (value flow into primitives) and separate future phase.
- Runtime / CI validation that the inventory stays current. Possible later via a checksum step.
- Comparing primitives against R7RS / SRFI / Racket specs. That is axis C, Phase 4 in the parent plan.
