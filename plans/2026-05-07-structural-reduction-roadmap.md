# Structural reduction roadmap

**Date**: 2026-05-07
**Source**: Cross-package coupling/LOC inventory after `internal/` + `machine/` analyses
**Status**: Planning — selects the next packages to subject to `/structural-reduction`
**Priority**: **Top priority** — sets context for both existing audit plans

## Why this plan exists

Two packages have already been analyzed:

- `plans/2026-05-06-machine-structural-reduction.md` (`machine/`, 7 findings + 3 opportunities)
- `plans/2026-05-07-internal-structural-reduction.md` (`internal/`, 7 findings + 4 opportunities)

Both are in TODO.md Tier 5 awaiting implementation. Before either is scheduled,
this roadmap **establishes the wider context**: which packages remain unaudited,
what their structural risk profile looks like, and which lens applies to each.
Sequencing decisions for the existing plans should reference this document so
the implementation order doesn't accidentally invalidate findings made before
adjacent packages have been examined.

## Selection criteria

Structural-reduction analysis is most valuable when **three** levers align:

1. **Size**: enough LOC and file count to have accumulated structure (≥ ~1K LOC,
   ≥ ~5 files). Below that, there's nothing to reduce.
2. **Afferent coupling (`Ca`)**: enough dependents that improvements compound.
   Low `Ca` → low blast radius → low payoff.
3. **Type/algebra surface**: the package must encode a non-trivial relationship
   (sum-types-as-products, hierarchies, registries, lattices). Structural
   reduction is fundamentally about **type algebra**, not raw code volume.

Different lenses apply to different packages. Don't apply structural-reduction
universally — it's a hammer for a specific nail. The right lens for each
package is recommended below.

## Inventory (all non-test, non-binary Go packages)

Computed 2026-05-07 via `wc` + grep on `import` statements.

```
Package                          Ca    Ce    LOC      Files   Status
────────────────────────────────────────────────────────────────────
values/                          33     4   11084     61      Tier A
machine/                          —     —    ~36000   139     Done (plan exists)
registry/core/                    2    15    6947     53      Tier C (conformance)
internal/                         —     —    ~14000   76      Done (plan exists)
.  (root, wile/Engine API)        0    21    3870     19      Tier B (API design)
environment/                     16     5    2719     11      Tier A
repl/                             1     7    2072      8      Tier B
registry/helpers/                14     6    1798     12      Tier B
extensions/math/                  2     6    1639      7      Tier C (conformance)
registry/                        19     9    1400      8      Tier A
extensions/threads/               2     6     865      3      Tier C (batch)
extensions/charsets/              1     8     858      3      Tier C (already refactored, see plan)
extensions/eval/                  2    11     853      3      Tier C (batch)
extensions/gointerop/             2     5     779      3      Tier C (batch)
extensions/files/                 2     6     509      4      Tier C (batch)
registry/testhelpers/             0    10     481      4      None (test-only)
security/                         8     2     437     10      Tier C (signals lens)
werr/                            32     1     410      1      None (trivial)
extensions/introspection/         1     9     393      3      Tier C (batch)
extensions/process/               2     6     355      3      Tier C (batch)
extensions/system/                3     5     220      3      Tier C (batch)
docparse/                         3     2     182      1      None (single-file)
examples/embedding/               0     1     132      1      None (example)
values/valuestest/                0     2      82      1      None (test helper)
```

## Tier A — strong structural-reduction candidates

These are the packages where the structural-reduction lens will produce the
highest insight-per-effort. Sequence in this order: each is more foundational
than the next.

### A.1 — `values/`  (11K LOC, Ca=33, 61 files)

**Highest-priority candidate.** The most-depended-on package in the codebase.

**Expected findings**:
- **Numeric tower tightness**: 8+ concrete numeric types (`Integer`, `Float`,
  `Rational`, `Complex`, `BigInteger`, `BigFloat`, `BigComplex`, `Byte`).
  `numeric_kind.go` is a flat enum (12+ variants) over a structurally rich
  algebra. The "ADDING A NEW NUMERIC TYPE" guide comment at
  `values/numeric_kind.go` flags 12 update points — a coupling smell that
  asks for either a sum type or a typeclass-style table-driven dispatch.
- **Port hierarchy collapsibility**: 8 port types (`*InputPort`,
  `*OutputPort`, byte/character variants, in-memory vs. file-backed). Sum
  type vs. interface-method-set trade-off worth examining.
- **Registration mechanism guides**: the package has multiple "ADDING A
  NEW X" guide comments in source (7 items in `values/values.go` for the
  Value-type guide, 12 items in `values/numeric_kind.go` for the numeric
  guide). When a guide says "edit these 7 places," that's a missing
  abstraction — every guide is a candidate.
- **Tuple/Pair migration completeness**: similar to the
  `*SyntaxPair`/`SyntaxEmptyList` finding in `internal/`, the `values/`
  side completed a parallel migration. Worth confirming no defensive guards
  remain.

**Lens**: `/structural-reduction ./values`

**Why first**: Every improvement here compounds across 33 dependents. Any
finding in `internal/` Finding 1 (the `SyntaxPair` migration) has a parallel
in `values/` that should be confirmed *closed*, since the migration story
references `values/` as the precedent.

**Estimated analysis effort**: M (large package, but well-organized; one file
per type makes scanning fast).

### A.2 — `environment/`  (2.7K LOC, Ca=16, 11 files)

**Second-priority candidate.** The binding-resolution algebra.

**Expected findings**:
- **Multiple frame types**: `EnvironmentFrame`, `GlobalEnvironmentFrame`,
  `LocalEnvironmentFrame`, `Namespace`, `PhaseRegistry` — 5 frame-like
  abstractions. Likely sum-type or interface tightness opportunities.
- **Recent migration seams**: PR #544 renamed `TopLevelEnvironment → Namespace`
  and moved registry/authorizer onto `Namespace`. Migrations like that often
  leave shadows — old method names, redundant accessors, transitional types.
- **`Binding`, `BindingID`, `BindingType`**: three related types that may
  encode the same concept at different layers (similar to the
  `bindingIdentity` redundancy in `internal/validate`).
- **`FileResolver`** in `environment/` despite source-loading being a peer
  concern (`docs/embedding/source-loading.md` documents 4 implementations) —
  worth checking if the package boundary is right.

**Lens**: `/structural-reduction ./environment`

**Why second**: Binding lookup is on the hot path of every variable
reference. Structural debt here is paid by the VM at runtime. `Ca=16` means
every consumer benefits.

**Estimated analysis effort**: S (only 11 files; should be a 1-pass read).

### A.3 — `registry/`  (1.4K LOC, Ca=19, 8 files)

**Third-priority candidate.** The contract surface for everything pluggable.

**Expected findings**:
- **`PrimitiveSpec` field saturation**: `PrimitiveSpec` is the central
  registration record. With recent additions (`Keywords`, `ParamTypes`,
  `Category`, `IsVariadic`, `Phase`, etc.), the struct may have grown to a
  product type encoding multiple orthogonal concerns. Worth tightening to a
  clearer composition.
- **`Phase` enum vs. `Registry` separation**: `PhaseRuntime`, `PhaseExpand`,
  `PhaseCompile` — does the registry treat these uniformly, or is there
  asymmetry?
- **Variadic convention**: `ParamCount: 0 + IsVariadic: true` *panics*
  at registration (verified in `registry/registry.go`'s
  `validateParamTypes`). That's a runtime invariant that the type system
  doesn't enforce — exactly the kind of thing structural-reduction is
  supposed to surface.
- **`Contract`** (`registry/contract.go`): how does it relate to
  `PrimitiveSpec.ParamTypes`? Two ways to express constraints suggests
  consolidation opportunity.

**Lens**: `/structural-reduction ./registry`

**Why third**: 19 dependents — every primitive author. Tightening the
contract surface here propagates to ~500 primitives.

**Estimated analysis effort**: S (small but dense; expect to read all 8
files closely).

## Tier B — second-priority, structural-reduction still appropriate

### B.1 — `wile/` root (3.9K LOC, 19 files, Ca=0)

**Different value proposition**: nobody depends on it (Ca=0), but it's the
**embedding API** — what every external user sees. Worth analyzing for
**API design quality** rather than internal coupling.

**Expected findings**:
- **`Engine` field saturation**: similar to `MachineContext` in
  `machine/`. With 19 files of public surface (`Engine`, `Expression`,
  `FFI*`, `Profile`, `Sandbox`, `Compiled`, etc.), expect grouped-field
  opportunities.
- **`WithX` option proliferation**: option-functions are good, but if there
  are 20+ of them, are some redundant? Are some really configurations of
  others?
- **`profile.go` + `sandbox.go` coupling**: `WithProfile(p)` and
  `WithSandbox()` are documented as orthogonal modifiers (see
  `plans/2026-03-26-environment-profiles-impl.md`). Confirm the
  orthogonality is real and not an accident of evolution.
- **FFI surface**: 4 files (`ffi.go`, `ffi_arg_converters.go`,
  `ffi_ret_converters.go`, `ffi_wrapper.go`) — likely has hand-unrolled
  conversion patterns.

**Lens**: `/structural-reduction .` (or named explicitly)

**Why B not A**: Ca=0 means improvements help only future API consumers,
not internal code. Still worth doing because it's the *first impression* for
every new embedder.

**Estimated analysis effort**: M.

### B.2 — `repl/`  (2K LOC, Ca=1, 8 files)

Self-contained component (completer, debug, doc, meta, pager,
doc_provider). Embedder-facing. Lower Ca means lower compound impact, but
the package is moderately complex and pleasingly self-contained — a quick
analysis would deliver a clean component improvement.

**Lens**: `/structural-reduction ./repl`

**Estimated analysis effort**: S.

### B.3 — `registry/helpers/`  (1.8K LOC, Ca=14, 12 files)

Argument-conversion helpers used across `registry/core` and extensions.
`Ca=14` amplifies any cleanup. Likely candidate for "hand-unrolled
extract-and-validate patterns" findings — exactly the shape that
structural-reduction surfaces.

**Lens**: `/structural-reduction ./registry/helpers`

**Estimated analysis effort**: S.

## Tier C — better with a different lens

These packages are not structural-reduction targets. Use the recommended
lens instead.

| Package | LOC | Recommended lens | Rationale |
|---|---|---|---|
| `registry/core/` | 6.9K, 53 files | **`scheme-conformance`** | Mostly R7RS primitive implementations. Big LOC but flat structure (one file per category). Conformance-checking is the right question. |
| `extensions/math/` | 1.6K | **`scheme-conformance`** + signals-engineer | Transcendentals: `sin`, `cos`, `expt`. Question is correctness across number types and edge cases (NaN/inf), not structure. |
| `security/` | 437 | **`signals-engineer`** | Authorization gates: `Ca=8` but the structural shape is fixed by the security model. Real questions are about completeness of gate placement and bypass surfaces — a reliability lens. |
| `extensions/{eval,files,threads,gointerop,charsets,system,process,introspection}` | 200–900 each | **batch `staff-engineer` sweep** | 8 small, similar packages. Better as a group ("are these consistent? what shared abstractions are missing?") than individually. |
| `werr/` | 410, Ca=32 | **None** | Highest Ca but trivially structured: a flat sentinel list + `WrapForeignErrorf`. Don't over-engineer. |
| `docparse/` | 182 | **None** | Single-file utility. Too small. |

## Sequencing recommendation

1. **`/structural-reduction ./values`** — start here. Highest Ca, highest LOC,
   contains the numeric tower and port hierarchy. Findings here may reframe
   `internal/` Finding 1 (the `SyntaxPair`/`SyntaxEmptyList` migration story
   references `values/` as precedent — confirm closure of the parallel
   migration).
2. **`/structural-reduction ./environment`** — binding-resolution algebra.
   Recent namespace migration (PR #544) likely left seams.
3. **`/structural-reduction ./registry`** — contract surface for ~500
   primitives.
4. **`/structural-reduction .`** — embedding API design quality.
5. Tier B leftovers (`repl/`, `registry/helpers/`) opportunistically.
6. Tier C with appropriate lens.

After these analyses, **then** sequence implementation of the existing two
plans (`machine/` and `internal/`) — informed by what the foundational
analyses revealed about `values/`, `environment/`, and `registry/`.

## Why this is gating

The implementation phasing in
`plans/2026-05-06-machine-structural-reduction.md` (Phase 7: named
sub-records for `MachineContext`) and
`plans/2026-05-07-internal-structural-reduction.md` (Phase 7: finish the
`SyntaxPair`/`SyntaxEmptyList` migration) both touch types that *cross*
into `values/` and `environment/`. Specifically:

- The `internal/syntax/` Finding 1 migration follows a precedent set in
  `values/`. If the precedent has gaps, fixing them first prevents
  the `internal/` migration from re-exporting a flawed pattern.
- The `machine/` Finding 7 (sub-record extraction) interacts with how
  `EnvironmentFrame` is held on `MachineContext`. If `environment/` analysis
  finds that the frame types should be re-shaped, the sub-record boundaries
  for `MachineContext` need to be drawn after that decision.

Running the foundational analyses first lets the implementation phases of
the two existing plans absorb context they don't currently have.

## Cross-references

- `plans/2026-05-06-machine-structural-reduction.md` — `machine/` analysis
  (awaiting implementation; should incorporate findings from
  Tier A.2 `environment/` before starting Phase 7).
- `plans/2026-05-07-internal-structural-reduction.md` — `internal/`
  analysis (awaiting implementation; should incorporate findings from
  Tier A.1 `values/` before starting Phase 7).
- `plans/2026-05-05-charsets-structural-refactor.md` — prior point-fix on
  one extension package; a related but narrower precedent.
- The "ADDING A NEW X" guide comments in source flag known multi-site
  update obligations (5 in total: value type at `values/values.go`,
  numeric type at `values/numeric_kind.go`, extension at
  `internal/bootstrap/bootstrap.go`, special form at
  `internal/validate/register.go`, core primitive at
  `registry/core/register.go`). Each guide is a candidate "missing
  abstraction" for the corresponding package's analysis.
- `TODO.md` Tier 5 "FCA-Derived" — the `vmCore sub-struct extraction` item
  is a peer to `machine/` Finding 7 and should be sequenced together.
