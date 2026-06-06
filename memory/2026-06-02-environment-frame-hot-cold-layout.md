# EnvironmentFrame Hot/Cold Field Layout Reduction

**Branch**: `chore/env-phases-invariant-guard` (was `perf/env-slimming`)
**Date**: 2026-06-02
**Status**: **Closed — Phase 0 done; Phases 1–3 NOT pursued.** The performance case
died under Phase-0 measurement (the VM pools env frames, so the hot path is already
zero-alloc); the *structural* case for Phase 1 died too, because its enabling
invariant is **false** (see "The enabling invariant" below — `phases` is NOT
redundant with `namespace.phases`). What shipped: the Phase-0 size guard + copy-cost
benchmarks, and a regression test pinning the false invariant
(`TestNamespace_ChildRuntimePhasesNotFoldable` in `environment/namespace_test.go`).
**Related**: `memory/2026-05-09-environment-structural-reduction.md` (Finding 1 deletes
`GlobalEnvironmentFrame.namespace`; this plan is the per-frame-layout sibling).

## Motivation

`EnvironmentFrame` is allocated/reset on **every closure application** (pooled via
`FreeList`, see `NewApplyFrame`/`InitApplyFrame`/`ResetForPool`) and is walked on
**every variable reference**. It is the single hottest heap object in the VM after
`Binding`. Shrinking it and cutting its per-apply copy cost is a direct VM win.

## Evidence (wile-goast)

Measured with `go-ssa-field-index` over `environment/`, cross-referenced against the
VM methods the bytecode dispatch loop actually calls (`machine/` grep). Static
site-counts are nearly uniform across all six fields (13–24 sites each) and are
**not** a hotness signal; hotness only emerges once accesses are weighted by which
methods run per-instruction versus per-call versus at setup.

| Field        | Tier      | Read by (per-instruction VM path)                                  |
|--------------|-----------|---------------------------------------------------------------------|
| `parent`     | 🔥 hottest | `resolveLocal`, `resolveGlobal`, `GetLocalBindingBySlotDepth`, `Parent` (25 machine sites), `SetLocalValueBySlotDepth` |
| `local`      | 🔥 hot     | `resolveLocal`, `GetLocalBindingBySlotDepth`, `GetLocalBinding`, `LocalEnvironment` |
| `global`     | 🔥 hot     | `resolveGlobal` (reads 5×), `GetGlobalBinding`                       |
| `phaseLevel` | ❄️ cold    | **zero** lookup-path methods — only `NewApplyFrame`/`InitApplyFrame` bulk-copy + phase setup |
| `phases`     | ❄️ cold    | **zero** lookup-path methods — only bulk-copy + `AtPhase`/`PhaseRegistry` ops |
| `namespace`  | ❄️ cold    | **zero** lookup-path methods — only bulk-copy + namespace/library loading |

The dividing line is sharp: `resolveLocal`, `resolveGlobal`, `GetLocalBindingBySlotDepth`,
and `Parent` — the methods executed per bytecode instruction — read **only** `parent`,
`local`, `global`. The three cold fields surface exclusively in frame construction and
cold plumbing.

## The enabling invariant

`NewApplyFrame`, `InitApplyFrame`, and `NewEnvironmentFrameWithParent` all set the child's
`global`/`phaseLevel`/`phases`/`namespace` **directly from `parent`**, unchanged. So all
four are **invariant along a phase's lexical frame chain**. Only `parent` and `local`
vary per frame.

A *new* `global`/`phaseLevel`/`namespace` triple is allocated only where a new phase or
isolated runtime is born — three cold sites: `createPhaseEnv`, `initRuntimeFrame`,
`NewChildRuntime`. (`global` differs across the phase→runtime boundary, which is why
`resolveGlobal` legitimately walks the chain; see "Why global stays reachable" below.)

Two further facts confirmed by wile-goast:
- ~~**`phases` is redundant with `namespace.phases`.**~~ **FALSE — see correction
  below.** The original claim observed that `phases` and `namespace` are co-written in
  the 7 constructors as `(phases=registry, namespace=registry.owner)`, and inferred
  `namespace.phases == phases`. That inference does not hold.
- **`phaseLevel` is `int8`** but occupies an 8-byte slot after alignment.

### CORRECTION (2026-06-02): `phases` is NOT redundant with `namespace.phases`

The plan conflated two distinct facts:

- `frame.phases.owner == frame.namespace` ✓ — true everywhere (this is what was observed).
- `frame.namespace.phases == frame.phases` ✗ — **false for child-runtime frames.**

`NewChildRuntime` (`environment/namespace.go:718`, used for library loading) reuses the
**parent** `Namespace` (for shared syntax interning) but allocates its **own**
`PhaseRegistry`:

```go
runtime := &EnvironmentFrame{ ..., namespace: p }      // shares the parent Namespace
runtime.phases = newPhaseRegistryForChild(p, runtime)  // but gets a FRESH registry
```

So `child.namespace.phases` is the **importer's** registry, while `child.phases` is the
library's isolated one — they differ. `registry.owner` being the namespace does *not*
imply `namespace.phases == registry`; the namespace's own `phases` field points at the
namespace's own registry, which for a child runtime is the parent's.

`AtPhase` (`environment_frame.go:268`) reads the `phases` field directly. Deriving it
from `namespace.phases` would route a library's macro-expansion phase environments into
the importer's registry — collapsing exactly the isolation `NewChildRuntime` exists to
provide. Empirically confirmed (probe, since removed):

```
runtime:       phases == namespace.phases   (same registry)
CHILD RUNTIME: phases ≠ namespace.phases     (distinct registries)
AtPhase(Expand) via field ≠ via namespace    (deriving would change behavior)
```

Pinned permanently by `TestNamespace_ChildRuntimePhasesNotFoldable`.

**Consequence:** Phase 1 (drop `phases`) is unsafe as written. Phase 2's Design A is
also unsafe — it derives `phases` via `p.meta.namespace.phases`, the same false step.
The only correctness-preserving structural variant is a `frameMeta` that keeps `phases`
**explicit** (`{phases, namespace, phaseLevel}` behind one pointer, not derived). That
still shrinks the frame ~16 B, but Phase 0 already showed the shrink is below noise on
the pooled hot path — so it is not worth pursuing on performance grounds, and carries
no correctness benefit. Phases 1–3 are therefore **not pursued.**

## Current vs proposed layout

```
Current EnvironmentFrame  ── ≈80 bytes, 4 cold-field copies per apply
┌──────────────────────────────────────────────────┐
│ parent      *EnvironmentFrame          8   🔥     │
│ local       LocalEnvironmentFrame     40   🔥     │  keys(8)+bindings(24)+keysShared(1+pad)
│ global      *GlobalEnvironmentFrame    8   🔥     │
│ phaseLevel  Phase (int8 + 7 pad)       8   ❄️     │
│ phases      *PhaseRegistry             8   ❄️     │
│ namespace   *Namespace                 8   ❄️     │
└──────────────────────────────────────────────────┘
```

### Design A (recommended) — hoist the 3 cold fields, keep all 3 hot fields direct

Extract a per-phase shared header; drop `phases` (derive from `namespace.phases`).

```
type frameMeta struct {            // one per phase-env instance (cold to allocate)
    namespace  *Namespace          // → namespace.phases replaces the phases field
    phaseLevel Phase
}

Proposed EnvironmentFrame ── ≈64 bytes, 2-field copy per apply
┌──────────────────────────────────────────────────┐
│ parent  *EnvironmentFrame              8   🔥     │
│ local   LocalEnvironmentFrame         40   🔥     │
│ global  *GlobalEnvironmentFrame        8   🔥     │
│ meta    *frameMeta                     8   (shared)│
└──────────────────────────────────────────────────┘
```

- **Saves ≈16 bytes/frame** (3 fields → 1 pointer).
- **Per-apply copy** drops from `q.global,q.phaseLevel,q.phases,q.namespace` (4) to
  `q.global, q.meta` (2). Both still inherited unchanged from parent (same phase).
- **All hot fields stay direct** — no new indirection on any lookup-path read. This
  honors the measurement: `global` is hot, so it does not move behind a pointer.
- `PhaseLevel()` → `p.meta.phaseLevel`; `Namespace()` → `p.meta.namespace`;
  `phases` uses → `p.meta.namespace.phases`. All cold call sites.

### Design B (aggressive follow-up) — also fold `global` into `frameMeta`

`global` is per-phase invariant too, so it *could* live in `frameMeta`, yielding
`{parent, local, meta}` ≈56 bytes and a 1-field apply copy. **Trade-off:** every
`resolveGlobal` step becomes `ge.meta.global` (one extra indirection on a hot path).
Defer unless Phase-0 benchmarks show the extra 8 bytes dominate; the global lookup
already pays a mutex `RLock` + map probe, so the indirection is likely noise — but
that is an empirical question, not an assumption.

### Why `global` stays reachable per-frame (correctness)

`resolveGlobal` walks `parent` because phase-env roots own a *different* global than the
runtime root they parent to (`createPhaseEnv`). Under both designs each phase's frames
carry that phase's `global` (Design A: direct field; Design B: via its own `meta`), so
the chain walk reads the correct global at each level. The invariant is preserved, not
weakened.

## Phased implementation

### Phase 0 — Measurement gate (DONE — 2026-06-02)
Memory lesson: *micro-benchmarks mislead; profile end-to-end; profile BEFORE committing.*

Artifacts landed:
- `environment/layout_size_test.go` — `TestEnvironmentFrameLayout` (size regression guard).
- `environment/environment_bench_test.go` — `BenchmarkApplyFrameCopyCost` (pure copy,
  zero-alloc) + `BenchmarkNewApplyFrame/bindings=0` (alloc + copy).

**Measured (darwin/arm64, this machine):**

| Struct | sizeof |
|--------|--------|
| `EnvironmentFrame` | **80 B** (matches estimate) |
| `LocalEnvironmentFrame` | 40 B |
| `GlobalEnvironmentFrame` | 56 B |
| `Binding` | 32 B |

| Apply path | ns/op | B/op | allocs |
|------------|-------|------|--------|
| `NewApplyFrame` n=0 (alloc + copy) | 18.3 | 80 | 1 |
| `NewApplyFrame` n=1 | 30.5 | 112 | 2 |
| `ApplyFrameCopyCost` n=0 (**pooled, pure copy**) | **2.69** | 0 | 0 |
| `ApplyFrameCopyCost` n=1 | 3.21 | 0 | 0 |
| `ApplyFrameCopyCost` n=5 | 5.33 | 0 | 0 |

**Decomposition of the per-apply cost at n=0:** ~2.7 ns pure field-copy +
~15.6 ns struct allocation. **Allocation dominates (~85%) — *when the frame is
allocated*.**

**But the VM pools env frames.** Hot apply uses `InitApplyFrame` into a pooled
`*EnvironmentFrame` (`machine/pool.go` `envFramePool`, `mc.envPooled=true`). The pool
is a GC-surviving freelist: per its own comment, *"after warmup every acquire is a
hit."* So in steady-state recursion the real per-apply cost is the **2.69 ns zero-alloc
copy path**, not the 18 ns allocating path. The allocating path fires only on pool-cold
misses (warmup, new recursion depth).

### Verdict — the *performance* case for the layout change is weak

- Steady-state apply already pays **0 allocations** and **2.69 ns** of copy. The layout
  change (Design A) drops the cold-field copy from 3 assignments to 1 pointer — saving
  **at most ~1 ns/apply**, likely less. Below noise on end-to-end Gabriel/extended.
- The 80→64 B shrink does **not** speed the pooled hot path (no allocation there). It
  reduces (a) pool-resident memory (~16 B × live recursion depth) and (b) the cold
  non-pooled apply path (compile/top-level wrappers, `machine_context_apply.go:67`),
  which is rare.
- This is precisely the "micro-benchmark would have misled us; the pool already solved
  the allocation problem" pattern from `memory/`. **Recommendation: do not pursue
  Phase 2/3 as a perf optimization.**

**What the change is still good for (structural, not speed):** a smaller, clearer
`EnvironmentFrame` with an explicit hot/cold split, less pool memory, and one fewer
field (`phases`, Phase 1) — fold it into the structural-reduction effort
(`memory/2026-05-09-environment-structural-reduction.md`), not the `perf/env-slimming`
performance goal. Phase 1 (drop redundant `phases`) is the only step with a clean
cost/benefit: zero hot-path risk, removes a denormalized field.

Original Phase 0 steps 2–3 (end-to-end `make bench-gabriel`/`bench-extended` + alloc
profile) remain worth running *if* Phase 1 lands, to confirm no regression — but are not
needed to justify *stopping* the perf-motivated layout change.

### Phase 1 — Drop the redundant `phases` field (lowest risk, independent win)

> ⛔ **UNSAFE — do not implement.** Its premise (`phases` derivable from
> `namespace.phases`) is false for child-runtime frames; see the CORRECTION above.
> Folding `phases` would collapse library phase isolation.

1. Delete `phases *PhaseRegistry` from `EnvironmentFrame`.
2. Replace the 8 writers' `phases: …` with nothing; replace `p.phases` reads with
   `p.namespace.phases` (guarding the `newEnvironmentFrame` nil-namespace isolated case —
   it already panics on `AtPhase`, so a nil-namespace check preserves behavior).
3. `make lint && make covercheck`; re-assert `unsafe.Sizeof` (−8 bytes expected after
   repacking, or neutral if it lands in former padding).

### Phase 2 — Extract `frameMeta` (Design A)

> ⛔ **UNSAFE as written — do not implement Design A.** It derives `phases` via
> `p.meta.namespace.phases` (step 3), the same false step as Phase 1. A
> correctness-preserving variant must keep `phases` explicit in `frameMeta`
> (`{phases, namespace, phaseLevel}`); but per Phase 0 the resulting shrink is
> below noise on the pooled hot path, so it is not pursued.

1. Add `frameMeta{namespace, phaseLevel}`; allocate one in each of the 3 cold birth
   sites (`createPhaseEnv`, `initRuntimeFrame`, `NewChildRuntime`).
2. Replace `phaseLevel`/`namespace` fields with `meta *frameMeta`. Inheriting
   constructors set `q.meta = parent.meta`.
3. Forward accessors: `PhaseLevel()`, `Namespace()`, and `AtPhase`/`Runtime`/`Expand`/
   `Compile`.
4. Audit `ResetForPool` (`*p = EnvironmentFrame{}` — meta pointer is cleared, correct,
   since `InitApplyFrame` re-sets it from parent) and `Copy` (shares meta — same phase).
5. `make lint && make covercheck`; re-assert `unsafe.Sizeof` (≈64).

### Phase 3 — Re-measure & decide on Design B
1. Re-run Phase-0 benchmarks; diff. Require no regression on Gabriel/extended; expect a
   small improvement from reduced copy + smaller frame.
2. If frame size is the bottleneck and global indirection benches clean, do Design B as
   a separate change; else stop.

## Risks / watch-list
- **Pooling**: `ResetForPool` zeroes the struct; `InitApplyFrame` must re-populate `meta`
  (it currently re-populates all cold fields, so this is a 1:1 swap). Verify no pooled
  frame is read between reset and re-init.
- **Isolated envs** (`newEnvironmentFrame`): nil namespace ⇒ nil meta. Keep the existing
  "panic on `AtPhase`" contract; add nil-meta guards where the old code relied on
  `phases == nil`.
- **API surface**: `PhaseLevel()` and `Namespace()` are public; signatures unchanged,
  bodies forward to `meta`. No embedder-visible change.
- **Concurrency**: `frameMeta` is immutable after construction and shared read-only; no
  new synchronization. (`global.mu` continues to guard global bindings as today.)

## Out of scope
- `LocalEnvironmentFrame` internal layout. (Aside: its hot sub-field is `bindings`
  (34 reads); `keys` (15) is mostly compile-time; `keysShared` is the cold CoW flag.
  Worth a separate look only if Phase-3 numbers point there.)
- The structural-reduction findings in `memory/2026-05-09-environment-structural-reduction.md`
  (dead `GlobalEnvironmentFrame.namespace`, `BindingMeta` accessors). Independent; can land
  in any order, though deleting the dead global namespace field first slightly simplifies
  Phase 2's audit.
```
