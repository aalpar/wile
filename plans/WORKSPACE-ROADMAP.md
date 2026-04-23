# Workspace Roadmap — wile × wile-goast

**Status:** Living document. Cross-project queue + dependency graph for work that spans `wile` and `wile-goast` (the two Go modules tied by `~/projects/wile-workspace/go.work`).

**Scope:** Items where a deliverable in one repo has a consumer (or gates work) in the other. Single-repo items stay in the owning project's `TODO.md` / `plans/` — this file is strictly for cross-project coordination.

**Owner:** `wile/` is the natural coordination point because the dependency runs wile-goast → wile. If wile-goast-originated initiatives grow large enough to warrant parallel roadmapping, split this into per-initiative files under `wile/plans/workspace/`.

---

## Queue

Three buckets: **Now** (actively in progress), **Next** (sequenced to start after current work), **Later** (queued without a firm start date).

### Now

| Initiative | wile-side | wile-goast-side | Status |
|---|---|---|---|
| Algebra extraction (symbolic/abstract-domain/dataflow) | PR #705 merged (commit `afc674fa`) | PR #8 open, CI green | Awaiting merge authorization on PR #8 |

### Next — pending algebra work

Prioritized per 2026-04-22 session: Tier B two-sided matching before the lower-priority §5.7 items.

| Initiative | wile-side | wile-goast-side | Blocks |
|---|---|---|---|
| **Tier B: `(wile algebra matching)`** | New library — Gale-Shapley, Irving rotations, assignment game, many-to-one (hospital/intern), many-to-many (Kelso-Crawford substitutes). `plans/2026-04-17-algebra-foundations-directions.md` §4.6 | Not-yet-scoped: duplicate-detection consumer, symbol-matching across commits | Coverage library-tracking (below) |
| **Tier B: §4.2 Hungarian primitive** | Surface `(wile algebra matrix)`'s tropical permanent as `tropical-assignment`. `plans/2026-04-17-algebra-foundations-directions.md` §4.2 | Cross-version symbol matching for refactor-tracking / blame | — |
| **Tier B: §4.2 Max common subgraph** | Bipartite matching + branch-and-bound over §5.6 combinatorial-graph. `plans/2026-04-17-algebra-foundations-directions.md` §4.2 | True CFG-level clone detection (beyond unify.scm's aligned-tree diff) | — |
| **wile-goast AC-match migration** | Already shipped in wile (`ac-unify`). `plans/2026-04-21-wile-goast-ac-match-migration.md` | Migrate `unify.scm:421` from `discover-equivalences` to `ac-unify`; benchmark crossover; term-protocol conformance tests | — |

### Later — queued, not blocking current work

| Initiative | wile-side | wile-goast-side | Blocked by |
|---|---|---|---|
| **Coverage: library-template tracking** | `plans/2026-04-23-coverage-library-tracking.md` — extend `trackTemplateTree` to walk `CompiledLibrary.Template` via import observer. Resolves Q-a on stdlib filter behavior. ~35–95 LOC. | Scheme-coverage tests mirroring wile's `engine_coverage_test.go`. Tests currently deferred because library bodies don't show up in coverage. | Tier B (per user 2026-04-23) |
| **§5.7 Connes-Kreimer Hopf algebra** | `(wile algebra hopf)` — coproduct-cuts-subtrees on rooted trees. `plans/2026-04-17-algebra-foundations-directions.md` §5.7 | Formalize `ast-transform`/`ast-splice` in `wile-goast/cmd/wile-goast/lib/wile/goast/utils.scm` as Hopf-algebra operations | §5.7 tier |
| **§5.7 Submodular optimization** | `(wile algebra submodular)` — greedy approximation framework | wile-goast: program slicing, test-suite selection, import minimization | §5.7 tier |
| **§5.7 Matroids** | `(wile algebra matroid)` — rank, circuits, duality, intersection. Blocks Kelso-Crawford for matching many-to-many. | wile-goast: register allocation, scheduling via matroid-intersection framing | §5.7 tier |
| **§5.7 Symmetric functions / RSK** | Research-tier | LCS→LIS→RSK for `unify.scm` statement/parameter-list diff | §5.7 tier |
| **Track C4: CFL-reachability** | Needs context-free-grammar path algebra on wile-side — not yet scoped | `wile-goast/TODO.md` §C4 "context-sensitive analysis" | wile-side path algebra design |
| **Track C5: Galois connections for AI** | `(wile algebra galois)` extension — `make-abstract-domain` / `make-concrete-domain` / `gc-from-pair` per `wile-goast/TODO.md` §C5 | Connect Go concrete values to abstract domains | — |
| **gonum integration** | Ships `bench-stats/` module. `plans/2026-04-18-gonum-integration-directions.md` §5.2 | Independent track: `goastgraph/` bridge | **Independent of algebra queue — can parallel-track** |

---

## Cross-project dependency graph

```
wile deliverable ──▶ wile-goast consumer

(wile algebra unification)    ──▶  unify.scm AC-match migration [QUEUED]
(wile algebra abstract-domain) ─┐
(wile algebra dataflow)        ─┴─▶ PR #8 dataflow/domains rewire [OPEN]
(wile algebra matching)        ──▶  symbol-matching / dup-detect [NEXT]
§4.2 Hungarian primitive       ──▶  cross-version symbol matching [NEXT]
§4.2 max common subgraph       ──▶  CFG-level clone detection    [NEXT]
coverage library-tracking      ──▶  scheme-coverage tests         [LATER]
§5.7 Connes-Kreimer            ──▶  ast-transform formalization   [LATER]
§5.7 matroids                  ──▶  register allocation framing   [LATER]
§5.7 submodular                ──▶  slicing / test selection      [LATER]
§5.7 RSK                       ──▶  unify.scm diff enrichment     [LATER]
CFL-reachability path algebra  ──▶  context-sensitive analysis    [LATER]
Galois connection framework    ──▶  Go value → abstract domain    [LATER]
```

---

## How this file interacts with per-project plans

Per-project plans stay in `wile/plans/` and `wile-goast/plans/`. This file does **not** replace them — it only tracks *which* plans on each side are coupled, and what order they need to land in.

**Discipline:**

- A new plan with a cross-project dependency (wile-side shipping a consumer in wile-goast, or vice versa) gets a row in the dependency graph above, with links to both sides' plan files.
- A plan's front-matter may cite this file for "queued-after" status (e.g., the coverage plan's status says *"Queued — blocked by pending algebra Tier B, per `plans/WORKSPACE-ROADMAP.md`"*).
- When an initiative ships on one side, move its row from Next/Later down to **Shipped** (below) — don't delete; we want the history.

**Not in scope for this file:**

- Within-repo sequencing (handled by per-project `TODO.md`).
- Ideas without a paired consumer (stay in the originating repo's `TODO.md`).
- Broad roadmap-level "where is the product heading" narrative (handled by `wile/CLAUDE.md` §"Product Vision" and the algebra directions doc).

---

## Shipped

Cross-project deliverables whose wile-side and wile-goast-side both landed. Preserved for historical traceability.

| Initiative | wile-side | wile-goast-side | Completed |
|---|---|---|---|
| Algebra extraction (symbolic facade, abstract-domain, dataflow + CFG protocol) | PR #705 (commit `afc674fa`) | PR #8 (open — CI green, awaiting merge) | 2026-04-23 (partial) |

---

## Pointers

- `wile/TODO.md` — wile-side full backlog.
- `wile-goast/TODO.md` — wile-goast-side full backlog.
- `wile/plans/CLAUDE.md` — wile-side plan index.
- `wile/plans/2026-04-17-algebra-foundations-directions.md` — algebra roadmap (source for Tier B/C ordering).
- `wile/plans/2026-04-23-coverage-library-tracking.md` — coverage follow-up (cited above as [LATER]).
- `~/projects/wile-workspace/go.work` — workspace binding that makes cross-repo local development work.
