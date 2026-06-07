# Sage Oracle Coverage Extension — Design Spec

- **Date:** 2026-06-07
- **Status:** Approved (design); pending implementation plan
- **Author:** (brainstormed)
- **Related:** `memory/2026-04-12-sage-algebra-validation-design.md` (original oracle design),
  `tools/sage/verify_algebra.sage` (harness), `test/wile/sage-generated/` (snapshots)

## Problem

The SageMath oracle (`tools/sage/verify_algebra.sage`, PR #643) validates Wile's
`(wile algebra)` libraries against an independent computer-algebra system by
comparing Wile's computed values to Sage's. It currently covers **6 structures**
(integer-ring, rational-field, modular-ring, powerset-lattice, boolean-semiring,
tropical-semiring) across two phases (structure value-correctness, rewriting
soundness), emitting one static `.scm` snapshot per structure.

Since the snapshots were generated (2026-04-12), the algebra library has grown to
**28 libraries**. Roughly 14 were added or substantially extended, several of
which have value-bearing operations that no independent oracle checks. The oracle
has not kept pace; coverage breadth is the gap. (The existing snapshots are *not*
stale — they pass under `cover-scm`, so covered structures have not drifted. This
spec is purely additive.)

## Goals

- Extend the Sage oracle to validate **6 additional structures** that produce
  computed values comparable to a Sage builtin or an independent reference:
  **polynomial, semiring-matrix, group, graph, heyting, interval**.
- Add Makefile ergonomics for running and regenerating the oracle.
- Preserve the harness's core tenets: static snapshots, **no Sage dependency in
  CI**, regeneration as a deliberate act.

## Non-goals (YAGNI)

- `galois` — only `gc-sound?` (an adjunction-soundness boolean); no computed value
  to oracle against Sage. Excluded.
- `order` — every operation returns a boolean; the poset *is* the `leq?` closure
  passed in, so a Sage comparison mostly re-checks the harness's own transcription.
  Predicate-agreement only; excluded this round.
- `fca` — concept-lattice ordering is non-canonical and needs heavy normalization
  for modest signal. Excluded this round.
- `symmetric-group` `group-op` — needs a 0↔1 index shift + cycle-notation bridge to
  Sage; deferred. Cyclic groups cover the clean case.
- Polynomial `gcd`/`divmod` over ℤ — Wile requires a field for these; oracle over ℚ
  only.
- Nondeterministic graph traversal ops (connected-components, bfs/dfs, bipartition,
  maximum-matching) — output order is implementation-specific; excluded (scalar and
  canonical invariants cover graph).

## Background

The harness is plain Python/Sage. Each structure is a self-contained
`validate_<name>(args)` function that (a) computes expected values in Sage or a
Python reference, then (b) either runs live (compares against
`WILE_BIN -e <code>`) or emits a `.scm` snapshot via `write_snapshot(...)`.

`write_snapshot(filename, test_name, imports, body, seed)`
(`tools/sage/verify_algebra.sage:82`) writes a complete chibi-test file:
header (Sage version, date, seed) + `(import (scheme base) (chibi test) <imports>)`
+ `(test-begin name)` + body + `(test-end)` + **`(test-exit)`**. Because it always
emits `(test-exit)`, every new snapshot **auto-gates** under
`tools/sh/cover-scm.sh` (which gates `make covercheck` on each test file's process
exit code). No extra gating work is required.

`run_phase1(args)` (`tools/sage/verify_algebra.sage:566`) calls each structure
validator and sums failures. New structure validators register here.

All 6 target structures' constructors and operations resolve under the umbrella
`(import (wile algebra))` in the standard `make build` binary — verified at runtime
on 2026-06-07. (No kitchen-sink profile needed: the chosen graph ops are pure-Scheme
combinatorial invariants, not the profile-gated `(wile algebra graph)` SCC kernel.)

## Decisions

### D1 — Structure set: the 6 clean value-oracles

polynomial, semiring-matrix, group, graph, heyting, interval. (Rationale and the
excluded structures: see Non-goals.)

### D2 — Oracle source: hybrid, labeled per file

Use a **genuine Sage builtin** wherever one exists (strongest independence); use an
**in-harness Python reference** only where Sage has no clean builtin. Each snapshot
header gains an **oracle-source label** (`Sage-builtin` | `Python-reference`) so a
reviewer sees each file's independence level at a glance. A Python reference is
still independent of Wile (it catches Wile bugs) but is weaker than a mature CAS;
the label keeps that honest.

## Per-structure oracle design

Each row emits one snapshot `test/wile/sage-generated/sage-structures-<name>-test.scm`.

### polynomial — `Sage-builtin`

- **Constructors:** `(make-poly R coeffs)` (ascending-power coeff list), base ring
  `(integer-ring)` / `(rational-field)`. `(poly-coeffs p)` returns the ascending
  coeff list; scalar accessors `poly-degree`, `poly-leading-coeff`, `poly-eval`.
- **Sage:** `R.<x> = PolynomialRing(ZZ)` / `PolynomialRing(QQ)`; `p.list()` is also
  ascending, lining up directly.
- **Ops oracled:** `poly-plus`, `poly-minus`, `poly-times`, `poly-negate`,
  `poly-derivative` (compare `poly-coeffs`); `poly-eval`, `poly-degree`,
  `poly-leading-coeff` (scalars); `poly-gcd`, `poly-divmod` **over ℚ only** (Wile
  returns monic; Sage `.monic()` to match).
- **Comparison:** literal lists for ℤ coeffs; for ℚ coeffs reuse the existing
  `format_rational_for_wile` string-trick (`verify_algebra.sage:170`).
- **Watch:** zero polynomial is `'()` with `poly-degree => -1` (PARI convention);
  confirm Sage `R(0).degree()` convention or special-case.
- **Verified example:** `(let ((R (integer-ring))) (test '(1 2 1) (poly-coeffs (poly-times (make-poly R '(1 1)) (make-poly R '(1 1))))))`.

### semiring-matrix — `hybrid`

- **Constructor:** `(semiring-matrix-from-rows S rows)`; results via
  `(semiring-matrix->rows M)` (nested lists). **Both operands must come from one
  `eq?`-shared semiring instance** — bind `(let ((S (counting-semiring))) …)` and
  build both from `S` (mul raises "semirings differ" otherwise).
- **counting-semiring → `Sage-builtin`** `Matrix(ZZ, rows)`: `semiring-matrix-mul`
  (= `A*B`), `semiring-matrix-power` (= `A^k`), `semiring-matrix-permanent`
  (= `A.permanent()`).
- **tropical / boolean → `Python-reference`:** tropical permanent (min-cost
  assignment) and mul (min-plus); boolean closure (transitive closure) and mul.
- **Comparison:** `semiring-matrix->rows` nested lists (deterministic).
- **Verified examples:** counting mul `((19 22)(43 50))`; counting permanent `10`;
  tropical permanent `3`; boolean closure `((#t #t #t)(#f #t #t)(#f #f #t))`.

### group — `hybrid`

- **Constructor:** `(cyclic-group n)` = ℤ/nℤ under addition (elements `0..n-1`).
- **cyclic → `Sage-builtin`** `Zmod(n)` / `Integers(n)`: `group-op` (= a+b mod n),
  `group-inverse` (= -a mod n), `group-order` (= n). Scalars.
- **Burnside → `Sage-builtin`:** define a concrete action (e.g. cyclic rotation on
  k-colorings of n positions); oracle `burnside-count` (scalar orbit count) against
  Sage `PermutationGroup` orbit counting.
- **orbit / stabilizer → cardinality only:** compare `(length (orbit …))` /
  `(length (stabilizer …))` against Sage orbit/stabilizer sizes (raw lists are
  discovery-ordered, not canonical).
- **Verified examples:** `(group-op (cyclic-group 5) 2 4) => 1`;
  `(group-order (cyclic-group 6)) => 6`.

### graph — `Sage-builtin`

- **Library:** `(wile algebra combinatorial-graph)` (pure-Scheme invariants), via
  the `(wile algebra)` umbrella. Presets `complete-graph`, `cycle-graph`,
  `path-graph`, `complete-bipartite-graph`, `petersen-graph` are **0-indexed**,
  matching Sage `graphs.*`.
- **Ops oracled (deterministic / canonical):**
  - `graph-spanning-tree-count` → Sage `G.spanning_trees_count()` (scalar).
  - `graph-chromatic-polynomial` → Sage `G.chromatic_polynomial()`; extract
    ascending coeff list to match Wile's format.
  - `graph-tutte-polynomial` → Sage `G.tutte_polynomial()`; extract coefficients
    into Wile's row-list format.
- **Excluded:** components, bfs/dfs, bipartition, maximum-matching (impl-specific
  order); SCC (kitchen-sink-gated + non-canonical ids).
- **Verified examples:** `(graph-spanning-tree-count (complete-graph 4)) => 16`;
  `(graph-chromatic-polynomial (cycle-graph 4)) => (0 -3 6 -4 1)`.

### heyting — `Python-reference`

- **Constructor:** `(powerset-heyting universe)` — subsets of `universe` as lists;
  join=union, meet=intersection, implies=(¬a ∪ b), negate=¬a, leq?=subset.
- **Reference:** Python powerset boolean algebra over the same universe.
- **Determinism:** Wile's set ops preserve universe/operand order. Drive the
  snapshot with a **sorted universe and sorted operands**, so set-valued results are
  universe-ordered and match a reference that emits results in the same canonical
  order. Additionally oracle `heyting-leq?` broadly (boolean, representation-free).
- **Sage note:** Sage has finite-lattice Heyting ops, but its element representation
  (indices/frozensets) mismatches Wile's symbol lists; the Python powerset reference
  is exact and simpler — hence `Python-reference`, labeled.
- **Verified example:** `(heyting-leq? (powerset-heyting '(x y z)) '(x) '(x y)) => #t`.

### interval — `Python-reference`

- **Constructor / values:** intervals are `(lo . hi)` pairs; `'interval-bot` is the
  absorbing bottom; bounds may be ±inf (via `inf+`/`inf-`/`inf*`).
- **Reference:** Python implementation of Wile's documented formulas — add
  `[lo+lo, hi+hi]`; sub `[lo-hi, hi-lo]`; mul = four-corner min/max; bottom absorbs.
- **Comparison:** `(lo . hi)` pair literals (clean, deterministic).
- **Sage note:** `RealIntervalField` doesn't model abstract-interpretation ±inf /
  bottom semantics, so a reference is used — labeled `Python-reference`.
- **Verified example:** `(interval-add '(1 . 2) '(3 . 4)) => (4 . 6)`.

## Determinism & comparison strategy

Most oracled ops are scalar or canonical (poly coeffs ascending; matrix rows; graph
invariants) → direct `(test EXPECTED (op …))`. The two list/set risks are handled by
**controlling inputs**, not runtime sorting:

- heyting: sorted universe + sorted operands ⇒ deterministic universe-ordered
  results; plus boolean `heyting-leq?`.
- group: set-valued `orbit`/`stabilizer` compared by **cardinality** only.

No new in-snapshot comparison helpers are required.

## Make targets

Two targets in `Makefile`, both depending on `build`, both **guarded** on Sage
availability (`command -v sage` — clear message and clean skip if absent), and both
**excluded from `make ci` / `make test`** (no Sage dependency in CI):

- **`make sage-verify`** — run the live oracle (both phases) against the built
  binary (`sage tools/sage/verify_algebra.sage`).
- **`make sage-snapshot`** — regenerate the static `.scm` snapshots
  (`sage tools/sage/verify_algebra.sage --snapshot`).

## Provenance & gating

- Snapshot headers keep existing fields (SageMath version, date, seed) **plus an
  oracle-source label** (D2). The `scm_header`/`write_snapshot` path is extended to
  carry and print the label.
- Gating is automatic: `write_snapshot` emits `(test-exit)`, so each new snapshot
  exits non-zero on any assertion failure and is enforced by `cover-scm.sh`.
- Regeneration remains a deliberate act (`make sage-snapshot`), committed explicitly.

## Verification loop (Sage 10.8 installed locally)

Per structure, in order:

1. Write `validate_<name>(args)`; register in `run_phase1`.
2. `make sage-verify` (or `--phase structures`) — confirm Wile == Sage/reference live.
3. `make sage-snapshot` — emit the snapshot.
4. Run the snapshot under `./dist/wile -f <file>` — confirm exit 0.
5. After all 6: `make lint && make covercheck` — confirm the full suite is green.

## Files touched

- `tools/sage/verify_algebra.sage` — +6 `validate_<name>` functions; register in
  `run_phase1`; add oracle-source label to `write_snapshot`/`scm_header`; +Python
  reference helpers (interval arithmetic, tropical/boolean matrix, heyting powerset).
- `test/wile/sage-generated/sage-structures-{polynomial,semiring-matrix,group,graph,heyting,interval}-test.scm`
  — 6 new generated snapshots.
- `Makefile` — `sage-verify`, `sage-snapshot` targets (guarded; not in CI).
- `tools/sage/README.md` — document the two targets; fix the design-doc path
  (`plans/…` → `memory/2026-04-12-sage-algebra-validation-design.md`).
- `memory/2026-04-12-sage-algebra-validation-design.md` — update the coverage table
  to list the 6 new structures.

## Implementation phasing

One structure per increment (each independent: a `validate_X` + one snapshot,
verified live then via snapshot), in ascending risk/effort order:

1. **polynomial** (cleanest Sage builtin; establishes the ℚ string-format reuse).
2. **graph** (Sage builtin invariants; coeff-list/row extraction).
3. **semiring-matrix** (counting Sage builtin + tropical/boolean reference).
4. **group** (cyclic Sage builtin + Burnside action; cardinality compares).
5. **interval** (Python reference; ±inf / bottom cases).
6. **heyting** (Python reference; canonical-input ordering).

Then the Makefile targets, README/design-doc updates, and the final
`make lint && make covercheck`.

## Risks & mitigations

| Risk | Mitigation |
|---|---|
| ℚ coefficient formatting (`1/3`) breaks literal compare | reuse existing `format_rational_for_wile` string-trick |
| heyting set-result ordering non-canonical | drive with sorted universe/operands; oracle `heyting-leq?` broadly |
| group set ops (orbit/stabilizer) order-dependent | compare cardinality only |
| graph Tutte/chromatic format mismatch | extract Sage coefficients into Wile's exact list/row shape |
| Python-reference oracles are weaker than a CAS | label each file's oracle source (D2); prefer Sage builtins |
| zero-polynomial degree convention | confirm `R(0).degree()` vs Wile `-1`; special-case if needed |

## Open questions

None — scope (D1), oracle source (D2), and make targets resolved during
brainstorming.
