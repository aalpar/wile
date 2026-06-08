# Balanced Graph Partition — Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Status (2026-06-08):** IMPLEMENTED on branch `feat/graph-partition`. All bodies committed to
`combinatorial-graph.scm`; 4 test groups pass (326 total in the suite). The implemented code is the
authoritative source; the code blocks below are kept in sync for review.

**Engine correction (2026-06-08):** the original design specified single-vertex FM moves. That
**deadlocks**: a single move shifts `|A|-|B|` by ±2, so from a balanced even-`n` seed no move is
admissible under a tight tolerance (the default `tol=0.25` freezes `n=6`). Replaced with **true
Kernighan-Lin pair-swaps** (exchange one vertex from each side; sizes invariant). Consequence: KL
preserves the seed's A/B ratio, so `balance` bounds the **seed ratio** (validated up front) rather
than gating moves. `%movable?` was dropped; `%allowed-diff` moved to seed validation.

**Goal:** Ship `graph-partition` in `(wile algebra combinatorial-graph)` — a deterministic,
balance-preserving 2-way partition of a weighted graph via Kernighan-Lin pair-swaps —
so wile-goast's `recommend_split` can call a *real* cut primitive instead of the heuristic
mislabeled "min-cut". Pure Scheme, no new dependency.

**Architecture:** One new public operation on the existing combinatorial-graph record type
(sibling of `graph-bipartition`). The engine is **Kernighan-Lin refinement** (pair-swaps: one
vertex from each side per step, gain `D(v)+D(u)-2·w(v,u)`, sizes invariant). The seed sets the
A/B ratio and KL holds it. No new record type — the result is an alist, matching the
algebra-library convention for operation outputs.

**Tech Stack:** R7RS Scheme; `(wile algebra setoid)` plumbing (`assv-or`, `validate-opts-keys`,
`assert-procedure`, `setoid-assoc`); existing combinatorial-graph accessors (`graph-vertices`,
`graph-edges` → `(u v edge-data)` triples, `graph-neighbors` → `((neighbor . edge-data) ...)`);
`(chibi test)`; auto-discovered by `test/run-all.sh` (`find ... -name '*-test.scm'`).

**Design:** `plans/2026-06-08-balanced-graph-partition-design.md` (read §2, §4, §5 first — the
objective, the rejected-algorithm rationale, and the KL/FM decision live there).

**Key decisions carried from design (do not re-litigate):**
- Objective = **balanced cut**, NOT global/s–t min-cut (design §4). Global min-cut (Stoer-Wagner,
  Karger, FF, Dinic) is rejected; the degeneracy-guard test (Task 3) encodes *why*.
- Home = `(wile algebra combinatorial-graph)` (OQ-1), not `(wile algebra graph)`.
- Edge weight is supplied by a caller `'weight` accessor over `edge-data`; the graph type itself
  is not extended. Default weight = `1` per edge (multiplicity via parallel edges if `multi?`).
- Determinism is a hard requirement (wile-goast caches/diffs findings): fixed seed, fixed
  tie-break by adjacency order. No randomization, no restarts.

---

## File Structure

- Modify `stdlib/lib/wile/algebra/combinatorial-graph.sld` — add `graph-partition` to `(export ...)`.
- Modify `stdlib/lib/wile/algebra/combinatorial-graph.scm` — add `graph-partition` (public),
  `%kl-refine` (engine), `%partition-gain`, `%edge-weight`, `%cut-weight`, `%total-weight`,
  `%allowed-diff`, `%default-seed`, `%validate-seed`, `%side-of`, `%weight-of` (private `%`).
  (No `%movable?` — KL pair-swaps preserve balance, so no per-move balance gate is needed.)
- Modify `stdlib/lib/wile/algebra.sld` — append `graph-partition` to the umbrella `export` list
  (combinatorial-graph is already imported at `algebra.sld:349`).
- Modify `test/wile/algebra-combinatorial-graph-test.scm` — add a `graph-partition` test group.
- Modify `docs/algebra/reference.md` — add a `graph-partition` row/section; state the objective is
  *balanced cut* and explicitly note it is **not** min-cut (kill the mislabel at the doc layer).
- Modify `TODO.md` — record the shipped primitive; note Phase 2 (wile-goast rewire) and Phase 3
  (Shi-Malik, gated on gonum) as follow-ons.

---

## Task 1 — KL engine + helpers

**Files:** modify `combinatorial-graph.scm`.

- [x] **Step 1: `%weight-of` and `%cut-weight`.**

```scheme
;; Weight accessor: (weight-fn edge-data) -> non-negative number. Default 1.
(define (%weight-of weight-fn edge-data)
  (weight-fn edge-data))

;; Side lookup. `side` is an alist vertex -> 'a | 'b keyed under G's setoid S.
;; Vertices may be arbitrary atoms/strings, so look up with setoid-assoc — the
;; SAME equality the rest of combinatorial-graph.scm uses (cf. graph-neighbors
;; at combinatorial-graph.scm:195). Plain assv/assoc would be wrong for
;; setoid-typed vertices. Arg order matches the library: (setoid-assoc S key alist).
(define (%side-of S side v)
  (let ((p (setoid-assoc S v side)))
    (and p (cdr p))))

;; Total weight of edges crossing the partition. graph-edges yields each
;; undirected edge ONCE as (u v edge-data), so each crossing edge is counted
;; once. Iterate once.
(define (%cut-weight G side weight-fn)
  (let ((S (graph-setoid G)))
    (fold
      (lambda (e acc)
        (let ((u (car e)) (v (cadr e)) (d (caddr e)))
          (if (eq? (%side-of S side u) (%side-of S side v))
              acc
              (+ acc (%weight-of weight-fn d)))))
      0
      (graph-edges G))))

;; Total weight over all edges — denominator for the normalized-cut metric.
(define (%total-weight G weight-fn)
  (fold (lambda (e acc) (+ acc (%weight-of weight-fn (caddr e))))
        0
        (graph-edges G)))
```

- [x] **Step 2: `%default-seed` — deterministic balanced bipartition.**

Split `(graph-vertices G)` (adjacency-order, deterministic) into first ⌈n/2⌉ → `'a`, rest → `'b`.
Returns an alist `vertex → 'a|'b`. The consumer (Phase 2) overrides this via `'seed` with the
FCA-derived bipartition; the default exists so the primitive is callable standalone.

- [x] **Step 3: `%partition-gain`, `%edge-weight`, `%allowed-diff`.**

`%partition-gain` is the per-vertex gain `D(v)` = (opposite-side incident weight) − (same-side
incident weight). KL's *swap* gain is `D(v) + D(u) − 2·w(v,u)`, where `%edge-weight` supplies
`w(v,u)`. The self-loop branch in `%partition-gain` is a correctness requirement: a self-loop never
crosses the cut, so it contributes 0 (else swap gains would not equal the actual cut change).
`%allowed-diff` is used to **validate the seed ratio** (KL preserves it), not to gate moves —
there is no `%movable?`.

```scheme
;; D(v) = (weight to OPPOSITE side) - (weight to SAME side).
(define (%partition-gain G side weight-fn v)
  (let* ((S  (graph-setoid G))
         (my (%side-of S side v)))
    (fold
      (lambda (nbr acc)
        (let ((u (car nbr)) (d (cdr nbr)))
          (cond
            ((setoid-equiv? S u v) acc)                 ; self-loop: never crosses -> 0
            ((eq? (%side-of S side u) my)               ; internal edge
             (- acc (%weight-of weight-fn d)))
            (else                                       ; external edge
             (+ acc (%weight-of weight-fn d))))))
      0
      (graph-neighbors G v))))

;; Total weight of edges directly between v and u (sums parallel edges).
(define (%edge-weight G weight-fn S v u)
  (fold (lambda (nbr acc)
          (if (setoid-equiv? S (car nbr) u)
              (+ acc (%weight-of weight-fn (cdr nbr)))
              acc))
        0
        (graph-neighbors G v)))

;; Allowed integer size-difference under the balance tolerance:
;;   clamp(floor(tol*n), n mod 2, n-2)
;; Lower floor (n mod 2) admits the most-balanced split for odd n; upper cap
;; (n-2) keeps both groups non-empty. Used to VALIDATE the seed ratio.
(define (%allowed-diff tol n)
  (max (modulo n 2)
       (min (exact (floor (* tol n))) (- n 2))))
```

- [x] **Step 4: `%default-seed`, `%validate-seed`, and `%kl-refine` — the KL pass loop.**

A *pass* makes a sequence of pair-swaps, then keeps the best prefix:
- Unlock all. Repeatedly pick the unlocked `(v∈A, u∈B)` maximizing `D(v)+D(u)−2·w(v,u)` (D computed
  once per vertex per step; earliest-v-then-earliest-u tie-break → determinism), swap them, lock
  both, add the gain to a running cumulative total, and track the side at the maximum cumulative
  gain **online**.
- Each side contributes ≤ once per pass ⇒ ≤ min(|A|,|B|) swaps; the pass terminates. **Sizes are
  invariant under swaps**, so the seed's A/B ratio is preserved exactly — this is what fixes the
  single-vertex-FM deadlock.
- Cost: O(|A|·|B|·deg) per swap — trivial at the splitter's scale; the Go fast-path (out of scope)
  is the lever if profiling ever demands it.

The outer loop re-runs passes while one yields strictly positive cumulative gain; the partition set
is finite ⇒ termination. The `guard` is a graceful backstop (returns the current valid partition).

**Precondition:** `side0` covers every vertex — guaranteed by `%default-seed`, or by
`%validate-seed` for a caller `'seed`.

```scheme
;; Deterministic balanced seed: first ceil(n/2) vertices (adjacency order) -> 'a.
(define (%default-seed G)
  (let* ((vs (graph-vertices G)) (n (length vs)) (half (quotient (+ n 1) 2)))
    (let loop ((vs vs) (i 0) (acc '()))
      (if (null? vs) (reverse acc)
          (loop (cdr vs) (+ i 1)
                (cons (cons (car vs) (if (< i half) 'a 'b)) acc))))))

;; Validate a caller seed: full coverage with values in {a,b}, both sides
;; non-empty, imbalance within the balance tolerance (KL holds this ratio).
(define (%validate-seed G seed tol)
  (let* ((S (graph-setoid G)) (vs (graph-vertices G)) (n (length vs)))
    (for-each
      (lambda (v)
        (let ((p (setoid-assoc S v seed)))
          (unless (and p (memq (cdr p) '(a b)))
            (error "graph-partition: seed must assign every vertex a side in {a,b}"
                   (list 'fix "provide (vertex . a) or (vertex . b) for every vertex") v))))
      vs)
    (let* ((na (count (lambda (v) (eq? (%side-of S seed v) 'a)) vs)) (nb (- n na)))
      (when (or (zero? na) (zero? nb))
        (error "graph-partition: seed must place at least one vertex on each side"
               (list 'fix "both 'a and 'b sides must be non-empty")))
      (when (> (abs (- na nb)) (%allowed-diff tol n))
        (error "graph-partition: seed imbalance exceeds the balance tolerance"
               (list 'fix "raise 'balance or supply a more balanced seed")
               (list 'sizes na nb 'allowed (%allowed-diff tol n)))))
    seed))

;; Kernighan-Lin: pair-swap passes keeping the best prefix; |A|,|B| invariant.
(define (%kl-refine G side0 weight-fn)
  (let* ((S (graph-setoid G)) (vs (graph-vertices G)) (n (length vs)))

    (define (a-side side) (filter (lambda (v) (eq? (%side-of S side v) 'a)) vs))
    (define (b-side side) (filter (lambda (v) (eq? (%side-of S side v) 'b)) vs))

    (define (swap side v u)            ; v: A->B, u: B->A
      (map (lambda (e)
             (cond ((setoid-equiv? S (car e) v) (cons (car e) 'b))
                   ((setoid-equiv? S (car e) u) (cons (car e) 'a))
                   (else e)))
           side))

    (define (remove-v x lst)           ; drop first setoid-equiv to x
      (let loop ((lst lst) (acc '()))
        (cond ((null? lst) (reverse acc))
              ((setoid-equiv? S (car lst) x) (append (reverse acc) (cdr lst)))
              (else (loop (cdr lst) (cons (car lst) acc))))))

    ;; argmax over (v in A-unlocked, u in B-unlocked) of D(v)+D(u)-2w(v,u).
    (define (best-pair side a-un b-un)
      (let ((da (map (lambda (v) (cons v (%partition-gain G side weight-fn v))) a-un))
            (db (map (lambda (u) (cons u (%partition-gain G side weight-fn u))) b-un)))
        (let outer ((as da) (bv #f) (bu #f) (bg #f))
          (if (null? as) (values bv bu bg)
              (let* ((vp (car as)) (v (car vp)) (dv (cdr vp)))
                (let inner ((bs db) (bv bv) (bu bu) (bg bg))
                  (if (null? bs) (outer (cdr as) bv bu bg)
                      (let* ((up (car bs)) (u (car up)) (du (cdr up))
                             (g  (- (+ dv du) (* 2 (%edge-weight G weight-fn S v u)))))
                        (if (or (not bg) (> g bg))
                            (inner (cdr bs) v u g)
                            (inner (cdr bs) bv bu bg))))))))))

    (define (one-pass side)
      (let loop ((side side) (a-un (a-side side)) (b-un (b-side side))
                 (cum 0) (best-side side) (best-gain 0))
        (if (or (null? a-un) (null? b-un))
            (values best-side best-gain)
            (let-values (((v u g) (best-pair side a-un b-un)))
              (let* ((side* (swap side v u)) (cum* (+ cum g)))
                (if (> cum* best-gain)
                    (loop side* (remove-v v a-un) (remove-v u b-un) cum* side* cum*)
                    (loop side* (remove-v v a-un) (remove-v u b-un) cum* best-side best-gain)))))))

    (let pass ((side side0) (guard n))
      (if (<= guard 0)
          side
          (let-values (((side* gain) (one-pass side)))
            (if (> gain 0) (pass side* (- guard 1)) side))))))
```

## Task 2 — `graph-partition` public surface

**Files:** modify `combinatorial-graph.scm`, `combinatorial-graph.sld`.

- [x] **Step 1: opts discipline + validation (mirror `make-graph`).**

```scheme
(define (graph-partition G . opts)
  "Partition weighted graph G into two groups minimizing cut weight at a fixed
balance, via Kernighan-Lin pair-swaps.

NOT a minimum cut: global min-cut degenerates to isolating one vertex; KL holds
the partition sizes (set by the seed) and optimizes only the cut. See
plans/2026-06-08-balanced-graph-partition-design.md §4.

Opts (trailing alist):
  (method . 'kernighan-lin)  default; only value in Phase 1. ('normalized-cut: Phase 3.)
  (balance . 0.25)           imbalance tolerance in (0,1); bounds the SEED ratio that
                             KL preserves. Allowed |A|-|B| =
                             clamp(floor(balance*|V|), |V| mod 2, |V|-2).
  (weight  . PROC)           edge-data -> non-negative number; default (lambda (_) 1).
  (seed    . ALIST)          vertex -> 'a|'b initial bipartition; default %default-seed.
Returns: alist (group-a . (...)) (group-b . (...)) (cut-weight . N)
                (sizes . (NA . NB)) (normalized-cut . inexact)
  normalized-cut = cut-weight / total-edge-weight (a COST; lower is better; 0.0 if no edges).
Category: algebra
Keywords: partition, balanced cut, kernighan-lin, fiduccia-mattheyses, package split"
  (validate-opts-keys "graph-partition" opts '(method balance weight seed))
  (let ((method  (assv-or opts 'method  'kernighan-lin))
        (balance (assv-or opts 'balance 0.25))
        (weight  (assv-or opts 'weight  (lambda (_) 1)))
        (seed    (assv-or opts 'seed    #f)))
    (assert-procedure "graph-partition" weight)
    (unless (and (real? balance) (< 0 balance) (< balance 1)) ; (0,1): 1 excluded (Q-1)
      (error "graph-partition: balance must be in (0,1)"
             (list 'fix "pass an imbalance tolerance such as 0.25 (1 is excluded: it would empty a side)")
             balance))
    (unless (eq? method 'kernighan-lin)
      (error "graph-partition: only 'kernighan-lin is available in Phase 1"
             (list 'fix "'normalized-cut is Phase 3, gated on the gonum eigensolver")))
    ;; (seed* (if seed (%validate-seed G seed balance) (%default-seed G)))
    ;; (side  (%kl-refine G seed* weight))    ; KL holds the seed ratio
    ;; then project side -> (group-a group-b cut-weight sizes normalized-cut);
    ;; n<2 is a degenerate guard (single group). See committed code for the body.
    ))
```

- [x] **Step 2: result projection.** Build `group-a`/`group-b` by iterating `(graph-vertices G)`
  in order and bucketing each vertex by `(%side-of (graph-setoid G) side v)` — this preserves
  deterministic adjacency order, so **no sort is needed** (the first draft said "sorted", which was
  both unnecessary work and ambiguous for setoid-typed vertices). Report `(sizes . (NA . NB))` as
  raw counts; the consumer derives whatever balance metric it wants. Compute `cut-weight` via
  `%cut-weight`. Report `normalized-cut = cut-weight / (%total-weight G weight)` as a
  scale-comparable COST (lower is better) the consumer maps to confidence (replacing the
  scale-sensitive 0.15/0.30 cut-ratio bands); when `%total-weight` is `0` (no edges / all-zero
  weights) report `0.0` rather than dividing by zero.

  *Review renames (resolved):* output `balance`→`sizes` (the old name **collided** with the input
  `balance` tolerance — same key, two meanings) and `quality`→`normalized-cut` (the value is a
  *cost*; naming a cost "quality" inverts the usual higher-is-better reading). Design doc §6 updated
  to match.
- [x] **Step 3: degenerate-input guards.** `n < 2` → single group, `cut-weight 0`,
  `normalized-cut 0.0`, `sizes (n . 0)`. Empty graph (`n = 0`) → both groups empty, `sizes (0 . 0)`.
  Disconnected graph → KL still runs (it just holds the seed ratio; an all-isolated graph yields
  the seed split at cut 0). How `n = 2` and small/odd `n` interact with the balance bound is
  governed by Q-1 (the `%allowed-diff` clamp, applied to the seed).
- [x] **Step 4: export.** Add `graph-partition` to `combinatorial-graph.sld` `(export ...)` and to
  the umbrella `algebra.sld` export list.

## Task 3 — Tests (`test/wile/algebra-combinatorial-graph-test.scm`)

- [x] **Step 1: planted balanced cut.** Two K₃ cliques joined by one light bridge edge; assert
  `graph-partition` recovers the two cliques and `cut-weight` equals the bridge weight.
  **Order the adjacency so the default ⌈n/2⌉ seed does NOT already coincide with the planted cut**
  (e.g. interleave the two cliques' vertices). Otherwise FM starts on the answer, does zero work,
  and the test exercises only `%default-seed` — not the refinement it is meant to verify.
- [x] **Step 2: seed-imbalance rejection (KL balance semantics).** A 3/1 seed under `tol = 0.25`
  (allowed_diff = 1) is rejected by `%validate-seed` (`test-error`) — demonstrating that `balance`
  bounds the seed ratio KL preserves. (The earlier "FM tolerance permits 60/12" framing no longer
  applies: with KL an unequal split is *seeded*, not discovered at search time.)
- [x] **Step 3: determinism.** Same graph + same seed ⇒ `equal?` results across two calls
  (Scheme values — assert with `equal?`, not "byte-identical").
- [x] **Step 4: degeneracy guard (designed 2026-06-08).**

> The test that validates the entire §4 rationale: on a star, the global min-cut isolates one leaf
> (cut 1); the balanced partition must refuse that and pay a larger cut. Designed at the user's
> request (was a learning-mode TODO). Two assertions: (1) neither side is a singleton; (2) it
> provably *pays* for balance — cut 3 vs the min-cut's 1.

```scheme
(test-group "graph-partition/degeneracy-guard"
  ;; Star: hub h + 6 leaves. GLOBAL MIN-CUT isolates a single leaf (cut = 1).
  ;; A balanced partition must NOT do that.
  (define star (make-graph '((h . ((l1)(l2)(l3)(l4)(l5)(l6)))
                             (l1 . ((h)))(l2 . ((h)))(l3 . ((h)))
                             (l4 . ((h)))(l5 . ((h)))(l6 . ((h))))
                           '(symmetrize? . #t)))
  (define p   (graph-partition star '(balance . 0.34)))
  (define sz  (cdr (assq 'sizes p)))         ; (NA . NB)
  (define cut (cdr (assq 'cut-weight p)))
  ;; n=7, balance 0.34 -> allowed_diff = clamp(floor(2.38), 1, 5) = 2 -> 4/3 split.
  ;; (1) the property that disqualifies global min-cut: neither side is a singleton.
  (test-assert "balanced partition keeps both sides >= 2 (not a 1/6 min-cut)"
               (>= (min (car sz) (cdr sz)) 2))
  ;; (2) it provably PAYS for balance: cut 3 (hub's 3 cross-edges) vs min-cut's 1.
  (test "pays cut 3 to stay balanced, vs degenerate min-cut 1" 3 cut))
```

## Task 4 — Docs + bookkeeping

- [x] `docs/algebra/reference.md`: added `graph-partition` under a new "Partition (balanced cut)"
  subsection with the "balanced cut, NOT min-cut" note and the design §4 cross-reference.
- [x] `TODO.md`: added the shipped Phase-1 entry (Phase 2 wile-goast rewire + Phase 3 Shi-Malik as
  follow-ons); updated the library-summary line.
- [x] Bibliography: added Kernighan-Lin 1970 and Fiduccia-Mattheyses 1982 to `BIBLIOGRAPHY.md`,
  attributed to `combinatorial-graph.scm`, with the FM-rejection note.

---

## Verification (definition of done)

- [x] `make lint` (0 issues) && `make covercheck` (all 41 packages ≥80%) both pass.
- [x] New `graph-partition` tests pass (4 groups; 326 total in `algebra-combinatorial-graph-test.scm`).
- [x] `(import (wile algebra))` exposes `graph-partition` (umbrella re-export wired; verified).
- [x] No new external dependency introduced (Scheme-only change).
- [x] Branch `feat/graph-partition` pushed to origin (6 commits). PR intentionally **not** opened
  per user instruction (2026-06-08); branch carries two unrelated commits (fca perf, threading
  benchmark) that would need separating before any future PR. Not merged.

## Open questions (from 2026-06-08 review)

**Q-1 — balance-constraint semantics — RESOLVED 2026-06-08.** Adopted: imbalance tolerance with
`allowed |A|-|B| = clamp(floor(balance·|V|), |V| mod 2, |V|-2)`, a hard non-empty-group invariant
independent of `balance`, and opt range `(0,1)` (1 excluded). Encoded in `%allowed-diff` (Task 1),
the `balance` opt docstring + range check (Task 2). Original defects (single-vertex-FM framing):
the default ⌈n/2⌉ seed could violate a tight tolerance for small odd `n`; `tol = 1` made the
constraint vacuous; and there was no explicit non-empty invariant.

**Q-2 — engine deadlock — RESOLVED 2026-06-08 (during implementation).** Single-vertex FM moves
shift `|A|-|B|` by ±2, so from a balanced even-`n` seed no move is admissible under a tight
tolerance (default `tol=0.25` freezes `n=6`, returning the scrambled seed). Switched to **true
Kernighan-Lin pair-swaps**, which hold sizes invariant and never freeze. Consequence: KL preserves
the seed's A/B ratio, so `balance` now **bounds the seed ratio** (validated by `%validate-seed`
using `%allowed-diff`) rather than gating moves; `%movable?` was removed. `%allowed-diff`'s clamp
(Q-1) is reused for seed validation.

## Out of scope (Phase 2 / Phase 3)

- wile-goast affinity-graph construction + `find-split` rewire + confidence recalibration +
  docstring fix (Phase 2, separate plan in the wile-goast repo).
- Shi-Malik `'normalized-cut` method and the hybrid seed→refine path (Phase 3, gated on
  `plans/2026-04-18-gonum-integration-directions.md`).
- Go-side FM fast-path (`algebra/graph/partition.go`) — only if profiling shows a need at
  general-library scale; not required for the splitter.
