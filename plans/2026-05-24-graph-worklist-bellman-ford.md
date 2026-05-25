# Worklist Bellman-Ford for `(wile algebra graph)`

**Status:** Design draft — not started.

**Scope:** Internal optimization to `make-graph-analysis` / `graph-query` / `graph-query-all`. No API surface change; no semiring contract change; no behavioral change on well-posed inputs.

**Repository:** `aalpar/wile` (the algebra library lives in `lib/wile/algebra/graph.sld`, *not* in `wile-goast`).

## Motivation

`graph-query` and `graph-query-all` currently implement textbook Bellman-Ford: V−1 outer iterations, each scanning all E edges and relaxing every one. On a 539-node, 623-edge call graph this is ~335K edge relaxations per iteration × 538 iterations = ~180M relaxations, regardless of how few values actually changed in any given iteration.

The dominant real-world failure mode for this library is the *opposite* of the academic pathology: most queries converge in a small fraction of V iterations because the underlying graphs are tree-shaped or shallow. Textbook B-F does the same V−1 scans whether convergence happened on iteration 3 or iteration 537.

A worklist variant only re-scans edges whose source value *changed in the previous round*. On typical call-graph queries (boolean reachability, tropical hop-count, counting on DAGs) this collapses runtime by 1–2 orders of magnitude. Worst case is identical to current behavior.

This change does **not** fix the cyclic-counting pathology (counting semiring on a graph with reachable cycles is intrinsically intractable — the values exceed int64 within a few iterations and become bignums). That is a *question-shape* problem requiring SCC condensation; see related plan TODO. Worklist B-F still helps *boolean* and *tropical* queries on cyclic graphs because their carriers stay bounded.

## Background — the 3-hour incident (2026-05-23)

Ran `(graph-query-all ga-count run-name)` from `(*MachineContext).Run` on the `machine` package's static call graph (539 nodes, 623 edges, 12 back-edges). Ran for ~3 hours before user escaped. Root cause: counting semiring + cycles produces bignum walk explosion. Worklist B-F would NOT have fixed this — counting on cycles is the wrong question. But the incident motivated a profiling pass that surfaced multiple constant-factor wins applicable to the *non-pathological* common case, of which worklist B-F is the most leveraged.

See `feedback-counting-semiring-on-cycles.md` for the incident memory.

## Current implementation

Per the library docs (verified at call sites in the eval session):

- `make-graph-analysis SEMIRING ADJACENCY WEIGHT-FN` returns a `graph-analysis` record. Internal indices probably built at construction time.
- `graph-query GA SOURCE TARGET` lazily computes single-source distances on first query per source; caches per-source results.
- `graph-query-all GA SOURCE` returns the full distance alist for one source; same lazy cache.

The single-source distance computation is presumably:

```
;; Pseudocode of current (textbook) implementation
(define (single-source-distances ga source)
  (let ((d (make-distance-map)))
    (distance-set! d source semiring-one)
    (for-each-iteration (- V 1)
      (for-each-edge ga
        (lambda (u v w)
          (let ((nd (semiring* (distance-ref d u) w)))
            (distance-set! d v (semiring+ (distance-ref d v) nd))))))
    d))
```

Cost: O(V × E × semiring-op-cost). Always V−1 iterations regardless of convergence.

## Design — worklist Bellman-Ford (SLF variant)

The Smallest-Label-First (SLF) and Largest-Label-Last (LLL) variants of B-F are well-studied. For semiring-parameterized analysis the relevant invariant is *value changed*, not *value ordering* (which doesn't generalize to non-ordered semirings like counting). So the right variant here is the **plain worklist** form:

```
;; Worklist Bellman-Ford
(define (single-source-distances ga source)
  (let ((d (make-distance-map))
        (in-worklist? (make-bitvector V #f))
        (worklist (make-queue)))
    (distance-set! d source semiring-one)
    (queue-enqueue! worklist source)
    (bitvector-set! in-worklist? source #t)
    (let loop ()
      (cond
        ((queue-empty? worklist) d)
        (else
          (let ((u (queue-dequeue! worklist)))
            (bitvector-set! in-worklist? u #f)
            (for-each-out-edge ga u
              (lambda (v w)
                (let* ((nd (semiring* (distance-ref d u) w))
                       (old (distance-ref d v))
                       (new (semiring+ old nd)))
                  (when (not (semiring-equal? old new))
                    (distance-set! d v new)
                    (when (not (bitvector-ref in-worklist? v))
                      (queue-enqueue! worklist v)
                      (bitvector-set! in-worklist? v #t))))))
            (loop)))))))
```

**Key properties:**

1. **Correctness identical to textbook B-F.** Both compute the same fixed point of `d[v] = ⊕ over edges (u,v) of d[u] ⊗ w`. Worklist just visits nodes lazily.
2. **Best case dramatically better.** On a tree-shaped subgraph rooted at the source, only V edge relaxations total (each node enqueued once). Textbook does V·E.
3. **Worst case identical.** On a complete graph with values that change every iteration, worklist degenerates to V·E (each node re-enters the queue up to V−1 times).
4. **Termination requires `semiring-equal?`.** This is the only new operation needed from the semiring side. Defined per-semiring:
   - Boolean: `eq?`
   - Tropical: `=` (numeric)
   - Counting: `=` (numeric — but `=` on bignums is itself O(d), still cheap relative to `+` and `*`)

   Wile's `semiring` record already has the carrier's equality available implicitly via the host `equal?`. Whether to add an explicit `semiring-eq?` slot or use host equality on the carrier values is a design question — see Q-1.

5. **Convergence detection is free.** When the worklist drains, we've converged. No need for the explicit V−1 outer bound. (Note: this means worklist B-F on a non-k-closed semiring with reachable cycles **does not terminate** — see Risk R-1.)

## Open design questions

- **Q-1:** Equality detection — add `semiring-eq?` slot to the `semiring` record (cleanest, lets each semiring optimize its own equality), or use host `equal?` on carrier values (zero API change, slightly slower for bignums)? **Default:** add the slot, default it to `equal?` if unspecified.

- **Q-2:** Termination guard for non-convergent semirings (counting on cyclic graphs) — should the worklist loop carry a hard iteration cap (e.g., V·V or V³) as a safety net, even though it changes semantics? **Default:** no — the proper fix is k-closedness detection at `make-graph-analysis` time (separate plan). Worklist B-F should be honest about non-termination on bad inputs; the caller's responsibility is to use a sensible semiring.

- **Q-3:** Queue vs. deque — SLF uses deque-front insertion for already-relaxed nodes, FIFO for new. SLF is empirically faster on shortest-path workloads but requires totally-ordered values. Plain FIFO worklist works on any semiring. **Default:** plain FIFO. SLF can be added later as an opt-in for ordered semirings (tropical specifically) if benchmarks justify.

## Implementation plan

### Phase 1 — equality slot + plumbing

- Add `semiring-eq?` accessor to the `semiring` record in `(wile algebra semiring)`.
- Update `make-semiring` to accept an optional `:eq?` keyword arg, default `equal?`.
- Update `boolean-semiring`, `tropical-semiring`, `counting-semiring` constructors to pass appropriate equality predicates.
- Tests: equality slot round-trips correctly for all three built-in semirings; user-defined semirings without an `:eq?` arg still work (default to `equal?`).

### Phase 2 — worklist core

- Add internal `single-source-distances-worklist` in `(wile algebra graph)`.
- Switch `graph-query` and `graph-query-all` to dispatch through it.
- Keep the existing textbook implementation under a debug flag (or delete after CI green — depends on review preference).
- Tests: every existing graph-query test passes unchanged. Add tests that exercise:
  - Tree shape (best case for worklist)
  - Dense small graph (worst case)
  - Disconnected source (worklist must terminate immediately)
  - Self-loop on source (one re-enqueue, then stop)
  - Multi-edge (parallel edges between same vertex pair)

### Phase 3 — benchmark + verification

- Add a benchmark suite in `bench/algebra-graph.sld` (or wherever the algebra benchmarks live):
  - Tree-shaped graph, V = 100, 500, 1000
  - DAG with depth ~log V
  - Cyclic graph, boolean semiring (should be fast)
  - Cyclic graph, tropical semiring (should be fast)
- Document expected speedup ranges in the benchmark output.
- Acceptance gate: no regression on any existing benchmark; ≥3× improvement on the tree-shape benchmark (the worklist's natural strength).

### Phase 4 — docs + PR

- Update `(wile algebra graph)` library description to note "uses worklist Bellman-Ford internally; convergence-detection-based, not fixed V−1 iterations."
- Add a one-paragraph note in the library docs about non-k-closed semirings: "Counting semiring on graphs with reachable cycles will not terminate. Use SCC condensation first or pick a different semiring." (Forward-references the SCC plan when it exists.)
- Open PR, dual review (Copilot + `/crosscheck`).

## Risks

- **R-1 — Non-termination on non-k-closed semirings with cycles.** Textbook B-F's V−1 cap silently bounded runtime (at the cost of incorrect results — the value at iteration V−1 isn't necessarily the fixpoint, it's just "the value after V−1 iterations"). Worklist B-F has no such cap, so on counting-with-cycles it runs forever (or until OOM from bignum growth). This is honest behavior — the existing textbook implementation was hiding a real failure mode by giving up early — but it changes the runtime characteristic. Mitigation: add k-closedness check at `make-graph-analysis` time in a separate plan; recommend it as a prerequisite-or-parallel-track to this one.
- **R-2 — Equality cost on bignum semirings.** Adding a per-relaxation `(semiring-equal? old new)` check costs O(d) for bignums. For very-large-bignum workloads this is a small overhead. Negligible compared to the cost of `+` and `*` (both O(d²) schoolbook), but worth measuring in the benchmark.
- **R-3 — Queue implementation choice.** Wile's standard data structures probably include a queue or deque; if not, a simple list-based FIFO (cons to one end, take from the other) is fine for this. Avoid building a custom data structure if a library primitive exists.
- **R-4 — Test coverage gap on convergence-detection edge cases.** Self-loops, disconnected components, single-vertex graphs, empty edge sets — all need explicit tests because worklist's behavior on these differs from textbook B-F (it terminates immediately rather than scanning V−1 empty iterations).

## Acceptance criteria

- All existing `(wile algebra graph)` tests pass.
- New worklist-specific tests cover the convergence-detection edge cases.
- Tree-shape benchmark shows ≥3× speedup (target — measure first, lower bound to ≥1.5× if reality disagrees).
- Cyclic-graph boolean reachability benchmark shows ≥2× speedup.
- No regression on any existing benchmark.
- `make lint && make covercheck && make ci` all green.

## Out of scope

- SCC condensation primitive (separate plan — would let *exact* counting work on cyclic graphs by quotienting out the cycles).
- Tarjan / Kosaraju implementations (separate plan).
- Topological sort primitive (separate plan).
- SLF / LLL ordering variants (deferred per Q-3 default).
- Sparse adjacency hash-table — orthogonal; `make-graph-analysis` may already do this internally. Confirm during Phase 2 and file separately if not.
- Bignum performance work in Wile's numeric tower (allocation reduction, in-place arithmetic, scratch pool) — sibling plan `2026-05-24-bignum-allocation-reduction.md`. Helps the cyclic-counting case and any bignum-heavy workload; orthogonal to the convergence-detection work here. (Karatsuba was originally listed; verified during audit that `math/big` already provides it.)
- Approximate-counting semirings (saturating, modular, log-space) — these make cyclic-counting queries tractable at the cost of exactness, and are the complementary fix to this plan's convergence-detection work. See sibling plan `2026-05-24-approximate-counting-semirings.md`. Together: worklist B-F speeds up convergent queries; approximate counting makes (otherwise non-convergent) cyclic-counting queries terminate in bounded time.

## References

- Bellman-Ford-Moore worklist variant: standard algorithms textbook treatment, e.g. Cormen et al. *Introduction to Algorithms*, 3rd ed., §24.1 exercises.
- Mohri (2002) *Semiring Frameworks and Algorithms for Shortest-Distance Problems* — establishes the k-closedness condition for B-F termination on general semirings. Forward-reference for the planned setup-time check.
- `feedback-counting-semiring-on-cycles.md` — incident memory motivating the broader optimization sweep.
- `2026-04-17-algebra-foundations-directions.md` — algebra roadmap (this plan slots into §5 graph-algorithm improvements; flag for inclusion next revision).
