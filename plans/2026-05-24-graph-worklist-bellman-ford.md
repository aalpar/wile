# Per-semiring equality for `(wile algebra graph)` convergence detection

**Status:** Design revised 2026-05-26 — narrowed from the original "worklist Bellman-Ford" scope. Phases 2–3 of the original plan shipped via a different mechanism (topological-order dispatch + worklist for cyclic) in the PR #757/#758 wave. Remaining work is the `(eq?)` opts plumbing on `<semiring>` and threading `semiring-eq?` through `graph.scm` in place of the current `equal?` hardcode.

**Scope:** Internal correctness/extensibility fix to `(wile algebra graph)`'s convergence-detection path. No public API additions on the graph side; one new optional opts key on `make-semiring` + one new accessor `semiring-eq?`. No behavioral change on the three built-in semirings (boolean / tropical / counting / bigint-counting) since their natural equality matches `equal?` on their carrier values.

**Repository:** `aalpar/wile`. Files: `lib/wile/algebra/semiring.scm` (constructor + accessor), `lib/wile/algebra/graph.scm` (call sites), tests under `lib/wile/algebra/`.

## What this plan was, and what it became

The original 2026-05-24 plan targeted the textbook V−1-iteration Bellman-Ford in `graph-query` / `graph-query-all`, proposing to replace it with a worklist variant for convergence detection. Headline target: ≥3× speedup on tree-shaped graphs (the common case in call-graph queries).

Between the design draft and any execution of it, three commits in the PR #757/#758 wave already replaced the textbook B-F with a more sophisticated dispatcher that subsumes most of the original plan's value:

- **`compute-via-topological-order`** (`graph.scm:306-340`) — single forward pass on DAGs, each edge relaxed exactly once. **Strictly better than worklist B-F** on tree-shaped or shallow graphs (the plan's headline target). Correct for *all* semirings including non-idempotent counting, because each node is visited only after its predecessors have settled.
- **`compute-via-worklist`** (`graph.scm:357-409`) — used for cyclic subgraphs. Convergence detection via `(equal? merged old-val)` at line 401. Already has a 2·V·E safety cap with a diagnostic pointing at SCC condensation / approximate semirings (which resolves Q-2 from the original plan).
- **Dispatcher** (`graph.scm:108-118`) — picks topo-order for DAGs, worklist for cyclic, `count-paths-in-dag` for the big-int-carrier fast path.

What this means for the plan's three original phases:

| Original phase | Status |
|----------------|--------|
| Phase 1 — `:eq?` slot + plumbing | **Still pending.** Open question: which equality predicate does the worklist use to detect convergence? Today it uses `equal?` hardcoded at `graph.scm:401`. |
| Phase 2 — worklist core (replace textbook B-F) | **Shipped via different mechanism.** Topo-order is better than worklist on the headline target; worklist already handles the cyclic case. Neither uses textbook V−1 iterations. |
| Phase 3 — benchmarks vs. textbook B-F | **Obsolete.** No textbook B-F left to compare against. The current dispatcher's wins are documented in the PR #757/#758 trail. |
| Phase 4 — docs + PR | Folded into Phase 1's PR. |

The remaining piece — Phase 1's `:eq?` plumbing — has standalone value:

1. **Correctness for non-canonical carriers.** `equal?` is the wrong predicate for some carriers. Modular ints want `(mod-eq? p)`. Log-space floats want tolerance-based equality (`abs(a-b) < ε`). Future user-defined non-canonical reps need their own equality. The current `equal?` hardcode locks `(wile algebra graph)` to canonical carriers.
2. **Prerequisite for approximate-counting semirings.** `2026-05-24-approximate-counting-semirings.md` (sibling plan, still not started) adds three new constructors — saturating, modular, log — each with a different equality contract. None can be wired into graph queries cleanly without the `(eq?)` slot.
3. **Consistency with the existing carrier-opt pattern.** `<semiring>` already accepts `(carrier . SYM)` via the same trailing-alist mechanism. Adding `(eq? . PROC)` matches the established shape exactly — `validate-opts-keys` just grows by one symbol.

## Motivation — what the `(eq?)` slot is *for*

The worklist algorithm at `graph.scm:357-409` terminates by detecting that no node's distance changed in a relaxation step. The check at line 401 is currently:

```scheme
(if (equal? merged old-val)
    (edge-loop (cdr edges) wl d)        ; no change, skip
    (let ((new-d ...))
      (edge-loop ...)))                  ; changed, propagate
```

For the three built-in semirings this is correct:

- **Boolean:** `equal?` on `#t`/`#f` = correct.
- **Tropical:** `equal?` on numerics = correct (numeric `=` and `equal?` agree on finite IEEE 754 except for NaN, which has no role in shortest-path values).
- **Counting (exact int / bigint):** `equal?` on exact integers = correct.

For carriers where it goes wrong:

- **Log-space `float64`:** Two log-counts can differ by `1e-300` and represent the same physical count for all reasonable purposes. `equal?` says no → the worklist re-propagates indefinitely on cycles. Tolerance-based equality would terminate cleanly.
- **Modular ℤ/Pℤ:** Carrier values must be normalized (i.e., always in `[0, P)`) before equality. If the operation doesn't normalize and the value is `(+ P 3)` vs `3`, `equal?` says no → re-propagation. Modular-aware equality terminates.
- **Saturating int64:** `equal?` is correct, but a future variant that stores `(saturated . count)` pairs would need pair-aware equality.

The fix is structural: the semiring should declare its own equality predicate at construction time, defaulting to `equal?` when the user doesn't specify. The graph code consults `semiring-eq?` instead of `equal?` hardcoded.

## Design

### `<semiring>` record — add an `eq?` field with default

The existing record (`semiring.scm:9-15`):

```scheme
(define-record-type <semiring>
  (make-semiring* plus-fn times-fn zero one carrier)
  semiring?
  (plus-fn  semiring-plus-fn)
  (times-fn semiring-times-fn)
  (zero     semiring-zero)
  (one      semiring-one)
  (carrier  semiring-carrier))
```

Becomes:

```scheme
(define-record-type <semiring>
  (make-semiring* plus-fn times-fn zero one carrier eq?-fn)
  semiring?
  (plus-fn  semiring-plus-fn)
  (times-fn semiring-times-fn)
  (zero     semiring-zero)
  (one      semiring-one)
  (carrier  semiring-carrier)
  (eq?-fn   semiring-eq?-fn))
```

New accessor `semiring-eq?` calls `eq?-fn` on two values:

```scheme
(define (semiring-eq? S a b)
  ((semiring-eq?-fn S) a b))
```

### `make-semiring` — extend opts allowlist with `eq?`

Current opts handling (`semiring.scm:18-23`):

```scheme
(define (make-semiring plus times zero one . opts)
  "..."
  (assert-procedure "make-semiring" plus)
  (assert-procedure "make-semiring" times)
  (validate-opts-keys "make-semiring" opts '(carrier))
  (make-semiring* plus times zero one (assv-or opts 'carrier #f)))
```

Becomes:

```scheme
(define (make-semiring plus times zero one . opts)
  "..."
  (assert-procedure "make-semiring" plus)
  (assert-procedure "make-semiring" times)
  (validate-opts-keys "make-semiring" opts '(carrier eq?))
  (let ((eq?-fn (assv-or opts 'eq? equal?)))
    (assert-procedure "make-semiring" eq?-fn)
    (make-semiring* plus times zero one
                    (assv-or opts 'carrier #f)
                    eq?-fn)))
```

Docstring grows by one bullet under the trailing alist key list, mirroring the existing `(carrier . SYM)` entry.

### Built-in semiring constructors — explicit `eq?` where natural

The three (now four) built-ins each declare their natural equality even though it matches `equal?`, both for documentation value and so that user-defined drop-ins have a worked example:

```scheme
(define (boolean-semiring)
  (make-semiring or-bool and-bool #f #t '(eq? . eq?)))   ; #t/#f → eq? suffices

(define (tropical-semiring)
  (make-semiring tropical-min tropical-add tropical-inf 0 '(eq? . =)))
  ; numeric = is faster than equal? on numerics and handles +inf.0 correctly

(define (counting-semiring)
  (make-semiring + * 0 1 '(eq? . =)))                    ; numeric =, fast

(define (bigint-counting-semiring)
  (make-semiring + * 0 1
                 '(carrier . big-int)
                 '(eq? . =)))                            ; = on bigints, in-place compare in Go
```

Open question: are these worth declaring explicitly, or should we rely on the `equal?` default? Documentation value vs. four near-tautological lines of opts. **Default decision: declare them explicitly.** The lines establish a worked example for future custom semirings; the cost is negligible.

### `graph.scm` — switch `equal?` to `semiring-eq?`

Two call sites need attention. The plan's audit during execution will confirm whether more exist (the only one identified in code review is line 401).

**Site 1 — worklist convergence detection (`graph.scm:401`):**

```scheme
(if (equal? merged old-val) ...)
```

becomes

```scheme
(if (semiring-eq? S merged old-val) ...)
```

**Site 2 — topo-order processing.** The current code at `graph.scm:306-340` doesn't have an analogous equality check because it doesn't iterate to a fixpoint — each node is visited exactly once. So nothing to change here. Confirmed by reading the body: `inner` always recurses on `(cdr edges)` regardless of whether `merged` differs from `old-val`. Worth noting in the implementation commit so a future reader doesn't think we missed a site.

**Site 3 — `count-paths-in-dag` fast path (`graph.scm:147-241`).** This is the bigint-counting Go-side kernel. Equality is internal to the kernel (`*big.Int` compare in Go) and not visible to the Scheme side. Unaffected.

## Open design questions

- **Q-1: Do we declare `(eq? . =)` etc. on the built-ins, or rely on the `equal?` default?** **Default: declare them explicitly** for documentation value and to give custom-semiring authors a worked example. Lines are cheap.
- **Q-2: Should `semiring-eq?` be reflexive on `#f`?** I.e., what does `(semiring-eq? S #f #f)` mean if the carrier doesn't include `#f`? **Default: don't special-case.** Caller's responsibility to only pass carrier values. The contract is "on carrier values, returns booleantruthy iff they're equal." Free monad: out-of-carrier inputs are undefined behavior.
- **Q-3: Does the bigint-counting fast path need its own equality?** **No.** It bypasses the Scheme semiring's `eq?` entirely — the comparison happens inside the Go kernel on `*big.Int`. No change needed.

## Implementation plan

### Phase 1 — semiring `:eq?` slot + plumbing

`lib/wile/algebra/semiring.scm`:

1. Add `eq?-fn` field to `<semiring>` record-type definition (sixth field).
2. Update `make-semiring*` record-type constructor signature to take six arguments.
3. Add `semiring-eq?-fn` accessor (private — used by `semiring-eq?` only).
4. Add public `semiring-eq?` procedure with docstring.
5. Update `make-semiring` opts allowlist: `'(carrier)` → `'(carrier eq?)`.
6. Default `eq?` arg to `equal?` if not supplied.
7. Validate the supplied procedure with `assert-procedure`.
8. Update the four built-in semiring constructors with explicit `(eq? . PROC)` per the design above.
9. Update `make-semiring`'s docstring to document the new opts key.
10. Make sure validate-semiring continues to work — it doesn't currently consult `eq?` (it uses `equal?` internally for spot-checks of laws; we leave that alone since `validate-semiring` operates on *the semiring under test*, not on values produced by it; consult `equal?` there is the right contract for the law-checker itself).

### Phase 2 — thread through `(wile algebra graph)`

`lib/wile/algebra/graph.scm`:

1. Replace `(equal? merged old-val)` at line 401 with `(semiring-eq? S merged old-val)`.
2. Audit `compute-via-topological-order` — confirm no `equal?` on semiring values exists. Document the absence in a brief comment so a future reader doesn't add one expecting parity with the worklist path.
3. Audit `compute-via-count-paths-in-dag` — confirm fast path doesn't surface Scheme-side equality (it doesn't; the Go kernel handles its own bigint compare).
4. Verify the `assoc` calls in both `compute-via-*` paths still use `equal?` for keys (node names) — `equal?` is correct for node identifiers regardless of the semiring's value equality.

### Phase 3 — tests

Add to the existing graph + semiring tests:

1. `semiring-eq?` default behavior: `(semiring-eq? (counting-semiring) 1 1) => #t`, `(semiring-eq? (counting-semiring) 1 2) => #f`. Same for boolean, tropical.
2. Custom `:eq?` round-trip: `(let ((S (make-semiring + * 0 1 '(eq? . my-eq?)))) (semiring-eq? S a b))` calls `my-eq?` (use a counter-incrementing predicate to verify dispatch).
3. `validate-opts-keys` rejects unknown keys: `(make-semiring + * 0 1 '(eqq? . equal?))` (typo) raises.
4. Graph query with custom semiring whose `eq?` differs from `equal?`: construct a "tolerance semiring" on floats (`(eq? (lambda (a b) (< (abs (- a b)) 1e-9)))`) on a cyclic graph; verify the worklist terminates where `equal?` would have iterated to the 2·V·E cap.
5. `assert-procedure` on the `eq?` arg: `(make-semiring + * 0 1 '(eq? . 42))` raises with the same `make-semiring: eq? must be a procedure` shape as the existing `plus`/`times` checks.
6. Backward compatibility: every existing graph/semiring test continues to pass unchanged.

### Phase 4 — docs + PR

1. Update `(wile algebra semiring)` library description (the `define-library` `description` field, if present) to mention the optional `:eq?` opt.
2. Update `docs/algebra/reference.md` semiring section: new `semiring-eq?` accessor, new `eq?` opt on `make-semiring`.
3. Add a one-line note in `(wile algebra graph)`'s docstring explaining that convergence detection consults the semiring's declared equality, not host `equal?`.
4. Open PR with the commit chain: revised plan (already committed) + Phase 1 + Phase 2 + Phase 3 + docs.
5. Request Copilot review.
6. Optionally dispatch `/crosscheck:crosscheck all` per `plans/CLAUDE.md` implementation workflow.

## Risks

- **R-1 — Hidden `equal?` sites in graph or downstream code.** Mitigated by Phase 2 audit; covered by Phase 3 tests on tolerance-based equality (which would reveal any site still hardcoded to `equal?` by causing non-convergence on a cyclic graph).
- **R-2 — Performance regression from the indirection.** Replacing `(equal? merged old-val)` with `(semiring-eq? S merged old-val)` adds one dispatch (record-field access + procedure call). Negligible on bignum or float workloads where the value comparison itself dominates; potentially measurable on boolean reachability (where the comparison was `eq?`-fast). Bench-gate: ≤ 0.5% geomean per the project's standard threshold; if it regresses, inline the per-built-in equality at construction time (pre-curry).
- **R-3 — Documentation drift.** The `(carrier . SYM)` docstring exists; `(eq? . PROC)` needs an analogous entry. The two are conceptually parallel (both are advisory metadata that consumers can dispatch on), so the doc paragraph can mirror the existing one with `s/carrier/eq?/`.
- **R-4 — Stale test premise.** Some existing semiring test may assert `(eq? (make-semiring + * 0 1) (make-semiring + * 0 1))` or similar — adding a closure field could break record equality if tested. Phase 3 step 6 (backward compat) catches this.

## Acceptance criteria

- `semiring-eq?` accessor returns `#t` / `#f` on each built-in semiring's natural value domain, matching `equal?` on those carriers.
- Custom `:eq?` opt round-trips: `(semiring-eq? S a b)` calls the user-supplied predicate.
- Graph query with a tolerance-based equality semiring terminates on a cyclic float graph where `equal?` would have hit the 2·V·E iteration cap.
- All existing `(wile algebra graph)` + `(wile algebra semiring)` tests pass unchanged.
- `make lint && make covercheck && make ci` all green.
- No regression > 0.5% geomean on the existing graph benchmark suite.

## Out of scope

- Approximate counting semirings — sibling plan `2026-05-24-approximate-counting-semirings.md` consumes this work but ships separately.
- Bignum allocation reduction in the numeric tower — sibling plan `2026-05-24-bignum-allocation-reduction.md` (Phase 5 shipped; remaining phases tracked separately).
- SCC condensation primitive — already shipped via `plans/2026-05-26-scc-condensation.md` (PR #757).
- Worklist Bellman-Ford replacement of textbook B-F — already shipped via topo-order + worklist dispatch in the PR #757/#758 wave. See "What this plan was, and what it became" above.

## References

- `feedback-counting-semiring-on-cycles.md` — original incident memory.
- `2026-04-17-algebra-foundations-directions.md` — algebra roadmap.
- `2026-05-24-approximate-counting-semirings.md` — sibling plan; consumer of this work.
- `stdlib/lib/wile/algebra/CLAUDE.md` — algebra library conventions, carrier-opt pattern.
- `graph.scm` (lines 357-409) — current worklist; line 401 is the equality site to update.
- `semiring.scm` (lines 9-23) — current `<semiring>` record + `make-semiring`.
