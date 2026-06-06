# Interval Dataflow + Galois Connections — Design

**Status**: **Realized (PR #765, merged 2026-06-06).** Implemented per `2026-05-31-interval-dataflow-widening-impl.md`; the §6.4 single-`'unbounded`-sentinel gamma was superseded during review by typed sentinels (see the impl plan's "Post-review revisions").
**Date**: 2026-05-31.
**Related work**:
- `(wile algebra dataflow)` (`stdlib/lib/wile/algebra/dataflow.scm`) — the MFP worklist solver this work extends.
- `(wile algebra interval)` (`stdlib/lib/wile/algebra/interval.scm`) — the interval lattice + arithmetic this work makes usable as a dataflow domain.
- `(wile algebra abstract-domain)` (`stdlib/lib/wile/algebra/abstract-domain.scm`) — the sign domain (`abstract-sign`, `sign-binop`) whose interval analogs are missing.
- `(wile algebra galois)` (`stdlib/lib/wile/algebra/galois.scm`) — the Galois-connection framework; this work adds pre-built instances.
- `fixpoint/widen` (`stdlib/lib/wile/algebra/lattice.scm:129`) — the existing standalone widening fixpoint, *not yet wired into the CFG solver*.

---

## 1. Motivation

The dataflow solver `run-analysis` already proves properties over the **sign** domain: it ran the loop `x = 1; while(?) x = x + 1` and returned `pos` at the loop exit — a proof that `x > 0` on every execution, for any iteration count. This works because the sign lattice has **finite height** (`bottom ⊑ atom ⊑ top`, 2 steps), so Kleene iteration to a fixed point terminates.

The natural next domain — **intervals** — proves numeric *bounds* (`0 ≤ x ≤ 100`) rather than just signs. The interval lattice already exists (`interval-lattice`, `interval.scm:74`) with infinity-aware join/meet/leq, and interval arithmetic ships (`interval-add/sub/mul`, `interval.scm:103-128`). But the interval lattice has **infinite height**: `[0,0] ⊏ [0,1] ⊏ [0,2] ⊏ …` ascends forever. Running the same loop through `run-analysis` on the interval lattice **does not terminate** — the worklist never reaches a fixed point because each visit to the loop body produces a strictly larger interval.

Observed directly (raw join, hand-iterated):

```
round 1: (0 . 0)
round 2: (0 . 1)
round 3: (0 . 2)   ... never stabilizes
```

The standard remedy is a **widening operator** `∇`: at loop headers, instead of joining, jump unstable bounds to infinity, forcing the ascending chain to become finite. Wile *already has* the generic mechanism — `fixpoint/widen` converged the exact chain above to `(0 . pos-inf)` in one call, proving `x ≥ 0`. The gap is purely integration: **`fixpoint/widen` is a standalone fixpoint over a single value; it is not reachable from the CFG worklist solver `run-analysis`.**

This plan closes that gap and ships the small supporting pieces (interval abstraction, bottom-aware arithmetic, a widening operator, Galois instances) so intervals become a first-class dataflow domain with a certified soundness story.

---

## 2. Scope and non-goals

**In scope:**

- A widening hook in `run-analysis`: optional `(widen OP)` tagged argument, applied at loop-header blocks in place of raw join, guaranteeing termination on infinite-height lattices.
- Loop-header detection (where to widen) using the back-edge structure already implicit in the solver's reverse-postorder ranking.
- `interval-widen` — the standard interval widening operator (unstable bound → ∞), shipped in `(wile algebra interval)`.
- `abstract-interval` — `int → (n . n)`, the missing analog of `abstract-sign`, shipped in `(wile algebra interval)` (or `abstract-domain`; see §8 Open Questions).
- Bottom-awareness for `interval-add/sub/mul` so `interval-bot` propagates instead of raising.
- Two pre-built Galois-connection instances: `P(Z) ↔ interval` and `P(Z) ↔ sign`, shipped as constructors, each passing `gc-sound?`.
- Documentation: a worked tutorial example tying an interval `run-analysis` result to its Galois connection (the soundness certificate).

**Out of scope (deferred):**

- **Narrowing** (precision recovery after widening overshoots). Tightening `(0 . pos-inf)` back to `(0 . 99)` under a guard `x < 100` requires the transfer to model guards/conditions, which the current opaque-block design does not. Separate, larger design.
- **Widening with thresholds** (jump to the nearest constant in a candidate set instead of ∞). A precision refinement on top of basic widening.
- A general front-end that *generates* transfer functions from an AST. The solver remains transfer-agnostic; callers write transfers.
- Relational domains (octagons, polyhedra). Intervals are non-relational by construction.

**Not on the roadmap:**

- Replacing the MFP/worklist core with a different fixpoint engine.
- Widening as the *default* behavior. It must be opt-in: on finite-height lattices raw join is both terminating and maximally precise, and silently widening would needlessly lose precision.

---

## 3. Current state (pinned to source)

**What already works:**

| Piece | Location | Notes |
|---|---|---|
| Interval lattice | `interval.scm:74-101` | join/meet/leq are bottom-aware and infinity-aware |
| Interval arithmetic | `interval.scm:103-128` | `interval-add/sub/mul`; **not** bottom-aware (call `car`/`cdr` directly) |
| Generic widening fixpoint | `lattice.scm:129` | `(fixpoint/widen L f x widen)`; correct, but single-value |
| Plain Kleene fixpoint | `lattice.scm:112` | `fixpoint`; what the solver effectively does per block |
| MFP worklist solver | `dataflow.scm:249` | `run-analysis`; raw join only |
| Galois framework | `galois.scm` | `make-galois-connection`, `gc-alpha`, `gc-gamma`, `gc-sound?` |
| Sign abstraction | `abstract-domain.scm:33,76` | `abstract-sign`, `sign-binop` (bottom-aware) |

**What is missing:**

1. No `abstract-interval` (the `abstract-sign` analog).
2. `interval-add/sub/mul` raise on `interval-bot` (verified: `interval-add` at `interval.scm:103` calls `(car a)` with no bottom guard, unlike `sign-binop` which absorbs bottom).
3. No `interval-widen` operator.
4. `run-analysis` cannot apply widening — it joins predecessor out-states unconditionally (`dataflow.scm:391-401`) and converges only when `lattice-leq?` reports no increase (`dataflow.scm:413-417`). On an infinite-height lattice the convergence test never fires at loop headers.
5. No pre-built Galois instances connecting a concrete domain to interval or sign.

---

## 4. The core change: widening in `run-analysis`

### 4.1 Where join happens today

In the worklist loop (`dataflow.scm:391-401`), a block's `in-val` is the join of its flow-predecessors' out-states:

```scheme
(in-val (if (null? pred-idxs)
            (if (memv idx seed-idxs) initial-state bot)
            (let join-preds ((ps pred-idxs)
                             (acc (if (memv idx seed-idxs) initial-state bot)))
              (if (null? ps) acc
                  (join-preds (cdr ps)
                    (lattice-join lattice acc (get-out (car ps))))))))
```

Convergence is then governed by (`dataflow.scm:413-417`):

```scheme
(if (lattice-leq? lattice out-val old-out)
    (loop rest-wl)                              ; no increase — done with this block
    (loop (worklist-insert-all rest-wl (flow-succs blk))))
```

On a finite-height lattice this terminates. On the interval lattice, a loop header's `in-val` strictly increases every revisit, `out-val` never satisfies `lattice-leq? out-val old-out`, and the worklist never drains.

### 4.2 The fix: widen at loop headers

Widening compares the **previous** iterate at a block against the **new** joined iterate and returns something ≥ their join whose ascending chain is finite. The solver already retains the previous in-state via `get-in idx`. The change at the `in-val` computation:

```
joined-in  := join of predecessor out-states            ; as today
in-val     := if (widening-point? idx)
                 (widen (get-in idx) joined-in)          ; ∇ at loop headers
                 joined-in                               ; raw join elsewhere
```

`widen` is the optional operator threaded in via a new `(widen OP)` tagged argument, mirroring the existing `(init-state VALUE)` plumbing (`dataflow.scm:298-310`). When absent, behavior is identical to today (pure MFP).

### 4.3 Termination argument

A correct `widen` guarantees every ascending chain under it is finite (per `fixpoint/widen`'s contract, `lattice.scm:129`). Applied at every loop header — at least one block on every cycle in the CFG — every cyclic feedback path passes through a widening point, so no in-state can ascend infinitely. Acyclic portions retain exact join precision. Therefore the worklist drains. This is the standard Cousot–Cousot result; the new code inherits it from the operator's contract rather than re-proving it.

---

## 5. Design decision: which blocks are widening points

This is the one genuine design choice; everything else is mechanical.

**Requirement:** widen on at least one block per cycle (for termination), and as few blocks as possible (for precision). The textbook answer is **loop headers** — the target of a back-edge.

**Detecting back-edges with what the solver already has.** `run-analysis` computes `reverse-postorder` (`dataflow.scm:135`) and a `rank-map` assigning each block its position in flow order (`dataflow.scm:319-324`). A flow-edge `p → b` is a **back-edge** exactly when `rank-of(p) ≥ rank-of(b)` — the predecessor sits at or after the successor in reverse-postorder. A block `b` is a **widening point** iff it has at least one predecessor `p` with `rank-of(p) ≥ rank-of(b)`.

This is a sound over-approximation of loop headers for reducible CFGs and remains correct (terminating) for irreducible ones: any cycle has at least one edge `p → b` with `rank-of(p) ≥ rank-of(b)`, so at least one block per cycle is flagged. Precision on irreducible CFGs may suffer (more widening points than strictly necessary), which is acceptable and standard.

**Why not Bourdoncle weak topological ordering (WTO)?** WTO yields a provably minimal, optimally-ordered set of widening points and is the production-grade choice. It is also a substantially larger algorithm (recursive SCC decomposition into nested components). For v1, RPO-rank back-edge detection is simple, reuses existing solver state, and is correct. WTO is a precision/performance follow-up, not a correctness prerequisite. **Recommend: ship RPO back-edge detection in v1, note WTO as deferred.**

**Alternative considered and rejected — widen everywhere.** Trivially terminating, but discards join precision on straight-line and branch-only code (which is most code). Rejected: it would make interval analysis pointlessly imprecise on exactly the cases the sign domain handles well today.

---

## 6. Supporting helpers

All small; the design weight is entirely in §4–§5.

### 6.1 `abstract-interval`

```scheme
(define (abstract-interval n) (cons n n))   ; int -> point interval [n,n]
```

The analog of `abstract-sign` (`abstract-domain.scm:33`). Ships in `(wile algebra interval)`.

### 6.2 Bottom-aware interval arithmetic

`interval-add/sub/mul` (`interval.scm:103-128`) must treat `interval-bot` as absorbing — an unreachable operand yields an unreachable result — matching `sign-binop`'s bottom handling. Either guard each operator, or wrap them. **Recommend a single shared guard** rather than three copies (avoid the hand-unrolled-loop smell):

```scheme
(define (interval-lift op)
  (lambda (a b)
    (if (or (eq? a 'interval-bot) (eq? b 'interval-bot)) 'interval-bot (op a b))))
```

Decision: whether to change the public `interval-add/sub/mul` semantics (they currently *raise* on bottom) or add bottom-aware variants. Changing them is the right call — raising on bottom is a latent bug for any lattice client, and Wile is pre-consumer (break freely; CLAUDE.md versioning). **Recommend: make `interval-add/sub/mul` bottom-aware directly.**

### 6.3 `interval-widen`

The standard operator: keep a bound if stable, else jump to infinity.

```scheme
(define (interval-widen cur next)
  (if (eq? cur 'interval-bot) next
      (cons (if (inf<= (car cur) (car next)) (car cur) 'neg-inf)
            (if (inf<= (cdr next) (cdr cur)) (cdr cur) 'pos-inf))))
```

Verified to converge the motivating loop to `(0 . pos-inf)` via `fixpoint/widen`. Ships in `(wile algebra interval)`.

### 6.4 Galois-connection instances

The framework and laws-checker exist; only instances are missing. Concrete domain = finite sets of integers (sorted lists); abstract = interval or sign.

```scheme
;; P(Z) <-> interval
;; alpha(S) = [min S, max S];  gamma([a,b]) = {x : a <= x <= b}
(make-galois-connection alpha-iv gamma-iv subset-po (interval-lattice))
```

A hand-built instance of exactly this shape already passes `gc-sound?` on sample elements (`#t`). Ship as `interval-galois-connection` and `sign-galois-connection` constructors. `gamma` for unbounded intervals returns a sentinel (`'unbounded`) rather than enumerating; the sound-check samples bounded elements.

---

## 7. Soundness story (the Galois tie-in)

The Galois connection is not decoration — it is the theorem that licenses trusting a `run-analysis` result. For the interval domain:

- `α` (abstraction) and `γ` (concretization) form an adjunction: `α(S) ⊑ iv  ⟺  S ⊆ γ(iv)`.
- `gc-sound?` spot-checks the two laws (soundness: `S ⊆ γ(α(S))`; reductiveness: `α(γ(iv)) ⊑ iv`) — both verified `#t` on samples.
- The transfer functions must be **sound abstractions** of the concrete operations: `α(f_concrete(S)) ⊑ f_abstract(α(S))`. For interval arithmetic this is the four-corner / endpoint-sum construction already implemented.

The deliverable here is a documented worked example: take an interval `run-analysis` result, exhibit the Galois connection, and show that the abstract answer over-approximates the concrete set of reachable values — making "the analysis says `0 ≤ x`" a claim with a proof behind it, not a plausible guess. This is the conceptual payoff and belongs in the tutorial (candidate: a new chapter or an extension of `09-dataflow-analysis.scm`).

---

## 8. Open questions

1. **Home library for `abstract-interval`.** `abstract-domain` currently houses `abstract-sign`/`sign-binop` and is described as the "pre-built domains" library, but `interval` is its declared sibling and already owns the arithmetic. Putting `abstract-interval` + `interval-widen` in `(wile algebra interval)` keeps interval concerns together; putting them in `abstract-domain` keeps all *dataflow-facing* abstractions together. **Lean: `(wile algebra interval)`** (cohesion with the arithmetic), but worth a one-line confirmation.
2. **Public-semantics change to `interval-add/sub/mul`.** Making them bottom-aware changes observable behavior (raise → return `interval-bot`). Pre-consumer, so allowed — confirm no internal caller depends on the raise.
3. **Widening-point API surface.** Should `(widen OP)` always derive widening points internally (RPO back-edges), or also accept a caller-supplied predicate `(widen OP point?)` for callers who computed WTO themselves? **Lean: internal-only for v1**, predicate as a later overload.
4. **Narrowing.** Explicitly deferred (§2). Confirm that shipping widening *without* narrowing is acceptable for v1 — it is sound, just imprecise after the widen (bounds pinned to ∞ until a future narrowing pass tightens them).

---

## 9. Provisional phasing (for the `-impl.md`)

1. **Helpers** — `abstract-interval`, bottom-aware `interval-add/sub/mul`, `interval-widen`, with unit tests (`test/wile/algebra-interval-*.scm`). Independently shippable, no solver change.
2. **Widening hook in `run-analysis`** — `(widen OP)` tagged arg + RPO back-edge widening-point detection. Tests: the motivating increment/decrement loops now *terminate* and return `(0 . pos-inf)` / `(neg-inf . pos-inf)`; sign-domain tests unchanged (widening absent ⇒ identical behavior).
3. **Galois instances** — `interval-galois-connection`, `sign-galois-connection`, each with a `gc-sound?` regression test.
4. **Tutorial + docs** — worked interval-dataflow example with the Galois soundness certificate; update `docs/algebra/reference.md` and `overview.md` (the overview mermaid already lists interval + Galois — verify it stays accurate).

Each phase ends with `make lint && make covercheck` green per the project build-clean rule.

---

## 10. Summary

~90% of the machinery already ships. The one real task is **threading a widening operator into `run-analysis`** so infinite-height lattices terminate; the one real design decision is **which blocks widen** (RPO back-edge loop headers, WTO deferred). The remaining work is four small, independently-testable helpers. The Galois side needs no structural work — only convenience instances and a documented example tying α/γ to the soundness of a dataflow result.
