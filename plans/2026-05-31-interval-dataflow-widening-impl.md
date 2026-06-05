# Interval Dataflow + Galois Connections — Implementation Plan

**Status**: Ready to implement. Companion to `2026-05-31-interval-dataflow-widening-design.md`.
**Date**: 2026-06-05.
**Flow**: branch `feat/interval-dataflow-widening` → 4 phases (one commit each) → `make ci` → PR → dual review.

## Resolved open questions (from design §8)

- **Q1 — home for `abstract-interval` / `interval-widen`**: `(wile algebra interval)` (cohesion with the arithmetic). The Galois instances live with the domain they abstract to: `interval-galois-connection` in `interval.scm`, `sign-galois-connection` in `abstract-domain.scm` — keeps `galois.scm` a dependency-free framework (depend toward stability; galois must not import specific domains).
- **Q2 — bottom-aware `interval-add/sub/mul`**: make them bottom-aware **directly** (return `interval-bot` when either operand is bottom). Verified safe: `grep` shows **no caller outside `interval.scm`** depends on the current raise-on-bottom behavior. Pre-consumer license applies.
- **Q3 — widening-point API**: internal-only for v1 (RPO back-edge detection inside `run-analysis`). A caller-supplied predicate overload is a later addition.
- **Q4 — narrowing**: deferred. Shipping widening without narrowing is sound, just imprecise after the widen (bounds pinned to ∞). Acceptable for v1.

## Source anchors (verified 2026-06-05)

| Target | File:loc |
|---|---|
| `interval-add/sub/mul` (not bottom-aware) | `interval.scm:103,107,111` |
| `inf<=`, `inf-min/max`, `inf+/-/*` | `interval.scm:18-70` |
| `init-state` record (mirror for `widen`) | `dataflow.scm:128-131` |
| `run-analysis` arg-parse loop | `dataflow.scm:~298-318` |
| `rank-map` / `rank-of` / `flow-preds` | `dataflow.scm:319-334` |
| in-val join (the edit point) | `dataflow.scm:391-401` |
| `make-galois-connection`, `gc-sound?` | `galois.scm:16,30` |
| `abstract-sign` / `sign-binop` (pattern) | `abstract-domain.scm:33,76` |
| `fixpoint/widen` (contract reference) | `lattice.scm:129` |

---

## Phase 1 — Interval helpers (no solver change, independently shippable)

**`stdlib/lib/wile/algebra/interval.scm`:**

1. **Bottom-aware arithmetic.** Add a shared guard rather than three copies (avoid hand-unrolled smell):
   ```scheme
   (define (interval-lift op)
     (lambda (a b)
       (if (or (eq? a 'interval-bot) (eq? b 'interval-bot)) 'interval-bot (op a b))))
   ```
   Rewrite `interval-add/sub/mul` so the public bindings are `(interval-lift <core>)`, with the existing bodies moved to internal `interval-add* / -sub* / -mul*` cores. Keeps docstrings on the public names.
2. **`abstract-interval`**: `(define (abstract-interval n) (cons n n))` — point interval `[n,n]`, analog of `abstract-sign`. Full docstring (Parameters/Returns/Category + examples).
3. **`interval-widen`**: the standard operator (design §6.3) — keep a bound if stable, else jump to ∞; `interval-bot` → `next`. Full docstring.

**`stdlib/lib/wile/algebra/interval.sld`**: export `abstract-interval`, `interval-widen` (and `interval-lift` only if useful externally — likely keep internal).

**Tests** (`test/wile/algebra-interval-test.scm`, extend existing):
- `interval-add/sub/mul` with `interval-bot` operand → `interval-bot` (was: raise).
- `abstract-interval 5` → `(5 . 5)`.
- `interval-widen`: `(0 . 0)` vs `(0 . 1)` → `(0 . pos-inf)`; stable bound preserved; `interval-bot` cur → next.
- Convergence check: `(fixpoint/widen (interval-lattice) (lambda (iv) (interval-add iv '(1 . 1))) '(0 . 0) interval-widen)` → `(0 . pos-inf)`.

Gate: `make lint && make covercheck`.

---

## Phase 2 — Widening hook in `run-analysis` (the one real change)

**`stdlib/lib/wile/algebra/dataflow.scm`:**

1. **Tagged wrapper** mirroring `init-state` (after line 131):
   ```scheme
   (define-record-type <widen> (widen op) widen? (op widen-op))
   ```
2. **Arg-parse loop** (`~298-318`): add a third clause binding `widen-fn` from `(widen-op (car args))`; reject duplicates like `init-state`. Thread `widen-fn` out of the `let*-values` (default `#f`).
3. **Widening-point set** (after `rank-of`/`flow-preds` are defined, before the worklist loop): a block `idx` is a widening point iff some flow-predecessor `p` has `rank-of(p) >= rank-of(idx)` (back-edge target, design §5). Compute once into a predicate `widening-point?` (build a list of such idxs; only when `widen-fn` is set — skip the work entirely otherwise).
4. **in-val edit** (`391-401`): bind the join result to `joined-in`; then
   ```scheme
   (in-val (if (and widen-fn (widening-point? idx))
               (widen-fn (get-in idx) joined-in)
               joined-in))
   ```
   `get-in idx` is the previous iterate (already available).
5. **Docstring**: document the `(widen OP)` optional arg in `run-analysis`'s doc block (alongside `init-state`/`check-monotone`), noting it is required for infinite-height lattices and a no-op (identical to pure MFP) when absent.

**`dataflow.sld`**: export `widen widen? widen-op`.

**Tests** (`test/wile/algebra-dataflow-test.scm` — create if absent, else extend):
- **Termination + correctness**: the increment loop `x=1; while(?) x=x+1` over `(interval-lattice)` with `(widen interval-widen)` **terminates** and yields `(0 . pos-inf)` (or the loop's documented bound). Decrement loop → `(neg-inf . _)`.
- **No-widen regression**: same sign-domain analyses without `(widen ...)` return byte-identical results to today (widening absent ⇒ pure MFP). Pin one existing sign result.
- **Widening-point detection**: a straight-line CFG has zero widening points (raw join everywhere); a single-loop CFG flags exactly the header.
- Without widening on the interval loop: assert it would not converge (guard with `(fixpoint ... fuel)` or omit to avoid a hang — prefer asserting the *with-widening* path converges).

Gate: `make lint && make covercheck`.

---

## Phase 3 — Galois instances

**`interval.scm`** — `interval-galois-connection`:
- `α(S)` = `[min S, max S]` over a non-empty sorted int list; `α('())` = `interval-bot`.
- `γ([a,b])` = `{x : a ≤ x ≤ b}` for bounded `[a,b]`; unbounded → sentinel `'unbounded` (do not enumerate).
- concrete-po = subset/containment partial order on sorted int lists; abstract = `(interval-lattice)`.
- Built via `make-galois-connection`. Requires `make-partial-order` (verify its home/import; likely `(wile algebra order)`).

**`abstract-domain.scm`** — `sign-galois-connection`: α = `abstract-sign` lifted over a set (join of per-element signs); γ = representative concrete set; abstract = `(sign-lattice)`.

**`.sld`**: export both constructors.

**Tests**: each connection passes `gc-sound?` on bounded samples (extensive + reductive). Mirror the in-design hand-built instance that already returned `#t`.

Gate: `make lint && make covercheck`.

---

## Phase 4 — Tutorial + docs

- Worked example tying an interval `run-analysis` result to its Galois connection (the soundness certificate) — extend `09-dataflow-analysis.scm` (tutorial) or add a chapter.
- Update `docs/algebra/reference.md` (interval widening + Galois instances) and verify `overview.md`'s mermaid (already lists interval + Galois) stays accurate.
- Update `(wile algebra)` umbrella re-exports if the new public bindings should surface there.

Gate: `make ci` (full).

---

## Risk register

| Risk | Mitigation |
|---|---|
| Widening makes a finite-height (sign) analysis lose precision | Widening is opt-in; absent `(widen ...)` ⇒ behavior byte-identical. Phase 2 regression test pins this. |
| Widening-point over-approximation on irreducible CFGs | Sound (terminating) by design §5; precision loss acceptable, documented. |
| `interval-bot` semantics change breaks a caller | Verified no external caller depends on the raise (Q2). |
| `make-partial-order` import wrong for galois instances | Resolve in Phase 3; fall back to inline po if needed. |
| Convergence test hangs if widening misbehaves | Test asserts the *with-widening* path converges; never run the un-widened interval loop unbounded. |

## Validation summary

Per-phase `make lint && make covercheck`; final `make ci`. The headline acceptance test: the motivating loop, non-terminating today on the interval lattice, terminates under `(widen interval-widen)` and returns the expected bounded-below interval — with a `gc-sound?`-backed Galois connection certifying the result over-approximates the concrete reachable set.
