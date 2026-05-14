+++
title = "Implementation plan — (wile algebra incidence)"
date  = "2026-04-21"
status = "Design locked — ready to implement"
parent = "2026-04-17-algebra-foundations-directions.md §5.2"
+++

# Implementation Plan — `(wile algebra incidence)`

Incidence algebras and Möbius functions on locally-finite posets, per
Rota (1964). Ships the "Möbius / incidence" direction called out in
`2026-04-17-algebra-foundations-directions.md` §2.1 / §5.2 as the
smallest-effort principled fix in the foundation layer.

---

## Context

Four wile-goast posets handle direct-vs-transitive translation ad-hoc
today (dominator trees, subtype lattices, call-graph reachability,
import DAGs); the belief-DSL's overlap normalization is inclusion-
exclusion on the subset lattice, also ad-hoc. A general Möbius-function
abstraction replaces each with a declarative formulation.

Prerequisite libraries `(wile algebra order)` (`order.scm:1-73`) and
`(wile algebra ring)` (`ring.scm:1-184`) are shipped and stable.

---

## Scope

### In scope

1. `stdlib/lib/wile/algebra/incidence.sld` + `incidence.scm` — new
   library, ~200 lines including docstrings.
2. `test/wile/algebra-incidence-test.scm` — full test suite.
3. `stdlib/lib/wile/algebra.sld` — umbrella re-exports the new surface.

### Out of scope (future work)

- `incidence-invert` over arbitrary functions (requires unipotent
  detection or field; μ is the only classical instance — it gets its
  own named function). Can be added additively when a consumer surfaces.
- Topological Möbius via order complex (directions doc §2.5 — connects
  to homology; separate track).
- Characteristic polynomial of a poset (depends on rank function, not
  yet needed).

---

## Resolved design decisions

| Question | Choice | Rationale |
|----------|--------|-----------|
| Q1 — Library placement | **(b)** new `(wile algebra incidence)` | Parallels `(wile algebra polynomial)` — structure over a ring. No collision with `fca.scm`'s informal "incidence" (FCA relation, not algebra). |
| Q2 — Poset representation | **(b)** `<locally-finite-poset>` with `(leq? interval-proc)` | Matches Rota's definition — the incidence algebra is defined for locally-finite posets, not merely finite ones. Unlocks divisor lattice, Young's lattice, subword order without library rework. |
| Q3 — Ring parameter | **(c)** parameterized, default `(integer-ring)` | Matches `polynomial` idiom (`make-poly R coeffs`). Free generality — classical Möbius is the integer specialization. |
| Q4 — Memoization | **(b)** lazy via hashtable | Forced by Q2 — infinite posets forbid eager tabulation. `make-hashtable` uses `equal?` (see `registry/core/hashtables.go:24`), so `(cons x y)` keys work structurally. |
| Q5 — API shape | Record-based only | Matches `polynomial` library's pattern — no bare `poset-mobius` shortcut. Caller always goes through `make-incidence-algebra`. |

---

## Exports

    ;; Locally-finite poset — new richer type
    make-locally-finite-poset locally-finite-poset?
    lf-poset-leq? lf-poset-interval

    ;; Incidence algebra over a ring
    make-incidence-algebra incidence-algebra?
    incidence-algebra-poset incidence-algebra-ring

    ;; Canonical elements (return procedures over intervals)
    zeta-function mobius-function

    ;; Convolution in the incidence algebra
    incidence-convolve

    ;; Classical Möbius inversion
    mobius-inversion

---

## Representation

### `<locally-finite-poset>`

    (define-record-type <locally-finite-poset>
      (make-locally-finite-poset leq? interval)
      locally-finite-poset?
      (leq?     lf-poset-leq?-fn)
      (interval lf-poset-interval-fn))

- `leq? : (λ (x y) → boolean)` — partial-order predicate. Reflexive,
  antisymmetric, transitive.
- `interval : (λ (x y) → list)` — enumerates `[x,y] = {z : x ≤ z ≤ y}`.
  **Required invariants:** returns `'()` when `¬(x ≤ y)`; returns
  `(list x)` when `x = y`; each returned element `z` satisfies
  `x ≤ z ≤ y`; no duplicates (element equality via R7RS `equal?`).

No element-list is stored. The existing `<partial-order>` in
`(wile algebra order)` remains unchanged — this library introduces a
richer type alongside it, not a replacement.

**Compatibility helper** (convenience, not core):

    (define (finite-set->locally-finite-poset leq? elements)
      (make-locally-finite-poset
        leq?
        (lambda (x y)
          (filter (lambda (z) (and (leq? x z) (leq? z y))) elements))))

This lets callers with an explicit finite element set use the library
without fabricating an interval procedure. Lives in `incidence.scm`,
exported for convenience.

### `<incidence-algebra>`

    (define-record-type <incidence-algebra>
      (make-incidence-algebra* poset ring mu-cache)
      incidence-algebra?
      (poset    incidence-algebra-poset)
      (ring     incidence-algebra-ring)
      (mu-cache incidence-algebra-mu-cache))

    (define make-incidence-algebra
      (case-lambda
        ((poset)      (make-incidence-algebra* poset (integer-ring)
                                                 (make-hashtable)))
        ((poset ring) (make-incidence-algebra* poset ring
                                                 (make-hashtable)))))

- Default ring: `(integer-ring)`. Explicit ring parameter for modular
  / rational / other.
- `mu-cache` is a hashtable keyed on `(cons x y)` pairs, storing
  ring elements. Initialized empty; populated lazily by
  `mobius-function`.

---

## Algorithms

### ζ (zeta) — trivial

    ;; ζ(x,y) = 1 if x ≤ y, else 0
    (define (zeta-function IA)
      (let ((R     (incidence-algebra-ring IA))
            (poset (incidence-algebra-poset IA)))
        (lambda (x y)
          (if ((lf-poset-leq? poset) x y)
              (ring-one R)
              (ring-zero R)))))

No memoization — constant-time per call.

### μ (Möbius) — lazy memoized recursion

Definition (Rota 1964):

    μ(x,x) = 1
    μ(x,y) = -Σ_{x ≤ z < y} μ(x,z)    for x < y
    μ(x,y) = 0                          when ¬(x ≤ y)

Implementation skeleton:

    (define (mobius-function IA)
      (lambda (x y) (compute-mu IA x y)))

    (define (compute-mu IA x y)
      (let ((cache (incidence-algebra-mu-cache IA))
            (key   (cons x y)))
        (let ((hit (hashtable-ref cache key #f)))
          (or hit
              (let* ((result (compute-mu-uncached IA x y)))
                (hashtable-set! cache key result)
                result)))))

    (define (compute-mu-uncached IA x y)
      (let ((R     (incidence-algebra-ring IA))
            (poset (incidence-algebra-poset IA)))
        (cond
          ((not ((lf-poset-leq? poset) x y))
           (ring-zero R))
          ((equal? x y)
           (ring-one R))
          (else
           (let* ((iv     ((lf-poset-interval poset) x y))
                  (proper (filter (lambda (z) (not (equal? z y))) iv))
                  (sum    (fold (lambda (z acc)
                                  (ring-plus R acc (compute-mu IA x z)))
                                (ring-zero R)
                                proper)))
             (ring-negate R sum))))))

Complexity: each `compute-mu(x,y)` enumerates `[x,y]` once (O(|iv|))
and recurses on proper sub-intervals. Memoization yields O(n²)
distinct entries for a finite n-element poset, with total work
bounded by O(n³) — matches the theoretical floor for dense Möbius
tabulation.

**Element equality note.** `equal?` is used for element comparison
(identifying `z = y` in the interval, hashtable key equality). This is
R7RS `equal?` — structural for pairs, vectors, strings, bytevectors,
records-with-equal-fields; `eqv?`-equivalent for atoms. Callers using
exotic element types (e.g., mutable records where `eq?` identity
matters) must preprocess to equivalence-class canonical forms. This is
the same constraint as the existing `<partial-order>` and consistent
across the library.

### Convolution

    ;; (f * g)(x,y) = Σ_{x ≤ z ≤ y} f(x,z) · g(z,y)
    (define (incidence-convolve IA f g)
      (let ((R     (incidence-algebra-ring IA))
            (poset (incidence-algebra-poset IA)))
        (lambda (x y)
          (if (not ((lf-poset-leq? poset) x y))
              (ring-zero R)
              (let ((iv ((lf-poset-interval poset) x y)))
                (fold (lambda (z acc)
                        (ring-plus R acc
                          (ring-times R (f x z) (g z y))))
                      (ring-zero R)
                      iv))))))

Returns a procedure — consistent with `zeta-function` / `mobius-function`
and with the classical view that incidence-algebra elements are
functions on the interval set. User f and g are not memoized by the
library — callers that want caching wrap their own.

### Möbius inversion

Given `g(x) = Σ_{y ≤ x} f(y)`, the classical inversion formula is:

    f(x) = Σ_{y ≤ x} μ(y,x) · g(y)

A locally-finite poset does **not** in general have finite principal
ideals `{y : y ≤ x}` (e.g., ℤ with ≤). Therefore the API requires the
caller to supply the ideal explicitly:

    ;; (mobius-inversion IA g x lower-set) → R
    ;; PRECONDITION: lower-set contains exactly {y ∈ P : y ≤ x}.
    ;; Violation: result undefined (no runtime validation for perf).
    (define (mobius-inversion IA g x lower-set)
      (let ((R  (incidence-algebra-ring IA))
            (mu (mobius-function IA)))
        (fold (lambda (y acc)
                (ring-plus R acc
                  (ring-times R (mu y x) (g y))))
              (ring-zero R)
              lower-set)))

For posets with a bottom `⊥` reachable by the interval-proc,
`(lf-poset-interval poset ⊥ x)` returns the principal ideal — caller
convention, not library invariant.

---

## Test plan (`algebra-incidence-test.scm`)

Tests use `(chibi test)` idiom already established across the algebra
suite. Target ~25 tests grouped:

1. **Classical divisor lattice μ(n) on {1..12}** — μ(1,12) = 0, μ(1,30) = -1,
   μ(1,p) = -1 for primes p. Verifies against known values. Uses
   `finite-set->locally-finite-poset` with divides? predicate.

2. **Subset-lattice μ (inclusion-exclusion)** — μ(A,B) = (-1)^|B\A| for
   A ⊆ B. Four-element ground set; checks all 16 subsets.

3. **Chain μ** — On `{1,2,...,n}` with ≤, μ(i,j) = 1 if i = j; -1 if j
   = i+1; 0 otherwise.

4. **ζ behavior** — ζ(x,y) ∈ {0, 1}; ζ*μ = δ (Kronecker delta) spot-
   checked on a 6-element poset.

5. **Convolution identity** — (δ * f)(x,y) = f(x,y) where δ is the
   Kronecker delta; (f * δ)(x,y) = f(x,y).

6. **Möbius inversion roundtrip** — for a random f on subset lattice,
   compute g = Σ f over lower-set, then recover f via
   `mobius-inversion`; assert equality.

7. **Ring parameter** — μ on chain {1,2,3} computed over `(modular-ring 7)`:
   μ(1,2) = 6 mod 7 (i.e., -1 ≡ 6).

8. **Lazy memoization** — call `(mu x y)` twice; assert hashtable size
   grew only on first call. Counts via `hashtable-size`.

9. **Edge cases** — μ(x,x) = 1; μ(x,y) = 0 when ¬(x ≤ y); interval
   returning `'()` handled without division-by-zero.

10. **Locally-finite without finite element set** — construct a poset
    on ℕ with divisibility where `interval` uses trial-division;
    verify μ(1,30) = -1 without any global element enumeration.

---

## Commit strategy

One feature branch, phased commits. Sequence:

| Phase | Commit | Contents |
|-------|--------|----------|
| P1 | scaffold | `.sld` + `.scm` skeleton, records, imports; umbrella re-export stubs |
| P2 | ζ + μ | `zeta-function`, `mobius-function` with memoization |
| P3 | convolution | `incidence-convolve` |
| P4 | inversion | `mobius-inversion` |
| P5 | tests | full test suite |
| P6 | docs + polish | docstrings with `Category:`/`Keywords:`, examples, umbrella integration verified |

Each phase compiles and tests pass before moving on. Typical cadence:
six commits, one PR.

---

## Definition of done

- `make lint && make covercheck` clean.
- `algebra-incidence-test.scm` passes — 25+ tests.
- Umbrella `(wile algebra)` re-exports all new surface.
- Docstrings include `Category: algebra` and `Keywords:` per
  `keywords-motivation.md` convention.
- MCP `apropos` finds the new surface on queries like "mobius",
  "incidence", "poset", "inversion".
- No regressions in `algebra-accuracy` benchmark.

---

## Future extensions (deferred)

- **`incidence-invert`** for arbitrary unipotent f (f(x,x) = 1 for all
  x). Algorithm is an O(n³) back-substitution analogous to μ's
  definition. Add when a non-ζ unipotent function appears.
- **Topological Möbius** via the order complex / nerve construction.
  Connects to simplicial homology; separate library, separate plan.
- **Characteristic polynomial** of a graded poset with rank function.
  Needs rank abstraction; defer until graded-poset consumers exist.
- **Finite-set shortcut API** — `make-finite-poset leq? elements`
  returning a `<locally-finite-poset>` with a precomputed element
  membership check. Today the convenience helper
  `finite-set->locally-finite-poset` covers this; promote if adoption
  warrants a dedicated predicate.
