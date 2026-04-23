# Algebra Tutorial

A runnable, self-verifying walkthrough of `(wile algebra ...)`. Every chapter is a `.scm` file under [`examples/algebra/tutorial/`](../../examples/algebra/tutorial/). Read the file, run it, modify it.

Drift between tutorial and library cannot hide: CI runs every chapter via `make tutorial-test`. Any mismatch in expected output fails the build.

## How to run

One chapter:

```
wile --file examples/algebra/tutorial/chapters/01-getting-started.scm
```

Everything (CI-enforced):

```
make tutorial-test
```

Each successful check prints `  ok  <label>` (two-space lead-in and gap, for cheap visual scanning). A failed check prints the mismatch and exits non-zero. No special test harness -- just `wile --file <path>`.

## Reading order and prerequisites

Deep chapters build on each other; work through them top-to-bottom. Quick-tour files are standalone -- read the ones that match your interest.

Dependency graph (one arrow = "builds on"). The table below is authoritative when the two disagree:

```
                ┌─► 05
                │
01 ──► 02 ──────┼─► 06
                │
                ├─► 07
                │
                ├─► 08 ──► 09
                │
                └─► 03 ──► 04 ──► 10
                          │
                          └─► 11
```

| Chapter | Topic | Depends on |
|---------|-------|------------|
| 01 | Getting started: monoids | -- |
| 02 | Structures: lattice, semiring, group, ring, field, differential, Boolean | 01 |
| 03 | Rewriting basics: axioms, normalizers | 01, 02 |
| 04 | Boolean simplifier: theories, recursive normalization, Boolean vs Heyting | 03 |
| 05 | Symbolic and polynomial differentiation | 02 |
| 06 | Combinatorial graph algorithms | 02 |
| 07 | Group actions: orbits, stabilizers, Burnside | 02 |
| 08 | Lattice presets, Birkhoff, Dedekind numbers, Möbius | 02 |
| 09 | Dataflow analysis: MFP solver, sign domain | 02, 08 |
| 10 | Unification: syntactic and AC-modulo | 03, 04 |
| 11 | Equivalence discovery across sub-theories | 03, 04 |

## Deep chapters

Each chapter covers one problem that composes several sub-libraries.

| File | What it covers |
|------|---------------|
| [`chapters/01-getting-started.scm`](../../examples/algebra/tutorial/chapters/01-getting-started.scm) | `(wile algebra monoid)` -- construction, fold, power, validate, non-numeric carriers, catching fake monoids (subtraction with 0) |
| [`chapters/02-structures.scm`](../../examples/algebra/tutorial/chapters/02-structures.scm) | `lattice`, `semiring`, `group`, `ring`, `field`, `differential`, `boolean`, `heyting` -- the forgetful projection tower with explicit substitution checks |
| [`chapters/03-rewriting-basics.scm`](../../examples/algebra/tutorial/chapters/03-rewriting-basics.scm) | `rewrite` -- term protocols, all seven axiom types with firing and non-firing examples, composed normalizers, manual fixpoint loop |
| [`chapters/04-boolean-simplifier.scm`](../../examples/algebra/tutorial/chapters/04-boolean-simplifier.scm) | `boolean`, `heyting`, `symbolic`, `rewrite` -- `boolean->theory` with 11 axioms, recursive normalizer + trace inspection, Heyting vs Boolean on `(not (not x))`, `symbolic-boolean-normalize` facade |
| [`chapters/05-symbolic-differentiation.scm`](../../examples/algebra/tutorial/chapters/05-symbolic-differentiation.scm) | `polynomial`, `differential`, `ring` -- polynomials over Z, `poly-derivative`, `polynomial-derivation` differential ring, hand-written symbolic differentiator (plain S-expression walker, not library-based) cross-checked against `poly-derivative` at 8 concrete x values |
| [`chapters/06-graph-algorithms.scm`](../../examples/algebra/tutorial/chapters/06-graph-algorithms.scm) | `combinatorial-graph` -- K_n, C_n, Petersen, K_{3,3}; BFS/DFS; Hopcroft-Karp; τ(K_n) = n^(n-2); τ(Petersen) = 2000; chromatic polynomials on K_n, C_n, empty; Tutte polynomial; C_6 vs 2·K_3 isomorphism canary |
| [`chapters/07-group-actions.scm`](../../examples/algebra/tutorial/chapters/07-group-actions.scm) | `group` -- presets (trivial, cyclic, symmetric, product), preset actions (natural, regular, conjugation), orbit + stabilizer (verifying orbit-stabilizer identity), Burnside on necklaces (C_4 on 2^4 colorings = 6 necklaces, C_2 on 2^2 = 3) |
| [`chapters/08-lattice-presets.scm`](../../examples/algebra/tutorial/chapters/08-lattice-presets.scm) | `lattice`, `incidence` -- five preset lattices; `distributive?` + `modular?` distinguishing M_3 and N_5; Birkhoff roundtrip; Dedekind D(0)..D(4); Möbius on the divisor poset of 12 |
| [`chapters/09-dataflow-analysis.scm`](../../examples/algebra/tutorial/chapters/09-dataflow-analysis.scm) | `dataflow`, `abstract-domain`, `lattice` -- sign-lattice and its five elements, `sign-binop` with annihilation and top propagation, linear 3-block CFG + 5-block branching CFG showing merge-induced top |
| [`chapters/10-unification.scm`](../../examples/algebra/tutorial/chapters/10-unification.scm) | `unification` -- pattern variables, substitutions (lookup/compose/apply), syntactic unification via `ac-unify` with empty theory, AC unification over `+`, `diophantine-basis` for Stickel's kernel, `flatten-ac` |
| [`chapters/11-equivalence-discovery.scm`](../../examples/algebra/tutorial/chapters/11-equivalence-discovery.scm) | `rewrite`, `symbolic`, `boolean` -- `discover-equivalences` across sub-theories, theory combinators (`filter`, `exclude`, `prioritize`, `merge`), `format-trace`, fuel exhaustion |

## Quick-tour files

One per sub-library not featured in a deep chapter. Each is ~50-100 lines: construct the structure, exercise 3-5 core operations, validate if applicable, one representative use case.

| File | Library | Focus |
|------|---------|-------|
| [`quick-tour/setoid.scm`](../../examples/algebra/tutorial/quick-tour/setoid.scm) | `setoid` | Built-in presets (eqv/default/numeric/string); custom case-insensitive string setoid |
| [`quick-tour/partial-order.scm`](../../examples/algebra/tutorial/quick-tour/partial-order.scm) | `order` | Divisibility as a PO; comparability, monotonicity, validation with/without setoid |
| [`quick-tour/closure.scm`](../../examples/algebra/tutorial/quick-tour/closure.scm) | `closure` | Downward closure on {1..5}; closed-elements; closure->closed-lattice |
| [`quick-tour/category.scm`](../../examples/algebra/tutorial/quick-tour/category.scm) | `category` | Procedure category, composition, endomorphism monoid projection |
| [`quick-tour/galois.scm`](../../examples/algebra/tutorial/quick-tour/galois.scm) | `galois` | Sign abstraction on `{-1, 0, 1}`; `gc-sound?` validation |
| [`quick-tour/fca.scm`](../../examples/algebra/tutorial/quick-tour/fca.scm) | `fca` | Mammals/carnivores context; intent, extent, concept lattice |
| [`quick-tour/graph.scm`](../../examples/algebra/tutorial/quick-tour/graph.scm) | `graph` (abstract; distinct from `combinatorial-graph`) | Boolean-semiring reachability, tropical-semiring shortest path |
| [`quick-tour/interval.scm`](../../examples/algebra/tutorial/quick-tour/interval.scm) | `interval` | Infinity-aware arithmetic, four-corner multiplication, interval lattice with containment ordering |
| [`quick-tour/matrix.scm`](../../examples/algebra/tutorial/quick-tour/matrix.scm) | `matrix` | 2x2 counting-semiring arithmetic, identity, powers, Boolean adjacency reachability |
| [`quick-tour/pareto.scm`](../../examples/algebra/tutorial/quick-tour/pareto.scm) | `pareto` | Factor comparison over booleans and numbers, two-objective car dominance, frontier |

## Structure of each file

```scheme
;; ========================================================
;; Chapter NN -- Title
;;
;; What you will learn: ...
;; Prerequisites: ...
;; Sub-libraries used: ...
;; ========================================================

(import ...)
(include "../lib/check.scm")

;; ---------------------------------------------------------
;; Part 1: concept
;; ---------------------------------------------------------
(define ...)
(check= ...)

;; ---------------------------------------------------------
;; Exercises (commented out; uncomment to try)
;; ---------------------------------------------------------
;; (check= (...) <?>  "your turn")
```

`lib/check.scm` provides five helpers:

- `(check= actual expected label)` -- equal? comparison
- `(check-approx= actual expected tolerance label)` -- floating-point
- `(check-true actual label)` -- must be truthy
- `(check-false actual label)` -- must be strictly #f
- `(check-error thunk label)` -- thunk must raise

## See also

- [`overview.md`](overview.md) -- design philosophy, structure hierarchy, the three library layers.
- [`reference.md`](reference.md) -- complete API reference for every sub-library.
- `test/wile/algebra-*.scm` -- the library's own test suite (independent of this tutorial).
