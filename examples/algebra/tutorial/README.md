# Wile algebra tutorial

Runnable walkthrough of `(wile algebra ...)` -- a self-verifying tutorial
that doubles as an acceptance-test surface. Every chapter is a plain
Scheme file: read it, run it, modify it.

## How to run

Run a single chapter:

    wile --file examples/algebra/tutorial/chapters/01-getting-started.scm

Run every chapter and every quick-tour file (enforced by CI):

    make tutorial-test

Each file prints `ok` lines for successful checks and exits non-zero on
any mismatch. No special test harness -- just `wile --file <path>`.

## Reading order

The **deep chapters** build on each other. Work through them in order:

| Chapter | Topic | Sub-libraries |
|---------|-------|---------------|
| `chapters/01-getting-started.scm` | Monoids from scratch | `monoid` |
| `chapters/02-structures.scm` | Lattice to ring to field tower | `lattice`, `semiring`, `group`, `ring`, `differential`, `boolean` |
| `chapters/03-rewriting-basics.scm` | Axioms, normalizers, traces | `rewrite` |
| `chapters/04-boolean-simplifier.scm` | Boolean vs Heyting simplification | `boolean`, `heyting`, `symbolic`, `rewrite` |
| `chapters/05-symbolic-differentiation.scm` | Polynomial calculus + hand-written S-expression differentiator | `polynomial`, `differential`, `ring` |
| `chapters/06-graph-algorithms.scm` | BFS, isomorphism, Tutte, Hopcroft-Karp | `combinatorial-graph` |
| `chapters/07-group-actions.scm` | Orbits, stabilizers, Burnside | `group` |
| `chapters/08-lattice-presets.scm` | Canonical lattices, Birkhoff | `lattice`, `incidence` |
| `chapters/09-dataflow-analysis.scm` | MFP solver, sign domain | `dataflow`, `abstract-domain`, `lattice` |
| `chapters/10-unification.scm` | Syntactic and AC unification | `unification` |
| `chapters/11-equivalence-discovery.scm` | Sub-theory comparison | `rewrite`, `symbolic`, `boolean` |

The **quick-tour files** in `quick-tour/` cover the sub-libraries not
featured in a deep chapter. Read them a la carte.

| Quick tour | Library |
|------------|---------|
| `quick-tour/setoid.scm` | `setoid` |
| `quick-tour/partial-order.scm` | `order` |
| `quick-tour/closure.scm` | `closure` |
| `quick-tour/category.scm` | `category` |
| `quick-tour/galois.scm` | `galois` |
| `quick-tour/fca.scm` | `fca` |
| `quick-tour/graph.scm` | `graph` (abstract, distinct from `combinatorial-graph`) |
| `quick-tour/interval.scm` | `interval` |
| `quick-tour/matrix.scm` | `matrix` |
| `quick-tour/pareto.scm` | `pareto` |
| `quick-tour/matching.scm` | `matching` (two-sided / stable marriage) |
| `quick-tour/sat.scm` | `sat` |
| `quick-tour/cfl.scm` | `cfl` (context-free-language reachability) |

## How it works

- `lib/check.scm` provides five helpers: `check=`, `check-approx=`,
  `check-true`, `check-false`, `check-error`. Each prints `ok` on pass
  and raises an error (non-zero exit) on fail.
- Chapters include `lib/check.scm` via `(include "../lib/check.scm")`.
- Some chapters include `"try this yourself"` exercises as commented
  `check=` lines -- uncomment, guess the value, run the file.

## See also

- [`docs/algebra/overview.md`](../../../docs/algebra/overview.md) -- design philosophy and structure hierarchy.
- [`docs/algebra/reference.md`](../../../docs/algebra/reference.md) -- complete API reference.
- [`docs/algebra/tutorial.md`](../../../docs/algebra/tutorial.md) -- tutorial index with chapter prerequisites graph.
