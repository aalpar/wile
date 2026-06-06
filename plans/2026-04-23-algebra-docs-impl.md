+++
title = "Implementation plan — algebra tutorial + reference catch-up"
date  = "2026-04-23"
status = "Shipped (PR #706); post-ship sat + matching catch-up done 2026-06-05"
parent = "2026-04-14-algebra-documentation-impl.md (shipped)"
+++

# Implementation Plan — Algebra Tutorial + Reference Catch-up

> **Status note (2026-06-05).** Phases 1–9 shipped in PR #706 (tutorial
> scaffold, 11 deep chapters, quick-tours, overview/index refresh, legacy
> `examples/algebra/*.scm` retired). PR #765 later added the `interval`
> quick-tour with the widening/Galois example. Two sub-libraries that
> shipped *after* this plan was authored — `(wile algebra matching)`
> (2026-05-02) and `(wile algebra sat)` (2026-05-30) — were left
> undocumented in the tutorial. The 2026-06-05 catch-up closes that gap:
> `quick-tour/matching.scm`, `quick-tour/sat.scm`, the `## SAT` reference
> section + cross-ref row (matching's reference section already existed),
> and the README / tutorial.md / overview.md index tables. The success
> criterion "every `*.sld` has a chapter or quick-tour" now holds for all
> 28 sub-libraries.

Ships the user-facing algebra documentation to match the library's current
surface. Three deliverables, one PR:

1. A new tutorial under `examples/algebra/tutorial/` — tiered (deep chapters
   for the ~11 core structures, quick-tour files for the remaining
   sub-libraries), thematic (chapters organized by problem, not by
   sub-library), self-verifying (assertions via `check=`), integrated into
   `make test`.
2. Reference catch-up in `docs/algebra/reference.md` — adds entries for 11
   undocumented sub-libraries and updates 5 existing entries whose surface
   has grown since the reference was last touched.
3. Overview refresh in `docs/algebra/overview.md` — hierarchy diagrams
   extended, Learning Path retargeted, new pattern entry for preset
   structures.

The existing `examples/algebra/*.scm` files (six of them) are absorbed into
the new tutorial structure and their old locations deleted.

## Scope (ship)

### New files

- `examples/algebra/tutorial/README.md` — tutorial index and run instructions.
- `examples/algebra/tutorial/lib/check.scm` — `check=`, `check-approx=`,
  `check-true`, `check-false`, `check-error`.
- `examples/algebra/tutorial/chapters/01-getting-started.scm`
- `examples/algebra/tutorial/chapters/02-structures.scm`
- `examples/algebra/tutorial/chapters/03-rewriting-basics.scm`
- `examples/algebra/tutorial/chapters/04-boolean-simplifier.scm`
- `examples/algebra/tutorial/chapters/05-symbolic-differentiation.scm`
- `examples/algebra/tutorial/chapters/06-graph-algorithms.scm`
- `examples/algebra/tutorial/chapters/07-group-actions.scm`
- `examples/algebra/tutorial/chapters/08-lattice-presets.scm`
- `examples/algebra/tutorial/chapters/09-dataflow-analysis.scm`
- `examples/algebra/tutorial/chapters/10-unification.scm`
- `examples/algebra/tutorial/chapters/11-equivalence-discovery.scm`
- `examples/algebra/tutorial/quick-tour/setoid.scm`
- `examples/algebra/tutorial/quick-tour/partial-order.scm`
- `examples/algebra/tutorial/quick-tour/closure.scm`
- `examples/algebra/tutorial/quick-tour/category.scm`
- `examples/algebra/tutorial/quick-tour/galois.scm`
- `examples/algebra/tutorial/quick-tour/fca.scm`
- `examples/algebra/tutorial/quick-tour/graph.scm`
- `examples/algebra/tutorial/quick-tour/matrix.scm`
- `examples/algebra/tutorial/quick-tour/pareto.scm`
- `docs/algebra/tutorial.md` — thin markdown index (60–100 lines).

### Modified files

- `docs/algebra/reference.md` — add 11 new sections, update 5 existing
  sections, update cross-reference table at bottom. See §Reference Updates.
- `docs/algebra/overview.md` — extend hierarchy diagrams to include
  `abstract-domain`, `dataflow`, `combinatorial-graph`; retarget Learning
  Path to new tutorial locations; add "preset structures" pattern entry.
  See §Overview Updates.
- `docs/INDEX.md` — mention tutorial entry point under algebra.
- `docs/TOC.md` — add `tutorial.md` bullet under `## Algebra`.
- `Makefile` — add `tutorial-test` target; append to `test` target.

### Deleted files

- `examples/algebra/getting-started.scm`
- `examples/algebra/structures.scm`
- `examples/algebra/rewriting.scm`
- `examples/algebra/symbolic.scm`
- `examples/algebra/boolean-simplifier.scm`
- `examples/algebra/equivalence-discovery.scm`

## Explicitly not in scope

- Go source code changes (no R7RS citation additions, no primitive
  refactors).
- New non-algebra tutorials.
- `CLAUDE.md` updates at any level.
- Reorganization of the `docs/` topic tree.
- Formatting/style harmonization of existing reference sections. New entries
  follow the existing template; pre-existing asymmetries are not cleaned up
  here.
- Inline examples in `reference.md`. The reference stays a reference;
  examples live in the tutorial.

## Chapter contents

Each chapter is a runnable `.scm` file with block-comment prose, `check=`
assertions, and optional commented-out "try this yourself" exercises.
Lengths are estimates; the actual cut-off is "every nontrivial claim is
asserted."

### 01 — Getting Started (~200 lines)

Sub-libraries: `(wile algebra monoid)`.

- What a monoid is; the two-field record.
- `make-monoid`, `monoid-fold`, `monoid-power`, `monoid-identity`,
  `monoid-op`.
- `validate-monoid` on a valid monoid (integer addition) and an invalid one
  (subtraction — not associative).
- `with-monoid` destructuring.
- Monoids on non-numeric carriers (string concatenation, list append, max
  with `-inf.0`).

### 02 — Structures (~350 lines)

Sub-libraries: `lattice`, `semiring`, `group`, `ring`, `differential`,
`boolean`.

- Lattice: `make-lattice`, `join`, `meet`, `fixpoint`.
- Integer semiring, cross to integer ring by adding negate.
- Ring → differential ring by adding a derivation.
- Ring → field (rational numbers) by adding reciprocal.
- Forgetful projections: `field->ring`, `ring->semiring`, `ring->group`,
  `group->monoid`. Verify that every projection preserves the relevant
  operations.
- Boolean algebra (via `(wile algebra boolean)`), projection
  `boolean->heyting->lattice`, cross-tower `boolean->ring`.

### 03 — Rewriting Basics (~300 lines)

Sub-libraries: `rewrite`.

- Term protocols (what `(wile algebra rewrite)` needs from an
  S-expression).
- All seven axiom types: identity, commutativity, absorbing, idempotence,
  involution, absorption, associativity. One `check=` per axiom showing a
  firing rewrite and a non-firing rewrite.
- `make-normalizer` composition.
- Single-step vs fixed-point rewriting.

### 04 — Boolean Simplifier (~300 lines)

Sub-libraries: `boolean`, `heyting`, `symbolic`, `rewrite`.

- Build a concrete Boolean algebra.
- Derive its theory via `boolean->theory`.
- Simplify a handful of expressions; compare against hand-computed normal
  forms via `check=`.
- Show what Boolean can do that Heyting cannot (double negation
  elimination): build a Heyting algebra, derive its theory, show the same
  input produces a different normal form.
- Introduce `symbolic-boolean-normalize` and verify it agrees with the
  manual theory construction on a shared test set.

### 05 — Symbolic Differentiation (~450 lines)

Sub-libraries: `polynomial`, `differential`, `symbolic`, `ring`.

- Construct `(wile algebra polynomial)` over the integer ring.
- Build polynomials from coefficient lists; evaluate, add, multiply, take
  formal derivatives.
- Derivative of x^n via the polynomial library's native derivative.
- Symbolic differentiation of S-expression terms via `(wile algebra
  differential)` + `(wile algebra symbolic)` — product rule, sum rule,
  chain rule.
- Verify that the symbolic derivative matches the polynomial derivative on
  shared test inputs.
- GCD and polynomial ring operations.

### 06 — Graph Algorithms (~500 lines)

Sub-libraries: `combinatorial-graph`.

- `<graph>` record, vertex setoid, tier-1/tier-2/tier-3 distinction.
- BFS and DFS on K_n, C_n, Petersen.
- Bipartite check on K_{3,3} (yes), on Petersen (no), on odd cycle (no).
- Graph isomorphism: K_4 isomorphic to itself with relabeled vertices;
  C_6 vs two triangles (cospectral but non-isomorphic — the canary).
- Spanning-tree count: τ(K_n) = n^(n-2) for n ≤ 6; τ(Petersen) = 2000.
- Chromatic polynomial: P(K_n, k) = k(k-1)…(k-n+1); P(C_n, k) = (k-1)^n + (-1)^n (k-1).
- Tutte polynomial: small fixtures.
- Hopcroft-Karp on a small bipartite example; check matching size against
  König's theorem.

### 07 — Group Actions (~400 lines)

Sub-libraries: `group`.

- Preset groups: `trivial-group`, `cyclic-group`, `symmetric-group`,
  `product-group`, `dihedral-group`.
- `enumerate-finite-group`, `subgroup-closure`.
- `<group-action>` record; `trivial-action`, `permutation-action`,
  `regular-action`, `conjugation-action`, `product-action`.
- Orbit, stabilizer, fixed-points on small examples.
- Burnside's lemma: count necklaces / bracelets; verify against closed-form
  expressions.
- `orbit-representative` tie-breaker behavior.

### 08 — Lattice Presets (~350 lines)

Sub-libraries: `lattice`, `incidence`.

- `chain-lattice`, `boolean-lattice`, `diamond-lattice` (M_3),
  `pentagon-lattice` (N_5), `free-distributive-lattice`.
- `distributive?` — yes on chain, on boolean-lattice, on free-distributive;
  no on M_3, N_5.
- `modular?` — yes on M_3; no on N_5.
- `join-irreducibles` and `meet-irreducibles` on each preset.
- Birkhoff's representation theorem: `birkhoff-representation` + reconstruct
  via `birkhoff-reconstruction`; verify roundtrip isomorphism.
- Dedekind numbers through D(5) = 7581 via free-distributive-lattice.
- `lattice->locally-finite-poset` projection.

### 09 — Dataflow Analysis (~400 lines)

Sub-libraries: `dataflow`, `abstract-domain`, `lattice`.

- `sign-domain` from `(wile algebra abstract-domain)`: construction,
  `abstract-sign`, `sign-binop`.
- Build a small straight-line program represented as a list of statements.
- CFG protocol: `<cfg-protocol>` record, accessors required by `mfp-solve`.
- Run `mfp-solve` and inspect `analysis-in`, `analysis-out`, `analysis-states`.
- Interval domain from `(wile algebra interval)` as an alternative abstract
  domain — plug into the same CFG protocol.

### 10 — Unification (~300 lines)

Sub-libraries: `unification`.

- `<pattern-var>` records via `parse-pattern`.
- Substitutions, `empty-substitution`, substitution composition.
- Syntactic unification with `unify`; occurs-check examples.
- AC unification via `ac-unify` — Stickel reduction, pure-variable case.
- Empty list = no solution; error = misuse.
- `diophantine-basis` on a small instance; verify against hand-computed
  Hilbert basis.

### 11 — Equivalence Discovery (~250 lines)

Sub-libraries: `rewrite`, `symbolic`, `boolean`, `ring`.

- Adapt existing `equivalence-discovery.scm` content.
- `discover-equivalences` across sub-theories.
- Show that "a + b" and "b + a" collapse under a commutative theory but not
  under a non-commutative theory.
- Practical framing: "which axioms does your system assume?"

## Quick-tour template (~50–100 lines each)

Every quick-tour file follows this shape:

    ;; quick-tour: <sub-library>
    ;;
    ;; What this library is (2 sentences).
    ;; When you would reach for it (1 sentence).

    (import (wile algebra <lib>))
    (include "../lib/check.scm")

    ;; Part 1: Construction.
    ;; ...

    ;; Part 2: Core operations (3–5 `check=` lines).
    ;; ...

    ;; Part 3: Validation (if the library ships one).
    ;; ...

    ;; Part 4: One representative use.
    ;; ...

Nine quick-tour files, one per sub-library not covered by a deep chapter:
`setoid`, `partial-order`, `closure`, `category`, `galois`, `fca`,
`graph` (abstract graph, distinct from `combinatorial-graph`), `matrix`,
`pareto`.

Libraries that appear in deep chapters (e.g., `heyting`, `incidence`,
`interval`, `semiring`, `abstract-domain`) are not given a separate
quick-tour file. Their deep-chapter appearance is the coverage.

## Reference updates

### Eleven new sections (in `docs/algebra/reference.md`)

Each follows the existing template: `Constructors` → `Predicates` →
`Operations` → `Validation` → `Destructuring` → any structure-specific
subsection.

- `abstract-domain` — sign lattice + sign arithmetic
  (`make-sign-domain`, `sign-add`, `sign-mul`, `sign-neg`, `sign-domain`
  preset).
- `combinatorial-graph` — `<graph>` record, algorithms (constructors,
  BFS/DFS, `bipartite?`, isomorphism, `spanning-tree-count`,
  `chromatic-polynomial`, `tutte-polynomial`, Hopcroft-Karp).
- `dataflow` — MFP worklist (`make-dataflow-problem`, `mfp-solve`,
  `<cfg-protocol>` accessors).
- `fca` — Formal concept analysis (context, concept lattice, intents,
  extents).
- `graph` — abstract graph (distinct from `combinatorial-graph`):
  construction, traversal, path predicates.
- `incidence` — locally-finite posets + Möbius (`<locally-finite-poset>`
  with new optional `elements`, `mobius`, `zeta`, `incidence-convolve`,
  `mobius-inversion`).
- `interval` — interval arithmetic (constructors, arithmetic ops,
  containment, width).
- `matrix` — dense + sparse (constructors, arithmetic, transpose,
  determinant, solve; cross-reference to `2026-04-21-matrix-path-d-impl.md`).
- `pareto` — dominance, front construction, layering.
- `polynomial` — univariate/multivariate (construction, +, *, quot, rem,
  gcd, eval, derivative).
- `unification` — `unify`, `ac-unify`, pattern-var helpers,
  `diophantine-basis`, substitution suite.

### Five updates to existing sections

- `lattice`: add preset constructors, `distributive?`, `modular?`,
  `join-irreducibles`, `meet-irreducibles`, `birkhoff-representation`
  (+ `/unchecked`), sample validators, new optional metadata fields on
  `<lattice>`.
- `group`: add preset constructors (trivial/cyclic/symmetric/product/dihedral),
  `enumerate-finite-group`, `subgroup-closure`, group-actions surface —
  `<group-action>`, all preset actions, `orbit`, `stabilizer`,
  `fixed-points`, `orbit-representative`, `burnside-count`.
- `incidence`: note new optional `elements` field on
  `<locally-finite-poset>`, `lf-poset-elements` accessor.
- `symbolic`: add `symbolic-boolean-normalize` and
  `symbolic-boolean-equivalent?`.
- `unification`: if the section already exists it gets `ac-unify` +
  `diophantine-basis` additions; otherwise covered by the new-sections
  list above.

### Cross-reference table

Append rows for all new sub-libraries to the "Cross-Reference: Sub-library
to Import Path" table at the bottom of `reference.md`.

## Overview updates

In `docs/algebra/overview.md`:

- **Hierarchy diagrams**: extend the Foundation diagram with
  `abstract-domain` as a lattice specialization ("sign domain is one
  concrete abstract domain layered on Lattice"). Add a new **Analysis**
  diagram cluster for `dataflow` (depending on lattice + abstract-domain +
  CFG protocol) and `combinatorial-graph` (with
  chromatic-polynomial / tutte-polynomial feeding into `polynomial`).
- **Learning Path**: replace the current six-item numbered list pointing at
  `examples/algebra/*.scm` with an eleven-item list pointing at
  `examples/algebra/tutorial/chapters/*.scm`. Add a short final section
  listing the quick-tour files.
- **Patterns**: add a new pattern entry, "Preset structures" — covers
  `cyclic-group`, `chain-lattice`, `sign-domain`, etc. as a distinct idiom
  (constructors that bake in classical examples).

## `check=` helper

`examples/algebra/tutorial/lib/check.scm`:

    (define (check= actual expected label)
      (if (equal? actual expected)
          (begin (display "  ok  ") (display label) (newline))
          (begin
            (display "  FAIL ") (display label) (newline)
            (display "    expected: ") (write expected) (newline)
            (display "    actual:   ") (write actual) (newline)
            (error "tutorial check failed" label))))

Plus `check-approx=` (tolerance parameter for floating-point),
`check-true`, `check-false`, `check-error`. Fifteen lines total, ballpark.

Load via `(include "../lib/check.scm")` — we verify Wile's include
resolution against this path during Phase 1 smoke test; if relative include
misbehaves, fall back to `(load ...)`.

## Make target

    tutorial-test: build
    	@for f in examples/algebra/tutorial/chapters/*.scm \
    	          examples/algebra/tutorial/quick-tour/*.scm; do \
    	  echo "-- $$f"; \
    	  ./dist/$(GOOS)/$(GOARCH)/wile --file "$$f" || exit 1; \
    	done

Append to the existing `test` target so `make test` runs tutorial
verification alongside Go tests.

## Phases and commits

Following `CLAUDE.md` plan convention and user preference for large
commits. Each phase is one commit.

### Phase 1 — Scaffolding + smoke chapter

- `examples/algebra/tutorial/lib/check.scm` with all five helpers.
- `examples/algebra/tutorial/README.md` stub.
- `Makefile` `tutorial-test` target + append to `test`.
- `examples/algebra/tutorial/chapters/01-getting-started.scm` in full.
- Verify `make tutorial-test` passes.

Commit: `feat(docs/algebra): scaffold tutorial infrastructure + chapter 01`.

### Phase 2 — Deep chapters 02–06

- Chapters 02 (structures), 03 (rewriting-basics), 04 (boolean-simplifier),
  05 (symbolic-differentiation), 06 (graph-algorithms).
- `make tutorial-test` green.

Commit: `feat(docs/algebra): tutorial chapters 02-06 (structures, rewriting, boolean, symbolic, graphs)`.

### Phase 3 — Deep chapters 07–11

- Chapters 07 (group-actions), 08 (lattice-presets), 09 (dataflow-analysis),
  10 (unification), 11 (equivalence-discovery).
- `make tutorial-test` green.

Commit: `feat(docs/algebra): tutorial chapters 07-11 (groups, lattices, dataflow, unification, equivalence)`.

### Phase 4 — Quick-tour files

- All nine `quick-tour/*.scm` files.
- `make tutorial-test` green.

Commit: `feat(docs/algebra): tutorial quick-tour files for remaining sub-libraries`.

### Phase 5 — Reference catch-up: new sections

- Eleven new sections in `docs/algebra/reference.md`.
- Cross-reference table updated.

Commit: `docs(algebra): reference sections for 11 undocumented sub-libraries`.

### Phase 6 — Reference catch-up: updates

- Updates to `lattice`, `group`, `incidence`, `symbolic`, `unification`
  sections.

Commit: `docs(algebra): reference updates for lattice/group/incidence/symbolic/unification`.

### Phase 7 — Overview + index + legacy cleanup

- Overview diagrams, Learning Path, patterns entry.
- `docs/algebra/tutorial.md` index.
- `docs/INDEX.md`, `docs/TOC.md` updates.
- Delete `examples/algebra/{getting-started,structures,rewriting,symbolic,boolean-simplifier,equivalence-discovery}.scm`.

Commit: `docs(algebra): refresh overview + index; retire examples/algebra/*.scm in favor of tutorial/`.

### Phase 8 — Verification pass

- `make tutorial-test`, `make test`, `make lint`, `make covercheck`, `make ci`.
- Fix any regressions.
- Self-review diff (Copilot-hat pass).

Commit (if fixes needed): `fix(docs/algebra): address verification findings`.

### Phase 9 — PR

- `gh pr create` with test plan and reference list.
- Request Copilot review.
- Dispatch `/crosscheck:crosscheck all`.
- Address findings per `plans/CLAUDE.md` §Implementation Completion Workflow.

## Success criteria

- `make test` passes (includes `tutorial-test`).
- Every sub-library under `stdlib/lib/wile/algebra/*.sld` has either a deep
  tutorial chapter or a quick-tour file.
- Every public procedure added since `2026-04-14-algebra-documentation-impl.md`
  appears in `docs/algebra/reference.md`.
- `docs/algebra/overview.md` hierarchy diagrams include `abstract-domain`,
  `dataflow`, `combinatorial-graph`.
- `docs/INDEX.md` and `docs/TOC.md` link to the tutorial.
- `examples/algebra/*.scm` (the six legacy files) are deleted.
- `make lint` and `make covercheck` clean.

## Risks and mitigations

- **Tutorial file execution time.** 20 files, each loading libraries and
  running checks, could slow `make test` meaningfully. Mitigation: keep
  quick-tour files lean; if `make tutorial-test` runs over ~30s, split into
  `tutorial-test-chapters` and `tutorial-test-quick-tour` and run only the
  former on every `make test`, the latter in `make ci`.
- **Include-path resolution.** `(include "../lib/check.scm")` may interact
  poorly with Wile's resolver. Mitigation: Phase 1 smoke test verifies;
  fall back to `(load ...)` or absolute path if necessary.
- **Reference drift during authoring.** The library may see further commits
  while this plan is in flight. Mitigation: last commit before PR rebases
  on master and re-greps for sub-library exports to catch additions.
- **"New algebra features" scope creep.** The scan window is "since
  2026-03-01." Anything older is out of scope even if undocumented; file
  a follow-up plan rather than absorbing it here.

## References

- Prior algebra docs plan: `2026-04-14-algebra-documentation-impl.md`
  (shipped, moved to `memory/`).
- Algebra library implementations:
  `stdlib/lib/wile/algebra/*.{sld,scm}`.
- Recent algebra work:
  - `2026-04-21-matrix-path-d-impl.md` (matrix polymorphism).
  - `2026-04-21-incidence-algebra-impl.md` (Möbius/incidence).
  - `2026-04-21-ac-matching-impl.md` (AC unification).
  - `2026-04-22-group-actions-burnside-impl.md` (group actions).
  - `2026-04-22-lattice-birkhoff-impl.md` (Birkhoff, presets).
  - `2026-04-22-combinatorial-graph-impl.md` (graph library).
  - `2026-04-22-wile-goast-algebra-extraction-design.md`
    + `-impl.md` (abstract-domain, dataflow, symbolic-boolean).
- Doc conventions: `docs/CLAUDE.md`.
- Plan conventions: `plans/CLAUDE.md`.
