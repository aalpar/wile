# CFL-Reachability Path Algebra — Design

**Date**: 2026-06-05.
**Status**: Design draft. Implementation plan (`-impl.md`) to follow after approval.
**Library**: new `(wile algebra cfl)`.
**Consumer**: wile-goast Track C4 ("CFL-reachability — context-sensitive analysis"),
the single demand-justified open algebra item (see `TODO.md` Tier A and the
2026-06-05 wile ↔ wile-goast demand audit).

## Motivation — why a new library, not a `semiring.scm` preset

Every shipped path-algebra in wile-goast is a **semiring**: edge labels compose
under an associative `times` with identity `1` and annihilator `0`
(Boolean-semiring reachability, tropical shortest-path). CFL-reachability
composes labels under a **context-free grammar** — a path from `s` to `t`
"counts" iff its label string lies in `L(A)` for a nonterminal `A`. Derivation
is grammar-constrained: not associative, no `(plus, times, 0, 1)` shape. It
therefore cannot be parameterized from the existing semiring API — which is
exactly why wile-goast's C4 Boolean/tropical sub-items shipped but this one
stalled with the note "can't be parameterized from existing semiring API."

The canonical program-analysis applications are **interprocedural** reachability
(call/return edges must balance) and **field-sensitivity** (field read/write
brackets must balance). Both are *Dyck-language* (matched-delimiter) instances
of the general problem.

Algorithm: the Melski–Reps / Reps–Horwitz–Sagiv (1995) dynamic-programming
worklist over `(s, A, t)` triples. On a finite graph the triple set is finite,
so the worklist reaches a fixpoint and **terminates trivially** — there is no
productive-cycle / infinite-derivation hazard. Complexity `O(n³·|G|)`.

## Scope (v1)

General context-free-grammar engine **plus** a `dyck-grammar` preset (decided
2026-06-05). The preset is the program-analysis entry point; the general engine
satisfies the broadest-application priority. Out of scope for v1: arbitrary-RHS
grammar normalization (the typed constructors enforce normal form by
construction — long-RHS grammars are normalized by the caller), demand/single-
source evaluation (all-pairs is the standard formulation), node setoids, and a
`with-cfl-grammar` binder. Each is a clean later addition gated on a consumer.

## API

### Grammar — typed production kernels + record

Four production kernels, each one of the four normal forms, so a grammar is
**normalized by construction** (no separate CNF transform to build or get
wrong):

```
(cfl-epsilon  A)        ; A → ε       seeds (n, A, n) for every node n
(cfl-terminal A t)      ; A → t       seeds (s, A, d) for every edge s ──t──▶ d
(cfl-unary    A B)      ; A → B
(cfl-binary   A B C)    ; A → B C
```

Each returns an opaque `<cfl-production>`. Collected into a grammar:

```
(make-cfl-grammar start productions) → <cfl-grammar>
(cfl-grammar? x)
(cfl-grammar-start g)            ; the start nonterminal
(cfl-grammar-productions g)      ; the production list
(validate-cfl-grammar g) → #t | (list-of violation)
```

`validate-cfl-grammar` enforces the one real correctness trap — **the terminal
and nonterminal symbol sets must be disjoint** — plus: `start` has ≥1
production; every `cfl-unary`/`cfl-binary` RHS symbol is a nonterminal (appears
as some production's LHS); a symbol used in `cfl-terminal` as the terminal `t`
is never also a nonterminal. Symbols are plain Scheme symbols; the LHS of a
production is always a nonterminal.

### Graph — labeled directed edges

```
(make-cfl-graph nodes edges) → <cfl-graph>   ; edges = list of (from label to)
(cfl-graph? x)
(cfl-graph-nodes G)
(cfl-graph-edges G)
(validate-cfl-graph G) → #t | (list-of violation)   ; edges reference declared nodes
```

Node equality is `equal?` for v1 (nodes are typically symbols or small data).

### Solver + query (mirrors `graph.sld`'s `make-graph-analysis` / `graph-query`)

```
(cfl-solve grammar graph) → <cfl-solution>     ; closes the (s,A,t) relation
(cfl-reachable? sol s t)     → #t iff (s, START, t) is derivable
(cfl-reachable-from sol s)   → list of t with (s, START, t)
(cfl-reachable-pairs sol)    → list of (s . t) under START
(cfl-derives? sol s A t)     → general (s, A, t) for any nonterminal A
```

`cfl-derives?` exposes the full closed relation — free given the solver already
computes it, and it broadens applicability beyond the start symbol.

### Dyck preset — the program-analysis entry point

```
(dyck-grammar bracket-pairs) → <cfl-grammar>
;; bracket-pairs = list of (open-label . close-label)
```

Builds, for start `S` and each pair i:
`S → ε`, `S → S S`, `S → Oᵢ Tᵢ`, `Tᵢ → S Cᵢ`, `Oᵢ → openᵢ`, `Cᵢ → closeᵢ`.
So `S` derives exactly the balanced strings over the declared brackets. A
wile-goast interprocedural analysis calls
`(dyck-grammar '((call₁ . return₁) (call₂ . return₂) …))` — one pair per
call site — and never hand-writes a grammar.

## Solver algorithm (normative)

`cfl-solve` runs the worklist to fixpoint over a set `R` of `(s, A, t)` triples:

1. **Seed.** For each `A → ε`: add `(n, A, n)` for every node `n`. For each
   `A → t` and every edge `s ──t──▶ d`: add `(s, A, d)`.
2. **Propagate.** Pop `(s, A, t)`. Then:
   - for each `B → A` (unary): add `(s, B, t)`;
   - for each `B → A C` (binary, A on the left): for every `(t, C, e) ∈ R`, add `(s, B, e)`;
   - for each `B → C A` (binary, A on the right): for every `(e, C, s) ∈ R`, add `(e, B, t)`.
   Adding an already-present triple is a no-op (this is what bounds the work).
3. **Stop** when the worklist drains. `R` is the closed relation; index it by
   `(start, s)` for `cfl-reachable?`/`-from`/`-pairs` and keep the full `R` for
   `cfl-derives?`.

## The canary test (the core acceptance criterion)

This is the test that proves the library does something `semiring.scm` cannot:
**CFL/Dyck reachability is more precise than plain Boolean reachability** — it
excludes interprocedurally-infeasible paths whose call/return brackets do not
match. If this passes, the library earns its existence.

The graph models one procedure `p` reached from two call sites. Plain
reachability says `a1 ⇝ b2` (there is a directed path `a1 → p → b2`); Dyck
reachability must say it does **not** hold, because the path's label string
`call₁ return₂` is unbalanced.

```scheme
;; test/wile/algebra-cfl-test.scm  (excerpt — the canary)
(import (scheme base) (wile algebra cfl))

;; Procedure p, single node. Two call sites:
;;   caller A:  a1 --call1--> p --return1--> a2
;;   caller B:  b1 --call2--> p --return2--> b2
(define g
  (make-cfl-graph
    '(a1 a2 b1 b2 p)
    '((a1 call1 p) (p return1 a2)
      (b1 call2 p) (p return2 b2))))

(define dyck
  (dyck-grammar '((call1 . return1) (call2 . return2))))

(define sol (cfl-solve dyck g))

;; Balanced, interprocedurally-feasible paths ARE start-reachable:
(check-true  (cfl-reachable? sol 'a1 'a2)  "matched call1/return1: a1 ⇝ a2")
(check-true  (cfl-reachable? sol 'b1 'b2)  "matched call2/return2: b1 ⇝ b2")

;; THE CANARY — mismatched brackets: a directed path EXISTS (plain
;; reachability would say #t), but the call/return don't balance, so
;; CFL reachability is precise and excludes it:
(check-false (cfl-reachable? sol 'a1 'b2)  "call1 then return2 is infeasible: a1 ⇏ b2")
(check-false (cfl-reachable? sol 'b1 'a2)  "call2 then return1 is infeasible: b1 ⇏ a2")
```

The proof is the **asymmetry under one solve**: `a1⇝a2` is `#t` and `a1⇝b2`
is `#f`, yet both have a directed path of length 2 (`a1 → p → a2` and
`a1 → p → b2`). Plain Boolean reachability cannot tell them apart — it would
return `#t` for both. Only a grammar-constrained (Dyck) reachability returns
`#f` for the mismatched pair. The canary asserts exactly this, using only the
public `cfl-reachable?` surface (no dependence on the preset's internal
nonterminal naming).

### Worked derivation (why the canary holds under the normative solver)

Seeded terminals: `(a1,OPEN1,p)`, `(p,CLOSE1,a2)`, `(b1,OPEN2,p)`, `(p,CLOSE2,b2)`.
ε gives `(n,S,n)` for all `n`. Then `T1 → S C1` gives `(p,T1,a2)`, and
`S → O1 T1` gives `(a1,S,a2)` ✓. Symmetrically `(b1,S,b2)` ✓. For `(a1,S,b2)`:
the only `S`-production reaching `b2` is `O2 T2`, but `a1` has no `OPEN2` out-
edge; and `O1 T1` from `a1` ends only at `a2`. `S → S S` composes balanced
factors but `a2` has no out-edges, so it adds nothing new. Hence `(a1,S,b2) ∉ R`
✓ — the canary's `#f` is forced by the semantics.

## Other tests

- **Dyck nesting**: `[[ ]]`-shaped graph (`call1` twice then `return1` twice)
  is start-reachable; one-sided (`call1 call1 return1`) is not.
- **Sequence**: `S → S S` — two balanced groups in series are reachable.
- **General (non-Dyck) grammar**: an `aⁿbⁿ` grammar over a linear graph,
  exercising the engine outside the Dyck preset.
- **Validation negatives**: terminal/nonterminal collision; `start` with no
  production; `cfl-binary` RHS that is a terminal; edge to an undeclared node.

## Files

- `stdlib/lib/wile/algebra/cfl.sld` + `cfl.scm`
- `test/wile/algebra-cfl-test.scm`
- Umbrella re-export in the `(wile algebra)` umbrella `.sld`
- `docs/algebra/reference.md` section + cross-reference row
- `examples/algebra/tutorial/quick-tour/cfl.scm` + the three index tables
  (so it lands documented from day one — unlike sat/matching, which needed a
  later catch-up)

## References

- Reps, Horwitz, Sagiv (1995). "Precise interprocedural dataflow analysis via
  graph reachability." POPL.
- Melski, Reps (2000). "Interconvertibility of a class of set constraints and
  context-free-language reachability." TCS.
- Reps (1998). "Program analysis via graph reachability." Information &
  Software Technology — the Dyck/L-reachability survey.
