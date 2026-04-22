# `(wile algebra unification)` — AC-matching and AC-unification design

**Status:** Design locked; ready for phased implementation.
**Date:** 2026-04-21
**Predecessor:** `plans/2026-04-17-algebra-foundations-directions.md` §5.3
**Task-level plan:** `plans/2026-04-21-ac-matching-impl.md`
**Dependencies shipped:** `plans/2026-04-21-matrix-path-d-impl.md` (§5.1 matrix algebra), `plans/2026-04-21-incidence-algebra-impl.md` (§5.2 incidence algebra)
**Follow-up:** `plans/YYYY-MM-DD-wile-goast-ac-match-migration.md` (to be filed at Phase-6 close)

---

## Context

`(wile algebra symbolic)` ships `discover-equivalences` — a combinatorial-wasteful workaround that enumerates normal forms under every single-axiom sub-theory and deduplicates via `equal?`. Its only workspace consumer is `wile-goast/cmd/wile-goast/lib/wile/goast/unify.scm:421`, which compares two Go AST nodes modulo algebraic theory by testing overlap between two full equivalence-class enumerations.

This plan ships `(wile algebra unification)`, providing AC-matching and AC-unification primitives that replace the enumerate-and-compare pattern with polynomial-time matching/unification modulo associativity and commutativity. The directions-doc estimate of "~400–600 LOC" is honored (library proper) with an additional `diophantine-basis` public export serving broader combinatorial applications per saved algebra-library goal #1.

## Scope

### In scope

- `(wile algebra unification)` library: `ac-match`, `ac-unify`, `diophantine-basis`
- `<pattern-var>` and `<substitution>` record types, `parse-pattern` ergonomic helper
- Reuse of existing `<term-protocol>` (from `rewrite.sld`) and `<theory>` (from `symbolic.sld`) without modification — no changes to their APIs, no shadowing vocabulary
- Phase-gated rollout with the matcher shipping correctness-complete before matrix-permanent optimization
- Comprehensive test suite (~47 tests) covering all public exports
- Integration-path documentation in the plan's dedicated section (deferred as a separate plan)

### Out of scope (deferred)

- wile-goast `unify.scm:421` migration from `discover-equivalences` to `ac-unify` (tracked as separate follow-up plan)
- Retirement of `discover-equivalences` from `symbolic.scm` (tracked with the migration plan)
- Trace-reconstructing variant (`ac-unify-with-trace`) for diagnostic paths
- Small-arity fast path (decision driven by Phase 4 benchmark)
- Hashtable backing for `<substitution>` (deferred pending profile evidence)
- Full E-matching (arbitrary equational theory beyond AC)
- E-graph / equality saturation (different algorithmic track)
- Library split moving `ac-match` into `rewrite.sld` (revisit once usage patterns stabilize)

## Resolved design decisions

| # | Decision | Rationale |
|---|---|---|
| Q1 | Scope: `ac-match` **and** `ac-unify` (not `ac-match` only, not full E-matching) | Ac-unification unlocks term-equation solving beyond the `discover-equivalences` replacement; full E is open-ended scope creep with no current consumer |
| Q2 | Library location: new `(wile algebra unification)`, no deferred split annotation | Unification is conceptually distinct from rewriting — own vocabulary (substitution, MGU/CSU, occurs-check), own return-type semantics, own mental model. Matches pattern set by `polynomial`, `matrix`, `incidence` |
| Q3 | Pattern variables: `<pattern-var>` records with `parse-pattern` ergonomic helper | Records can't collide silently; one canonical form; matches Maude/Stratego/Elan precedent; pattern-var-ness is a property of *the value*, not of *how a caller looks at it*. `parse-pattern` recovers literal-sexpr ergonomics at construction-time without semantic cost downstream |
| Q4 | Integration scope: library-only in this plan; wile-goast migration in follow-up | Decouples API decisions from one consumer's specific usage; lets the library get real-use pressure before consumer is moved |
| — | `ac-unify` returns **list of substitutions (CSU)**, not a single MGU | AC-unification is finitary but not unitary (Stickel 1975 / Fages–Huet 1986). The directions-doc signature `→ mgu or #f` was incorrect; this plan fixes it |
| — | Nonlinear patterns supported by default (no separate `nonlinear-match` export) | `(+ ?x ?x)` matches `(+ a a)`, fails on `(+ a b)` — standard semantics. Goal #1 (broadest application) at zero extra API cost |
| — | Empty-list result means "no solution"; errors are reserved for caller misuse | Consistent with existing `discover-equivalences` return type; composes with `for-each`/`map`; distinguishes semantic failure from bug |

**Anchoring goals** (from `memory/feedback-algebra-design-goals.md`): broadest mathematical application > robust > consistent > performance (via gonum/BLAS/LAPACK-shaped APIs) > brevity/flexibility. Each design decision above is traceable to at least one goal in priority order.

## Layering

New file pair `stdlib/lib/wile/algebra/unification.{scm,sld}`. Imports:

```
(wile algebra unification)
  ├── (scheme base)
  ├── (wile algebra rewrite)   ; <term-protocol>, axiom types (commutativity, associativity)
  ├── (wile algebra symbolic)  ; <theory>, theory-axioms, theory-associative-ops
  └── (wile algebra matrix)    ; boolean-semiring permanent (§5.1) — feasibility pruning
```

No reverse imports. `rewrite`, `symbolic`, `matrix` do not depend on `unification`. The aggregator `(wile algebra)` re-exports `unification` at Phase 6.

## Exports

```scheme
;; Pattern variables
make-pattern-var  pattern-var?  pattern-var-name
parse-pattern                                 ; sexpr with ?-convention → records

;; Substitutions
empty-substitution
make-substitution  substitution?
substitution-lookup                           ; sub × var → term | #f
substitution-bindings                         ; sub → alist ((var . term) ...)
substitution-compose                          ; sub × sub → sub | #f  (#f on occurs-check)
substitution-apply                            ; sub × proto × term → term

;; Matching: pattern has vars, subject is ground
ac-match      ; pattern × subject × theory × proto → list<substitution>

;; Unification: both sides may have vars; returns CSU
ac-unify      ; t1 × t2 × theory × proto → list<substitution>

;; Numeric helper — published for broader combinatorial use (goal #1)
diophantine-basis   ; a × b → list<(u . v)>  minimal ℕ-solutions of a·u = b·v
```

Empty list on no-solution. Errors (via Scheme `error`) reserved for caller misuse: non-theory, non-protocol, negative coefficients, malformed input.

## Representation

### `<pattern-var>`

```scheme
(define-record-type <pattern-var>
  (make-pattern-var name)
  pattern-var?
  (name pattern-var-name))      ; symbol
```

Public constructor: `(make-pattern-var name)`. Minimal — name only. Sort/type constraints are deferred until check semantics are designed (see Future extensions); callers can filter candidates externally via a predicate layer if they need typed patterns in v1.

Identity is **name-based**. Two vars with the same `name` are the same var. `parse-pattern` interns by name during parse, so `(parse-pattern '(+ ?x ?x))` produces two references to one record.

### `<substitution>`

```scheme
(define-record-type <substitution>
  (make-substitution* bindings)
  substitution?
  (bindings substitution-bindings))   ; alist ((var . term) ...)
```

Record-wrapped alist. Alist backing matches established rewriter precedent (goal #4) and is optimal for typical small substitutions (1–5 bindings). Record wrapper means we can swap to hash-table internally later without breaking consumers.

### Reused types

`<term-protocol>` from `rewrite.sld` — unchanged. **Protocol contract** (documented in library header): `term-compare` must be a total order consistent with `equal?` modulo the AC-equivalence induced by the theory. A protocol-conformance test exercises this contract on `sexp-term-protocol`.

`<theory>` from `symbolic.sld` — unchanged. AC-detection at match-start: one pass over `theory-axioms` to identify operators carrying `<commutativity-axiom>` and `<associativity-axiom>` records, cached as a per-theory op-flag table for the matcher's lifetime.

## Algorithms

### AC-match (Eker-style decomposition)

Top-down recursion with AC-operator dispatch.

**Base cases:**

1. Pattern is `<pattern-var>`: if var unbound, extend substitution with `(var . subject)`; if already bound, require `term-compare` equality between subject and existing binding (handles nonlinearity).
2. Pattern is a non-var atom: match iff `term-compare pattern subject = 0`.
3. Pattern is compound, subject is not: empty list.

**Recursive case** — compound pattern `(op p₁…pₘ)` vs compound subject `(op s₁…sₙ)`:

- Operators differ → empty list
- `op` is not AC in theory → require `m = n`, match pᵢ ↔ sᵢ positionally (syntactic recursion)
- `op` is AC in theory → AC-case decomposition below

**AC-case decomposition:**

1. **Flatten** (associativity): `(op a (op b c))` → multiset `⟦a, b, c⟧`, one pass.
2. **Partition pattern positions**: ground / bound-var / free-var / compound.
3. **Peel ground and bound positions greedily**: for each, find a single matching subject element via `term-compare`; remove it from subject multiset. Any failure short-circuits to empty list.
4. **Bipartite feasibility via `(wile algebra matrix)`**: build boolean matrix `M[i,j] = 1` iff remaining pattern position `i` is structurally compatible with remaining subject element `j` (operator matches, arity compatible, no conflict with current bindings). Compute boolean-semiring permanent. **Permanent = 0 → empty list (early prune).** This is the concrete §5.1 consumer.
5. **Enumerate assignments**: backtracking over `M = 1` entries, respecting distinctness; recurse on each assignment.
6. **Variables binding to multisets** (AC+associative only): a free pattern variable in an AC-op position may bind to a non-empty subset of remaining subject elements, re-wrapped as `(op s_{j1} …)`. Enumerated via subset iteration after the bipartite pass.

**Termination:** strict decrease on `|pattern| + |subject|` per recursive call.

**Complexity:** worst-case exponential in pattern arity (AC-match is NP-complete — inherent); polynomial in typical inputs via matrix-permanent early rejection.

**Honest framing on matrix role.** The boolean permanent gives *feasibility* ("does any matching exist?"), not enumeration (counting perfect matchings is #P-complete). Enumeration uses backtracking after feasibility. Overclaiming the matrix "does" the matching would violate goal #2.

### AC-unify (Stickel's Diophantine reduction)

For an AC-op equation `p₁ + … + pₘ =_AC s₁ + … + sₙ` where both sides may contain variables:

1. **Abstract**: assign each distinct pattern variable and each distinct non-variable subterm an integer label. The AC equation becomes a linear Diophantine equation over non-negative integer multiplicities.
2. **Compute basis** via `diophantine-basis`: minimal non-negative integer solutions of the abstracted system.
3. **Reconstruct unifiers**: each basis element corresponds to a candidate unifier; sums of basis elements cover the complete solution space.
4. **Recurse**: non-AC parts use standard Robinson (syntactic) unification; AC parts use the above.
5. **Compose**: merge partial substitutions with `substitution-compose` (occurs-check returns `#f`, discarding that branch).

**Return**: complete set of unifiers (CSU) — possibly empty, possibly exponential in pattern size, always finite (Stickel 1975; Fages–Huet 1986 finiteness proof). Non-AC operators fall through to standard unification; no Diophantine machinery invoked.

### `diophantine-basis` (Contejean–Devie 1994)

**Input**: non-negative integer coefficient vectors `a = (a₁…aₘ)`, `b = (b₁…bₙ)`.
**Output**: list of pairs `(u . v)` with `u ∈ ℕᵐ`, `v ∈ ℕⁿ`, satisfying `a·u = b·v`, each component-wise minimal.

Algorithm sketch:

- BFS over ℕ^(m+n) from unit vectors
- Partial-sum constraint `a·u − b·v` tracks distance from zero at each node
- Prune nodes dominated by known minimal solutions
- Emit nodes where `a·u = b·v`
- Terminate via Dickson's lemma (finitely many minimal ℕ-vectors)

**Publication rationale** (goal #1): basis enumeration has uses beyond unification — Petri-net place invariants, integer-programming combinatorics, algebraic statistics. Exporting the primitive lets consumers beyond unification benefit without re-implementing.

**Shared termination argument.** AC-unification's finitary property (Fages–Huet 1986) and the termination of Contejean–Devie (1994) both rest on Dickson's lemma: any set of non-negative integer vectors has finitely many minimal elements. The two results are the same theorem applied at different levels — CSU finiteness upstairs, basis finiteness downstairs. That's why these three algorithms ship together.

## Integration path with wile-goast (deferred follow-up plan)

The consumer at `wile-goast/cmd/wile-goast/lib/wile/goast/unify.scm:421` becomes:

```scheme
;; Before — combinatorial
(let ((forms-a (map car (discover-equivalences theory proto node-a)))
      (forms-b (map car (discover-equivalences theory proto node-b))))
  (ormap (lambda (a) (member a forms-b)) forms-a))

;; After — polynomial
(not (null? (ac-unify node-a node-b theory proto)))
```

Both sides are ground terms, so the CSU reduces to either `{empty-substitution}` (AC-equal) or `{}` (not). No protocol-adapter changes; wile-goast's existing `<term-protocol>` for Go AST nodes drops into `ac-unify` as-is.

A second, *new* capability emerges: pattern-based beliefs in wile-goast's belief DSL. `(parse-pattern '(binary-op ?f ?x (binary-op ?f ?y ?z)))` plus `ac-match` lets beliefs match modulo AC directly — net-new expressive power, no existing code to migrate.

### Three risks to watch at integration time

1. **Term-protocol contract compliance.** AC-matching requires `term-compare` be a total order consistent with `equal?` modulo AC-theory axioms. wile-goast's current Go-AST protocol likely satisfies this, but the follow-up plan should add a protocol-conformance test before flipping the call site.
2. **Trace-emitting diagnostic paths.** `discover-equivalences` returns `(form . trace)` pairs; `ac-unify` produces no rewrite trace (it's a proof of equality, not a rewrite sequence). Any consumer depending on the trace stays on `discover-equivalences` until a trace-reconstructing variant exists.
3. **Small-arity benchmark.** For operand counts ≤ ~3, Eker-decomposition + matrix permanent may be slower than direct permutation enumeration. Benchmark before claiming the polynomial win; add small-case dispatch iff crossover is real.

Follow-up plan scope: call-site migration (~10 LOC), benchmark harness (~40 LOC), protocol-conformance test (~30 LOC), optional `discover-equivalences` retirement from `symbolic.scm` (~20 LOC). Estimated ~100 LOC total.

## Test plan (~47 tests, `test/wile/algebra-unification-test.scm`)

| Layer | Count | Coverage |
|---|---:|---|
| Pattern vars & parsing | 5 | `make-pattern-var`; `parse-pattern`; var deduplication by name; nested patterns; malformed input error |
| Substitution ops | 7 | empty; lookup hit/miss; compose non-conflicting; compose conflicting; occurs-check returning `#f`; apply to atom/compound; bindings accessor |
| `diophantine-basis` | 5 | `x = y`; Stickel canonical `x + y = z` (two basis elements); asymmetric coefficients; negative-coeff error; empty-vector error |
| AC-matching | 13 | ground AC-equality; single-var; nonlinear `?x ?x`; commutative-only (no flatten); AC flatten; non-AC positional; var binding to multiset; nested AC; operator/arity mismatches; protocol-conformance edge case |
| AC-unification | 13 | ground equality/inequality; basic var; `x + y vs a + b` (two-unifier CSU); nonlinear; mixed arities; infeasible parity; occurs-check; non-AC Robinson fallback; mixed AC + non-AC; small-CSU completeness sanity |
| Integration | 4 | `sexp-term-protocol` interop; theory-driven AC detection; normalize-then-unify vs direct-unify equivalence; stress on moderately-sized random AC terms |

Table-driven where shapes repeat. Each AC-unify CSU-completeness test compares the returned set against a hand-computed reference.

## Commit strategy — 6 phases

Each phase lands green on CI (`make lint && make covercheck`) before the next begins. Progressive, matching `incidence` plan cadence.

| Phase | Deliverable | Lib LOC | Test LOC |
|---|---|---:|---:|
| 1 | Scaffolding: record types, `parse-pattern`, substitution ops | ~150 | ~80 |
| 2 | `diophantine-basis` (standalone, no match/unify dependency) | ~80 | ~40 |
| 3 | AC-match **without** matrix prune — Eker decomposition + direct backtracking | ~100 | ~90 |
| 4 | Matrix-permanent feasibility prune — Phase 3 tests stay green; pathological-case benchmark | ~30 | ~10 |
| 5 | AC-unify via Stickel + Robinson syntactic fallback | ~120 | ~100 |
| 6 | Integration tests, docstrings for every public export, `(wile algebra)` aggregator re-export | ~30 | ~30 |
| **Total** | | **~510** | **~350** |

Total ~860 LOC. Larger than directions-doc estimate (~400–600) because this plan also publishes `diophantine-basis`.

**Phase-3-before-Phase-4 is intentional.** Ship the correctness-complete matcher first; layer the matrix prune on top. If Phase 4's benchmark shows no measurable win on realistic inputs, the Phase 4 code can be dropped without backtracking — the library works at Phase 3. Goal #4: commit to matrix-backed perf only with evidence.

## Definition of done

- All tests pass
- `make lint && make covercheck` clean
- Every public export has a docstring with **Parameters / Returns / Category / Keywords** (LLM-reliability convention)
- `(wile algebra)` aggregator re-exports `unification`
- Library header comment documents `term-protocol` contract (`term-compare` total-order consistent with AC-equivalence)
- Follow-up plan stub filed as `plans/YYYY-MM-DD-wile-goast-ac-match-migration.md` capturing the three integration risks and the migration-site spec
- No wile-goast repo changes in this plan's commits
- `TODO.md` §5.3 entry marked `[x]` with pointer to this plan's closing commit

## Future extensions (deferred)

- **Sort/type constraints on `<pattern-var>`** — add a `sort` field and design the compatibility-check mechanism. Open questions: sort as symbol, predicate, or term-protocol extension (`term-sort`)? Maude/OBJ precedent favors a lattice of symbol sorts. Deferred until a consumer has concrete typing needs.
- `ac-unify-with-trace` — if a diagnostic consumer emerges
- Small-arity fast path — decision driven by Phase 4 benchmark
- Hashtable backing for `<substitution>` — swap internal representation iff profiling shows `substitution-lookup` hot
- Full E-matching — separate library if a consumer emerges; not currently visible
- E-graph / equality saturation — different algorithmic track
- Library split (`ac-match → rewrite.sld`) — revisit once AC-match has real wile-goast callers and usage patterns are known
