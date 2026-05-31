# SAT Solver — Design

**Status**: Design draft. Implementation plan to follow in a separate `-impl.md` file.
**Date**: 2026-05-30.
**Related work**: `(wile algebra symbolic)` (`stdlib/lib/wile/algebra/symbolic.scm:670-734`) — the existing axiomatic primitives this work complements; `(wile algebra boolean)` (`stdlib/lib/wile/algebra/boolean.scm`) — the *Boolean-algebra-as-structure* library (distinct concept; not touched by this work); `extensions/algebragraph/` — precedent for Go-kernel + Scheme-frontend extension layout.

---

## 1. Motivation

`(wile algebra symbolic)` exposes `symbolic-boolean-normalize` and `symbolic-boolean-equivalent?` over S-expression boolean formulas (`and`, `or`, `not` over atom variables). These are *axiomatic* rewrites driven by the theory `boolean->theory` produces: commutativity, associativity, identity, idempotence, absorption of join/meet, complement-involution. The library's own docstrings (`symbolic.scm:679-682`, `:715-717`) call out the gap explicitly:

> **Not applied under the current theory:** De Morgan, complement laws (`x ∧ ¬x = ⊥`), bound identities (`x ∨ ⊤ = ⊤`, `x ∧ ⊥ = ⊥`).
>
> "Pairs differing only by De Morgan or complement laws will return `#f`."

As a result, `symbolic-boolean-equivalent?` is *sound but incomplete*: every `#t` it returns is genuine, but `#f` can be a false negative. Concrete examples:

```scheme
(symbolic-boolean-equivalent? '(not (and x y)) '(or (not x) (not y)))  ; => #f (false neg)
(symbolic-boolean-equivalent? '(and x (not x)) '#f)                    ; => #f (false neg)
(symbolic-boolean-equivalent? '(and x (or y z))
                              '(or (and x y) (and x z)))                ; => #f (false neg)
```

A complete decision procedure for propositional satisfiability closes this gap: equivalence under classical interpretation reduces to UNSAT-checking the negated biconditional `¬(a ↔ b)`. Users also gain a general-purpose primitive for encoding their own problems (puzzles, configuration, planning) into CNF and getting a model back.

**Crucially, this does not modify `symbolic-boolean-equivalent?`.** That primitive's axiomatic semantics are fast, predictable, and sound; changing them would alter a published contract. Instead, this design ships parallel SAT-backed primitives that callers opt into when they want completeness (see §6). The two compose naturally:

```scheme
(define (cheap-or-complete-equivalent? a b)
  (if (symbolic-boolean-equivalent? a b) #t       ; cheap, sound — done if #t
      (boolean-decide-equivalent? a b)))          ; SAT-backed fallback on #f
```

The `type-constraint-extension-design` plan (2026-04-21 §2.1) explicitly rejected SMT-style solvers as a refinement-type backend ("heavy + incomplete"). That decision stands. This SAT solver is not a type-checker backend; it is a complete decision procedure for propositional logic, narrower than SMT and broader than the current axiomatic check.

The `type-constraint-extension-design` plan (2026-04-21 §2.1) explicitly rejected SMT-style solvers as a refinement-type backend ("heavy + incomplete"). That decision stands. This SAT solver is not a type-checker backend; it is a complete decision procedure for propositional logic, narrower than SMT and broader than the current axiomatic check.

---

## 2. Scope and non-goals

**In scope (v1):**

- A CDCL (conflict-driven clause learning) solver in Go, MiniSat-class.
- Watched-literal unit propagation, 1-UIP conflict analysis, clause learning, VSIDS branching, Luby-sequence restarts, activity-based clause-database deletion.
- Scheme front-end with Tseitin transform (formula → CNF) and two primitives: `sat?` (arbitrary formula) and `sat-cnf?` (raw CNF).
- Model retrieval on SAT via a separate primitive (`sat-model` / `sat-cnf-model`).
- Three-valued return: `#t` (SAT), `#f` (UNSAT), `'unknown` (budget exhausted or ctx cancelled).
- Conflict-count budget parameter; honors engine `ctx.Done()` at restart boundaries.
- Boolean-algebra integration via two new primitives — `boolean-decide-sat?`, `boolean-decide-equivalent?` — without changing the semantics of the existing axiomatic primitives.

**Out of scope (deferred to v2 or later):**

- LBD-based clause quality metric (Glucose-style).
- Pre-processing (variable elimination, blocked-clause elimination, inprocessing).
- UNSAT cores, proof logging, MUS extraction.
- Incremental solving with assumptions.
- Parallel portfolio solving.
- DIMACS file I/O.
- All-models enumeration (`#SAT`).

**Not on the roadmap:**

- SMT (theories over arithmetic, arrays, etc.).
- MaxSAT / weighted clauses.
- QBF.

---

## 3. Architecture

Two physical layers, mirroring the `algebragraph` precedent:

```
extensions/sat/
  doc.go              package-level description
  register.go         primitive registration (PhaseSetRuntime)
  cnf.go              CNF representation; Scheme↔Go conversion
  solver.go           CDCL solver: trail, watched literals, clause DB, VSIDS, restarts
  solver_test.go      unit + property tests
  solver_bench_test.go performance regression guards
  prim_sat.go         (sat-cnf?) and (sat-cnf-model) Go primitives
  BENCH.md            baseline benchmark numbers

stdlib/lib/wile/algebra/
  sat.sld             library declaration: (wile algebra sat)
  sat.scm             Scheme front-end:
                        - Tseitin transform (formula → CNF)
                        - sat?, sat-cnf?, sat-model, sat-cnf-model
                        - boolean-decide-sat?, boolean-decide-equivalent?
  sat-test.scm        Scheme-level tests
```

**Dependency direction:** `extensions/sat/` knows nothing about `(wile algebra boolean)`'s formula AST. The Scheme layer translates symbolic ASTs to CNF before crossing the FFI boundary. Single direction of knowledge.

**Authorization:** SAT is pure compute; no privileged operations. No gate sites needed.

---

## 4. Scheme API

```scheme
;; (wile algebra sat) exports:

(sat? formula [#:conflict-budget N])
  ;; formula: boolean expression built from #t, #f, symbols (as variables),
  ;;   and (and ...), (or ...), (not e), (xor a b), (iff a b), (=> a b).
  ;; Returns:
  ;;   #t           — satisfiable; model stashed (retrieve with sat-model)
  ;;   #f           — unsatisfiable (proven)
  ;;   'unknown     — conflict budget exhausted or ctx cancelled
  ;; conflict-budget: default 1_000_000. Pass #f for unbounded.

(sat-model)
  ;; After (sat? expr) returns #t, returns an alist ((sym . #t/#f) ...)
  ;; over the variables that appeared in the most recent SAT call.
  ;; Returns #f if the most recent call returned #f or 'unknown.

(sat-cnf? clauses [#:conflict-budget N])
  ;; clauses: list of clauses; each clause is a list of literals.
  ;; A literal is an exact integer: positive = var, negative = ¬var, never 0.
  ;; Variables numbered 1..N (DIMACS convention). N inferred from max(|lit|).
  ;; Same three-valued return as sat?.

(sat-cnf-model)
  ;; Vector indexed 1..N of #t/#f, or #f if no current model.

(boolean-decide-sat? formula)
  ;; Thin wrapper around sat?; no budget knob; returns #t/#f/'unknown.

(boolean-decide-equivalent? a b)
  ;; (sat? '(not (iff ,a ,b))) → if UNSAT, returns #t; if SAT, #f; else 'unknown.
```

### 4.1 Three load-bearing API choices

**(a) Three-valued return.** A SAT primitive that returns only `#t`/`#f` forces callers to either trust completeness (unsafe — SAT is NP-hard) or wrap every call in a timeout (annoying). `'unknown` as a symbol — *not* a boolean — makes incompleteness explicit at the API. `(if (sat? f) ...)` would treat `'unknown` as truthy; that is wrong, and the type difference makes the mistake loud at the call site.

**(b) Model retrieval is a separate call.** Keeps the common decision-only path allocation-free. Stateful: `sat-model` returns the model from the *most recent* `sat?` or `sat-cnf?` call. Two consecutive calls overwrite the buffer. Documented prominently.

**(c) No `N` (variable count) parameter on `sat-cnf?`.** `N` is inferred from `max(|lit|)` over all clauses. One fewer parameter; one fewer way for caller and callee to disagree.

### 4.2 Boolean-algebra integration

`symbolic-boolean-equivalent?` keeps its axiom-only semantics (fast, predictable, no solver dependency). The new `boolean-decide-equivalent?` is the SAT-backed complete check. Users choose: cheap heuristic versus complete. Two reasons for the split:

1. The axiomatic check is fast (polynomial, often nearly linear) and predictable. SAT-backed checks are NP-hard in the worst case.
2. Changing the semantics of a published primitive is risky; adding a new one is safe.

### 4.3 Implementation of `boolean-decide-equivalent?`

```scheme
(define (boolean-decide-equivalent? a b)
  ;; a ≡ b  iff  (a ↔ b) is a tautology  iff  ¬(a ↔ b) is UNSAT
  (let ((result (sat? `(not (iff ,a ,b)))))
    (cond
      ((eq? result #f) #t)          ; UNSAT proven — a and b are equivalent
      ((eq? result #t) #f)          ; SAT found — model is a counterexample
      (else 'unknown))))            ; budget exhausted — incomplete answer
```

Three observations:

1. **Counterexamples are free.** If the result is `#f`, `(sat-model)` holds the assignment that distinguishes `a` from `b`. We could expose a `boolean-decide-counterexample` primitive that surfaces it; cheap to add later.
2. **`iff` is part of the Tseitin transform**, not user syntax sugar. The Tseitin pass in `sat.scm` recognizes `iff` and emits the equivalent CNF clauses directly rather than expanding to `(and (=> a b) (=> b a))`. Smaller CNF.
3. **`'unknown` propagates cleanly.** A budget exhaustion on equivalence-checking is "I tried but couldn't prove it either way" — exactly what `'unknown` means.

### 4.4 Composition with the axiomatic checker

Three usage patterns the design supports:

**(a) Fast-path-then-complete.** Because `symbolic-boolean-equivalent?` is sound (every `#t` is genuine — only false negatives, never false positives), `#t` from the cheap check is conclusive. Only `#f` needs the expensive verification.

```scheme
(define (cheap-or-complete-equivalent? a b)
  (if (symbolic-boolean-equivalent? a b) #t
      (boolean-decide-equivalent? a b)))
```

**(b) Normalize-first-then-decide.** The axiomatic normalizer often shrinks the formula before SAT sees it.

```scheme
(define (decide-after-normalize a b)
  (let-values (((na _ta) (symbolic-boolean-normalize a))
               ((nb _tb) (symbolic-boolean-normalize b)))
    (boolean-decide-equivalent? na nb)))
```

**(c) Direct SAT for non-algebraic problems** — puzzles, configuration, planning — that don't go through the algebra layer at all.

```scheme
;; "Is this 2-node graph 3-colorable?" — vars name (node, color) facts.
(sat? '(and (or n1-red n1-blue n1-green)
            (or n2-red n2-blue n2-green)
            (not (and n1-red n2-red))
            (not (and n1-blue n2-blue))
            (not (and n1-green n2-green))))
;; => #t, with witness in (sat-model)
```

---

## 5. CDCL solver internals (Go kernel)

### 5.1 Core data structures

```go
type literal int32   // 2*var + sign; var = lit>>1, sign = lit&1, ¬lit = lit^1
type clauseRef int32 // index into clause arena; -1 = none

type clause struct {
    learnt   bool
    activity float32
    lits     []literal // first two are the watched literals
}

type solver struct {
    // Assignment + trail
    assigns  []int8       // var → 0=unassigned, 1=true, -1=false
    level    []int32      // var → decision level it was set at
    reason   []clauseRef  // var → antecedent clause (-1 if decision)
    trail    []literal    // assignments in order
    trailLim []int32      // start index in trail of each decision level

    // Clause database
    clauses []clause     // both originals and learnt
    watches [][]clauseRef // 2*N entries, one per literal

    // VSIDS branching
    activity      []float32
    order         varHeap   // max-heap keyed by activity
    activityDecay float32

    // Restart / clause-deletion policy
    conflicts      int64
    conflictBudget int64    // -1 = unlimited
    nextRestart    int64
    learntLimit    int

    ctx context.Context
}
```

### 5.2 Algorithm

Three loops, all standard CDCL:

**(a) Unit propagation (`propagate`).** Pop literal from trail, scan its watch list. For each watched clause: try to find a new watch among non-falsified non-watched literals. If no new watch and the other watched literal is falsified, return the conflict clause. If unassigned, enqueue as unit. Returns `noRef` if no conflict.

**(b) Conflict analysis (`analyze`).** Walk the implication graph backwards from the conflict, resolving against reason clauses until we have a 1-UIP (first-unique-implication-point) clause. Computes the backtrack level. Adds the learnt clause; bumps activity of seen variables and the learnt clause itself.

**(c) Search (`search`).** Main loop:
- if `propagate` returns a conflict at decision level 0 → return UNSAT;
- if `propagate` returns a conflict at level > 0 → analyze, backjump, learn, continue;
- if no conflict and trail covers all vars → return SAT;
- if `conflicts >= nextRestart` → backjump to 0, advance Luby sequence;
- if `conflicts >= conflictBudget` → return `unknown`;
- if `ctx.Done()` → return `unknown`;
- otherwise: pick highest-activity unassigned variable, branch on preferred polarity, push decision.

### 5.3 Policies

**Restart:** Luby sequence × unit (e.g., 100). Standard, easy, works.

**Clause deletion:** When learnt count exceeds `learntLimit`, halve by activity, then double `learntLimit`. MiniSat-style.

**Ctx check cadence:** Only at restart boundaries — not per-conflict, not per-propagation. Restarts are frequent enough for responsive cancellation and rare enough to keep the inner loop clean.

**Determinism:** Same CNF + same conflict budget → same result and same model bits. The solver carries a seedable RNG; default seed is fixed. Random tie-breaking is a portability landmine.

### 5.4 Size estimate

~1500–2000 lines of Go including comments and tests. Comparable to MiniSat's core (~2500 LOC of C++).

---

## 6. CNF representation across the FFI

**User-facing shape** (what `sat-cnf?` accepts): a list of clauses, each clause a list of literals — `'((1 -2 3) (-1 4) (2 -3 -4))`. Ergonomic for hand-written CNF and matches what `(wile algebra ...)` libraries elsewhere expect.

**Wire shape** (what crosses the FFI to the Go primitive): a single vector of exact integers with `0` as clause terminator — `#(1 -2 3 0 -1 4 0 2 -3 -4 0)`.

**Where the conversion happens.** `sat.scm` exposes a helper `(cnf->flat clauses) → vector` that walks the list-of-lists once and produces the flat vector. The user-facing `sat-cnf?` calls it before invoking the Go primitive `sat-cnf-flat?` (registered in `extensions/sat/`). Users who already have the flat vector form can call `sat-cnf-flat?` directly to skip the translation pass; we document this but `sat-cnf?` is the recommended entry.

**Why the flat shape across the FFI:**

1. **One allocation, not N.** A 10k-clause formula crosses as one `*values.Vector` carrying a `[]int64`, not 10k `*values.Pair` cells.
2. **Linear scan matches what the solver wants.** Watched-literal construction walks every literal once.

**Internal representation is the same CNF, bit-packed.** `parseCNF` in `cnf.go` translates each `0`-delimited group into a `clause{lits: []literal}`, with the literal packing `int64 v → literal int32` defined as `2*|v| + (v < 0 ? 1 : 0)`. No semantic transform; only a tighter encoding for cache density and fast negation (`¬lit = lit ^ 1`).

**Why CNF and not AIG / BDD / formula tree:** Not a stylistic choice. CDCL's algorithms — watched-literal propagation, resolution-based conflict analysis, clause learning — are defined over clauses. AIG and formula tree both need to *become* CNF before they can be solved. BDDs are a different algorithmic family and blow up exponentially on industrial SAT. CNF is what CDCL consumes; the wire format and internal form are the same thing in two transport packages.

**Variable numbering contract:** DIMACS — variables are positive integers ≥ 1. `N` inferred from `max(|lit|)` over all clauses.

---

## 7. Error model and return shape

### 7.1 Values, not exceptions

`'unknown` is a *value*, not an error. Budget exhaustion and `ctx.Done()` both return `'unknown`. Callers can re-call with a larger budget or accept the incomplete answer.

If `'unknown` raised an exception instead, every caller would need a `(with-exception-handler ...)` wrapper just to do a probabilistic check. Bad ergonomics for a primitive that is likely to be called inside `(if (sat? ...) ...)` loops.

### 7.2 Errors that raise

Via `werr.WrapForeignErrorf(sentinel, "site: what failed")`. No `fmt.Errorf` anywhere (per ruleguard `noFmtErrorf`).

| Failure | Sentinel |
|---|---|
| Clause contains `0` mid-clause | `werr.ErrInvalidArgument` |
| Literal is non-integer | `werr.ErrTypeMismatch` |
| Variable index overflows `int32` | `werr.ErrInvalidArgument` |
| Empty clause in input (literal `0` with no preceding lits) | `werr.ErrInvalidArgument` — **not** UNSAT |
| `sat-model` called when no current model exists | `werr.ErrInvalidArgument` |

The "empty clause = error, not UNSAT" choice is deliberate: an empty input clause `()` is almost always a Scheme-side bug. Silently returning `#f` would mask it. A formula with *zero clauses* (the trivial `#t`) is a different case — that returns SAT with the empty model.

### 7.3 Model staleness

`sat-model` / `sat-cnf-model` return the model from the most recent `sat?` / `sat-cnf?` call on the same Engine. Two consecutive calls overwrite the buffer. The model lives on the engine's namespace, not in a global.

For multi-threaded use within a single Engine: a thread that calls `sat?` and then `sat-model` without yielding sees its own result. Otherwise, the contract is "you raced, that's on you." SAT calls themselves are atomic from the VM's perspective.

---

## 8. Testing strategy

### 8.1 Go-level unit tests (`solver_test.go`, `cnf_test.go`)

- **Canonical formulas as ground truth.** `pigeonhole-3-2` (UNSAT), `php-3-3` (SAT), parity xor chains (UNSAT), `(a ∨ b) ∧ (¬a ∨ ¬b)` (SAT, two models).
- **Table-driven** per `registry/CLAUDE.md` convention: one slice of `{name, clauses, expected, expectedModelHash}`.
- **Watched-literal invariant** as a property test: after `propagate` returns, every non-conflict clause has at least one non-falsified watch, OR the clause is unit and the second watch is the implied literal. Small randomized inputs.
- **1-UIP property:** every learnt clause contains exactly one literal at the current decision level. Asserted via a debug flag.
- **Determinism:** same CNF + same budget → same result and same model bits.

### 8.2 Scheme-level integration tests (`sat-test.scm`)

- **Tseitin round-trip** for hand-derived SAT/UNSAT formulas.
- **`boolean-decide-equivalent?` vs. `symbolic-boolean-equivalent?` agreement:** every formula pair the axiomatic check says `#t` *must* also be `#t` under the SAT-backed check (axiomatic is sound). Cases where axiomatic says `#f` but SAT-backed says `#t` are the interesting ones — that gap is the new primitive's purpose.
- **Three-valued discipline:** `(sat? hard-formula #:conflict-budget 10)` returns `'unknown`.
- **Ctx cancellation:** spawn a thread, cancel engine ctx mid-solve, assert `'unknown` returned within one Luby period.

### 8.3 Benchmark guards (`solver_bench_test.go`)

- `BenchmarkPHP-N` for `N ∈ {4,5,6,7,8}` (pigeonhole UNSAT scaling).
- `BenchmarkRandom3SAT-100` at clause-density ratio 4.26 (phase transition).
- Baselines in `extensions/sat/BENCH.md`. Regression > 20% on these flags a code-level perf regression in review (same convention `algebragraph` uses).

### 8.4 Property tests (`solver_test.go`)

- Generate random CNF at varying densities; solve; verify the model against the input clauses on SAT. O(clauses × literals) verification catches "solver said SAT but model doesn't satisfy" — a class no unit test would find.

### 8.5 Coverage

`make covercheck` per project convention. One tricky path needs targeted coverage: conflict-at-level-0 from unit propagation of level-0 facts. Random inputs rarely exercise it.

---

## 9. Risks and trade-offs

**Risk: `'unknown` ergonomics.** A three-valued return is unusual in Scheme APIs and may surprise users who expect `#t`/`#f`. *Mitigation*: prominent documentation; `boolean-decide-equivalent?` example showing the explicit `(eq? r #t)` check; clear error messages on misuse if we detect `(if (sat? ...) ...)` patterns in linting (out of scope for v1).

**Risk: Tseitin growth.** Tseitin produces O(formula size) new variables; for nested xor/iff chains the constant factor can be 3–5×. *Mitigation*: documented in `sat.scm`; users with already-CNF input use `sat-cnf?` and pay nothing.

**Risk: Budget tuning.** Default conflict budget of 1M is arbitrary. *Mitigation*: easy to tune; the parameter is exposed; future work could auto-scale based on variable count.

**Risk: Non-determinism on platforms.** Floating-point activity decay can produce different VSIDS orderings across platforms. *Mitigation*: use `float32` everywhere; document that "deterministic" means same-platform-same-build; CI runs on a fixed Go version and OS.

**Trade-off: Watched literals vs. counter-based propagation.** Watched literals win on industrial formulas (sparse clauses, many learned clauses) but lose on small dense formulas. We pay this cost for scalability; the alternative scales worse.

---

## 10. Future work hooks

The architecture leaves clean extension points for v2:

- **Pre-processing** would slot between `parseCNF` and `solver.solve` entirely in Go. The Scheme front-end never sees it.
- **UNSAT cores** add a proof-logging mode flag on the solver and a `sat-unsat-core` Scheme primitive. Resolution chains for learnt clauses are computed in `analyze`; we just need to record them.
- **Incremental solving** moves the solver state from per-call to a `*values.OpaqueValue` handle; the existing primitives keep their shape, plus new `(sat-incremental)`, `(sat-assume)`, `(sat-solve)`, `(sat-finalize)`.
- **DIMACS I/O** is ~50 lines: a Scheme reader producing the same flat-int format `sat-cnf?` already accepts.

None of these require changes to the v1 surface.

---

## 11. Open questions

None at design-finalize time. All architectural choices have been made; the implementation plan can proceed.
