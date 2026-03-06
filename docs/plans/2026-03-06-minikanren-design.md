# miniKanren for Wile — Design Document

## Goal

Port miniKanren to Wile as a pure Scheme R7RS library. Primary purpose: conformance stress test for closures, streams, macros, and the library system. Secondary: a usable logic programming library for Wile users.

## Approach

Pure Scheme, no Go code. Two layered libraries, incremental delivery.

## Library Structure

```
lib/wile/microkanren.sld    →  (wile microkanren)
lib/wile/kanren.sld          →  (wile kanren)
```

### (wile microkanren) — Core

The ~40-line microKanren core from Hemann & Friedman (2013). Procedural interface only, no macros.

**Exports:** `var`, `var?`, `var=?`, `walk`, `ext-s`, `unify`, `==`, `call/fresh`, `disj`, `conj`, `mplus`, `bind`, `unit`, `mzero`

**Data structures:**
- Logic variable: `(vector c)` where `c` is an integer counter
- Substitution: association list of `(var . value)` pairs
- State: pair `(substitution . counter)`
- Stream: empty list (failure), pair (answer . rest), or zero-argument procedure (suspension)

**R7RS adaptation:** The canonical implementation uses R6RS `assp`. Replace with R7RS `assoc` using a custom comparator:
```scheme
(assoc u s (lambda (v) (var=? u v)))
```

### (wile kanren) — Macro Sugar

Imports and re-exports `(wile microkanren)`, adds syntactic forms via `syntax-rules`:

- `fresh` — introduces logic variables: `(fresh (x y) goal ...)`
- `conde` — disjunctive clauses: `(conde ((g1 g2 ...)) ...)`
- `run` — bounded result collection: `(run n (x) goal ...)`
- `run*` — unbounded result collection: `(run* (x) goal ...)`

Also provides `reify` for converting raw substitutions to readable output.

## Test Strategy

Integration tests in `integration/kanren/`:

### Phase 1: microKanren core
- Basic unification (`==` on atoms, pairs, variables)
- Variable creation and walking (including chains)
- Disjunction and conjunction
- Stream operations (finite and lazy/suspended)
- Triangular substitutions

### Phase 2: miniKanren macros
- `fresh` with multiple variables
- `conde` with multiple clauses
- `run` / `run*` collecting results
- Classic relations: `appendo`, `membero`

### Conformance stress tests
- Diverging goals (interleaving fairness)
- Deep unification (nested structures)
- Occur check behavior (microKanren omits it — documented)

## Wile Features Exercised

| miniKanren Feature | Wile Feature |
|--------------------|-------------|
| Closures as goals | First-class closures |
| Procedures as streams | Procedure-as-data pattern |
| `assoc` with custom predicate | R7RS variadic `assoc` |
| Recursive `walk`/`unify` | Tail call optimization, deep recursion |
| `syntax-rules` for `fresh`/`conde` | Hygienic macro system |
| Library imports | R7RS library system |
| Quasiquote in `ext-s` | Quasiquote/unquote |

## References

- Hemann, J. and Friedman, D.P. (2013). "microKanren: A Minimal Functional Core for Relational Programming." Workshop on Scheme and Functional Programming.
- Byrd, W.E. et al. (2017). "A Unified Approach to Solving Seven Programming Problems." ICFP.
- https://minikanren.org/
- https://github.com/jasonhemann/microKanren
