# `(wile algebra matching)` — Many-to-many extension (Kelso-Crawford)

**Status:** Stub — gated on `(wile algebra matroid)` (§5.7 Tier C, not shipped).
**Predecessor:** `plans/2026-05-02-algebra-matching-design.md`, `plans/2026-05-02-algebra-matching-impl.md` (shipped 2026-05-02 covering one-to-one and many-to-one).
**Roadmap entry:** `plans/2026-04-17-algebra-foundations-directions.md` §4.6, §5.7.

## Scope

Extend `(wile algebra matching)` with **many-to-many stable matching** under the Kelso-Crawford (1982) substitutes condition. Adds a `many-to-many-match` export and possibly a richer profile type to encode workers' set-valued preferences.

## Why deferred

The substitutes condition states that an agent's demand for any worker (or job) does not increase when other workers (or jobs) are removed from the available set. Algorithmically this is a matroid-intersection property: the set of feasible matchings forms the intersection of two matroids (one per side), and stable matchings correspond to particular vertices of the matroid intersection polytope.

Without `(wile algebra matroid)` (~300 LOC, Tier C of the algebra-foundations roadmap), the v1 implementation would either:

1. Re-implement matroid intersection inline — duplicating work that the matroid library will deliver
2. Hand-encode substitutability via case analysis — incorrect for general substitutes preference structures

Both paths regress the algebra library's coherence (Part 8 principle 3 of the directions doc). The clean dependency direction is `matching → matroid`, so the matroid library must ship first.

## When to revive

When BOTH conditions are met:

1. `(wile algebra matroid)` ships (Tier C of the algebra-foundations roadmap) with at least:
   - `<matroid>` record type
   - Independence-oracle protocol
   - Matroid intersection algorithm
2. A workspace consumer needs many-to-many matching (e.g., a wile-goast scheduling pass that wants to assign multiple AST transformations to multiple test fixtures with substitutability constraints).

Until then, this stub serves as:
- A reminder of the planned API extension
- Documentation of the dependency direction (`matching → matroid`)
- A pointer for the next contributor to find when they search for "Kelso-Crawford" or "many-to-many"

## Sketch of the v2 work

When revived:

1. Design questions to answer:
   - Q1: Does the workers' side's set-valued preference get its own record type, or extend `<preference-profile>`?
   - Q2: Does the existing `<bipartite-matching>` generalize to a `<many-to-many-matching>`, or is a fresh record needed?
   - Q3: Does Kelso-Crawford's salary-adjustment-process formulation need a separate API, or is the substitutes-only version enough for v1?
2. Implementation sketch:
   - Workers propose to firms in their preferred bundle.
   - Firms accept the maximum-utility subset under their substitutes-restricted choice function.
   - Iterate until stable (no firm wants to swap a worker for another available worker).
3. Test fixture: Roth-Sotomayor §6 worked example; consumers' subset-matching with `boolean-semiring-matching` analog.

Estimated scope: ~150-200 LOC for the algorithm, ~80 test LOC, plus the Kelso-Crawford salary-process layer if Q3 chooses to include it.

## References

- Kelso, A. S. & Crawford, V. P. (1982). "Job Matching, Coalition Formation, and Gross Substitutes." *Econometrica* 50(6).
- Roth, A. E. & Sotomayor, M. A. O. (1990). *Two-Sided Matching*. Cambridge University Press, §6.
- Hatfield, J. W. & Milgrom, P. R. (2005). "Matching with Contracts." *American Economic Review* 95(4). (Lattice generalization beyond Kelso-Crawford.)
