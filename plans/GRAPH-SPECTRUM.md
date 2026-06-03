# GRAPH-SPECTRUM — Spectral graph theory for `(wile algebra)`

**Status:** Design / directions. Not scheduled. No implementation committed.
**Date:** 2026-06-02
**Author seed:** Prototype evidence in this document was produced and verified in a live
Wile session (Jacobi + Faddeev–LeVerrier on C₄, Petersen, path graphs). All numeric
claims below are reproducible — see §5.

---

## 0. What this is

The *spectrum* of a graph is the multiset of eigenvalues of one of its matrices:

| Matrix | Definition | Notes |
|--------|------------|-------|
| Adjacency `A` | `A[i][j] = 1` iff `i~j` | undirected ⇒ symmetric ⇒ real spectrum |
| Laplacian `L = D − A` | `D` = degree diagonal | PSD; smallest eigenvalue 0 |
| Signless Laplacian `Q = D + A` | | bipartiteness detector |
| Normalized `𝓛 = I − D^(−1/2) A D^(−1/2)` | | eigenvalues in [0, 2] |

Pipeline: **graph → matrix → eigenvalues (+ optional eigenvectors) → derived invariants.**

This plan scopes what `(wile algebra)` must add to compute these, grounded against the
current code and against two prior-art decisions that **must not be silently overridden.**

---

## 1. Prior-art reconciliation (read first)

Two existing artifacts already touch this. This plan is a *response* to them, not a
greenfield proposal.

1. **`2026-04-17-algebra-foundations-directions.md` Part 7 / `TODO.md:99` explicitly
   list "spectral graph matching" as a NON-GOAL** — "no prospective consumer; documented
   so the exclusion is visible rather than mistaken for oversight." This plan does **not**
   reverse that. It exists so that *if* a consumer appears (see §6), the path is already
   designed and the pure-Scheme feasibility is already proven.

2. **`2026-04-18-gonum-integration-directions.md` proposes `gonum/mat` (pure Go) as the
   field-valued linear-algebra source** for any spectral work (Laplacian eigenvalues for
   CFG fingerprinting), "gated on a concrete wile-goast consumer." That plan correctly
   states (line 8): *"semirings lack subtraction and division, so eigendecomposition …
   require `gonum/mat` or equivalent, not semiring matrices."*

   **The contribution of this plan is the "or equivalent":** a pure-Scheme field-matrix +
   eigensolver is viable (§5 proves it), giving a build-vs-FFI fork that the gonum plan
   left open. See §4 for the decision.

---

## 2. Current state (file-grounded, verified 2026-06-02)

| Capability | Status | Evidence |
|---|---|---|
| Graph representations | ✅ two | `(wile algebra graph)` semiring/Bellman-Ford; `(wile algebra combinatorial-graph)` with `graph-adjacency`, `graph-vertices` (adjacency-order), `graph-degree`, generators (`complete-graph`, `cycle-graph`, `petersen-graph`, …) |
| Matrix library | ⚠️ **semiring**-parameterized | `matrix.sld`: `matrix-add/mul/power/closure/permanent` over an arbitrary semiring. Extension registry present: `register-matrix-rep!`, `register-matrix-op!`, `matrix-op-supported?` |
| Subtraction / determinant / decomposition / eigen | ❌ absent | `grep -riE 'determinant|gauss|qr|lu|eigen' algebra/` → nothing |
| Field abstraction | ✅ exists, **unused by matrices** | `ring.scm`: `make-field`, `field-negate`, `field-divide`, `field-reciprocal`, `rational-field` |
| Numeric foundation | ✅ complete | complex (`make-rectangular`, `0+1i`), `sqrt -1 ⇒ 0+1i`, `magnitude` all verified |
| Polynomial library | ✅ exists | `(wile algebra polynomial)`: `make-poly`, `poly-derivative`, `poly-eval` |

### The one structural obstacle

The matrix layer is built on **semirings**, which have **no additive inverse**. Eigenvalue
computation is intrinsically a **field** operation: you need subtraction to form `A − λI`
and `D − A`, and division for Gaussian elimination / eigenvector normalization. You cannot
express spectral methods in the current `matrix` abstraction. This is not a missing
function — it is a missing algebraic structure under the matrices.

The codebase already knows this. `graph-spanning-tree-count`
(`combinatorial-graph.scm`) uses deletion-contraction (capped at |E| ≤ 20) and its own
docstring says: *"The Kirchhoff-matrix-tree theorem (via Laplacian minor determinant) is a
v2 opt-in that would lift the cap to polynomial in |V|."* **The matrix-tree theorem and the
graph spectrum are the same missing capability** — field linear algebra.

---

## 3. Prototype evidence (the design rests on this)

Two candidate eigensolver kernels were implemented in pure Scheme and run against graphs
with known spectra. Reproduction recipes in §5.

### Results

| Case | Jacobi (symmetric, QR-family) | Faddeev–LeVerrier + Durand–Kerner |
|---|---|---|
| C₄ adjacency (exact {2,0,0,−2}) | exact ✓ | exact ✓ |
| Petersen (exact {3, 1×5, −2×4}) | exact ✓ | exact ✓ |
| Path P₅₀ (`2cos(kπ/51)`, irrational/clustered) | err ≈ 4.6e−7 (120 sweeps, not fully converged) | err ≈ **4.4e−16** (machine precision) |
| Path P₇₀, high iteration count | fine | **timed out** (complex arithmetic × iterations) |

### What the prototypes actually settled

- **Accuracy is NOT the differentiator.** The textbook instability of Faddeev–LeVerrier did
  *not* reproduce on bounded-degree simple graphs (spectrum ⊂ [−2,2] ⇒ traces of `Aᵏ` stay
  tame). It is real only for large eigenvalue dynamic range (weighted/directed graphs).
  Stating this honestly: the instability hypothesis that motivated the comparison was wrong
  for the graphs you'd actually feed it.

- **Eigenvectors ARE the differentiator.** Jacobi yields eigenvectors for free (accumulate
  the rotations). The char-poly route gives eigenvalues only; the Fiedler vector (spectral
  partitioning, Layer 4) then needs a separate `(A−λI)v=0` nullspace solve per eigenvalue —
  which requires the field-matrix machinery anyway. For anything past bare eigenvalues,
  char-poly is a dead end.

- **Faddeev–LeVerrier needs only RING operations** (traces of powers; no division). Run over
  exact integers/rationals it produces the *exact integer characteristic polynomial*,
  answering exact questions (graph integrality, exact τ(G) via matrix-tree, cospectrality).
  It needs **no field** and reuses `(wile algebra polynomial)`, so it can ship before the
  field-matrix work.

### Conclusion: complementary, not either/or

- **Primary kernel — symmetric Jacobi/QR** over a field-backed dense matrix → robust real
  eigenvalues **and eigenvectors**; covers all undirected graphs and every derived quantity.
  Mirrors LAPACK's `syev`/`geev` split (symmetric vs general); add a QR-on-Hessenberg path
  for directed graphs later. Aligns with the gonum/BLAS-shaped-API preference recorded in
  `feedback-blas-style-dispatch` and `feedback-algebra-design-goals`.
- **Companion — Faddeev–LeVerrier over the exact ring** → exact char-poly on small graphs,
  no field dependency, reuses the polynomial library. Durand–Kerner stays optional (numeric
  roots on demand).

---

## 4. The build-vs-FFI decision (OPEN — owner: user)

This is the one fork that changes everything downstream. It is **not** resolved here.

| Option | What it is | Pros | Cons |
|---|---|---|---|
| **A. Pure-Scheme field-matrix** | New field-backed dense rep via `register-matrix-rep!`; Jacobi + Faddeev–LeVerrier in Scheme | No new dependency (CLAUDE.md: prefer stdlib). Stays inside the algebra library's idiom. Proven feasible (§3). Exact-ring path is a genuine niche gonum can't serve. | O(n³) dense Scheme is slow; Durand–Kerner timed out at n≈70. Not competitive with LAPACK on large graphs. |
| **B. `gonum/mat` FFI extension** | Go extension exposing eigendecomposition, per `2026-04-18-gonum-integration-directions.md` | Fast, robust, battle-tested `syev`/`geev`. Pure Go (no CGo) — satisfies the hard constraint. | New large dependency. Lives in a Go extension (kitchen-sink profile), not pure Scheme. Duplicates the matrix abstraction at the FFI boundary. |
| **C. Both, layered** | Pure-Scheme exact-ring char-poly (small/exact) + gonum FFI numeric kernel (large/fast) | Each serves its niche; exact invariants in Scheme, scale via gonum | Most surface area; two code paths to keep coherent |

**Recommendation if a consumer materializes:** start with **A's exact-ring char-poly**
(ships without the field-matrix work, retires the matrix-tree TODO, zero new deps), and only
add **B** when a *large-graph numeric* consumer is real. Do not build the pure-Scheme numeric
Jacobi kernel for production scale — the prototype proves correctness, not performance.

**Blocking question for the user:** is there a concrete consumer (CFG spectral fingerprinting
in wile-goast? spectral clustering?) or is this still pre-consumer R&D? The non-goal status
(§1.1) says pre-consumer. If so, this plan stays as directions and **only §7 (exact-ring
char-poly) is worth shipping opportunistically**, because it has standalone value
(matrix-tree theorem) independent of spectra.

---

## 5. Reproduction (prototype recipes)

Run in a Wile session / `mcp__wile__eval`. Flat-vector dense matrices, `mref/mset!` helpers.

- **Adjacency builder:** `(adj n edges)` → symmetric flat vector.
  - C₄: `(adj 4 '((0 1)(1 2)(2 3)(3 0)))`
  - Petersen: outer pentagon `(0 1)(1 2)(2 3)(3 4)(4 0)`, spokes `(i i+5)`, inner pentagram
    `(5 7)(7 9)(9 6)(6 8)(8 5)`.
- **Jacobi:** cyclic sweep; per (p,q) compute `phi=(aqq−app)/(2·apq)`,
  `t=sign(phi)/(|phi|+√(phi²+1))`, `c=1/√(t²+1)`, `s=t·c`; rotate rows/cols; zero `apq`.
  Eigenvalues = diagonal after ~30–120 sweeps.
- **Faddeev–LeVerrier:** `M₁=A`; `c_{n-k} = −tr(M_k)/k`; `M_{k+1}=A·(M_k + c_{n-k}·I)`.
  Yields monic char-poly coeffs. C₄ ⇒ `(0 0 −4 0 1)` = `x⁴−4x²` ✓.
- **Durand–Kerner:** seed `z_i=(0.4+0.9i)^i`; iterate
  `z_i ← z_i − p(z_i)/∏_{j≠i}(z_i−z_j)`; take real parts for symmetric input.

> Gotchas hit during prototyping (record for next time):
> - Wile `sort` is `(sort less? list)` — predicate first, not list first.
> - No `list-head`; write a local `take-n`.
> - A compile error in an eval block rejects **all** defines in that block — redefine
>   dependencies together.

---

## 6. Layered change plan (if/when a consumer is approved)

| Layer | Change | Depends on | Ship independently? |
|---|---|---|---|
| **1a** | Faddeev–LeVerrier char-poly over the existing **ring** (`integer-ring`/`rational-field`) | nothing new | **Yes** — standalone value: exact τ(G), integrality, retires matrix-tree TODO |
| **1b** | **Field-backed dense matrix** rep via `register-matrix-rep!` + subtract / scalar-mul / determinant / nullspace | `make-field` (exists) | The real structural work; unblocks all numerics |
| **2** | `graph->adjacency-matrix`, `graph->laplacian-matrix` (+ signless, normalized); guard tier-2 infinite/generator graphs (spectrum defined only for finite graphs) | 1b (needs `−` for `D−A`) | with 1b |
| **3** | Jacobi (symmetric) eigensolver → eigenvalues **+ eigenvectors**; QR-on-Hessenberg for directed later. OR gonum FFI per §4-B | 1b (or B) | with 1b/B |
| **4** | `graph-spectrum`, `graph-laplacian-spectrum`, Fiedler value/vector, spectral gap, connected-component count (= mult. of eigenvalue 0 in `L`), spanning-tree count (= ∏ nonzero Laplacian eigenvalues / n), spectral radius | 2,3 | thin wrappers |

### Correctness invariants to encode as tests
- Undirected ⇒ symmetric matrix ⇒ all-real spectrum (assert imaginary parts ≈ 0).
- Directed ⇒ general matrix ⇒ complex spectrum allowed (result type already supported).
- `L` is PSD; smallest Laplacian eigenvalue is exactly 0; its multiplicity = #components.
- Canaries from existing combinatorial-graph fixtures: C₆ vs 2K₃ are **cospectral but
  non-isomorphic** — spectrum alone must not be claimed as an isomorphism certificate.
- Petersen adjacency spectrum = {3, 1×5, −2×4}; Laplacian = {0, 2×5, 5×4}.

---

## 7. Minimum shippable slice (lowest-risk, consumer-independent)

If anything from this plan is built opportunistically, build **Layer 1a only**:
Faddeev–LeVerrier over `integer-ring` producing the exact characteristic polynomial, plus
`graph-characteristic-polynomial` and the matrix-tree spanning-tree count it unlocks. This:

- needs **no field-matrix, no eigensolver, no new dependency**;
- reuses `(wile algebra polynomial)` and the existing semiring `matrix-mul`;
- retires the deferred matrix-tree TODO in `graph-spanning-tree-count` (lifts the |E|≤20 cap
  to polynomial-in-|V| for the determinant route);
- gives exact spectral *invariants* (integrality, cospectrality witness) without ever
  computing a floating-point eigenvalue.

Everything past Layer 1a waits on a real consumer and the §4 build-vs-FFI decision.
