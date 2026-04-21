+++
title = "Implementation plan — (wile algebra matrix)"
date  = "2026-04-20"
status = "Active"
parent = "2026-04-17-algebra-foundations-directions.md §5.1"
+++

# Implementation Plan — `(wile algebra matrix)`

Semiring-parameterized matrix algebra. Ships the "matrix layer" already
flagged as a blocker by `2026-04-16-recurrence-categories-design.md:5`
(matrix_ops benchmark) and called out as §5.1 / highest leverage in
`2026-04-17-algebra-foundations-directions.md`.

## Scope (ship)

1. `stdlib/lib/wile/algebra/matrix.sld` + `matrix.scm` — new library.
2. `test/wile/algebra-matrix-test.scm` — full test suite.
3. Update umbrella `stdlib/lib/wile/algebra.sld` to re-export.

Explicitly *not* in scope (future work):

- Hungarian O(n³) tropical assignment (algorithm-specific, not semiring-level).
- Karatsuba / Strassen matrix multiplication (no benchmark justifies it yet).
- Ryser's formula for permanent (requires subtraction — impossible over a
  general semiring; see §Rationale).

## Exports

Per §5.1's proposal, plus accessors the rest of the library idiom requires:

    ;; Construction / predicate
    make-semiring-matrix semiring-matrix?
    semiring-matrix-from-rows semiring-matrix->rows
    semiring-matrix-identity
    ;; Accessors
    semiring-matrix-ref semiring-matrix-shape
    semiring-matrix-rows semiring-matrix-cols
    semiring-matrix-semiring
    ;; Operations
    semiring-matrix-add semiring-matrix-mul
    semiring-matrix-power semiring-matrix-closure
    semiring-matrix-permanent
    ;; Sparse representation
    make-sparse-semiring-matrix sparse-semiring-matrix?
    sparse-semiring-matrix-ref
    semiring-matrix->sparse sparse->semiring-matrix
    ;; Destructuring
    with-semiring-matrix

## Representation

**Dense (primary):** `<semiring-matrix>` record with fields
`(semiring rows cols data)` where `data` is a row-major Scheme vector of
length `rows × cols`. Indexing: `(+ (* r cols) c)`. Chosen over
vector-of-vectors because flat vectors give us cache-friendly iteration and
simpler shape arithmetic.

**Sparse:** `<sparse-semiring-matrix>` with alist entries
`((row . col) . value)` storing only non-zero values. Non-present entries
read as `(semiring-zero S)`. Alist (not hashtable) matches the idiom already
established by `graph.scm` adjacency lists — we can switch later if
benchmarks justify.

## Algorithmic choices

| Op | Cost | Notes |
|----|------|-------|
| add | O(rows·cols) | Elementwise `semiring-plus`. Shape + semiring must match. |
| mul | O(n³) schoolbook | `C[i,k] = Σⱼ A[i,j] ⊗ B[j,k]`. `A.cols = B.rows` required. |
| power | O(log k · n³) via repeated squaring | `M^0 = I`, `M^1 = M`, else split. |
| closure | Iterative `T ← I + M·T` until `equal?` fixpoint | Caller-provided max-iterations; default `max(rows, cols)`. Document divergence on counting semiring with cycles. |
| permanent | O(n! · n) permutation enumeration | Only option over a general semiring (see Rationale). |

### Rationale — why not Ryser for permanent

Ryser's formula is `perm(A) = (-1)ⁿ Σ_{S⊆{1..n}} (-1)^|S| Πᵢ (Σ_{j∈S} A[i,j])`
and runs in O(n·2ⁿ). It **requires additive inverses**. In the boolean
semiring `(∨, ∧, #f, #t)` there is no `-1`. Same for `(min, +, ∞, 0)`
(tropical) and `(max, +, -∞, 0)`. Permutation enumeration is the only
universal formula: `perm(A) = Σ_{σ∈Sₙ} Πᵢ A[i, σ(i)]`. For n ≤ 8 this is
tractable (40k perms); for larger matrices the semiring-generic permanent
is intrinsically expensive. Tropical-specific Hungarian in O(n³) is noted
as future work.

### Rationale — fixpoint vs bounded iteration for closure

`M* = I + M + M² + M³ + …`. For boolean semiring on n vertices the series
saturates at M^(n-1) (walks of length ≥ n add nothing new). For tropical
with non-negative weights same bound. For **counting semiring** with any
cycle, the series diverges. Implementation iterates `T_{k+1} ← I + M·T_k`
until `equal?` detects a fixpoint; if the optional `max-iterations`
argument is exceeded, raise an error rather than silently returning
garbage. Default cap is `n` (square matrix) or `max(rows, cols)`.

### Shape / semiring checks

All binary operations (`add`, `mul`) verify both operands were constructed
with the same `semiring` record (pointer identity, `eq?`) and compatible
shapes. `power`, `closure`, `permanent` require square matrices. Mismatches
raise errors via `error` — this matches the `poly-divmod` pattern in
`polynomial.scm:167`.

## Test plan (algebra-matrix-test.scm)

| Group | What it checks |
|-------|----------------|
| construction | make / predicate / shape |
| from-rows / to-rows | round-trip and shape inference |
| identity | `I·M = M`, `M·I = M` for a small M |
| ref | in-bounds values, out-of-bounds error |
| add | elementwise; identity via zero matrix; shape mismatch errors |
| mul | hand-computed 2×2 over counting semiring; 3×3 over boolean semiring; incompatible shapes error |
| power | `M^0 = I`, `M^1 = M`, `M^2 = M·M`, `M^3 = M²·M` |
| closure (boolean) | reachability on a 4-vertex graph — matches hand-computed result |
| closure (tropical) | shortest-paths on same graph, compare with hand-computed distances |
| permanent | 2×2: `a·d + b·c` under counting semiring; tropical permanent = min-cost assignment on a 3×3; boolean permanent = existence of a perfect matching |
| sparse round-trip | dense → sparse → dense preserves non-zero entries |
| with-semiring-matrix | macro rebinds names correctly |

## Commit strategy

Per user guidance "target large commits": ship in **two** commits.

1. **Library + tests** — `matrix.sld`, `matrix.scm`, `algebra-matrix-test.scm`,
   plan file, umbrella re-export, ~350 LOC library + ~200 LOC tests.
2. **Any follow-up fixes surfaced by `make lint && make covercheck`**
   (if needed).

## Non-requirements

- No Go-side registration: this is pure Scheme, discovered via the
  embedded stdlib FS.
- No MCP-server wiring: the MCP server already exposes `libraries` and
  docstrings will flow through automatically.
- No new engine options or extensions.

## Definition of done

- [ ] `make build && ./dist/darwin/arm64/wile --run test/wile/algebra-matrix-test.scm`
      passes 100%.
- [ ] `make lint` clean.
- [ ] Umbrella `(wile algebra)` imports re-export the matrix names.
- [ ] `,apropos matrix` in REPL surfaces the new primitives (sanity check on
      docstring + keyword discovery).
