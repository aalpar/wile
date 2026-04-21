# Matrix library: sparse/dense open design questions

**Status:** Design draft — not implemented. Surfaced by crosscheck review of
PR #681 (`feat/algebra-matrix`) on 2026-04-20.

**Background:** `stdlib/lib/wile/algebra/matrix.scm` ships two matrix records —
`<semiring-matrix>` (dense, row-major vector) and `<sparse-semiring-matrix>`
(alist of `((row . col) . value)` entries). Every operation is currently
implemented on the dense rep; sparse participates only as a storage/
conversion target. Three related questions remain open.

Implementing any of these is a breaking change to the public API. Zero
consumers exist today; deciding now is cheaper than deciding later.

## Why one plan

Questions 1 and 2 are separable. Question 3 is downstream of both. Grouping
them means each decision is made with the others in view — picking 2a without
committing to 3a would leave the abstraction half-built.

---

## Question 1: Does `sparse-semiring-matrix-entries` freeze the alist shape as public API?

### Status quo

```scheme
(sparse-semiring-matrix-entries SM)
;; => (((0 . 0) . 5) ((1 . 2) . 7))   ; alist of ((row . col) . value)
```

The accessor returns the internal alist directly. Its shape is documented in
the docstring and in the `.sld` export list, pinned by
`test/wile/algebra-matrix-test.scm:317` which reads
`(length (sparse-semiring-matrix-entries SM))`.

### Why it matters

The alist representation was chosen for simplicity. A hash-table, flat vector
of COO triples, or CSR-style (`rowptr`, `colidx`, `values`) would be faster
for large matrices and is the natural evolution. Today's accessor makes that
switch a breaking change: any caller that pattern-matches the alist shape
breaks silently.

### Options

**1a. Freeze explicitly.** Keep the accessor; document the alist shape as
part of the API contract. Commit to the rep forever (or until a v2). Cheap
now, expensive later.

**1b. Add iterator-style accessor, deprecate raw access.** Introduce
`sparse-semiring-matrix-for-each` taking a `(lambda (row col value) ...)`
and `sparse-semiring-matrix-fold` with `(lambda (row col value acc) -> acc)`.
Mark the raw `-entries` as deprecated-for-removal; in the interim it stays
but its result becomes a copy (prevents mutation shortcuts). Costs one round
of deprecation churn; buys representation freedom.

**1c. Remove raw access immediately.** Replace `-entries` with the iterator
API. Zero-consumer-friendly; most aggressive. If someone does want the alist,
they can `fold` it themselves into whatever shape they want.

### Trade-offs

- (1a) preserves the test at
  `test/wile/algebra-matrix-test.scm:317` as-is; (1b)/(1c) require the test
  to use the iterator.
- (1b) and (1c) both unlock switching to hash-table/CSR later. (1c) skips the
  deprecation phase because there are no consumers.
- User preference on the `feedback-unified-api-design.md` memory note favors
  "single form over parallel variants" — argues against (1b)'s two-way surface.

### Recommendation placeholder

Defer until question 2 is decided. If the sparse rep is going to become a
first-class peer (via the polymorphic protocol in Q2), the iterator shape
falls out naturally as part of that protocol, and we avoid introducing a
sparse-only `for-each` that the protocol then subsumes.

---

## Question 2: Polymorphic `matrix?` / `matrix-ref` protocol across dense + sparse?

### Status quo

Parallel accessors:
- `semiring-matrix?` / `sparse-semiring-matrix?`
- `semiring-matrix-rows` / `sparse-semiring-matrix-rows` (same for `-cols`,
  `-semiring`, `-ref`)

Callers wanting polymorphism must type-dispatch:

```scheme
(cond ((semiring-matrix? M) (semiring-matrix-ref M i j))
      ((sparse-semiring-matrix? M) (sparse-semiring-matrix-ref M i j))
      (else (error "not a matrix")))
```

### Why it matters

This is the structural split the crosscheck's type-design lens flagged. The
current API says "here are two unrelated records that happen to be
related by your imagination"; consumers that want to write rep-agnostic code
have to write the dispatch themselves.

### Options

**2a. Unified dispatching protocol.** Add:
- `matrix?` — true for either record
- `matrix-rows`, `matrix-cols`, `matrix-semiring`, `matrix-ref` — dispatch
  internally via `cond`
- Possibly `matrix-shape`

Existing `semiring-matrix-*` and `sparse-semiring-matrix-*` accessors stay
as concrete-rep shortcuts for callers who know which rep they hold.

**2b. Abstract record via interface ("generic" style).** Define a
`define-record-type` hierarchy with a tagged variant — but R7RS does not
support record inheritance. Would require a custom dispatch table or a
records-as-procedures encoding. Rejected on complexity cost.

**2c. Status quo, better docs.** Keep the parallel accessors; clarify in
docstrings that sparse is a secondary storage form requiring explicit
conversion (`sparse->semiring-matrix`) before polymorphic use. Pragmatic if
we want to minimize surface area; concedes that sparse stays second-class.

### Trade-offs

- (2a) adds API surface (~5 new names) and mild dispatch overhead per call.
  Makes the representation split visible at the protocol level rather than
  hidden behind parallel names.
- (2a) does not subsume the concrete accessors — callers with a known rep
  still use the faster direct path.
- (2c) matches the existing plan (`2026-04-20-algebra-matrix-impl.md`) which
  scoped sparse as "storage + conversion". Argues for minimal-surface
  discipline.
- (2a) aligns with how `(wile algebra)` sibling modules expose their records
  (one predicate per structure), but those modules only have one rep.

### Recommendation placeholder

Pick (2a) if Q3 is answered 3a (mixed-operand ops). (2a) + (3a) compose into
"sparse is a peer representation" coherently. Pick (2c) if Q3 is 3c (no
sparse ops at all).

---

## Question 3: Do `add` / `mul` / `power` / `closure` / `permanent` accept sparse operands?

### Status quo

Every binary op on sparse requires explicit conversion:

```scheme
(semiring-matrix-mul (sparse->semiring-matrix A)
                     (sparse->semiring-matrix B))
```

This materializes every zero cell into the dense rep before multiplying,
defeating the storage saving that motivates sparse in the first place.

### Why it matters

The bigger the sparse matrix, the more expensive the conversion. At some
size, `sparse * sparse` via conversion is strictly slower than
`sparse * sparse` that skips zero cells — which is precisely when a user
would reach for the sparse rep. Today's API forces the wrong trade-off.

### Options

**3a. Mixed-operand protocol.** Each operation dispatches on its operands
and picks an implementation:
- dense × dense → existing O(n³) schoolbook
- sparse × dense → iterate non-zero entries of LHS, scatter into dense accum
- dense × sparse → transpose of above
- sparse × sparse → iterate LHS entries, look up each column's non-zeros in
  RHS (naive O(nnz(A) · nnz(B) / n) for random sparsity)

Result rep is a design decision per operation: `sparse + sparse = sparse`,
`sparse * sparse = sparse`, `sparse * dense = dense`. Needs (2a) to be
tractable.

**3b. Sparse-specialized parallel ops.** Add `sparse-semiring-matrix-mul`
etc. — callers do their own dispatch. Avoids internal `cond` dispatch but
doubles the API surface and still requires callers to know their rep shape.

**3c. No sparse ops; conversion-only.** Document that sparse is storage-
only. Callers who want sparse math convert, compute, convert back. Minimal
surface, but users reaching for sparse for perf reasons are paying double.

### Trade-offs

- (3a) is the only option that delivers sparse's performance promise. Cost:
  5 × 4 = 20 operation pairs to implement, though many reduce to common
  kernels.
- (3a) requires deciding result rep per operation. `closure` on a sparse
  matrix may densify anyway (reflexive + transitive = usually dense) — so
  the promised speedup may not materialize for some operations.
- (3b) keeps each implementation simple at the cost of API breadth.
- (3c) is the current state. Defensible for a v1.

### Recommendation placeholder

(3a) is the correct long-term answer. In-scope for follow-up work after the
matrix library stabilizes. Not blocking shipping the current branch.

---

## Dependency map

```
Q2 ──── chooses ────▶ polymorphic protocol ─┐
                                             ├──▶ enables Q3a
Q3 ──── operation dispatch ──────────────────┘

Q1 ──── Q2a via iterator ──▶ eliminates need for raw -entries
Q1 ──── Q2c (no protocol) ──▶ Q1 stands alone, decide independently
```

## Decision order

1. **Q2 first** (protocol or not). This is the architectural commitment.
2. **Q3 follows.** (3a) only makes sense under (2a).
3. **Q1 last.** If (2a) + iterator protocol, (1c) is natural. If (2c),
   decide (1a) vs (1b) independently.

## Definition of done (for this plan)

- [ ] Q1 / Q2 / Q3 each resolved with a recorded decision
- [ ] If any "a" option chosen: follow-up impl plan file created
- [ ] If any "c" option chosen: relevant docstrings state the decision
      clearly so users don't expect the "a" behavior
