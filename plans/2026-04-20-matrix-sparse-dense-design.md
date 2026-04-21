# Matrix library: sparse/dense open design questions

**Status:** Design draft — not implemented. Surfaced by crosscheck review of
PR #681 (`feat/algebra-matrix`) on 2026-04-20. Library investigation (gonum,
SciPy, Julia, BLAS) added 2026-04-20; **Path D** in the Synthesis section
below is the current proposed design.

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

### Recommendation

**Q1c — hide the alist, replace with iterator API.** See Path D synthesis
below. BLAS's opaque-handle philosophy and the convergent pattern across
mature libraries (no library exposes raw sparse storage) argue decisively
against freezing the alist shape. Zero consumers now makes the change free.

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

### Recommendation

**Q2a — unified protocol, dispatch internally.** The original framing above
treated 2a/2c as binary ("unified API" vs "parallel names"). Library
investigation shows this conflated two distinct axes: **API unification is
independent of implementation unification**. Every mature library (gonum,
SciPy, Julia) unifies the caller-facing name and dispatches internally to
per-rep implementations. Math is not rep-agnostic; the API can be. See Path
D synthesis below.

Keep narrow predicates `semiring-matrix?` and `sparse-semiring-matrix?` as
gonum-style constraint interfaces alongside the broad `matrix?`.

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

### Recommendation

**Q3a-subset — implement only the pairs with genuine algorithmic speedup,
following Sparse BLAS's subset principle.** The original framing treated
"3a" as all 20 op pairs. Sparse BLAS standardizes only matrix-vector
multiply, matrix-matrix multiply, and triangular solve — not the full dense
BLAS, because many operations (permanent, closure-under-reflexive-transitive)
densify their input anyway. Implement `mul` and `add` for all four pairs;
keep `power`/`closure`/`permanent` dense-only. See Path D synthesis below.

---

## Library investigation (2026-04-20)

User rejected the binary framing "unified API or not" with the observation
that math cannot be representation-agnostic. Investigation of four mature
matrix libraries confirms the principle AND surfaces the convergent design
pattern that resolves the apparent tension.

### Convergent pattern across gonum, SciPy, Julia, BLAS

All four libraries split the problem at the same seam:

| Layer | Polymorphic? | Mechanism |
|---|---|---|
| Inspection (shape, element access) | Yes, universal | Minimal interface: `Dims`, `At`, `T` |
| Arithmetic | Unified name at caller — per-pair at implementation | Concrete-type dispatch |
| Conversion | Explicit, never implicit | `tocsr()`, `sparse(A)`, `Array(S)` |
| Storage format | Opaque | No library exposes raw representation |

### gonum

The `Matrix` interface is three methods — `Dims`, `At`, `T`. Arithmetic is
**not on the interface** — it's defined on concrete types and type-switches
internally via auxiliary interfaces (`RawMatrixer`, `RawSymmetricer`,
`*VecDense`) to pick the right BLAS routine.

```go
type Matrix interface {
    Dims() (r, c int)
    At(i, j int) float64
    T() Matrix
}
```

`c.Mul(a, b)` dispatches internally: `blas64.Gemm` for dense×dense,
`blas64.Symm` for dense×symmetric, `blas64.Gemv` for matrix×vector.

gonum also has **narrow constraint interfaces** (`Symmetric`, `Triangular`,
`Banded`, `Diagonal`) alongside the broad `Matrix`. Callers demanding a
specific structural property use the narrow predicate; callers accepting
any matrix use `Matrix`.

### SciPy

Seven sparse formats: CSR, CSC, COO, BSR, DIA, LIL, DOK — each optimized
for different access patterns. Abstract base class `sparray`. All share
conversion methods (`tocsr()`, `tocsc()`, `toarray()`).

`csr_matrix @ dense_ndarray` dispatches via Python's `__matmul__`/
`__rmatmul__`. Documentation: *"To perform manipulations such as
multiplication or inversion, first convert the array to either CSC or CSR
format."* Format is a storage choice, not a semantic peer.

### Julia

`AbstractSparseArray{Tv,Ti,N}` supertype; concrete `SparseMatrixCSC`,
`SparseVector`. Standard operators `*`, `+` work identically — multiple
dispatch selects the implementation. From docs: *"Arithmetic operations on
sparse matrices also work as they do on dense matrices."*

Julia distinguishes "structural nonzeros" (explicitly stored, may be
numerically zero) from "numerical nonzeros" — managed via `dropzeros()`.
Wile's strip-zeros-on-construction is simpler and acceptable for v1.

### BLAS / Sparse BLAS

From the NIST Sparse BLAS specification:

> "There is no single 'best' storage format for any sparse matrix
> operation, [so] the sparse matrix arguments to the Level 2 and 3 sparse
> BLAS do not use one particular storage format."

Sparse BLAS uses **opaque integer handles**. User calls a creation routine,
library picks internal rep. This is the strongest form of "hide the
representation."

Sparse BLAS standardizes **only a subset** of dense operations: Level 2
(matrix-vector multiply, triangular solve), Level 3 (matrix-matrix
multiply, triangular solve with multiple RHS). Full dense BLAS is not
mirrored — only ops with genuine algorithmic speedup on sparse inputs.

### Implications for Wile

1. **API can be unified (single name) even though implementations are
   per-rep.** All four libraries do this. The plan's earlier 2a-vs-2c
   framing conflated "unified API" with "unified implementation."
2. **Don't implement all 20 op pairs.** Sparse BLAS's subset principle:
   implement only where sparse has a genuine algorithmic advantage.
3. **Opaque rep is universal.** No library exposes raw storage. Q1c
   (iterator, hide alist) is the correct choice regardless of Q2 outcome.
4. **Narrow predicates stay.** gonum's precedent: broad `Matrix`, narrow
   `Symmetric`/`Triangular`/`Banded`. Wile should keep `semiring-matrix?`
   and `sparse-semiring-matrix?` as narrow constraint predicates alongside
   a new broad `matrix?`.

---

## Synthesis — Path D (the proposed design)

Path D combines Q2a + Q3a-subset + Q1c. It matches the convergent pattern
across all four investigated libraries.

### Q2 component: inspection + dispatched-arithmetic protocol

Unified API (polymorphic via internal `cond` dispatch):

- Predicate: `matrix?`
- Inspection: `matrix-rows`, `matrix-cols`, `matrix-shape`, `matrix-ref`,
  `matrix-semiring`
- Arithmetic: `matrix-add`, `matrix-mul`, `matrix-power`, `matrix-closure`,
  `matrix-permanent`
- Capability: `matrix-op-supported?` — `(matrix-op-supported? 'op M ...)`
  → `#t` iff the dispatch table has an entry for `(op, rep(args))`.
  Lets callers branch on capability instead of catching errors. See OQ3.

Narrow constraint predicates (kept):

- `semiring-matrix?` — dense only
- `sparse-semiring-matrix?` — sparse only

Concrete accessors (kept as shortcuts for callers with known rep):

- `semiring-matrix-ref`, `sparse-semiring-matrix-ref`, etc. — direct path,
  no dispatch overhead

### Q3 component: implement pairs where algorithms genuinely differ

Following Sparse BLAS's subset philosophy:

| Operation | D×D | S×D | D×S | S×S | Rationale for gaps |
|---|---|---|---|---|---|
| `mul` | ✓ | ✓ | ✓ | ✓ | Core op; all four pairs have distinct algorithms |
| `add` | ✓ | ✓ | ✓ | ✓ | Element-wise; cheap to specialize |
| `power` | ✓ | — | — | — | Non-idempotent semirings densify quickly |
| `closure` | ✓ | — | — | — | Reflexive + transitive usually densifies |
| `permanent` | ✓ | — | — | — | Still exponential; sparse saves nothing |

Result-rep decisions:

- `sparse + sparse → sparse`
- `sparse × dense → dense`
- `dense × sparse → dense`
- `sparse × sparse → sparse`
- `closure`, `power`, `permanent` → always dense

### Q1 component: hide the alist (Q1c)

Replace `sparse-semiring-matrix-entries` with iterator API:

```scheme
(sparse-semiring-matrix-for-each SM proc)
    ;; proc :: (row col value) -> _

(sparse-semiring-matrix-fold SM proc z)
    ;; proc :: (row col value acc) -> acc
```

Rationale: BLAS's opaque-handle philosophy; no mature library exposes raw
storage. Zero consumers means we can do this cleanly now. Switching
internal rep later (hash-table, CSR, CSC) becomes non-breaking.

Test update: `test/wile/algebra-matrix-test.scm:317` switches from
`(length (sparse-semiring-matrix-entries SM))` to a fold or dedicated
`sparse-semiring-matrix-nnz` accessor.

### Reference semantics (pair/vector alignment)

Scheme accepts reference-mutable values despite being functional —
pairs, vectors, strings, and hashtables are all mutable reference types,
each with an established `!`-suffix convention. Matrices adopt the same
pattern. This matters because (1) matrix copy cost is too high to justify
pure-only semantics, (2) Wile will eventually interop with gonum whose
`c.Mul(a, b)` receiver model maps one-to-one onto Scheme's
destination-first mutators, and (3) users familiar with `set-car!` /
`vector-set!` / `vector-copy!` will have correct intuition about matrix
mutation without needing to learn a new model.

**Identity & sharing** (pair analog):

| Expression | Result |
|---|---|
| `(eq? A A)` | `#t` |
| `(eq? A (matrix-copy A))` | `#f` |
| `(define B A)` then `(matrix-set! A 0 0 99)` | visible through B — B is A |

This is exactly how `(define p (cons 1 2)) (define q p) (set-car! p 99)`
works for pairs. Matrices are reference values; `define` and `let` bind
names, not copies.

**Element-level mutation** (pair `set-car!` / vector `vector-set!` analog):

```scheme
(matrix-set! M i j value)    ; mutate single cell; return unspecified
(matrix-fill! M value)       ; fill all cells
(matrix-copy! dest src)      ; dest ← src; same shape, same rep required
```

For sparse matrices, `matrix-set!` preserves the strip-zeros-on-
construction invariant: setting `(i,j)` to `(semiring-zero S)` removes
the entry; setting to non-zero adds/updates it.

**Operation-level mutation** (gonum `c.Mul(a, b)` analog):

Pure/bang pairs, destination-first, following `vector-copy!` convention:

```scheme
;; Pure — allocate and return new matrix
(matrix-add   A B)
(matrix-mul   A B)
(matrix-power A n)
(matrix-closure A)

;; Mutating — write into destination C, return unspecified
(matrix-add!     C A B)
(matrix-mul!     C A B)
(matrix-power!   C A n)
(matrix-closure! C A)
```

**Future: submatrix views** (list-tail analog). Scheme's `list-tail`
returns a shared-structure sublist; gonum's `m.Slice(i,k,j,l)` returns a
shared-storage submatrix. Both mutate-through-view. Wile v1 defers views,
but the dispatch protocol must allow adding `<matrix-view>` as another
`matrix?`-satisfying record later without breaking existing code.

---

## Open implementation questions

Six decisions still to resolve before Path D implementation begins.
OQ1-OQ3 were the original three from the investigation. OQ4-OQ6 surfaced
from the reference-semantics refinement (see *Reference semantics*
subsection under Path D above).

### OQ1: which call-convention variants to expose

**Resolved: both pure and bang forms, in pair/vector style.**

The original framing cast this as return-new *vs* mutate-receiver.
Scheme's prior art on pair / vector / string / hashtable mutation shows
the answer is always **both** — `vector-ref` + `vector-set!`,
`vector-copy` + `vector-copy!`. Pure forms allocate; `!` forms mutate.
Users pick per call-site based on whether allocation pressure matters.

Matrices are reference types (see *Reference semantics* above), so the
bang forms aren't a compromise — they're the idiomatic Scheme expression
of "mutate this reference." Deferring them to "later if profiling shows
a bottleneck" would contradict the pair/vector precedent and leave Wile
misaligned with gonum when interop arrives.

Implementation implication: each arithmetic operation ships as a pair
of primitives from day one. `matrix-add` and `matrix-add!`,
`matrix-mul` and `matrix-mul!`, etc. The pure form is implementable as
`(define (matrix-add A B) (let ((C (matrix-allocate-for-add A B)))
(matrix-add! C A B) C))` — implement the bang form first, derive the
pure form from it. Zero code duplication.

### OQ2: narrow sub-interface predicates for future sparse subtypes

**Resolved 2026-04-20: absorbed by OQ3's `matrix-op-supported?` predicate.**

gonum has `Symmetric`, `Triangular`, `Banded`, `Diagonal` as narrow
interfaces. If Wile later adds CSR, CSC, or banded representations, do
callers want predicates like `(csr-matrix? M)`, `(banded-matrix? M)` to
constrain arguments?

The motivating use case was capability discovery — "can I do this op on
this matrix?" — which OQ3's `matrix-op-supported?` now answers directly
without a per-rep predicate. See OQ3's "Why this absorbs OQ2" subsection.

The two existing narrow predicates (`semiring-matrix?`,
`sparse-semiring-matrix?`) stay — they serve type-dispatch use cases
that `matrix-op-supported?` doesn't address (e.g., branching on rep to
pick a direct accessor for performance). Future narrow predicates are
added only when a specific caller demands them.

### OQ3: error behavior for unimplemented (rep, op) pairs

**Resolved 2026-04-20: typed error + required docstring gap + capability
predicate. Absorbs OQ2.**

If user calls `(matrix-permanent SM)` where `SM` is sparse and we haven't
implemented sparse permanent:

- (a) Clear typed error: *"matrix-permanent: not implemented for sparse
  operands; convert via `sparse->semiring-matrix`"*
- (b) Silent densify + compute
- (c) (a) + document the gap in the docstring

#### Runtime vs anticipatable

Two questions sharpen the design:

1. **Are these errors runtime errors?** Yes, by necessity. Wile's Scheme
   is dynamically typed; no earlier phase has rep information. The
   parser sees an identifier, the expander has no types, the compiler
   emits a foreign call. Only the primitive, at invocation, can inspect
   the argument's rep. Lifting earlier requires a type system Wile
   doesn't have.

2. **Could the programmer have anticipated it?** Fully. The programmer
   wrote `matrix-permanent` explicitly, passed a sparse matrix
   explicitly, and could have read the docstring. This is categorically
   different from unanticipatable runtime errors (network timeout, OOM,
   stack overflow) — it's a choice the library rejects, closer to
   "wrong argument type" than "resource unavailable."

The anticipation answer constrains the design: *technically* anticipatable
but *practically* un-anticipatable (only documented in prose) is the
anti-pattern. The fix is a programmatic capability query.

#### Resolution: three-part

**Part 1 — typed error.** Wile's sentinel + wrap pattern. Add
`werr.ErrUnsupportedMatrixRep` (or similar) and wrap at each call site
with an actionable message:

```
matrix-permanent: sparse operand not supported; convert via
  `(sparse->semiring-matrix M)` or check
  `(matrix-op-supported? 'permanent M)` first
```

Silent densification is rejected — it would hide performance cliffs and
violate the explicit-conversion principle that motivated Q1c's iterator
design.

**Part 2 — required docstring gap section.** Every op with (rep, op)
gaps carries an `Unsupported:` or `Not supported:` line in its docstring
listing the rejected reps. Not optional. Example:

```
matrix-permanent
  Compute the permanent of a dense matrix over the given semiring.
  Not supported: sparse matrices (convert via
    sparse->semiring-matrix first).
```

This makes the gap machine-discoverable via the existing documentation
system (`(doc matrix-permanent)`, `apropos`) — no prose-mining required.

**Part 3 — `matrix-op-supported?` predicate.** A capability query:

```scheme
(matrix-op-supported? 'mul A B)        ; binary: query rep-pair
(matrix-op-supported? 'permanent M)    ; unary: query rep
(matrix-op-supported? 'closure M)      ; unary
```

Backed by the internal dispatch table: returns `#t` iff `(op, rep(args))`
has a dispatch entry, `#f` otherwise. Lets users write capability-
branching code instead of catching errors:

```scheme
(if (matrix-op-supported? 'permanent M)
    (matrix-permanent M)
    (matrix-permanent (sparse->semiring-matrix M)))
```

Symbol-based (not procedure-based) because the query is against the
op's *name* in the dispatch table, not against procedure behavior;
procedure reference would introduce circularity.

#### Why this absorbs OQ2

OQ2 asked whether to expose narrow sub-interface predicates (`csr-matrix?`,
`banded-matrix?`) for future reps, modeled on gonum's `Symmetric`,
`Triangular`, `Banded`. The motivation there was *capability discovery* —
"can I do this op on this matrix?" gonum answers by letting the caller
check for a specific structural interface before calling.

`matrix-op-supported?` answers the same question more directly. Instead
of "is this matrix `Symmetric`?" (from which the caller infers "so I can
call `symm.Solve`"), the caller asks "can I call `solve` on this matrix?"
directly. No inference step; no speculative predicate per future rep.

The two existing narrow predicates (`semiring-matrix?`,
`sparse-semiring-matrix?`) stay — they're needed for type-dispatching
code that cares about the concrete rep (e.g., picking a direct accessor
vs going through the polymorphic one). What doesn't get added: predicates
for *future* reps (CSR, banded, diagonal) that don't exist yet. When a
future rep ships, callers who need the capability check already have
`matrix-op-supported?`; a `csr-matrix?` predicate is only needed if a
caller wants a rep-specific code path, which can be added at that time.

### OQ4: destination-rep strictness for bang forms

**Resolved 2026-04-20: strict.**

For `(matrix-mul! C A B)` where `A` and `B` are sparse, the pure-form
result-rep rule says the result is sparse. What if `C` is dense?

Two options considered:

- **Strict (gonum-like):** C must match the expected result rep. Error
  if `(sparse-semiring-matrix? C)` when inputs would produce dense,
  or vice versa.
- **Destination-drives:** C's concrete rep picks the algorithm. Dense C
  with sparse inputs uses the scatter-into-dense kernel (which exists
  as an implementation anyway — it's how `sparse × dense → dense` is
  built). Sparse C with dense inputs would have to densify-then-extract,
  which is pointless.

#### Resolution

Strict. The destination's rep is part of the primitive's contract, not
a hint. `matrix-mul!` errors when C's rep does not match the rep the
pure form would produce for the same inputs. This extends OQ5's
"`!` = no hidden allocation" rule to the representation dimension:
just as `!` forms never allocate the *result* matrix, they never
silently convert the *shape* of the result either. One mental model
covers both: a bang form is a pre-allocated version of the pure form,
and the caller is responsible for allocating a destination of the
right kind.

Error message:

*"matrix-mul!: destination rep `<dense|sparse>` does not match expected
result rep `<sparse|dense>`; use `(matrix-mul A B)` for auto-allocation
or convert inputs via `sparse->semiring-matrix` /
`semiring-matrix->sparse`"*

Implications:

- Destination-drives-widening (dense C with sparse inputs) is explicitly
  deferred. It can be added later as a named primitive
  (`matrix-mul-into!` or similar) without breaking the strict contract
  on `matrix-mul!`. v1 does not ship the escape hatch — callers who
  need that pattern write `(matrix-copy! C (matrix-mul A B))`, which
  makes the intermediate allocation visible.
- This sets the precedent for every future `!` form with a natural
  result rep (`matrix-solve!`, `matrix-factor!`, future decompositions).
  Strict here = strict by default everywhere; a widening variant is a
  separate, named primitive.
- gonum's precedent is weaker than it looks — gonum enforces *shape*
  invariants (`Symmetric`, `Triangular`) that matter for correctness;
  Wile's sparse/dense is a *storage* choice. Wile chooses strict for a
  different reason: to preserve the `!`-convention promise that bang
  forms never surprise the caller with allocation or conversion.

### OQ5: aliasing rules for bang forms

**Resolved 2026-04-20: per-op rule, partitioned by hazard class.**

gonum allows `a.Pow(a, 6)` (destination aliases input; legal because
Pow reads before writing in a safe order) but forbids `a.Copy(a.T())`
(aliasing would corrupt because the transpose view shares storage with
`a`). Wile needs a rule.

Originally framed as a binary choice:

- **Forbid all aliasing:** simplest; leaves no corner for silent
  corruption.
- **Per-operation rule:** read-once-write-once ops allow aliasing;
  incremental-write ops forbid it.

#### What forces the decision

The `(matrix-mul! A A A)` case (square in place) collapses the choice.
Every output cell `A'[i,j] = Σₖ A[i,k]·A[k,j]` depends on every row
and every column of the original A. Writing in place corrupts reads —
computing `A'[0,0]` and storing it into `A[0,0]` invalidates the read
path for `A'[0,1]` and every later cell. No instruction ordering
rescues it; full self-aliased `matrix-mul!` *requires* an internal
temp matrix.

That leaves two consistent positions:

- Silently allocate the temp → violates the implicit `!` = no-alloc
  contract that Scheme's bang convention establishes (`vector-set!`,
  `string-set!`, `hash-table-set!` never allocate). Hidden allocation
  in a primitive named with `!` is a surprise.
- Forbid the alias → preserves the contract; costs an explicit
  pre-alloc or `matrix-copy!`-of-pure-form at the call site.

The symmetric reasoning says `(matrix-add! A A B)` (A += B) is safe
and should work — each cell is read once then written, no hazard, no
temp needed. Forbid-all would force a spurious `let`-and-copy that
allocates precisely what `!` was supposed to avoid.

Those two cases together rule out both endpoints. The ops themselves
partition cleanly into two hazard classes.

#### Resolution: partition ops by hazard class

| Hazard class | Ops | Aliasing rule |
|---|---|---|
| Read-once-write-once (no hazard) | `matrix-add!`, `matrix-sub!`, `matrix-scale!`, `matrix-fill!`, element-wise | Any aliasing legal, including `(op! A A A)` |
| Incremental-write (dest depends on unread cells) | `matrix-mul!`, `matrix-power!`, `matrix-closure!` | Forbid `eq?` overlap between dest and any operand |

Under this rule:

- `(matrix-add! A A B)` ✓ — idiomatic `A += B`
- `(matrix-add! A A A)` ✓ — idiomatic `A *= 2`
- `(matrix-mul! A A B)` ✗ — typed error: *"matrix-mul!: destination
  cannot alias operand; use `(matrix-copy! A (matrix-mul A B))` or
  pre-allocate a scratch matrix"*
- `(matrix-mul! A A A)` ✗ — same error; the
  `matrix-copy!`-of-pure-mul form makes the required allocation
  visible in the source

Element mutators (`matrix-set!`, `matrix-fill!`) don't apply — they
mutate their only argument. `matrix-copy! dest src` errors on
`(eq? dest src)` (no-op that would otherwise be silently correct; the
error surfaces user confusion).

This preserves "`!` = no hidden allocation" universally while keeping
the common in-place accumulation patterns idiomatic. Cost: per-op
aliasing legality becomes a docstring fact (~2 lines per primitive).
gonum goes the other way (detect alias, silently allocate temp);
gonum users expect allocation noise because Go doesn't signal "no
alloc" via naming. Scheme's `!` convention does, so Wile's rule
diverges from gonum here by design.

### OQ6: submatrix views — explicitly deferred, but protocol must allow

Views would look like:

```scheme
(matrix-slice M i-start i-end j-start j-end)   ; view, shares storage
(matrix-row   M i)                              ; row view
(matrix-col   M j)                              ; column view
```

Semantically aligned with `list-tail`: view shares structure; mutation
through view is visible in parent. gonum's `m.Slice(i,k,j,l)` is the
direct precedent.

**Proposal:** defer implementation; design the Path D dispatch so that
adding `<matrix-view>` as a third `matrix?`-satisfying record type is
additive. Concretely: every place in the dispatch that enumerates
concrete reps (`cond` on `(semiring-matrix? x)` / `(sparse-semiring-
matrix? x)`) must have a clearly-extensible shape so a view case slots
in cleanly. Views introduce a new hazard — two views into the same
parent may alias through the parent even when `eq?` says they're
distinct. OQ5's aliasing rule will need to generalize (structural
aliasing, not just `eq?`) when views ship.

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

Under Path D all three are decided together (2a + 3a-subset + 1c).

## Definition of done (for this plan)

- [x] Q1 / Q2 / Q3 each resolved with a recorded decision (Path D:
      Q1c, Q2a, Q3a-subset — 2026-04-20)
- [x] OQ1 resolved: expose both pure and `!` forms, pair/vector style
      (2026-04-20)
- [x] OQ2 resolved: absorbed by OQ3's `matrix-op-supported?` predicate;
      future narrow rep predicates added only on demand (2026-04-20)
- [x] OQ3 resolved: typed error (`werr.ErrUnsupportedMatrixRep`),
      required `Unsupported:` docstring section, and
      `matrix-op-supported?` capability predicate (2026-04-20)
- [x] OQ4 resolved: strict destination-rep matching for arithmetic bang
      forms in v1; widening variant (e.g. `matrix-mul-into!`) deferred
      as future additive extension (2026-04-20)
- [x] OQ5 resolved: per-op aliasing rule by hazard class — no-hazard
      ops (`add!`, `sub!`, `scale!`, `fill!`, element-wise) allow any
      aliasing; incremental-write ops (`mul!`, `power!`, `closure!`)
      forbid `eq?` overlap between dest and operands (2026-04-20)
- [ ] OQ6 resolved: views deferred; dispatch shape designed to accept
      `<matrix-view>` additively
- [ ] Follow-up implementation plan file created for Path D
- [ ] Test at `test/wile/algebra-matrix-test.scm:317` migrated from
      `-entries` to iterator API
- [ ] `sparse-semiring-matrix-entries` deprecation or removal decision
      recorded in `matrix.sld` comments

---

## Sources (library investigation)

- [gonum.org/v1/gonum/mat package documentation](https://pkg.go.dev/gonum.org/v1/gonum/mat)
- [SciPy sparse reference](https://docs.scipy.org/doc/scipy/reference/sparse.html)
- [Julia SparseArrays stdlib](https://docs.julialang.org/en/v1/stdlib/SparseArrays/)
- [Intel oneMKL BLAS and Sparse BLAS routines](https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-c/2024-1/blas-and-sparse-blas-routines.html)
- [NIST Sparse BLAS baseline implementation](https://math.nist.gov/spblas/)
- [Netlib Sparse BLAS chapter 3](https://www.netlib.org/blas/blast-forum/chapter3.pdf)
- [BLAS on Wikipedia](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms)
