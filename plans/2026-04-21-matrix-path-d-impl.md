+++
title = "Implementation plan — Matrix Path D"
date   = "2026-04-21"
status = "Draft — all decisions resolved 2026-04-21; ready for implementation"
parent = "2026-04-20-matrix-sparse-dense-design.md"
+++

# Implementation Plan — Matrix Path D

Executes the design recorded in `2026-04-20-matrix-sparse-dense-design.md`
(PR #682).

**Path D** = Q1c (hide alist, iterator API) + Q2a (unified polymorphic
protocol, dispatch internally) + Q3a-subset (`mul`/`add` for all four
rep-pairs; `power`/`closure`/`permanent` dense-only) + OQ1..OQ5 resolved.

---

## Preconditions

- PR #682 merged (design doc frozen).
- Zero external consumers of `sparse-semiring-matrix-entries`: confirmed
  by grep across the workspace; only internal uses are inside
  `matrix.scm` itself and the test file.
- Three test sites currently call `sparse-semiring-matrix-entries`:
  - `test/wile/algebra-matrix-test.scm:436`
  - `test/wile/algebra-matrix-test.scm:453`
  - `test/wile/algebra-matrix-test.scm:459`

  (Design doc estimated "one site at line 317" — actual scope is three.)

---

## Scope

### In scope

1. **Unified predicates and accessors.** `matrix?`, `matrix-ref`,
   `matrix-shape`, `matrix-semiring`, `matrix-rows`, `matrix-cols`.
   Existing rep-specific predicates (`semiring-matrix?`,
   `sparse-semiring-matrix?`) stay — they serve type-dispatch code,
   per the design's "narrow predicates as gonum-style constraint
   interfaces" note.

2. **Iterator API replacing `-entries` alist exposure.**
   - `matrix-for-each-entry M proc` — proc receives `(r c value)`.
   - `matrix-fold-entries M init proc` — left fold over entries.

   These are the unified peers of the sparse-only alist accessor. On a
   dense matrix, iteration visits every cell; on a sparse matrix, only
   stored non-zero cells. This asymmetry is intentional — it's the
   explicit-rep principle Q1c adopted from BLAS.

3. **Polymorphic arithmetic.** `matrix-add`, `matrix-mul`, and their
   bang-form peers `matrix-add!`, `matrix-mul!` (OQ1).

4. **Capability predicate.** `matrix-op-supported?` backed by the
   internal dispatch table (OQ3).

5. **Dense-only ops** `matrix-power`, `matrix-closure`, `matrix-permanent`.
   Typed error on sparse input per OQ3; each carries an `Unsupported:`
   docstring line per OQ3 Part 2.

6. **Strict destination-rep** for `!` forms (OQ4).

7. **Aliasing rule per hazard class** (OQ5) — enforced in the bang
   primitives, documented per-op.

8. **Remove `sparse-semiring-matrix-entries`** from the library export
   list before v1 stabilizes. Zero consumers + wile's "break freely
   in zero-consumer projects" policy makes this a clean removal.

### Out of scope

- Submatrix views (OQ6 deferred; dispatch shape must accept a third
  record additively).
- Widening bang variant (`matrix-mul-into!`).
- CSR, CSC, banded representations.
- BLAS / gonum interop (separate funding-gated plan).

---

## Design decisions

All three decisions below were resolved on 2026-04-21 before coding
began. Each has the rationale recorded for future reference; the
alternatives are preserved so later maintainers can see what was
considered, not just what was chosen.

### D1 — Dispatch mechanism **[Resolved 2026-04-21: option (b) with sub-variant (b-i)]**

Path D says "unified API, dispatch internally." Two viable Scheme
mechanisms were considered:

- **(a) Cond-on-predicate inline** in each polymorphic procedure. Direct,
  no initialization order, zero extra state, matches every other
  `(wile algebra ...)` library. Downsides: `matrix-op-supported?` must
  be hand-maintained as a duplicate cond, and each new rep edits N op
  procedures.

- **(b) Internal dispatch table** keyed by `(op-sym rep-tag ...)`,
  looked up at call time. `matrix-op-supported?` becomes a trivial
  `hashtable-contains?`; new reps register once and get every op; the
  full rep-combination matrix is introspectable via `hashtable-keys`.

#### Resolution

**(b).** The single decisive argument is that `matrix-op-supported?`
is free under (b) and requires a hand-maintained parallel cond under
(a). OQ3 already commits the design to a capability predicate; duplicating
its logic in two places (dispatch cond + capability cond) is exactly
the hand-unrolled pattern Wile's global CLAUDE.md flags as a red
signal.

Counterweight accepted: (b) introduces the first top-level mutable
dispatch table in `(wile algebra ...)`. `fca.scm` uses hashtables only
as per-call algorithmic state, and `rewrite.scm` threads rule
databases as values, not global state. (b) sets a new precedent.
Justified here by OQ3's capability-query requirement; we would not
adopt it without that requirement.

Load-order is a non-issue at current scope because `matrix.scm` is a
single file: top-level forms execute in source order (table →
kernels → registrations → dispatchers), and any external caller runs
after the library's `import` completes. A library-preamble comment
will record this as a single-file invariant — if the library ever
splits, the split boundary must preserve ordering or introduce an
explicit `ensure-registered!` guard.

#### Sub-choice: rep-tag resolver (b-i vs. b-ii)

Given (b), the dispatcher needs to turn a matrix *value* into the
*symbol* used as the lookup key. Two shapes were considered:

- **(b-i) Predicate-cond function.** A single `matrix-rep-tag`
  function with one cond chain; returns `'dense` / `'sparse` / etc.
  No changes to existing record types. The cond exists but exists
  *once* and is the sole OQ6 extension point — new reps add one
  clause.

- **(b-ii) Rep-tag field on each record.** Bake the tag into each
  instance at construction; dispatch becomes a field read. Still
  requires an outer cond to pick the right accessor (Scheme records
  have no common supertype), so the cond isn't eliminated — and every
  constructor call site must be edited to pass the tag, storing it
  redundantly on every instance forever.

**Resolution: (b-i).** The outer cond is unavoidable under (b-ii) as
well, so (b-ii) buys no structural simplification — only churn on
existing records and redundant per-instance storage. (b-i) keeps the
existing record types untouched; the rep-enumeration cond lives in
one named function (`matrix-rep-tag`) that doubles as the OQ6
extension point.

#### Implementation cost of (b) + (b-i)

Roughly 30 lines of scaffolding on top of the kernels Path D already
requires. Concretely: dispatch table + `register-matrix-op!` + lookup
helper (10 lines), `matrix-rep-tag` function (5 lines), dispatcher
wrappers (1 per polymorphic op, 5 lines each), `matrix-op-supported?`
(3 lines), bootstrap-sanity test (15 lines), library-preamble
invariant note (3 lines).

#### Implementation note: alist backing, not hashtable

Discovered during P2 (2026-04-21): Wile's `make-hashtable` rejects
list keys with `"key is not hashable"` — the hasher requires atomic
types only. The dispatch table is therefore implemented as a
top-level association list (`*matrix-ops*`) rather than a hashtable,
with `register-matrix-op!` prepending entries and `matrix-op-lookup`
scanning via `assoc`. At the expected table size (≤20 entries for
Path D's 4 ops × 4 rep-pairs, minus dense-only gaps), the O(n)
linear scan is negligible next to any realistic matrix operation.
Keys stay readable as lists (`(list 'add 'dense 'sparse)`).
Semantics match the design: one table, one registration path, one
lookup path, `matrix-op-supported?` is still a single-line
`assoc`-based check.

### D2 — Error surfacing convention **[Resolved 2026-04-21: option (a)]**

Existing `matrix.scm` raises errors via R5RS `(error msg . irritants)`
at 15 sites. No R7RS condition records; no error-object type; no Go
sentinels visible. The design doc's `werr.ErrUnsupportedMatrixRep` is
a Go-layer construct — not reachable from pure Scheme.

Three options were considered:

- **(a) Bare `(error ...)` with actionable message prefix.** Matches
  precedent exactly; caller discrimination is by
  `error-object-message` prefix.

- **(b) Scheme record type for matrix errors.** Define
  `<matrix-error>` with structured fields; raise via `(raise ...)`.
  Adds structure but diverges from the library's style.

- **(c) Tagged-irritant convention.** First irritant is a symbol
  like `'unsupported-matrix-rep`; callers pattern-match via
  `error-object-irritants`. Splits the difference.

#### Resolution

**(a).** Rationale: programmatic capability is already available via
`matrix-op-supported?` (OQ3), so errors don't need to carry a parallel
discrimination mechanism — they're for humans. (a) also means the
`-entries` removal, the new dispatcher misses, the OQ5 aliasing
violations, and the OQ4 rep-mismatches all raise errors in the *same*
shape as the 15 existing call sites, keeping the library visually
uniform.

#### Irritant convention

Errors raised from Path D code follow the same pattern as existing
code: `(error "<function>: <what failed>; <advice>" <offending values>)`.
No symbolic tag as first irritant; the error message carries the
diagnostic text. Example for the dispatcher miss-branch:

```scheme
(error "matrix-add: unsupported rep combination; check (matrix-op-supported? 'add A B)"
       (matrix-rep-tag A) (matrix-rep-tag B))
```

The rep-tag resolver's own error branch follows the same convention:

```scheme
(error "matrix-rep-tag: not a matrix" M)
```

No pre-commitment to (c); if a future requirement genuinely needs
structured error discrimination, migration is mechanical (prepend a
symbol to existing irritant lists at call sites).

### D3 — Phase ordering: pure-first or bang-first **[Resolved 2026-04-21: option (a) — bang-first]**

Design doc (§OQ1) prefers bang-first: "implement the bang form first,
derive the pure form from it. Zero code duplication."

Options considered:

- **(a) Bang-first**, per design doc. Pure form is a one-liner
  wrapping the bang form with allocation.
- **(b) Pure-first**. Ship pure ops first; add bang forms and the
  aliasing machinery in a second phase.

#### Resolution

**(a).** Two reasons: (i) zero code duplication between pure and
bang — the bang form does the work, the pure form allocates and
calls the bang; (ii) OQ4 (strict destination-rep) and OQ5 (aliasing
rules by hazard class) are the highest-risk pieces of Path D, so
forcing them into the first arithmetic phase surfaces bugs early
rather than deferring them to a second round of review.

Phase P5 is therefore "bang form with full OQ4/OQ5 enforcement, then
derive pure wrapper." Each arithmetic op ships as a pair from day
one, matching the pair/vector precedent cited in OQ1.

---

## Phases

(D3 resolved to bang-first. Sequence below is final.)

**P1. Baseline safety net.** Ensure every existing test in
   `algebra-matrix-test.scm` passes unchanged. Snapshot coverage
   numbers for regression check at P-final.

**P2. Dispatch scaffold** (D1=b + b-i). Add in source order at the top
   of `matrix.scm`, immediately below the existing preamble comment:

   1. Library-preamble invariant note (single-file load order).
   2. `(define *matrix-ops* (make-hashtable))`.
   3. `register-matrix-op!`, `matrix-op-lookup` helpers.
   4. `matrix-rep-tag` function — one cond clause per existing rep
      (`semiring-matrix?` → `'dense`; `sparse-semiring-matrix?` →
      `'sparse`). Error branch per D2's irritant convention.

   At this phase no polymorphic op exists yet and no registrations
   happen; the scaffold is dark (tests assert the table is empty).
   Subsequent phases (P4, P5, P6, P7) register entries as they land.

**P3. Iterator API.** Add `matrix-for-each-entry`, `matrix-fold-entries`
   for both reps. Migrate three test sites at lines 436/453/459 to the
   iterator API — critically, rewrite entry-count assertions so they
   don't depend on alist enumeration order. Remove
   `sparse-semiring-matrix-entries` from exports and from the `.sld`
   file; delete its definition in `matrix.scm`.

**P4. Unified accessors.** Add `matrix?`, `matrix-ref`, `matrix-shape`,
   `matrix-semiring`, `matrix-rows`, `matrix-cols`. Each delegates via
   dispatch. Docstrings include `Category: algebra` and keywords for
   apropos.

**P5. Unified arithmetic (bang first per D3=a).**
   - `matrix-add!` — no-hazard class; any aliasing legal.
   - `matrix-mul!` — incremental-write class; `eq?` overlap
     between dest and any operand is a typed error (OQ5).
   - Strict destination-rep per OQ4 (error if dest is wrong rep for
     the computed result).
   - Derive `matrix-add` and `matrix-mul` as pure wrappers.

**P6. Capability predicate** `matrix-op-supported?`. Symbol-based per
   OQ3; returns `#t` iff `(op, rep-tags-of args)` has a dispatch entry.

**P7. Dense-only op sparse-error paths.** `matrix-power`,
   `matrix-closure`, `matrix-permanent` raise the chosen typed error
   (per D2) on sparse operand. Each docstring gets an `Unsupported:`
   line enumerating rejected reps.

**P8. Aliasing rule enforcement.** For incremental-write `!` ops,
   check `(eq? dest A)`, `(eq? dest B)` at entry and raise the typed
   error with the recommended workaround (`matrix-copy!` of the pure
   form).

**P9. Umbrella and apropos.** Re-export new names from
   `stdlib/lib/wile/algebra.sld`. Verify `(apropos matrix)` surfaces
   the new primitives with good keyword coverage.

**P10. Doc + release note.** Update `docs/extensions/libraries.md` if
   matrix is referenced; add a line to release notes explaining that
   `-entries` is removed in favor of iterator API.

---

## Test matrix (P5 checkpoint)

For each op in `{add, mul}`:

| Case | Expected |
|---|---|
| dense × dense | value-equal to existing `semiring-matrix-add`/`-mul` |
| dense × sparse | value-equal to dense(dense × dense(sparse)) |
| sparse × dense | same, commuted |
| sparse × sparse | for `add`: sparse result; for `mul`: sparse (via scatter-accumulate) |
| mixed semirings | typed error |
| shape mismatch | typed error |
| `!` form with correct dest rep | in-place mutation verified via `eq?` |
| `!` form with wrong dest rep | typed error (OQ4) |
| no-hazard op `(add! A A A)` | succeeds, A doubled (OQ5) |
| incremental-write `(mul! A A B)` | typed error (OQ5) |
| `matrix-op-supported?` matches actual dispatch | symmetric check |

For each op in `{power, closure, permanent}`:

| Case | Expected |
|---|---|
| dense input | existing behavior preserved |
| sparse input | typed error with advice message |
| `matrix-op-supported?` | `#f` for any sparse arg |

---

## Definition of done

- [x] D1, D2, D3 decisions recorded in this file (2026-04-21).
- [ ] New names exported from `matrix.sld`; `-entries` removed.
- [ ] Three test migrations complete with order-independent assertions.
- [ ] `make build && make test` pass.
- [ ] `make lint && make covercheck` pass.
- [ ] New code coverage ≥ library baseline captured in P1.
- [ ] `(apropos matrix)` surfaces new primitives with keywords.
- [ ] Design-doc `2026-04-20-matrix-sparse-dense-design.md` Definition-
      of-Done checkboxes updated (OQ6 dispatch-shape confirmation,
      follow-up-impl-plan row, test-migration row, `-entries` removal
      row).
- [ ] `plans/CLAUDE.md` index updated: move this plan from Open to
      Completed upon merge.

---

## Risks & cross-cutting concerns

1. **Entry-order assertions.** Tests at 436/453/459 compare alist
   length, not contents. Length is order-independent, so the migration
   is safe — but any other call site that compared full alists would
   break silently. Audit during P3.

2. **`with-semiring-matrix` macro.** `matrix.scm` exports
   `with-semiring-matrix` that destructures a dense record. Under Path D
   either (i) it stays dense-only and we ship a polymorphic peer
   `with-matrix`, or (ii) the macro dispatches on rep at expansion time.
   Decide in P4. (ii) is cleaner at the use site; (i) keeps the macro
   body simple.

3. **OQ6 protocol-shape guarantee.** Every dispatch site (D1=a: every
   `cond`; D1=b: the registration table) must be structured so a third
   record type can slot in as an additive commit. Write a one-line
   contract note in the library preamble stating this.

4. **Bootstrap order** (D1=b). The library is a single file, so
   top-level source order guarantees the table is fully populated
   before any external caller reaches a dispatcher. Preamble note
   records this as a single-file invariant. Bootstrap-sanity test
   (added in P2) asserts the expected key set after library load
   and catches "forgot to register" regressions. If the library is
   ever split across files, re-visit — either preserve the source-
   order invariant across files or introduce an `ensure-registered!`
   guard in each dispatcher.

5. **`matrix-copy!` does not yet exist** — it's referenced in the
   OQ5 error-message advice. Either define it in P5 (cheap) or change
   the advice to a form the library already has.

---

## Open questions (post-decision)

- Should `matrix-op-supported?` accept varargs (`(matrix-op-supported?
  'mul A B)` OR `(matrix-op-supported? 'mul A B C)` for possible future
  ternary ops)? Default: yes, varargs; the dispatch table key is
  `(op . rep-tags)`.

- `matrix-copy!` — dense-only or polymorphic? Polymorphic is consistent
  with Path D; dense-only is simpler and probably sufficient for v1
  (the only caller is the OQ5 error advice).

- Keyword-field updates for `apropos`: the new unified names need
  keywords that surface on searches like "matrix add" without
  crowding the existing rep-specific hits. Coordinate with
  `keywords-motivation.md`.

---

## Follow-ups (deferred from P5a)

- **Sparse-sparse add complexity.** The P5a `matrix-add!/sparse/sparse/
  sparse` kernel uses repeated `(assoc k ea)` / `(assoc k eb)` inside
  its merge loops, giving O(|ea|·|eb|) worst-case. Acceptable for v1.x
  with zero consumers; the right place to fix this is alongside P5b's
  mul kernel, where scatter-accumulate infrastructure (coordinate-
  keyed hashtable or pre-sorted alist + linear merge) will already be
  in place. Copilot review on PR #687 raised this; deferred by design.
