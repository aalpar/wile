# Crosscheck Follow-ups — `5a53b985..HEAD`

**Source:** `/crosscheck` (code / errors / consistency lenses) over the per-thread
allocation pools + apply-in-place refactor and the `graph-partition` / `fca`
changeset.

**Verdict from the review:** no Critical findings, no correctness or race bugs.
Full suite green (326/326 Scheme; Go `machine`/`registry/core`/`extensions/threads`;
vet/lint clean). Every item below is **doc or style only and behavior-preserving** —
there is nothing here that changes runtime behavior. This plan exists to clear
low-severity convention drift and tighten one inaccurate checked-in claim.

Each task is independent; they can land in any order, in a single commit, on a
branch off `master`. No new tests required (no behavior changes); re-run
`make lint && make covercheck` and the algebra/machine suites as the gate.

---

## Tier 1 — convention + accuracy (do)

### 1. `fca.scm:142` — one-armed `if` → `unless`

`context-from-alist` uses a one-armed conditional with a side-effecting `begin`:

```scheme
(if (not (hashtable-ref obj->attrs o #f))
  (begin
    (hashtable-set! obj->attrs o (sort-strings as))
    (set! obj-acc (cons o obj-acc))))
```

The directory idiom (`algebra/CLAUDE.md`; `combinatorial-graph.scm` uses
`when`/`unless` 17×, including the `%validate-seed`/`graph-partition` code added
in this same range) is:

```scheme
(unless (hashtable-ref obj->attrs o #f)
  (hashtable-set! obj->attrs o (sort-strings as))
  (set! obj-acc (cons o obj-acc)))
```

- **Grounding:** [CONVENTION] — directory-wide idiom; `fca.scm` itself had no prior
  `when`/`unless`, so this only drifts from the package, not the file.
- **Risk:** none — `unless` is exactly one-armed `(if (not …) (begin …))`.

### 2. `plans/2026-06-08-per-thread-pools-invariant.md:32` — "no mutex" → "uncontended"

The plan doc states: *"With per-thread pools there is no mutex."* This is literally
false: each `FreeList` retains `mu sync.Mutex` (`machine/pool_generic.go:209`); the
per-thread freelists are built by the same `NewFreeList` and still take the lock.
The actual win is that the lock is **uncontended** (touched by one goroutine), not
absent.

- Fix: reword to "each freelist's mutex is uncontended — touched by exactly one
  goroutine — so the lock is never a serialization point" (or similar).
- Note `machine/pool.go:133` is already correct ("removing the mutex and
  atomic-counter *contention*") — leave it; only the plan-doc line is wrong.
- **Grounding:** [CONVENTION] — verifiable against `pool_generic.go:209`.

### 3. `machine/machine_context_apply.go:85` — clarify `reconfigured` flag scope

`Apply` sets `p.reconfigured = true` on *every* closure application, including the
bytecode `OpApply` path where no one reads it before the next foreign dispatch
clears it (`machine_context_apply.go:134`, `call_foreign_cached.go:85`). Harmless
today, but the field is transiently "dirty true" outside its meaningful window.

- Fix: one-line comment at the set site noting the flag is **only meaningful in the
  clear→call→read window of `applyForeign`/`callForeignCached`**; a stale `true`
  from a bytecode `Apply` is always cleared before any dispatcher reads it. Guards a
  future opcode author from reading it stale.
- **Grounding:** [SYMPTOM→doc] — defensive comment, not a behavior change.

### 4. `combinatorial-graph.scm` `graph-partition` docstring (Examples ~line 2070) — add weighted/seed example

The docstring's only example is the zero-option form
`(graph-partition (complete-bipartite-graph 3 3))`. The `(weight . PROC)` and
`(seed . ALIST)` options must each be a **separate trailing arg** per the
`make-X . opts` convention; a single combined alist double-wraps and trips
`validate-opts-keys`. Add one weighted-form line to the Examples block, e.g.:

```
  (graph-partition g '(balance . 0.3) (cons 'weight (lambda (d) (car d))))
```

- **Grounding:** [CONVENTION] — matches `algebra/CLAUDE.md` opts convention; prevents
  a real misuse the current example invites.

---

## Tier 2 — doc-only clarification (do)

### 5. `machine/machine_continuation.go:146` — document intentional global-pool alloc in `Copy()`

`Copy()` is a `*MachineContinuation` method with no `*MachineContext` receiver, so it
allocates from the **global** `acquireContinuation()`, while the resulting frame is
later released via `mc.releaseContinuation()` into a **per-thread** pool. Both review
lenses concur this is **benign**: same-goroutine sequential access, a
`*MachineContinuation` is valid in any freelist of its type, and thread-confinement
prevents cross-goroutine release. The only residual is slow asymmetric *drift*
between global and per-thread pools over a program's life — an efficiency wrinkle,
not correctness.

- Fix: a comment at the `acquireContinuation()` call explaining (a) why it uses the
  global pool (no MC receiver), (b) that the frame may be released into a per-thread
  pool, and (c) why that is safe (same goroutine; type-valid in any freelist;
  drift-only, not a race). Records the decision so a future reader doesn't "fix" it
  by plumbing a receiver through the delimited-continuation copy hot path.
- **Decision:** comment only. A receiver-aware `Copy` variant is over-engineering for
  a benign drift on a hot path (cf. `memory/` optimization-history notes); not worth
  it.
- **Grounding:** [SYMPTOM→doc].

---

## Tier 3 — same-file consistency (do; out of original diff scope)

### 6. `combinatorial-graph.scm:139` — migrate `make-graph` proc-opt check to `assert-procedure`

`graph-partition` (new code) validates its `weight` proc-opt with
`(assert-procedure "graph-partition" weight)` — the codebase-wide helper mandated by
`algebra/CLAUDE.md` "Procedural-argument discipline" and used across `category.scm`,
`ring.scm`, etc. Its same-file sibling `make-graph` is the **outlier**, hand-rolling:

```scheme
(when (and nfn (not (procedure? nfn)))
  (error "make-graph: neighbor-fn must be a procedure" nfn))
```

- Fix: replace with `(assert-procedure "make-graph" nfn)` **iff** `assert-procedure`
  accepts the `(and nfn …)` optionality (verify it no-ops / errors appropriately on
  `#f` — `make-graph`'s `nfn` is optional). If `assert-procedure` rejects `#f`,
  guard it: `(when nfn (assert-procedure "make-graph" nfn))`. **Read
  `assert-procedure`'s definition in `(wile algebra setoid)` before editing** — do
  not assume its `#f` behavior.
- **Grounding:** [CONVENTION] — `make-graph` diverges from both the new code and the
  package convention.

---

## Not addressed (documented for the record)

- **`graph-partition` `weight-fn` result unchecked** (`combinatorial-graph.scm:158`).
  A non-numeric result surfaces as a loud Go type error through `+`; a *negative*
  weight silently yields a wrong cut. Left as-is: matches the algebra-library ethos
  that weight/carrier contracts are advisory caller-responsibility
  (`algebra/CLAUDE.md`). Documented-precondition surface, not a defect.

- **Cross-`Run` continuation pool migration** — a continuation captured in one
  top-level `Eval` and invoked in a later one (same primordial goroutine) migrates
  frames between two `newThreadPools()` instances. Benign (sequential, same
  goroutine, per-`FreeList` uncontended lock). Subsumed by item 5's reasoning; no
  separate action.

---

## Execution gate

After the edits:

```bash
make lint && make covercheck
go test ./machine/... ./registry/core/... ./extensions/threads/...
# Scheme algebra suite (graph-partition + fca):
wile --file test/wile/algebra-combinatorial-graph-test.scm
wile --file test/wile/algebra-fca-test.scm
```

All must stay green. Since no behavior changes, green = wording/style edits did not
regress anything.
