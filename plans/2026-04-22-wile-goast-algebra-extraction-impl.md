## `plans/2026-04-22-wile-goast-algebra-extraction-impl.md`

# Implementation plan — Extract pure algebra substrate from wile-goast

Companion to `plans/2026-04-22-wile-goast-algebra-extraction-design.md`. Scope: Tiers 1+2+3.

**Branch**: `feat/algebra-from-wile-goast`
**Commit cadence**: one commit per phase, conventional-commit style, numeric fixtures in phase-N commit messages.
**Definition of done**: `make lint && make covercheck && make ci` green; Copilot + `/crosscheck:crosscheck all` reviewed.

---

## Phase 1 — Branch + plan commit

- `git checkout -b feat/algebra-from-wile-goast` from current master.
- Commit: `docs(algebra): plan — extract wile-goast algebra substrate`. Adds design doc + this impl doc.

Verification: `git log --oneline master..HEAD` shows exactly one commit.

---

## Phase 2 — Scaffold `(wile algebra abstract-domain)`

**New files**:
- `stdlib/lib/wile/algebra/abstract-domain.sld` — library definition with empty export list.
- `stdlib/lib/wile/algebra/abstract-domain.scm` — empty body (module prelude comment only).
- `stdlib/lib/wile/algebra/abstract-domain-test.scm` — chibi-test skeleton.

**Modify**:
- `stdlib/lib/wile/algebra.sld` — import + re-export (empty list initially).

**Registration**:
- `Makefile` / embedded-stdlib manifest if discoverability is path-based. (Verify: `grep -r abstract-domain stdlib/` before writing; if stdlib uses directory-scan it's automatic.)

**Verification**: `make build && ./dist/*/wile -e '(import (wile algebra abstract-domain))'` returns no error; `make test` all pass.

Commit: `feat(algebra/abstract-domain): scaffold library`.

---

## Phase 3 — Scaffold `(wile algebra dataflow)`

Same shape as Phase 2:
- `stdlib/lib/wile/algebra/dataflow.sld`
- `stdlib/lib/wile/algebra/dataflow.scm`
- `stdlib/lib/wile/algebra/dataflow-test.scm`
- umbrella re-export wiring.

Commit: `feat(algebra/dataflow): scaffold library`.

---

## Phase 4 — `(wile algebra abstract-domain)` body

**Port from** `../wile-goast/.../dataflow.scm` L20-29 and `.../domains.scm` L179-213.

**Exports**:
| Symbol | Signature | Notes |
|---|---|---|
| `boolean-lattice` | `() → <lattice>` | Bounded `{#f, #t}` with `or`/`and`/implication |
| `sign-lattice` | `() → <lattice>` | 5-element flat `{⊥, neg, zero, pos, ⊤}`; uses `flat-lattice '(neg zero pos) eq?` |
| `abstract-sign` | `integer → symbol` | Abstraction function `n ↦ sign(n) ∈ {neg, zero, pos}` |
| `sign-binop` | `(op, sign-a, sign-b) → sign` | Sign arithmetic table for add/sub/mul with ⊥/⊤ strictness |

**Design notes**:
- `sign-binop` is the only function that's not strictly "lattice algebra" — it's a 3-valued abstract-interpretation transfer. Document this in a comment: "Not a lattice operation; sign arithmetic for abstract interpretation. Kept here as the natural complement to `sign-lattice` (cf. `(wile algebra interval)` pattern)."
- Port `atom-compare` from `boolean-simplify.scm` here too? **No** — it belongs with the normalizer in symbolic. Decision: stays in Phase 5.

**Tests** (`abstract-domain-test.scm`):
- Boolean lattice: verify `(lattice-bottom) = #f`, `(lattice-top) = #t`, `(lattice-join #f #t) = #t`, `(lattice-meet #f #t) = #f`, `(lattice-leq? #f #t) = #t`, `(lattice-leq? #t #f) = #f`.
- Sign lattice: verify 5-element membership, pairwise leq? (bottom < {neg, zero, pos} < top; neg/zero/pos mutually incomparable).
- `abstract-sign`: -5 → neg, 0 → zero, 7 → pos.
- `sign-binop`: representative cells from the arithmetic table — `(sign-binop 'add 'neg 'pos)` → top; `(sign-binop 'mul 'zero anything)` → zero; `(sign-binop 'add 'bot x)` → bot (strictness).

Target: ~20 tests, all passing.

Commit: `feat(algebra/abstract-domain): boolean + sign lattices with sign arithmetic`.

---

## Phase 5 — `(wile algebra symbolic)` extensions

**Port from** `../wile-goast/.../boolean-simplify.scm` L23-69.

**New exports** (added to `symbolic.sld`):
- `symbolic-boolean-normalize` — `(term) → (values normal-form trace)`
- `symbolic-boolean-equivalent?` — `(term1 term2) → boolean`

**Implementation shape**:
```scheme
;; Lazy singletons — construction is order of microseconds but not free.
(define *symbolic-boolean-theory* #f)
(define *symbolic-boolean-normalizer* #f)

(define (atom-compare a b)
  ;; Lexicographic compare via (write) — works for any S-expression atom.
  (let ((sa (let ((p (open-output-string))) (write a p) (get-output-string p)))
        (sb (let ((p (open-output-string))) (write b p) (get-output-string p))))
    (string<? sa sb)))

(define (ensure-symbolic-boolean-normalizer!)
  (unless *symbolic-boolean-normalizer*
    (let* ((B (powerset-boolean '(_)))     ; minimal 1-atom Boolean algebra
           (th (boolean->theory B 'or 'and 'not))
           (proto (sexp-term-protocol atom-compare))
           (norm (make-recursive-normalizer th proto)))
      (set! *symbolic-boolean-normalizer* norm)
      (set! *symbolic-boolean-theory* th))))

(define (symbolic-boolean-normalize term) ...)
(define (symbolic-boolean-equivalent? t1 t2) ...)
```

**Mathematical docstring for `symbolic-boolean-normalize`**: state that this computes a canonical representative in the free Boolean algebra on S-expression atoms, modulo commutativity, absorption, idempotence, and involution. Cite: "Closes TODO §2.2 (Free Boolean algebra on atoms) via extraction from wile-goast's `boolean-simplify.scm`."

**Why a 1-atom powerset-boolean suffices**: `boolean->theory` extracts only the *equational* laws (axioms), not the carrier. The normalizer operates syntactically; the Boolean algebra's ground-truth evaluation is never invoked. So the minimal Boolean algebra (1-atom, 2-element `{∅, {_}}`) has the same equational theory as the free Boolean algebra on any number of atoms, which is what we want.

**Tests** (`symbolic-test.scm` extension):
- Commutativity: `(symbolic-boolean-equivalent? '(and a b) '(and b a)) → #t`.
- Absorption: `(symbolic-boolean-normalize '(and x (or x y)))` → first value is `x`.
- Idempotence: `(symbolic-boolean-normalize '(or x x))` → `x`.
- Involution: `(symbolic-boolean-normalize '(not (not x)))` → `x`.
- De Morgan (if included in theory): `(symbolic-boolean-equivalent? '(not (and a b)) '(or (not a) (not b))) → #t` — **verify** this is in the theory before asserting; if not, document the limitation.
- Opaque atoms: `(symbolic-boolean-equivalent? '(and (foo x) (foo x)) '(foo x)) → #t` — non-`and/or/not` compound terms treated as atoms.
- Trace well-formedness: second return is a list of `<rewrite-step>` records; every step's `step-before` and `step-after` are S-expressions.

Target: ~15 tests.

Commit: `feat(algebra/symbolic): symbolic-boolean-normalize formalizes TODO §2.2 free Boolean algebra`.

---

## Phase 6 — `(wile algebra dataflow)` body

**Port from** `../wile-goast/.../dataflow.scm` L99-239 (`reverse-postorder`, `analysis-in`, `analysis-out`, `analysis-states`, `run-analysis`).

**New exports** (in `dataflow.sld`):
- `make-cfg-protocol`
- `cfg-protocol?`
- `cfg-protocol-blocks-of`, `cfg-protocol-index-of`, `cfg-protocol-preds-of`, `cfg-protocol-succs-of`
- `reverse-postorder` — `(blocks, protocol) → list of indices in RPO`
- `run-analysis` — new signature: `(direction lattice transfer fn protocol . optional-args)`
- `analysis-in`, `analysis-out`, `analysis-states`

**CFG protocol record** (at the top of `dataflow.scm`):
```scheme
(define-record-type <cfg-protocol>
  (make-cfg-protocol blocks-of index-of preds-of succs-of)
  cfg-protocol?
  (blocks-of cfg-protocol-blocks-of)   ; fn → list of blocks
  (index-of cfg-protocol-index-of)     ; block → identifier (eqv?-comparable)
  (preds-of cfg-protocol-preds-of)     ; block → list of identifiers
  (succs-of cfg-protocol-succs-of))    ; block → list of identifiers
```

**Signature change vs wile-goast original**:
- Old: `(run-analysis direction lattice transfer ssa-fn . args)`, uses hardcoded `(nf b 'index)` / `(nf b 'preds)` / `(nf b 'succs)`.
- New: `(run-analysis direction lattice transfer fn protocol . args)` — all field access goes through `protocol`.
- `optional-args`: same as before — `[initial-state] ['check-monotone]`.

**`reverse-postorder`**: accept `(blocks protocol)` instead of reading blocks/succs via `nf`. Internals unchanged otherwise.

**Tests** (`dataflow-test.scm`) — use a pure-Scheme CFG fixture, no SSA dep:

```scheme
;; Tiny alist-shaped CFG: a diamond.
;;   0 → {1, 2} → 3
(define test-cfg
  '((0 () (1 2))
    (1 (0) (3))
    (2 (0) (3))
    (3 (1 2) ())))

(define test-protocol
  (make-cfg-protocol
    (lambda (fn) fn)                  ; fn IS the list of blocks
    car                                ; index-of = first element
    cadr                               ; preds = second element
    caddr))                            ; succs = third element
```

Test cases:
- `reverse-postorder`: returns `'(0 1 2 3)` or `'(0 2 1 3)` (both valid RPOs for the diamond).
- `run-analysis` forward with `boolean-lattice` + identity transfer: all blocks reach fixpoint with `#f` in, `#f` out (trivial bottom).
- `run-analysis` forward with reachability transfer (in-state joined to `#t`): all blocks reach `#t` from entry.
- `run-analysis` backward with liveness-shape transfer on powerset lattice: verify def-kill semantics.
- `run-analysis` with initial state override: confirm argument parsing works.
- `run-analysis` with `check-monotone` and a non-monotone transfer: confirm monotonicity-violation error.
- `analysis-in`/`analysis-out` accessors return expected shape from a completed run.
- Linear CFG (0 → 1 → 2 → 3): RPO matches natural order; forward analysis propagates initial state through all blocks.
- Single-block CFG: RPO is singleton; both directions trivially converge.

Target: ~20 tests, all passing.

Commit: `feat(algebra/dataflow): MFP worklist solver with CFG-protocol abstraction`.

---

## Phase 7 — Umbrella + docs

**Modify** `stdlib/lib/wile/algebra.sld`:
- Re-export all new symbols from `abstract-domain`, `dataflow`, and new entries from `symbolic`.

**Modify** `plans/CLAUDE.md` forward-looking tables: add entries for the two new libraries, mark this plan as shipped (move to "Completed Plans").

**Modify** `TODO.md`:
- Close Tier-A §2.2 item (mark completed, cite this PR + `symbolic-boolean-normalize` entry point).
- Close the wile-goast-migration follow-ups that newly apply (boolean-simplify consumers; `run-analysis` consumers).

**Modify** `CLAUDE.md` if the new libraries are load-bearing enough to warrant top-level mention — default: no, libraries listed in `TODO.md` status line is sufficient.

**Modify** `plans/2026-04-17-algebra-foundations-directions.md` §2.2: annotate "shipped via extraction, see `2026-04-22-wile-goast-algebra-extraction-impl.md`".

Commit: `docs(algebra): close §2.2, register abstract-domain + dataflow libraries`.

---

## Phase 8 — PR open + review cycle

**Pre-PR verification**:
- `make lint` clean.
- `make covercheck` — no coverage regressions on wile side.
- `make ci` — all green.
- Read full diff as Copilot hat: check for drift, comment/code mismatches, invariant violations.

**PR body must cite**:
- Kildall-Kam-Ullman for MFP fixpoint theory.
- Cousot & Cousot (1977) for abstract interpretation framework context.
- The TODO §2.2 entry being closed.
- Test-count delta (symbolic: +15; abstract-domain: +20; dataflow: +20 → +55 total).

**Review**:
- Request Copilot: `gh pr edit N --add-reviewer Copilot`.
- Dispatch `/crosscheck:crosscheck all`.

**Wait for both** streams, aggregate findings into Critical / Notable-Unambiguous / Notable-Ambiguous / Clean buckets. Apply Critical + Notable-Unambiguous. For Notable-Ambiguous, propose defaults and ask user (Q-a / Q-b / … on this PR).

**Final commit**: `fix(algebra): address Copilot + crosscheck findings on PR #N`. Itemized body, source lens tagged per item.

---

## Phase 9 — wile-goast sibling PR (AUTHORIZATION GATE)

**Requires explicit user authorization** to edit `../wile-goast`. Otherwise this becomes a follow-up TODO.

Sequence:
1. Wait for wile PR to merge.
2. `cd ../wile-goast && git checkout -b feat/consume-wile-algebra-extraction`.
3. Bump wile version in `go.mod`, `go.sum`, workspace `go.work.sum` if needed.
4. Edit `.../lib/wile/goast/boolean-simplify.sld`:
   - Add `(import (wile algebra symbolic))` alongside existing imports.
   - Re-export `boolean-normalize` / `boolean-equivalent?` via `(rename (symbolic-boolean-normalize boolean-normalize) (symbolic-boolean-equivalent? boolean-equivalent?))` or via trivial `define` aliases in `boolean-simplify.scm`.
5. Edit `.../lib/wile/goast/boolean-simplify.scm`:
   - Delete L23-69 (the extracted substrate).
   - Replace with thin aliases: `(define boolean-normalize symbolic-boolean-normalize)` etc.
6. Edit `.../lib/wile/goast/dataflow.sld`:
   - Add `(import (wile algebra dataflow) (wile algebra abstract-domain))`.
   - Remove `boolean-lattice`, `run-analysis`, `analysis-in/out/states`, `reverse-postorder` from exports (they're now from wile).
   - Add new export `ssa-cfg-protocol`.
7. Edit `.../lib/wile/goast/dataflow.scm`:
   - Delete `boolean-lattice` (L20-29), `reverse-postorder` (L99-113), `analysis-*` accessors (L131-143), `run-analysis` (L145-239).
   - Add `ssa-cfg-protocol` adapter constructor (~10 LOC).
   - Modify `defuse-reachable?` to pass `(ssa-cfg-protocol)` as the new protocol argument to `run-analysis`.
8. Edit `.../lib/wile/goast/domains.sld`:
   - Add `(import (wile algebra abstract-domain))`.
   - Remove `sign-lattice` from exports.
9. Edit `.../lib/wile/goast/domains.scm`:
   - Delete `sign-lattice` (L179-181), `abstract-sign` (L183-187), `sign-binop` (L189-213).
   - `make-sign-analysis` already calls `sign-lattice` / `sign-binop` by name — these now resolve through the import.
   - Modify SSA-bound dataflow functions (`make-reaching-definitions`, `make-liveness`, `make-constant-propagation`, `make-sign-analysis`, `make-interval-analysis`) to pass `(ssa-cfg-protocol)` to `run-analysis`.
10. Run wile-goast's test suite: `make test` (or equivalent in wile-goast's Makefile). All pre-existing tests must continue to pass with identical output.
11. Open PR; same review cycle as Phase 8.

**Key risk**: the Phase-9 signature change of `run-analysis` (adding `protocol` arg) breaks all wile-goast call sites simultaneously. Every site must be updated in one commit. Use `grep -rn "run-analysis" .../lib/wile/goast/` to enumerate — currently 5 sites in `domains.scm` + 1 site in `dataflow.scm`'s `defuse-reachable?`.

---

## Overall verification checklist

Before reporting done:
- [ ] All 7 wile-side phase commits present on `feat/algebra-from-wile-goast`.
- [ ] `make lint && make covercheck && make ci` green on wile branch.
- [ ] `symbolic-boolean-normalize` on `(not (and a b))` and `(or (not a) (not b))` produces identical normal forms (or — if not in theory — docstring documents the limitation).
- [ ] `run-analysis` on alist-CFG diamond fixture produces expected forward + backward results.
- [ ] `abstract-domain`'s `sign-binop` matches wile-goast's reference table cell-for-cell (port-correctness check: diff implementations before vs after extraction).
- [ ] Umbrella `(wile algebra)` importable and exports new symbols.
- [ ] Plan index updated, TODO §2.2 closed.
- [ ] Copilot review applied; crosscheck Critical + Notable-Unambiguous applied.
- [ ] PR body cites test-count delta (+55) and published references.
- [ ] Phase 9 either executed (with explicit authorization) or filed as a follow-up TODO item.

---

## Estimated effort

| Phase | LOC | Tests | Complexity |
|---:|---:|---:|---|
| 1 | — | — | Trivial |
| 2 | 15 | 0 | Scaffold |
| 3 | 15 | 0 | Scaffold |
| 4 | 42 | 20 | Port |
| 5 | 55 | 15 | Port + math docstring |
| 6 | 140 | 20 | Port + CFG-protocol abstraction (the only non-trivial design step) |
| 7 | — | — | Doc pass |
| 8 | — | — | Review cycle |
| 9 | ~30 | 0 | Call-site updates + tests re-green |
| **Total (wile-side)** | **~270** | **+55** | |
| **Total (+ wile-goast)** | **~300** | **0 net delta** | |

Phase 6 (CFG-protocol design + port) is the load-bearing phase. Other phases are mechanical.
