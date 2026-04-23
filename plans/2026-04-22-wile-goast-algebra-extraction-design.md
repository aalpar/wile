## `plans/2026-04-22-wile-goast-algebra-extraction-design.md`

# Extract pure-algebra substrate from wile-goast into `(wile algebra ...)`

**Status:** Design locked (2026-04-22). Answers: Q-a `(wile algebra symbolic)`, Q-b new `(wile algebra abstract-domain)`, **Q-c include Tier 3**, Q-d qualified names (`symbolic-boolean-normalize`), Q-e re-export from `boolean-simplify.sld`. One follow-up Q-f surfaced by Tier-3 inclusion; default selected unless user redirects.

**Motivation.** wile-goast has three small islands of pure-algebra convenience code sitting on top of wile's existing algebra libraries. Per user directive (2026-04-22 session): *all Go/SSA/AST analysis stays in wile-goast; pure-algebra operations should live in wile's algebra library so wile-goast consumes them instead of redefining.*

Scope-checked: static analysis (SSA passes, AST parsers, callgraph, belief DSL, path analysis, unify/diff, FCA context builders) stays. What moves is the algebraic substrate those passes call into.

---

## Classification of wile-goast Scheme modules

| File | LOC | Class | Action |
|---|---:|---|---|
| `boolean-simplify.scm` | 195 | **Mixed** | Extract algebra half (L1-69, ~55 LOC); leave Go projections (L71-195) in place |
| `dataflow.scm` L20-29 | 7 | Pure algebra (convenience) | Extract as `boolean-lattice` convenience |
| `dataflow.scm` L99-239 | 140 | **Generic algorithm over CFG** | **Tier 3 — deferred unless user opts in** (needs CFG-protocol design) |
| `domains.scm` L179-213 | 35 | Pure algebra | Extract `sign-lattice` + `abstract-sign` + `sign-binop` |
| `path-algebra.scm` | 44 | Go-specific wrapper | **Stays** (call-graph adjacency projection) |
| `fca-algebra.scm` | 85 | Go-specific wrapper + re-exports | **Stays** (`annotated-boundary-report` uses Go dot-notation types) |
| `dataflow.scm` rest | 92 | Go/SSA-specific | **Stays** (`ssa-all-instrs`, `make-reachability-transfer`, `defuse-reachable?`) |
| `domains.scm` rest | 331 | Go/SSA-specific | **Stays** (`make-reaching-definitions`, `make-liveness`, `make-constant-propagation`, `make-interval-analysis`) |
| `ssa-normalize`, `unify`, `fca`, `fca-recommend`, `belief*`, `split`, `utils` | ~2700 | Go-specific | **Stays** |

Net: ~100 LOC move (Tiers 1+2), or ~240 LOC (Tier 1+2+3) if `run-analysis` included.

---

## Tier 1 — Boolean normalization (resolves TODO §2.2)

**What moves.** Lines 23-69 of `boolean-simplify.scm`:
- `atom-compare` — lexicographic compare via `write` (generic helper)
- `ensure-normalizer!` / `*bool-theory*` / `*bool-normalizer*` — lazy singleton construction of a Boolean-theory normalizer on top of `(wile algebra boolean)` + `(wile algebra symbolic)`
- `boolean-normalize` — entry point `(term) → (values normal-form trace)`
- `boolean-equivalent?` — entry point `(t1 t2) → boolean`

**What stays in wile-goast.** Lines 71-195:
- `selector->symbolic` — belief DSL projection (`contains-call`, `has-params`, `name-matches`, etc.) — Go-AST analysis
- `ast-condition->symbolic` — Go AST `binary-expr`/`unary-expr`/`ident`/`lit` projection — Go-AST analysis

**Effect on §2.2.** The TODO's "~30 LOC for free-monoid substrate" estimate was to *write* a formalization; this extraction ships a working one (~55 LOC, already battle-tested in wile-goast) with an honest name. The mathematical statement: the normalizer computes a canonical form in the free Boolean algebra on S-expression atoms modulo commutativity/absorption/idempotence/involution. The "free Boolean algebra on atoms" framing is exactly what the code already does — it was just never named.

**Placement question (Q-a).** Three options:
1. Extend `(wile algebra boolean)` with `boolean-normalize` / `boolean-equivalent?`. Clean — co-locates with `powerset-boolean`. Downside: adds a symbolic-layer dependency to what was a pure-algebraic-structures library.
2. Extend `(wile algebra symbolic)` with them. Co-locates with `boolean->theory` / `make-recursive-normalizer` which are the building blocks. Downside: `symbolic` is currently theory-agnostic; adding Boolean-specific entry points breaks that symmetry.
3. New `(wile algebra boolean-normalize)` library. Clean separation but adds a library for ~55 LOC — feels heavy.

**Recommendation: Option 2 (extend `symbolic`).** Theory-agnostic normalizer lib naturally hosts *applications* of the normalizer; the entry point is really "a Boolean-theory-specific instance of `make-recursive-normalizer`". If/when lattice/Heyting/ring counterparts follow, they land next to it.

---

## Tier 2 — Convenience lattice constructors

**What moves.** Two small lattice constructors:
- `boolean-lattice` from `dataflow.scm` L20-29 (~7 LOC): bounded lattice `{#f, #t}` with `or`/`and`/implication.
- `sign-lattice` + `abstract-sign` + `sign-binop` from `domains.scm` L179-213 (~35 LOC): 5-element flat lattice `{⊥, neg, zero, pos, ⊤}` plus abstraction-from-integer and sign arithmetic table.

**Why these.** Both are pre-built abstract domains that generalize across consumers (the sign domain is a classical abstract-interpretation introductory example, not Go-specific). Both are currently callable as single-liners via `(wile algebra lattice)`'s `flat-lattice` / `make-lattice` — extracting them adds named entry points.

**Placement question (Q-b).** Two options:
1. Extend `(wile algebra lattice)` with both constructors (+sign arithmetic). Simple, co-located with other lattice constructors. `sign-binop` is a 3-valued abstract-interpretation operator, not a lattice operation — it would sit awkwardly.
2. New `(wile algebra abstract-domain)` library hosting pre-built AI domains: `boolean-lattice`, `sign-lattice`, and future domains (parity, congruence, etc.). `(wile algebra interval)` already exists as a domain-specific library — this generalizes the pattern.

**Recommendation: Option 2 (new `(wile algebra abstract-domain)`).** `sign-binop` doesn't belong in the lattice library; pattern (one library per abstract domain) is already established by `(wile algebra interval)`. New library has room to grow.

---

## Tier 3 — MFP dataflow solver (IN SCOPE per Q-c)

**What would move.** Lines 99-239 of `dataflow.scm` (~140 LOC):
- `reverse-postorder` — DFS over CFG
- `analysis-in` / `analysis-out` / `analysis-states` — result accessors
- `run-analysis` — worklist-based MFP solver, forward/backward, lattice-parameterized, with optional monotonicity check

**Why this is genuinely bigger work.** `run-analysis` consumes a CFG via named-field accessors: `(nf ssa-fn 'blocks)`, `(nf b 'index)`, `(nf b 'preds)`, `(nf b 'succs)`. The "SSA function" shape is implicit in these accessor names. Extracting requires either:
- Abstracting over a **CFG protocol** — pass in `(blocks-of fn)`, `(index-of block)`, `(preds-of block)`, `(succs-of block)` as first-class procedures (Roth-style protocol pattern, matches wile's existing `sexp-term-protocol` design).
- Or accepting a structured shape — a `<control-flow-graph>` record with defined field accessors.

Either is a real design pass, independent of the Tier 1+2 extractions. `(wile algebra combinatorial-graph)` shipped recently and has vertex/edge accessors — a CFG variant with block-level structure could extend it, or live as a sibling `(wile algebra dataflow)` library.

**Decision Q-c: include Tier 3.**

**CFG protocol design (Tier 3 implementation shape).** Mirrors existing `sexp-term-protocol` in `(wile algebra symbolic)` — a record holding the four accessor closures:

```scheme
(define-record-type <cfg-protocol>
  (make-cfg-protocol blocks-of index-of preds-of succs-of)
  cfg-protocol?
  (blocks-of cfg-protocol-blocks-of)   ; fn → list of blocks
  (index-of cfg-protocol-index-of)     ; block → identifier (eqv?-comparable)
  (preds-of cfg-protocol-preds-of)     ; block → list of identifiers
  (succs-of cfg-protocol-succs-of))    ; block → list of identifiers

(run-analysis direction lattice transfer fn protocol . args)
  ; direction : 'forward | 'backward
  ; transfer  : (block, in-state) → out-state
```

wile-goast ships an SSA adapter:

```scheme
(define (ssa-cfg-protocol)
  (make-cfg-protocol
    (lambda (fn) (or (nf fn 'blocks) '()))
    (lambda (b)  (nf b 'index))
    (lambda (b)  (or (nf b 'preds) '()))
    (lambda (b)  (or (nf b 'succs) '()))))
```

Block interior remains opaque to `run-analysis` — the transfer function is the only code that knows what's inside a block. That's what makes the solver CFG-shape-agnostic.

**Sub-question Q-f (placement for Tier 3 library).** Two options:
1. Fold into `(wile algebra abstract-domain)` — one library for all dataflow-adjacent constructs. Downside: mixes data (lattices) with algorithm (solver).
2. New `(wile algebra dataflow)` library — separates structure (lattices, domains) from algorithm (MFP solver). Matches wile's broader taxonomy (data in algebra libs, algorithms that consume them live in sibling libs — cf. `(wile algebra graph)` for semiring-parameterized Bellman-Ford).

**Default: Option 2** — new `(wile algebra dataflow)` library. The structure/algorithm distinction is load-bearing in CLAUDE.md's framing of the library hierarchy. `(wile algebra graph)` is precedent — a Bellman-Ford *algorithm* library built on `(wile algebra semiring)` *structures*. `(wile algebra dataflow)` is the MFP analog.

---

## Implementation shape (Tiers 1+2+3, per locked decisions)

Branch: `feat/algebra-from-wile-goast`. Commit plan file as commit 1.

| Phase | Action | LOC | Tests |
|---:|---|---:|---:|
| 2 | Scaffold `(wile algebra abstract-domain)` library file + .sld export declaration + registration in umbrella. Empty body initially. | ~15 | — |
| 3 | Scaffold `(wile algebra dataflow)` library file + .sld + umbrella registration. | ~15 | — |
| 4 | `(wile algebra abstract-domain)`: add `boolean-lattice` (7 LOC) + `sign-lattice` + `abstract-sign` + `sign-binop` (~35 LOC). Tests for lattice laws + sign arithmetic table. | ~42 | ~20 |
| 5 | `(wile algebra symbolic)`: add `symbolic-boolean-normalize` + `symbolic-boolean-equivalent?` + `*bool-theory*` singleton. Reuses existing `powerset-boolean` + `boolean->theory` + `make-recursive-normalizer`. Tests for commutativity/absorption/idempotence/involution. | ~55 | ~15 |
| 6 | `(wile algebra dataflow)`: `<cfg-protocol>` record + `reverse-postorder` (CFG-protocol-parameterized) + `run-analysis` MFP worklist solver + `analysis-in`/`analysis-out`/`analysis-states` accessors. Tests use a pure-Scheme alist-CFG fixture (no SSA dep). | ~140 | ~20 |
| 7 | Umbrella `(wile algebra)` re-exports all new symbols. Docs: CLAUDE.md plan index (one-line entries), `TODO.md` (close §2.2, add Tier-3 closure note), update `plans/CLAUDE.md` forward-looking table. | — | — |
| 8 | Pre-PR self-review + `make ci` + open PR with published-reference citations in body (Kildall-Kam-Ullman for MFP, Cousot-Cousot for abstract interpretation). Copilot review + `/crosscheck:crosscheck all`. | — | — |

**Phase 9 (separate PR, wile-goast repo).** After wile PR merges:
- Bump wile dep in wile-goast's `go.mod` / `go.sum` / `go.work.sum`.
- `boolean-simplify.sld`: add `(import (wile algebra symbolic))`, export re-list `boolean-normalize` / `boolean-equivalent?` (local defines become `(define boolean-normalize symbolic-boolean-normalize)` trivial aliases).
- `dataflow.sld`: add `(import (wile algebra dataflow) (wile algebra abstract-domain))`; delete `boolean-lattice`, `reverse-postorder`, `analysis-*`, `run-analysis` local defines (keep `ssa-all-instrs`, `ssa-instruction-names`, `make-reachability-transfer`, `defuse-reachable?`, `block-instrs`). Add `ssa-cfg-protocol` adapter.
- `domains.sld`: add `(import (wile algebra abstract-domain))`; delete `sign-lattice`, `abstract-sign`, `sign-binop` local defines.
- `boolean-simplify.scm`: delete L23-69 algebra substrate; keep L71-195 Go projections.
- Run wile-goast's test suite, verify all passes still green.
- Open wile-goast PR.

---

## Decision summary (locked 2026-04-22)

| # | Question | Answer |
|---|---|---|
| Q-a | Host for `boolean-normalize` / `boolean-equivalent?` | Extend `(wile algebra symbolic)` |
| Q-b | Host for `boolean-lattice` + `sign-lattice` + sign arithmetic | New `(wile algebra abstract-domain)` |
| Q-c | Tier 3 (`run-analysis` MFP solver) in scope? | **YES — in scope** |
| Q-d | Naming | Qualified (`symbolic-boolean-normalize` / `symbolic-boolean-equivalent?`) |
| Q-e | wile-goast-side | Re-export from `boolean-simplify.sld`; internal API stable |
| Q-f | Host for Tier 3 (surfaced by Q-c=YES) | **Default: new `(wile algebra dataflow)` library.** Structure/algorithm separation matches `(wile algebra graph)` = Bellman-Ford solver built on `(wile algebra semiring)` structures. Flag if user prefers folding into `(wile algebra abstract-domain)`. |

**Open authorization question.** Am I authorized to edit `../wile-goast` in Phase 9 (the sibling-repo update)? Per implementation-completion workflow: one PR per repo, wile first, then wile-goast's `go.mod` bumped. If not authorized, Phase 9 becomes a follow-up TODO entry.

---

## What this closes

- TODO Tier-A §2.2 "Free Boolean algebra on atoms" — the TODO entry gets replaced by the extraction (~55 LOC, already-working, vs ~30 LOC to-be-written).
- §5.4-5.6 pattern of "extend existing libraries in place vs new libraries" — follows the established split: pure-structure extensions go in existing libs, pre-built instances (like `(wile algebra interval)`) go in domain-specific libs.

## What this does NOT close

- Tier 3 `run-analysis` MFP solver — stays as a new TODO item (Tier A wile-goast-first or Tier C, depending on whether a wile-native consumer emerges).
- §2.2 directions-doc's broader "free-monoid / free-group / free-lattice / free-distributive-lattice" menu — only the `free-Boolean` slice is covered. The rest can still be filed as a follow-up if demand surfaces.
- The "wile-goast migration of register-renaming" and similar follow-ups from §5.4/§5.5/§5.6 — independent items.
