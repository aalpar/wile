# Tree Edit Distance for `(wile algebra tree)` — Implementation Plan

**Status:** Ready to implement — design decisions resolved 2026-06-09.
**Motivation:** The AST-level sibling of the CFG-level MCS
(`2026-06-09-mcs-combinatorial-graph-impl.md`). Completes the structural-diff
trilogy the foundations doc maps onto `unify.scm`:
- CFG (control flow) → graph iso + MCS — **shipped**
- **AST (syntax tree) → tree edit distance — THIS PLAN**
- sequences (stmt/param lists) → LIS/RSK — `TODO.md:143`

**Parent design:** `plans/2026-04-17-algebra-foundations-directions.md` §3.4 row 5
("`unify.scm` structural diff → tree edit distance (AST)") + §3.2. Note: this is
*not* in the doc's prioritized §5.x list — it surfaced from the §3.4
named-vs-unnamed table once MCS shipped and made it the obvious next sibling.
**Consumer (per doc Part 3):** wile-goast `unify.scm` AST structural diff.
**Lands in:** new `(wile algebra tree)` library, reusing `<term-protocol>` from
`(wile algebra rewrite)` for node navigation.

---

## 1. What this delivers

A new export:

    tree-edit-distance T1 T2 proto . opts
      → (values cost mapping)
      cost    : non-negative number (minimum edit cost)
      mapping : alist ((t1-node . t2-node) ...) where
                  (a . b)  a relabeled-or-matched to b
                  (a . #f) a deleted from T1
                  (#f . b) b inserted into T2

Ordered, labeled, rooted **tree edit distance** (Zhang & Shasha 1989): the
minimum-cost sequence of node relabel / insert / delete operations transforming
T1 into T2, where child order is significant (ASTs are ordered — `(- a b)` ≠
`(- b a)`). Returns both the scalar cost and the node correspondence the diff
consumer needs.

`proto` is a `<term-protocol>` (from `(wile algebra rewrite)`): the same
abstraction `unification.scm`/`symbolic.scm` already use. A node's **label** is
`(term-get-operator proto node)` if compound else the node itself; its
**children** are `(term-get-operands proto node)` if compound else `()`. So the
same trees that flow through AC-matching flow through edit distance — no new
representation, direct interop with the wile-goast consumers.

### Design decisions (resolved)

| Axis | Decision | Rationale |
|------|----------|-----------|
| Ordered vs. unordered | **Ordered** | ASTs are ordered; Zhang-Shasha is polynomial for ordered trees. Unordered tree edit distance is NP-hard — out of scope. |
| Output | **Distance + node mapping** | The diff consumer (`unify.scm`) needs the correspondence, not just a score. Parallels MCS returning the mapping. Requires DP backtracking. |
| Node representation | **Reuse `<term-protocol>`** from `(wile algebra rewrite)` | Established codebase pattern; interoperates with `unification.scm`/`symbolic.scm`/`rewrite.scm` — the exact wile-goast consumers. No parallel protocol. |
| Cost model | **Unit costs default**, `cost` opts to override | relabel = 0 if labels equal else 1; insert = delete = 1. Custom costs (type-aware) are additive; a metric only if the override is itself a metric (documented caveat). |
| Library home | **New `(wile algebra tree)`** | ASTs are *ordered rooted labeled* trees; the `<graph>` record has no root/child-order, so they don't fit combinatorial-graph. `rewrite.scm` is rewriting, not metrics. A `tree` library parallels `combinatorial-graph` ("trees as combinatorial objects") and gives a home for future tree algorithms (LCA, tree iso, Connes-Kreimer §5.7). |

---

## 2. Algorithm (Zhang & Shasha 1989)

Classic ordered-tree edit distance. `O(n·m·min(depth,leaves)₁·min(depth,leaves)₂)`
time, polynomial — the tractable case the "ordered" decision buys.

**Preprocessing (per tree):**
- **Postorder index** every node `1..n`.
- `l(i)` = postorder index of the leftmost-leaf descendant of node `i`.
- **LR keyroots** = `{ k : no k' > k with l(k') = l(k) }` — equivalently the root
  plus every node that has a left sibling. The set of subtree roots the DP
  iterates over.

**Forest-distance DP.** For each `(i ∈ keyroots₁, j ∈ keyroots₂)` fill a forest
table `FD`; tree-distances `TD[di][dj]` are read off when both `di,dj` are the
"full subtree" case. The recurrence at `(di, dj)`:

    FD[di][dj] = min(
      FD[di-1][dj]   + delete(di),                 ;; drop di from T1
      FD[di][dj-1]   + insert(dj),                  ;; add dj to T2
      (l(di)=l(i) ∧ l(dj)=l(j))                     ;; both at subtree root?
        ? FD[di-1][dj-1] + relabel(di, dj)          ;;   match/relabel
        : FD[l(di)-1][l(dj)-1] + TD[di][dj])        ;;   compose sub-tree dist

with base row/col seeded by cumulative delete/insert. Answer = `TD[n₁][m₂]`.

**Backtracking → mapping.** Walk `FD`/`TD` from `(n₁, m₂)` choosing which of the
three branches was the minimizer at each step; a relabel/match step emits
`(di-node . dj-node)`, a delete emits `(di-node . #f)`, an insert emits
`(#f . dj-node)`. The forward DP must retain the per-cell tables (not just the
final scalar) so the walk can recover the argmin.

---

## 3. Internal layout

New files:
- `stdlib/lib/wile/algebra/tree.scm` — implementation.
- `stdlib/lib/wile/algebra/tree.sld` — `(define-library (wile algebra tree) ...)`,
  importing `(scheme base) (srfi 1) (wile algebra setoid) (wile algebra rewrite)`.
- Test `test/wile/algebra-tree-test.scm`.
- Umbrella mirror in `stdlib/lib/wile/algebra.sld` (see §8 — REQUIRED).

Private helpers (`%` prefix per `algebra/CLAUDE.md`):
- `%tree-postorder proto T` → vector of nodes in postorder + parent/child maps.
- `%tree-l proto post` → `l(i)` leftmost-leaf-descendant table.
- `%tree-keyroots l-table` → ascending keyroot index list.
- `%ted-forest-dist ...` → the FD recurrence  ← **user-authored core (§6)**.
- `%ted-backtrack ...` → reconstruct the node mapping from the tables.

Reuse from `(wile algebra rewrite)`: `term-compound?`, `term-get-operator`,
`term-get-operands`, `make-term-protocol` (for the built-in s-expression
protocol callers will most often pass). Reuse from `(wile algebra setoid)`:
`assv-or`, `validate-opts-keys`, `assert-procedure`.

---

## 4. Phases (TDD; one commit per phase)

### Phase 0 — Plan + library scaffold
- Commit 1 = this plan.
- Create `tree.sld` + `tree.scm` with a stub `tree-edit-distance` raising
  "not implemented"; wire the new library into the build.
- **Mirror the export in `algebra.sld`** now (per `algebra/CLAUDE.md` §"Export
  wiring") so `TestAlgebraUmbrellaCoversLeafExports` passes from the start.
- `make build` green.

### Phase 1 — Tree preprocessing
- `%tree-postorder`, `%tree-l`, `%tree-keyroots` over a `<term-protocol>`.
- Tests: postorder ids, `l()` values, and keyroot sets for hand-drawn trees
  (a leaf, a binary node, a left-leaning vs right-leaning tree).

### Phase 2 — Forward DP → scalar distance  ← user-authored core (§6)
- `%ted-forest-dist` (USER writes the recurrence) + the keyroot double-loop;
  `tree-edit-distance` returns the cost (mapping stubbed to `'()`).
- **Metric-property test (USER writes this, §6):** on the fixture set assert
  `d(T,T)=0`, symmetry `d(T1,T2)=d(T2,T1)`, and triangle inequality
  `d(T1,T3) ≤ d(T1,T2)+d(T2,T3)`. These hold iff the recurrence is correct under
  unit costs — a wrong min-branch breaks one of them.
- Hand-fixture sizes (see §5).

### Phase 3 — Backtracking → node mapping
- Retain the FD/TD tables; `%ted-backtrack` reconstructs the alist.
- Tests: `d(T,T)` mapping is all-matched identity (every pair labels-equal, no
  `#f`); single-relabel mapping pairs every node with exactly one differing
  label; delete/insert fixtures produce exactly one `(_ . #f)` / `(#f . _)`.
- Mapping-consistency invariant: the mapping's implied cost equals the returned
  scalar cost (sum of relabel/insert/delete costs over the mapping = `cost`).

### Phase 4 — Cost model, protocol, edge cases, docstring
- `cost` opt: `(cost . (relabel-fn insert-fn delete-fn))` or an alist; default
  unit. `validate-opts-keys`.
- Edge cases: two empty/leaf trees; `d(leaf, big-tree)` = insert-all cost;
  mismatched protocols rejected; `compare`-based vs `equal?`-based label equality.
- Full structured docstring + `(list 'fix ...)` error shape.

### Phase 5 — Docs + green build
- `docs/algebra/reference.md` entry (bespoke headings OK per `TODO.md:214`).
- `make lint && make covercheck && make ci` green.

---

## 5. Fixtures (hand-verifiable; s-expression protocol, unit costs)

Built-in sexp protocol: `(make-term-protocol pair? car cdr (lambda (t a) (cons (car t) a)) <less>)`.
Nodes: a compound `(op c1 c2 ...)` has label `op`, children `c1..`; an atom is a leaf.

| T1 | T2 | distance | edit |
|----|----|----------|------|
| `(f a b)` | `(f a b)` | 0 | identical |
| `a` | `b` | 1 | relabel a→b |
| `(f a b)` | `(f a c)` | 1 | relabel b→c |
| `(f a b)` | `(f a)` | 1 | delete b |
| `(f a)` | `(f a b)` | 1 | insert b |
| `(f (g a) b)` | `(f (g c) b)` | 1 | relabel a→c (deep) |
| `(f a b)` | `(g a b)` | 1 | relabel root f→g |
| `(f a b)` | `(f b a)` | 2 | ordered! relabel a→b + b→a (not 0) |

The last fixture is the **ordered-ness discriminator**: an *unordered* distance
would call these equal (0); ordered tree edit distance pays 2. It pins the
"ordered" design decision.

Metric checks (Phase 2): `d(T,T)=0` on all; symmetry on all pairs; triangle on
the triples formed from `{a, (f a b), (f a c), (g a b)}`.

---

## 6. User-authored pieces (the design-bearing core)

Consistent with the MCS plan — scaffolded, left as `TODO` (completable on
request):

**(a) `%ted-forest-dist` recurrence (Phase 2).** The §2 three-way min with the
keyroot/non-keyroot branch. Scaffold:

    (define (%ted-forest-dist ...)
      ;; Fill FD for the (i, j) keyroot pair. At each (di, dj):
      ;;   min of  delete(di), insert(dj), and EITHER relabel(di,dj) (when both
      ;;   are at their subtree root) OR compose-with TD[di][dj] (otherwise).
      ;; TODO(you): write the min and the keyroot/non-keyroot branch.
      ;;   Getting the branch condition (l(di)=l(i) ∧ l(dj)=l(j)) wrong yields a
      ;;   value that passes d(T,T)=0 but violates the triangle inequality.
      (error "TODO: implement the Zhang-Shasha forest-distance recurrence"))

Why you: the recurrence *is* the algorithm. The subtle part — the keyroot vs
non-keyroot branch — is where almost every from-scratch implementation goes
wrong, and the failure is silent on identity/symmetry but caught by triangle
inequality. Writing it and seeing which metric axiom breaks on a mistake is the
whole lesson.

**(b) The metric-property regression test (Phase 2).** Reference at the top of
the test file, leave the assertions as a TODO:

    ;; TODO(you): on every fixture/triple, assert
    ;;   (= 0 (tree-edit-distance T T proto))                       ; identity
    ;;   (= (ted T1 T2) (ted T2 T1))                                ; symmetry
    ;;   (<= (ted T1 T3) (+ (ted T1 T2) (ted T2 T3)))               ; triangle
    ;; Under unit costs TED is a metric; a wrong recurrence breaks one of these
    ;; even when the hand-fixture point values happen to look right.

---

## 7. Out of scope (v2 / re-open when a consumer surfaces)

- **Unordered** tree edit distance (NP-hard).
- **Full edit script** (explicit replayable op list) — the mapping is the chosen
  output; a script is a post-processing pass over it.
- **Subtree/structural hashing** for fast near-duplicate prefiltering before the
  O(n²m²)-ish DP — a clone-detection scaling concern, not a v1 need.
- **The `unify.scm` (wile-goast) consumer wiring** — wile-goast-side work; this
  plan ships the capability and the protocol interop, not the diff renderer.

---

## 8. Verification checklist (per `plans/CLAUDE.md` completion workflow)

- [ ] Branch `feat/algebra-tree-edit-distance` from `master`; plan = commit 1.
- [ ] **New-library two-file export:** `tree.sld` export AND the `algebra.sld`
      umbrella mirror, or `TestAlgebraUmbrellaCoversLeafExports` fails `make ci`
      (the coupling documented in `algebra/CLAUDE.md` §"Export wiring" — the same
      gap that bit the MCS work).
- [ ] Phases 1–5, one commit each, tests green per phase.
- [ ] `make lint && make covercheck && make ci` green locally.
- [ ] Master remote CI green before PR.
- [ ] Self-review (Copilot-hat): recurrence branch condition stated at the site;
      mapping-cost == scalar-cost invariant tested; ordered-ness fixture present.
- [ ] PR body cites the §5 fixture table + the metric-axiom checks.
- [ ] Do NOT merge without explicit instruction.
