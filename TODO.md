TODO
----

**Last Updated**: 2026-07-27 — closed items collapsed to the one-line archive form used by
`## Completed`; the three rotted `..HEAD` review ranges pinned to the commits they meant. Per-edit
history is `git log -p TODO.md`; release history is [`CHANGELOG.md`](CHANGELOG.md).

### Current Project Status

> Orientation only, and it drifts. Authoritative: [`VERSION`](VERSION) / `wile --version` and
> [`CHANGELOG.md`](CHANGELOG.md) for the release; [`docs/INDEX.md`](docs/INDEX.md) for the
> documentation map; `Engine.AvailableLibraries()` for what a build actually exposes.

**Version**: 1.17.x line
**Core Language**: R7RS-small complete — hygienic macros, composable continuations, numeric tower
**Extensions**: 12 packages under `extensions/` plus internal ones, all importable as `(wile <name>)`.
`bootstrap.ProfileExtensions` (`pkg/internal/bootstrap/bootstrap.go`) is the single source of truth
for which profile grants which. Go static analysis lives in
[wile-goast](https://github.com/aalpar/wile-goast).
**Embedding**: CLI runs on the public Engine API; embedded stdlib via `stdlib.FS`; named profiles
(`Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink`) via `WithProfile`, with `WithSandbox`
as an orthogonal modifier.
**Libraries**: R7RS-small, the chibi/SRFI set, and the `(wile algebra)` umbrella (27 sub-libraries).
### Ordering

Items ordered by perceived priority for the project's success as an embedding product. Tiers: Security/Correctness → Embedding API → Tooling/DX → Performance → Tech Debt → Deferred → Nice-to-Haves. Completed items at the bottom for reference.

### Conventions

- **Completed items** are marked `- [x]` and include `Done` (or `Done — note`) in the brackets after the difficulty estimate. Example: `[Medium, Done]` or `[Medium, Done — P3 deferred]`. The bracket marker makes completion machine-grep-able alongside the Markdown checkbox; the bracket note may carry a one-line deferral fact when a sub-item is intentionally postponed.
- **Deferred sub-items within a completed parent** are noted parenthetically in the entry body (e.g., *"Phase 10 deferred — benchmark-gated"*) rather than spawned as a separate `[ ]` entry. Re-open as a top-level entry only if the deferral becomes the active work.

### References to `plans/` and `memory/` are intentionally unresolvable here

This file is tracked; **`plans/` and `memory/` are not** — `.gitignore` excludes `/plans/`,
`/memory/`, and `*.local.md`, which hold private design reasoning. So every `plans/…` and
`memory/…` citation below is a dead link for anyone but the maintainer, and that is deliberate,
not rot. Do not "fix" them by deleting the citation, inlining the plan, or checking `plans/` in.
No CI target validates them (`make check-readme-links` and `make check-docs-orphans` do not cover
this file), which is the argument for keeping the Plan Index below current by hand.

A cited plan has one of three homes: `plans/` = live, `memory/` = shipped or abandoned, gone
entirely = moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans). Two
different `memory/` directories exist — the repo's holds archived plans, while Claude Code's
auto-memory lives outside the repo and is cited as "auto-memory `<name>.md`" with no path prefix.
Plan files use the `.local.md` suffix; a citation written as plain `.md` is almost certainly stale
by a rename.
## Plan Index

Every file in `plans/`, with the status recorded in the file itself. **A status
of "not started" on a `-design` note is not evidence the work is unstarted** —
check `memory/` for an archived `-impl` twin first; that mistake has produced
three false "open" verdicts in past triage passes.

Sections below hold the detail; this table is the map.

### Open — implementation work with a live plan

| Plan | Status |
|---|---|
| `2026-03-26-extension-contracts-impl.local.md` | Phase 1 + Phase 4 (runtime enforcement) shipped; Phases 2–3 annotation rollout **partial** |
| `2026-03-26-extension-contracts-phase2-design.local.md` | Infrastructure + enforcement complete; extension annotations partial |
| `2026-05-14-stderr-flush-on-exit.local.md` | Design locked, implementation **not started** (re-verified against source) |
| `2026-06-18-frame-reclaim-precision-coverage.local.md` | A/E/B/D + A-local **shipped**, only A-local converted to clock (`primes` −4.2%); B/D moved verdicts, emitted ~nothing; **C deferred** 2026-07-29 (the `or` lowering removed 340 macro frames more cheaply, but reaches a disjoint set — Appendix A); F/G open |
| `2026-07-15-review-2026-07-13-sec4-remediation.md` | 22/23 closed; **AU.1 audit open** |
| `2026-07-17-review-remediation-impl.md` | Confirmed at HEAD; documented guarantee and runtime disagree |
| `2026-07-18-bootstrap-core-unification-and-signals.md` | W1 shipped; remainder open |
| `2026-07-19-compile-check-and-call-site-arity.md` | Design ready, **not started** |
| `2026-07-20-scope-keyed-tier1-remediation.md` | **Partially implemented** (2026-07-21) |
| `2026-07-23-scope-set-type-impl.local.md` | Impl draft on `refactor/scope-set-type` |
| `2026-07-26-claude-local-md-accuracy-audit.local.md` | Audit complete; remediation applied 2026-07-26 |
| `2026-07-29-name-keyed-identity-residuals.local.md` | Finding report; all 4 repros re-verified on `ba93c936` 2026-07-29 |
| `2026-07-29-name-keyed-identity-residuals-design.local.md` | Design **resolved**; 2 of the report's conclusions corrected |
| `2026-07-29-name-keyed-identity-residuals-impl.local.md` | Branch A (Finding 2) **DONE** `bff525a8`; Branch B (Finding 1) not started |

### Design-only — approved or drafted, no code

| Plan | Status |
|---|---|
| `2026-04-17-mcp-server-sota-design.local.md` | Proposed, 5 phases |
| `2026-04-21-type-constraint-extension-design.local.md` | Design draft; impl deferred to follow-ups |
| `2026-06-04-srfi-204-match-design.local.md` | Design draft; `-impl` to follow |
| `2026-06-05-mcp-llm-support-design.local.md` | Phase 1 implementation-ready |
| `2026-06-09-polynomial-ideal-domain-design.local.md`<br>`2026-06-09-polynomial-ideal-domain-impl.local.md` | Design approved; impl steps unchecked |
| `2026-06-13-layered-environment-architecture.local.md` | Design note; Q3 decided |
| `2026-06-24-tinyclos-object-system-design.local.md` | Design proposal, opt-in object system |
| `2026-06-24-unboxed-scalar-arithmetic-design.md` | Design **A rejected**; Design B + Phase 4 is the live plan |
| `2026-06-26-promoted-primitive-inline-registry.local.md` | Draft v2, awaiting human review |
| `2026-06-27-srfi18-uncaught-exception-wrapper.local.md` | Design draft, gated on Q1 |
| `2026-07-10-climbing-tower-design.local.md`<br>`2026-07-10-climbing-tower-impl.local.md` | Tier 1 **approved — build now** (Tier 1 since shipped; Tier 2 deferred) |
| `2026-07-10-engine-services-generic-keyed-slot-design.local.md` | Design only, not scheduled |
| `2026-07-11-scheme-pipeline-seams-design.local.md` | `current-eval` / `current-print` / `current-read` seams |
| `2026-07-12-numeric-zero-and-tier2-fold.local.md` | Four verified defects from the 2026-07-12 review |
| `2026-07-22-free-template-id-hygiene-design.local.md`<br>`2026-07-22-free-template-id-hygiene-impl.local.md` | Shipped PR #814; retained for the D0–D3 reasoning |
| `2026-07-24-free-identifier-origin-provenance-design.local.md` | Phase 1 + inline-HOF P2 shipped; scoping doc retained |
| `2026-07-29-anonymous-lambda-inlining-impl.local.md` | Implementation-ready, ~25 LOC; Q1 (arity mismatch: refuse vs compile error) open |
| `2026-08-01-srfi18-sequencer-design.local.md` | Design proposed, 6 phases, **not started**. Scoped to adversarial protocol testing (not seed-reproducible simulation); ships in production with a FIFO policy. 3 open questions |
| `ARCHITECTURE.local.md` | 1/4 sections complete |
| `DEBUGGER.local.md` | Both proposals unstarted |
| `MACRO_SYSTEM.local.md` | Both sections unstarted |
| `GRAPH-SPECTRUM.local.md` | Directions, not scheduled |

### Parked, gated, or blocked

| Plan | Gate |
|---|---|
| `2026-04-16-recurrence-categories-design.local.md`<br>`2026-04-16-recurrence-impl-plan.local.md` | 0/5 tasks; matrix category blocked on `(wile algebra matrix)` |
| `2026-04-18-gonum-integration-directions.local.md` | Funding-gated directions |
| `2026-04-17-algebra-foundations-directions.local.md` | Funding-gated directions |
| `2026-04-20-copilot-review-data-mining.local.md` | Imminent, not started |
| `2026-04-21-wile-goast-ac-match-migration.local.md` | Stub; deferred follow-up in wile-goast |
| `2026-04-23-coverage-library-tracking.local.md` | Blocked by algebra Tier B per `WORKSPACE-ROADMAP.local.md` |
| `2026-04-23-docs-sweep-impl.local.md` | Planned, not started |
| `2026-05-02-algebra-matching-many-to-many.local.md` | Gated on `(wile algebra matroid)` (§5.7 Tier C) |
| `2026-05-05-iter-seq-cascade.local.md` | Draft; sequenced after the charsets refactor (shipped) |
| `2026-07-11-chibi-derived-ergonomics-backlog.local.md` | Parked; neither item on a critical path |
| `TECH-DEBT-2026-04.local.md`<br>`TECH-DEBT-2026-04-IMPL.local.md` | 25/27; only 6.2 (`context.TODO` in test files) open |
| `PERFORMANCE.local.md` | 1 complete, 1 open (env frame slimming), 1 rejected |
| `2026-03-25-b3-c2-c6-design.local.md` | B3/C2/C3/C4 done in wile-goast; **C5/C6 open** |

### Trackers, investigations, and technique notes

| File | Kind |
|---|---|
| `2026-07-01-staff-engineer-sweep.md` | Whole-codebase tech-debt tracker; larger M/L findings still open |
| `2026-07-17-review-remediation.md` | 2026-07-17 full-review fix plan (14 defects — resolved, see Tier 1) |
| `2026-07-17-pair-gc-investigation.md` | Read-only investigation of three cons-cell GC levers |
| `2026-07-18-scope-keyed-global-bindings-design.md` | Scope-keyed global bindings; successors transcribed into Tier 1 |
| `2026-07-10-climbing-tower-q4-mutation-boundary-note.local.md` | **Resolved 2026-07-11** — Q4 moot, footgun unconstructible |
| `2026-06-17-inverse-verification.md` | Technique note |
| `divergent-design-subcontext-workflow.local.md` (+ `.js`) | Technique note plus runnable harness |
| `WORKSPACE-ROADMAP.local.md` | Living cross-project queue (`wile` ↔ `wile-goast`) |
| `CLAUDE.local.md` | Plan-file conventions and the implementation-completion workflow |

---

## Top Priority — Triaged 2026-07-09

Promoted from a `plans/` + TODO.md open-item triage (2026-07-09) as the two
genuinely-open, high-value items — after verifying the perf-refactor backlog
against source: escape-gated frame reclaim and the layered-environment carve are
BOTH already SHIPPED (their design notes in `plans/` read "not started," but the
`memory/` impl twins are COMPLETE), leaving unboxed arithmetic as the sole real
perf lever.

- [ ] **`make planlint` — flag plan headers whose Status is stale vs reality.**
  Check each `plans/*.md` Status line against merged PRs / archived `memory/`
  twins. Motivating failure: three consecutive triage hits (escape-gated,
  layered-env, frame-reclaim) had the *impl* plan archived to `memory/` as
  COMPLETE while the *design note* left in `plans/` still read "not started," so
  the plans index inherited a false "open" verdict. Companion to the existing
  `make doclint` item (Tier 3, citation-range check). [Small]
- [ ] **Unboxed scalar/float arithmetic — kill per-op `*values.Float` heap alloc.**
  The `sumfp` loop spends ~85% CPU in GC, not compute. Verified open against
  source: the eval stack is still a boxed `values.Vector` (`pkg/machine/stack.go:25`);
  no unboxed register/cell path, no `floatVal` in vmState/`Binding`. Two designs
  in `plans/2026-06-24-unboxed-scalar-arithmetic-design.md` (Design A `[]cell`
  stack vs Design B fixed register bank); `memory/UNBOXED-FLOAT-PIPELINE.local.md`
  is the older 3-layer framing (superseded, archived) — pick one before starting. Sole remaining perf
  lever after escape-gated + layered-env verified shipped. [Large]

---

## Tier 1 — Security & Correctness

Items that block production embedded use or prevent silent state corruption.

### `(environment '(wile <profile>))` is ungated and crosses profile boundaries (2026-07-29)

- [ ] **Gate the profile-environment constructor** [High, M, filed from the `docs/` refresh]:
  `(environment '(wile kitchen-sink))` evaluated from a `Small` or `ConsoleWithLoad` engine
  returns a namespace carrying the *named* profile's extensions, not the engine's. The path is
  `PrimEnvironment` → `tryWileProfile` (`extensions/eval/prim_eval.go`) → `eval.ProfileFactory`
  (wired in `pkg/internal/bootstrap/bootstrap.go`'s `init`) → `ProfileExtensions(name)` +
  `NewProfileEnvironment`, and **no `security.Check`/`CheckWithAuthorizer` appears anywhere on
  it**. Contrast `PrimEval`/`PrimCompile`, which are gated `code:eval` in the same package.
  **What is already bounded, and why this is not a total escape:** `NewProfileEnvironment` builds
  the child via `callerNS.NewChildNamespace()`, which copies the authorizer, and `tryWileProfile`
  copies the caller's `EnvMap`. So every *gated* resource stays under the engine's authorizer, and
  `WithSandbox()`'s env restriction carries. **What is not bounded:** extensions that define no
  gate sites at all — `threads`, `gointerop`, `namespace` — become reachable from an engine that
  never registered them, and an authorizer cannot refuse what it is never asked about.
  **Aspirational behavior:** the constructor is a privileged operation and asks the authorizer
  before it builds anything. Open design questions, in the order they need answering:
  1. **Vocabulary.** Reuse `code:eval` with the profile name as target, or add a resource for
     namespace construction? `code:eval` is the closest existing fit and needs no new constants,
     but it conflates "may run new code" with "may widen the extension set", so an authorizer
     that permits `eval` under `/tmp` would still hand over `gointerop`.
  2. **Policy, not just a gate.** A gate that a permissive authorizer waves through does not fix
     the widening. The stronger rule is that a profile may only construct a namespace no wider
     than the engine's own, which needs a containment order over profiles
     (`Tiny ⊆ Console ⊆ ConsoleWithLoad ⊆ Small? ⊆ KitchenSink` is *not* currently a chain —
     `Small` is an R7RS-small baseline, not a superset of `Console`). Deriving that order from
     `ProfileExtensions` set-inclusion is mechanical; deciding whether incomparable profiles are
     refused or merely gated is not.
  3. **Compatibility.** `(environment '(wile <profile>))` widening is the documented way to get a
     richer namespace from Scheme. Tightening it is a breaking change for any embedder relying on
     it; needs a decision on whether the default is refuse-by-default with an opt-out option, or
     permit-by-default with the gate only consulted when an authorizer is installed.
  **Test-coverage trap:** `pkg/wile/engine_sandbox_test.go`'s `TestAuthorizer_DenyAllSweep`
  already concedes in a comment that the constructor is ungated, and concludes "no escalation to
  reject". That conclusion holds only for gated primitives, so the suite is green and will stay
  green while the widening is live. Any fix needs a test that asserts on an *ungated* extension's
  primitive becoming reachable, not on an authorizer denial.
  Documented as a known limitation in `docs/security/sandboxing.md` ("does NOT cover" table) as of
  the 2026-07-29 docs refresh; that row comes back out when this ships.

### Ambiguous binding references resolve silently instead of erroring (2026-07-18)

- [x] **Fixed at the cause, not by erroring** [Medium, S, Done 2026-07-21, approach 1a per
  `plans/2026-07-20-scope-keyed-tier1-remediation.md` Item 1]: the filed fix — raise on every
  `bestOf` tie — was **rejected**. It regresses ordinary R7RS nested `let-syntax` keyword shadowing,
  which currently *is* an incomparable equal-cardinality tie: `(let-syntax ((m …outer…)) (let-syntax
  ((m …inner…)) (m)))` weighs both binders 1 under a weight-2 reference, and keep-first (innermost)
  is the correct INNER result. Two further corrections to the original filing: the fix does not
  belong in `bestOf`, which takes two ints and cannot see scope sets (and whose one edit site serves
  two semantics — the local reducer compares across frames, the global one is per-frame first-wins);
  and "error on a tie" conflates shadowing with ambiguity.
  **1a — fix the cause.** `expander_let_syntax.go` bound each `let-syntax`/`letrec-syntax` keyword
  on the bare singleton `{letScope}`, discarding the keyword's accumulated enclosing scopes. It now
  binds on `slices.Clone(keywordSym.Scopes())` + `letScope` at all three coupled sites (letrec
  pre-register, compile-loop create, and the re-resolve, which must use the identical set or
  `MaybeCreateLocalBinding` keys a second slot). A nested binder is then a strict scope-set superset
  of its enclosing binder and wins by maximality — a perfect match — so the tie never forms.
  Top-level binders are unchanged.
  **Census confirms completeness**: a throwaway `WILE_TIE_CENSUS` probe on the tie branch (since
  reverted) showed the nested case producing a `weight=1 target=2` tie before 1a and none after.
  Across `r7rs-tests.scm`, `macros-test.scm`, and the ER-macro corpus there are **zero `weight>0`
  ties** — no genuine ambiguity — and every residual tie is `weight=0`, two `{}`-bindings of a name,
  which is benign shadowing resolved by frame order. So there is nothing to error on, and 1b (a real
  `ErrAmbiguousBinding` on a same-frame incomparable tie) stays deferred as the fallback if such a
  tie is ever constructed. Guard: nested and triple-nested cases in
  `TestScopeResolution_LetSyntaxShadowing`.
  **Residual, filed separately**: `resolveNodeByScopes` (`frame_reclaim_build.go`) hand-rolls the
  same argmax over a Go *map*, so its tie is non-deterministic rather than first-wins. Unreachable
  in the corpus, but it is the one site where a tie would resolve non-deterministically — a
  determinism follow-up, not gated on this item.
### Name-keyed identity survives in consumers of scope-keyed bindings (2026-07-19)

Consequences of `8afeb66a`/`a60e32e1` making one name own several slots. Each lived only inside a
`plans/` section — several marked RESOLVED — where anyone scanning for open work would skip it.
Filed for three consumers; `freeIds` and `BindingID` joined 2026-07-20 from the same Stage C
review, and the self-tail emit and `CompileSymbol` pin ordering joined 2026-07-29 from a targeted
audit — which is itself the argument against trusting a hardcoded count. **All eight are closed** as of
2026-07-29: the `lookupMacroBinding` for-syntax symmetry, filed UNPROVEN by the `CompileSymbol` fix,
was proven reachable and fixed, and the self-tail family (Finding 1) landed on
`fix/selftail-scope-identity`.

- [x] **Frame-reclaim's verdict domain was name-keyed** [Medium, M, Done 2026-07-23, branch
  `fix/framereclaim-scope-keyed-verdict`]: `ClassifyFrameReclaim` returns
  `map[validate.ScopedBindingKey]bool`, keyed on the scope-discriminated identity
  `{Sym.Key, ScopeFingerprint(scopes)}` — the SAME identity `findDuplicateSymbols` and
  `match.FreeIdKey` use, NOT the physical-slot `BindingID` the prior note correctly ruled out as the
  vehicle. Two hygiene-distinct same-name defines now carry SEPARATE verdicts, so the safe one
  recovers reclamation while the capturing one stays denied, retiring the collision-*conservative*
  forfeiture. Nodes key on `ScopedBindingKeyOf(name)` and same-unit edges resolve by subset over
  nodes, replicating `env.GetBinding` WITHOUT consulting the binding — so node creation no longer
  depends on a predeclared binding, completing the T1.5 decoupling. **Subset, not fingerprint
  equality, is load-bearing** for a let/lambda-nested cross-call whose ref scopes strictly exceed
  the callee define-name scopes: `TestFrameReclaimSeam_LetNestedMutualCallResolves` pins it
  (equality ⇒ both false; subset ⇒ both true). `resolveNodeByScopes` **refuses** an ambiguous
  maximum — two same-name nodes with equal-cardinality incomparable scope sets both subset-matching
  the ref — returning nil ⇒ unsafe rather than picking by map-iteration order, since `GetBinding`
  breaks that tie deterministically by binding creation order and this map cannot cheaply replicate
  it; a false positive would corrupt, so that is the sound direction
  (`TestResolveNodeByScopes_AmbiguousMaxRefusesToGuess`). Chose scope-set identity over `*Binding`
  pointer keying, which would have consumed a deprecated artifact.
  - [x] **Sub-hole: the verdict leaked below top level** [Correctness, S, Done 2026-07-20,
    `82046952`]: `collided` closes the same-`Key` hole for **top-level** binders, which is all
    `collectTopLevelDefines` visits — but an **internal** define can share the `Key` too, and
    reached `frameReuseForDefine` with the map still live: `CompileValidatedLet` compiles a let body
    on the *same* compiler with `p.env` swapped, the only such swap in the package (every other body
    compile builds a child continuation, where the map is nil). It collected the top-level verdict —
    a false positive, which this subsystem defines as silent state corruption rather than lost
    reclamation, and one the classifier cannot see to guard. Fixed by reading through
    `unitFrameReclaimVerdict`, which re-tests the condition the map was armed under: it can withhold
    a verdict, never grant one. Guarded over all four binder kinds
    (`pkg/wile/framereclaim_letbody_leak_test.go`). Identity keying now makes this belt-and-
    suspenders, since an internal define's name carries the enclosing scope, but the gate is
    retained as an explicit tightening.
- [x] **`MaybeCreateLocalBinding` used `ScopesCompatible` where exact equality is correct** [Medium,
  S, Done 2026-07-20]: that predicate returns true whenever the *existing* set is empty, so a
  `{m}`-scoped binder could reuse and clobber a `{}`-scoped slot — the hole the global path
  deliberately avoided by using exact scope-set equality at creation. **The bug was using a
  visibility predicate to decide identity.** The predicate now calls `scopeSetsEqual`, the global
  creation predicate *itself* rather than a copy, so the two creation paths cannot drift; lookup
  keeps `ScopesCompatible`, which is correct there, since a pre-hygiene binding with no scopes
  really is visible to every reference. **Latency confirmed, not assumed**: the obvious repro — a
  macro-introduced `(define x 42)` spliced into a body that already has a user `(define x 1)` —
  returns the hygienic `1`. The masking is not "a fresh frame per binding form" as filed: a user's
  binder in a body *carries the body's scope*, so the short-circuit never fires. Reaching it needs a
  genuinely **empty** scope set sharing a frame with a scoped binder, and the nil-passing callers
  use dedicated frames. So the guard is a unit test, not an integration test — there is no known
  Scheme program that reaches it. `TestMaybeCreateLocalBinding_EmptyScopedSlotNotReused` covers both
  nil and empty-non-nil existing sets and asserts the existing binding was not retroactively
  re-scoped; the sibling `_ScopeDistinctKeys` test cannot see this hole, since `{A}` vs `{B}` already
  returns false. Only one direction was ever broken, so a fix keyed on the *new* binder's scopes
  alone would pass a one-directional test — the test comment records this. The reuse branch's
  `Scopes` backfill was dead under exact equality (it needed an empty-but-non-nil set, where the
  write is a semantic no-op) and was **deleted rather than pinned**; it had been half the clobber
  mechanism, the reuse aliasing the slot and the backfill rewriting its identity in place.
- [x] **`DeleteBinding` was name-keyed while the namespace read surface is scope-exact** [Medium, S,
  issue #805, Done 2026-07-20]: `GlobalEnvironmentFrame.DeleteBinding` removed **every** slot a name
  owns, so `(namespace-undefine! ns 'x)` destroyed a macro-introduced `x` that `(namespace-ref ns
  'x)` reports as unbound — you could destroy a binding you cannot read. Pre-existing; before the
  read side became scope-exact the whole surface was uniformly coarse, so the diff promoted this
  from coarseness to a hole. Now `DeleteBinding(sym, scopes)` resolves through `bestSlotLocked` —
  the literal call the read makes, so delete cannot drift from ref — nils that one slot, prunes its
  index, and drops the map entry when the name owns no more (leaving dead indices would strand the
  two consumers that treat `slots[0]` as the name's representative). `matchAny` is hardcoded false,
  mirroring the two read entry points, so **delete has no wildcard mode** and `nil` here means the
  empty set, deliberately diverging from this file's nil-means-match-any convention: routing the
  delete-all operation through the nil case would make the `AmbientScopes` footgun fire
  destructively. **The cost was the tests, not the code** — the delete-all test and the
  `DeleteBinding` doc pinned the old contract by name and by comment, so both were **inverted**, not
  updated: `TestGlobalFrame_DeleteClearsMultiSlotNameOneScopeSetAtATime` keeps the multi-slot
  coverage and drops the delete-all contract, joined by scope-matched-only, macro-only-no-op, and
  `TestNamespaceUndefine_RemovesAmbientAndSparesMacroBinder` at the primitive layer; all four were
  verified discriminating by mutating `matchAny` to true. The sealed-base probe was made scope-exact
  too, reversed from an initial "no test could fail first" call under the nil-means-NONE convention:
  a wildcard probe answers "is *some* binding of this name sealed" rather than "is the binding I
  just failed to delete sealed", raising `ErrImmutableBinding` for a name the ambient read calls
  unbound. Note the effect there is a spurious *denial*, not a permission — wildcard widens the
  candidate set, and which way that lands depends on the call.

- [ ] **`namespace-undefine!` does not stop compiled code from reading the binding**
  [Correctness, S, 2026-07-20 — root cause MEASURED 2026-07-21, fix DEFERRED as disproportionate]:
  found while measuring #805, **pre-existing on master**. After `(define v 7)` `(define (get-v) v)`
  `(namespace-undefine! ns 'v)`, `namespace-bound?` correctly answers `#f` but `(get-v)` still
  returns `7`. **Measure-first done — it is the global binding cache, not a re-resolving pinned
  index.** Disassembly shows `get-v` compiles to `OpLoadCachedBinding` (`bindings: [7]`): the
  closure captures the `*Binding` **pointer** at compile time and reads `cachedBindings[i].Value()`
  directly, never consulting the slot that `DeleteBinding` nils. This holds in BOTH mutable and
  immutable top level (the cache captures a live location so `set!` still works through it). A
  bare `OpLoadGlobal` re-resolver *does* observe the nil'd slot (with a redefine after undefine,
  `get-v` errors "no such global binding"), which is why two references to the same undefined name
  can disagree. **Why deferred**: making cached reads observe deletion requires a per-read check in
  the VM's hottest opcodes (`OpLoadCachedBinding`/`OpPushCachedBinding`/`OpCallCachedBinding`) —
  there is no existing "undefined value" sentinel to reuse cheaply — a poor risk/perf trade for a
  non-standard reflective primitive, in the exact global-read path the memory repeatedly warns
  against ([[global-binding-cache-already-exists]]). The current behavior (undefine removes the
  NAME — `bound?`→#f, new references fail — while closures keep the captured LOCATION) is also
  defensible lexical-capture semantics. The `DeleteBinding` comment ("stale GlobalIndex references
  … see nil, caught by resolveGlobal") is corrected in code to note the cached-pointer path it does
  not cover. Needs a maintainer call on whether the hot-path cost is worth it before proceeding.

- [x] **Scope-set resolution let the zero value answer an unanswerable question** [Correctness +
  API, M, Phases 0–2 done 2026-07-23, `d2e2e625` + `eaee2ed0`]: the convention is **nil means
  NONE**, with "All" an explicit special value — the environment read surface did the opposite,
  reading a nil scope set as MATCH ANY. Nil is indistinguishable from an uninitialized value, so a
  caller that merely forgot to thread its scopes silently got a *wider* resolution with nothing in
  the signature to flag it. **What the wider resolution returns is the defect**: `bestSlotLocked`
  with `matchAny` does not return a union, it returns `slots[0]`, the first live slot, and slot
  order is an expansion-order artifact — `AmbientScopes`' own doc says so. So the harm is an
  arbitrary binding, not a granted permission. **Corrected 2026-07-20**: this entry previously
  called the behavior "fail-open" and a "security posture question"; both were overstated and are
  withdrawn. Nothing fails — a legal value is simply ambiguous between "unset" and "all" — and
  "open" implies permission while the effect can land either way (the sealed-base probe above widens
  the match and produces a spurious denial). No case crosses the sandbox boundary; hygiene is a
  correctness boundary, not an authorization one.
  **Fix**: `values.ScopeSet` carries three named states (All / empty / specific), collapsing the
  three symptoms of one undersized domain — `AmbientScopes()` existing only to route around the nil
  default, the `matchAny` bool riding alongside the value because the value could not carry the
  state (17 references), and `GlobalIndex.scopeKeyed` existing only because nil cannot distinguish
  "matched the empty set" from "no key at all". This was `state-trace` shaped. `GetBinding`,
  `GetLocalIndex`, `GetGlobalIndexWithScopes`, `HasLocalVariableBinding`, `ResolveBindingID`, and
  `ResolveBindingRef` all take it, so nil is un-passable at the boundary and `scopesToQueryMatchAny`
  is deleted — the footgun is gone from the whole query surface. All 58 call sites were **triaged
  per intent, not mass-rewritten**: a mechanical `nil` → `AmbientScopes()` sweep would have frozen
  today's accidents into explicit form. Scoped refs → `ScopesOf`, bare-name/introspection lookups →
  `AllScopes`, empty-scope fast paths → `EmptyScopes`.
  **Two wildcards KEPT, documented as deliberate.** `compile_syntax_form.go`'s pattern-var lookup:
  pattern vars are bound **scopeless** in the innermost `createPatternVarEnvironment` frame, so a
  wildcard returns the FIRST (innermost) match and the pattern var wins, whereas a scoped query
  switches to *maximal* resolution where an enclosing scoped lexical of the same name could outrank
  it — narrowing is a regression, not a fix. `expander_time_continuation.go` ARM 3: the scope already
  routed to the home library via `LookupLibraryEnv`, and the helper's definition scopes are not a
  subset of the use-site scopes, so a scoped query would filter it out and break cross-library helper
  resolution. Mutation-tested — narrowing both kept the suite green (ARM 3 is thin, reached mainly
  via `expand-once`; the pattern-var/lexical collision is uncovered), so the verdict rests on the
  resolution semantics, not the suite. Rationale is in a comment at each site.
- [x] **`freeIds` collapsed scope-verified answers into a name-keyed map** [Correctness, S, Done
  2026-07-20]: `collectFreeIdentifiersWithEllipsis` resolved each template identifier under its own
  scope set, then stored the result in a `map[string]*FreeIdResolution` keyed on `symVal.Key` and
  overwritten unconditionally. Two same-named template identifiers carrying different scope sets
  resolved individually and correctly, then the second silently discarded the first — the exact
  shape C1/C2 fixed on the *lookup* side, surviving one layer out in *storage*. **Reachable from
  surface Scheme, not latent**: a macro-generating macro whose generated template holds one literal
  `mh` (its own intro scope) beside a pattern var substituted with a user identifier also named `mh`
  gave `(99 99)` where hygiene demands `(1 99)`, and a distinct-name control gave `(1 99)`,
  isolating the collapse; it expands at top level, so it dodges the internal-define-syntax
  visibility limitation. **Fix**: a scope-discriminated key end to end —
  `match.FreeIdKey(name, scopes) = ScopeFingerprint(scopes) + "|" + name` — at the one writer and
  the one reader; the two verbatim-copy transfer sites needed no change, since the key is still a
  string. `ScopeFingerprint` consolidates validate's private copy. Exact-scope-set keying, not
  Flatt-maximal: this replays a per-occurrence pre-resolved answer keyed by the same immutable
  template symbol at both ends, verified reading the same field at storage and consumption. Guards:
  `TestMacroGeneratingMacro_SameNameFreeIdsDoNotCollapse`,
  `TestFreeIdKey_DiscriminatesScopeAndName` (including the `|`-delimiter unambiguity).
- [x] **`BindingID` needed a scope discriminator before the load-order plan shipped** [Correctness +
  sequencing, M, discharged in design 2026-07-23; **MOOT 2026-07-24**]: the load-order plan is
  archived as superseded, so the `BindingID{Origin, Phase, Sym, Local}` variant it defined will not
  be built. It was corrected in the spec first regardless, so it would be right by construction if
  ever built: Part III now carries `ScopeKey string` (the `ScopeFingerprint` of the definition-site
  scope set — the same basis as `validate.ScopedBindingKey` and `match.FreeIdKey`, and a string, so
  `BindingID` stays a comparable value; `""` for locals). The reason it mattered: that struct existed
  specifically to replace three disagreeing notions of "same binding" with one, and after Stage B two
  hygiene-distinct bindings produce equal name-keyed `BindingID`s — so shipping it name-keyed would
  have yielded a fourth wrong answer instead of a fix. Note the *shipped*
  `BindingID{*LocalEnvironmentFrame, slot}` is a physical local slot and is **not** this.
- [x] **The self-tail / frame-reclaim family decides self-identity by name** [Correctness, M, filed
  2026-07-29, **Done 2026-07-29**, branch `fix/selftail-scope-identity`]: measured, per repro, as
  `OpSelfTailCall` site counts **and** values (each against its distinct-name control, which was
  correct before and after):

  | repro | mechanism | before | after |
  |---|---|---|---|
  | (a) named-let arm | emit gate | 2 sites / hangs | 1 site / `300` |
  | (a) `define` arm | emit gate | 2 sites / hangs | 1 site / `300` |
  | (b) macro-hidden `set!` | shadow set | 2 sites / hangs | 0 sites / raises |

  **The `define` arm was a second LIVE repro, not "exposed but unproven".** The report attributed its
  0 armed sites to `frameReuseSelfTail`'s `IsStable()` conjunct under a mutable top level; the missing
  fact is that **immutable top level is the DEFAULT** (the layered-environment carve), so the arm fires
  and mis-emitted exactly as the named-let arm did. Same fix, no extra code (`TestSelfTailEmit_DefineArmEscape`).
  Ratchet unchanged across all three phases: primes 1/1, plain named let 1, `tak` default 1, `tak`
  explicit-immutable 1, `tak` explicit-**mutable** 0 — that last row, not the immutable one, is the
  discriminator, since the default already arms.
  **Mutation-verified**: ignoring `shadowLookup`'s subset filter fails repro (b) plus
  `TestShadowLookupRetainsOuterBinder`; restoring the emit gate's `Sym.Key` comparison fails both (a)
  cases. The tri-state's `shadowUnknown` branch in `exprMutatesName` killed **nothing** in the suite or
  corpus — recorded as a non-load-bearing tightening in the code (the file's own convention) and pinned
  by `TestExprMutatesNameReportsThroughAmbiguousTie` rather than left implied.
  **Two design corrections to the impl plan.** (1) The seven signatures do not all take `selfSym`: the
  exported entries do, and the internals take the **resolved `*Binding`**, which is what "resolve once
  per entry and thread it down" actually requires. (2) The plan justified the unresolvable-self entry
  guard by "a nil self reaching a mutation walk stops detecting `set!`, which fails open" — that is
  **wrong**, because the five walkers stay name-fed from `selfKey`, so detection is unaffected. The
  guard is kept on the other ground: an unresolvable self means the caller passed the wrong env, and
  refusing converts that plumbing error into an arming-count drop the ratchet catches.
  `LetBindingFrameReleasable` / `InternalDefineFrameReleasable` pass `self = nil` deliberately — every
  group member is in the seed, so a self call is answered by the shadow set before the identity test.
  Original filing: a **sixth**
  consumer in this class, unfiled when the other five were. `validate`'s self-tail predicates thread
  the closure's own name as a bare `string` (`self_tail.go:130` tests `name == selfName` *ahead* of
  the scoped `env.GetBinding` at `:137`) and the lexical shadow set is a `map[string]…` (`nameSet`,
  `frame_reclaim_build.go:260,587`); the emit gate compares spellings
  (`compile_call.go:64`, `frameReuse.name` from `compile_time_call_context.go:105`). The predicates
  run post-expansion, so a macro-introduced and a user identifier differ *only* in scopes. **Two live
  repros, each discriminated against a distinct-name control** (counting `OpSelfTailCall` sites):
  (a) a tail escape to a same-spelled *different* binding is emitted as `OpSelfTailCall`, so
  `machine_context.go:564` jumps to pc=0 instead of calling it — control 1 site/`300`, collision
  2 sites/**hangs**; (b) a macro-introduced local named like the loop makes `seqMutatesName` blind to
  a `set!` of the real loop, arming self-tail on a mutable name — control 0 sites/correct raise,
  collision 2 sites/**hangs**. Both fail **open**. Fix is two mechanisms, not one: self-identity
  becomes resolved-`*Binding` comparison (subset-based, so extra body scopes still resolve —
  `ScopedBindingKey` *equality* would silently total-deopt every loop, invisible to value
  assertions), and `nameSet` becomes scope-discriminated with a **subset** query per
  `environment.scopedBestOf`. `frameReuseForDefine` shares the mechanism but armed 0 sites without
  `WithImmutableTopLevel` — exposed-but-unproven, in scope. Needs an **arming-count ratchet**, not
  just value tests.
  **Scope narrowed by the design pass** (`…-design.local.md` §2, `…-impl.local.md` Branch B): the
  report reads as "migrate `selfName string` wholesale", which is wider than needed. Repro (a)'s
  corrupting site is the **emit gate alone** — the extra `OpSelfTailCall` is the escaped `(f i)`,
  reached by `compile_call.go:64`, while clause (4) correctly found only the genuine self call. That
  splits the family into three roles: emit gate = *authority* (false positive corrupts),
  `calleeCaptureSafe` = *safety* (false positive trusts a same-named capturer), the mutation/tail
  walks = *applicability* (errors are deopts once the gate is exact). So 7 signatures move and 5 keep
  `string`; `exprMutatesName` **must** stay name-only, since narrowing it by identity risks missing a
  `set!` of the real self, which fails open. `nameSet` becomes `map[string][]localBinding` with
  per-entry scopes, **not** `ScopedBindingKey`-keyed: a fingerprint cannot serve a subset query and a
  composite key forces a whole-map scan per call operator. The shadow query needs a **tri-state**,
  because `exprMutatesName` reads "shadowed" with the OPPOSITE safety polarity from
  `calleeCaptureSafe`/`classifyCallee`/`tailExprHasSelfCall` — there it *suppresses* the `set!`
  report, so one boolean cannot be conservative for both.
- [x] **`CompileSymbol`'s definition-time pin outranks the scoped *global* match** [Correctness, S,
  filed 2026-07-29, **Done 2026-07-29**, branch `fix/compilesymbol-cointroduced-global`, `bff525a8`]:
  fixed by Option A, with the recommended cardinality guard, as a global-only arm between
  `GetLocalIndex` and `tryResolvedBinding` (`GetGlobalIndexWithScopes` + `GetOwnGlobalBinding`'s
  pinned-slot read, not `GetBinding` — the latter searches locals first and a cached-binding load for
  a local would be wrong). `(entry 0)`: **0 → 21**. A second live case the report did not file, a
  co-introduced global binder coexisting with a same-named user global, went **(100 100) → (7 100)**.
  **Mutation-verified both directions**: disabling the arm fails 3 of 6 new subtests; dropping the
  cardinality guard (the naive reorder) fails `TestGlobalIndexCollision_MacroReadsLibraryBinding`,
  `_UserDefineDoesNotWriteLibraryFrame`, `_SetBangDoesNotWriteLibraryFrame` and
  `TestLibraryInternalSyntaxCaseEllipsisHygiene` — exactly the cross-library free-identifier class the
  pin protects. nil-pin census unchanged (23/0). Guards in
  `TestScopeResolution_CoIntroducedGlobalShadowsPin` (`pkg/machine/scope_resolution_test.go`).
  **Report correction**: the "user `rec` defined after the expansion" variant was filed as a separate
  path; it is the SAME defect. `predeclareBodyDefines` creates every top-level define's binding before
  any of them compiles, so in a `(begin …)`-wrapped unit the collision is visible at
  macro-definition time wherever it is written, and the `#!void` was just the un-run slot (it now
  yields `21` too). The **compilation-unit boundary** is the discriminator, not source order, so the
  real control is a two-unit split. Design + phased impl in
  `plans/2026-07-29-name-keyed-identity-residuals-{design,impl}.local.md`.
  Root cause noted but **not** fixed: `collectFreeIdentifiersWithEllipsis` calls every
  non-pattern-variable template identifier free, including ones the template BINDS. Filtering it is
  undecidable in general (a template may contain `(my-binder x …)` where `my-binder` is a
  binding macro), so it would be a partial precision improvement on top of this arm, not a
  substitute — see the design's "Option D rejected".
- [x] **`lookupMacroBinding` arm 1 has no cardinality guard (for-syntax symmetry)** [Correctness, S,
  filed 2026-07-29, **Done 2026-07-29**, branch `fix/compilesymbol-cointroduced-global`]: the
  macro-path twin of the item above. **PROVEN reachable, and the filed mechanism was right about the
  phase and wrong about the site.** `begin-for-syntax` / `define-for-syntax` / `eval-when` do NOT
  reach it: all three deliberately root the expander at `p.env` and let arm 2's `NextPhase()` do the
  climb (`compile_helpers.go:82`'s own comment says so), which is why the filed `begin-for-syntax`
  probes could not discriminate. The reaching site is a **procedural transformer body**:
  `compileAndEvalLambdaTransformer` (`compile_transformer.go:108-110`, shared by `lambda` and
  `er-macro-transformer`) calls `ExpandAndCompile` with `env.NextPhase()`, so `expand_and_compile.go`
  roots the expander at the phase-1 frame where phase-0 `define-syntax` deposits its `∅`-scoped
  keywords. Repro: a library exports `mac` over a private `helper` macro; the use site defines its own
  `define-syntax helper` and puts `(mac 1)` in a lambda transformer body. **`user-helper` → `lib-helper`**
  (`TestForSyntaxPin_TransformerBodyKeepsPinnedTemplateID`). Fixed by DEMOTING the `∅`-scoped arm-1
  match below the pin, not skipping it: arm 1 is the only arm that finds a current-phase macro (arm 2
  looks a phase *above* `p.env`), so a *drop* regresses every direct macro reference in a transformer
  body. **Mutation-verified both directions**: reverting the guard restores `user-helper`; dropping
  instead of demoting makes both the collision case and the ambient-macro control fail to expand.
  Both arms now share `coIntroducedByExpansion`, which answers this item's open question — the shared
  helper is warranted now that both call sites are proven, and it gives the cardinality argument one
  home. Original filing, kept because the reachability audit it records is still the useful part: arm 1
  (`expander_time_continuation.go:355`) is a scope-precise `p.env.GetBinding` ahead of the D2 pin with
  **no** `len(binding.Scopes()) > 0` guard, so an ambient `∅`-scoped syntax binding reachable from
  `p.env` would capture a pinned free template identifier. Reachability, as far as it was established:
  all three `BindingTypeSyntax` creation sites write to an **Expand-phase** global
  (`compile_define_syntax.go:91`, `expander_body.go:172`) or a **local** with `keywordScopes`
  (`expander_let_syntax.go:146,215`), so **no `∅`-scoped syntax global lands in phase 0** and
  phase-0 expansion is safe — confirmed empirically, the `guard-aux` hijack vector returns
  `(caught boom)` with and without the colliding top-level `define-syntax`. But under **for-syntax**
  expansion (`compile_define_for_syntax.go:97`, `compile_eval_when.go:194`, `compile_helpers.go:82`)
  `p.env` is a **phase-1** frame, which is exactly where a phase-0 `define-syntax` deposits
  `∅`-scoped syntax globals, so the mechanism is present there. **No repro constructed**: the `guard`
  vector inside `begin-for-syntax` fails identically with and without the collision, for the
  unrelated reason that `raise` aborts to the exit prompt at expand time.

### Scope-keyed globals — successor work not built in the arc (2026-07-19)

Surfaced by the Stage C adversarial review and deliberately left out of scope so the arc could
land. Previously visible only inside a plan's "do not build here" section, which is not where open
work is looked for. All four closed.

- [x] **Internal `define-syntax` was invisible in a shorthand-`define` body** [Correctness, S, Done
  2026-07-21]: **broader than the filed two-symptom framing** — *any* internal `define-syntax` in a
  `(define (f) …)` body was invisible to later body forms, even a plain non-generating one, while
  `(let () …)`, `(lambda () …)`, and named-`let` bodies all worked. `expandDefineForm`'s
  function-shorthand branch expanded its body via `ExpandSyntaxArgumentList`, a flat argument-list
  pass with no letrec* pre-scan, no `define-syntax` registration, and no body scope, whereas
  `expandLambdaForm` runs the body through `ExpandBodyWithDefineSyntax` under a fresh body scope.
  Two consequences: internal macros were never registered, *and* body identifiers carried no scope,
  violating `CompileSymbol`'s fast-path invariant, so the use fell through to a global lookup and
  errored. Fixed by extracting `expandProcedureBody` and routing the shorthand body through it —
  form preserved, not desugared, so the compiler keeps its self-tail and frame-reclaim
  optimizations (verified: 5M-iteration tail loop, mutual recursion, generated shorthand-defines).
  Guards: `TestInternalDefineSyntax_InShorthandDefineBody`, `_LambdaLetParityUnbroken`, RED before /
  GREEN after over 6 shorthand cases.
- [x] **A library-local `define-syntax` did not shadow a same-named imported macro** [Correctness,
  M, Done 2026-07-21]: **broader than the export-only framing** — the imported macro won not just in
  `findLibraryBinding` but in the library's **own body** too, while the variable analogue shadowed
  correctly. `copyLibraryBindingsDirect` installed *every* imported binding into the library's
  RUNTIME frame, and syntax bindings *additionally* into Expand; the runtime mirror at ambient `{}`
  then shadowed the library's own `define-syntax`, which stores into Expand at `{libScope}`. Fixed
  by installing a syntax binding into Expand **only** — variables still install into runtime, where
  a local define already shadows the import through the shared frame. Applied to **both** install
  paths; the second was surfaced by the post-fix `/crosscheck`, since the first pass fixed only the
  library path and left top-level import-then-redefine still shadowed. Guard:
  `TestLibraryLocalMacroShadowsImportedMacro` (export, library-body, top-level, renamed-import,
  plain-imported-still-usable). **Known residual**, below the fix's bar: because syntax and
  variables now land in different frames, a cross-*kind* same-name clash across two imported
  libraries (macro `foo` vs variable `foo`) is no longer conflict-detected per R7RS §5.6; same-kind
  clashes still are.
- [x] **No sealed base above phase 0** [Correctness, M, RESOLVED 2026-07-22 on
  `feat/free-template-id-hygiene`, PR #814]: the reported library-import vector did **not**
  reproduce — importing a library that rebinds `guard-aux` leaves `(guard …)` intact. What did
  reproduce is a **top-level** `(define-syntax guard-aux …)` compromising `guard`. `guard`'s
  template freely references `guard-aux`, both are ambient `{}`-scoped bootstrap macros, so they
  share a slot and the user's transformer wins: a free template identifier that should resolve to
  its definition-site binding does not. Fixed by the D0–D3 arc rather than the originally-deferred
  "architectural decision". A per-namespace `sealedExpandBase` (phase-1 sealed frame parented on
  `sealedBase`) holds bootstrap macros and special-form expanders; D3 was **retargeted** there after
  routing it into the phase-0 frame regressed the general `Dialect.Forms().Remove()` contract, which
  settled the design's own open question — the phase-0 value frame is not collision-free for
  compile-time handlers. D0 reordered sibling helpers (`guard-aux` above `guard`) so their pins are
  non-nil at macro-definition time, dropping the census 47 → 44 exactly, the 3 helper edges. D2
  threads the `SyntaxSymbol` into `lookupMacroBinding` and consults the pin *after* arm 1 (local
  let-syntax) and *before* arm 2, mirroring the value path's R1 ordering.
  A 5-lens `/crosscheck` then caught two gaps the single-clause repro masked: the nil-pin census was
  **vacuous**, because its discriminator used the composite `FreeIdKey` so the bare-name lookup
  always missed; and a recursive helper's **self**-reference was still capturable, since a reorder
  cannot pin a self-reference. Fixed by `pinTemplateSelfReferences`, which back-patches after
  `define-syntax` creates the binding (preserving create-after-compile, so a failed compile leaves no
  binding) and closes the class uniformly for `and`/`or`/`cond`/`case`/`do`/`define-values`. The
  census is now a mutation-verified ratchet — it flags 21 recursive self-refs without the back-patch
  and 0 with (44 → 23 nil-pins) — and an unpinned expand-bound sibling reference is a **defect**
  rather than merely unchecked. **Successor (2026-08-03):** the seal it built is exactly ONE level
  deep — phase 2 has none, so a transformer body's own `define-syntax` climbs off the sealed axis.
  Pinned by `TestSealedClimbStopsAboveExpand`; see "Sealed axis keyed by `(phase, kind)`" in Tier 5.
- [x] **CHANGELOG documents scope-keyed global storage** [Docs, S, Done 2026-07-21]: covers the
  `8afeb66a…4f73936d` arc's user-visible semantics — a macro-generating macro expanded twice now
  gets two binders instead of sharing one, template-introduced library exports are rejected eagerly,
  and `namespace-undefine!` deletes one scope-matched slot instead of every slot a name owns.
  `docs/environment/system.md` Invariant 5 was already current.
### plans/ sweep — correctness deltas not previously in TODO (2026-07-21)

Correctness work that lived only inside plan files, invisible to a TODO scan.

- [x] **stderr data can be lost on exit** [Correctness, S, RESOLVED 2026-07-21 — no longer
  reproduces]: verified against the built binary — no textual output is lost on any exit path.
  `writeAndFlush` (`pkg/extensions/io/prim_write.go`) flushes the port after **every**
  `write`/`display`/`newline`/`write-char`/`write-string`, so the bufio buffer never accumulates
  across writes (5000 writes then `(exit 7)`, a single 100 000-char `display`, and
  error-port-then-exit all arrive in full). This per-write flushing postdates the 2026-05-14 plan,
  which explicitly said "no per-message flushing is added", and supersedes its flush-on-exit design:
  implementing that design now would add redundant machinery for a data loss that no longer occurs.
  The plan was incomplete regardless — it targeted only `cmd/wile/main.go`'s `os.Exit` sites and
  missed `(exit)`/`(emergency-exit)` in `extensions/system/prim_system.go`, the bug's own canonical
  repro. `plans/2026-05-14-stderr-flush-on-exit.local.md` is an archive candidate.
- [x] **SRFI-18 `thread-join!` wraps an uncaught exception** [Correctness/conformance, S, Done
  2026-07-21]: implemented with Q1 = A (wrap unconditionally — strict SRFI-18; zero external
  consumers, break freely). A joined thread that died on an uncaught exception now surfaces to the
  joiner as an `uncaught-exception` whose `uncaught-exception-reason` is the original condition,
  where previously the bare condition was re-raised; reason identity is preserved (`eq?`). Added
  `values.UncaughtException`, `werr.ErrNotAnUncaughtException`, and the two predicates. Guard:
  `TestThreadJoinWrapsUncaughtException` (6 cases including identity and sentinel rejection).
- [x] **SRFI-18 exception predicates + `thread-state`** [Correctness/conformance, S, Done
  2026-08-02]: `join-timeout-exception?`, `terminated-thread-exception?`, and
  `abandoned-mutex-exception?` were the last three procedures missing from SRFI-18's index.
  The *conditions* already fired; they reached Scheme as generic error-objects carrying only a
  string, so a joiner could catch them but not tell them apart. `values.JoinTimeoutException`,
  `TerminatedThreadException`, and `AbandonedMutexException` are now `values.Value` as well as
  `error` (opaque handles, pointer identity, mirroring `UncaughtException`), and
  `thread-join!` / `mutex-lock!` hand them to `machine.RaiseInPlace` instead of returning them.
  Two behavior changes fall out: joining a `thread-terminate!`d thread now raises
  terminated-thread-exception rather than uncaught-exception (spec: separate conditions), and
  none of the three is an `error-object?` any more. Also added `thread-state` — **not** SRFI-18
  (that spec has `mutex-state` only); the name follows Gambit, and the stale
  `pkg/values/thread.go` comment claiming SRFI-18 specifies it is corrected. Guards:
  `TestSRFI18ExceptionPredicatesDiscriminate` (8 cases), `TestThreadState` (5 cases).
  **Residual, not fixed:** `(current-thread)` still returns the symbol `'primordial` on the main
  goroutine rather than a thread object, so `(thread-state (current-thread))` errors there.
  Pre-existing, and its own SRFI-18 gap.

> **The load-order plan is dead (archived 2026-07-24).** Part II's motivation is discharged: the C6
> capture is fixed by the bootstrap reorder (`1af62cd2`) plus the free-template-id-hygiene arc
> (PR #814), and `TestBootstrapMacrosPinLateBoundReferents` is a CI ratchet reporting 0 capturable
> nil pins — the regression guard Part II's cure would have provided. Part III is obsoleted as
> written: origin-based identity was **rejected** for import conflict (PR #793) in favor of by-name
> `sameImportedBinding`, so migrating all three sites onto `BindingID` would regress shipped
> behavior. One narrow residual survives, below.

- [~] **`free-identifier=?` and ER-compare were non-conformant on COMPLEMENTARY cases**
  [Correctness/conformance, M, verified 2026-07-24 vs Racket + Chez, low impact; **conformance FIXED**
  (`70a34421` + `36e1d268`), Phase-2 consumers remain]: `free-identifier=?` compared bindings by
  pointer, wrong on rename-aliases — two rename-imports of ONE binding answered "different" where
  Racket and Chez say same. `erBindingsEqual` added a `BindingType()`+`Value()` fallback, wrong the
  other way — two DISTINCT defines of the same value answered "same". **So pointing
  `free-identifier=?` at ER-compare was not the fix**; it swaps one wrong answer for the other.
  Neither pointer-eq nor value-eq is the correct notion. The conformant one is same binding
  **origin**, which Wile stored nowhere: imports copy the value into a fresh cell and a fresh
  `*Binding`, and `markBindingImported` recorded only a boolean.
  **Fixed** by folding each imported binding's root `OriginRef{RootLib,RootName}` at import (keyed on
  `internalName`, no traversal) and comparing via `environment.SameBinding`. ER-compare was pulled
  from Phase 1 and finished as option B: switching it to `SameBinding` *alone* regressed
  internal-vs-import compare `#t`→`#f`, because a definition-site rename had nil Origin while the
  import had one. `stampLibraryExportOrigins` closes that by giving every library export its own
  self-root at library **finalization** — chosen over an import-side-effect stamp so Origin stays a
  pure function of the definition and imports never mutate library-internal state — and the value
  fallback is deleted. Note the origin approach was rejected for import-*conflict* detection
  (PR #793) because it false-flags a legal define-over-import shadow; that trade-off does **not**
  transfer to identifier *equality*, where origin is the correct semantics. Design:
  `plans/2026-07-24-free-identifier-origin-provenance-design.local.md` (model: "same binding" = same
  root in the provenance graph Wile already walks at import, then discards).
  **Phase 2 — 1 of 3 shipped:**
  - [x] `stampImportedInlineHOF` gates on `Origin.RootLib` — Done 2026-07-24 (`9b2afa8c`). Fixed the
    latent re-export miss and a coupled pre-existing miscompile the crosscheck surfaced: inline
    dispatch selected the template by the CALL-SITE name, so a curated HOF renamed onto another
    curated HOF's name inlined the wrong body. Dispatch now keys on the stamped
    `BindingMeta.InlineHOFName`, reset on every re-import so a conflation cannot strand a stale
    template.
  - [~] `,doc` follow `Origin` instead of eager-copying `Doc` at every import hop. **The defect this
    would have incidentally fixed was found and fixed on its own 2026-07-25, and needed no origin.**
    Repro: two libraries exporting one macro name, the first documented and the second not — under
    the §5.6 by-name conflation the second won the value while `,doc` kept reporting the first's
    docstring. The cause was not the copying but the `if doc != ""` guard on it, which made the field
    un-clearable: the same staleness class as the inline-HOF stamp reset three lines above it, which
    the doc field was never added to. Unconditional assignment (`library_bindings.go:127`) is smaller
    than the guarded copy it replaced, and the guard protected nothing (full suite green without it).
    Guard: `TestImportedMacroDocTracksTheWinningValue`, verified RED first. Procedures were always
    immune — a closure carries its docstring on its template, so it tracks its value for free.
    **What remains is structural-only and BLOCKED as filed**: `Binding.Doc()` lives in
    `pkg/environment`, but resolving `OriginRef` needs `LibraryRegistry.Lookup` + `findLibraryBinding`
    in `pkg/machine/compilation`, three layers up; `OriginRef.RootLib` is a `string` key, not a
    `LibraryName`, so even from above it needs a reverse lookup or a scan; and `repl/` is
    deliberately decoupled from `machine/compilation`. So the options are re-couple, inject a
    resolver into `environment`, or store a `*Binding` (rejected by D2). With no defect left
    motivating it, **re-justify before starting, or drop it.**
  - [ ] Site 3 `sameImportedBinding` (`library_bindings.go:571`) still compares `*MachineClosure`/
    `*ForeignClosure` by NAME with an `EqualTo` default. **Gated, not merely unstarted** — its own
    doc comment frames the by-name conflation as a deliberate irreducible gap, and origin was
    rejected here by PR #793 for false-flagging define-over-import. The gate is re-reading #793's
    actual objection: an import-edge origin is a different signal than the source location it
    rejected (a define-over-import shadow is a non-imported local with nil `Origin`, so it never
    enters an import-vs-import root comparison) — a hypothesis to verify, not a claim.
### Resolved Tier-1 defects, 2026-07-14 → 2026-07-21

Closed. One line each so the fix stays findable; detail is in the cited commits and archived plans.

- [x] **Opaque-subtree over-marking loosened the immutable-top-level check** (2026-07-16,
  `57973333` + `3cce6754`): `forEachRawSymbol` marked template *data* no unquote can reach. That
  cost the `Stable` stamp — and top-level immutability rides on that stamp — so it silently turned
  immutability off for any name a template mentioned; cross-unit, ``(begin (define x 1) `(x))`` then
  `(set! x 2)` compiled where a plain `define` was rejected. It now threads quasiquote depth and
  marks only evaluated positions, matching the compiler's own `quasiquoteNeedsRuntime`/`expandQuasi`
  walk — agreement with that walk is the soundness argument, since it decides what is live and this
  one only predicts it. The filed premise ("two consumers want opposite error directions from the
  same data") was **false**: both enforcement sites key on the same `Stable` flag, so an over-mark
  withdraws the optimization and both enforcements together, and a second map would have bought
  nothing. Three shapes decide correctness, all pinned: dotted unquote (a bare unquote in the
  **spine**, which keyword dispatch cannot see), `quote` as a barrier at depth 0 **only** (nested
  unquotes stay live inside a template per R7RS §4.2.6 — the one mistake that fails silently), and
  nested depth. Guard: `TestImmutableTopLevel_OpaqueSubtreeOverMark`.
- [x] **`Value` Go-comparability is now a stated, enforced contract** (2026-07-14,
  `fix/value-comparability-contract`): the rule that actually decides it — **the receiver, not the
  underlying type** — is on the `Value` doc comment (`Vector` is `[]Value` and is safe because its
  methods take pointer receivers), enforced by `reflect.TypeOf(v).Comparable()` over rosters, since
  Go comparability has no method set and cannot be asserted at compile time. **Three violators, not
  the two audited**: `reflect` found `machine.boxedValuesType`, the only one reachable from Scheme
  (`OperationBoxValues` puts it in the value register, so `dynamic-wind` gets there) — it became
  pointer-shaped. `Operations` and `MultipleValues` are **no longer `values.Value`**; neither is a
  Scheme datum, and having an equality method is not the same as being a `Value`. A live host-crash
  was fixed on the way: `equalWorklist.step` compared `a == b` *before* establishing both sides were
  `DeepEqualer`s, so two same-typed non-comparable leaves meeting as components panicked — the
  regression test merged that same day paired a leaf against a `*Pair`, differing dynamic types,
  which Go answers without faulting, so the hazard sat in the one shape that does. A
  `SchemeComparable` interface was **rejected**: it is the intuitive fix and the wrong one, giving
  the offenders a *supported* way to be non-comparable and ratifying the free ride. Identity may not
  be delegated to a method — R7RS §6.1 defines `eq?`/`eqv?` on aggregates as denoting the same
  location, and `eq?` is the finest equivalence in the lattice, so a type computing its own identity
  can lie and `eq? ⊆ eqv? ⊆ equal?` stops being structurally guaranteed.
- [x] **`eqv?`/`equal?` numeric-lattice nonconformances (F1/F2/F3)** (2026-07-14, `c302b702`):
  `EqvNumber` (`pkg/values/eqv.go`) is the single authority the three sites consume. Two exact
  numbers now compare across representations (`(eqv? (+ 1/2 1/2) 1)` ⇒ `#t`, which the tower also
  fixes upstream by canonicalizing denom-1 results to `*Integer`), exactness contagion is corrected
  (F2), and NaN is reflexive (F3, matching Chez) — deliberately finer than the literal pool's
  `literalIdentical`. F4/F5 documented as conformant divergences.
- [x] **Macro-introduced top-level binders; `define-values` under NoMutation** (2026-07-14,
  `d594beeb`; **mechanism superseded 2026-07-18 by `a60e32e1`**): the crypto-random rename pass no
  longer exists — binders are separated by scope-keyed global storage instead. `define-values` was
  rewritten `set!`-free with a template temporary, so it works under the immutable top level, across
  compilation units, and under NoMutation (a definition, not a mutation — R7RS §5.3.3). Outcome
  unchanged and `TestNoMutationKeepsDefineValues` still guards it.
- [x] **General form-removal `*PrimitiveExpander` leak** (2026-07-14,
  `fix/form-removal-expander-leak`): a **user** macro whose template referenced a *removed* form
  leaked that form's expand-phase expander into runtime — on a NoMutation engine, a `set!`-using
  template applied `#<primitive-expander:set!>` instead of failing with `ErrNoSuchBinding`. Root
  cause: `fr.Remove` drops only the compiler `FormSpec`, so the `PrimitiveExpander` survives and gets
  pinned onto the introduced identifier. Fixed by a `compileTimeHandler` marker on
  `namedHandlerBase`, so `tryResolvedBinding` falls through to `ErrNoSuchBinding` — the documented
  removed-form contract — for *any* removed form carrying an expander, not just `set!`. Guard:
  `TestNoMutationRemovedFormInMacroTemplateIsUnbound`.
- [x] **R7RS library export supersets + the `(description)` declaration** (2026-07-14, `cc3c48bb`):
  documented as deliberate deviations rather than deleted, since removing exports is a user-visible
  API break. Pinned by `TestLibraryExportSupersets`, which imports each binding through
  `(only (scheme …) id)` — so narrowing any library back to the strict R7RS surface fails the test
  and forces a deliberate doc update instead of a silent break.
- [x] **`GlobalIndex` literal identity must include `Env`** (2026-07-14, `fa9804d6`): a literal-pool
  collision — two distinct globals with the same `Index` symbol but different `Env` deduped to one
  slot. `EqualTo` now compares `Env`.
- [x] **2026-07-17 full-review remediation — all 14 confirmed defects** (2026-07-21; source
  `reviews/2026-07-17/REVIEW.md`, plan `plans/2026-07-17-review-remediation.md`): twelve landed
  across six file-disjoint PRs (#808–#813), two earlier via `1af62cd2`. Covered the SRFI-18
  condition-variable lost wakeup, a symlink-following resolver, ungated `(command-line)` argv, an
  uncatchable port-type panic plus a dead recover, case-lambda params unbound in body, `unless`
  referential transparency, `%parameter-raw-set!` surviving NoMutation, write/write-shared skipping
  hashtable interiors, `(())` mislocating its error, RunSimple dropping a final unterminated line,
  the `\x…;` hex-escape digit cap (R7RS 7.1.1), and two broken example/benchmark paths. **The CI gap
  that hid two of them is closed** — `make test-examples` is now a CI stage; CI had never run the
  shipped `.scm` tree. Two findings did not survive triage: #5 (`-specific-set!` under NoMutation)
  was refuted as a documented design nit, and #10 (SRFI-13 comparison return) was **no deviation at
  all** — SRFI-13 specifies boolean and Wile is conformant, so the mistaken doc entry was removed
  rather than added. Copilot caught two issues beyond the original findings: a create-time
  parent-symlink escape still open in the #809 fix (closed in `10cdbf56`) and a `ReadLine` `n>0`+EOF
  byte-drop in the #810 fix (closed in `2c72c925`).
### Continuation multiple-values follow-ups (from PR #800 crosscheck, 2026-06-25)

Value-count behavior is documented in `docs/reference/r7rs-differences.md` → "Continuation
Value-Count".

- [x] **`dynamic-wind` preserves multiple values from its thunk**: box/unbox the thunk result so
  0/1/N values occupy exactly one eval-stack slot (`OperationBoxValues`/`OperationUnboxValues` in
  `CompileValidatedDynamicWind`).
- [x] **`procedure-arity` reports continuations as `(0 . #f)`**: both `*ComposableContinuation` and
  the newly handled `*CapturedContinuation`, matching their `AcceptsArity` and Racket's
  arity-at-least-0.
- [x] **Single-value resumption contexts splice rather than raise** — investigated 2026-06-25,
  **NOT pursued**: strictness needs a value-count check on the `RestoreContinuation` hot path plus a
  compile-time single/any classification, and it breaks `(wile control)` variadic resumption and
  pervasive normal-return splices — all to enforce behavior R7RS leaves unspecified, where the
  current splice already conforms. If ever needed, do it as an opt-in `WithStrictValueArity` engine
  option, not a default change. Rationale:
  `memory/2026-06-25-continuation-arity-strictness-design.local.md`.

### Trampoline continuation invocation to bound Go-stack growth — SHIPPED 2026-06-28

- [x] **Unified reification + winding-aware resume ("the flip")** [Performance/Correctness, L, Done
  on `feat/continuation-resume-trampoline`]: `applyCapturedContinuation` used to run the resumed
  computation in a *nested* sub-context and abort to the prompt, so a continuation-heavy program
  accumulated live Go frames across its dynamic extent — `ctak(18,12,6)` peaked ~40k and the Gabriel
  warmup + 10-iteration loop ~525k, against Go's ~675k fatal overflow. It now emits
  `ErrResumeContinuation` and the resume runs on the driver (`RunResumable`/`ReinstallSegment`) —
  O(1) Go frames. Consequently `maxContinuationDepth` and its `contNestDepth` tracking were
  **retired** (the resource they guarded no longer exists), along with the `-race` ctak skip and
  `pkg/wile/raceflag_*_test.go`, so `TestDeepConvergingContinuationConverges` runs under `-race`
  again. A post-landing A/B `/crosscheck` found and fixed one escalation regression: a sticky
  context-global `isolatedMarks` swallowed R7RS §6.11 secondary exceptions after any resume, fixed
  path-precisely with a `resumeGeneration` counter.
  **Four falsified attempts preceded it, and why they failed is the durable part.** A
  resume-side-only trampoline is *provably impossible*: the resume needs a chain-resident boundary
  to place itself, and reinstall-at-nearest breaks escape-past (an outer continuation invoked to
  escape past a `call-with-values` producer double-executes the consumer) while abort-to-top breaks
  guard. Reifying one boundary while the others stay sub-contexts regresses nested guard, because
  the inner construct runs inside the outer guard's producer sub-context, off the main chain where
  `FindPrompt` cannot reach it — so the reification is **atomic** across all six boundaries. And
  reification ⟺ winding-aware resume are themselves one atomic change: implementing the full cluster
  alone produced 4 CRITICALs and a red `make ci`. Two lessons worth keeping: `go test ./...` does
  **not** run `control-test.scm`/`exceptions-test.scm`, so `make ci` is the gate; and the suite was
  **blind** to the escape-past regression class until
  `pkg/registry/core/continuation_escape_past_oracle_test.go` was committed — proven non-blind by
  running it against the falsified attempt, where the call-with-values row returns
  `CONSUMER-WRONGLY-RAN` and the oracle fails on a change that had shipped `make ci`-green. Designs
  and per-attempt findings: `memory/2026-06-2*` (resume-aware-prompt-catches, coupled-fix,
  cluster-reification-impl).

### Restricted-profile `(scheme base)` export-validation — RESOLVED by-design 2026-06-29

- [x] **Not a supported combination; the strict eager validation stays** (#801 closed by-design): a
  profile that does not register base's primitives makes `(scheme base)` an invalid library in that
  configuration, including under `(only …)` — correct R7RS §5.6 enforcement, not a defect. The error
  is the **capability boundary asserting itself**: `Tiny` is a sandbox choice (which primitives are
  *exposed*), orthogonal to the language-standard axis, and subset-importing `(scheme base)` under a
  sandbox is not the mechanism for "I want a smaller standard". The "yes" path has near-zero value —
  the names that do resolve under such a subset (`car`, `cons`) are core primitives already bound
  with no import at all — and disproportionate cost: tolerating profile-gated primitives inverts the
  `machine/compilation`→`registry/` layering, and deny-stubs pollute every namespace. The shipped
  diagnostic (`43d7d085`) names both causes, so the failure is actionable; the contract is documented
  in `docs/embedding/source-loading.md`. **Follow-on, distinct axis**: the legitimate "start as R5RS
  or R6RS" need is a *language-standard* selector, not a security profile. Today `(scheme r5rs)` only
  layers R5RS names over the full R7RS core, so there is no non-R7RS baseline; a first-class
  `WithDialect` startup point is designed but unstarted, tracked under the Dialect System in
  `plans/ARCHITECTURE.local.md`. **Note**: `WithStrictNamespace()` sidesteps the friction for profiles
  whose extensions *are* registered, but does not resolve #801, which concerns genuinely-`Tiny`
  profiles where the primitives are registered nowhere.
### Layered-environment carve regressions (review `d8911c15..b04c6d74`, 2026-06-15)

Sealed-base carve + immutable-top-level-default arc. Two root patterns: own-frame `Keys()`
iterators that did not span the sealed base, and the immutable default reaching contexts the
design meant to stay mutable. **Scope decision (2026-06-15):** immutability is scoped to
**compilation units only** (Chez model) — immutable for files and `-e` batches, which preserves
the frame-reclaim GC win; mutable for every interactive/eval context (REPL, `--mcp`,
`(environment …)`, `scheme-report-environment`). Implemented as a root-namespace property, with
child namespaces always mutable and the `set!`-gate keying on `IsStable()` directly so anchors
stay protected inside mutable children. Design:
`plans/2026-06-13-immutable-toplevel-by-default-scoping.local.md`. 14/15 closed; D1 survives.

- [x] **A1–A4 — read paths dropped every sealed-base name** [Done 2026-06-15]: post-carve
  primitives and bootstrap procedures live in the sealed base rather than a phase entry, so
  own-frame `Keys()` walks lost `caar`, `map`, `zero?`, `call/cc` and the 28 cxr accessors.
  `collectBindingNames` (REPL completion) and `searchEnvironmentBindings` (`,apropos`) now walk
  `SealedBase()` too, and the completer test was strengthened to assert a sealed-base-only name —
  it had asserted only `car`, which survives via the expand phase and so masked the bug.
  `namespace-undefine!` on a sealed name now raises `ErrImmutableBinding` instead of silently
  reporting success while the name stayed bound (`TestNamespaceUndefineSealedRejected`); user
  shadows are still removable.
- [x] **B1–B4 — the immutable default reached entry points meant to stay mutable**
  [Done 2026-06-15]: each REPL line is its own unit, so a first `(define x 1)` was stamped
  `Stable` and a later `(define x 2)` rejected — verified on the built binary. The CLI now adds
  `WithMutableTopLevel()` when entering the REPL, `mcp.go` is always mutable, `runEval` begin-wraps
  `-e` into one unit like `runFile` (the two had diverged on redefine), and child namespaces are
  mutable so `(eval '(define zz 1) e)` twice works (`TestSealedBase_B3_*`). B4 closed as
  **documentation**: `internal/bootstrap` staying mutable while public `NewEngine` is immutable is
  mechanism-vs-policy separation, not a split brain — the immutable default is a product policy
  applied by the public Engine, and `internal/` is not an embedder API.
- [x] **C1–C3 — two divergent `Stable`-stamping mechanisms** [Done 2026-06-15]: in a profile child
  `(set! car …)` was permitted while `(set! caar …)` was rejected — the opposite of the engine root,
  where both are rejected. Profile children are now mutable, so bootstrap procedures go unstamped
  and the reclaim classifier is not asked to trust them there (`TestSealedBase_C1_*`), and the
  `set!`-gate keys on `IsStable()` directly, decoupled from `ImmutableTopLevel()`, so a Stable
  anchor is never `set!`-able even in a mutable child. C2 closed as a **doc fix**: freezing
  bootstrap procedures in the compiled program is intentional (they are anchors), and narrowing the
  stamp would lose the user-recursion GC win, so `docs/reference/r7rs-differences.md` was corrected
  rather than the code.
- [x] **D2 — lock asymmetry on a thread-shared global** [Low, M, pre-existing, Done 2026-07-01,
  `fbcd7654`]: `Value()`/`SetValue()` were unsynchronized while `set!` writes locked the frame
  mutex. Global bindings now publish through an `atomicCell` (atomic publish, lock-free load), with
  the `noCopy` `atomic.Pointer` in the heap cell so `Binding` stays copylocks-clean for the
  value-embedded local frame; locals keep the plain field, paying no atomic op on the hot Apply
  arg-bind path. Every global-frame entry point establishes the cell, so "in a global frame ⇒ has a
  cell" is structural. `binding_race_test.go` is RED under `-race` pre-fix. **Cost: +4.6% geomean on
  bench-gabriel, 15/16 slower** (`Binding` 32→40B inflates local slabs; global reads gain two
  pointer hops) — accepted, correctness over performance. The shrink-`Binding` recovery lever is a
  Tier 4 follow-up.
- [x] **E1–E3 — altitude, duplication, and a per-import re-walk** [Done 2026-06-15]: the carve left
  `TopLevel()` returning the sealed base while 8 production sites migrated to
  `.Namespace().Runtime()`, each carrying the same explanatory comment — the next contributor
  reaching for `TopLevel()` would silently get the frozen base. `EnvironmentFrame.MutableRuntime()`
  replaces all 8 (deliberately not `Runtime()`, which diverges for library frames), with a unit test
  pinning the distinction. `loadBootstrapSources` existed twice and had **behaviorally diverged** —
  one path optimized templates and pooled contexts, the other did not, so bootstrap procedures
  loaded via the internal path ran un-optimized; `compilation.LoadBootstrapSources` is now the single
  pipeline and `wireRuntimeFrames` the single source of truth for the two-frame topology.
  `registerSchemeDocstrings` re-parsed ~500 root docstrings on **every** `(import …)` (a library env
  shares the root namespace) and is now guarded by `!env.IsNamespaceRuntime()`.

- [ ] **D1 — sub-context/thread capture can leak library-eval defines** [Low, S — DEFERRED 2026-06-15, arguably-not-a-bug]: `machine/machine_context_subcontext.go` now captures `MutableRuntime()` (named) which returns the engine-root mutable global even from a library frame. For SRFI-18 THREADS this is correct by design (threads share the engine global). The only edge is a sub-context (`load`/`call-with-exit`) spawned *during a library's own load* landing defines in the engine global rather than the library frame — an extreme, untested case. Revisit only if a concrete isolation bug surfaces; not worth a speculative fix.

---

- [x] **Data race: error/backtrace capture vs concurrent VM mutation under `thread-terminate!`**
  [High, M, Done 2026-06-13]: pre-existing on master, and **not surfaced by `make ci`**, which does
  not run `-race` on the threads package. When one SRFI-18 thread terminated another mid-execution,
  the terminator's stack-trace walk read the victim's `mc.template`/`mc.pc`/continuation chain while
  the victim's own `Run` loop wrote them — no happens-before edge, so a torn read could yield a
  corrupt backtrace or a nil-deref in `SourceAt`. **Root cause narrower than "terminate doesn't
  quiesce"**: `NewThreadSubContext` set the thread's `parentMC` to the *live* spawning context. That
  link is for *synchronous* sub-contexts, where the parent is paused on the same goroutine; a
  thread's parent runs concurrently, so every `parentMC` walk (`CaptureStackTrace`,
  `findParameterInMarks`, the pool release counter) crossed the goroutine boundary. An earlier fix
  had snapshotted the parent's *fields* at spawn but left the *pointer* to be dereferenced later.
  Fixed by severing `parentMC` for thread contexts — a thread is an independent root, not a
  sub-context — one change covering all three walks. Trade-off: cross-thread dynamic-parameter
  inheritance is dropped, since it was a racy live read rather than a creation-time snapshot and was
  not SRFI-18-correct anyway. Same family as PR #561, which removed `NoCopyApply` as unsafe under
  concurrent invocation; that covered concurrent *apply*, this is the concurrent *error/terminate*
  path and was never recorded.
- [x] **Five recursion-depth bounds, counted identically** [Medium, Done]: the VM's
  `DefaultMaxCallDepth` was joined by `DefaultMaxParseDepth`, `DefaultMaxExpandDepth` (50000,
  configurable via `WithMaxExpandDepth`), `DefaultMaxWriteDepth` (10000), and that same write bound
  reused for `Value.SchemeString`. All count root = 1, +1 per container descent, so write and read
  trip on exactly the same structures — the guiding invariant is *anything the writer emits must be
  valid on read*. **Length ≠ depth was the recurring bug**: the writer's two analysis passes and
  `SchemeString` each recursed once per cdr-spine element while the output pass already iterated it,
  so a *flat* list of any length — nesting depth 1, perfectly re-readable — overflowed the host
  stack. Both now walk the spine iteratively and recurse only into cars/elements. Path-scoped
  *cycle* detection does not bound *depth*: the guarantees are orthogonal, since an acyclic chain
  never re-hits a marked node. The expander guard is shared **by pointer** across child expanders,
  unlike the parser's one-object-per-parse. `SchemeString` diverges on failure semantics — its
  signature is the `Value` interface contract and cannot raise, so it degrades to a distinct
  `#<deep>` marker (vs the cycle marker `...`). No `WithMaxWriteDepth` engine option: the writer has
  no engine-owned entry point, being reached only through the io primitives.
- [x] **Parser fuzz targets + reader crash-safety hardening** [Medium, Done]: the repo's first Go
  native fuzz targets — `FuzzReadSyntax` (untrusted input must never panic; every non-EOF error is a
  located `*ParserError`) and `FuzzReadWriteRoundTrip` — found **8 pre-existing reader bugs in ~2
  minutes, 5 of them host panics**, all fixed with committed corpus under
  `pkg/parser/testdata/fuzz/`. The example-based tests had enforced the contract only on inputs
  someone thought to write down. Two classes: leaked foreign error types (`*tokenizer.TokenizerError`,
  `*strconv.NumError`), closed as a CLASS by a boundary catch-all `locateReaderErr`; and unguarded
  nil-at-delimiter derefs (`' )`, `#\<NUL>`, `#e)`, `#b0/0`, `#0=(#d)`). The round-trip target also
  caught a real conformance bug — `String.SchemeString` used Go `%q` instead of R7RS `\xHH;` and
  mnemonics. **Deferred**: the numeric external-representation tail — `#m` big floats write without
  their prefix, so an in-range bigfloat loses its type on read. The other half is closed: scientific
  notation whose magnitude overflows float64 now promotes to `BigFloat` across both the reader and
  `string->number`.
- [x] **Unify complex/imaginary number parsing** [Medium, Done, staff-sweep #5]: the reader and
  `string->number` implemented the same rectangular-complex grammar twice and had **already drifted
  into two different wrong answers** on `+3/4i` — the reader rejected it, `string->number` accepted
  it as inexact `0.0+0.75i`, and R7RS §6.2.5 makes it exact `0+3/4i`. The pure-imaginary path gated
  its exact branch on `isIntegerString`, so a rational coefficient fell through to the inexact
  parser, while the reader's twin used a bare `strconv.ParseFloat` that rejected `3/4` outright.
  Fixed the shared grammar (`isExactPartString`) and made the reader **delegate** to the pure
  functions, adding only its source-located error. Guard:
  `TestParseNumber_ReaderAgreesWithStringParsers`, which pins the single-source-of-truth invariant.
- [x] **Stable-matching selectors failed; matching tests did not gate CI** [High, M, Done]: one bug,
  two symptoms. `walk-for-cycle` stored each rotation cycle newest-first because an extra `reverse`
  undid the ordering the cons-accumulation already produced, while `apply-rotation` reads a cycle as
  "proposer mᵢ → receiver of m_{i+1}", which holds only oldest-first. A 2-cycle is its own inverse,
  so the 2×2 fixtures masked it; length-≥3 cycles ran the rotation backwards, collapsing M_top
  straight to M_bot and hiding every interior stable matching. **The CI-gate gap is the more
  important half**: 12 chibi-test files lacked `(test-exit)` and so reported failures while exiting
  0, and `sat-test.scm` used a custom harness that printed "FAIL:" and exited 0. All now gated.
- [x] **Audit `PrimitiveSpec` `ReturnType`/`ParamTypes` annotations** [High, L, complete]: the
  four-axis framework (docs ↔ annotation ↔ implementation ↔ R7RS) is closed. Findings: the
  declared-too-narrow bucket is **empty** (3 false positives confirmed); declared-too-wide is
  dominated by ~85 TypeConstraint-vocabulary gaps; ~25 candidates are R7RS sub-domain refinements
  ("exact non-negative integer", "byte in [0,255]") below `ValueType` granularity. Wile-specific
  primitives with no entry in any adopted standard need a **local spec written before they can be
  audited** — without a spec there is nothing to drift from. This becomes load-bearing the moment
  Extension API contracts ship compile-time checking, since unsound annotations then turn into
  wrongly-rejected programs and the R7RS-compliance claim starts depending on evidence rather than
  assertion. Next work is vocabulary extension, separately scoped.
- [x] **Silent failures in `compilation/operation_syntax_case.go`** [Medium-High, Done, PR #732]:
  four error-handling defects, pre-existing. `matcher.Match` errors were swallowed wholesale on the
  premise that "match failed = normal control flow", collapsing context cancellation, malformed
  input, and ellipsis-depth violations into "no matching clause" — the `nolint:nilerr` comment was
  the smoking gun, since the linter had detected exactly this hazard. Now gated on
  `errors.Is(err, match.ErrNotAMatch)`. The bind loop discarded `MaybeCreateLocalBinding`'s error and
  fell through to `SetLocalValue(li, nil)` with no diagnostic, conflating three branches (creation
  failed / already bound outer / value missing for a declared pattern var); they are now separated.
  The `SyntaxCaseState()` assertions collapsed "field nil" and "type mismatch" into one message,
  which matters because the marker-interface revert means a wrong type *can* be stored without
  compile-time rejection. Error messages gained input and source-location context.
- [x] **Exceptions and error stack traces** [Medium, Done, PR #657]: `SourcedError` in
  `compilation/`; `CompileExpression` wraps errors with source context and `CompilationError.Source`
  is populated from the cause chain. Datum-level functions operate on `values.Value` without syntax
  context, so callers wrap. Foreign stack-trace entries for Native → Foreign → Native callback
  crossings (P3) remain deferred — `memory/2026-04-14-error-stack-traces-design.local.md` §P3.
- [x] **`read` mid-parse EOF raises a read-error instead of returning EOF** [Done]: `(read "(foo")`
  returned `#!eof`. `wrapMidParseEOF` converts `io.EOF` to a `ParserError` wrapping
  `io.ErrUnexpectedEOF` at all four mid-parse sites; the primitives needed no change, since their
  existing `errors.Is(err, io.EOF)` check correctly rejects the new error and falls through to
  `WrapForeignReadErrorf`, producing a condition that maps to `NativeErrorKindRead` so
  `(read-error? e)` is `#t`.
- [x] **Error type identity** [Medium, Determined]: `CompilationError` and `RuntimeError` are
  **public boundary types**, translating internal errors to the embedder API; they should NOT
  implement `SchemeError` or `ForeignError`. Embedders match them with `errors.As`.
- [x] **vmState field coverage test** [High, S]: reflection-based, enumerating `vmState` fields and
  asserting each appears in a coverage table keyed by operation, so adding a field without handling
  it fails the build rather than silently corrupting state.
- [x] **MCP eval fails on schelog `include`** [Not a bug]: the report was missing `puzzle.scm` and
  `(set! *schelog-use-occurs-check?* #t)`; without the occurs check the puzzle infinite-loops into
  the MCP timeout.

---
## Tier 2 — Embedding API & Product Value

The embedding experience that differentiates Wile.

- [ ] **Extension API contracts Phase 2+** [Embedding, High]: Compile-time (compiler consults `ParamTypes` for static call sites — error before execution, zero runtime cost) and runtime (`buildValidator` wires `ParamTypes` → `SetValidator`). Integration with linter. Prerequisite vocabulary-extension design at `plans/2026-04-21-type-constraint-extension-design.local.md` (Julia-subset nominal lattice, `OpaqueTypeConstraint`, `Subtype` as primary operation; excludes refinement and union types per invertibility/no-duplication principles). Original parent: `memory/2026-03-26-extension-contracts-design.local.md`
- [x] **Environment profiles** [Embedding, Done]: Named profiles (Tiny, Console, ConsoleWithLoad, Small, KitchenSink) via `WithProfile`; orthogonal `WithSandbox` modifier; virtual env map (`WithEnv`, `WithEnvMap`); Scheme-level `(environment '(wile <profile>))` support; `SafeExtensions`/`AllExtensions` removed. `memory/2026-03-26-environment-profiles-impl.local.md`
- [x] **Eager documentation index** [Tooling, Done]: Shipped as lazy-build-and-cache rather than eager scan. `LibraryExportIndex` is built on first `apropos`/`doc` query and cached on `Namespace`; Scheme-level `(apropos)`, REPL `,apropos`, and MCP share the same index, so LLMs can discover unloaded-library procedures from the first query. See PRs #623–625 (`memory/LIBRARY-EXPORT-INDEX.local.md`) and post-#623 asymmetry fix (`memory/PRIM-APROPOS-EXPORT-INDEX.local.md`). Original eager-scan design (`2026-04-08-eager-doc-index-design.md`) was superseded before any code shipped.
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.local.md`
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.
- [ ] **MCP triggering rewrite (Lever A)** [Embedding, Text-only]: Rewrite `cmd/wile/mcp.go` `WithInstructions`, 9 tool descriptions, and `prompts/wile-scheme.md` to trigger LLM tool use on algebra/modular/polynomial domains. Correct misleading `libraries` description (currently claims "loaded only" but tool returns full catalog). Validation via `algebra-accuracy` benchmark: closes `powerset_lattice` regression. No code logic changes. `memory/2026-04-18-mcp-triggering-rewrite.local.md`

### Algebra & Analytics Roadmap

Directions documents — identify prioritized capability extensions. Priority sequence per 2026-04-22 decision: **wile-goast-first** (Tier A — named consumers in wile-goast analysis code, giving wile-goast a complete algebraic palette without digressions into wile), **matching-second** (Tier B — Roth-Sotomayor two-sided matching), then **§5.7 lower-priority** (Tier C).

- [x] **Algebra library roadmap** [Directions]: `plans/2026-04-17-algebra-foundations-directions.local.md` identifies 6 prioritized directions extending `(wile algebra ...)`. §5.1 `(wile algebra matrix)` shipped via Path D (PRs #684–#691, #695, #696). §5.2 Möbius / incidence algebra — shipped (commit `4ff8a314`, `memory/2026-04-21-incidence-algebra-impl.local.md`). §5.3 AC-matching shipped via `(wile algebra unification)`, `memory/2026-04-21-ac-matching-impl.local.md` (Phase 6 closeout). §5.4 Group actions & Burnside shipped as extension of `(wile algebra group)` (`memory/2026-04-22-group-actions-burnside-impl.local.md`). §5.5 Distributive/modular lattices + Birkhoff shipped as extension of `(wile algebra lattice)` (`memory/2026-04-22-lattice-birkhoff-impl.local.md`). §5.6–§5.7 broken out as individual items below.
- [ ] **Benchmark statistics (gonum)** [Directions]: `plans/2026-04-18-gonum-integration-directions.local.md` §5.2 identifies a benchmark-statistics gap in wile. Ships `bench-stats/` ~100–150 LOC; pure Go, no CGo, one `go.mod` entry. Independent track from the companion wile-goast `goastgraph/` work (see wile-goast TODO). Distinct algebraic setting from `(wile algebra matrix)`: gonum is field-valued (ℝ/ℂ), not semiring-parameterized.

#### Tier A — wile-goast-first (named consumers in Appendix A)

- [x] **§5.4 Group actions & Burnside** [Algebra, wile-goast, High]: Shipped as extension of `(wile algebra group)` in place (D1 — not a new `(wile algebra group-action)` library). Extends `<group>` record with optional metadata (element?, setoid, order, elements, generators); adds `<group-action>` record, BFS-from-generators `orbit`/`stabilizer`/`fixed-points`, `orbit-representative` with documented tie-breaker, `burnside-count` with divisibility validation, presets (`trivial`/`cyclic`/`symmetric`/`product` groups; `trivial`/`permutation`/`regular`/`conjugation`/`product` actions). 124 tests; end-to-end verified via Burnside on conjugation-action of S_3 = 3 conjugacy classes. Available for wile-goast migration of register-renaming (`goastssa/prim_canonicalize.go`), binop commutativity (`ssa-normalize.scm` `ssa-rule-commutative`), and `boolean-simplify.scm`. Plan: `memory/2026-04-22-group-actions-burnside-impl.local.md`.
- [x] **§5.5 Distributive/modular lattice + Birkhoff** [Algebra, wile-goast, Matching, High]: Shipped as extension of `(wile algebra lattice)` in place (not a new library). Extends `<lattice>` record with three optional metadata fields (setoid, cardinality, elements); ships `distributive?` / `modular?` exhaustive axiom-check predicates + sample-based `validate-*[/setoid]` siblings; `join-irreducibles` / `meet-irreducibles` via lower/upper cover counting; `birkhoff-representation` / `birkhoff-reconstruction` roundtrip with smart O(|downsets(P)|) enumerator; `lattice->locally-finite-poset` projection; five presets (chain, boolean, diamond/M3, pentagon/N5, free-distributive). Also extends `<locally-finite-poset>` with optional `elements` field + `lf-poset-elements` accessor. Dedekind numbers verified through D(5) = 7581 (~1.5s). 155 tests. Available for wile-goast migration of `dataflow.scm` `run-analysis` MOP=MFP certification and `domains.scm` precision annotations. Plan: `memory/2026-04-22-lattice-birkhoff-impl.local.md`.
- [x] **§5.6 Combinatorial graph** [Algebra, wile-goast]: Shipped as new `(wile algebra combinatorial-graph)` — distinct from `graph.sld` (which remains as semiring-Bellman-Ford). 1-WL color refinement + individualization-refinement backtracking for complete graph isomorphism (Weisfeiler–Leman 1968; McKay–Piperno 2014), spanning-tree count via deletion-contraction with fast paths (Cayley, C_n, tree, empty), chromatic and Tutte polynomials via deletion-contraction (Read 1968, Tutte 1954) with |V|+|E|≤20 size cap, Hopcroft-Karp O(E·√V) bipartite matching, six preset fixtures (K_n, C_n, P_n, K_{m,n}, empty, Petersen). Setoid-carried vertex equality, tier-1/tier-2/tier-3 finiteness per §5.4 pattern. 225 tests including Petersen backtracking-correctness canary and C_6 vs 2K_3 cospectral non-iso canary. `memory/2026-04-22-combinatorial-graph-impl.local.md`.
- [x] **Balanced graph partition (`graph-partition`)** [Algebra, wile-goast]: Shipped as an extension of `(wile algebra combinatorial-graph)` — a two-way *balanced* cut (NOT a global min-cut, which degenerates to isolating one vertex) via Kernighan-Lin pair-swaps. Holds the seed's A/B ratio; `balance` bounds the seed ratio; returns `group-a`/`group-b`/`cut-weight`/`sizes`/`normalized-cut`. The s–t and global min-cut family (Ford-Fulkerson, Dinic, Karger, Stoer-Wagner) was rejected with rationale; single-vertex FM was tried and dropped (deadlocks from a balanced seed under a tight tolerance). 4 test groups (K(3,3) optimal-cut + star degeneracy-guard canaries). Motivating consumer is wile-goast `recommend_split`, which currently mislabels a heuristic as "min-cut". **Phase 2** (wile-goast): build the import-signature affinity graph, rewire `find-split` to call `graph-partition`, recalibrate confidence off `normalized-cut`, fix the docstrings — separate plan in the wile-goast repo. **Phase 3** (deferred): Shi-Malik normalized-cut `'method`, gated on the gonum eigensolver. Design+impl: `memory/2026-06-08-balanced-graph-partition-design.local.md`, `-impl.md`.
- [x] **§2.2 Free Boolean algebra on atoms** [Algebra, wile-goast, Done]: Shipped via extraction from wile-goast's `boolean-simplify.scm` L23-69. Named entry points `symbolic-boolean-normalize` / `symbolic-boolean-equivalent?` in `(wile algebra symbolic)` — normalize under `boolean->theory`. **Axiom completion (2026-06-09)**: De Morgan, complement laws (x ∧ ¬x ⇒ ⊥), and bound identities are now applied. `boolean->theory` was re-architected from 11 pairwise axioms to 7, replacing pairwise commutativity+associativity+idempotence with a single AC-normalization axiom (flatten → sort → dedup → fold). This fixed a **pre-existing** associativity+commutativity non-termination (≥3-leaf AC terms fuel-exhausted) and made complement detection n-way (`a ∧ b ∧ ¬a ⇒ ⊥`). Not a decision procedure (no distributivity — use `(wile algebra sat)`). Follow-up: the same AC fix is available for semiring/ring/field `+` (same latent bug, not migrated this task). `memory/2026-06-09-free-boolean-axiom-completion.local.md`. Also shipped Tier 2+3 of the same extraction plan: `(wile algebra abstract-domain)` with `sign-lattice` + `abstract-sign` + `sign-binop`; `(wile algebra dataflow)` with `<cfg-protocol>` record + `run-analysis` MFP worklist solver + `reverse-postorder` + `analysis-in/out/states`. `memory/2026-04-22-wile-goast-algebra-extraction-design.local.md` + `-impl.md`.
- [x] **SAT solver** [Algebra, Done]: `(wile algebra sat)` ships `sat?`, `sat-cnf?`, `sat-model`, `boolean-decide-sat?`, `boolean-decide-equivalent?`. CDCL kernel in `extensions/sat/` (watched-literal propagation, 1-UIP analysis, VSIDS, Luby restarts). Closes De Morgan, complement-law, distributivity, bound-identity gaps in `symbolic-boolean-equivalent?`. `memory/2026-05-30-sat-solver-design.local.md`, `-impl.md`.
- [x] **CFL-reachability path algebra** [Algebra, wile-goast, Done]: **Shipped** — `(wile algebra cfl)`: general CFG engine (typed production kernels `cfl-epsilon`/`-terminal`/`-unary`/`-binary`, normalized by construction) + labeled-edge graph + Reps–Horwitz–Sagiv worklist solver (`cfl-solve` + `cfl-reachable?`/`-from`/`-pairs`/`cfl-derives?`) + `dyck-grammar` preset for interprocedural/field-sensitive analysis + validators. Context-sensitivity canary proves it is strictly more precise than Boolean (semiring) reachability. Design+impl: `memory/2026-06-05-cfl-reachability-design.local.md`, `-impl.md`. Original entry: New `(wile algebra cfl)` (or a `semiring.scm` extension) for context-free-language reachability — the path-algebra generalization where edge labels compose under a context-free grammar rather than a free semigroup. **The single open wile-side algebra gap with a named, current consumer**: wile-goast TODO C4 ("CFL-reachability — context-sensitive analysis") tags it explicitly as a *wile-side gap*. It is **not** expressible through the existing semiring API — the composition rule is grammar-constrained, not associative/free, so it cannot be parameterized from `semiring.scm`'s `(plus, times, zero, one)` shape (this is why C4's Boolean/tropical sub-items shipped but this one stalled). Algorithm: Reps–Horwitz–Sagiv (1995) "Precise interprocedural dataflow analysis via graph reachability" — worklist over (node, grammar-symbol) pairs. **Demand-audit note (2026-06-05)**: a wile ↔ wile-goast TODO cross-check found this is the *only* algebra item with a real downstream consumer; the entire §5.7 Tier C menu below (matroids, Hopf, submodular, RSK, category extensions, partitions) currently has **no** wile-goast consumer, despite the "wile-goast-first" priority principle. Scope: design doc first (grammar representation, productive-cycle termination, demand vs exhaustive evaluation), then `-impl.md`. Consumer: wile-goast interprocedural/field-sensitive context-sensitive analysis.

#### Tier B — Two-sided matching (Roth-Sotomayor)

- [x] **`(wile algebra matching)` library** [Algebra, Matching, Done]: Two-sided matching per Roth & Sotomayor (1990). Gale-Shapley deferred acceptance (proposer + receiver optimal), hospital/intern many-to-one via Roth's reduction, Conway distributive lattice on stable matchings via Birkhoff (load-tests §5.5), Irving rotations enumeration, egalitarian + sex-equal selectors. Many-to-many (Kelso-Crawford) deferred to follow-up gated on §5.7 matroids (`plans/2026-05-02-algebra-matching-many-to-many.local.md`). `memory/2026-05-02-algebra-matching-design.local.md`, `memory/2026-05-02-algebra-matching-impl.local.md`.
- [x] **§4.2 Tropical permanent / Hungarian primitive** [Algebra, Matching, Done]: `tropical-assignment` shipped in `(wile algebra matching)` — Kuhn-Munkres O(n³) Jonker-Volgenant 1987 form. Returns `(matching . cost)`. Forbidden pairs via `+inf.0`. Unequal sides via padding. Sanity-checked on a 4×4 textbook fixture against brute-force optimum.
- [ ] **§4.2 Maximum common subgraph** [Algebra, Matching]: True code clone detection — bipartite matching between candidate node pairs, branch-and-bound with assignment relaxation. Overlaps §5.6 combinatorial-graph. `plans/2026-04-17-algebra-foundations-directions.local.md` §4.2.

#### Tier C — §5.7 lower priority

> **Demand note (2026-06-05):** a wile ↔ wile-goast TODO cross-check found **none** of the items below currently have a named wile-goast consumer — the "consumers" cited in each entry (register allocation, `ast-transform` formalization, etc.) appear nowhere in wile-goast's actual TODO. These are completeness-driven, not demand-driven. **CFL-reachability — the one item that had a real wile-goast consumer — has since shipped (`(wile algebra cfl)`)**, so there is currently **no** demand-justified open algebra item. Re-validate demand against `wile-goast/TODO.md` before promoting any item here.

- [ ] **§5.7 Matroids** [Algebra, Low]: `(wile algebra matroid)` — rank function, circuits, duality, Tutte polynomial, matroid intersection. ~300 LOC. Blocks Kelso-Crawford substitutes for many-to-many matching; also unlocks matroid-intersection framing of register allocation and scheduling. `plans/2026-04-17-algebra-foundations-directions.local.md` §5.7.
- [ ] **§5.7 Integer partitions & Young's lattice** [Algebra, Low]: `(wile algebra partition)` — `partitions-of`, conjugate partition, dominance order, Young's lattice as a poset. ~150 LOC. Natural addition given `order.sld`. `plans/2026-04-17-algebra-foundations-directions.local.md` §2.6 + §5.7.
- [ ] **§5.7 Category theory extensions** [Algebra, Low]: Functors, natural transformations, general adjunctions beyond `galois.sld`'s Galois-connection special case. Formalizes abstract-interpretation composition (Cousot & Cousot 1977). ~400 LOC. `plans/2026-04-17-algebra-foundations-directions.local.md` §5.7.
- [ ] **§5.7 Connes-Kreimer Hopf algebra on rooted trees** [Algebra, Low]: Coproduct cuts subtrees — matches `ast-transform`/`ast-splice` primitive operation in wile-goast's `utils.scm`. Formalizes rewrite-rule composition. ~300 LOC. `plans/2026-04-17-algebra-foundations-directions.local.md` §5.7.
- [ ] **§5.7 Submodular optimization** [Algebra, Low]: Greedy approximation framework. Applies to program slicing, test-suite selection, import minimization (submodular-maximization-under-cardinality). ~200 LOC. `plans/2026-04-17-algebra-foundations-directions.local.md` §5.7.
- [ ] **§5.7 Symmetric functions / RSK** [Algebra, Research, Low]: Research-tier. Small consumer: LCS→LIS→RSK connection for statement/parameter-list diff in `unify.scm`. ~500 LOC. `plans/2026-04-17-algebra-foundations-directions.local.md` §5.7.

#### Follow-ups (deferred from shipped plans)

- [ ] **wile-goast AC-match migration** [Algebra, Follow-up]: Migrate `wile-goast/.../unify.scm:421` from `discover-equivalences` to `ac-unify`. Three risks: (1) term-protocol contract compliance, (2) trace-emitting diagnostic paths (`ac-unify` produces no rewrite trace), (3) small-arity benchmark before crossover claim. Scope ~100 LOC. `plans/2026-04-21-wile-goast-ac-match-migration.local.md`.
- [ ] **AC-matching v2 deferred decisions** [Algebra, Follow-up]: 8 decisions deferred in `memory/2026-04-21-ac-matching-design.local.md` "Open questions" — non-unit-multiplicity Stickel, sort-typed pattern-vars, E-matching scope. Re-open when a consumer surfaces.
- [ ] **Incidence algebra future extensions** [Algebra, Follow-up]: Items in `memory/2026-04-21-incidence-algebra-impl.local.md` "Future extensions (deferred)" section.

> Explicitly excluded as Part 7 non-goals in `plans/2026-04-17-algebra-foundations-directions.local.md` (no prospective consumer; documented here so the exclusion is visible rather than mistaken for oversight): tropical algebraic geometry, simplicial complexes / persistent homology, vector spaces as algebraic objects, holographic algorithms / Pfaffians, spectral graph matching, symmetric-function machinery beyond the LIS connection already tracked above.

### plans/ sweep — feature & new-capability deltas (2026-07-21)

Open feature work found only in `plans/` during the 2026-07-21 triage; recorded here so a TODO
scan sees it. Spans embedding, tooling, macro system, and algebra — kept together by provenance
rather than split across tiers.

- [x] **`--check` compile-only mode + call-site arity checking** [Embedding/Tooling, M, Done]:
  `wile --check` compiles without running (`Engine.CheckProgram`, `pkg/wile/check.go`;
  `runCheck` in `cmd/wile/main.go`), plus static call-site arity checking against primitives,
  imports, and same-unit defines (`pkg/machine/compilation/compile_call_arity.go`,
  `validate.UnitArityOf`). All 3 phases shipped.
  `plans/2026-07-19-compile-check-and-call-site-arity.md`.
  Five plan claims were wrong and are corrected in the implementation — recorded here because
  each one is a trap for anyone re-reading that plan:
  (1) compiling a `define` **does** register its binding, so the plan's "does not execute" test
  premise was false; the real proof is differential against `EvalProgram`.
  (2) `EvalProgram` has **three** call sites in `main.go`, not two — patching only
  `runFile`/`runEval` would let `--check -f a.scm -f b.scm` execute `a.scm`.
  (3) resolving the callee globals-only (`GetGlobalIndexWithScopes` alone) is a **false compile
  error** on `(define (k car) (car 1 2))`; identity against `GetBinding` is required.
  (4) formals for `(define (h x y) ...)` live on the define, not on a `SubExp` lambda.
  (5) the unit arity table cannot live on `CompileExpression` (re-entrant, 11 call sites), and
  cannot be gated on `Binding.IsStable()` — that flag is not stamped until the define itself
  compiles, which silently skipped every forward reference.
  Behavior note: arity checking fires in **all** compiles, not only `--check`. Two Scheme tests
  asserting *runtime* arity errors were rewritten through `apply` to keep the count hidden until
  run time (`test/scheme/records-test.scm`, `test/scheme/strings-test.scm`); that was the entire
  blast radius across `test/`, `examples/`, `pkg/stdlib/lib/`, and `benchmarks/`.
- [ ] **Pipeline seams as parameters** [Embedding, S–M, plan not-started]: expose `current-eval` /
  `current-print` / `current-read` as parameters so embedders can intercept the REPL pipeline.
  `plans/2026-07-11-scheme-pipeline-seams-design.local.md`.
- [ ] **Climbing-tower Tier 2 — per-phase mutable-state instantiation** [Macro system, L, gated]:
  Tier 1 shipped; Tier 2 (§6) instantiates fresh per-phase mutable state, plus the Boundary-2
  resolution rework (§7.3). Explicitly gated on owner sign-off.
  `plans/2026-07-10-climbing-tower-design.local.md`.
- [ ] **Bootstrap self-check diagnostics (W2/W3)** [Diagnostic/guard, S]: a load post-condition
  self-check (W2) and a nil-pin census (W3) over the bootstrap core.
  `plans/2026-07-18-bootstrap-core-unification-and-signals.md`.
- [ ] **`er-macro-transformer`-equivalent on the sets-of-scopes core** [Macro system, M, parked]:
  chibi-derived ergonomics backlog #5 — an explicit-renaming procedural-macro entry point plus a
  `datum->syntax` companion. `plans/2026-07-11-chibi-derived-ergonomics-backlog.local.md`.
- [ ] **MCP LLM support / SOTA server** [Tooling, phased]: LLM-support phases
  (`plans/2026-06-05-mcp-llm-support-design.local.md`, Phase 1 impl-ready) and the bring-to-SOTA
  design (`plans/2026-04-17-mcp-server-sota-design.local.md`). Distinct from the "MCP triggering
  rewrite (Lever A)" item in Tier 2 above.
- [ ] **Copilot-review data mining** [Tooling, not-started]: mine Copilot PR-review data (Tier 2
  target; Tiers 3–4 gated on Tier 2). `plans/2026-04-20-copilot-review-data-mining.local.md`.
- [ ] **All-executed-code coverage tracking** [Tooling, queued]: extend Scheme coverage beyond
  top-level templates to every executed form; blocked on algebra Tier B.
  `plans/2026-04-23-coverage-library-tracking.local.md`.
- [ ] **`(wile algebra polynomial-ideal)`** [Algebra, plan impl stalled]: univariate polynomial
  ideal abstract domain. Design approved; impl 10/41 boxes ticked but **nothing in git** — likely
  stalled mid-flight, confirm before resuming.
  `plans/2026-06-09-polynomial-ideal-domain-{design,impl}.local.md`.
- [ ] **`(wile algebra recurrence)` — set-closure & graph-reachability** [Algebra, impl 0/5]:
  `plans/2026-04-16-recurrence-{categories-design,impl-plan}.local.md`.
- [ ] **SRFI-204 `match`** [Standard library, design draft]:
  `plans/2026-06-04-srfi-204-match-design.local.md`.
- [ ] **TinyCLOS object system** [Standard library, design only]: classes, MOP, multimethods.
  `plans/2026-06-24-tinyclos-object-system-design.local.md`.
- [ ] **Kelso-Crawford many-to-many matching** [Algebra, gated]: stub, gated on the
  `(wile algebra matroid)` §5.7 item above. `plans/2026-05-02-algebra-matching-many-to-many.local.md`.

> Cross-repo (deliverables land in **wile-goast**, not this repo, but tracked here for
> visibility): b3-c2-c6 **C5** Galois auto-lifting + **C6** belief graduation
> (`plans/2026-03-25-b3-c2-c6-design.local.md`).

---

## Tier 3 — Tooling & Developer Experience

- [ ] **Scheme linter** [Tooling, High, Partially Closed]: Static analysis for Wile Scheme code — catch "plausible but wrong" before execution. **Closed:** unbound bindings and arity mismatches, via `wile --check` and the compile-time call-site arity check (see the `--check` item above). **Still open:** unused bindings, type mismatches, unreachable code, style warnings — plus the two structural limits the shipped work accepted rather than solved: only the *first* error is reported (the compiler stops there, so there is no multi-diagnostic pass), and `--check` is not side-effect-free because `(import ...)` executes library bodies at compile time. Research needed: what do Racket (Check Syntax), Guile, CHICKEN lint tools actually check? How much at expand time vs separate pass? Interaction with type system is a key design question.
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.local.md`
- [x] **Scheme-side line coverage** [Tooling, M, Done]: Shipped and merged to master — `WithCoverage(*coverage.Collector)` engine option (`options.go:443`), `pkg/coverage/` package, `--cover PATH` + `--cover-stdlib` CLI flags (`cmd/wile/main.go:56-57`), Go cover v1 output consumable by `go tool cover -html`, end-to-end `cmd/wile/cover_integration_test.go`. Docs: `docs/coverage/scheme-coverage.md`. `memory/2026-04-18-scheme-line-coverage.local.md`
- [ ] **Source file tracking in Syntax Objects** [Tooling]: Utilities for finding source locations and providing source lines.
- [ ] **`make doclint` target** [Tooling, S]: Extract `foo.go:N` citations from `docs/**/*.md` and `plans/**/*.md`; assert each file exists and `N` is within `wc -l file`. Cheap version catches the bulk of drift. Existing `check-readme-links.sh` only validates markdown link targets, not prose citations. Past multi-commit doc sweeps (PRs #707, #710, #711, #712, #713) are evidence the check would pay for itself. Stronger form would `go/ast`-parse the cited line and verify the enclosing decl name matches a nearby identifier in the doc.
- [ ] **`make planlint` target** [Tooling, S]: Flag plan files whose header status is stale. A plan's status lives in two places — its own `**Status:**`/`status =` header and the central `plans/CLAUDE.local.md` index — and only the central one is on the post-merge checklist, so per-file headers rot. Cheap version: for each `plans/*.md` whose header matches `not started|design only|design draft|ready to implement|pending`, extract any cited `PR #N` / `#N` and assert it is *not* merged (`gh pr view N`); a merged PR under a "not started" header is the drift signal. Evidence the check pays for itself: a 2026-06-05 audit found **10** plan headers claiming not-done for work merged on master (interval-dataflow-widening, sat-solver, numeric-registry, values-SR, approximate-counting-semirings, bignum-allocation-reduction, algebra-docs). Stronger form: cross-check each header against its `plans/CLAUDE.local.md` row and flag mismatches. Companion to `make doclint` above. `1` lone candidate left unresolved by that audit: `2026-04-20-axis-b-annotation-bugs` (cleanup-shipped claim unverifiable from git).
- [ ] **POSIX API / SRFI-170 remaining phases** [Standard library, 9 phases]: Phases 2-10 not started. Phase 1 (directory ops + process extension) completed in PR #565.
- [ ] **REPL tab completion still offers macro-introduced binders** [Tooling + hygiene, S, 2026-07-19]: `Namespace.BoundSymbolNames` (`pkg/environment/namespace.go:315`) now lists only names resolvable under the ambient scope set, via `GlobalEnvironmentFrame.AmbientKeys` (`global_environment_frame.go:267`). The completion path was deliberately left on the unfiltered walk — `Completer.collectBindingNames` (`pkg/repl/completer.go:83`) → `Engine.BoundNames` (`pkg/wile/engine.go:842`) → `Namespace.BoundNamesAcrossPhases` (`namespace.go:342`), which ranges `global.Keys()` at `:353`. The two listings now disagree, and completion can still offer a name that resolves to nothing. **Why it was not filtered alongside:** `BoundNamesAcrossPhases` also walks the expand and compile phase frames, where `define-syntax` keywords live (`compile_define_syntax.go:91`), so an ambient filter would drop any keyword whose binder carries a non-empty scope set. `ee918fd1`'s message states a top-level user binder carries the empty set, which suggests keywords survive — but that is read off a commit message, not measured, and library-defined + imported macros are unchecked. **Measure first:** apply the filter, diff the completion list before/after on a KitchenSink engine; missing macro keywords (`when`, `unless`, stdlib forms) is the disqualifying signal. Not at risk: `let-syntax`/`letrec-syntax` keywords are local bindings (`expander_let_syntax.go:137`), never in `Keys()`. Same read-path family as A1 above, which fixed the sealed-base half of this walk.

---

## Tier 4 — Performance

- [ ] **Recover the 1.3% `MachineClosure` widening cost by splitting the legacy in-place closure into its own type** [Performance, S-M, HYPOTHESIS UNTESTED — 2026-08-01]: The closure pair split (`perf(machine,environment): capture closures as shape+parent`) removed an 80-byte frame per evaluated lambda and grew `MachineClosure` 16→24B, because it carries `frame` **and** `parent` where it used to carry one `env`. Measured cost on `BenchmarkParallelScalingCompute` (fib, which never builds a closure in its loop but runs the altered apply path on every recursive call): **+1.25% / +1.56% / +1.17% at P=1/2/4, p=0.002, n=6**; indistinguishable at 8/16 where the spread is ±3%. Deliberately accepted — the same change is −20% to −38% on `…ScalingControl`. Numbers and method are in the `scaling_bench_test.go` header.

  **Separate what is measured from what is guessed.** Measured: the regression, and (via `-gcflags=-m`) that `ApplyParent` and `InitApplyFrame` both still inline, so it is *not* a lost inline. Guessed: that the 16→24B widening is therefore the cause, and that returning to 16B would recover it. Neither has been tested. Do that first — a 24B and a 16B struct land in different Go size classes, but so would any other layout change, and 1.3% is close enough to this benchmark's ±1% floor that it needs `-count 6` + benchstat against an adjacent baseline (see the header's replication warning; an earlier cut in this arc first read as +14% and was in fact p=0.699).

  **The lever, and it got simpler.** The two fields were carried to discriminate two representations, but the second one **has no production producer**. Every caller of `NewClosureWithTemplate` — three, not the two an earlier draft of this entry claimed; it omitted `createTransformerClosure` (`pkg/machine/compilation/compile_syntax_rules.go:876`), which builds every `syntax-rules` transformer — passes a frame from `NewEnvironmentFrameWithParent`, which panics on a nil parent. Verified by panicking in that arm and running the whole suite green. So `parent` is always non-nil, and `frame` could hold the compile-time frame alone with the parent read from it, or the two could collapse some other way. This entry originally cited "the in-place case must read `frame.Parent()` LATE" as the constraint making a split hard; that requirement came from `(compile …)` capturing a *pooled* frame whose parent went nil, which was the use-after-release bug fixed in the same arc. It no longer binds.

  **Gate:** only worth doing if (a) a 16B prototype actually recovers the 1.3% under benchstat, and (b) it does not put dynamic dispatch on the apply path — this codebase has a measured preference for switch over table dispatch, and `applyClosure` is the hot path the change is trying to speed up. If (b) eats the gain, close this as WONTFIX and leave the note.

- [ ] **`resolveGlobal` re-locks one frame once per lexical depth** [Performance + structure, S for the guard / L for the carve, 2026-07-19]: `NewEnvironmentFrameWithParent` (`pkg/environment/environment_frame.go:152`) sets `global: parent.global`, so every lexically-nested frame shares one `*GlobalEnvironmentFrame`. But `resolveGlobal` (`environment_frame.go:498`) walks the **EnvironmentFrame** chain, taking `ge.global.mu.RLock()` and running `bestSlotLocked` at *every* hop. A 12-deep closure nest therefore does 12 RLocks and 12 map lookups to answer a question with at most 2 distinct answers — only a hop where `ge.global` actually changes can differ. **Inferred from those two lines; UNMEASURED.** Measure before acting (per `memory/`: profile end-to-end, micro-benchmarks mislead here).

  **Cheap lever:** track the previous `*GlobalEnvironmentFrame` in the walk and skip a hop whose global pointer is unchanged. No structural or semantic change.

  **Structural lever (the real fix, deferred):** give `GlobalEnvironmentFrame` its own parent so the global chain is walked directly and the EnvironmentFrame walk for globals disappears. Today `EnvironmentFrame` owns the local chain *and*, transitively, the global chain; splitting them is a separation of concerns, not a duplicated chain. **This is a migration, not a field addition:** the parent relation is *computed*, not static — `phaseParent` derives it via `SealedTargetAt`/`SealedAt`, which routes to the frozen sealed base for the layered main namespace (the hermeticity cut) but to the frame itself for a flat `NewChildRuntime` library frame (`namespace.go:976`). Moving that decision into global-frame construction goes through the hermetic-phases work. Distinct globals in a chain today: `wireRuntimeFrames` builds `sealedBase(sealedGlobal) ← runtime(mutableGlobal)` (`namespace.go:1063-1073`), and each phase frame owns one parented via `phaseParent`.

  **Rejected alternative:** make `GlobalEnvironmentFrame` satisfy an `EnvironmentFrame` interface with `Parent()` always nil. There is no such interface — `EnvironmentFrame` is a struct (`environment_frame.go:95`) — so this means introducing one and putting dynamic dispatch on the VM's hottest path, against this codebase's measured preference for switch over table dispatch. It is also false to the structure: there are genuinely ≥2 ordered global frames, and a permanently-nil parent erases the sealed-base shadowing the layered carve exists to provide. Neighborhood is flagged in `memory/` (cross-engine sealed-base sharing SHELVED at D4).

- [ ] **Benchmark + profile the cycle-detection and context-poll cost added to `Pair.ForEach`** [Performance, S, UNMEASURED — crosscheck `15b68433..8c297173`, 2026-07-14]: `Pair.ForEach` (`pkg/values/pair.go`) gained Brent's cycle detection (pointer compare + increment + branch, plus a power-of-two checkpoint teleport) **and** an amortized `ctx.Err()` poll, on every walk. The correctness win is real and not in question — it closed the unbounded walk that let `(apply + circular-list)` grow the eval stack past every configured limit. What is in question is the cost, because **this is *the* list walker**: the code's own comment names the blast radius (`ForEachProperList`, `length`, `list-copy`, `append`, `reverse`, and apply's argument spread all funnel through it). No benchmark evidence was produced with the change.
  Work: A/B `make bench-gabriel` and `make bench-extended` across the commit, and profile a list-heavy workload end-to-end (`wile --cpuprofile`). Per `memory/`: micro-benchmarks mislead here — profile end-to-end, and do it *before* deciding anything. If the cost is material, the levers are (a) hoist the cycle check behind a length threshold so short lists (the common case) pay nothing, or (b) split a `ForEachUnchecked` for callers that have already established properness. Do not pre-emptively optimize: measure first, and record the numbers here either way so the next person does not re-ask.

- [ ] **Shrink `Binding` to recover the D2 atomicCell regression** [Performance, M — PUNTED 2026-07-06: too complex for the payoff, don't pick up without new evidence]: The D2 race fix (commit `fbcd7654`) grew `Binding` 32→40B (heap `atomicCell` pointer), inflating the value-embedded local frame slabs (`[]Binding`) and costing **+4.6% geomean on bench-gabriel (15/16 slower)**. Recovery lever: shrink `Binding` back so the local-frame slab footprint returns to baseline while globals keep the atomic cell (e.g. move rarely-used fields off the hot struct, or split local vs global binding representations). Gate on re-running bench-gabriel to confirm the recovery. Pure-perf follow-up; correctness is already banked. **Punt rationale (2026-07-06 analysis):** the recoverable win is *capped at the slab half* of the 4.6% — the other half is the global-read pointer hops (`*Binding → cell → atomic.Load → deref boxed value`), intrinsic to atomically publishing a 2-word `values.Value` and unrecoverable without 1-word NaN-boxing (separate `unsafe`-blocked plan). And getting the local slab to 32B soundly is not cheap: `value`/`cell` are a mutually-exclusive union but Go has no unions; pointer-tagging `bindingType`/`cell` low bits is GC-unsafe; `bindingType int`→`uint8` alone pads back to 40. The only sound mechanism is a cross-package `LocalBinding`/`*Binding` type split (ripples `environment/` + `internal/validate/` + `machine/pool`), whose natural unifier (an interface) adds dispatch to the Apply hot path and can eat the gain. Better lever for the same 4.6% is MORE frame reclamation (remove the slab allocation entirely so its size stops mattering) — but that arc is itself PAUSED (see `plans/2026-06-18-frame-reclaim-precision-coverage.local.md`: value core A/E shipped, tails B/C/D/F/G stopped under "limited payoff is a valid stop", resume gated on a real workload showing frame-leak pressure). **Resume this only if a representative embedding workload profiles as local-slab-allocation-bound AND the type-split measures net-positive on bench-gabriel.**
- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.local.md`
- [ ] **B3 effective capture refinement** [Performance, Research]: Propagate B2 escape results back into B1 capture status. A binding marked `Captured` by B1 is effectively non-captured if every lambda that references it is stored in a non-escaping binding (B2). Cross-binding analysis over B1+B2 results.
- [x] **`PrimitiveSpec` capture-safety capability field** [Performance, M, Done — PR #776]: Shipped as `PrimitiveSpec.InvokesProcedure` (`pkg/registry/apply.go`); each primitive self-declares, and the classifier stamps `Binding.CaptureSafe = !spec.InvokesProcedure` (`apply.go:240`) — extension primitives self-cover, no central list. The hand-maintained `captureSafePrimitiveNames` whitelist was retired (`frame_reclaim_build_test.go:58`). Below: original scoping. The escape-frame classifier's Layer C (`internal/validate/frame_reclaim_build.go`) decides whether a *primitive* callee is capture-safe via a hand-maintained, sound-by-default name whitelist (`captureSafePrimitiveNames` — `+`, `cons`, `<`, … contribute no edge; unlisted ⇒ unsafe). A primitive is capture-*unsafe* iff it can invoke a Scheme procedure that captures (`apply`, `map`, `for-each`, `call-with-values`, `dynamic-wind`, `with-exception-handler`, `sort`, …). The principled replacement is a `PrimitiveSpec.InvokesProcedure` (or `CapturesContinuation`) capability field so each primitive — including extension primitives — self-declares, instead of a central name list that silently under-covers extensions. **Gate:** only worth building once the sibling escape-gated plan's Phase 2 measurement shows the classifier's precision matters (the whitelist is sound regardless; this is a coverage/scalability dial). Weight false-positives (declaring a capturing primitive safe) as unacceptable per `feedback_annotation_stability.md`. Q-1 of `memory/2026-06-12-escape-frame-validation-impl.local.md`.
- [x] **`markCaptured`/`.Captured` is dead code — delete or unify** [Tech debt, S, follow-up to escape-frame-validation, Done]: Resolved via option (a) — deleted `markCapturedBindings` (`internal/validate/validate_capture.go`), the `ValidatedLetBinding.Captured` field, its 5 call sites in `validate_let.go`, and `validate_capture_test.go`. Shared test helpers from that file (`call`/`symRef`/`lam`/`makeTestEnvAndBindings`/… still used by sibling test files) were relocated to `internal/validate/sharedtest_test.go`. The live `markEscaped`/`.Escapes` path (read at `compile_let.go:240` for let-lambda inlining) and the unrelated live `bodyReferencesCaptureOperator` (call/cc detection, read by `frame_reclaim_build.go`) were left untouched. Unification (option b) was rejected: with B1/B3 never built there is no second consumer to unify against, and folding a fail-safe predicate in with the best-effort one risked regressing the inlining contract.
- [ ] **Benchmark coverage gaps** [Performance, S-M]: No benchmarks for compiler, expander (syntax-rules expansion), library import resolution, or continuation capture/restore cycle. Existing benchmarks cover VM dispatch, fibonacci, tokenizer, parser, environment, and symbol interning.
- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.local.md`
- [ ] **Inline-budget guard for `checkStackSize` and similar hot-path wrappers** [Performance, S]: `checkStackSize` (`machine/machine_context.go:1185`) is split from `reportStackOverflow` specifically to stay under Go's 80-cost inline budget (currently 67). A future innocuous edit could push it over and silently regress the VM hot path (the Gabriel suite would catch it, but only post-hoc and noisily). Write a test that runs `go build -gcflags='-m=2' ./machine/` and asserts `"can inline (*MachineContext).checkStackSize"` appears in the output. ~30 LOC test infrastructure; reusable for future hot-path wrappers. Surfaced by Finding 5 / PR #734 type-design review.

---

## Tier 5 — Tech Debt

### Sealed axis keyed by `(phase, kind)` — SHIPPED 2026-08-03 (`74c72256`)

- [x] **Sealed-frame routing collapsed into one table** [Tech debt / legibility, M, Done]: the two
  sealed frames were reachable only through machinery specialized to phases 0 and 1 **by name** —
  `SealedBaseTarget`/`SealedExpandBaseTarget`, a `phase == PhaseExpand` branch in `AtPhase`,
  another in `phaseParent`, and a hardcoded frame pair at each enumeration site. Seven sites
  hand-unrolled one two-row loop and the parallelism was carried by comments. `sealedAxis`
  (`pkg/environment/sealed_base_frame.go`) is now the single decision site, read by `SealedAt`,
  `sealedFrameAt`, `SealedFrames`, and `IsSealed`; call sites read as coordinates
  (`SealedTargetAt(PhaseRuntime, SealKindValue)` for bootstrap procedures,
  `(PhaseExpand, SealKindHandler)` for primitive expanders). The overloaded
  `SealedBaseTarget() != env` predicate became `IsNamespaceRuntime()`, the layered-vs-flat
  question it was actually asking. **No behavior change on any live path** — every guard was
  traced, not assumed. Three findings came out of the `/crosscheck`, all fixed in the same
  commit: (1) generalizing the routing seam had silently generalized "there is no seal here" to
  cover "the seal is missing", inverting two deleted comments that said a broken construction
  must fail loud — `SealedAt` now returns `(*EnvironmentFrame, bool)` and a declared-but-absent
  frame panics through `mustSeal`, mutation-verified across all six routing paths; (2)
  `phaseParent` ran under the `PhaseRegistry` **write lock** and had started calling a seam that
  can re-enter `GetOrCreate` (reproduced as a real hang, not a panic), and it asked
  `SealedAt(phase, SealKindHandler)` for a link that is **kind-independent**, so a value-only row
  would have been parented to by nothing — both fixed by `sealedFrameAt` plus a pure-pointer
  `IsNamespaceRuntime` guard; (3) `registry.SearchDoc` (`,apropos`) still hand-enumerated both
  frames, the second enumeration site a new row would have silently missed.

- [ ] **Expand-phase registry primitives are NOT sealed** [Correctness?, S, **unverified — reproduce
  first**]: `sealedAxis` made the asymmetry legible rather than creating it. `registry.Apply`'s
  `phaseTargets` binds `PhaseExpand` primitives into `env.Expand()` — the **mutable** expand
  child — while special-form expanders and bootstrap macros go to `sealedExpandBase`
  (`apply.go`, the `expandEnv := env.Expand()` row). That is the `(expand, value)` cell in the
  table, and it is plausibly the same latent gap that motivated `sealedExpandBase` one row up:
  `CreateGlobalBinding` dedups by `scopeSetsEqual` ignoring `BindingType` and
  `SetOwnGlobalValue` overwrites in place, which is exactly how a user `define-syntax` used to
  kill `let-syntax`. **The vector is untested**: does a `begin-for-syntax`/`define-for-syntax`
  at top level land in the same slot as an expand-phase primitive of that name and overwrite it?
  Write the repro before sizing any fix — two of seven filed bugs in the 2026-07-21 sweep did not
  reproduce as filed. If it does reproduce the fix is one cell (`sealsValue|sealsHandler` on the
  phase-1 row) plus rerouting `phaseTargets`; if it does not, the cell is correct and deserves a
  comment saying why.

- [ ] **The seal is exactly one level deep, and the exit is silent** [Design, S, deliberate — decide
  before Tier 2 of the climbing tower]: phases ≥ 2 have no seal, so a `define-syntax` inside a
  transformer body (env == `sealedExpandBase`, `NextPhase()` → phase 2) climbs off the sealed axis
  into the **mutable** compile frame. Pinned by `TestSealedClimbStopsAboveExpand`, so it is
  documented behavior rather than an accident, and latent today because the climb engages only for
  **procedural** transformers and no bootstrap macro has one. It becomes live if a bootstrap macro
  ever gets a procedural transformer body that defines a macro: that macro's own compile-time
  helper would be user-overwritable, the property D3 exists to prevent one phase lower. Adding a
  phase-2 seal is one `sealedAxis` row plus construction in `wireRuntimeFrames`, but it **forces an
  unanswered question**: is the new frame's parent `sealedBase` (flat, mirroring the mutable axis's
  no-phase→phase-edge rule) or `sealedExpandBase` (chained, mirroring the phase tower)? The two
  coincide today only because phase 0's seal is also the graph root. Supersedes nothing in
  "No sealed base above phase 0" (Tier 1, RESOLVED 2026-07-22) — that entry closed the phase-1
  hole; this is the phase-2 successor it did not reach.

### `predeclareBinding` leaves an unwritten `#!void` twin slot per library-body define (2026-07-19)

- [ ] **Orphan slot per library-body `define`** [Tech debt / allocation, S,
  **count unverified**]: reported at 104 orphans in `(srfi 1)` alone. C3 (scope-keyed export
  resolution) made them *unreachable* rather than removing them, so the correctness question
  is closed and only the allocation remains — which is why this sits in Tier 5 and not with
  the correctness successors in Tier 1 (see "Scope-keyed globals — successor work"). Verify
  the count before sizing the work; it comes from the plan's review notes, not from a
  measurement in-tree.

### Channel done-channel lifecycle follow-ups (adversarial review 2026-07-16, `fix/channel-lifecycle-ctx`)

Residue from the adversarial review of the done-channel lifecycle rewrite. The
rewrite itself is a real correctness win (closes the `channel-send!` TOCTOU host
panic and the ctx-ignoring parked-goroutine leak) and is `-race`-clean; **no new
correctness bug survived verification** (the `with-timeout ∘ channel-receive`
laundering I hypothesized is blocked by the eager `ErrTimerExpired` check at
`call_foreign_cached.go` after every foreign return: 60/60 handler-runs, 0/40
side-effect leaks). These four items are the leftover design/API/docs/test debt.

- [ ] **Cancellation "seam" is built but discarded** [Design, S–M — action (A) DONE, B-vs-C still gated]: **(A) done 2026-07-16:** the `PrimChannelSend` comment (`extensions/gointerop/prim_gointerop.go`) now names the real, non-local invariants per source and points at `docs/concurrency/cancellation.md`. Two corrections to the framing below, which is stale: the old comment's "the thread is unwound anyway" was wrong for `thread-terminate!` *even on its own terms* — safety there is now `Thread.setOutcome`'s write-once rule, **not** the ≈1024-op ctx-check window (a cancelled op in tail position has no following op to trip it; see the `thread-terminate!` items above). And the claim "no channel or timer test would catch the change" is no longer true: `TestWithTimeoutInterruptsParkedReceive` fails if the eager recheck regresses (mutation-verified). **B-vs-C still open**, but narrowed: with `with-timeout` covered by the eager recheck and `thread-terminate!` by write-once, an embedder-supplied deadline is the *only* source where the seam would change an observable result — so item 1's B-vs-C and open decision 3 (embedder-deadline observability) have collapsed into one question. If that laundering is acceptable, B has no consumer and C follows. Original scoping below.
- [ ] **~~Cancellation seam~~ (original scoping, retained for context)** [Design/Correctness-adjacent, S–M]: `SendOutcome`/`RecvOutcome` (`pkg/values/channel.go`) exist expressly to keep the ctx-cancellation cause visible ("the seam"), but `PrimChannelSend`/`PrimChannelReceive` (`extensions/gointerop/prim_gointerop.go`) collapse it unconditionally (Option A): `RecvCancelled`→`Void` (indistinguishable from a legitimately closed+drained channel), `SendCancelled`→`ErrChannelClosed` (catchable by an ordinary channel-error `guard`). The justifying comment ("the thread is unwound anyway") is true only for `thread-terminate!`; it does **not** cover `with-timeout` (safe only via the eager `ErrTimerExpired` recheck in `callForeignCached`, a non-local invariant the comment never states) or an embedder-supplied deadline (`mc.timer == nil`, cause `DeadlineExceeded` → no eager recheck; body runs up to `contextCheckMask`≈1024 ops with a laundered `Void` before the deadline propagates — bounded, VM-consistent, and strictly better than the old infinite hang, but the one path where "cancelled receive looks exactly like closed channel" is observable). **Possible actions:** (A) tighten the comment to name the real invariant (eager `ErrTimerExpired` check + bounded `thread-terminate!` latency + the embedder-deadline window); (B) *use* the seam — surface `RecvCancelled`/`SendCancelled` distinctly so a cancelled op cannot be confused with close; (C) delete the seam and collapse to a bool if Option A is a committed contract. **Recommendation: A now** (cheap; closes the misleading-comment hazard that guards the test item below). **B or C deferred**, gated on the open question "is Option A a committed contract?" — if committed, C; if placeholder, B. Do not do both A-longterm and C.
- [x] **`ChannelSelect` was complete, tested, CHANGELOG-cited — and registered nowhere** [API/dead-code, S–M, Done — deleted]: removed `ChannelSelect`, `SelectCase`, `SelectCaseKind` + its 3 constants, `firstDeadCase`, and the 8 `TestChannelSelect*` functions — ~312 lines, no consumer anywhere. Three corrections found while scoping it: (1) it was **exported from `values/`, a public embedding package**, reachable from Go even though no Scheme program could call it, so this is a public API removal taken under the zero-consumer rule, not internal cleanup; (2) wiring it would have needed a **ctx arm, not just `done` arms** — it took no `context.Context` while `Send`/`Receive` both do, so exposing it as-is reintroduces the T1.3 leak at a new site (a throwaway prototype confirmed 2N+1 arms work `-race`-clean, so the decision was never technical); (3) the CHANGELOG line was **not** deleted — 1.18.0 and 1.3.0 are released sections and deleting from them rewrites shipped history, so a removal note plus a correction went into `[Unreleased]` (the 1.18.0 entry had announced `channel-select` as a Scheme primitive that never existed). If a consumer ever appears: `reflect.Select` panics past 65536 cases and the list would come from Scheme, so it needs an arity guard.
- [ ] **Stale sub-context comment on `with-timeout`** [Docs, XS]: `PrimWithTimeout` (`pkg/registry/core/prim_timer.go`) header says "The sub-context pattern ... a fresh sub-context isolates the thunk's execution," but the same function's body twelve lines down (and `RunBodyUnderTimer`) says it runs the thunk **INLINE on the live chain, not in a sub-context** (the accurate description). REVIEW.md lists stale comments as a recurring trap; this one misdescribes the isolation model of the code that makes the `with-timeout` cancellation path safe. **Possible actions:** (A) delete the stale sub-context sentence; (B) rewrite it to match the inline model. **Recommendation: A (delete)** — the accurate description already exists in the same comment, so B just duplicates it.
- [x] **Scheme-level cancellation tests added; two real defects found writing them** [Test-coverage + Correctness, S, Done — A+B+C shipped]: `extensions/gointerop/channel_cancellation_test.go` adds `TestWithTimeoutInterruptsParkedReceive` (mutation-verified: disabling the eager `ErrTimerExpired` recheck in `callForeignCached` makes it fail with the laundered `Void`) and `TestTerminateUnparksBlockedThread` (mutation-verified: disabling `Receive`'s ctx arm makes it fail with `JoinTimeoutException`). Go-level cancellation tests retained. Writing them surfaced two defects:
  - **`thread-terminate!` discarded its own SRFI-18 end-exception** [Correctness, S, Done]: `Thread.Terminate` stored a `TerminatedThreadException` and `Thread.Start`'s goroutine then unconditionally overwrote it; `defer close(p.done)` is registered first and so runs last, guaranteeing the overwrite landed before any joiner was released — a started thread's joiner could **never** observe the exception. Worst case: a thread parked in `channel-receive` in **tail position** returned the cancelled receive's laundered `Void` as its ordinary result, with no following VM op to trip the ctx check, so terminate-then-join reported the terminated thread as having *succeeded*. That invalidated the design doc's three-sources table, which had argued the ≈1024-op unwind window was itself the protection. Fixed by making the outcome write-once (`Thread.setOutcome`): first writer wins, so `Terminate` beats the completion path while SRFI-18's "if the thread is not already terminated" clause still holds. Prior coverage was vacuous — both existing tests asserted the `#t` literal they wrote. Guard: `extensions/threads/prim_threads_terminate_outcome_test.go`.
  - **`thread-join!` on a terminated but never-started thread blocked forever** [Correctness, S, Done]: `done` is closed only by the goroutine `Start` spawns, which never ran and never could (`Start` rejects `state != ThreadNew`), so `Join` parked forever while the exception the joiner wanted sat in the outcome field. `Terminate` now closes `done` when it is the one ending a `ThreadNew` thread. The two closers are mutually exclusive because `Start` makes the `ThreadNew → ThreadRunnable` transition under `p.mu` before spawning and refuses any other state — so no `sync.Once` is needed, which mattered: a double close is a fatal host panic, the same hazard class as the `channel-send!` TOCTOU. Guards: `pkg/values/thread_lifecycle_test.go` (20000-trial `-race` no-double-close with a starting gate) and `TestThreadTerminateNeverStartedThreadIsJoinable`, which joins with *no* timeout so a regression is an unbounded park rather than a misleading `JoinTimeoutException`.

**Two decisions gate the deferred sub-items:** *is Option A a committed contract?* (drives item 1's B-vs-C) and *is `channel-select` on the roadmap?* (drives item 2's A-vs-B). Everything else (comment fixes + the tests) is safe to do now regardless. Suggested order: tests → delete stale comment → tighten seam rationale → resolve `ChannelSelect` → resolve the seam.

### List/Pair Primitive Cleanup (from inline annotations)

- [x] **List/pair primitive cleanup** [Low, XS–S, Done]: relocated from inline `// CLAUDE:` source annotations, which were removed because inline comments shift primitive line numbers and break `TestBuildAxisBManifest`. `(*Pair).Append` removed — confirmed dead, superseded by `PrimAppend`, which builds its own spine; the `Tuple.Append` interface method and both implementers went with it. `PrimReverse` was already `PairBlock`. `PrimListCopy` converted to `PairBlock`, reversing the earlier tail-pointer choice: `Tuple.ForEach` yields both the count and the terminating cdr, so an improper tail is preserved by re-pointing the block's last cdr after `LinkSpine` — 63→18 allocs, ~10% faster on 50 elements. `PrimAppend` remains the sole tail-pointer exemplar, since multi-arg concat genuinely cannot pre-count.

### FCA-Derived

- [x] **Structural-reduction roadmap** [Planning, Done — closed 2026-07-08]: spent and archived to `memory/2026-05-07-structural-reduction-roadmap.local.md`. Tier A closed (`values/` PRs #747–#756, `environment/` PR #730, `registry/` PR #728); Tier B closed (`wile/` PR #764; `repl/` + `registry/helpers/` on `refactor/structural-reduction-b2-b3` — low yield as predicted, most findings refuted or declined as churn); Tier C reassigned to `plans/2026-07-01-staff-engineer-sweep.md`.
- [x] **Machine package structural reduction** [Done 2026-05-13]: all 7 findings closed. Shipped — `Stack.Push` max-stack (PR #734), `OpKind()` discriminator (PR #735), vmState value-register consolidation + ruleguard (PR #736), correlated-field sub-records (PRs #742/#743/#745). Declined — syntaxCase marker interface (PR #731), maxCallDepth sentinel removal, tail/non-tail opcode collapse via sign-bit encoding (PR #737: geomean +2.5%, all 16 benches slower), Stage-3 sub-records (field-independence analysis found no co-variance, `9382a3b3`). `memory/2026-05-06-machine-structural-reduction.local.md`
- [x] **Internal / values / environment structural reduction** [Done]: `internal/` all 7 findings (PRs #739–#741, including the `*SyntaxPair`/`SyntaxEmptyList` duality migration that restores Chez-conformant `(equal? (syntax ()) '())`); `values/` Phases 0–4 (PRs #747–#756 — 9 port types collapsed to one `*Port` with capability slots, ~900 LOC, and a `NumericTypeSpec` registry replacing the 12-step ADDING-A-NEW-NUMERIC-TYPE guide); `environment/` Phases 1–9 (PR #730), Phase 10 (`*LocalIndex` allocation audit) deferred benchmark-gated. Plans archived under `memory/2026-05-0*`.
- [x] **vmCore sub-struct extraction** [High, M, DECLINED on re-evaluation 2026-06-05]: the genuine always-transfer set is only `{env, template, pc}` — the trivial fields. `callDepth` is not always-transfer but a *guarded maintained counter* (`SaveContinuation` ++, `PopContinuation` --, both continuation constructors derive it from the parent), transferring verbatim at 3 of 8 sites, so bundling it forces override-after-copy at 4 sites and risks clobbering its guards. The FCA "High" rating rested on the divergent fields (`evals` 4 ownership modes, `envPooled` 4 behaviors, `marks` clone-vs-direct), none of which a vmCore touches, and the drift concern is already answered by `testVmStateFieldCoverage`. Net ~6 lines saved at 3 sites on the VM's hottest path. Parallels the prior decline of machine SR Finding 7 Stage 3.
- [ ] **Bidirectional opcode conversion test** [Medium, S]: Verify `operationToInstruction` and `instructionToOperation` cover the same opcode set.
- [ ] **LocalEnvironmentFrame pointer ambiguity** [Low, S]: Doc comment on `NewLocalEnvironment` explaining lifecycle (value-vs-pointer ownership).
- [ ] **Honor `WithInlineThreshold` for imported libraries** [Low, S]: The library import/load chain (`LoadLibrary` → `loadLibraryFromReader` → `compileAndExecuteLibrary`, `machine/compilation/library_loader.go:215,223`) has **no `inlineThreshold` parameter**, so imported libraries always compile at `DefaultInlineThreshold = 5`, ignoring the engine's `WithInlineThreshold(n)` (`pkg/wile/options.go:275`). Every *in-process* child compiler re-threads the parent's value via the two-line `NewCompileTimeContinuation(...)` + `SetInlineThreshold(p.inlineThreshold)` idiom (6 sites: `compile_syntax_case.go:253`, `compile_closure.go:123`, `compile_library_forms.go:109`, `compile_helpers.go:51`, `compile_time_continuation.go:347`, `expand_and_compile.go:53`); the load path is the one site that cannot reach the value. **Not a correctness bug** — inlining here is the behavior-preserving synthetic-let transform (PR #605), so results are unchanged; it is a config-honoring / debuggability inconsistency (disabling inlining, e.g. for predictable stack traces, is silently not honored across the `import` boundary). Fix: thread `inlineThreshold` through the three `LoadLibrary`/`loadLibraryFromReader`/`compileAndExecuteLibrary` signatures (or expose it via `Namespace`/`EngineServices` so the load path can read it) and `SetInlineThreshold` on the library compiler. Discovered during the `CompileTimeContinuation` God-object triage (2026-07-09); the fix also illustrates why the "stable config should be inherited, not hand-copied" refactor (staff sweep tail) has real payoff — a shared services pointer would close this gap by construction.
- [x] **Unified binding reference (`BindingRef`) for local+global** [Medium, M, Done 2026-07-08, `229e0b72`]: `BindingRef` sum type + `ResolveBindingRef` in `environment/`; the validator's mutation set collapsed from 3 maps to 2. **Storage stays split, deliberately** — locals are positionally addressed (`LocalIndex{over,up}`), copied every `Apply`, single-threaded, `[]Binding` by value; globals are symbolically addressed, shared across SRFI-18 threads, and `[]*Binding` pointer-stable because the lock-free `cachedBindings` read cache requires it. Only the reference type unified. **Premise correction**: the validator was NOT structurally blind to top-level `set!` — the symbolic `mutatedKeys` sidecar already compensated and `StableInUnit` was correct — so this shipped as a semantics-preserving structural tidy, not the bug fix the original framing implied. The conservative over-mark (a `set!` to a local shadow still marks the top-level name non-stable) is the frame-reclaim soundness margin, now guarded by `TestStableInUnit_SetToLocalShadowStillMarksTopLevel` (verified non-tautological).
- [x] **Unify `atan2Operand` with `helpers.ToFloat64`** [Low, S, Done, PR #754]: `atan2Operand` re-implemented the Number-assert → complex-reject → float64-extract sequence just to swap the loss policy from strict to silent-truncate. Extracted shared `screenReal` into `registry/helpers/value_conv.go` and added `helpers.ToFloat64Lossy` as the lossy counterpart to strict `ToFloat64`; `atan2Operand` deleted, both `PrimAtan` sites routed through it. Lossy semantics (`(atan 1/3)`) preserved per R7RS §6.2.6.

### Tech Debt Plan (remaining)

- [ ] **Task 6.2: Replace `context.TODO()` in tests** [Low, S]: 431 occurrences across 39 test files. Mechanical `→ context.Background()`.
- [x] **Task 6.4: Add `typeswitchlint` to value type guide** [Low, S, Done — `a41ec0b7`]: Resolved by a stronger mechanism than the guide comment — `a41ec0b7` made `typeswitchlint` opt-in, CI-gating, and **drift-guarded** (`cmd/typeswitchlint/main_test.go`), so `knownValueTypes` diverging from the actual value-type set now fails CI mechanically rather than relying on a human reading a comment.
- [x] **Task 8.1: Extract `machine/compilation/resolver/`** [Done]: FileResolver implementations extracted. `LibraryEnumerator` replaced with `FileEnumerator.EnumerateFiles` (returns paths, not `LibraryName`). Type aliases in compilation for backward compat. `memory/2026-04-13-resolver-extraction-impl.local.md`
- [ ] **Task 8.2: Evaluate `wile.Value` wrapper** [Low, M]: Wrapper provides minimal methods beyond `Internal()` escape hatch.
- [ ] **Task 8.4: Make `DefaultBigFloatPrecision` configurable** [Low, M]: 256-bit precision hardcoded across 12 call sites. No engine option.
- [ ] **Error sentinel grouping** [Low, S]: ~109 sentinels in flat list. Consider category-specific files if count exceeds ~150.
- [ ] **Namespace registry typing** [Low, S]: Namespace's registry should have a type instead of `any`.
- [ ] **ValueType refactoring** [Low]: ValueType doesn't have grounding in Scheme or Go — determine use and scope of type domains.
- [ ] **Evaluate need for Primitive Annotation Enforcement** [Low]: Enforcement may not be needed.

### Algebra library consistency (2026-04-23 staff-engineer audit)

- [x] **Shared helpers promoted into `(wile algebra setoid)`** [High, S, Done]: `setoid-member?`, `setoid-assoc`, `setoid-dedup`, `assv-or`, and `validate-opts-keys` are public there; the private `%`-copies are deleted from group, combinatorial-graph, incidence, and lattice — four libraries, not the two the audit found.
- [x] **Drift-check test for umbrella `algebra.sld`** [High, S, Done — option (c)]: `algebra_umbrella_drift_test.go` parses every leaf `.sld` export clause and asserts umbrella coverage. First run caught real drift (`rewrite.sld`, `semiring.sld`). Deleting or generating the umbrella stays deferred until measured drift frequency justifies either.
- [x] **Structure-API convention documented instead of abstracted** [Medium, S, Done]: a `define-with-binder` meta-macro would have saved ~10 lines across 15 libraries at the cost of indirection — declined. `stdlib/lib/wile/algebra/CLAUDE.md` documents the five-part structure API (`make-X` / `X?` / accessors / `with-X` / `validate-X`), the shared plumbing, options-alist discipline, and validator shape instead; duplication stays mechanical.
- [x] **`validate-X` / `assert-X` / `make-X` idioms collapsed to generic helpers** [Medium, S, Done]: `make-violation-reporter` (two-mode — call with type+args to record, call bare to finalize) retrofitted across 14 libraries, replacing the `(set! violations (append …))` parent-delegation pattern. `assert-validation` replaces what would have been 18 per-structure `assert-X` symbols (+1 symbol, not +18), preserving the source expression in the error datum. `assert-procedure` retrofits 11 non-validating `make-X` constructors, capturing the source identifier so `(assert-procedure "make-ring" plus)` names both sides.
- [x] **`combinatorial-graph.scm` monolith — first cut** [Medium, M, Done (partial)]: custom insertion sort replaced by `list-sort` from `(srfi 132)`; 1,787 → 1,726 lines. The remaining `%`-helpers are genuinely WL/isomorphism-specific; splitting into sub-files is deferred until it buys review scope.
- [ ] **Watch `matrix.scm` for split pressure** [Low, S, Deferred]: 1,302 lines with two record types (`<semiring-matrix>` at 839, `<sparse-semiring-matrix>` at 1137) in one file. Shared helpers justify co-location today. Revisit once a third representation (banded, symmetric, etc.) appears — no action needed now.
- [ ] **Harmonize `docs/algebra/reference.md` section template** [Low, M, Deferred; 2026-04-23 crosscheck consistency finding]: First 15 sections use a fixed 5-heading template (Constructors → Predicates → Operations → Validation → Destructuring). The 11 sections added in PR #706 (matrix, polynomial, incidence, interval, graph, combinatorial-graph, unification, fca, pareto, abstract-domain, dataflow) use bespoke headings because their library shapes don't match the 5-part structure pattern (e.g. dataflow has no "law checker"; unification has pattern-vars, substitutions, matching as three parallel concerns). Decision at the time: keep bespoke headings since forcing the template would obscure real structural differences. Revisit if either (a) the template gets extended to cover the new shapes cleanly, or (b) a reader reports navigation trouble across sections.
- [x] **Back-port legacy Sage validators to `check_or_snapshot`** [Low, M, Done 2026-06-09]: 5 of the 6 legacy structure validators route through the shared helper and emit flat top-level `(test …)` fixtures. `powerset-lattice` stays hand-rolled and says why at the function: `lattice-join`/`lattice-meet` return sets in input order, not canonical order, so its live check must compare order-insensitively while its snapshot asserts only cardinality — a divergence the single-expression helper cannot express without weakening the membership check. Added a `('num', token)` sentinel to `to_wile_display`/`to_wile_test_literal` so rational-field asserts exact rationals by `equal?` instead of string-matching `number->string`. Fixtures regenerated under Sage 10.8.

### Helpers TypeName Encoding (PR #725 deferred items)

Items surfaced by /crosscheck adversarial review on PR #725 (helpers
typeName encoding refactor). Deferred per scope or design choice.

- [ ] **Distinct `*TypeSentinel` type for compile-time enforcement** [Tech debt, M, Deferred per Q1=A]: Type-design analyzer recommended splitting `*StaticError` into two types: `*StaticError` for non-type sentinels and `*TypeSentinel` for type-mismatch sentinels (embedding or wrapping `*StaticError`). Helpers like `RequireArg`/`RequireType` would take `*TypeSentinel` directly, making "passing a non-type sentinel to a type helper" a compile error. Current design uses runtime sum-as-struct discriminant (empty `expectedType` = non-type) plus `TestTypeSentinelsCarryTypeName` allowlist as the guard. Future cleanup once a real misuse incident motivates the rename across the codebase. See PR #725 review.
- [ ] **Store bare noun in `expectedType`, apply `articleFor` at format time** [Tech debt, S, Deferred]: Currently `NewTypeSentinel("string")` stores `expectedType: "a string"` (with article baked in). Type analyzer recommended storing `noun: "string"` and applying `articleFor` during `Error()`/`TypeName()`. Would let the article rule evolve (e.g., switch to phonetic) without regenerating sentinels, and would isolate the orthographic rule from the data. Pass-through irregulars ("a once") would need a separate `irregularArticle` field or override map.
- [ ] **`TypeNamer` interface for `typeNameFromSentinel`** [Tech debt, S, Deferred]: Currently `typeNameFromSentinel` matches on concrete `*werr.StaticError` via `errors.As`. Type analyzer recommended an open-extensible `interface { TypeName() string }` so any future error type could opt in. Trade-off: opens to accidental participation by unrelated types adding `TypeName() string`. Address when a second carrier of TypeName actually appears.
- [ ] **`Lengthable` rename to `IndexedSequence`** [Bikeshed, S, Deferred]: Type analyzer noted the helpers use the constraint as "indexed finite sequence" but the name `Lengthable` promises only `Length() int`. `*String`, `*Pair`, and `emptyListType` accidentally satisfy `Lengthable` but cannot meaningfully participate in `SequenceRef`/`SequenceSet`. Rename when the asymmetry causes real confusion.
- [ ] **Reflection-based `TestTypeSentinelsCarryTypeName`** [Test debt, S, Deferred]: Currently the inventory test enumerates ~55 type sentinels by hand. Test analyzer recommended a reflection-based variant that walks all exported `*StaticError` vars in `werr/` and asserts any whose `Error()` starts with `"not "` has a non-empty `TypeName()`. Self-maintaining, ~20 lines replacing ~60. Add when a contributor adds a new sentinel and forgets the inventory entry.
- [ ] **Extension-level message-content tests for new sentinels** [Test debt, M, Deferred]: Test analyzer flagged that no extension-level test asserts the user-visible "expected an integer/namespace/once" message content. Helper-level tests in `registry/helpers/args_test.go` pin the plumbing end-to-end through `TestRequireType_ErrorMessageContainsTypeName`, but a regression that, say, swaps `ErrNotAnInteger` back to `ErrNotANumber` in `make-vector` would not be caught by a test. Belt-and-suspenders coverage; add per primitive when message wording becomes load-bearing for users.
- [x] **`ParseOptionalStartEnd` / `ParseOptionalArg` literal phrases** [Tech debt, S, Done 2026-07-01]: resolved by comment, not migration. These are *shape* errors (proper-list, arity) with no expected-type noun to plumb through a `*TypeSentinel`; both doc comments in `pkg/registry/helpers/args.go` now say so, while the per-argument type checks still draw their noun from a sentinel.
- [x] **`read-line` / `peek-char` dropped `UnreadRune` errors and misclassified read failures** [Bug, S, Done 2026-05-06, `460c73a5`]: both sites now use `WrapForeignReadErrorf`, so `(read-error? e)` is `#t` per R7RS §6.11 — it had been `#f`, a direct violation. `io.EOF` after a bare `\r` stays silent; anything else propagates. Fault-injection infra added at `pkg/internal/extensions/iotest/`, asserted in `pkg/extensions/io/prim_read_error_test.go`.
- [x] **Library-binding installation swallowed errors silently** [Bug, S, Done 2026-07-01]: two `SetOwnGlobalValue` returns in `machine/compilation/library_bindings.go` were `_ =`-discarded — the asymmetry was evolved, since the sibling base-phase installs already wrapped-and-returned. A swallowed failure in the syntax-binding branch means a macro is silently not installed in the expand environment and expansion later fails mysteriously. Both now wrap-and-return. Also added the `targetPhase + sourcePhase` int8-overflow guard: `Phase` is `int8`, so a `for-meta` target of 127 (permitted by the parse-time guard) plus a syntax binding's +1 wrapped to −128 and misrouted the binding. Guard: `TestCopyLibraryBindingsPhaseOverflow`.

### Machine value-register follow-ups (PR #736 deferred items)

Items surfaced by /crosscheck on PR #736 (consolidate value-register
accessors on *vmState — Finding 3 of `memory/2026-05-06-machine-structural-reduction.local.md`).
Deferred per scope or design choice.

- [ ] **`SetValues(sub.GetValues()...)` nil-vs-empty ambiguity** [Tech debt, M, Deferred — pre-existing]: Silent-failure-hunter flagged 13 call sites that propagate a sub-context's value register into the parent via `mc.SetValues(sub.GetValues()...)`. `GetValues()` returns `nil` for an empty register (both fields nil); spreading `nil...` calls `SetValues()` with zero args, which now canonicalizes to (nil, nil) post-Q-e. Sub-contexts that exited abnormally without writing a value, sub-contexts that returned `(values)` (R7RS zero-value return), and sub-contexts that returned a real value all collapse into indistinguishable parent-side state. Call sites: `extensions/eval/prim_eval.go:104`, `extensions/files/prim_files.go:179`, `registry/core/prim_timer.go:127`, `registry/core/prim_barrier.go:72`, `registry/core/prim_cont_marks.go:187`, `registry/core/prim_prompt.go:135,149`, `registry/core/prim_control.go:87,200,365`, `registry/core/prim_exit.go:105`. Pre-existing; surfaced by but not introduced by PR #736. Fix shape: distinguish "no value produced" from "(values) zero-return" at each call site, or document the collapse as intentional R7RS behavior.

### Continuation vmState descriptor follow-ups (#1 Tier-1 shipped `834b2db7`)

Follow-ups from the staff-sweep #1 lever — `vmState` save/restore descriptor +
oracle. Tier-1 (descriptor + driven oracle + completeness ratchet across all six
save/restore/copy sites) shipped to master `834b2db7` with bodies unchanged.
Design: `memory/2026-07-02-continuation-vmstate-descriptor-oracle.md` (archived;
Decisions D-c, Option B). **Do NOT touch continuation method bodies without the red-suite +
A/B `/crosscheck` gate** (most-reverted neighborhood; auto-memory `tail-frame-recycling-unsound.md`,
auto-memory `c1-continuation-not-frame-reclaim.md`).

- [x] **Tier-2 — wile-goast capture-site shared-invariant belief** [Tech debt, M, Done, `3ddbe839`, `.goast-beliefs/continuation-capture-marks-shared.scm`]: asserts every function constructing a Captured/Composable continuation marks the live `mc.cont` chain shared (`MarkChainShared`) before any release path can fire — the `RELEASE_OLD_ENV`/`POOL_FRAME` precondition the field-oracle documents but cannot enforce. This is the class the two canonical reverts belong to: both had `RestoreAndRelease` doing exactly what the descriptor says while an upstream capture site failed to mark. Validated 5/5 capture sites; the reaches-call checker was added in wile-goast.
- [ ] **Option B — codegen the six save/restore/copy bodies from `contDescriptor`** [Perf/structure, L, Deferred — perf-gated]: The literal "data-driven" half of finding #1. `go:generate` the six method bodies *from* the descriptor so the spec lives in data and the code is emitted, not hand-transcribed — identical runtime (generated Go, not interpreted). **Hard gate:** an end-to-end benchmark proving normal-return-path parity (`memory`: micro-benchmarks mislead; sites #3–#5 are the hot path where table/reflection dispatch loses to a `switch`, and this path is the dominant GC contributor). Promotes `contDescriptor` from a `_test.go` spec to a generator-readable data file — a real restructuring, not a freebie. Do NOT gate #1's drift-catching value on this; Tier-1 already delivered that.

### Internal-SR follow-ups (PR #739 deferred items)

Items surfaced by /crosscheck on PR #739 (internal/ structural reduction
phases 1-5 — Findings 7, 4, 3, 2, 6 of
`memory/2026-05-07-internal-structural-reduction.local.md`). Deferred per scope.

- [ ] **`*SyntaxObject.Datum()` and `*SyntaxObject.Unwrap()` duplication** [Tech debt, XS, Deferred — pre-existing]: Both methods return `p.datum` with no transformation (`internal/syntax/syntax_value.go:94-96` and `:103-105`). `Unwrap` is the `SyntaxValue` interface method; `Datum` is the historical accessor. Pre-existing; surfaced by but not introduced by PR #739. Fix shape: audit callers (which name does each use?) and delete one. If Unwrap is interface-required, delete Datum or make it a one-line forward; otherwise reverse the choice. Out of scope for the structural-reduction phases; clean-up commit when next touching syntax_value.go.

- [ ] **`qt.Assert(t, ...)` vs `c := qt.New(t); c.Assert(...)` style split in `internal/validate/`** [Tech debt, S, Deferred — pre-existing]: The validate package's test files mix two quicktest invocation styles. Older files (`walk_sub_exprs_test.go`, `validate_capture_test.go`, `validate_escape_test.go`) use the `c := qt.New(t); c.Assert(...)` form; recent additions (`env_helpers_test.go` from PR #739, `walk_binding_refs_test.go` from PR #740) use the package-level `qt.Assert(t, ...)` form. Both are valid quicktest API; the split is purely stylistic. Fix shape: pick one and propagate — likely the package-level `qt.Assert(t, ...)` since it's the more recent precedent and is what other Wile packages use. Out of scope for any one PR.

### Loss-signals API follow-ups (numeric-loss-signals impl)

Items from the numeric loss-signals plan
(`memory/2026-05-14-numeric-loss-signals-design.local.md` /
`memory/2026-05-14-numeric-loss-signals-impl.local.md`). Track decisions
that were made on the impl path but warrant revisiting once usage
patterns are visible.

- [ ] **Revisit hybrid return shape if helper set grows** [Tech debt, M, Deferred]: Current API uses a **hybrid** return shape — `ToFloat64WithAccuracy` returns positional 4-tuple `(float64, big.Accuracy, bool, error)`; `ToComplex128WithAccuracy` returns `(Complex128Result, error)` with a named struct (fields `Value`, `RealAcc`, `ImagAcc`). The rule: positional when slot types disambiguate roles; struct when adjacent slots share a type and could be silently swapped. Decision rationale + alternatives (all-positional, all-struct) documented at `memory/2026-05-14-numeric-loss-signals-design.local.md` § "Decision record: return shape — hybrid (positional + struct)". **Revisit triggers**: (a) a second `WithAccuracy`-shaped helper with a single accuracy signal is added (rationals, intervals, matrix elements with one component) — re-evaluate whether the new helper should follow `ToFloat64WithAccuracy` (positional) or be promoted to struct for consistency with `ToComplex128WithAccuracy`; (b) a third or fourth multi-component helper is added (quaternion, matrix with N≥3 same-type slots) — at that point the asymmetry-as-domain-structure argument weakens, consider a unified struct convention; (c) FFI converter is refactored to consume the struct directly for both helpers (eliminates the discard-idiom advantage motivating positional `ToFloat64WithAccuracy`); (d) a `realAcc/imagAcc` swap bug is reported despite the struct — indicates the safety property failed, revisit whether stricter encoding is warranted (e.g., distinct newtypes `type RealAccuracy big.Accuracy` / `type ImagAccuracy big.Accuracy`).
- [x] **Big-precision numeric sweep — rounding, division, transcendentals** [Correctness, L, Done 2026-07-07; branches `fix/bigcomplex-precision-loss`, `feat/bigcomplex-angle-atan2`, `feat/big-transcendentals`, `feat/big-complex-transcendentals`]: removed the float64 round-trip from every numeric path that had one. `magnitude`/`sqrt`/`floor`/`ceiling`/`truncate`/`round` and the `floor-`/`truncate-quotient`/`remainder` family take exact `big.Int`/`big.Rat` paths — `(floor-quotient (expt 10 30) 7)` used to saturate at int64. `pkg/values/big_transcendental{,_complex}.go` adds arbitrary-precision `BigPi`/`BigAtan2`/`BigExp`/`BigLog`/`BigSin`/`BigCos`/`BigAsin` and the seven complex twins, because `math/big` has native `Sqrt` but no transcendentals; `BigSin`/`BigCos` scale working precision to the argument's exponent (the big analogue of Payne–Hanek), so large exact args reduce correctly rather than best-effort. **Tier rule**: unbounded-tier input (BigFloat/BigInteger/Rational/BigComplex) yields `*BigFloat`, bounded stays float64 since `math.*` is already Payne–Hanek-correct. Complex kernels run only where `cmplx.*` returns non-finite, so in-range branch-cut behavior is unchanged; `exp` additionally rescues a *bounded* operand whose `math.Exp` overflows (its ~709 threshold sits inside float64 range, unlike the others). Documented caveat: `BigComplexAtan` on the imaginary-axis branch cut returns principal `+π/2` where Go gives signed-zero `−π/2` — reachable only out of float64 range, where `cmplx.Atan` is NaN anyway. Guards: `TestRoundingBigPrecision`, `TestIntegerDivisionBigPrecision`, `TestBigComplexTranscendentalPrecision`, `TestBigComplex_Sqrt`, plus precision-honesty canaries (`4·atan(1)=π`, `BigLog(2)=ln2` to 60+ digits). Designs: `memory/2026-07-07-bigcomplex-angle-atan2-design.local.md`, `memory/2026-07-07-big-transcendentals-design.local.md`. Untouched: the rational-operand remainder float64 truncation (`(floor-remainder 7/2 2)` → `1`, exact is `3/2`) — non-conformant args per R7RS §6.2.6.
- [ ] **Unify `ErrLossyConversion` / `ErrNotAReal` for the imag-drop case** [API design, S, Deferred]: After this PR, two sentinels flag the same underlying condition depending on which surface a caller uses. `ToFloat64Lossless` returns `ErrLossyConversion` (wrapped) for the `!isReal` branch at `values/conversion.go:89-91`; `NumberToFloat64` panics with `ErrNotAReal` at `values/promotion.go:337-338`. The two sentinels carry the same information about the failure but are not interchangeable for `errors.Is` callers. **Options**: (a) reuse `ErrNotAReal` in `ToFloat64Lossless` and reserve `ErrLossyConversion` strictly for rounding loss; (b) accept `ErrLossyConversion` as the canonical sentinel for any precision/component loss and document the historical role of `ErrNotAReal` as a `NumberToFloat64`-only panic discriminator. Revisit when an FFI consumer reports a confusing `errors.Is` mismatch, or when adding a third surface helper that needs to choose between them.
- [ ] **Reconsider `Exact` overload for NaN/Inf identity** [API design, M, Deferred — design choice Q-6]: The `big.Accuracy` slot returned by `ToFloat64WithAccuracy` is overloaded: (a) genuinely lossless rounding, (b) NaN bit-pattern identity, (c) preserved literal infinity. Doc tightening landed in this PR at `values/conversion.go:54-60` (per design Q-6 resolution). Callers screening "is this a meaningful real number?" must use `math.IsNaN` / `math.IsInf` independently. **Trigger to revisit**: a caller reports being unable to distinguish "rounded-but-real" from "NaN-or-Inf" from the tuple alone, OR a fifth `WithAccuracy` helper is added where the overload becomes too costly to maintain. Possible fix: a 4-valued enum `LossKind { Lossless, RoundedBelow, RoundedAbove, NaNOrInf }` replacing `big.Accuracy` at the public surface; would diverge from Go's stdlib vocabulary at the cost of being self-describing.
- [ ] **`ToFloat64Lossless` returns rounded value on error** [API design, S, Deferred]: When the conversion would round, `ToFloat64Lossless` returns `(f, ErrLossyConversion)` where `f` is the lossy float64 result — the caller can use the value if they want; the error is advisory. The nil-input error path returns `(0, ErrNotANumber)`. The asymmetry is real: lossy ⇒ best-effort value preserved; nil-input ⇒ zero. Go convention is "non-nil error ⇒ value is unspecified," which the lossy path softly violates. **Decision deferred**: changing this would force every strict-mode caller to abandon their value on rounding (which they wanted to fail-fast on anyway). Document the contract instead. Revisit if a caller reports relying on the "use the rounded value alongside the error" pattern in a way the API should officially support, OR if the asymmetry causes a bug at an FFI boundary.
- [ ] **`ToFloat64WithAccuracy` nil-defense: error vs panic** [API design, S, Deferred]: The function returns `ErrNotANumber` (wrapped) when `n == nil`. The signature is `n Number`, so a non-Number cannot be passed — the nil case is the only reachable error path. The neighboring `LookupNumericSpec` (`values/numeric_registry.go:163-169`) panics on analogous defensive bugs (out-of-range kind). The split is a style choice: errors-for-FFI-safety vs panic-for-Go-bug. **Revisit when**: (a) the wider codebase converges on one convention for "this should never happen" defensive checks at the `values` boundary, OR (b) an FFI consumer demonstrates a real path where `n == nil` is reachable from outside the type system (unlikely but possible via reflection paths).
- [ ] **File naming: `conversion.go` lacks `numeric_` prefix** [Tech debt, S, Deferred — taste call]: Other numeric-domain files in `values/` use the `numeric_` prefix (`numeric_kind.go`, `numeric_registry.go`, `numeric_tower.go`); test files follow suit (`numeric_dispatch_test.go`, `numeric_lattice_test.go`). The new `conversion.go` / `conversion_test.go` lacks the prefix. Counter-evidence: `promotion.go` is also unprefixed and lives in the numeric domain, so the convention isn't universal. Rename to `numeric_conversion.go` / `numeric_conversion_test.go` if `promotion.go` is also renamed for consistency, or leave both alone. Revisit when a third unprefixed numeric file is added — the convention either solidifies or breaks definitively.
- [ ] **Symbol-singleton location: `symbols_accuracy.go` separate vs co-located** [Tech debt, S, Deferred — taste call]: Prior art for state-symbol singletons in `values/` (`SymbolThreadNew` etc. in `thread.go:54-59`, `SymbolMutexNotOwned` etc. in `mutex.go:30-31`) puts them in the file of the owning type. `SymbolAccuracyBelow`/`Exact`/`Above` live in their own `symbols_accuracy.go`. The split is defensible (the symbols paraphrase `big.Accuracy`, not a Wile type; the natural owner would be `big_float.go` or `numeric_registry.go`, neither of which is a clean fit). Revisit if a third orphan symbol-set appears (then either consolidate all orphans into a single `symbols.go`, or formalize the per-domain-file convention). Update `values/CLAUDE.local.md` "Sentinel/Singleton Values" inventory either way.

### Postponed

Items deferred for stated reasons. Re-evaluate when preconditions change.

- [ ] **F11: Promote internal extensions** [Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. Structural difference is semantic, not accidental.
- [ ] **VM dispatch loop extraction** [Postponed]: `MachineContext.Run()` is 547 lines with 65 inlined opcode cases. Go has no computed goto; method dispatch adds measurable overhead on hot path. Intentional performance-over-readability trade-off.
- [ ] **Match: consolidate bytecode type files** [Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Postponed]: Requires design decision on canonical pattern.
- [ ] **Schemeutil: grab-bag reorganization** [Postponed]: Moving functions risks import cycle issues.

### plans/ sweep — refactor & tech-debt deltas (2026-07-21)

Open restructuring work found only in `plans/` during the 2026-07-21 triage.

- [ ] **Extension annotation-coverage gaps** [Tech debt, M, mechanical]: the contract enforcement
  stack shipped, but the annotations themselves are incomplete — charsets 0/20, system 2/6,
  eval 13/16, introspection 5/8, threads 27/30, io 38/41. This is the concrete backlog behind the
  "Extension API contracts Phase 2+" item in Tier 2.
  `plans/2026-03-26-extension-contracts-{impl,phase2-design}.local.md`.
- [ ] **Frame-reclaim precision Phases B/C/D/G** [Perf-precision, PAUSED]: sound-escape sibling (B),
  `OpSelfTailCall` v2 at depth>0 (C), local-recursion release for named-let/letrec (D),
  quasiquote unquote-aware reject (G). Value core (A/E) shipped; arc paused 2026-06-22, resume
  gated on a workload profiling as frame-leak-bound. Superset of the "B3 effective capture" item in
  Tier 4. `plans/2026-06-18-frame-reclaim-precision-coverage.local.md`.
- [ ] **`iter.Seq` Tier-2 defensive-copy accessors** [Tech debt, M]: 10 steps, sequenced after a
  charsets structural refactor ships. `plans/2026-05-05-iter-seq-cascade.local.md`.
- [ ] **Staff-sweep structural residuals** [Refactor, M]: `CompileTimeContinuation` God-object,
  `engine.go` facade, "N parallel tables" (ValueType ×5, docparse ×3), parser cluster. The `[S]`
  findings + Tier-1 already shipped. `plans/2026-07-01-staff-engineer-sweep.md`.
- [ ] **Unscheduled design-only notes** [Refactor/architecture]: engine-services generic keyed slot
  (`plans/2026-07-10-engine-services-generic-keyed-slot-design.local.md`), layered-environment
  architecture direction (`plans/2026-06-13-layered-environment-architecture.local.md`),
  data-driven promoted-primitive inline registry
  (`plans/2026-06-26-promoted-primitive-inline-registry.local.md`, DRAFT v2 awaiting review).
- [ ] **`docs/` audit sweep + §4 review audit AU.1** [Docs/verification]: the docs-subsystem sweep
  (`plans/2026-04-23-docs-sweep-impl.local.md`, follows algebra-docs) and the one unstarted audit
  task in `plans/2026-07-15-review-2026-07-13-sec4-remediation.md` (22/23 closed).

> **Stale-status housekeeping (planlint evidence).** Three sizable plans show 0/all-unchecked
> boxes but are fully shipped — the checkboxes lie. Archive to `memory/` and stop trusting their
> boxes: `2026-07-17-review-remediation.md` + `-impl.md` (all 14 defects landed, TODO already
> RESOLVED) and `2026-07-12-numeric-zero-and-tier2-fold.local.md` (Tier-2 fold + exact-zero cluster
> landed). These are exactly the drift the `make planlint` items (Top Priority + Tier 3) target.

---

## Tier 6 — Nice-to-Haves

No demand signal. Speculative or research-only.

### Tooling
- [ ] **Hygiene debugging** [Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.local.md`
- [ ] **Macro expansion tracing** [Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.local.md`
- [ ] **Programmatic tokenization/parsing**: Expose tokenizer/parser to Scheme. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Event callbacks**: Hooks for expansion, compilation, debugging. IDE integration, profiling.

### Standard Library
- [ ] **Hashtable SRFI compliance**: Current custom API (10 primitives) doesn't conform to any SRFI. Gaps vs SRFI-125: no custom hash/equality, no `hash-table-update!`, no fold/map, no immutable variant, naming uses `hashtable-*` not `hash-table-*`. Decide: SRFI-125 or keep custom.
- [ ] **Logging library**: Levels, structured output, handlers.
- [ ] **Go AST Phase 3 — Comments & generics** [S]: `Comment`/`CommentGroup` attachment, `BadExpr`/`BadStmt`/`BadDecl` error recovery, `IndexListExpr` for generics. Owned by [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans); the former `plans/GO-AST.md` moved with it.
- [ ] **`continuation-mark-set-first` accepts `#f` for mark-set** [XS, Racket-compat]: Racket lets `#f` stand in for "current continuation's marks" as the first argument; Wile's `PrimContinuationMarkSetFirst` (`registry/core/prim_cont_marks.go:54`) hard-requires `*machine.ContinuationMarkSet` via `RequireType`. One-branch fix: check `values.FalseValue` before the type check and substitute `mc.CollectContinuationMarks(machine.DefaultPromptTag)`. Surfaced by the audit findings crosscheck on PR #673; no demand signal yet. Defer until the audit's Phase 4 (axis C — Racket compliance sweep) or a real consumer asks.

### Core Language
- [ ] **Type system**: Covers base types, expandable. Discover useful type properties. Types as distinct top-level concept.
- [ ] **let-syntax*** [S]: Implement `let-syntax*`.
- [ ] **Scribble-style `@` reader notation** [Reader extension]: Racket-style at-expressions for rich documentation markup. `@cmd[datum ...]{text ...}` desugars to S-expressions.

### Architecture
- [~] **Dialect system** [In progress]: forms layer SHIPPED (SP1 per-engine codegen fork, `WithDialect`, `DefaultDialect`). Primitive-level control SHIPPED (`PrimitiveRemover` + `BootstrapProcedureRewriter` capabilities; `NoMutation` removes ALL 13 mutators genuinely — mutating `vector-map`/`string-map` swapped for a mutation-free bootstrap fragment; inline-HOF optimizer gated on `requires` so removal deopts cleanly). `NoMutation` is the one shipped leaf dialect; it exercises the forms seam (removes `set!`) plus both cross-ceiling capabilities. NoMutation import-reexpose remains a documented language-surface boundary (dialect ≠ sandbox), not a gap. The demo leaves `R5RSStrict` and `R7RSMinimal` — and the `DisableExpandForm` expander gate that only R5RSStrict used — were pruned once no product consumer wanted restricted-surface engines; the seam + `NoMutation` remain. `plans/ARCHITECTURE.local.md`
- [ ] **Plugin shadowing** [Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.local.md`
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined.
- [ ] **User labels/tags for FS resolvers**: Distinguish bootstrap from include/library loaders in fileResolver.

### Testing & Quality
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`stdlib/lib/*/test/`), new test cases.
- [ ] **Parser unit tests**: Unit tests for parser.

### Content
- [ ] **Blog area in repo**: Git blog area.
- [ ] **Finish blog article**: Scheme for sandboxing.

---

## Documented Exceptions

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Investigated & Rejected

These items were investigated and determined not to warrant changes:

- [x] **Promoted op table**: Table-driven dispatch regressed ~1.5% geo mean (15/16 Gabriel benchmarks slower). Go compiles contiguous-integer switches to jump tables; table-driven adds overhead. `memory/2026-04-05-structural-reduction.local.md`
- [x] **PrimitiveSpec dead fields (D1)**: Originally 5%/2% usage. Extension contracts Phase 1 populated both broadly. No longer dead.
- [x] **ForeignClosure redundant fields (D2)**: `doc` duplicates `PrimitiveSpec.Doc` but costs ~3.2KB total, set once, cannot diverge. Removing requires circular import workarounds.
- [x] **Namespace root/child state waste (D3)**: ~6 nil fields in children. Not worth splitting — zero-value costs nothing, children are rare.
- [x] **LocalIndex / BindingID overlap (D4)**: `LocalIndex` is relative (slot+depth), `BindingID` is absolute (frame pointer + slot). Both needed.
- [x] **Binding/BindingMeta (FCA)**: Clean lazy-initialization pattern. FCA false positive.
- [x] **PrimitiveRegistration/PrimitiveSpec (FCA)**: Orthogonal concerns properly separated. FCA false positive.
- [x] **CompileTimeCallContext (FCA)**: 2-field value type parameter, not coupling. FCA false positive.
- [x] **Opcode resource limits**: Per-category limits (match steps, expand steps, continuation copy depth). Existing mechanisms sufficient: `WithMaxCallDepth` bounds recursion, `WithMaxStackSize` bounds stack growth, `context.WithTimeout` checked every 1024 ops in VM and match loops. Deterministic per-category budgets are niche; timeout is an adequate proxy.

---

## FCA Assessment

Detailed staff-engineer assessment of cross-boundary coupling. Actionable items extracted into Tier 1 and Tier 5 above.

<details>
<summary>Full FCA findings (click to expand)</summary>

**[Priority: High] — vmState field-addition has 6 unguarded copy sites**

Where:
- machine/machine_context_continuation.go:31-224 (Restore, RestoreAndRelease, PopContinuation, SaveContinuation)
- machine/machine_continuation.go:96-113 (NewMachineContinuationFromMachineContext)
- machine/machine_continuation.go:157-183 (Copy)
- machine/machine_context.go:93-110 (NewMachineContext)

What: Adding a field to vmState requires updating 6 functions across 3 files, each with different copy semantics (transfer, clone, skip, force-false). No compile-time guard ensures all sites are updated. The documentation table at vm_state.go:78-93 is the only safety net — and it's comments.

Why it matters: Every field added has to be reasoned about independently at each site. The envPooled column alone has four different behaviors. The marks field uses cloneMarks in some paths and direct assignment in others. Miss one site → silent state corruption.

---

**[Priority: High] — No two transfer operations agree on which fields to copy**

Where: machine/machine_context_continuation.go — all four operations

What: Save, Restore, RestoreAndRelease, Pop each copy a different subset of vmState. The non-uniformity isn't accidental — each deviation is a semantic decision documented only in comments. vmState is treated as three implicit partitions (always-transfer, conditionally-transfer, never-transfer) but there's no type-level encoding.

The deeper issue: The evals field alone has four distinct ownership modes across the four operations.

---

**[Priority: Medium] — Opcode extension requires 7 coordinated edits**

Where:
- machine/opcode.go (constant + table entry)
- machine/machine_context.go:305-329 (dispatch switch)
- machine/native_template.go:129+, 256+ (both conversion directions)
- machine/operation_*.go (new Operation type)
- machine/compilation/*.go (compiler emission)
- machine/peephole.go (if fused)

What: Adding a new opcode touches 7 mandatory sites. The bidirectional conversion switches must stay synchronized.

---

**[Priority: Medium] — LocalEnvironmentFrame pointer ambiguity**

Where: environment/local_environment_frame.go:29-33, environment/environment_frame.go:93-108

What: LocalEnvironmentFrame is embedded by value in EnvironmentFrame (for heap savings), but NewLocalEnvironment() returns *LocalEnvironmentFrame (heap-allocated). Same type, two ownership semantics.

---

**State of the Code**: Wile's machine package is well-documented and intentionally designed, but carries real evolution risk in vmState transfer operations. The CESK architecture is sound. The debt isn't in the abstraction — it's in hand-unrolled field copying where each of 6 functions implements a different subset of a 12-field copy with different ownership semantics, guarded only by a comment table.

</details>

---

## Completed

<details>
<summary>Completed items (click to expand)</summary>

### Bugs & Correctness
- [x] **Peephole optimizer double-restore** [Fixed]: `savedCont` pointer-identity guard. `memory/OPTIMIZER-FIX.local.md`
- [x] **Degenerate form pipeline tests** [Done]: Full-pipeline tests for all core special forms. PR #571.
- [x] **Sub-context winding stack inheritance hazard** [Fixed]: Constructor parameter requirement. `machine/machine_context_subcontext.go`.
- [x] **`cond-expand (library ...)` bypasses FileResolver** [Fixed]: `machine/compilation/features.go`.
- [x] **syntax-rules ellipsis and hygiene bugs** [Fixed]: Three bugs — scope-aware duplicate binding detection (PR #607), cross-group ellipsis zipping, nested ellipsis depth tracking (PR #606).

### Refactoring
- [x] **`WalkSubExprs` for validated expression traversal** [Done]: `ChildRole` enum, B1 capture analysis migrated.
- [x] **Extract interface types from `environment/` `any` fields** [Done]: 15 type assertions removed across 7 files. `memory/2026-03-31-environment-any-fields.local.md`
- [x] **`Stack.Pull()` O(1) replacement** [Done]: `PullDrain()` in `OpPullApply`. `memory/2026-03-31-pulldrain-design.local.md`
- [x] **Split `ffi.go` by concern** [Done]: 1010 lines → 4 files. PR #599.
- [x] **Engine initialization order invariant** [Done]: 6-step DAG documented. `memory/2026-04-01-engine-init-order.local.md`
- [x] **`machine/` mega-package decomposition** [Done]: PRs #592, #593. `memory/2026-03-30-machine-decomposition-design.local.md`
- [x] **`file_resolver.go` chain of responsibility** [Done]: 541 → 469 lines.
- [x] **Timing-dependent concurrency tests** [Done]: PR #602. `memory/2026-04-01-timing-dependent-tests.local.md`
- [x] **ExpanderTimeContinuation convention deviations** [Done]: 18 deviations fixed.
- [x] **Opcode metadata consolidation (D5)** [Done]: `OperandKind` enum. `memory/2026-04-05-structural-reduction.local.md`

### Tech Debt
- [x] Task 1.1: `uint16` source table index overflow → `uint32`
- [x] Task 1.2: Opcode round-trip exhaustiveness test (already existed)
- [x] Task 1.3: Extension list consistency test (already existed)
- [x] Task 1.4: Eval stack size limit — `WithMaxStackSize(n)`. `memory/2026-04-11-eval-stack-limit-design.local.md`
- [x] Task 4.2: Security gate integration tests (already existed)
- [x] Task 5.1: `NamedCallable` interface
- [x] Task 5.2: `StringOrFalse` helper. PR #609.
- [x] Task 5.3: `ForEachList` for proper-list enforcement. PR #609.
- [x] Task 5.4: `requireSourceContext` helper. PR #609.
- [x] Task 5.5: `RequireArg[T]` migration (5 sites, 3 intentional deviations). PR #609.
- [x] Task 6.1: Delete `runtime/` package
- [x] Task 6.3: Receiver naming normalized. PR #609.
- [x] Task 7.1: Unified `machine/testutil` into `registry/testhelpers`. PR #609.
- [x] Task 8.3: REPL decoupled from `machine/compilation`. PR #639. `memory/2026-04-11-repl-decoupling-design.local.md`
- [x] Task 8.5: `prim_eval.go` funneled through `NewSubContext`. PR #637. `memory/2026-04-11-eval-subcontext-design.local.md`

### Performance
- [x] **GC pressure reduction** [Done]: -8.9% geo mean. PRs #562-563. `memory/GC-PRESSURE-REDUCTION.local.md`
- [x] **Core-let compilation** [Done]: PR #570. `memory/CORE-LET-IMPL.local.md`
- [x] **Procedure inlining** [Done]: PR #605. `memory/PROCEDURE-INLINING.local.md`
- [x] **B2 escape analysis** [Done]: PR #604. `memory/ESCAPE-ANALYSIS.local.md`

### Features
- [x] **Algebra library** [Done]: `(wile algebra)`. 158 tests. `memory/2026-03-25-algebra-library-design.local.md`
- [x] **`(wile algebra polynomial)` library** [Done]: Ring-parameterized univariate polynomials. 12/12 tasks. 60 tests passing. poly-plus/negate/minus/times, Horner evaluation, formal derivative (characteristic-safe, O(n) via accumulator threading), divmod (field-required), GCD (Euclidean, monic-normalized), polynomial-ring capstone (enables recursive rings R[x][y]), `with-polynomial` macro. Commits `69b98203`..`78bb7e2f`. `memory/2026-04-18-polynomial-library.local.md`
- [x] **`(wile algebra matrix)` library** [Done]: Semiring-parameterized matrix algebra (§5.1 of foundations). Path D implementation across 10 phases: sparse/dense representations, dispatch-table rep-tags, bang-first arithmetic, aliasing enforcement. Test count 112→303. PRs #684–#691 (P2–P10), #695 (error attribution), #696 (N1–N9 crosscheck follow-ups). `memory/2026-04-20-algebra-matrix-impl.local.md`, `memory/2026-04-21-matrix-path-d-impl.local.md`.
- [x] **`(wile algebra incidence)` library** [Done]: Möbius/incidence algebra on locally-finite posets per Rota (1964) (§5.2 of foundations). Formalizes ad-hoc direct-vs-transitive handling across four wile-goast posets (dominator trees, subtype lattices, call-graph reachability, import DAGs) and belief-DSL overlap normalization. `<locally-finite-poset>` with `(leq? interval-proc)`, ring-parameterized with `(integer-ring)` default, lazy memoization via `equal?`-keyed hashtable. ~200 LOC, ~25 tests. Commit `4ff8a314`. `memory/2026-04-21-incidence-algebra-impl.local.md`.
- [x] **`(wile algebra unification)` library** [Done]: AC-matching and AC-unification per Eker/Stickel/Contejean–Devie (§5.3 of foundations). `ac-match`, `ac-unify`, `<pattern-var>` records, substitution suite, `diophantine-basis`. `ac-unify` returns CSU (finitary-not-unitary per Fages–Huet 1986). PR #698 (30 commits on `feat/algebra-unification`). `memory/2026-04-21-ac-matching-design.local.md`, `memory/2026-04-21-ac-matching-impl.local.md`.
- [x] **SRFI-14 + `(wile charsets)`** [Done]: 17 FFI primitives + 23 derived Scheme procedures + 17 named char-sets (`char-set:letter`, `char-set:digit`, `char-set:whitespace`, etc.) sourced from Go's `unicode` tables; inversion-list representation, fully immutable. Char-set criteria enabled across 7 SRFI-13 procedures (`string-index`, `string-skip`, `string-count`, `string-trim*`, `string-tokenize`, `string-filter`, `string-delete`). `(wile charsets)` exposes `char-set-ranges` for efficient iteration. PR #TBD. `memory/2026-05-04-srfi-14-design.local.md`, `memory/2026-05-04-srfi-14-impl.local.md`.
  - **Completeness follow-up (Track A4) [Done]:** n-ary zero-arg identities (6F) + the previously-deferred cursor protocol, hash, and diff+intersection (6G). `char-set-union`/`-intersection`/`-xor` now return their identity element on zero args (empty/full/empty) via Scheme wrappers over `%char-set-*` folds; `char-set-difference` keeps its ≥1-arg arity (no identity per SRFI-14). All seven formerly-deferred names now implemented and exported: `char-set-hash` (content-stable, bounded, O(#ranges) over the canonical inversion list), `char-set-cursor`/`char-set-ref`/`char-set-cursor-next`/`end-of-char-set?` (cursor walks the inversion-list ranges; defensively skips the U+D800–U+DFFF surrogate block that `integer->char` rejects), and `char-set-diff+intersection`/`!`. `pkg/stdlib/lib/srfi/14/{algebra,cursor}.scm`, tests in `integration/testdata/srfi-14-tests-{algebra,cursor}.scm` + `extensions/charsets/charsets_test.go`. NOTE: the surrogate construction-time invariant (6D) remains deferred — only iteration is guarded.
- [x] **SRFI-13 + `(wile strings)`** [Done]: 60 SRFI-13 procedures + `string-trim-left` alias + 5 `(wile strings)` extras (`string-split`, `string-replace-all`, `string-byte-length`, `string-blank?`, `string-repeat`); 309 integration tests across 8 phases. All pure Scheme; FFI promotion deferred (profile-driven §6 of design). `(wile strings)` resolves the SRFI-13 vs R7RS `string-map` shadowing via `(except (scheme base) string-map)`. Char-set criteria enabled by SRFI-14; `string-titlecase`, `string-hash`, `string-unfold`, `xsubstring`, `*/shared` forms also deferred per §11 of design. PR #721. `memory/2026-05-03-string-primitives-design.local.md`, `memory/2026-05-03-string-primitives-impl.local.md`.
- [x] **Documentation system** [Done]: Full infrastructure — `,doc`, `,apropos`, `,topics`, `,topic`, library descriptions, docstring examples. PRs #579-591.
- [x] **MCP server** [Done]: `wile --mcp`. PR #588. `memory/2026-03-26-wile-mcp-server-design.local.md`
- [x] **`(available-libraries)` primitive** [Done]: PR #590. `memory/AVAILABLE-LIBRARIES.local.md`
- [x] **OpaqueValue type** [Done]: Generic opaque wrapper for Go objects in Scheme.
- [x] **Disassembler** [Done]: `(disassemble proc)`, `,dis`, MCP tool. PR #603.
- [x] **Go AST Phase 2** [Done]: 13 node types. PR #480. Plan moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans).
- [x] **Climbing macro tower — Tier 1** [Done 2026-07-10, branch `feat/climbing-tower-tier1`]: A phase-*N* macro whose transformer body defines and uses further macros climbs to *N+1*, *N+2*, … via relative phase accessors (`EnvironmentFrame.NextPhase()`) at the four macro-resolution sites (transformer-body compilation, define-syntax storage ×2 incl. the internal-body path, macro lookup ×2, begin/define-for-syntax + import placement). `phaseLevel 0` is byte-for-byte identical to pre-tower (level-0 identity). Pinning RED→GREEN: `TestClimbingTower_CrossPhaseCollision` (a name reused at two phases no longer collapse-clobbers). **Finding:** the feature affects *procedural* macro-writing-macros, NOT the declarative majority (declarative inner macros live in expansion *output* = same phase as use, always consistent — the plan's two/three-storey corpus is green with and without the tower, kept as level-0-identity guards). Bindings are shared, not per-phase-instantiated (Tier 2, gated). Task 6 (`GetGlobalIndexAcrossPhases` climbed-band) was a **no-op** — the R7RS `jabberwocky`/`march-hare` carve-out survives the climb unchanged. `docs/compiler/macro-system.md` §Phase Tower. `plans/2026-07-10-climbing-tower-{design,impl}.local.md`.
  - [ ] **Q4 mutation boundary (`ErrCrossPhaseMutation`) — BLOCKED, deferred**: Rejecting a `set!`-mutated binding shared across a climb (design option (b)) is blocked at the impl plan's Task 7 Step-0 gate: mutation-reachability is a whole-unit property computed in the validator pass and is NOT queryable at the cross-phase resolution site (`GetGlobalIndexAcrossPhases` / `compile_syntax_rules.go:342`), and a flag-based approximation false-positives on the (unmutated) carve-out (top-level defines aren't `Stable`). Ships as option (a) (silent share) — not a regression (master had no tower). Reachability of a genuine cross-phase mutable-sharing footgun is itself unproven. Natural home for the check: the Boundary-2 phase-precise use-time resolution rework (design §7.3), where the mutation gate and the `[0,1,2]` sweep would retire together. `plans/2026-07-10-climbing-tower-q4-mutation-boundary-note.local.md`.

### Other
- [ ] **Important refactoring**
    - When few fields are referenced from a struct within a function, pass in the field - do not pass in the struct or a reference to the struct

- [x] **Promote `eval` extension to public** [Done]: Moved `internal/extensions/eval/` → `extensions/eval/`, importable as `github.com/aalpar/wile/extensions/eval`. Required by wile-goast and any embedder wanting sandboxed `(eval ...)` / `(load ...)`. The naive composition `WithProfile(Console) + WithExtension(eval.Extension)` does **not** work — `ConsoleAuthorizer` denies `code:load`, so `(load ...)` fails. The fix is a baked `ConsoleWithLoad` profile (extensions + matching authorizer that allows `code:load` under `/tmp`), now part of `memory/2026-03-26-environment-profiles-impl.local.md`.

- Update skills to explicitly state where wile-goast is a fit for refactoring.  Add guidance.
- Add guidance to skills where Serena use makes sense.
- [x] **Reader fixes** [Done 2026-07-31, branch `feat/reader-hash-dispatch`]:
  three additions to the `#` reader space, plus the delimiter prerequisite the
  design missed. Design + spec evidence + constraints:
  `plans/2026-07-31-reader-hash-dispatch-model.local.md`. Phased implementation
  and the three decisions (all resolved as recommended — D1-a, D2-a, D3-a):
  `plans/2026-07-31-reader-hash-dispatch-impl.local.md`. The tests that were RED
  (`pkg/parser/{bigint_radix,box_read,float_radix,precision_marker}_test.go`:
  17 functions, 73 subtests) are green, and the whole suite is green.
  The governing rule is an invariant in `CLAUDE.local.md` → "`#` Reader
  Dispatch", with detail in `pkg/internal/tokenizer/CLAUDE.local.md` and
  `pkg/parser/CLAUDE.local.md`; user-facing docs in
  `docs/reference/r7rs-differences.md` and `docs/numeric/tower.md`.

  **Four defects surfaced during implementation that the plan did not name**,
  each fixed in the phase that exposed it:
    - `#e#x1.8` was 9/5 while `#x#e1.8` was 3/2. `#e` is "the number as written"
      and re-read the text as a *decimal* rational; it now defers a non-decimal
      literal to the value (`makeExactLiteral`).
    - `#x.f` scanned as the peculiar identifier `.f` — a-f are dot-subsequents as
      well as hex digits, and the symbol test ran first.
    - `read()`'s leading-dot arm hardcoded base 10 and never reset the radix, so
      the 16 in `(#x.8 19)` leaked and 19 read as 25.
    - `#0=#&#0#` reported "undefined datum label": `readLabelAssignment` read the
      whole datum before registering the label. It now pre-registers a box
      placeholder, as the list and vector arms do.

  Two behaviour changes worth knowing about, both deliberate:
    - `#x1f#t` is now an error. `readHashDigits` eats the `#` as an R7RS §7.1.1
      inexact-digit placeholder (the token is `1f#` = 496.0), leaving `t`
      adjacent to the numeral.
    - A `BigFloat` writes `l` instead of `e`, so `1e+1000` renders as `1l+1000`
      and `#m1.5` as `1.5l0`. Inside a complex the marker is omitted.
    - **Prerequisite the design missed: out-of-radix digits are not an error.**
      `#b19` reads as `1` and leaves `9`, so `(#b19)` is `(1 9)`; the tokenizer
      treats a digit outside the radix as a token boundary. Both digit-validation
      RED tests are unreachable until this is fixed, and `#b1.9` errors today
      only by accident. Scoping fork (radix-only vs. all numerals) is decision D1
      in the impl plan.
    - **BigInteger readers accept radix (eg, `#z#x`) tags.** Resolved as a
      *datum introducer*, not a third prefix slot: `#z <number datum>` reads the
      datum with the ordinary number reader and widens it, so radix, exactness,
      and digit validation are all inherited (`#z#x1f` = 31; `#z#e#x1f` works
      without `#z` knowing `#e` exists; `#z#b19` fails because `#b19` fails).
      Constraints: `#x#z1f` stays an error (radix is lexical, so its operand must
      be a literal) while `#e#z9` / `#i#z9` already work and must keep working
      (exactness is post-hoc); `#z#z5` is 5, not a nested anything; the datum
      must be an exact integer, so `#z1.5` and `#z#x1.8` are errors. `#m` gets
      the same shape. **Doc edit owed on landing:**
      `docs/reference/r7rs-differences.md` says `#z` "does not combine with the
      radix prefixes … in either order" — half of that goes wrong.
    - **Boxes can be read.** `#&5` is a box containing 5. The write side already
      exists (`values.PrefixBox`, `writeBox`), so this is a broken round trip,
      not a new feature. `#&` is an introducer, so `#&#x1f` is `#&31`. Must also
      read behind a datum label (`#0=#&…`, `#n#`) because the writer already
      emits them for shared and cyclic boxes — Racket accepts this, Chez does
      not, and matching Chez would leave Wile's own output unreadable.
    - **Floats (including BigFloats): radix for floating point reading set by
      radix tag.** `#x1.8` = 1.5, `#b1.1` = 1.5, and the same under `#m`.
      Extension, not conformance: R7RS §7.1.1 defines `⟨decimal R⟩` only for
      R = 10. Racket, Chez, and MIT all support it.
    - **Exponent markers denote precision, on read and output.** Verified against
      R7RS §6.2.5 (p.34) — the markers are an *optional* extension, and an
      implementation with fewer than four inexact representations maps the four
      size specs onto what it has. Wile has two, so `s`/`f`/`d`/`e` → `Float`,
      `l` → `BigFloat`. Output too: `1e+1000` becomes `1l+1000`, and because
      `BigFloat` currently writes `#m1.5` as bare `1.5`, round-tripping forces a
      marker where none is emitted today. Pinned as a *property*
      (`TestBigFloatWriteReadRoundTrip`), not a spelling. Note Wile will be the
      only Scheme where `l` selects a different representation — all three
      reference implementations collapse the four markers to one flonum type.
      **Exponent markers stay decimal-only** (decided): `#x1e2` is 482, since
      `e` is a hex digit. This is the R7RS §7.1.1 reading and diverges from all
      three implementations, which accept `#x1s3` = 4096.0 with the exponent
      base being the *radix*.
    - Not adopted, recorded for later: R6RS mantissa width `x|p` (r6rs.pdf p.16),
      which Chez and MIT implement (`1.1|24` → 1.100000023841858). A bit count
      maps onto `big.Float`'s precision parameter more directly than a
      four-letter code, and complements `l` rather than competing with it.

- [ ] **`BigFloat` rendering hangs on a huge exponent** [Low, S; found by
  `FuzzReadWriteRoundTrip` 2026-07-31, **pre-existing**, not caused by the reader
  fixes]: `(write 1e10000010000000)` takes ~11 s, effectively all of it inside
  `big.Float.Text('g', -1)`, which renders the value to roughly 10^7 decimal
  digits before formatting discards nearly all of them. Measured at 11.34 s on
  `47ae48dc` and 11.16 s on `feat/reader-hash-dispatch`; master's fuzzer reaches
  the same class of input within ~14 s, so both branches are equally affected.
  Reproducer: `"#I100000000E0000010000000"`. Deliberately **not** committed to
  `pkg/parser/testdata/fuzz/`, where it would add 11 s to every
  `go test ./pkg/parser/`. Note the artifact the fuzzer writes for this is
  *not* a reproducer: the minimizer times out too, so the saved file holds a
  partially-minimized candidate that passes when re-run. Fix is presumably a
  magnitude bound in
  `BigFloat.SchemeString` (render an exponent form directly rather than asking
  `Text` for shortest-round-trip digits), but the writer's contract needs
  deciding first: what should `write` emit for a value with 10^7 digits?

- [ ] **Delimiter termination for decimal numerals** [Low, S; residual of
  "Reader fixes" decision D1]: `1abc` scans as `1` followed by the symbol `abc`,
  because a numeral is only implicitly terminated when it carries an explicit
  radix prefix (`requireDelimiterAfterRadixNumeral`, `pkg/internal/tokenizer/`).
  R7RS §7.1.1 reads as though *every* numeral requires a delimiter ("tokens which
  require implicit termination … may be terminated by any ⟨delimiter⟩"), so the
  present split is a real semantic difference between decimal and radix numerals,
  introduced for scoping reasons. Extending the guard to `r == 0` is a one-line
  change; the work is measuring the blast radius, which reaches the fuzz corpus
  and every `.scm` under test. Deliberately not bundled with the reader-fixes
  change, which is a three-item feature and would have conflated the two.

  The `#z` / `#m` inline scan is the same family and is **worse**, because it
  truncates silently instead of splitting into two visible datums. `readBigNum`
  scans base 10 and stops at the first character it does not recognize, so:

  | Input | Reads as | Leaves |
  |---|---|---|
  | `#z1/2` | `1` | `/2` |
  | `#m1/2` | `1.0` | `/2` |
  | `#m2+3i` | `2.0` | `+3i` |

  Verified identical on `47ae48dc` and on `feat/reader-hash-dispatch`, so this
  is not introduced by the introducer rework — the degenerate `#z<digits>` path
  is unchanged. Arguably these should be errors (a `#z` operand that is not an
  exact integer is rejected everywhere else: `#z1.5` errors), which makes this a
  narrower and safer fix than D1-b proper.

</details>

