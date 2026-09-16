TODO
----

**Last Updated**: 2026-09-15: closed items collapsed to the archive form again (the 2026-07-27
pass, `2187a734`, had left the file at 167 KB; it had regrown to 476 KB); `plans/` and `memory/`
citations repointed after the 2026-09-14 `.local.md` → `.md` rename. Per-edit history is
`git log -p TODO.md`; release history is [`CHANGELOG.md`](CHANGELOG.md).

### Current Project Status

> Orientation only, and it drifts. Authoritative: [`VERSION`](VERSION) / `wile --version` and
> [`CHANGELOG.md`](CHANGELOG.md) for the release; [`docs/INDEX.md`](docs/INDEX.md) for the
> documentation map; `Engine.AvailableLibraries()` for what a build actually exposes.

**Version**: 1.20.x line
**Core Language**: R7RS-small complete: hygienic macros, composable continuations, numeric tower
**Extensions**: packages under `extensions/` plus internal ones, all importable as `(wile <name>)`.
`bootstrap.ProfileExtensions` (`pkg/internal/bootstrap/bootstrap.go`) is the single source of truth
for which profile grants which. Go static analysis lives in
[wile-goast](https://github.com/aalpar/wile-goast).
**Embedding**: CLI runs on the public Engine API; embedded stdlib via `stdlib.FS`; named profiles
(`Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink`) via `WithProfile`, with `WithSandbox`
as an orthogonal modifier.
**Libraries**: R7RS-small, the chibi/SRFI set, and the `(wile algebra)` umbrella
(`ls pkg/stdlib/lib/wile/algebra/*.sld` for the sub-libraries).
### Ordering

Items ordered by perceived priority for the project's success as an embedding product. Tiers: Security/Correctness → Embedding API → Tooling/DX → Performance → Tech Debt → Deferred → Nice-to-Haves. Completed items at the bottom for reference.

### Conventions

- **Completed items** are marked `- [x]` and include `Done` (or `Done — note`) in the brackets after the difficulty estimate. Example: `[Medium, Done]` or `[Medium, Done — P3 deferred]`. The bracket marker makes completion machine-grep-able alongside the Markdown checkbox; the bracket note may carry a one-line deferral fact when a sub-item is intentionally postponed.
- **Deferred sub-items within a completed parent** are noted parenthetically in the entry body (e.g., *"Phase 10 deferred — benchmark-gated"*) rather than spawned as a separate `[ ]` entry. Re-open as a top-level entry only if the deferral becomes the active work.
- **A closed item keeps one line of result plus what the commit and the code cannot tell you**:
  rejected alternatives and why, traps, load-bearing properties, deciding measurements,
  residuals. Narration of how it was found or fixed goes to the commit message.

### References to `plans/` and `memory/` are intentionally unresolvable here

`plans/` and `memory/` are tracked symlinks (`aec6efe2`) into a private sibling repository,
`../wile-memoplans/`, which holds design reasoning. So every `plans/…` and `memory/…` citation
below is a dead link for anyone but the maintainer, and that is deliberate, not rot. Do not "fix"
them by deleting the citation, inlining the plan, or vendoring the files. No CI target validates
them (`make check-readme-links` and `make check-docs-orphans` do not cover this file), which is
the argument for keeping the Plan Index below current by hand.

A cited plan has one of three homes: `plans/` = live, `memory/` = shipped or abandoned, gone
entirely = moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans). Two
different `memory/` directories exist: the repo's holds archived plans, while Claude Code's
auto-memory lives outside the repo and is cited as "auto-memory `<name>.md`" with no path prefix.
Plan files dropped their `.local.md` suffix on 2026-09-14 (`wile-memoplans` `c3776a3`); a citation
still written `.local.md` is stale by that rename. Repo-side `CLAUDE.local.md` files kept theirs.
## Plan Index

Every file in `plans/`, with the status recorded in the file itself. Sections below hold the
detail; this table is the map.

**Triage traps.** Each has produced false "open" verdicts in past passes, and each is a cheaper
check than the one it replaces:

1. **A status of "not started" on a `-design` note is not evidence the work is unstarted.** Check
   `memory/` for an archived `-impl` twin first.
2. **The index row can be stale about an `-impl` too.** The 2026-08-24 pass filed name-keyed
   residuals Branch B as partial by reading its row; the file's own §Branch B header said it had
   shipped sixteen days earlier (`ab495854`, `0f9b7751`). Trust the file over the row.
3. **Read the merge list, not the prose.** Status sentences here are written when a plan is
   *scheduled*, so they age in one direction; `git log --merges` names the branches. The same
   applies to prerequisites ("still unmatched" after it landed) and to must-fail-first labels
   (written at scheduling, not at build).
4. **A grep that finds the old shape is not evidence the fix is absent.** A name-keyed map can
   answer a syntactic question while the identity decision lives elsewhere; a `[]string` literal
   can be a ratcheted derivation.
5. **Run the repro before believing the row.**
6. **Unticked `- [ ]` boxes in a plan are not evidence.** This repo has never kept them.
7. **`make indexlint`'s plans direction accepts a citation anywhere in this file**, not an index
   row. A green run does not prove the index proper is complete.

**Archive passes** moved every plan whose work had landed, nothing partial, into `memory/`. Each
archived file carries its final Plan Index row verbatim in an ARCHIVED banner, and each pass's
commit message records its judgment calls: 2026-08-14 wave arc (`818f1267`, `d8e71c80`; indexed
by `memory/INDEX-reviews-and-remediation.md`), 2026-08-24 (`ff779a16`, 31 files), 2026-09-04
(`9dc7b072`), 2026-09-12 (`e8bf974a`, 8 files; indexed by
`memory/INDEX-environment-and-phases.md`).

**Closed by.** Review-confirmed files whose only citation lived inside an archived row. Kept
because the citation is what `make indexlint` counts:

| File | Closed by |
|---|---|
| `pkg/values/mutex.go` | W3 item 17: a **rendering** fix (`2062fe37`, `dfd8e230`), plus Phase 3's representation decision (`88312e27`): owner and abandonment are independent axes, `MutexAbandoned ⇒ owner == nil` deleted |
| `extensions/threads/prim_threads.go` | W3 item 16's three defects (35, 38, 40) and item 17's four rewritten assertions |
| `pkg/machine/foreign_closure.go` | W2 decision 7's first half: the `ErrAccessDenied` pass-through arm (`65ed2d8a`), `errors.Is` not `errors.As`; then W4 Phase 8 half C tightened the FFI recover above it |
| `pkg/registry/helpers/list.go` | W4 Phase 6 (5+7 half A, 2026-08-13): the last guard the phase needed |
| `SECURITY.md` | W2 item 12: the missing carve-out |
| `pkg/machine/peephole.go` | 2026-08-21 R1: the promoted-operator fusion checked the *promoted* arity against the call site, so a wrong-arity call could fuse; the guard now reads the underlying primitive's |
| `pkg/wile/dialect.go` + `pkg/machine/compilation/ref_index.go` | 2026-08-21 R2: `ref_index` boxed, so `ForEachOpaqueLiveSymbol` stops handing out a raw index that outlives its table |
| `pkg/security/filesystem_root.go` | 2026-08-21 R4: `FilesystemRoot` defaults to deny rather than to the process's root |
| `docs/extensions/architecture.md` | 2026-08-21 R3's documentation half: the authorizer gates on the **executing** namespace, not the registering one, a behaviour break by design (`b4ba36f1`) |

**Settled by the 2026-08-13/14 wave sweep; do not re-open.**

- Shipped 2026-08-13, after the sweep had filed them open: W4 item 4 (`6b3e1d25`), IM Phase 2b
  (`80c9bd34`) and Phase 3 (`f6e90d4b`, gates `f6974bce`); `noUncheckedArgCast`'s call-argument
  form (`ee90fc24`, 2026-08-10).

- **W6 §8 mechanism (1) shipped as one ratchet per premise, not the §8.3 table.** §8.4's gate
  requires the two named instances, membership in `ci:`, and checked-in inputs, not a table; an
  assertion next to what it asserts cannot be orphaned by a move. Ratchets: `4fc4020f`,
  `2c6dbffc` (`pkg/machine/compilation/compile_symbol_reachability_test.go`) and
  `TestProcedureInvokersMatchesInvokesProcedure`. **Do not re-open this looking for the table.**
- **W4 5+7 half C closed by tightening** (`fix/w4-half-c-ffi-recover`): `ffiSpec.makeWrapper`'s
  recover converts only the callback protocol and re-panics everything else, so a Go-level bug
  escapes `guard` from either registration route.
- **`pkg/wile`'s `RegisterFunc` converter chain has no escapes** beyond the struct converter fixed
  in that branch: 107 probes, held by `pkg/wile/ffi_degenerate_input_test.go` (six routes reach
  the struct arm).
- **A nil `context.Context` is a programmer error; the nil-deref panic is the intended answer**
  (maintainer, 2026-08-14). `NewEngine` / `EvalMultiple` / `MustParse` SIGSEGV while `Parse` /
  `Eval` / `Compile` return normally only because those three never dereference it. Do not
  substitute `context.Background()` or convert it to a typed error.
- Residual: the remaining shape-(2) candidates are all on the `RegisterPrimitive` path, which
  already bypassed `guard`; certifying them needs a `go/analysis` SSA pass, which ruleguard
  cannot be widened into.
- Residual: a parent-scoped `qt.New(t)` aborts a table on its first red row, so
  `panic_channel_gate_test.go`'s shape under-reports. A test-shape defect, not a contract question.

### Open — implementation work with a live plan

| Plan | Status |
|---|---|
| `2026-03-26-extension-contracts-impl.md` | Phase 1 + Phase 4 (runtime enforcement) shipped; Phases 2–3 annotation rollout **partial** |
| `2026-03-26-extension-contracts-phase2-design.md` | Infrastructure + enforcement complete; extension annotations partial |
| `2026-06-18-frame-reclaim-precision-coverage.md` | A/E/B/D + A-local **shipped**, only A-local converted to clock (`primes` −4.2%); B/D moved verdicts, emitted ~nothing; **C deferred** 2026-07-29 (the `or` lowering removed 340 macro frames more cheaply, but reaches a disjoint set — Appendix A); F/G open. **Phase C SHIPPED 2026-08-14.** Its inbound edge was satisfied by Wave 1 §4 (`48a52a3c`), whose fix sits at the delivery instruction rather than in `OpSelfTailCall`, so C inherited it and needed no arity work. **Three corrections to the plan's own Phase C text, all measured:** (1) Step 1's *"assert 0-alloc"* target is unreachable — a `let` frame costs 3 allocations and is not pool-owned, so the landed result is 5.0 → **3.0** for one let and 8.0 → **6.0** for two, removing the fixed 2 the leaked parameter frame cost; pooling `OpPushEnv` frames is a separate, unstarted lever. (2) Q-C1 resolved to **(a)**, a bit-packed `(argCount | popCount<<16)` where `popCount == 0` encodes byte-identically to the old operand — decided not on peephole grounds but because the pops and the rebind must be inseparable, and because `OpPopEnv` clears `envPooled` as a claim about the frame it pops TO. (3) Step 2's *"analysis returns depth"* was **not implemented and should not be**: `tailExprHasSelfCall` stopped counting depth, and the count is carried by the compile-time context at the single site that pairs 1:1 with `compileValidatedLet`'s single `OpPushEnv` — one counter instead of two that must agree, which retires A.7's coupling, and per CALL SITE rather than the per-closure max the plan specified (a procedure with self calls at two depths needs both). **Ledger:** armed sites 47 → 50 over 78 corpus files, none lost; the three are outside the Gabriel suite, which is why the interleaved A/B reads geomean +0.33% against a +0.12% A/A control. A.5's disjoint-frame-sets prediction held. Only F/G remain |
| `2026-07-18-bootstrap-core-unification-and-signals.md` | W1 shipped; remainder open |
| `2026-09-04-registry-core-tests-to-scheme-impl.md` | Executes M1 of `2026-07-31-go-volume-reduction-findings`. Pilot **shipped** on `test/scheme-list-migration` (`prim_list_test.go` → `test/scheme/lists-test.scm`, −1,820 Go LOC, both drivers green). Task 0 (`covercheck` merges the in-process Scheme suite's Go coverage) is a hard prerequisite: Go-only `registry/core` coverage projects to **40.0%** after full migration, union is 90.4%. Tasks 1–17 **not started** |
| `2026-09-04-scheme-coverage-fidelity.md` | Design + plan, **not started**. Follows `fix/cover-library-bodies`: one `TemplateObserver` compile seam so `--cover` reports every Scheme template the engine compiles, attributes macro-expanded code to the template that produced it, and lands the Go-cover profile on the right bytes |
| `2026-07-20-scope-keyed-tier1-remediation.md` | **Partially implemented** (2026-07-21) |
| `2026-07-28-alocal-capture-safe-proof-for-locals.md` | Phases 1–4 **DONE** (1–3 committed on `feat/callback-conditional-capture`); 19/68 → 42/68 capture-safe stamps, reclaim verdicts unchanged at 19/68 as predicted. Phase 5 (Lever B) open. Prerequisite `47c27f72` must land first or the two conflict textually |
| `2026-08-01-scheme-deadline-and-cancellation-cause.md` | Phase 1 **impl-ready**; Phase 2 designed and **gated** on the plan's scoping decision. Exposes the ambient `context.Context`'s deadline and cancellation cause to Scheme |

### Design-only — approved or drafted, no code

| Plan | Status |
|---|---|
| `2026-04-17-mcp-server-sota-design.md` | Proposed, 5 phases |
| `2026-04-21-type-constraint-extension-design.md` | Design draft; impl deferred to follow-ups |
| `2026-06-04-srfi-204-match-design.md` | Design draft; `-impl` to follow |
| `2026-06-05-mcp-llm-support-design.md` | Phase 1 implementation-ready |
| `2026-06-09-polynomial-ideal-domain-design.md`<br>`2026-06-09-polynomial-ideal-domain-impl.md` | Design approved; impl steps unchecked |
| `2026-06-24-tinyclos-object-system-design.md` | Design proposal, opt-in object system |
| `2026-06-24-unboxed-scalar-arithmetic-design.md` | Design **A rejected**; Design B + Phase 4 is the live plan |
| `2026-06-26-promoted-primitive-inline-registry.md` | Draft v2, awaiting human review |
| `2026-07-10-climbing-tower-design.md` | Tier 1 **shipped** 2026-07-10 (`f92568a8`); **Tier 2 (§6) unstarted and gated** on the §9 procedural-authoring pivot plus §10 owner sign-off. The `-impl` twin is Tier-1-only and archived to `memory/` 2026-08-24 — this row is why the design is not |
| `2026-07-10-engine-services-generic-keyed-slot-design.md` | Design only, not scheduled |
| `2026-07-11-scheme-pipeline-seams-design.md` | `current-eval` / `current-print` / `current-read` seams |
| `2026-07-24-free-identifier-origin-provenance-design.md` | Phase 1 + inline-HOF P2 shipped; scoping doc retained |
| `2026-07-29-anonymous-lambda-inlining-impl.md` | Implementation-ready, ~25 LOC; Q1 (arity mismatch: refuse vs compile error) open |
| `2026-08-01-srfi18-sequencer-design.md` | Design proposed, 6 phases, **not started**. Scoped to adversarial protocol testing (not seed-reproducible simulation); ships in production with a FIFO policy. 3 open questions |
| `2026-08-03-algebra-egraph-design.md` | Design proposed, 5 phases, **not started**. Reviewed against code + live evaluator same day; 3 findings changed the design (witness-term extraction, cycle-safe semiring gate, non-hashable e-node keys). Q1 (fund Phase 4, touches shared `rewrite.scm`) and Q2 (metadata policy on congruent merge) blocking |
| `2026-08-03-formspec-expand-consolidation-sizing.md` | Sizing only. **P1 shipped** (`48f6fa25`); P2–P5 deferred as marginal. §6 fork (wire expand dispatch to the field?) open, recommendation NO |
| `2026-09-04-expander-in-scheme-assessment.md` | Assessment, no code. Answers "move all macro expansion, templates included, to Scheme": **A** (`syntax-rules` derived over `syntax-case` in `bootstrap_macros.scm`, retiring the parallel clause compiler + runtime) sized as a Tier 5 candidate, not started; **B** (core expander in Scheme, psyntax/Racket style) **DECLINED** — no image format, so bootstrap cycles and per-`wile.New()` startup pay for it, and the extension benefit is already served. Also closes chibi backlog #5 as already built |
| `2026-09-04-scheme-specified-syntax-forms-design.md` | **DESIGN, direction approved 2026-09-04, not started.** Go keeps a kernel (scope sets, resolution, core forms, body scan, dispatch loop, one transformer protocol with the intro-scope flip, `quote-syntax`, `syntax-local-value`, one-level accessors); `syntax-case`, `syntax`, `with-syntax`, `quasisyntax`, `syntax-rules`, `er-macro-transformer` are specified in Scheme in two new bootstrap sources; `pkg/internal/match/` and ten `compile_syntax_*`/`operation_syntax_*`/ER files (≈6.5k Go) are deleted. Startup is a soft constraint by decision (pre-compiled bytecode later). Three of the six 2026-09-04 macro defects die with the deleted code, one (`define-syntax` head dispatch) is the P0 prerequisite, `begin-for-syntax` visibility is out of scope, and the sixth was closed on paper. Phases P0–P4 in §7; the three open questions in §8 are not blocking. Revised 2026-09-06: 40 verified findings from the adversarial review folded in place (§2.1 item 2's library-scope premise, §2.2 ER pass-through scoping, §3.5 ER `compare` evidence, §3.6 ratchets, §4 inventory, §6 pin-fidelity timing, §7 P0.3/P0.5 pins); author decisions marked in place as `> **Open (2026-09-06 review, …)**` |
| `2026-09-05-scheme-specified-syntax-forms-impl.md` | **IMPL PLAN 2026-09-05. Tasks 1-10 of 12 shipped** (P0.1-P0.5, P1, P2), branch per phase. P2 landed 2026-09-07 WITHOUT its default flip: `syntax-rules` and `er-macro-transformer` are specified in Scheme, but making the Scheme layer the default costs **31x startup** — 5.96 ms/engine and 87k allocations become 186 ms and 1.9M, at ~1 ms per `syntax-rules` clause over bootstrap's ~90 (F15). Both layers stay in the binary behind `WILE_SYNTAX_FORMS` / `WithSchemeSyntaxForms()`, default Go. **P3 (deletion) is blocked on the flip**, and the flip on a cheaper pattern representation or a pre-compiled bootstrap. Executing P2 also found a latent **P1** hygiene defect: `values.AddScopeToSet` aliased the caller's backing array, so two nested expansions of one macro clobbered each other's introduction scope — `(or (or #f #f) #t)` failed to compile and `(wile algebra interval)` was unloadable (F16). Twelve tasks over the design's P0.1–P0.5 and P1–P4, one branch per phase, 62 checkbox steps. §0 carries fourteen corrections to the design: the five review findings of 2026-09-05 (F1 the exclusion set is eight expander rows, ratcheted; F2 the Go producer's definition-site local arm dies at P0.1; F3 `define-syntax` docstrings exist and `,doc` reads them, so the seven `specialforms.go` rows go in P4; F4 the full deletion inventory; F5 `unsyntax`/`unsyntax-splicing` as violation-raising macros) and five measured while planning (F6 `datum->syntax` already copies the template's scopes, only the pin is at stake; F7 `GlobalIndex` is minted per query, so the P0.3 pin compares bindings; F8 P1 must take `with-syntax`/`quasisyntax` with `syntax`; F9 `WILE_SYNTAX_FORMS` as the both-ways default; F10 bootstrap macros are invisible from phase ≥ 2, closed by a kernel arm 2b). Four more (F11–F14) came from the adversarial review of 2026-09-06, folded 2026-09-06: F11 a library body is already stamped with its own scope, so §2.1 item 2's premise was false; F12 `bootstrap_nilpin_test.go` is replaced in P2, not deleted in P3; F13 `chibi/optional.sld`'s ER arm is behind a `cond-expand` Wile never selects; F14 `syntax-local-introduce` wiring belongs to P0.2, forced by `TestSyntaxLocalIntroduceIsNotWired`. That review also fixed four blocking defects: every `EvalMultiple` result in the P0 pin files asserted a `pkg/syntax` type directly on the `wile.Value` wrapper, which does not compile. Six more (F15-F20) came from EXECUTING Task 10 on 2026-09-07: F15 the startup measurement that deferred the flip; F16 the `AddScopeToSet` aliasing defect; F17 the plan's `er-contract` row cannot live in a script that must be green on both layers; F18 the nil-pin census is kept and pinned to the Go layer rather than replaced, with the behaviour rows added alongside; F19 the ER shim needs `%er-proc` to keep the definition-site arity refusal and to evaluate PROC once; F20 `pkg/registry/core/bootstrap.scm` is not executed, so editing its load-order comment fixes nothing; **F21 the `WILE_SYNTAX_FORMS=scheme` leg is no longer green** — 61 minutes for `pkg/wile` alone and seven failures in five classes, the worst being that the Scheme generators compute with USER-REPLACEABLE primitives, so `reg.Without("+")` plus an embedder's own `+` cannot load bootstrap. Nothing reaches the default; all of it is owed before the flip, and that class outranks the startup number |
| `ARCHITECTURE.md` | 1/4 sections complete |
| `MACRO_SYSTEM.md` | Both sections unstarted |
| `GRAPH-SPECTRUM.md` | Directions, not scheduled |

### Parked, gated, or blocked

| Plan | Gate |
|---|---|
| `2026-04-16-recurrence-categories-design.md`<br>`2026-04-16-recurrence-impl-plan.md` | 0/5 tasks; matrix category blocked on `(wile algebra matrix)` |
| `2026-04-18-gonum-integration-directions.md` | Funding-gated directions |
| `2026-04-17-algebra-foundations-directions.md` | Funding-gated directions |
| `2026-04-20-copilot-review-data-mining.md` | Imminent, not started |
| `2026-04-21-wile-goast-ac-match-migration.md` | Stub; deferred follow-up in wile-goast |
| `2026-04-23-coverage-library-tracking.md` | Blocked by algebra Tier B per `WORKSPACE-ROADMAP.md` |
| `2026-04-23-docs-sweep-impl.md` | Planned, not started |
| `2026-05-02-algebra-matching-many-to-many.md` | Gated on `(wile algebra matroid)` (§5.7 Tier C) |
| `2026-05-05-iter-seq-cascade.md` | Draft; sequenced after the charsets refactor (shipped) |
| `2026-07-11-chibi-derived-ergonomics-backlog.md` | **#5 RESOLVED 2026-09-04 — already built**, nothing to do: every primitive its "Verify first" asked about is registered in `pkg/registry/core/syntax.go` (`identifier?`, `syntax->datum`, `datum->syntax`, `generate-temporaries`, `bound-identifier=?`, `free-identifier=?`) and `er-macro-transformer` lives in `compile_er_macro.go` with `wile/er-macro-test.scm` green. #6 (chibi-ffi-style codegen) still parked |
| `2026-07-28-callback-conditional-capture-safety.md`<br>`2026-07-28-callback-conditional-capture-safety-impl.md` | **Phase 0a SHIPPED** (`47c27f72` on `feat/callback-conditional-capture`); phases 0b onward **blocked on Q1/Q3**. Q2 answered (sealed-base only), Q4 done. The `-impl` twin is the authority on phase state. The design note's header claims this index row was added 2026-08-08 and that adding it let `make planlint` see its stale header; the row did not exist until now, so `planlint` has never had that signal for this plan |
| `TECH-DEBT-2026-04.md`<br>`TECH-DEBT-2026-04-IMPL.md` | 25/27; only 6.2 (`context.TODO` in test files) open |
| `PERFORMANCE.md` | 1 complete, 1 open (env frame slimming), 1 rejected |
| `2026-03-25-b3-c2-c6-design.md` | B3/C2/C3/C4 done in wile-goast; **C5/C6 open** |
| `DEBUGGER.md` | **POSTPONED until every 2026-08-07 wave is complete.** Owns the W3-13a residual: breakpoints do not suspend inside `load`/`eval`. Wave 3 Phase 6 shipped suspension on the top-level chain only and pinned the gap green-on-both-sides (`pkg/wile/debugger_subcontext_test.go:81-125`, `suspendHits == 0`), so it is not mistaken for a gate. Sized `L`. Its own project because closing it means giving the sub-context a break boundary, which touches the open continuation-truncation surface — out of scope for any wave item. This file's own two proposals are separately unstarted; Proposal 1 is unbuildable at HEAD and superseded by the shipped Phase 6 |

### Trackers, investigations, and technique notes

| File | Kind |
|---|---|
| `2026-07-01-staff-engineer-sweep.md` | Whole-codebase tech-debt tracker; larger M/L findings still open |
| `2026-07-17-pair-gc-investigation.md` | Read-only investigation of three cons-cell GC levers |
| `2026-07-18-scope-keyed-global-bindings-design.md` | Scope-keyed global bindings; successors transcribed into Tier 1 |
| `2026-07-31-go-volume-reduction-discovery.md`<br>`2026-07-31-go-volume-reduction-findings.md` | Discovery/audit; the deliverable is a ranked findings inventory, not code. Phase 0 + Phase 1 **COMPLETE**, merged from `chore/volume-discovery-phase0` 2026-08-10; Phase 2 (verify) and Phase 3 (rank) **not started**, so **no finding is CONFIRMED**. M1/M3/M4 collide with the 2026-08-07 review waves — read the ordering notes in the findings file before resuming Phase 2 |
| `2026-08-25-write-only-fields.md` | Report-only inventory of struct fields written but never read (AST-level sweep over the module, 6 of 34 spot-verified); nothing deleted, each field needs its own judgment |
| `2026-08-22-complexity-inventory-refactor.md` | Measured, unscheduled; no item started. Where structural complexity concentrates in the Go tree, so a refactor is aimed at evidence |
| `2026-09-06-scheme-specified-syntax-forms-review.md` | Adversarial review record for the two syntax-forms plans; its verified findings are folded into both (the design's 2026-09-06 revision, the impl's F11–F14) |
| `2026-06-17-inverse-verification.md` | Technique note |
| `divergent-design-subcontext-workflow.md` (+ `.js`) | Technique note plus runnable harness |
| `wile-compiler-review.workflow.js` | Runnable harness: compiler subsystem review, nine dimension reviewers plus per-finding adversarial verification |
| `WORKSPACE-ROADMAP.md` | Living cross-project queue (`wile` ↔ `wile-goast`) |
| `CLAUDE.md` | Plan-file conventions and the implementation-completion workflow |

---
## Top Priority — Triaged 2026-07-09

Promoted from the 2026-07-09 open-item triage. Unboxed arithmetic was the sole real perf lever left
once escape-gated frame reclaim and the layered-environment carve were found already shipped.

- [x] **`make planlint`: flag plan headers whose Status is stale vs reality** [Done 2026-08-07]:
  `tools/sh/planlint.sh`. A status keyword alone is never a finding; an open-sounding Status line
  must coincide with a signal that the work landed: an `-impl` twin archived to `memory/`, a
  TODO.md row reporting it shipped, or a merged PR cited **on that Status line**. The PR scan is
  confined to the Status line because the any-`PR #N` form's 8 first hits were all a
  *neighbouring* plan's PR. Not in `make ci`: its inputs are gitignored. Motivated by three triage
  hits (escape-gated, layered-env, frame-reclaim) where a stale `-design` note in `plans/` hid an
  `-impl` twin archived as COMPLETE.
- [ ] **Unboxed scalar/float arithmetic — kill per-op `*values.Float` heap alloc.**
  **Phase 0 SHIPPED 2026-08-07** (`pkg/environment/cell.go` — `Cell`/`Box`/`Unbox`,
  plus the probe benchmarks the plan's acceptance section cited but which had
  never been written, `pkg/values/numeric_alloc_bench_test.go`). Phases 1 and 4
  are **on hold pending a go/no-go**, because Phase 0's measurement invalidated
  the premise:

  > **The "~85% CPU in GC" figure is wrong and must not be re-quoted.** Profiled
  > on master 2026-08-07 (`09e4aeca`, darwin/arm64 M4 Max — the SAME machine as
  > the original claim): `runtime.mallocgc` is **16.4%** of samples, not 85%, and
  > `gcBgMarkWorker` does not appear at all. The source of the 85% is
  > `memory/UNBOXED-FLOAT-PIPELINE.md:30`, whose table attributes 49% to
  > "GC coordination (`kevent`, `pthread_cond_wait`)" — those are parked-thread
  > symbols, so that profile was counting idle runtime threads as collection.
  > (Today those two symbols total 4.5%.) The number 85 most likely came from the
  > `MachineContext.Run` cumulative row, which reads 85.07% and just means "the
  > program".

  Corrected ceiling: `(*Float).Add` is 19.4% cumulative on `sumfp`, of which
  16.4% is `mallocgc`. **Removing every float box buys ~16%, plausibly ~20% with
  heap-growth churn — on the most float-heavy benchmark in the suite.** The
  per-op probes agree the headroom is smaller than assumed: the design doc's
  control gap is 18.21 → 4.25 ns (4.3×) on linux/amd64, but measured here it is
  6.95 → 3.78 ns (**1.84×**, a 3.2 ns delta rather than 14 ns).

  Still verified open against source: the eval stack is a boxed `[]values.Value`
  (`machine.Stack`, `pkg/machine/stack.go`); no unboxed register lane in
  `vmState`, no scalar lane on `Binding`. Design is settled — **Design A is REJECTED**
  (`plans/2026-06-24-unboxed-scalar-arithmetic-design.md`, "Decision note
  2026-07-11"); the live plan is Design B + Phase 4 binding-slot unboxing.
  Remaining work is Phase 1 (single register lane + fusion pass + no-loss gate)
  and Phase 4 (proven self-tail loop slots), both Large, for the ~16–20% above.
  `memory/UNBOXED-FLOAT-PIPELINE.md` is the older 3-layer framing
  (superseded, archived) and is also the source of the bad profile. [Large]

---

## Tier 1 — Security & Correctness

Items that block production embedded use or prevent silent state corruption.

### Defects from the 2026-09-14 doc-audit sweep

Found while checking docs against code (`99b8c99d`, `032728ab`) and while fixing what that pass
reproduced. Every fixed item has a regression test that fails on `032728ab`.

**Fixed, MERGED 2026-09-14 (`032728ab..6ce2f4c1`)**, all eleven plus
`fix/phase-shifted-expander-root` and `fix/let-merge-capture-safe` (entry below). Branches deleted.

| Branch | Fixes |
|---|---|
| `fix/environment-authorizer-under-evalin` | SECURITY. `environment`, `scheme-report-environment`, `null-environment`, `make-namespace`, `namespace-require` built from the registering namespace, so a stricter child authorizer under `EvalIn` was escaped. Also `checkProfileWidening` now reads `EffectiveAuthorizer` |
| `fix/containment-symlink-dotdot` | SECURITY. `<root>/link/..` passed `containedInRoot` by spelling and `set-current-directory!` left the root. A `..` that backs out of any symlink is now refused, even one that stays inside the root (the `os.Root` rule) |
| `fix/composable-callcc-pooled-frame` | `GraftContinuation` hung a shared segment on an unshared chain; `call/cc` inside a 2+-frame segment stopped `MarkChainShared` early and a pooled frame was recycled (index-out-of-range panic, or silent wrong values) |
| `fix/continuation-marks-under-tail-prompt` | A mark was recorded twice under a tail-position prompt/barrier/`call-with-values` (`RunBodyUnderFrame`), and `appendChainMarks` kept the prompt frame's own marks |
| `fix/read-shares-port-position` | `read` held look-ahead past the datum, so `read-char` after `read` was desynchronised (R7RS §6.13.2). Side effect: an error just after a datum now surfaces on the next read |
| `fix/let-syntax-body-literals` | `else`/`=>` failed in any `let-syntax` body: `literalScopesMatchWithDef` refused a literal carrying the body scope. `filterRebindingScopes` deleted; `Scope.IsRebinding` is now never read (public API, left) |
| `fix/define-for-syntax-within-unit` (on `fix/phase-shifted-expander-root`) | `define-for-syntax`/`begin-for-syntax`/`eval-when` ran at compile time, after the unit's `define-syntax` transformers were built. They now run during expansion. Side effects: inside `(meta …)` they no longer run; a transformer that `expand`s a subform containing one and returns it runs it twice; phase-1 code ignores `WithInlineThreshold` |
| `fix/phased-import-defects` | `(for-syntax A B)` / `for-template` / `for-meta` dropped every set after the first. Go `syntax-case` clause bodies read enclosing locals boxed (`#&7`) |
| `perf/optimize-eval-load-compile` | `compile`'s returned thunk skipped `Optimize()` (-27%). The report that `eval`/`load` procedures were unoptimized was false: `compileClosureBody` already optimizes them |
| `chore/stale-comments-and-doc-test` | `TestTrackedDocsDoNotReferenceIgnoredPaths` (exit 128 through the tracked symlinks); 23 files of stale comments; the 8 binding-tier ordinal labels (entry below); `compare-schemes.sh` for Chez/Racket |
| `docs/semantic-pass-remaining` | algebra, types, numeric, concurrency, coverage, learn docs checked against code |

**Open, needs a decision.**

- [x] **A `let` inside a procedure is not fresh when a continuation re-enters it** [High,
  correctness regression, Done 2026-09-14]: `6a017fa1` allocated a `let`'s slots in the enclosing
  procedure's frame, so re-entry wrote the slot a closure or continuation from the earlier pass
  still read (composable re-invocation `0 1 2 2`, Racket `0 1 2 1`). A `let` now merges only into
  a frame no continuation can be captured under (`29b9babc`); a capturing `let*`, whose frame
  exists before its inits run and was wrong before `6a017fa1`, compiles as nested lets
  (`9469c442`). Both MERGED in `6ce2f4c1`. Zebra +5.8%, deriv ~+2%, rest flat. Rejected: copying
  the frame on re-entry, unsound because assigned-but-uncaptured locals live unboxed. Internal
  `define`/`letrec` reuse the location too, which R7RS letrec* arguably permits and Chez/Racket do
  not do. Pinned by `TestLetIsFreshWhenAContinuationReentersIt`, `TestLetMergeFollowsCaptureSafety`.
- [x] **Composable continuation re-invocation** [High, Done 2026-09-14,
  `fix/composable-reinvocation-live-segment`]: re-invoking `kc` inside its own running segment
  (two or more frames; `call/cc` under a prompt inside its own run too) made `AcquireSegment` set
  `p.bottom.parent = nil` on a live frame, cutting the program off below the segment. Re-invocation
  now copies through the bottom frame (`DeepCopyThrough`) and writes no original. Trade-off: the
  original bottom keeps its graft edge, so a multi-shot continuation retains its first invoker's
  chain until dropped. Pinned by `TestComposableContinuationReinvocationKeepsLiveSegment`. The
  second filed symptom, locals leaking between invocations, was the `let` item above: ten binder
  shapes match Racket on `6ce2f4c1`.
- [x] **What an `(environment …)` namespace sees at phase ≥ 1** [Medium, Done 2026-09-14,
  `fix/profile-env-macro-phase-rows`]: an import-spec `(environment …)` stays empty above phase 0
  (R7RS §6.12; Racket's `make-base-namespace` phase 1 is empty), pinned by
  `TestImportSetEnvironmentIsEmptyAboveRuntime`. A profile environment carries the engine root's
  language rows: the row installers and vocabulary moved to
  `pkg/internal/bootstrap/language_rows.go` and `initializeEnvironmentWithRegistry` installs them,
  so both bootstrap sequences share one definition (a hook set from `pkg/wile` would have left
  testhelpers environments without them). Before, phase 1 worked in `(environment '(wile small))`
  only through the registry's exact phase-1 slots, and a phase-2 transformer body could not see
  `list`. Pinned by `TestProfileEnvironmentHasTheMacroVocabularyAtEveryPhase` and
  `TestBootstrapOwnersHaveTheMacroVocabularyAtPhaseTwo`.
- [x] **Schelog no longer runs unmodified** [Medium, docs claim]: decided 2026-09-15, keep phase
  separation. `schelog.scm` now carries `(import (for-syntax (scheme base) (scheme cxr)))`; all 13
  `run-all-tests.sh` cases pass and the zebra bench runs. README and BIBLIOGRAPHY no longer claim
  "unmodified" or `syntax-rules` (all 14 transformers are procedural). `test-schelog` runs only in
  `make cd`, which is why CI stayed green.
- [ ] **A macro defined in an `include`d file is unbound in the including file** [Medium, filed
  2026-09-15 as #820, not bisected]: `t6.scm` = `(define-syntax foo (syntax-rules () ((_ x) x)))`,
  `t7.scm` = `(include "t6.scm") (display (foo 42))`. `wile -f t7.scm` (with or without `-i`, and
  with an explicit `(begin …)` wrap) fails "no such local or global binding foo"; `wile -i < t7.scm`
  prints 42. A *sibling* include sees it (`(include "defs.scm") (include "use.scm")` works, which is
  why `stress-test.scm` passes); forms written in the file holding the `include` do not, whether
  that file is the `-f` file or itself included. A procedure defined the same way is visible.
  R7RS §4.1.7: `include` is `begin` over the file's contents, no new scope. Breaks `examples/logic/schelog/run-all-tests.scm` (`%which` unbound at line 65); its README
  instruction was removed 2026-09-15, restore it when this is fixed.
- [ ] **Library export of a name the library did not define** [see `findLibraryBinding` entry
  below]: the two-phase *defined* case already exports phase 0 correctly (measured
  2026-09-14, matches Racket); re-exporting `syntax-rules` from `(scheme base)` still fails
  (`TestLibraryExportTakesFirstPresentPhase`). ~~A `begin-for-syntax`-only define exports to
  phase 0 where Racket refuses at `provide`.~~ Refused since 2026-09-16, branch
  `feat/export-for-syntax` (`TestLibraryExportRefusesNameNotBoundAtItsPhase`).

**Open, no decision needed.**

- [ ] `call-with-immediate-continuation-mark` sees an outer frame's mark after an ordinary
  non-tail call: `(wcm 'a 1 (list (cicm 'a …)))` gives `1`, Racket the default
  (`GetImmediateMark` fallback).
- [ ] `(continuation-marks k)` for a `call/cc` captured in tail position of
  `with-continuation-mark` inside a procedure omits the capturing frame's marks (Wile `()`,
  Racket `(2)`).
- [ ] A literal's binding captured when a nested `syntax-rules` transformer compiles misses an
  enclosing `let-syntax`-bound `else`: `(let-syntax ((else …)) (let-syntax ((m (syntax-rules
  (else) …))) (m else)))` gives `other`, Chez `lit`.
- [ ] A macro expanding to `(begin-for-syntax (define (name x) …))` leaves `name` invisible to
  a later transformer, one form per unit.
- [ ] `(eval '(begin (define-syntax m …) (m)) (environment '(scheme base)))` gives `no such
  binding "m"`; two separate `eval`s work.
- [ ] Go ER transformers receive plain symbols, so `(datum->syntax (car f) …)` fails where the
  Scheme syntax forms work. Go `syntax-case` rejects a whole-clause `_` pattern.
- [ ] Numeric: `(sqrt #m2)` raises (`PrimSqrt` has no `BigFloat` case); the Inf/NaN guard in
  `pkg/values/promotion.go` (`lubIsComplex` branch) rounds a `BigComplex` operand to float64.
- [ ] Algebra: `run-analysis` returns all-`#f` outputs for a 3-block forward chain where block 1
  should be `#t`; `rewrite` treats the term comparison as boolean, `unification` as three-way
  (`ac-match` raises `=: expected number, got #f`).
- [ ] `(environment '(wile algebra))` fails with "unknown wile profile": every `(wile X)` is read
  as a profile name.
- [ ] Coverage: `stdlibPrefixes` (`coverage/gocover.go`) omits the embedded `chibi/` and `rnrs/`.
- [ ] Stale: `waitOnCondCtx` (`pkg/values/cond_wait.go`) and `LockContext`
  (`pkg/values/mutex.go`) say a terminated holder's lock stays held (`Thread.Terminate`
  abandons it); tutorial chapter 06 calls C_6 and 2·K_3 cospectral, chapter 05 lists a
  nonexistent `(wile algebra field)`; `Makefile` `bench-gabriel-compare` names `ack`;
  `kanren-benchmark.scm` prints no "Total time".
- [ ] `read-syntax` positions do not count characters `read-char` consumed between reads (needs
  port-owned position tracking).
- [ ] `set-current-directory!` still has a check-then-`os.Chdir` window.

### `Imported` is unsound evidence for `IsStable()` at phase 1 — DISCHARGED 2026-08-10, one refusal left (2026-08-09)

Opened by the wave-1 §5 / Phase 5 work, and it is the **named owner** for the
residual that work deliberately left standing. `Binding.IsStable()` returns
`m.Imported || m.Stable` (`pkg/environment/binding.go`), on the premise that a
define can no longer reach an import's slot. The T2 relocation made that premise
true at phase 0 only, and these three rows were the phase-1 gap.

**Discharged, re-verified 2026-08-14 by mutation:** `Imported` is sound evidence at phase 1 from
two directions: every route that rebinds an import's slot clears the flag (item 1), and the one
`Stable`-stamped phase-1 population is no longer user-writable (item 3). Item 2 is not work: it is
a **recorded refusal**, kept so nobody "finishes the job" by flipping a constant.

- [x] **`define-syntax` over an imported macro superseded in place and left `imported=true`**
      [Done 2026-08-09, `297bb18b`]: `m.Imported = false` on the supersede
      (`compile_define_syntax.go`), symmetric with the variable path (R7RS §5.3.1). The
      `UpdateMeta` call sat behind `docstring != ""`, so it now runs for every `define-syntax`.
      Gate `TestDefineSyntaxSupersedesImportClearsImported`
      (`pkg/wile/immutable_import_test.go`): only its `IsImported()` assertion discriminates; the
      two premise assertions pass either way and are the non-vacuity guard.

- [ ] **The phase-1 import installs cannot simply take the T2 tier.** This is the
      scope the relocation explicitly refused, recorded so nobody "finishes the
      job" by flipping a constant. `(ExactPhase(1), sealed)` is occupied by
      bootstrap macros and primitive expanders, so an imported macro would land on
      a bootstrap macro's exact coordinates under the same ambient scope set:
      `CreateGlobalBindingAt` reuses the slot, `importConflicts` returns false (a
      bootstrap macro is not `IsImported()`), and `SetOwnGlobalValue` overwrites
      the sealed transformer **in place, engine-wide**. Relocating phase 1 needs a
      way to keep imports off the startup set's coordinates — a distinct rank, or a
      non-ambient scope set — not a different argument.
      `TestImportDoesNotOverwriteSealedBootstrapMacro` (`pkg/wile/import_tier_seal_test.go`)
      is the gate; the whole rest of the suite is blind to this, measured. The
      file's header states the mutation that reddens it — flip either
      `placementInPlace` to `placementShadowable` in `library_bindings.go`.
      **The coordinate got more crowded on 2026-08-10, which raises the price of
      getting this wrong**: item 3's fix moved the 146 expand-phase primitive
      copies onto `(1, sealed)` beside the ~60 bootstrap names (`24e08ceb`'s
      census; intersection 0), so an imported macro landing there now has a
      larger startup population to collide with, not a smaller one.

- [x] **The phase-1 registry copy was stamped `Stable` over an open writer set** [Done
      2026-08-10, `24e08ceb`]: `phaseTargets` bound `PhaseExpand` through `env.Expand()`, putting
      the 146 copies at `(1, mutable)`, the coordinate a top-level `define-for-syntax` writes. The
      fix routes `bindingEnv` through `env.SealedWriteViewAt(PhaseExpand)`; `closureEnv` stays
      `expandEnv`, since user code resolves through it and the sealed-write view hides every user
      define. **So `Stable` is stamped only where the writer set is closed at phase 1 too, and
      `define-for-syntax` SHADOWS the registry copy (a new slot) rather than superseding it.**
      Pinned by `TestBindingModelMatrix`'s three M7 rows, of which only the third discriminates:
      the preservation and by-name rows read the same under supersede and shadow.

### `findLibraryBinding` exports the wrong phase for a two-phase name (2026-09-08)

- [ ] **Make the export walk pick the phase the importer needs, not the lowest one**
  [High, M, filed 2026-09-08 while checking whether an importable `syntax-rules` is possible
  for the Flatt Stage A design (`memory/2026-09-08-flatt-binding-model-a-design`, §9 Q3)]:
  `findLibraryBinding` (`pkg/machine/compilation/library_bindings.go`) walks
  `lib.Env.PresentPhases()` and returns the **first** hit. `PresentPhases`
  (`pkg/environment/environment_frame.go`) sorts ascending and trims below `PhaseRuntime`,
  measured as `[runtime expand phase(2)]`, so **phase 0 always wins**. For a name bound at
  two phases the export takes the phase-0 binding, which is not the one an importer of a
  syntactic name needs.

  **Measured, round-tripped through a real library** (`(export (rename X Y))`, then import
  and use). Pinned by `pkg/wile/library_export_phase_order_test.go`:

  | Exported | Import lands as | Usable |
  |---|---|---|
  | `let-syntax` (phase 1 only) | `*compilation.PrimitiveExpander` at exact@1 and exact@0 | yes — the renamed name expands, `(my-let-syntax ((m (syntax-rules () ((_) 42)))) (m))` is 42 |
  | `syntax-rules` (phase-0 `SyntaxCompiler` **and** phase-1 `PrimitiveExpander`) | `*compilation.SyntaxCompiler` at exact@0 only | no — `(define-syntax two (my-sr () ((_) 2)))` raises `no such local or global binding "my-sr"` |
  | `...` (ambient keyword row only) | `values.voidType` at exact@0 | no, and ellipsis is name-keyed in the matcher anyway (`match.DefaultEllipsis`) |

  **The failure is silent.** All three pass `validateLibraryExports`
  (`pkg/machine/compilation/compile_library_forms.go`), which asks only whether *some*
  binding is reachable via `findLibraryBinding` — the same first-hit walk. So export
  validation passing is not evidence the export is usable, and a library can ship an
  export that resolves to nothing at the use site.

  **Why it matters beyond the bug.** Stage A's §9 Q3 (does a `BulkSource` expose the whole
  store or a curated export list?) inherits this: a whole-store source must answer "which
  phase's slot does this name supply?", and "the lowest present one" is the wrong answer.
  A per-phase curated export list sidesteps it. It also blocks the idea of moving
  `syntax-rules` into an importable library so the phase-1 vocabulary is declared rather
  than hardcoded — the one name that motivates it is the failing row.

  **WITHDRAWN 2026-09-16:** exports now declare their phase (`(for-syntax ...)`, branch
  `feat/export-for-syntax`, Racket's `provide`), and `findLibraryBinding` takes the export's
  phase, not the importing one. Shifting an import shifts what an export denotes; it never
  re-selects it. Fork (a) below would contradict that. The record that follows is kept as
  history.

  ~~**Open fork, not decided.**~~ **DECIDED (a), 2026-09-10** (Stage B impl fork F). Either
  (a) `findLibraryBinding` takes the requesting phase and
  prefers a match there, falling back to lower phases, or (b) it returns every phase's
  binding and the install site picks. (b) is closer to what a bulk row wants but touches
  `importConflicts`, which currently compares one binding to one binding. Related:
  §4.6 of the Stage A design asserts the walk "probes phases for a *definition*" — true, but
  it does not say which definition wins, which is the whole defect.

  **Why (a), and what changed the answer.** Stage B settled fork A as **A2** — the phase-1
  reader gates STAY and imports are NOT routed through bulk rows — which removed (b)'s stated
  justification twice over. "(b) is closer to what a bulk row wants" is moot when there is no
  bulk row, and (b) would additionally have widened `importConflicts` from
  one-binding-vs-one-binding to set-vs-set for no remaining benefit. This stays a TODO.md item;
  **it is not Stage B work and nothing in Stage B depended on it.**

  **The obligation that rides with (a), and it is the half that will be forgotten.**
  `validateLibraryExports` uses the **same first-hit walk**, so **export validation passing is
  not evidence the export is usable**. Fixing `findLibraryBinding` alone leaves the false green
  standing: validation would keep certifying an export that resolves to nothing at the use
  site. The validator must ask the SAME widened question, in the same change.

  **Link to the phase-1 population question, in both directions** (Stage B fork E, declined
  below and in the design's §9 Q1): an importable `syntax-rules` is the motivating name here,
  and it is also the thing that would let the phase-1 vocabulary be **declared** rather than
  hardcoded. The recorded sequence is: identity fix across the import edge → **this item** →
  vocabulary becomes declarable → only then is Racket's empty phase 1 reachable. Doing them out
  of order does not work.

  **2026-09-15:** the motivating row (renamed `syntax-rules` in a transformer RHS,
  `TestLibraryExportTakesFirstPresentPhase/syntax-rules_(two_phases)_does_not`) turns out to be a phase-isolation
  question, not an export-walk defect. Under default phase isolation a plain `export` binds at
  phase 0 only, matching Racket's `rename-out`: `(only-meta-in 0 (prefix-in b: racket/base))`
  refuses `b:syntax-rules` in a transformer RHS, and `racket/base` declares exactly `...`, `_`,
  `syntax-id-rules`, `syntax-rules` at phase 1. The fix is the "Option 2" item below: declared
  per-phase export tables. That needs the renamed-keyword-by-binding dispatch shipped on
  `feat/keyword-denotation-dispatch` first, because `(import (scheme base) (for-syntax (scheme
  base)))` must see its two `syntax-rules` rows at phase 1 as one denotation before a per-phase
  export table can declare them as one name.

- [x] **Renamed `unquote`/`unquote-splicing` are matched by spelling, not by binding**
  [Done 2026-09-15, branch `feat/renamed-inner-positions`]: `quasi_expand.go`'s marker
  recognition (`dottedTailCell`, the depth walk `quasiNeedsRuntime`) compared a head symbol's
  spelling against `"unquote"` / `"unquote-splicing"`, never its resolved binding.
  `(import (rename (scheme base) (unquote uq))) `(1 (uq (+ 1 1)))` gave `(1 (uq (+ 1 1)))`,
  where Racket gives `(1 2)` — this branch now answers `(1 2)` too (same for
  `unquote-splicing`, and for a `prefix` import). **What now decides:** the form the marker's
  head denotes (`environment.DenotedForm`), with spelling as the fallback for a head that
  resolves to no form — an unbound head under a plain `prefix` import still keys on spelling
  unchanged (arm 2 of the keyword-denotation-dispatch rule). **Both marker walks moved, not
  one.** The first round moved only the expander's; `pkg/internal/validate/opaque_subtree.go`'s
  walk kept comparing spellings, and that walk is what records a quasiquoted `set!` as a
  mutation and a capture. The permissive disagreement its own doc warns about then happened:
  a renamed unquote left the `set!` unmarked, so the inliner kept a stale lambda body, a
  captured binder was never boxed, and top-level immutability REFUSED a legal `set!`. Fixed by
  giving the opaque node the environment it was validated in (`ValidatedQuasiquote.Env`,
  `ValidatedLiteral.Env`) so every consumer of the walk — the boxing pass's reference index and
  the or-shaped-let lowering — reads the same answer from the node rather than from wherever it
  happens to stand. Pinned by
  `TestRenamedQuasiquoteMarkers` and `TestRenamedQuasiquoteMarkerIsVisibleToTheOpaqueScan`
  (`pkg/wile/renamed_inner_positions_test.go`). **The first cost claim filed here was measured
  on a corpus without quasiquote in it** — `BenchmarkValidatePhase` and `BenchmarkFrontEndPhase`
  run `compileBenchCorpus`, which has no `quasiquote`, so "no statistically significant delta"
  said nothing about this change. Re-measured 2026-09-15 with a benchmark built for the path
  (`BenchmarkQuasiquoteExpand`, `pkg/machine/compilation`: five quasiquote defines through
  parse → expand → validate → compile, covering the element, nested-depth, splice, dotted-spine
  and deep shapes), 12-run interleaved A/B against `feat/keyword-denotation-dispatch`
  (079c62ab): **sec/op 66.71µ → 68.71µ, +2.99% (p=0.000, n=12)**; allocs/op 2.164k → 2.159k
  (−5, p=0.000); B/op −0.05%. The +3% is the resolution the fix adds per template head, in both
  the expander's walk and validate's; the −5 allocs are the `resolvedBy` closure this round
  replaced with a frame pointer on `quasiKeywords`. `BenchmarkValidatePhase` and
  `BenchmarkFrontEndPhase` remain flat (p=0.052 / p=0.198, 0 alloc change, same 12-run
  interleave), which is now a statement about quasiquote-free code rather than a claim about
  this fix.

- [x] **Renamed AUXILIARY keywords (`else`, `case`'s `else`, `=>`) are still matched by
  spelling, not by binding** [Done 2026-09-15, branch `feat/renamed-inner-positions`]:
  `sameLiteralBinding` (`pkg/internal/match/syntax_adapter.go`) accepted a pattern-literal
  match whenever both the definition-site and use-site bindings were `BindingTypePrimitive`,
  never asking whether the two denote the SAME form. `(import (scheme base) (rename
  (scheme base) (else otherwise))) (cond (#f 1) (otherwise 2))` failed on this branch's base
  and on master (`syntactic keyword "otherwise" used as a variable`); Racket answers `2` —
  this branch now answers `2` too (same for `case`'s `else`, and for a renamed `=>`). **What
  now decides:** the denoted form (`environment.DenotedForm`) instead of "both Primitive",
  narrowed further so an imported literal that denotes NO form (an ordinary variable or user
  macro) still falls back to matching spelling, or any imported name would satisfy any such
  literal (see `TestRenamedLiteralDoesNotOverAcceptUnrelatedImports`, which measured that
  exact over-acceptance before this narrowing). Pinned by `TestRenamedAuxiliaryKeywords` and
  `TestRenamedLiteralDoesNotOverAcceptUnrelatedImports`
  (`pkg/wile/renamed_inner_positions_test.go`). A reviewer flagged an unmeasured cost: an
  N-clause `cond`/`case` now pays a binding resolution per clause tried against `else`/`=>`,
  since match.go's literal arm used to refuse a differently-spelled symbol with a cheap
  string compare and now always resolves it (`pkg/internal/match/match.go`'s
  `ByteCodeCompareCar`). Measured 2026-09-15 with a benchmark built specifically to hit this
  path (`BenchmarkCondClauseLiteralExpand`, `pkg/machine/compilation`; bare-identifier clause
  tests, the shape whose else/=>-position input actually is a symbol — a compound-test clause
  like `(< n 2)` never reaches the literal comparison at all): no statistically significant
  delta and 0 extra allocs/op against `feat/keyword-denotation-dispatch`, 12-run interleaved.
  `case`'s clause heads are always `(atoms ...)` lists, so its cost stays O(1) regardless of
  clause count; `cond`'s bare-identifier/`=>`-clause shape is the one that pays O(N) in
  principle, and it still measures flat.

- [ ] **A quasiquote head can now raise `ErrAmbiguousBinding` on a DATUM** [Low, S, filed
  2026-09-15 during the renamed-inner-positions whole-branch review]: marker recognition resolves a
  template head through `env.GetBinding` (`headFormName`, `pkg/machine/compilation`;
  `markerName`, `pkg/internal/validate/opaque_subtree.go`), and `GetBinding` raises
  `ErrAmbiguousBinding` on an equal-cardinality incomparable maximum (Fork C, auto-memory
  `ambiguous-binding-raise-fork-c.md`). Every previous consumer of that raise was asking about
  code; the quasi walk asks about a head sitting in pure DATA, where an identifier's identity was
  never consulted before — so `` `(a (foo b)) `` can in principle refuse to compile for a reason
  that has nothing to do with what the form means. Latent: no Scheme-level repro constructed, and
  the shape needs two same-spelled imported bindings at incomparable scope sets reachable at a
  template head. The conservative answer for this consumer is "not a marker" (keep the spelling),
  which is what the existing `denoted == ""` fallback already does for every non-raising failure.

- [ ] **`(cond (otherwise 2))` works with a renamed `else` but `(cond-expand (otherwise …))` does
  not** [Low, S, filed 2026-09-15 during the renamed-inner-positions whole-branch review; an
  inconsistency this branch CREATES by fixing one of the two]: `cond`'s and `case`'s `else` are
  pattern literals and now resolve by binding, so `(rename (scheme base) (else otherwise))` makes
  `(cond (#f 1) (otherwise 2))` answer `2`. `cond-expand`'s `else` is not a pattern literal — it is
  a hand-written spelling test, `sym.Key() == "else"` at
  `pkg/machine/compilation/compile_cond_expand.go:187` — so the same rename leaves
  `(cond-expand (otherwise …))` unrecognized. Two forms named `else`, two mechanisms, one of them
  now binding-aware. Fix candidate: the same `environment.DenotedForm` reading the other two use,
  spelling as the fallback. `isElseClause` is a package function, but its only caller
  (`resolveCondExpandClause`, same file) is a `*CompileTimeContinuation` method and already reads
  `p.env`, so the environment is in reach; what is NOT settled is whether `else` in a
  `cond-expand` position resolves to a form keyword at all in the frames this runs in, including
  a library body's. Measure that before writing the comparison.

- [ ] **`environment.SameBinding` is the identity `literalNotShadowed` approximates** [Low, M,
  filed 2026-09-15 during the renamed-inner-positions whole-branch review]: the pattern-literal
  rider in `pkg/internal/match/syntax_adapter.go` accepts an imported use-site binding and then
  discriminates with `DenotedForm`, falling back to spelling when neither side denotes a form —
  which is what the BOUNDARY row of `TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding`
  records as refusing a legitimate prefixed re-export. `environment.SameBinding`
  (`pkg/environment/binding.go:489`) is the origin-based identity that answers it properly, and
  the reviewer measured that adding it flips exactly that one documented BOUNDARY row to the R7RS
  answer with everything else green. Filed rather than applied because origin-based identity was
  REJECTED for import-conflict detection (auto-memory `import-conflict-detection-shipped.md`) on a
  failure with the opposite polarity — there it over-ACCEPTED distinct bindings as the same, here
  it would under-accept nothing — so the rejection does not transfer and must be re-measured on
  this consumer, not assumed either way.

- [ ] **`quasisyntax`'s `#,` reaches the same permissive disagreement this branch fixed for
  `quasiquote`, live on MASTER, by a different route** [Medium, M, filed 2026-09-15 during the
  renamed-inner-positions whole-branch review]: `quasisyntax` is a bare passthrough
  (`registerPassthrough("quasisyntax")`, `pkg/internal/validate/register.go:75`), so
  `opaqueEntryDepth` enters its whole form at `quasiDepthCode` rather than at template depth, and
  `quasiHeadDepth("quote", 0)` is a BARRIER there — the validate walk stops at the `(quote …)`
  subform quasisyntax's own expansion wraps literal data in and marks nothing inside it. The
  compiler disagrees: `quasisyntaxKW.quoting` is `"syntax"`, not `"quote"`
  (`pkg/machine/compilation/quasi_expand.go:83-87`), and `expandQuasi` has no case for that
  quoting kind, so it descends into the same `(quote …)` at depth 1 and fires the `#,` inside —
  validate says barrier, the compiler says live, the exact shape the branch's C1 fix closed for
  `quasiquote`, reached here by a route that fix never touched. Measured identically on master
  `963cab4b`, on `079c62ab`, and at HEAD (`093679e7`) — this branch neither creates nor fixes it:
  `(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,(begin (set! n 1) (set! f (lambda ()
  99))))))) (list n (f)))` answers `(1 7)`; the canonical control (the same two `set!`s run
  directly, no quasisyntax) answers `(1 99)`.

- [ ] **The branch's headline fix does not reach a marker introduced by a macro template** [Low,
  M, filed 2026-09-15 during the renamed-inner-positions whole-branch review]: a library that
  imports `(rename (scheme base) (unquote uq))` and exports a `syntax-rules` macro whose template
  is `` `((uq e)) `` gives `(0 7)` at HEAD and at base (`079c62ab`) for
  `(let ((f (lambda () 7)) (n 0)) (mk (begin (set! n 1) (set! f (lambda () 99)))) (list n (f)))`
  — the `set!` never runs and `f` keeps its original body, where a recognized marker would answer
  `(1 99)`. The macro-introduced `uq` carries the macro's DEFINITION-site scopes and resolves to
  nothing at the compiled use site, so neither `markerName` nor the compiler's `headFormName` sees
  a marker there. Both walks agree, so this is a conformance gap, not the soundness hole the
  branch fixed — Racket fires the renamed unquote through the same shape.

- [ ] **`validateInclude` never reads the included file** [Medium, M, filed 2026-09-15 during the
  renamed-inner-positions whole-branch review]: `validateInclude`
  (`pkg/internal/validate/validate_macro.go:235`) calls `markOpaqueCode(env, pair, result)` on
  `pair`, the `(include "f.scm")` form ITSELF — its own head symbol and string filename arguments
  — never on the forms the named file actually contains, which this validation pass does not even
  parse (its own comment says so: "The file's contents are not even readable here"). A `set!` of
  a captured or let-bound binding written inside the included file is therefore invisible to the
  opaque scan the same way a renamed marker was before this branch's fix, except here there is no
  marker-recognition question at all — the file's contents are structurally unreachable from this
  walk. Same hazard class as the fixed C1 bug (a hidden `set!` leaves a stale inline body or an
  unboxed capture); unpinned — no repro constructed.

- [ ] **`include` inside a procedure body skips letrec\* predeclaration** [Medium, S, filed
  2026-09-15 during the keyword-denotation-dispatch whole-branch review; pre-existing on master, not
  caused by this branch]: `(define (k) (include "x.scm") (inc-g))`, where the included file
  forward-references a later included `define`, raises `application: expected a procedure, got
  #<void>` at run time. R7RS §5.3.2 body semantics predeclare every name a body defines before any
  initializer runs (letrec\*); the body scan that does this for defines written directly in the body
  does not descend into an `include`d file's forms to find the names it will introduce.

- [ ] **Option 2: declared per-phase export tables** [**partly done 2026-09-16**, branch
  `feat/export-for-syntax`: decision (1) is the export syntax, `(export (for-syntax <spec> ...))`,
  nesting like Racket's; decision (2) is phase 0 only for a plain export, matching Racket's
  `provide`. `CompiledLibrary.Exports` is keyed by `(phase, name)`, and import modifiers act on a
  name at every phase. **Still open:** nothing declares `syntax-rules`'s two rows as one name;
  `(scheme base)`'s forms are registered Go-side and export no `for-syntax` row. Original entry
  follows.] [Medium, L, filed 2026-09-15, follow-on to
  the `findLibraryBinding` item above]: that item's fix (preferring the requesting phase) only
  routes an import to the right EXISTING binding; it does not let a library DECLARE which phases
  a name exports for. `syntax-rules` needs both its phase-0 `SyntaxCompiler` row and its phase-1
  `PrimitiveExpander` row visible to an importer as one denotation, and Wile's stdlib forms are
  registered Go-side, not declared through `export` syntax at all. Two open decisions block a
  design: (1) declaration mechanism, a field on the Go registration call vs. an `export` syntax
  extension a library body writes; (2) whether a plain `(export name)` re-export (no `rename`,
  no explicit phase) carries the source's declared phase rows, or defaults to phase 0 only,
  matching Racket's `rename-out` under `only-meta-in`. Depends on the renamed-keyword-by-binding
  dispatch shipped on `feat/keyword-denotation-dispatch`: `(import (scheme base) (for-syntax
  (scheme base)))` must see the two `syntax-rules` rows at phase 1 as one denotation before a
  per-phase export table can declare them as one name.

### The D7 residual: a primitive expander above phase 1 (2026-09-09)

- [ ] **Precondition on any dialect that installs one; not an open defect today**
  [Low, S, filed 2026-09-09 closing out Flatt Stage A
  (`memory/2026-09-08-flatt-binding-model-a-impl`, Task 8 / Fork 1)]:
  `LookupPrimitiveExpander` reads `env.Expand()`, an ABSOLUTE phase 1. That is
  CORRECT rather than a residual, because the expander table is single-instance:
  `registerPrimitiveExpandersWithout`'s only write is
  `env.SealedWriteViewAt(PhaseExpand)`, the sole `NewPrimitiveExpander` write in
  the tree, so nothing installs an expander at any other phase and a phase-2 body
  finds its expanders BECAUSE the read is absolute. The design's D7 proposed
  `NextPhase()` there; measured, that one symbol breaks bootstrap loading
  (`LoadBootstrapCore: load bootstrap macros: … not a closure: values.voidType`),
  with the ambient tier still present — which also refuted D7's premise that
  ambient was masking the problem. D7 is WITHDRAWN.

  What is filed is the precondition: **if a dialect ever installs a primitive
  expander above phase 1, the absolute read becomes wrong at that moment.**
  `TestPhase2_UniformRule`'s third subtest passes for the absolute read's sake and
  says so in its comment, so a later reader does not re-file D7 from it.

### `bootstrap_syntax{,_procedures}.scm` and the phase-1 vocabulary (2026-09-09)

- [ ] **Re-derive the macro vocabulary when the Scheme syntax layer flips**
  [Medium, M, filed 2026-09-09 closing out Flatt Stage A, Task 10 / D9's residual]:
  `MacroVocabulary` (`pkg/internal/bootstrap/language_rows.go`) is pinned against the **Go**
  layer by `TestPhase1VocabularyMembership`. Under `WithSchemeSyntaxForms`,
  `syntax-rules` becomes a Scheme macro with a procedural body, and that body's own
  transformer needs phase-**2** visibility — so the flip revises the set rather
  than contradicting it.

  The hazard is specific: someone widens the vocabulary to make the Scheme leg
  pass and deletes D4's phase-distinctness break for the Go leg at the same time,
  because a wider vocabulary is a name a procedural transformer no longer has to
  import. The ratchet asserts membership in BOTH directions so that shows up as a
  failure naming the added names, and
  `TestPhase1VocabularyIsPinnedAgainstTheGoLayer` records the coupling.

  The `%`-prefix arm is what currently carries the Scheme layer: its 56+ private
  helpers live in the base store and no `.sld` exports one or could, so no import
  could reach them. If the flip makes those importable, the arm can narrow.

### A phase-1 `(import (for-syntax (scheme base)))` is not behaviour-neutral (2026-09-09)

- [x] **Fixed at three reader sites** [Done 2026-09-09, `e61a3e37`, branch
  `fix/phase1-import-neutrality`]: ONE writer-side cause, THREE readers that failed closed on it.
  **Cause:** `installImportedBinding`'s shadowable arm is guarded `env.PhaseLevel() ==
  PhaseRuntime` (`library_bindings.go`), so a phase-SHIFTED import lands at
  `(ExactPhase(N>0), MUTABLE)`, which outranks every sealed phase row (expanders, syntax
  compilers, bootstrap macros) at the same empty scope set. `(scheme base)` exports 20 of the 40
  phase-1 expander names as value-less keywords, so the import blanked the binding-form
  machinery. **The auxiliary-keyword reading was REFUTED:** `(only (scheme base) let)` reproduces
  it, `(only (scheme base) else)` does not; the trigger is the collision set.
  **The three readers:** (1) `LookupPhaseBinding` (`phase_registry.go`) falls back to
  `SealedBindingAt(sym, q, phaseEnv.PhaseLevel())` on a type mismatch; (2) `lookupMacroBinding`
  ARM 2b (`expander_time_continuation.go`) also opens on a wrong-typed ARM 2 answer (victim:
  `syntax-rules` under `WithSchemeSyntaxForms`, where the Go form was dispatched in silence);
  (3) `invokeERTransformer` builds the rename closure at the expansion's phase,
  `DefEnv().AtPhase(p.env.PhaseLevel())` (prior art `GetGlobalIndexFromLibraryScopes`).
  **The discriminator is `BindingTypePrimitive`, not import provenance:** an `IsImported()` gate
  was tried and leaves the `syntax-rules` column red (that mask carries no `Imported` meta), and
  the binding-type rule keeps a user `(define define-syntax …)` shadowing
  (`TestLookupSyntaxCompiler_SamePhaseShadowOutranksTheSealedCompiler` caught the wider version).
  **Reader 3 was not import-specific:** `(define list 5)` plus an ER macro renaming `list` split
  the same way; the fix also closes a rename smuggling a phase-1-ONLY import into phase-0 output.
  Racket answers "unbound identifier" there; Chez is not the oracle (R6RS implicit phasing).
  Gates: `TestPhase1BaseImportIsBehaviourNeutral` (imported arm EQUALS bare arm),
  `TestPhase1BaseImportMasksNoPhaseRow` (derives the name set from `SealedSlots()`),
  `TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules` (a COUNTER, because the value assertion
  passes either way), `TestERRenameDenotesTheOutputPhase`. **Trap: a black-box "do the core forms
  still work?" table was REJECTED as the ratchet because it passes on the unfixed tree** (a masked
  head still reaches the compiler's own dispatch). Seven workaround sites in five files reverted;
  the old instruction to find them by grepping "not behaviour-neutral" was wrong (five wordings).
  `pkg/stdlib/lib/chibi/optional.sld`'s `cadr` uses sit under `(cond-expand (chibi …))`, and
  `chibi` is in neither `supportedFeatures` nor `platformFeatures`
  (`pkg/machine/compilation/features.go`), so that arm is dead and was not a site.
  Residual: `integration/quasisyntax_test.go` and `extensions/eval/prim_eval_test.go` are blocked
  by their own harness (a bare `NewEngine` with no source FS resolves no import), a one-line
  cleanup independent of this defect.

- [ ] **RESIDUAL: the writer-side defect survives, and it belongs to Stage B**
  [Medium, M, filed 2026-09-09 by the fix above]: all three repairs are reader-side. The
  bad `(ExactPhase(N>0), mutable)` slot is still created, so any OTHER phase-N reader of
  that name still sees the import first. It is filed rather than fixed because there is no
  safe writer coordinate today: `installImportedBinding`'s own doc explains that
  `(ExactPhase(1), sealed)` would land an imported macro on exactly a bootstrap macro's
  coordinates with the same ambient scope set, so `CreateGlobalBindingAt` REUSES the slot,
  `created == false`, `importConflicts` returns false (the bootstrap macro is not
  `IsImported()`), and `SetOwnGlobalValue` overwrites the sealed transformer IN PLACE,
  ENGINE-WIDE — "from the outside, the import works". Stage A's D12 (route every import
  through a bulk row of its own) was declined in that same comment. Reopening it is a
  design decision, not a bugfix.
  The Stage-B-shaped question underneath: should `else` at phase 1 come from an IMPORT at
  all, or from the dialect's declared initial-import rows? `pkg/machine/compilation/er_macro_compare_test.go`
  currently depends on the import supplying it, which is what killed the writer-side variant.
  A cheap partial gate in the meantime: no import may land at T1 above a sealed phase row.

- [x] **The two reader gates STAY, and the reason is recorded in their own doc blocks**
  [settled 2026-09-10, Stage B Task 7, fork A answered A2]: they are not a workaround for a
  RANKING error Stage B would close. (1) **R7RS-small has no answer:** §5.6's `<import set>`
  admits `only`/`except`/`prefix`/`rename`; `for-syntax` is R6RS/Racket. (2) **The references
  disagree** (four-way: master, branch, `petite`, `racket`): Chez is SILENT, first-listed-wins
  (measured non-vacuously); Racket REFUSES require-vs-require but SHADOWS require-vs-language,
  hierarchy definition > require > language, discriminating on WHAT KIND the collidee is, not its
  phase. (3) **Wile's collidee is the LANGUAGE at phase 1** (`RegisterPhaseBindings`' rows, the
  `#lang` analogue), so under Racket's own rule the import out-ranking it is RIGHT; the design's
  `raise-already-bound` citation answers require-vs-require, which Wile already ships as
  `importConflicts`. (4) The winner passes `BindingType() == BindingTypePrimitive` while its VALUE
  is not a `*PrimitiveExpander`, which is why a reader must look past it to the sealed row; each
  library env mints its own `*Binding` per re-exported name, and `sameLiteralBinding`
  (`pkg/internal/match/syntax_adapter.go`) widens on `BindingTypePrimitive` for the same fact.
  Point 4's "identity defect" diagnosis as the gates' cause is struck by the follow-on below.

- [ ] **FOLLOW-ON: share binding identity across the import edge** [filed
  2026-09-10; **its stated MECHANISM STRUCK 2026-09-13, its PURPOSE reassigned**]:
  give a re-exported name ONE `*Binding` across the import, or give the base its
  own store so an import and the language never share a name table.

  ~~**That** is what makes the two reader gates removable, and it is the only
  thing that does.~~ **MEASURABLY FALSE, and it is the sentence that mis-scoped
  every attempt to act on this item.** Neither gate's predicate can see pointer
  identity. Gate 1 tests `bnd.BindingType() == BindingTypePrimitive` then
  `bnd.Value().(T)` (`pkg/machine/compilation/phase_registry.go:166-170`); gate 2
  tests `bnd != nil && bnd.BindingType() == BindingTypePrimitive`
  (`pkg/machine/compilation/expander_time_continuation.go:476`). The import
  already supplies both inputs verbatim: `installImportedBinding` takes
  `libBinding.BindingType()` as its create argument and writes `source.Value()`
  at the sole value-write site on any import path
  (`pkg/machine/compilation/library_bindings.go:736`). Make the pointer identical
  and both predicates evaluate exactly as they do today. The two production doc
  blocks that asserted the same thing are corrected in the same pass.

  **What the gates are actually caused by, and what to do instead.**
  `findLibraryBinding` walks `lib.Env.PresentPhases()` ASCENDING and returns the
  first hit, so the LOWEST phase wins regardless of the phase the import will
  install at (`library_bindings.go:481-503`, pinned by
  `TestFindLibraryBindingPrefersRuntimeOverExpand`). `(scheme base)` holds
  `syntax-rules` at phase 0 as a `*SyntaxCompiler` and at phase 1 as a
  `*PrimitiveExpander`; the export walk hands a for-syntax import the phase-0
  one, which then fails gate 1's type assertion at the importer's phase 1. That
  is fork (a) above, already decided and unshipped, and it is the item that
  should carry this work. Its rider stands: `validateLibraryExports`
  (`compile_library_forms.go:300-312`) makes the SAME first-hit call and
  discards both returns, so fixing the walk alone leaves the false green.

  **Three mechanisms for fork (a) were designed and all three took a fatal on
  adversarial review (2026-09-13, 20 agents).** Recorded so they are not
  re-proposed as novel: (i) a `targetPhase`-gated selector re-looks-up the NAME
  in the library env at another phase, where `ScopesCompatible(∅, {libScope})`
  is unconditionally true, so it substitutes a library's private
  `begin-for-syntax` define for the runtime export a name also has, and it never
  fires on fork (a)'s own pinned row (`library_export_phase_order_test.go:110-130`,
  a `targetPhase == 0` import); measured, it also swaps the installed object for
  105 of 233 ordinary procedures, whose replacement closure captures
  `env.Expand()` rather than `env` (the G12 closure-env blocker). (ii) A
  writer-side guard suppressing the redundant install bypasses the R7RS §5.6
  conflict check outright: `importConflicts` runs behind `!created`
  (`library_bindings.go:730`), so with nothing installed there is no `target` and
  no check. (iii) Giving the language its own store blinds every sealed-tier
  reader at once, because `SealedBindingAt` is `probeRankedLocked` over slots
  alone (`global_environment_frame.go:1409-1414`) and row consultation exists
  only in `resolveRankedLocked:660-663`; `headDenotesSpecialForm`,
  `quasiHead`'s anti-hijack pin, and `namespace-undefine!`'s startup-set refusal
  all fail OPEN and all sit outside that proposal's file list.

  **The unmeasured fourth option, which is where a re-opener should start:**
  discriminate the export hit by SCOPE PROVENANCE, not by phase preference.
  `findLibraryBinding` already merges two populations and `Binding.Scopes()`
  already separates them: a `{libScope}` hit is the library's own definition
  (lowest present phase is correct there, and is what makes `(define foo 1)` plus
  `(begin-for-syntax (define foo 2))` export the runtime one), while an
  ∅-ambient hit is a registry or import row the library merely re-exports, where
  "lowest phase" means nothing. It cannot commit (i)'s fault because it never
  reaches a library's own `begin-for-syntax` define. **Two costs nobody has
  priced, and they gated the other three silently:**
  `TestFindLibraryBindingPrefersRuntimeOverExpand`'s fixture is entirely ambient
  (`NewCompiledLibrary` with a nil `Scope`, `DefineOwnGlobal(..., nil)` —
  `library_bindings_test.go:212-221`), so its wants must be deliberately
  re-specified rather than assumed to survive; and the propagation arm is
  `placementInPlace`, so an un-renamed two-phase re-export lands on a sealed
  phase-1 coordinate, the engine-wide overwrite `library_bindings.go:826-841`
  refuses to relocate.

  **The gates are not the justification for any of it.** Delete them afterwards
  only if they measurably fall out, keeping `phase_registry.go:174-176` (the
  `0d1204c6` keeper). Their whole measured symptom is a `GoSyntaxFormCompiles()`
  delta of +1 plus value tests that pass today; every removal designed so far
  traded a loud, ranked, documented workaround for a silent one.

  **Identity sharing survives only under its own motivation, which is real and
  is NOT the gates** [Medium, L]: the stale imported variable (a library `set!`
  after import leaves the importer reading the old value; Racket reads through,
  Chez refuses the program) and the R7RS §4.3.2 over/under-acceptance. Note that
  `eq?`-across-import is NOT among them: the value object is already shared at
  `library_bindings.go:736`, and `eq?` answers `#f` because each library env
  re-mints its closures (`pkg/registry/apply.go:267`). The sound residue is
  sharing the storage LOCATION, not the object; sharing the OBJECT is refused on
  three independent grounds — `m.Imported` is half the slot coordinate
  (`global_environment_frame.go:1627`, `:1065`), `Binding.Scopes()` is the store
  key and the two sides need different sets (`:1630`, `:1112`;
  `library_bindings.go:708` vs `:485-489`), and the inline-HOF stamp is keyed on
  the per-import `exportName` so a re-exported sealed-base HOF would be
  permanently de-stamped at its home (`inline_hof.go:213-215`, `:230-233`). That
  shape was already run once by accident at `183171a1` and is written up in
  `installImportedBinding`'s own doc (`library_bindings.go:645-661`). Any re-open
  must move `Imported` and the scope set off `*Binding` onto `slotRef` FIRST.

  **Scope note: row 3 of the partition below was never in scope** (see that
  entry).

- [x] **The per-site failure partition, measured 2026-09-10** [scratch worktree at `c28616c1`,
  `GOWORK=off`, `make build` first]: recorded so the follow-on inherits its sensors. **Three
  SITES, two of them the reader gates**; each reverted ALONE:

  | # | Site | Reverting it alone reddens |
  |---|---|---|
  | 1 | `LookupPhaseBinding` (`pkg/machine/compilation/phase_registry.go`) | `TestPhase1BaseImportIsBehaviourNeutral` (6/6), `TestPhase1BaseImportMasksNoPhaseRow` (20 of 40 rows masked), `TestP02_ExpandOnceMirrorsTheLoop`, `TestP05_FreeIdentifierEqualShadowProbe`, `TestP2_ERRenameIsFreshPerInvocation`, `integration/TestERMacro_Mixed` |
  | 2 | `lookupMacroBinding` ARM 2b's `masked` gate (`pkg/machine/compilation/expander_time_continuation.go`) | `TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules`, `TestP2_SyntaxRulesAndERAreScheme`: both `compilation.GoSyntaxFormCompiles()` COUNTER deltas of +1; every value assertion stays green |
  | 3 | NOT A GATE: the ER rename closure's `.AtPhase(p.env.PhaseLevel())` (same file) | `TestERRenameDenotesTheOutputPhase` (2/3), `integration/TestERMacro_Cond` |

  The partitions are DISJOINT, which is why rows 1 and 2 are two gates, not one. **Row 3 is not
  a gate** (design B13: no `BindingType` test) and sharing binding identity would not retire it:
  a phase-1-ONLY import has no phase-0 slot to share identity with, so the smuggling hazard
  survives. **Do not fold row 3 into the follow-on; "three reader gates" is the recurring
  miscount.** The earlier filing's partition was wrong in both directions (missed three
  `pkg/wile` tests that carry `(import (for-syntax (scheme base)))` themselves, and
  `TestP2_SyntaxRulesAndERAreScheme`). **Do not delete `phase_registry.go`'s
  `bnd.BindingType() != BindingTypePrimitive` guard when the gate goes:** it predates the fix
  (`0d1204c6`, 2026-01-09) and keeps
  `TestLookupSyntaxCompiler_SamePhaseShadowOutranksTheSealedCompiler` green.

- [x] **`TestPhase1BaseImportMasksNoPhaseRow`'s non-vacuity floor has 20 names of headroom**
  [measured 2026-09-10 at `c28616c1`]: `len(before)` is **40** against a floor of `> 20`. The
  floor asks "is this test looking at anything", the `masked` assertion asks "did the import move
  any of them". A change dropping the expander table from 40 to 21 keeps reporting green while
  sensing half as much. A population that narrows must surface as an assertion failure, not as a
  quietly smaller universe the `masked` loop walks.

### Stage B S4 is STRUCK — the price, and what would have to be true first (2026-09-10)

S4 ("fold phase into the scope set as a representative scope, so `probeTiersLocked` loses its
`phase` parameter") is not happening. This records its price so it is not re-derived, and refiles
the design's two remaining §8.2 blind spots as preconditions. Figures re-measured at `c28616c1`.
**The rot has a direction**: of sixteen Stage B figures re-measured, all sixteen were wrong and
every one LOW, because code accretes, so a stale count systematically under-states the work. A
correction must record its command too: one "correction" was itself wrong and reverted in
`5d69a2fd`. **Re-run any figure in the Stage B documents before relying on it.**

- [x] **Q4, "can the phase registry be removed?", answers NO** [Done 2026-09-10]: four blockers.
  `PhaseRegistry.sealedViews` is the sole backing for `SealedWriteViewAt` (5 production callers) and
  `AtPhase`'s sealed climb; `GetOrCreate` is the only `EnsureMacroPhaseRows` hook, and the lazy,
  unbounded tower has no phase set to enumerate; `AtPhase` panics `ErrMissingPhaseRegistry` without
  it; the stable-pointer-per-(owner, phase) contract lives in `GetOrCreate`'s doc. **Do NOT scope a
  step as "rewrite the two `Phases()` consumers"** (`ApplyDocs`, `firstPhaseBinding`): `PresentPhases`
  itself calls `p.phases.appendPhases`, so freeing them frees nothing. The design's §7
  "frame-per-phase dissolves" is struck.
- [x] **The S4 inventory, as a price** [Done 2026-09-10]: non-test, tree-wide, at `c28616c1`.

  | Item | Plan said | Measured |
  |---|---|---|
  | phase-VALUE comparison bodies | 8 | **16** tree-wide, **10** in `pkg/environment`; a looser sweep reaches 18-19 |
  | struct fields typed `Phase` (incl. `map[Phase]…`) | 3 | **12**; **8** in `pkg/environment` |
  | signatures mentioning `Phase` | ~24 | **61** |
  | distinct widths | 3 | **3**, but **4 declarations** carry them |

  The bitset declarations (`exactPhases [2]uint64`, `macroPhaseSeenBits [2]atomic.Uint64`,
  `registry.PhaseSet uint8`) are dense-small-integer assumptions in TYPE form that no `>>6` grep
  finds. `appendExactPhases` is the reverse mapping `PresentPhases` needs, and `PresentPhases` is
  sort + compact + trim, so a fold that loses order loses dedup. Only 3 of the 10 `pkg/environment`
  comparisons are on the ranked-read path, so a read-side inventory finds under a third. The count
  is definition-sensitive (binary comparisons with a `Phase`-typed operand, excluding nil tests of
  `*PhaseRegistry` and `macroPhasesSeen`); quote the direction, not a bare number.
- [x] **The design's `ScopesCompatible` warning named the wrong artifact** [Done 2026-09-10]: its
  empty-set short-circuit is a no-op, since `ScopesMatch(u, ∅)` is already true; deleting it passes
  `go test ./...`, so an implementer told to preserve it would see green and stop trusting the
  document. The rule a fold would break is `∅ ⊆ X` inside `ScopesMatch` (12 direct calls in 5
  packages, plus 9 `ScopesCompatible` sites delegating). Three `ScopesMatch` sites are unreachable by
  editing `ScopesCompatible`: two in `internal/match` compare a pattern's scopes to a template's (a
  different relation), and `resolveNodeByScopes` is the frame-reclaim authority, whose false positive
  is corruption, so any change to the relation must reach it on purpose. `pkg/values/scope.go`'s doc
  now names all nine delegating consumers and that exception.

- [ ] **PRECONDITION 1 for any future S4: `ScopedBindingKeyOf`'s split is only half
  pinned** [Low, S, filed 2026-09-10]: the collapse direction has four ratchets
  (setting `ScopeKey := ""` reddens `validate_test.go`,
  `lambda_formals_hygiene_test.go` ×2, `framereclaim_scope_collision_test.go`), so
  `findDuplicateSymbols` is NOT unpinned. The frame-reclaim direction was fail-open and
  is now guarded by `TestBuildReclaimGraph_CollidedIsScopeKeyed`
  (`pkg/internal/validate/frame_reclaim_build_test.go`, landed `57353e22`), which is
  deliberately **scope-CONTENT-specific** — a uniform constant suffix appended to every
  `ScopeKey` preserves the partition and is invisible to a "duplicates are still caught"
  assertion. What remains open is that any S4 changing what a scope set CONTAINS must
  re-derive the expected key set from `syntax.ScopeFingerprint` rather than from the
  producer under test, or the ratchet stops discriminating.

- [ ] **PRECONDITION 2: `GlobalIndex.EqualTo` and `MaybeAppendLiteral` disagree about
  what a pin IS** [Low, S, filed 2026-09-10]: `EqualTo`
  (`pkg/environment/global_environment_frame.go`) compares `Index`, `Env` and `Slot`
  and deliberately ignores `query`, `phase` and `sealed`; the literal pool dedups
  through it (`pkg/machine/native_template.go` `MaybeAppendLiteral` → `literalIdentical`,
  the linear arm, since `*GlobalIndex` is not `Hashable`). So two pins that differ only
  in their re-resolution coordinates merge into one literal. The fields exist precisely
  to survive slot DEATH, which is after the merge has happened.

  **Widening `EqualTo` is not obviously free, and the usual objection is measured
  away.** The fear is that including the fields makes two pins to one LIVE slot
  distinct literals. Measured: widening `EqualTo` to compare `phase`, `sealed` and
  `query` moves the literal pool by **0 entries** — 9599 appends / 1809 `*GlobalIndex`
  literals, identical before and after, over a KitchenSink engine plus a program
  importing 14 libraries and defining three macros. The structural reason is in
  `GlobalIndex`'s own field doc: while a slot lives it is named by exactly one
  `slotRef`, so `(Env, Slot)` already determines `(phase, sealed)` and those two can
  never discriminate. Only `query` can differ for one live slot, and it did not occur.
  One test does go red, `TestCompileContext_CompileSetBang`, and it is a
  test-construction artifact: it builds an expected `*GlobalIndex` by hand, so its
  coordinates are zero values. Cost of the fix is therefore that one test, not pool
  growth — but re-measure on the workload of the day rather than trusting this line.

- [x] **`FreeIdKey`'s blast radius, and the encoding invariant any S4 must prove** [Done
  2026-09-10]: 3 production call sites (`compile_syntax_rules.go` ×2, `syntax_expand.go`); the inverse
  `FreeIdName` has zero production callers. The contract pin is `TestFreeIdKey_DiscriminatesScopeAndName`,
  which the design's ratchet list omits. The "nine failures across four packages" figure from
  `plans/2026-07-18-scope-keyed-global-bindings-design.md` is provenance, not a measurement of today's
  tree ([[freeids-collapse-is-wrong-pin-not-keying]]). **Residual (precondition):** `FreeIdKey` is
  `ScopeFingerprint(scopes) + "|" + name` and `FreeIdName` cuts at the FIRST `|`, which is unambiguous
  only because the fingerprint's charset is `[0-9,]`. Any slice putting a phase into the fingerprint
  must prove no `|` enters it, or every key silently truncates and surfaces as a wrong self-reference,
  not a parse error; a negative phase adds `-`, so move the charset doc with the encoding.

### Name-keyed identity survives in consumers of scope-keyed bindings (2026-07-19)

Consequences of `8afeb66a`/`a60e32e1` making one name own several slots, each previously visible only
inside a `plans/` section. The count grew from three to eight across two later reviews, which is the
argument against trusting a hardcoded one. **All eight are closed** (2026-07-29); the one open item
below is a separate pre-existing defect found while measuring #805.

- [x] **Frame-reclaim's verdict domain was name-keyed** [Medium, M, Done 2026-07-23, branch
  `fix/framereclaim-scope-keyed-verdict`]: `ClassifyFrameReclaim` returns
  `map[validate.ScopedBindingKey]bool`, the `{Sym.Key, ScopeFingerprint(scopes)}` identity shared with
  `findDuplicateSymbols` and `match.FreeIdKey`, not the physical-slot `BindingID`; hygiene-distinct
  same-name defines get separate verdicts. Edges resolve by subset over nodes without consulting the
  binding, completing the T1.5 decoupling. **Subset, not fingerprint equality, is load-bearing** for a
  nested cross-call whose ref scopes exceed the callee's (`TestFrameReclaimSeam_LetNestedMutualCallResolves`).
  `resolveNodeByScopes` **refuses** an ambiguous maximum rather than pick by map order, since
  `GetBinding`'s creation-order tie-break cannot be cheaply replicated and a false positive corrupts
  (`TestResolveNodeByScopes_AmbiguousMaxRefusesToGuess`). Chose scope-set identity over `*Binding`
  pointer keying.
  - [x] **Sub-hole: the verdict leaked below top level** [Correctness, S, Done 2026-07-20,
    `82046952`]: `CompileValidatedLet` compiles a let body on the same compiler with `p.env` swapped
    (the package's only such swap), so an internal define sharing a top-level `Key` collected the
    top-level verdict. Fixed by reading through `unitFrameReclaimVerdict`, which can withhold a verdict,
    never grant one (`pkg/wile/framereclaim_letbody_leak_test.go`). Identity keying now also covers
    it; the gate is kept as an explicit tightening.
- [x] **`MaybeCreateLocalBinding` used `ScopesCompatible` where exact equality is correct** [Medium,
  S, Done 2026-07-20]: a `{m}`-scoped binder could reuse and clobber a `{}`-scoped slot. **The bug was
  using a visibility predicate to decide identity**; creation now calls `scopeSetsEqual`, the global
  creation predicate itself, so the two paths cannot drift, and lookup keeps `ScopesCompatible`.
  Latent: no known Scheme program reaches it (a body binder carries the body scope; nil-passing
  callers use dedicated frames), so the guard is a unit test,
  `TestMaybeCreateLocalBinding_EmptyScopedSlotNotReused`. Only one direction was broken, so a
  one-directional test would pass a wrong fix. The reuse branch's `Scopes` backfill, half the clobber
  mechanism, was deleted rather than pinned.
- [x] **`DeleteBinding` was name-keyed while the namespace read surface is scope-exact** [Medium, S,
  issue #805, Done 2026-07-20]: `(namespace-undefine! ns 'x)` destroyed a macro-introduced `x` that
  `namespace-ref` reports unbound. `DeleteBinding(sym, scopes)` now resolves through the read's own
  resolution (today `resolveAtCoordsLocked`), so delete cannot drift from ref. **Delete has no
  wildcard mode**: `nil` means the empty set, deliberately diverging from the file's then-current
  nil-means-match-any convention, so the ambient footgun cannot fire destructively. The old
  delete-all tests were inverted, not updated (`TestGlobalFrame_DeleteClearsMultiSlotNameOneScopeSetAtATime`,
  `TestNamespaceUndefine_RemovesAmbientAndSparesMacroBinder`). The sealed-base probe became
  scope-exact too: a wildcard probe raised `ErrImmutableBinding` for a name the read calls unbound, a
  spurious denial rather than a permission.

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
  API, M, Phases 0–2 done 2026-07-23, `d2e2e625` + `eaee2ed0`]: the read surface treated a nil scope
  set as MATCH ANY, against the nil-means-NONE convention, so a caller that forgot to thread scopes
  silently got a wider resolution, and that resolution was `slots[0]`, an expansion-order artifact,
  not a union. An earlier "fail-open / security posture" framing was withdrawn 2026-07-20: the effect
  can land either way (see the spurious denial above) and hygiene is a correctness boundary, not an
  authorization one. **Fix**: `values.ScopeSet` carries All / empty / specific, collapsing
  `AmbientScopes()`, the riding `matchAny` bool, and `GlobalIndex.scopeKeyed`; nil is un-passable at
  the query surface and `scopesToQueryMatchAny` is deleted. All 58 call sites were **triaged per
  intent, not mass-rewritten**, since a mechanical `nil` → `AmbientScopes()` sweep would freeze
  accidents into explicit form. **Two wildcards kept deliberately**, with rationale comments at each:
  `compile_syntax_form.go`'s pattern-var lookup (pattern vars are bound scopeless innermost, and a
  scoped query switches to maximal resolution where an enclosing lexical could outrank them), and
  `expander_time_continuation.go` ARM 3 (the helper's definition scopes are not a subset of the use
  site's, so a scoped query breaks cross-library helper resolution). Narrowing both kept the suite
  green, so the verdict rests on resolution semantics, not the suite.
- [x] **`freeIds` collapsed scope-verified answers into a name-keyed map** [Correctness, S, Done
  2026-07-20]: two same-named template identifiers with different scope sets resolved correctly, then
  the second overwrote the first in storage. Reachable from surface Scheme: a macro-generating macro
  gave `(99 99)` where hygiene demands `(1 99)`. Fix: `match.FreeIdKey(name, scopes)` at the one writer
  and one reader. Exact-scope-set keying, not Flatt-maximal, is right because it replays a
  per-occurrence answer keyed by the same immutable template symbol at both ends. Pins:
  `TestMacroGeneratingMacro_SameNameFreeIdsDoNotCollapse`, `TestFreeIdKey_DiscriminatesScopeAndName`.
- [x] **`BindingID` needed a scope discriminator before the load-order plan shipped** [Correctness +
  sequencing, M, MOOT 2026-07-24]: the load-order plan is archived as superseded, so its
  `BindingID{Origin, Phase, Sym, Local}` will not be built; its spec was corrected anyway to carry
  `ScopeKey string`, because a name-keyed version would have been a fourth wrong notion of "same
  binding" instead of the one that replaces three. The shipped `BindingID{*LocalEnvironmentFrame, slot}`
  is a physical local slot and is not this.
- [x] **The self-tail / frame-reclaim family decided self-identity by name** [Correctness, M, Done
  2026-07-29, branch `fix/selftail-scope-identity`]: both repros failed open (hangs), each against a
  distinct-name control.

  | repro | mechanism | before | after |
  |---|---|---|---|
  | (a) named-let arm | emit gate | 2 sites / hangs | 1 site / `300` |
  | (a) `define` arm | emit gate | 2 sites / hangs | 1 site / `300` |
  | (b) macro-hidden `set!` | shadow set | 2 sites / hangs | 0 sites / raises |

  The `define` arm was a second LIVE repro, not "exposed but unproven": immutable top level is the
  DEFAULT, so it arms (`TestSelfTailEmit_DefineArmEscape`). In the arming ratchet, `tak` explicit-
  **mutable** 0 is the discriminating row, not the immutable one. **Roles, from the design pass**
  (`memory/2026-07-29-name-keyed-identity-residuals-{design,impl}.md`, Branch B): the emit gate is the
  *authority* (a false positive corrupts), `calleeCaptureSafe` is *safety*, the mutation/tail walks
  are *applicability* (errors are deopts once the gate is exact). So 7 signatures move to identity
  and 5 keep `string`; `exprMutatesName` **must** stay name-only, since narrowing it risks missing a
  `set!` of the real self. Self-identity is resolved-`*Binding` comparison (subset-based:
  `ScopedBindingKey` equality would silently total-deopt every loop). `nameSet` is
  `map[string][]localBinding` with per-entry scopes, not `ScopedBindingKey`-keyed, because a
  fingerprint cannot answer a subset query. The shadow query is **tri-state** because
  `exprMutatesName` reads "shadowed" with the opposite safety polarity from the other consumers; its
  `shadowUnknown` branch killed nothing and is pinned as a tightening
  (`TestExprMutatesNameReportsThroughAmbiguousTie`). Two plan corrections: internals take the
  resolved `*Binding`, not `selfSym`; and the unresolvable-self guard's "fails open" justification
  was wrong (the walkers stay name-fed), so it is kept on another ground: a wrong env becomes an
  arming-count drop the ratchet catches. `LetBindingFrameReleasable` / `InternalDefineFrameReleasable`
  pass `self = nil` deliberately.
- [x] **`CompileSymbol`'s definition-time pin outranked the scoped *global* match** [Correctness, S,
  Done 2026-07-29, branch `fix/compilesymbol-cointroduced-global`, `bff525a8`]: Option A with the
  cardinality guard, a global-only arm between `GetLocalIndex` and `tryResolvedBinding`
  (`GetGlobalIndexWithScopes` + `GetOwnGlobalBinding`, not `GetBinding`, which searches locals first).
  `(entry 0)` 0 → 21; an unfiled co-introduced-global case went `(100 100)` → `(7 100)`. Dropping the
  cardinality guard (the naive reorder) breaks the cross-library free-identifier class the pin
  protects (`TestGlobalIndexCollision_*`, `TestLibraryInternalSyntaxCaseEllipsisHygiene`). Pin:
  `TestScopeResolution_CoIntroducedGlobalShadowsPin`. **Report correction**: the "user `rec` defined
  after the expansion" variant is the same defect; `predeclareBodyDefines` makes the
  **compilation-unit boundary** the discriminator, not source order. **Residual:**
  `collectFreeIdentifiersWithEllipsis` calls every non-pattern-variable template identifier free,
  including ones the template binds; filtering is undecidable in general (a template may use a
  binding macro), so it would be a partial precision gain, not a substitute (design's "Option D
  rejected").
- [x] **`lookupMacroBinding` arm 1 had no cardinality guard (for-syntax symmetry)** [Correctness, S,
  Done 2026-07-29, branch `fix/compilesymbol-cointroduced-global`]: proven reachable; the filing was
  right about the phase and wrong about the site. `begin-for-syntax` / `define-for-syntax` /
  `eval-when` root the expander at `p.env` and climb via arm 2's `NextPhase()`, which is why the filed
  probes could not discriminate. The reaching site is a **procedural transformer body**:
  `compileAndEvalLambdaTransformer` (shared by `lambda` and `er-macro-transformer`) roots at
  `env.NextPhase()`, where phase-0 `define-syntax` deposits `∅`-scoped keywords. `user-helper` →
  `lib-helper` (`TestForSyntaxPin_TransformerBodyKeepsPinnedTemplateID`). Fixed by **demoting** the
  `∅`-scoped arm-1 match below the pin, not dropping it: arm 1 is the only arm that finds a
  current-phase macro, so a drop regresses every direct macro reference in a transformer body. Both
  arms share `coIntroducedByExpansion`. Phase-0 expansion was confirmed safe: no `∅`-scoped syntax
  global lands in phase 0.

### plans/ sweep — correctness deltas not previously in TODO (2026-07-21)

Correctness work that lived only inside plan files, invisible to a TODO scan.

- [x] **stderr data can be lost on exit** [Correctness, S, Done 2026-07-21, no longer reproduces]:
  `writeAndFlush` (`pkg/extensions/io/prim_write.go`) flushes after every write primitive, so no
  output is lost on any exit path (verified on the built binary). That supersedes the 2026-05-14
  flush-on-exit design, which said "no per-message flushing is added" and would now be redundant
  machinery; it also missed `(exit)`/`(emergency-exit)` in `extensions/system/prim_system.go`, the
  bug's own canonical repro. `memory/2026-05-14-stderr-flush-on-exit.md`.
- [x] **SRFI-18 `thread-join!` wraps an uncaught exception** [Correctness/conformance, S, Done
  2026-07-21]: a joined thread that died on an uncaught exception surfaces as an
  `uncaught-exception` whose reason is the original condition (`eq?`-preserved), not the bare
  re-raised condition. Q1 = A: wrap unconditionally, strict SRFI-18. Guard:
  `TestThreadJoinWrapsUncaughtException`.
- [x] **SRFI-18 exception predicates + `thread-state`** [Correctness/conformance, S, Done
  2026-08-02]: `join-timeout-exception?`, `terminated-thread-exception?`, and
  `abandoned-mutex-exception?` added. The three conditions are now opaque `values.Value` handles
  raised via `machine.RaiseInPlace`, not string-only error-objects. Behaviour changes: joining a
  `thread-terminate!`d thread raises terminated-thread-exception, not uncaught-exception; none of
  the three is `error-object?`. `thread-state` is Gambit's, **not** SRFI-18 (which has only
  `mutex-state`); the stale `pkg/values/thread.go` comment claiming otherwise was corrected. Guards:
  `TestSRFI18ExceptionPredicatesDiscriminate`, `TestThreadState`. **Residual:** `(current-thread)`
  returns the symbol `'primordial` on the main goroutine, so `(thread-state (current-thread))` errors
  there; a pre-existing SRFI-18 gap.

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
  `plans/2026-07-24-free-identifier-origin-provenance-design.md` (model: "same binding" = same
  root in the provenance graph Wile already walks at import, then discards).
  **Phase 2 — 1 of 3 shipped:**
  - [x] `stampImportedInlineHOF` gates on `Origin.RootLib` [Done 2026-07-24, `9b2afa8c`]: fixed the
    latent re-export miss and a coupled miscompile, where inline dispatch keyed on the call-site
    name, so a curated HOF renamed onto another curated HOF's name inlined the wrong body. Dispatch
    keys on the stamped `BindingMeta.InlineHOFName`, reset on every re-import.
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
  - [ ] Site 3 `sameImportedBinding` (`library_bindings.go`) still compares `*MachineClosure`/
    `*ForeignClosure` by NAME with an `EqualTo` default. **Gated, not merely unstarted** — its own
    doc comment frames the by-name conflation as a deliberate irreducible gap, and origin was
    rejected here by PR #793 for false-flagging define-over-import. The gate is re-reading #793's
    actual objection: an import-edge origin is a different signal than the source location it
    rejected (a define-over-import shadow is a non-imported local with nil `Origin`, so it never
    enters an import-vs-import root comparison) — a hypothesis to verify, not a claim.
### Layered-environment carve regressions (review `d8911c15..b04c6d74`, 2026-06-15)

Sealed-base carve + immutable-top-level-default arc. Two root patterns: own-frame `Keys()`
iterators that did not span the sealed base, and the immutable default reaching contexts the
design meant to stay mutable. **Scope decision (2026-06-15):** immutability is scoped to
**compilation units only** (Chez model) — immutable for files and `-e` batches, which preserves
the frame-reclaim GC win; mutable for every interactive/eval context (REPL, `--mcp`,
`(environment …)`, `scheme-report-environment`). Implemented as a root-namespace property, with
child namespaces always mutable and the `set!`-gate keying on `IsStable()` directly so anchors
stay protected inside mutable children. Design:
`memory/2026-06-13-immutable-toplevel-by-default-scoping.md`. 14/15 closed; D1 survives.

- [x] **A1–A4, read paths dropped every sealed-base name** [Done 2026-06-15]: own-frame `Keys()`
  walks lost sealed-base names (`caar`, `map`, `zero?`, `call/cc`, the cxr accessors).
  `collectBindingNames` (REPL completion) and `searchEnvironmentBindings` (`,apropos`) now walk
  `SealedBase()`; the completer test had asserted only `car`, which survives via the expand phase and
  masked the bug. `namespace-undefine!` on a sealed name raises `ErrImmutableBinding` instead of
  reporting false success (`TestNamespaceUndefineSealedRejected`).
- [x] **B1–B4, the immutable default reached entry points meant to stay mutable**
  [Done 2026-06-15]: each REPL line is its own unit, so a redefinition was rejected. The CLI REPL
  adds `WithMutableTopLevel()`, `mcp.go` is always mutable, `runEval` begin-wraps `-e` like
  `runFile`, and child namespaces are mutable (`TestSealedBase_B3_*`). B4 closed as documentation:
  `internal/bootstrap` mutable while public `NewEngine` is immutable is mechanism vs policy, not a
  split brain.
- [x] **C1–C3, two divergent `Stable`-stamping mechanisms** [Done 2026-06-15]: a profile child
  allowed `(set! car …)` but rejected `(set! caar …)`. Profile children are mutable, and the
  `set!`-gate keys on `IsStable()` directly, decoupled from `ImmutableTopLevel()`, so a Stable anchor
  is never `set!`-able even in a mutable child (`TestSealedBase_C1_*`). C2 closed as a doc fix:
  freezing bootstrap procedures is intentional (they are anchors, and narrowing the stamp loses the
  user-recursion GC win), so `docs/reference/r7rs-differences.md` was corrected, not the code.
- [x] **D2, lock asymmetry on a thread-shared global** [Low, M, pre-existing, Done 2026-07-01,
  `fbcd7654`]: global bindings publish through an `atomicCell` (atomic publish, lock-free load),
  with the `noCopy` pointer in the heap cell so `Binding` stays copylocks-clean; locals keep the
  plain field. "In a global frame ⇒ has a cell" is structural. `binding_race_test.go` is red under
  `-race` pre-fix. **Cost: +4.6% geomean on bench-gabriel, 15/16 slower** (`Binding` 32→40B),
  accepted; the shrink-`Binding` recovery lever is a Tier 4 follow-up.
- [x] **E1–E3, altitude, duplication, and a per-import re-walk** [Done 2026-06-15]:
  `EnvironmentFrame.MutableRuntime()` replaces 8 `.Namespace().Runtime()` sites (deliberately not
  `Runtime()`, which diverges for library frames; `TopLevel()` returns the frozen base). The two
  `loadBootstrapSources` had behaviourally diverged (one skipped optimization and pooling), so
  `compilation.LoadBootstrapSources` and `wireRuntimeFrames` are now the single sources.
  `registerSchemeDocstrings` re-parsed ~500 docstrings on every `(import …)` and is now guarded by
  `!env.IsNamespaceRuntime()`.

- [ ] **D1 — sub-context/thread capture can leak library-eval defines** [Low, S — DEFERRED 2026-06-15, arguably-not-a-bug]: `machine/machine_context_subcontext.go` now captures `MutableRuntime()` (named) which returns the engine-root mutable global even from a library frame. For SRFI-18 THREADS this is correct by design (threads share the engine global). The only edge is a sub-context (`load`/`call-with-exit`) spawned *during a library's own load* landing defines in the engine global rather than the library frame — an extreme, untested case. Revisit only if a concrete isolation bug surfaces; not worth a speculative fix.

---

- [x] **Data race: error/backtrace capture vs concurrent VM mutation under `thread-terminate!`**
  [High, M, Done 2026-06-13]: `NewThreadSubContext` set a thread's `parentMC` to the live spawning
  context, so every `parentMC` walk (`CaptureStackTrace`, `findParameterInMarks`, the pool release
  counter) crossed goroutines. The root cause is narrower than "terminate doesn't quiesce":
  `parentMC` is for synchronous sub-contexts only, and an earlier fix snapshotted the parent's fields
  but kept the pointer. Fixed by severing `parentMC` for threads. Trade-off: cross-thread
  dynamic-parameter inheritance dropped (a racy live read, not SRFI-18-correct). Not surfaced by
  `make ci`, which did not run `-race` on threads. Same family as PR #561 (concurrent apply).
- [x] **Five recursion-depth bounds, counted identically** [Medium, Done]: `DefaultMaxCallDepth`,
  `DefaultMaxParseDepth`, `DefaultMaxExpandDepth` (50000, `WithMaxExpandDepth`),
  `DefaultMaxWriteDepth` (10000, reused by `Value.SchemeString`). All count root = 1, +1 per
  container descent, so anything the writer emits reads back. **Length ≠ depth was the recurring
  bug**: spine recursion overflowed the host stack on long flat lists; spines now iterate and recurse
  only into elements. Cycle detection does not bound depth. The expander guard is shared **by
  pointer** across child expanders (the parser's is per-parse). `SchemeString` cannot raise, so it
  degrades to `#<deep>` (vs the cycle marker `...`). No `WithMaxWriteDepth`: the writer has no
  engine-owned entry point.
- [x] **Parser fuzz targets + reader crash-safety hardening** [Medium, Done]: `FuzzReadSyntax` (no
  panic; every non-EOF error a located `*ParserError`) and `FuzzReadWriteRoundTrip` found 8
  pre-existing reader bugs in ~2 minutes, 5 of them host panics; corpus in
  `pkg/parser/testdata/fuzz/`. Leaked foreign error types were closed as a class by the boundary
  catch-all `locateReaderErr`; nil-at-delimiter derefs fixed individually. The round-trip target
  also caught `String.SchemeString` using Go `%q` instead of R7RS `\xHH;`. Float64-overflowing
  scientific notation promotes to `BigFloat` in the reader and `string->number`. **Deferred**: `#m`
  big floats write without their prefix, so an in-range bigfloat loses its type on read.
- [x] **Unify complex/imaginary number parsing** [Medium, Done, staff-sweep #5]: the reader and
  `string->number` duplicated the rectangular-complex grammar and had drifted into two different
  wrong answers on `+3/4i` (R7RS §6.2.5: exact `0+3/4i`). The shared grammar is fixed
  (`isExactPartString`) and the reader delegates to the pure functions, adding only location. Guard:
  `TestParseNumber_ReaderAgreesWithStringParsers`.
- [x] **Stable-matching selectors failed; matching tests did not gate CI** [High, M, Done]:
  `walk-for-cycle` stored rotation cycles newest-first (an extra `reverse`) while `apply-rotation`
  needs oldest-first; a 2-cycle is self-inverse, so the 2×2 fixtures masked it. **The CI-gate gap is
  the more important half**: 12 chibi-test files lacked `(test-exit)` and `sat-test.scm` printed
  "FAIL:" and exited 0. All now gated.
- [x] **Audit `PrimitiveSpec` `ReturnType`/`ParamTypes` annotations** [High, L, complete]: the
  four-axis framework (docs ↔ annotation ↔ implementation ↔ R7RS) is closed. Declared-too-narrow is
  **empty** (3 false positives); declared-too-wide is ~85 TypeConstraint-vocabulary gaps; ~25 are
  R7RS sub-domain refinements below `ValueType` granularity. Wile-specific primitives need a **local
  spec before they can be audited**. Load-bearing once extension contracts check at compile time,
  when unsound annotations reject valid programs. Next: vocabulary extension, separately scoped.
- [x] **Silent failures in `compilation/operation_syntax_case.go`** [Medium-High, Done, PR #732]:
  `matcher.Match` errors are no longer swallowed as "no matching clause" (gated on
  `errors.Is(err, match.ErrNotAMatch)`; the `nolint:nilerr` was the smoking gun). The bind loop's
  three branches are separated instead of falling through to `SetLocalValue(li, nil)`, and
  `SyntaxCaseState()` distinguishes nil from a type mismatch (a wrong type *can* be stored since the
  marker-interface revert). Messages gained input and source context.
- [x] **Exceptions and error stack traces** [Medium, Done, PR #657]: `SourcedError` in
  `compilation/`; `CompilationError.Source` is populated from the cause chain; datum-level functions
  have no syntax context, so callers wrap. **Residual:** foreign stack-trace entries for Native →
  Foreign → Native crossings (P3), `memory/2026-04-14-error-stack-traces-design.md` §P3.
- [x] **`read` mid-parse EOF raises a read-error instead of returning EOF** [Done]:
  `wrapMidParseEOF` converts `io.EOF` to a `ParserError` wrapping `io.ErrUnexpectedEOF` at all four
  mid-parse sites; the primitives' `errors.Is(err, io.EOF)` check rejects it and
  `WrapForeignReadErrorf` makes `(read-error? e)` `#t`.

- [x] **Error type identity** [Medium, Determined]: `CompilationError` and `RuntimeError` are
  **public boundary types**, translating internal errors to the embedder API; they should NOT
  implement `SchemeError` or `ForeignError`. Embedders match them with `errors.As`.
- [x] **vmState field coverage test** [High, S]: reflection-based, enumerating `vmState` fields and
  asserting each appears in a coverage table keyed by operation, so adding a field without handling
  it fails the build rather than silently corrupting state.
- [x] **MCP eval fails on schelog `include`** [Not a bug]: the report was missing `puzzle.scm` and
  `(set! *schelog-use-occurs-check?* #t)`; without the occurs check the puzzle infinite-loops into
  the MCP timeout.

### Macro-system defects found 2026-09-04, owned by the Scheme-specified syntax forms design

All reproduced on v1.20.0 (`8ef4c75b`). Owner for the first four:
`plans/2026-09-04-scheme-specified-syntax-forms-design.md` §5; they close with its
phases, not with point fixes, except 5.2 which is the P0 prerequisite.

- [ ] **ER macros strip scopes from pass-through identifiers** [Correctness / hygiene, S, design §5.1]:
  `invokeERTransformer` (`expander_time_continuation.go`) calls `UnwrapAll` on the input and re-wraps
  the output with bare use-site context, so any identifier that merely passes through an ER macro
  loses its scopes. `(define-syntax er-id (er-macro-transformer (lambda (f r c) (cadr f))))`,
  `(define-syntax via-er (syntax-rules () ((_ e) (let ((tmp 1)) (er-id (+ tmp e))))))`, then
  `(let ((tmp 10)) (via-er tmp))` returns **2**; the `syntax-rules` twin returns 11. The comment at
  the site calls the asymmetry deliberate; the capture it causes was not on record. Dies with the shim
  when ER is derived from `syntax-case` (design §3.5).
- [ ] **`define-syntax` dispatches on the transformer's head symbol** [Conformance, S, design §5.2,
  **P0 prerequisite**]: `compileTransformerToMachineClosure` (`compile_transformer.go`) switches on
  `syntax-rules` / `lambda` / `er-macro-transformer` and never expands the right-hand side, so a macro
  that expands to a transformer is refused: `(define-syntax my-er (syntax-rules () ((_ p) (lambda (stx)
  (p stx)))))` then `(define-syntax m (my-er (lambda (stx) #'1)))` fails with "unsupported transformer
  type". R6RS requires an expression evaluated at the next phase. Same for `let-syntax`/`letrec-syntax`.
- [ ] **`syntax-case` rejects a lone-identifier pattern** [Conformance, XS, design §5.3]:
  `(syntax-case stx () (x #'1))` fails "pattern must be a list" (`match.CompileSyntaxPattern`). R6RS
  §12.4 allows any pattern. Dies with the Go `syntax-case`.
- [ ] **Every free local referenced from a `syntax-case` or `with-syntax` body arrives boxed**
  [Correctness, S, design §5.4]: `(let ((n 0)) (set! n 1) (syntax-case #'(a) () ((x) n)))` returns
  `#&1`, the box; the same under `with-syntax` returns `#&1`; the same inside a plain `lambda` returns
  `1`. An unassigned local procedure called from a clause body raises "expected a procedure, got
  `#&#<machine-closure>`", so today's `syntax-case` cannot use a local helper at all, and no test
  caught it. The clause-body compile path in `compile_syntax_case.go` / `compile_with_syntax.go`, not
  the flat-closure boxing pass (the `lambda` control is fine). Dies with those files; pin it before
  deletion so a general boxing regression cannot hide behind the rewrite.
- [ ] **A `begin-for-syntax` define is invisible to a later transformer** [Conformance, M, **not**
  owned by the design]: `(begin-for-syntax (define (helper x) x))` then `(define-syntax m (lambda (stx)
  (helper #'7)))` fails "no such binding helper". The Tier 1 note above (2026-07-29, `lookupMacroBinding`
  arm 1) says `begin-for-syntax` / `define-for-syntax` / `eval-when` "deliberately root the expander at
  `p.env`"; that describes the lookup arms, not a decision that user-level phase-1 definitions should be
  invisible to transformers, which is what R6RS §7 and Racket give. No stdlib file uses these forms
  (Q1 of the phase-hermeticity item), so the gap has no in-tree consumer. Design §5.5 does not need it:
  its helpers live in the sealed base.

  **Corrected 2026-09-14, measured: this is a WITHIN-UNIT ORDERING defect, not a phase or scope
  one, and the message names neither.** The same five lines pass across unit boundaries and fail
  inside one unit:

  | Delivery | Result |
  |---|---|
  | one form per REPL unit (piped to `./dist/wile`) | `7` |
  | the identical text in a file (one `(begin …)` unit) | `no such binding "helper" with compatible scopes at phase 1` |

  `begin-for-syntax` is `expandUnchanged` at expand time (`primitive_expanders_registry.go:52`)
  and runs its body only when the COMPILER reaches it, while `define-syntax` compiles and
  evaluates its transformer during the EXPANDER's body scan. So within one unit the transformer
  is built before the preceding `begin-for-syntax` has run, and `helper` does not exist yet.
  Across units the compile of unit 1 completes first and it resolves.

  "with compatible scopes" is a misdiagnosis the diagnostic invites: instrumented at the raise,
  a repeat lookup under `syntax.AllScopes()` also misses (`anyScopeHit=false`), so no scope set
  would have found it. The scoped arm is simply the last one tried. Whatever fixes the ordering
  should also stop that arm reporting a scope refusal when the name is absent outright.
- [x] **A `define-syntax` at phase > 0 is unusable from every rung** [Conformance, Done
  2026-09-13, `2e1e55c2`]: the keyword twin of the item above. `executeFormsAtCompileTime`
  (`compile_helpers.go`, shared by `begin-for-syntax` and compile-time `eval-when`) and
  `CompileDefineForSyntax` compiled the body against `p.env.NextPhase()` but rooted the expander
  at `p.env`, so a `define-syntax` in the body deposited at N+2 while lookup read N+1: off by
  one rung at every rung. Both sites now root the expander at `expandEnv`, as
  `compileTransformerValue` always did (`expand_and_compile.go:49`); both one-line halves are
  load-bearing. Enclosing-phase macros still resolve through `lookupMacroBinding`'s ambient arm.
  Pinned by `pkg/wile/phase1_define_syntax_reach_test.go` (five rows, plus a guard that a phase-1
  keyword stays invisible at phase 0: the fix lifts the deposit, it does not flatten the tower).
  The `begin-for-syntax`-define item above is a **different mechanism** and stays open.

### Reader residuals of the 2026-07-31 `#` dispatch rework

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

  **Scope narrowed 2026-08-09** (W5 cluster A): the ⟨infnan⟩ keywords and the
  unit imaginaries `+i`/`-i` now terminate implicitly too, so `+i2` and
  `+inf.0x` are single identifiers rather than a number plus a second datum.
  That rule lives in the sign-initial arms of `tokenizer_numbers.go`, **not** in
  `requireDelimiterAfterRadixNumeral` — extending the guard to `r == 0` as this
  item proposes would make `+i2` a read *error*, which is equally nonconformant.
  What remains open is only the unprefixed numeral: `1abc` still splits.

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

### Closed in Tier 1

Fully closed sections, collapsed to the archive form on 2026-09-15 and moved below the
open work. Headings kept so old references still find them.

#### The `race` CI job runs the multi-threaded tests only (2026-08-15)

- [x] **`make test-race` runs a derived multi-threaded selection, off the merge path** [Tooling,
  Done 2026-08-15]: the job had been red since `f483b4dd` added `test/scheme_inprocess_test.go`,
  a 30m budget overrun (not a deadlock): the detector reports only on concurrent access, so a
  single-threaded test pays 10–30× for no signal. `tools/sh/race-selection.sh` **derives** the set
  (a goroutine spawn, a `t.Parallel()` call, a Scheme thread/channel/mutex/timer call, at test-file
  granularity, plus every test of a package whose production code spawns a goroutine);
  `make test-race-list` prints it. **Derived on purpose: a hand-kept list fails OPEN.** Traps kept:
  the patterns are anchored as *calls*, because some files mention `t.Parallel` or thread
  primitives only to forbid or deny them; roots come from `go list`, after a hardcoded walk
  silently skipped `coverage/`, `integration/` and `tools/`. Two ratcheted exclusions:
  `TestSchemeSuiteInProcess` (fails the script if any `*-test.scm` under its roots starts a
  thread), and `cmd/wile`'s production-goroutine rule (`EXCLUDED_PROD_PKGS`; its one goroutine is
  the untriggered `setupSignals` SIGQUIT handler; fails if a second site appears). Not a
  whole-package drop: `mcp_test.go`'s goroutine tests stay, including
  `TestHandleEval_ZeroTimeoutDoesNotWedgeServer`.
  The job moved to `race.yml` (daily `schedule` + `workflow_dispatch`), so **nothing on the merge
  path covers concurrency**: a race can sit on master up to a day. Run `make test-race` or
  `gh workflow run race.yml` before landing anything touching threads, channels, mutexes, or shared
  compiler state. **Budget:** `-timeout` is per `go test` invocation, i.e. per package, so it bounds
  one hung package, not the run. `-timeout 8m` is 2.1× the slowest package (`cmd/wile`, 3m46s on a
  runner, run 31921030568, before its exclusion) and `timeout-minutes: 25` clears total +
  two per-package timeouts, because `go test`'s own timeout goroutine dump is the entire
  diagnostic and a job timeout kills the runner without one. Re-derive from the
  `race-selection: NmNNs total` line the script prints.

#### `(environment '(wile <profile>))` is ungated and crosses profile boundaries (2026-07-29)

- [x] **Gate the profile-environment constructor** [High, M, Done 2026-08-07]:
  `(environment '(wile kitchen-sink))` from a `Small` or `ConsoleWithLoad` engine returned a
  namespace carrying the named profile's extensions with no `security.Check` on the path
  (`PrimEnvironment` → `tryWileProfile` → `eval.ProfileFactory`). Gated resources stayed under
  the engine's authorizer (the child copies it); extensions with no gate sites (`threads`,
  `gointerop`, `namespace`) became reachable. Fix: `checkProfileWidening`
  (`pkg/internal/bootstrap/profile_containment.go`), called from the `ProfileFactory` closure.
  Decisions: (1) vocabulary is a new `security.ResourceNamespace` + `ActionCreate`, not
  `code:eval`, which would let an authorizer permitting eval under a confined root also hand over
  `gointerop`; (2) policy is **containment over primitive names, not extension identities**, which
  is load-bearing: `Console` carries `all.SafeExtension` and `kitchen-sink` carries `all.Extension`,
  so identity-inclusion reports `Console ⊄ KitchenSink`; incomparable profiles are refused, but as
  a question to the authorizer, so a custom policy can opt in; (3) permit-by-default, enforced only
  when an authorizer is installed, and a contained request never reaches the authorizer. **Trap:**
  a deny-all sweep over gated primitives stays green while the widening is live; the gate needs a
  test asserting an *ungated* primitive becomes reachable. `TestProfileFactoryRefusesUngatedExtensionReach`
  has a permit arm proving `make-thread` is reachable from Console asking for kitchen-sink;
  `TestProfileEnvironmentCannotWidenSandboxedEngine` goes end-to-end. `docs/security/sandboxing.md`'s
  "does NOT cover" row stays for the no-authorizer case.

#### `WithNamespace` silently discards every namespace-consumed option, including `WithSandbox` (2026-08-04)

- [x] **Namespace-consumed options are rejected at compile time** [High, Done 2026-08-24]:
  `NewEngine(WithNamespace(ns), WithSandbox())` returned a working engine with no sandbox and no
  error, because the authorizer is namespace state installed only inside `bootstrapNamespace`.
  Three mechanisms in three weeks: an error return for `WithDialect` alone; a runtime panic for
  the whole family (`rejectNamespaceConsumedOptions`, never released); then the type split that
  deleted `WithNamespace` for `NewEngineWithNamespace` (`7c47889f` typed options, `1a36e8b4`,
  `a71c973b`, `46648aa0` ratchets; `memory/2026-08-24-typed-engine-options-{design,impl}.md`).
  Docs: `docs/embedding/api-design.md`.
  **Decisions (maintainer).** A misplaced option is a programmer error (2026-08-14), decided for
  the **family**, not one member, which would leave the security-relevant one silent. The family is
  every option that writes namespace-scoped config, so it includes `WithRegistry`, `WithoutCore`,
  `WithExtension(s)` and `WithProfile` (a profile's authorizer has no other writer, so refusing
  only authorizer-carrying profiles would refuse `Console` and wave `Tiny` through). The config
  had to record requests that write nothing: `profileSet` (`WithProfile(Tiny)`), `envMapSet`
  (`WithEnvMap(nil)` opens the sandbox), `topLevelMutabilitySet` (`immutableTopLevel` defaults
  true). Ratchet: `TestEngineConfigFieldsAreClassified` reflects over `engineConfig`.
  Blanket rejection breaks no documented caller (measured): `NewNamespace`'s doc tolerates
  *engine* options only and its example is the separated pattern, no in-tree caller shares an
  option slice, and `WithDialect` already rejected. Accepted cost: a namespace option passed to
  the engine constructor fails even when the namespace already carries it.
  **Rejected arms.** Apply post-hoc: a second engine would mutate a namespace others share.
  Reject only when the option would change something: it cannot be written as "the authorizers
  differ". `AuthorizerFunc` compared with itself **panics** (identical uncomparable dynamic types);
  two structurally identical `security.All` composites compare **false silently**
  (`*compositeAuthorizer` is a pointer), and `All` with one argument returns it unwrapped, back on
  the panicking arm. Nil-ness is the only usable signal.
  **`WithContractEnforcement` straddled the line** (Answer A, the namespace owns it): the base
  environment was not enforced on the `WithNamespace` path while libraries and later registrations
  were. Rejecting it alone would only move the partiality: enforcement is baked into
  `*ForeignClosure` values in namespace-owned frames, so `environment.Namespace` gained
  `ContractEnforcement()`/`SetContractEnforcement()`, `applyOptionsFromConfig` became
  `applyOptionsFromNamespace(ns)`, and `Engine.contractEnforcement` was deleted. `RegisterFunc`
  was never a reader (no `ParamTypes`, so `BuildValidator` returns nil). Pinned by
  `TestContractEnforcement_PreBuiltNamespace`.

#### Phase hermeticity does not hold inside a library body (2026-08-04)

- [x] **A library environment owns a seal, and the library-scope arm is phase-relative** [High, M,
  Done 2026-08-05, `feat/library-phase-isolation`: `cadf219f` downward arm, `01fdf6b1` sealed axis
  onto `PhaseRegistry`, `95cdb2b6` phase-relative arm, `5d54b0ab` Q2 ratchet;
  `memory/2026-08-04-library-phase-isolation-{design,impl}.md`]: a `begin-for-syntax` reference to
  a phase-0 define was rejected at top level and silently read `#!void` in a library body.
  **Cause: one arm, not the parent edge.** `CompileSymbol`'s library-scope arm resolved through
  `GetGlobalIndexFromLibraryScopes` → `GetGlobalIndexAcrossPhases`, searching phases {0, 1, 2}
  regardless of the referring code's phase; top-level identifiers carry no library scope, so the
  arm never fired there. The filed `phaseParent` attribution is **refuted**: cutting that edge to
  nil closes neither leak. The arm's phase-0 reach was both the upward leak and phase-1 library
  code's only route to primitives, so it could not be made phase-relative alone; `NewChildRuntime`
  built one flat frame holding sealed base and mutable runtime.
  **What shipped (option A1).** The sealed axis moved off `Namespace` onto `PhaseRegistry`;
  `newSealedAxisFrames` is the one builder for every owner, and **an owner does not pick a subset
  of the axis** (else "is this (phase, kind) sealed?" needs "for whom?"). `ownsSealedAxis` replaces
  `IsNamespaceRuntime` as the sealed-routing discriminator; `IsNamespaceRuntime` survives for
  `pkg/wile/engine.go`'s doc-registration guard, where widening it would re-run `ApplyDocs` on every
  import (a documented race under concurrent SRFI-18 imports). `mustSeal`'s read-time guard became
  a construction-time panic in `newPhaseRegistry`. No new call site: `LoadBootstrapCore` already
  filled seals through `SealedWriteViewAt`. Q1 measured at zero: no `pkg/stdlib/lib/` file uses
  `begin-for-syntax`, `define-for-syntax`, or `eval-when`. Cost: +52 allocs/op on
  `BenchmarkEngineStartupWithImport` (~230k), import path +1.37% sec/op, engine construction
  unchanged; a double registry apply would have been ~150 000 allocations.
  Before the fix, `docs/compiler/macro-system.md` Q4's argument that no `ErrCrossPhaseMutation`
  check is needed ("the hermetic rejection *is* the loud failure") did not hold in a library body.
  **Q2 closed (Phase D):** `findLibraryBinding` and `GetGlobalIndexAcrossPhases` derive their probe
  set from `EnvironmentFrame.PresentPhases()`, ascending, so ties still prefer runtime
  (`TestFindLibraryBindingPrefersRuntimeOverExpand`) and a phase-3 define is exportable
  (`TestLibraryExportsPhaseThreeBinding`); `PhaseTemplate` (-1) is excluded deliberately.
  Residual: Q3, a predeclared-but-unwritten slot reads as `#!void` rather than raising; the
  `predeclareBinding` twin-slot entry stays open, and closing this did not require it, which shows
  the two are separable, not that they are different bugs. `docs/environment/system.md`
  invariant 6, `docs/environment/diagram.md`, and `docs/compiler/macro-system.md` now attribute the
  divergence to the arm rather than `phaseParent`.

#### The `SealKind` dimension is inert — every production answer is fixed by phase alone (2026-08-05)

- [x] **Delete the kind axis, or give it a consumer** [Medium, S, Done 2026-08-05,
  `feat/flat-binding-model` design Phase B]: deleted. `SealKind`, its constants, `sealKindSet`,
  `sealsValue`/`sealsHandler`/`has()` and `sealedAxis.kinds` are gone; `SealedTargetAt` became
  `SealedWriteViewAt` and lost its kind parameter in the Phase C store fold. Measured before
  deleting: `sealedAt(phase, kind)` checked the row's kinds and then returned `sealAt(phase)`, so
  none of the six production requests was answered differently by kind, and the one cell where
  kind could discriminate, (1, value), had only test callers (`registry.Apply` reaches phase-1
  primitives through `env.Expand()`). Deletion was free only because the leak entry below
  resolved as option 3 (authority in the binding); option 1 (authority in the frame) would have
  made the kind load-bearing. **Do not revive the axis by adding a (1, value) caller to justify
  it**: the question is whether anything needs the distinction, and the measured answer was no.
  Under the flat model the kind's routing role is gone by construction and its meaning role
  lives on the binding (`BindingType`).

#### Syntax compilers are readable as runtime values (2026-08-05)

- [x] **Decide how a phase-0 handler is kept off the value-resolution path** [High, M, Done
  2026-08-05, `feat/flat-binding-model` design Phase A, fork resolved as option 3]: all 20 syntax
  compilers (`syntaxCompilerEntries`) resolved as ordinary top-level variables, so
  `(display define-syntax)` printed `#<syntax-compiler:define-syntax>`, an R7RS §4.1.1/§4.3.1
  divergence. Now `emitCachedBindingLoad`
  (`pkg/machine/compilation/compile_time_continuation.go`), the chokepoint every resolved-global
  arm of `CompileSymbol` funnels through, refuses any `BindingType() != BindingTypeVariable` with
  `werr.ErrSyntacticKeywordAsVariable`. Since 2026-09-01 the ambient keyword names (`if`,
  `lambda`, …) hit the same arm. Pointers: `0935b349` (the `SealKindHandler` doc claim corrected),
  `c8080848` (`LookupSyntaxCompiler` deleted).

  Why option 3, and what the fork established:
  - **The premise that costed option 3 as the widest was false.** Go primitive procedures are
    `BindingTypeVariable`; `BindingTypePrimitive` has exactly two production creation sites, both
    compile-time handlers (`RegisterPhaseBindings`, `registerCompileTimeBinding`). The tag
    already meant "compile-time handler", so option 3 needed no new enum case and no
    exhaustive-switch audit. The defect was three competing encodings with none authoritative:
    the value's Go type (`compileTimeHandler`) on the compile path, the binding tag on the
    expander path (`lookupMacroBinding`'s pin arm), and frame topology in between.
  - **Rejected: option 1, authority in the FRAME** (separate phase-0 value and handler cells). A
    second phase-0 frame per owner, library envs included; no surveyed implementation records
    meaning there, since frames answer where a value lives, not what a name means.
  - **Rejected: option 2, authority in the VALUE** (Chibi-shaped: no phases, `sexp_macrop` on the
    value). Same edit site as option 3, but keeps a Go type assertion as the load-bearing tag and
    leaves the compile and expander paths disagreeing about where the answer lives.
  - **The check must REFUSE, not fall through.** `tryResolvedBinding` may return false because it
    is a fallback; at `emitCachedBindingLoad` the ordinary path is the one already resolving to
    the handler, so falling through re-emits the leak.
  - **A tag is not a structural impossibility, on option 2 or 3.** In Wile the handler IS the
    value in the cached cell, so the tag is a check every reaching site must remember. Racket's
    leak is impossible by construction because its table holds descriptors and transformer
    values live in a separate compile-time store. Do not claim option 3 makes the leak
    unreachable.
  - **Option 3 is a field change, not Racket's architecture.** What makes Racket flat is one
    uniform resolution rule (same symbol, reference scopes ⊇ binding scopes) over a stratified
    store (`racket/src/expander/syntax/binding.rkt`, read 2026-08-05).

  **The flat-model endpoint shipped on the same branch** (`memory/2026-08-05-flat-binding-model-design.md`)
  in four phases: A (this fix), B (`SealKind` deletion, above), C (store fold: one scope-keyed
  `GlobalEnvironmentFrame` per owner, ranked tiers replacing the frame graph;
  `pkg/environment/CLAUDE.local.md` "One Store Per Owner, Slot Coordinates"), D (the hard-wired
  `{0,1,2}` ceiling on `GetGlobalIndexAcrossPhases` and `findLibraryBinding` lifted via
  `EnvironmentFrame.PresentPhases()`). What the code does not record:
  - **Conservation law:** deleted topology reappears as resolution rules (a key coordinate, a
    per-binding attribute, or a rank); it does not disappear. Before the fold Wile phased the
    ADDRESS (phase chose the store); Racket phases the query and the answer.
  - **Phase is an explicit key coordinate, not phase-as-scope.** Racket's multi-scope encoding buys
    a phase-free lookup rule at the price of deep `pkg/syntax` surgery, for generality Wile does
    not use. The phase wildcard is an explicit named value, never nil.
  - **Irreducible, exactly two:** locals (activation records compiled to (slot, depth)) and owner
    identity. No binding resolution crosses an owner boundary; every cross-owner flow is an
    explicit copy (registry apply, import, snapshot), so per-owner stores add no resolution
    structure.
  - **Floor:** ambience makes lookup a ranked probe (exact phase, then ANY). A single exact-match
    probe would need per-phase instantiation of the ambient set, Racket's import-per-phase model;
    Wile's ambience (primitives visible at a tower-climbed phase-47 frame) is deliberate.
  - **Sealing is per-binding, not topological** (`a8087e5d`). Three states: frozen (`Stable`),
    supersedable but not settable (`Imported`), free. `Stable` has two soundness consumers, the
    `OpSelfTailCall` emit gate and `compile_call_arity.go`'s compile-time arity refusal, whose
    output is the absence of a program, so no runtime check can rescue a falsified proof; any
    redesign must preserve both. Since 2026-08-09 an import installs at the sealed exact-phase-0
    tier and a later define shadows it instead of superseding in place, and `DefineOwnGlobal`
    refuses rebinding a `Stable` binding at matching coordinates, closing `Engine.Define`,
    `Engine.RegisterPrimitive` and `namespace-define!` together (host-owned names stay
    rebindable). The guarantee is language-level integrity for compiled pins and hermetic
    viewers, not memory protection; sandboxing stays in the Authorizer/profile layer.
  - **Kind does not come along with phase:** a phase key does not stop a phase-0 variable query
    from matching a phase-0 handler, so the handler fork had to be settled under any store shape.
  - **Crosscheck round, 2026-08-06:** `GlobalIndex` carried `(name, query, Slot)` but not
    `(phase, sealed)`, so both stale-pin heals re-resolved by name across phases. It reproduced
    on the shipped immutable default, not only under `WithMutableTopLevel` as the
    accepted-residual note had claimed. The pin now records its `slotRef` coordinates,
    `healWriteLocked` re-resolves at exactly those, `healReadLocked` re-runs the ranked probe at
    the pin's phase, and `bestSlotLocked` is gone. Interleaved A/B: runtime allocations
    byte-identical, `sec/op` within the ±1% same-binary drift band.

  Residual (a separate decision, not taken): library envs joining the engine's flat store by
  scope, so imports ALIAS instead of copy, would delete the primitive-islands cost (~7 ms/1.7 MB
  per library env, `eq?` broken across imports, the reason `PrimitiveIdentity` exists). It
  changes import semantics, and its concrete blocker is `registerPhasePrimitive`
  (`registry/apply.go`) capturing each owner's mutable runtime as the closure env: Wile has
  re-instantiation, not `namespace-attach-module`.

#### Should Wile's phase 1 start EMPTY, like Racket's? DECLINED (2026-09-10)

- [x] **DECLINED as a product decision, 2026-09-10** (Stage B impl fork E; design §9 Q1).
  Recorded with its numbers because the cheap version, "just delete the vocabulary row", is wrong
  in a way that looks right. Racket's phase 1 holds 0 names; Wile's holds 226 reachable / 215
  exact slots on a default engine and 239 / 217 on KitchenSink (re-measured at `5d69a2fd` by
  enumerating `UnscopedKeysAt(PhaseExpand)` and asking `ExactBindingAt` per name). Four population
  mechanisms, partitioned by binding type so the sum closes:

  | | mechanism | default | KitchenSink | binding type |
  |---|---|---|---|---|
  | 1 | `pkg/registry/apply.go`'s `phaseTargets`: primitives with `Phases.Has(PhaseExpand)`, plain procedures (`car` yes, `cadr` no) | 155 | 155 | `Variable` |
  | 2 | `primitiveExpanderEntries` through `SealedWriteViewAt(PhaseExpand)` | 40 | 40 | `Primitive` |
  | 4 | `define-syntax` evaluated during bootstrap or extension load (`pkg/registry/core/bootstrap_macros{,_late}.scm`, `extensions/files/with_file_macros.scm`) | 20 | 22 | `Syntax` |
  | 3 | `pkg/wile/engine.go`'s `InstallMacroPhaseRow` vocabulary row | 11 | 22 | row-only, no slot |

  - **Mechanism 4 is open-ended.** `(*PrimitiveRegistry).AddMacroSource` is exported, so any
    third-party extension gets a phase-1 write with no table a deletion step could find. Deleting
    only the vocabulary row and the expander registration leaves 177 slots (155 + 22). A
    re-opener must start from mechanisms 1 and 4.
  - Trap: mechanism 2 was first counted as 38, because two rows name their form through a
    constant (`TransformerSyntaxRules`, `TransformerERMacro`) that a string-literal grep misses.
  - **For zero:** it would make the phase-shifted-import collision impossible rather than masked,
    retiring the two reader gates, and it is the endpoint of `183171a1`'s direction ("the dialect
    declares what each phase sees").
  - **Against, decisive: zero makes the import-identity defect load-bearing.** The phase-1
    expanders ARE mechanism 2; with an empty phase 1 they must arrive through
    `(import (for-syntax …))`, and an import today supplies a different object for a re-exported
    name. Racket's zero requires the identity fix first, not instead of it. It also moves R7RS
    conformance results.
  - **Sequence for whoever re-opens it:** identity fix across the import edge →
    `findLibraryBinding` takes the requesting phase (entry above, decided (a)), making
    `syntax-rules` importable → the phase-1 vocabulary becomes declarable → only then is zero
    reachable.

#### A library DECLARATION's `(import (for-syntax …))` silently drops the phase shift (2026-09-09)

- [x] **Compose the import phase shift in library declaration position** [High, S, Done
  2026-09-09, branch `fix/phase1-import-neutrality`]: `(import (for-syntax X))`, `for-template`
  and `for-meta n` in `define-library` DECLARATION position were parsed, then installed at phase 0
  as if unmodified, with no diagnostic (a library whose `probe` called a for-syntax-imported
  `my-helper` printed `101`); the same import inside the body, or at top level, composed
  correctly. **Polarity trap:** first filed as "a library BODY drops the shift", which is
  backwards; `pkg/stdlib/lib/wile/er-macro-test.scm` worked only because its for-syntax imports sit
  in the included body. Fix: `processLibraryImport` (`pkg/machine/compilation/compile_import.go`)
  composes `composePhaseShift("import", lib.Env.PhaseLevel(), res.ImportSet.PhaseShift)` and
  passes it to `copyLibraryBindingsDirect`'s new `targetPhase`; syntax lands at a phase composed
  relative to it, so a `for-meta` near the ceiling hits the same int8 refusal instead of wrapping.
  Shift 0 is bit-for-bit unchanged.
  - **Rejected: route the declaration path through `ResolveAndInstallImportSet`.** The filed cost
    ("`lib.Env` vs `p.env`") is vacuous; they are one pointer. The real cost is the placement
    tier: `CopyLibraryBindingsToEnvAtPhase` passes `placementShadowable`, which gives a library
    env a sealed phase-0 answer, and
    `TestBindingModelMatrix/imported_rename_shadows_set!_special_form` pins that a library env is
    deliberately a flat island with no sealed tier.
  - `copyLibraryBindingsDirect`'s doc comment was rewritten because its stated reason was false:
    `lib.Env.AtPhase(n)` is the library's own phase-n frame, not the parent's. Leaving it invited
    the rejected route.
  - Pin: `TestLibraryDeclarationImportComposesPhaseShift` (declaration rows RED before; the
    body-position twins are guards, so both positions now give the same answers). Blast radius
    measured empty: no declaration-position shifted import existed in the tree.

#### `(import (scheme base))` damages the engine: Stage A regression (2026-09-09)

- [x] **A plain phase-0 import reused the base's own slot and restamped it `Imported`**
  [**High**, M, Done 2026-09-10, `91bbe8a1`, branch `fix/import-reuses-base-slot`; filed
  2026-09-09 from `memory/2026-09-09-flatt-binding-model-b-design` §1 = slice S0]: a Stage A
  regression bisected to `183171a1` ("delete the ambient tier"). `(import (scheme base))`
  stripped the names it covered out of the phase-1 macro vocabulary (`not` unbound in
  `begin-for-syntax`) and made every covered base primitive deletable by
  `namespace-undefine!`. Cause: `installImportedBinding` wrote `(ExactPhase(0), sealed)` with
  empty scopes onto the base's `nil`-scoped slot, `scopeSetsEqual(nil, [])` is true, so
  `CreateGlobalBindingAt` reused it and `markBindingImported` stamped the startup set's own
  binding. Fix: a third tier, `environment.tierExactImported`, between the mutable tier and
  the startup set's, and an import takes its OWN slot there via
  `CreateImportedGlobalBindingAt`. The old doc's "there is no third coordinate" was the premise
  that failed. Three things the fix had to get right, each found by a red test:
  (1) **the reuse refusal is SEALED-TIER ONLY**: at the mutable tier reuse IS the R7RS §5.3.1
  supersede rule (`define-syntax` over an imported macro, last import wins), and a blanket
  refusal keeps the FIRST at equal tier (gates `TestDefineSyntaxSupersedesImportClearsImported`,
  `TestImportedMacroDocTracksTheWinningValue`); (2) the slot is stamped `Imported` BEFORE
  publication, since `tierOf` ranks on it; (3) `namespace-undefine!` asks the TIER
  (`IsImportedBindingAt`), not `SealedBindingAt(...).IsImported()`, preserving the documented
  `(namespace-undefine! (environment '(scheme base)) 'car)` capability
  (`docs/environment/system.md`). **Two store questions that used to share one probe:**
  `IsImportedBindingAt` (ranked, "does this name DENOTE an import here") and `ImportedBindingAt`
  (floored, "what did an import bind here regardless of a user shadow"); after
  `(define list-copy 7)` the first is false and the second is not, and only the second tells
  shadow from assign-through. **Trap:** `TestImportedBindingTakesTheSealedPhaseZeroTier` passed
  THROUGH the defect (it could not tell "slot above the base" from "stamped the base"); it is
  rewritten to assert the import wins the tier AND the startup set survives underneath.
  `storeBulkSource.ownInstallsOnly` is now redundant with `minTier = tierExactSealed` and kept as
  a second independent reason. Structural pin: `TestImportDoesNotMutateTheBaseBinding` (holds
  the base's binding OBJECT across the import, so it is independent of the fix's shape).

#### `namespace-undefine!` over an import destroyed the base (2026-09-10)

Introduced by the S0 repair above, and invisible from Scheme. Filed and fixed in the same
pass because it was found while planning Stage B, not by a gate.

- [x] **Deleting an import deleted the STARTUP SET's slot and left the import standing**
  [**High**, M, Done 2026-09-10, `6829f3be` on `feat/flatt-binding-model-b`]: S0 fixed the
  CREATE path; the DELETE path still thought `(phase, sealed, scopes)` named one slot.
  `namespace-undefine!` asked the tier question, then deleted through `DeleteBindingAt`, which
  resolves by COORDINATE and returned the first slot, the base's. **Why no gate caught it:**
  `namespace-bound?` answers `#t` through the surviving import, so
  `TestImportDoesNotMakeABasePrimitiveDeletable` passed THROUGH the bug; the discriminating
  observable is a sequence, **two consecutive undefines unbind a name a single undefine on a
  fresh engine refuses** (pins red on `91bbe8a1`). Fix: `DeleteImportedBindingAt` resolves
  through `ImportedBindingAt`'s ranked probe floored and pinned at `tierExactImported`,
  sealed-tier-only as in `createGlobalBindingAt`; two named entry points sharing
  `removeSlotLocked` rather than a provenance parameter threaded through
  `resolveAtCoordsLocked`, whose `healWriteLocked` caller would then need a policy with no
  mandate. **General lesson: adding a ranking axis leaves every COORDINATE-addressed operation
  behind, and nothing types the gap.** Any future axis owes the same sweep. Siblings audited:
  `setValueAtCoords` is provenance-blind at the sealed tier and that is right for its callers
  (doc says so). Residual: `OwnGlobalIndex`'s doc does not separately address the import tier
  (its callers do not raise the question today); **not separately pinned.**

#### `GlobalEnvironmentFrame.Copy` dropped three things it claims to carry (2026-09-10)

- [x] **`(scheme-report-environment 7)` had no phase-1 macro vocabulary at all** [**High**,
  M, Done 2026-09-10, `a60f7895` on `feat/flatt-binding-model-b`; filed as Stage B design §8.2
  item 8]: a transformer body calling `(not #f)` worked under `(interaction-environment)` and
  raised "no such binding `not` … at phase 1" under `(scheme-report-environment 7)`; the
  programs differ only in the environment, which pins it on `Copy`. Three faults: (1) the
  re-point ran through the UNRESTRICTED source constructor, so a source meaning "the base"
  supplied the copy's phase-0 MUTABLE tier at every macro phase; (2) it type-asserted
  `*storeBulkSource`, so filtered and renamed rows were skipped and became **INERT**
  (materialization requires `store == p`, so resolution misses); (3) `macroPhaseRows`,
  `macroPhasesSeen`, `macroPhaseSeenBits` were dropped. **`BulkRowCount` was EQUAL (2 vs 2)
  across the defect**, so a count ratchet could never see it; the pins assert on ANSWERS.
  **Shape decisions worth keeping:** `Repoint` is on the `BulkSource` **interface**, so wrappers
  forward and the compiler enforces it (the package-level type switches over the three types
  produced fault 2), and it **carries** `minTier`/`ownInstallsOnly` rather than reconstructing
  them; the three macro fields travel **with** the materialized rows, because the installer
  appends unconditionally and zeroing the seen-set trades a missing row for a duplicated one.

#### Ordinal tier labels ("T2", "T3") in prose are known-dirty (2026-09-11)

- [x] **Swept the 8 sweepable binding-tier ordinal labels** [Low, M, Done 2026-09-14, filed
  2026-09-11 after Stage B S1; swept in `04b6011a`, MERGED `6ce2f4c1`; earlier corrections in
  `ff7f8534`]: inserting `tierExactImported` renumbered every tier below it, and **every "(T2)" /
  "(T3)" in prose silently changed referent with no test going red**. A label is a second,
  unchecked copy of the ordering; an identifier moves with the constant. All 8 now name the tier
  identifier; the other 10 of 18 binding-tier labels quote replaced text or state the rule
  (81 `T[123]` occurrences in `pkg/`: 18 binding-tier, 47 closure tiers, 16 architectural review;
  the same shorthand names boxing tiers and `T1.5`, so a mechanical `sed` is unsafe). **Trap:
  the entry's own "all four wrong" verdict was 2 of 4 wrong**, both false verdicts from reading
  `(phase, sealed)` and skipping the `Imported` stamp. **A coordinate is not a tier; read the
  write path.** Filed with the sweep: `pkg/wile/library_export_scope_test.go:153` calls a T3
  probe "ambient", a tier deleted in Stage A (ordinal correct). **The tripwire does not
  validate the baseline:** `TestTierOrdinalsHaveNotRenumbered` pins the enum values and goes red
  on a renumbering, but a label already wrong stays green; a site-count ratchet is the wrong
  shape, because inserting a tier does not change the population.

#### Three Stage B decisions reversed on request (2026-09-11)

Not defects. Each shipped deliberately, was argued and reviewed, and was then reversed
because the maintainer changed his mind about the trade-off. Filed so the arguments survive
the reversal and are not re-discovered as novel.

- [x] **The `BulkSource` seal is gone: `repoint` is now the exported `Repoint`** [reversed
  2026-09-11, `458828c7`]: the unexported method on an exported interface closed `BulkSource`
  to out-of-package implementations, because a fourth implementation that could not be
  re-pointed would be a row `GlobalEnvironmentFrame.Copy` **aliased in silence**: INERT, no
  panic, no count change (see the Copy section above). Reversed so embedders can implement it;
  the obligation (return a source reading the GIVEN store, carrying tier floor, rename table,
  admission predicate, phase, name) is now a voluntary contract in `Repoint`'s doc, and no
  ratchet can enforce it since the failing implementation lives outside the repository.
  **Do NOT re-seal** as a tidy-up; it is a deliberate API break with a named consumer cost.
  `Repoint` on the interface rather than `*storeBulkSource` is orthogonal and still right.

- [x] **`BulkOrigin`'s zero value is `BulkOriginUnknown`, not `BulkOriginLanguage`**
  [reversed 2026-09-11, `2a63ccfd`]: the original put `BulkOriginLanguage` at `iota`, arguing
  both installers take origin positionally so no unset origin exists, and an omitted field
  ranked `tierExactSealed` (fail-safe, and silent). Now zero means "nobody said":
  `InstallBulkRow` and `InstallMacroPhaseRow` **panic** (wrapped `werr.ErrInvalidArgument`), and
  `bulkTierOf`'s `tierNone` arm is documented as unreachable because the installers refuse.
  Pins: `TestBulkRowInstallersRefuseAnUndeclaredOrigin`, and `TestBulkOriginValuesHaveNotRenumbered`
  (the enum is exported and Stage C will serialize a row, so a renumbering is wire-visible).
  **A standing hole closed in the same pass:** `BulkOrigin(99)` from an out-of-tree caller ranked
  `tierExactSealed` via `bulkTierOf`'s `default:` arm, silently language-declared. **That hole
  predates this branch** (an earlier revision of the entry wrongly said the branch created it);
  the reversal added only the zero-value case. `BulkOrigin.valid` (`> Unknown && <
  bulkOriginCount`) closes both, and `bulkTierOf` asks the ORIGIN before `sealed`, since asking
  `sealed` first ranked an unclassified unsealed row `tierExactMutable`, the highest tier.
  `TestEveryDeclaredOriginIsClassified` sweeps the whole `uint8` domain.

- [x] **The two tests `ce0ffe88` deleted are re-implemented against the new subject**
  [`db9e9c0b`]: `TestCreateGlobalBindingAtRefusesAnyPhase` and `phase_distinctness_test.go`
  were deleted correctly (their subject, the wildcard phase coordinate, is gone). `BulkOriginUnknown`
  recreates the *condition* they pinned, so they return as
  `TestBulkRowInstallersRefuseAnUndeclaredOrigin` and `TestNoInstalledRowHasAnInvalidOrigin`
  (`bulk_origin_census_test.go`). **Not a restoration:** the original assertions cannot be
  written at all, and the file is deliberately not named `phase_distinctness_test.go` so it does
  not overclaim continuity.

#### `set!`'s two immutable-binding refusals report a stale location (2026-09-09)

- [x] **Both immutable-binding refusals now locate the identifier** [Done 2026-09-09, branch
  `fix/phase1-import-neutrality`]: all three unstamped returns in `CompileValidatedSetBang`
  (`pkg/machine/compilation/compile_validated.go`) wrap `wrapSourcedError(v.Name.SourceContext(),
  …)`, as the `ErrNoSuchBinding` arm already did; `errors.Is(err, werr.ErrImmutableBinding)` is
  unaffected (asserted in the pin). Before: the Go API reported the enclosing `set!` FORM
  (`4:1` vs the identifier's `4:6`); the CLI reported the import form on another line, because it
  wraps a file in `(begin …)`. Same defect, two distances. Pin:
  `TestSetBangRefusalLocatesTheIdentifier` asserts full `file:line:col`, with a CONTROL row (the
  already-stamped unbound arm) that passes both sides. The third raise (`internal error: binding
  found but no index`) is stamped but explicitly UNPINNED: no constructible reproducer, and the
  stamp must not read as coverage.

#### Ambiguous binding references resolve silently instead of erroring (2026-07-18)

- [x] **Fixed at the cause, not by erroring** [Medium, S, Done 2026-07-21, approach 1a per
  `plans/2026-07-20-scope-keyed-tier1-remediation.md` Item 1]: the filed fix, raise on every
  `bestOf` tie, was **rejected**: it regresses ordinary R7RS nested `let-syntax` keyword
  shadowing, which *is* an incomparable equal-cardinality tie (`(let-syntax ((m …outer…))
  (let-syntax ((m …inner…)) (m)))` weighs both binders 1 under a weight-2 reference, and
  keep-first, innermost, is correct). The fix does not belong in `bestOf` either, which takes two
  ints and cannot see scope sets, and "error on a tie" conflates shadowing with ambiguity.
  **1a:** `expander_let_syntax.go` bound each `let-syntax`/`letrec-syntax` keyword on the bare
  singleton `{letScope}`; it now binds on `slices.Clone(keywordSym.Scopes())` + `letScope` at all
  three coupled sites (letrec pre-register, compile-loop create, and the re-resolve, which must use
  the identical set or `MaybeCreateLocalBinding` keys a second slot). A nested binder is then a
  strict superset and wins by maximality, so the tie never forms. A throwaway census over
  `r7rs-tests.scm`, `macros-test.scm` and the ER-macro corpus found **zero `weight>0` ties**; every
  residual tie is `weight=0`, benign shadowing by frame order. So 1b (a real
  `ErrAmbiguousBinding` on a same-frame incomparable tie) stays deferred as the fallback. Guard:
  nested and triple-nested cases in `TestScopeResolution_LetSyntaxShadowing`.
  **Residual, filed separately**: `resolveNodeByScopes` (`frame_reclaim_build.go`) hand-rolls the
  same argmax over a Go *map*, so its tie is non-deterministic rather than first-wins; unreachable
  in the corpus, a determinism follow-up not gated on this item.

#### Two fail-open guards in the frame-reclaim analyser were unpinned (2026-09-10)

- [x] **Two correct-but-unwatched guards in `pkg/internal/validate/frame_reclaim_build.go` now
  have witnesses** [Medium, S, Done 2026-09-10, `57353e22`]: found auditing for Stage B
  (`2026-09-09-flatt-binding-model-b-design.md` §8.2 item 1); neither was a live bug. This
  classifier's false positive is a use-after-release, so an unwatched widening is the expensive kind.
  (1) Deleting `resolveNodeByScopes`' `ScopesMatch` subset guard left the whole tree green; non-subset
  usually means strict SUPERSET, so an ungated argmax *prefers* the wrong node rather than merely
  admitting it. Pin: `TestResolveNodeByScopes_NonSubsetNodeIsNotACandidate`. (2) Hard-wiring
  `collided: false` also stayed green, and is fail-open: a same-name/same-scope redefinition flips the
  survivor to reclaimable while define #1's closure is live. Two traps: the test named
  `…_TwiceDefinedNotReclaimable` had a single-define unit, so it never saw `dup` (renamed
  `…_NonStableNameNotReclaimable`); and `collided`'s doc claimed `StableInUnit` covered the case,
  which is false (`nodeSafe` never reads `rebindStable`; only `classifyCallee` does, for CALLERS), so
  deleting `collided` on that sentence would have shipped the soundness bug. Pin:
  `TestBuildReclaimGraph_CollidedIsScopeKeyed`, deliberately scope-CONTENT-specific: a uniform suffix
  on every `ScopeKey` preserves the partition and is invisible to "duplicates are still caught", so
  expected keys come from `syntax.ScopeFingerprint`, not the producer under test.

#### Scope-keyed globals — successor work not built in the arc (2026-07-19)

Stage C adversarial-review successors, left out so the arc could land. All four closed.

- [x] **Internal `define-syntax` was invisible in a shorthand-`define` body** [Correctness, S, Done
  2026-07-21]: broader than filed, any internal `define-syntax` in `(define (f) …)` was invisible,
  because `expandDefineForm`'s shorthand branch used the flat `ExpandSyntaxArgumentList` pass (no
  letrec* pre-scan, no body scope). Routed through the extracted `expandProcedureBody`, form preserved
  rather than desugared so self-tail and frame-reclaim still apply. Pin:
  `TestInternalDefineSyntax_InShorthandDefineBody`.
- [x] **A library-local `define-syntax` did not shadow a same-named imported macro** [Correctness, M,
  Done 2026-07-21]: broader than filed, the import won in the library's own body too, because
  `copyLibraryBindingsDirect` also mirrored syntax bindings into the runtime frame at `{}`. Syntax
  bindings now install into Expand only, on both install paths (the second, top-level
  import-then-redefine, found by `/crosscheck`). Pin: `TestLibraryLocalMacroShadowsImportedMacro`.
  **Residual:** syntax and variables now land in different frames, so a cross-kind same-name clash
  across two imported libraries (macro `foo` vs variable `foo`) is no longer conflict-detected per
  R7RS §5.6; same-kind clashes still are.
- [x] **No sealed base above phase 0** [Correctness, M, Done 2026-07-22, PR #814, branch
  `feat/free-template-id-hygiene`]: the filed library-import vector did not reproduce; a top-level
  `(define-syntax guard-aux …)` compromising `guard` did. Fixed by the D0–D3 arc: a per-namespace
  `sealedExpandBase` holds bootstrap macros and form expanders; D3 was retargeted there after the
  phase-0 value frame regressed `Dialect.Forms().Remove()`, settling that the phase-0 frame is not
  collision-free for compile-time handlers. `/crosscheck` caught that the nil-pin census was vacuous
  (composite `FreeIdKey` vs a bare-name lookup) and that a recursive helper's self-reference stayed
  capturable, fixed by `pinTemplateSelfReferences` back-patching after `define-syntax` creates the
  binding (preserving create-after-compile). The census is a mutation-verified ratchet (44 → 23
  nil-pins), and an unpinned expand-bound sibling reference is a defect. **Successor (2026-08-03):**
  the seal was one level deep (`TestSealedClimbStopsAboveExpand`); see "Sealed axis keyed by
  `(phase, kind)`" in Tier 5.
- [x] **CHANGELOG documents scope-keyed global storage** [Docs, S, Done 2026-07-21]: the
  `8afeb66a…4f73936d` arc's user-visible semantics (twice-expanded macro-generating macros get two
  binders, template-introduced library exports rejected eagerly, `namespace-undefine!` deletes one
  scope-matched slot).

#### Resolved Tier-1 defects, 2026-07-14 → 2026-07-21

- [x] **Opaque-subtree over-marking loosened the immutable-top-level check** (2026-07-16,
  `57973333` + `3cce6754`): `forEachRawSymbol` marked template data no unquote can reach, dropping
  the `Stable` stamp and with it top-level immutability for any name a template mentioned. It now
  threads quasiquote depth and marks only evaluated positions, matching
  `quasiquoteNeedsRuntime`/`expandQuasi`; agreement with that walk is the soundness argument. The
  filed premise ("two consumers want opposite error directions") was **false**: both enforcement
  sites key on the same `Stable` flag, so a second map buys nothing. The shape that fails silently:
  `quote` is a barrier at depth 0 **only** (nested unquotes stay live, R7RS §4.2.6); a dotted
  unquote in the spine is the other shape keyword dispatch cannot see. Guard:
  `TestImmutableTopLevel_OpaqueSubtreeOverMark`.
- [x] **`Value` Go-comparability is a stated, enforced contract** (2026-07-14,
  `fix/value-comparability-contract`): the deciding rule, **the receiver, not the underlying type**,
  is on the `Value` doc comment and enforced by `reflect.TypeOf(v).Comparable()` over rosters (Go
  cannot assert comparability at compile time). Three violators, not the two audited:
  `machine.boxedValuesType` (Scheme-reachable via `dynamic-wind`) became pointer-shaped; `Operations`
  and `MultipleValues` are no longer `values.Value`. Also fixed a host crash: `equalWorklist.step`
  compared `a == b` before establishing both sides were `DeepEqualer`s. A `SchemeComparable`
  interface was **rejected**: it gives offenders a supported way to be non-comparable, and identity
  may not be delegated to a method (R7RS §6.1 `eq?` denotes a location; a type computing its own
  identity can break `eq? ⊆ eqv? ⊆ equal?`).
- [x] **`eqv?`/`equal?` numeric-lattice nonconformances (F1/F2/F3)** (2026-07-14, `c302b702`):
  `EqvNumber` (`pkg/values/eqv.go`) is the single authority the three sites consume. Exact numbers
  compare across representations (F1), exactness contagion is corrected (F2), and NaN is reflexive
  (F3, matching Chez; deliberately finer than the literal pool's `literalIdentical`). F4/F5
  documented as conformant divergences.
- [x] **Macro-introduced top-level binders; `define-values` under NoMutation** (2026-07-14,
  `d594beeb`; mechanism superseded 2026-07-18 by `a60e32e1`, scope-keyed global storage replacing the
  rename pass): `define-values` is `set!`-free with a template temporary, so it works under the
  immutable top level and NoMutation (a definition, R7RS §5.3.3). Guard:
  `TestNoMutationKeepsDefineValues`.
- [x] **General form-removal `*PrimitiveExpander` leak** (2026-07-14,
  `fix/form-removal-expander-leak`): `fr.Remove` drops only the `FormSpec`, so a user macro whose
  template referenced a removed form got that form's expander pinned at runtime
  (`#<primitive-expander:set!>` under NoMutation). A `compileTimeHandler` marker on
  `namedHandlerBase` makes `tryResolvedBinding` fall through to `ErrNoSuchBinding` for any removed
  form. Guard: `TestNoMutationRemovedFormInMacroTemplateIsUnbound`.
- [x] **R7RS library export supersets + the `(description)` declaration** (2026-07-14, `cc3c48bb`):
  documented as deliberate deviations, not deleted, since removing exports breaks users.
  `TestLibraryExportSupersets` imports each binding through `(only (scheme …) id)`, so narrowing a
  library forces a deliberate doc update.

- [x] **`GlobalIndex` literal identity must include `Env`** (2026-07-14, `fa9804d6`): a literal-pool
  collision — two distinct globals with the same `Index` symbol but different `Env` deduped to one
  slot. `EqualTo` now compares `Env`.
- [x] **2026-07-17 full-review remediation, all 14 confirmed defects** (2026-07-21;
  `reviews/2026-07-17/REVIEW.md`, `memory/2026-07-17-review-remediation.md`): twelve in PRs
  #808–#813, two via `1af62cd2`. Covered: SRFI-18 condition-variable lost wakeup, symlink-following
  resolver, ungated `(command-line)`, uncatchable port-type panic, case-lambda params unbound in
  body, `unless` referential transparency, `%parameter-raw-set!` under NoMutation, write/write-shared
  skipping hashtable interiors, `(())` error location, RunSimple dropping a final line, the `\x…;`
  digit cap, two broken example/benchmark paths. **The CI gap that hid two is closed**: `make
  test-examples` is a CI stage (CI had never run the shipped `.scm` tree). Triage refuted #5
  (`-specific-set!` under NoMutation, a documented nit) and #10 (SRFI-13 comparisons are boolean, so
  Wile was conformant; the mistaken doc entry was removed). Copilot found two escapes in the fixes:
  a create-time parent-symlink escape in #809 (`10cdbf56`) and a `ReadLine` `n>0`+EOF byte drop in
  #810 (`2c72c925`).

#### Continuation multiple-values follow-ups (from PR #800 crosscheck, 2026-06-25)

- [x] **`dynamic-wind` multiple values; continuation arity** [Done 2026-06-25]: `dynamic-wind`
  preserves 0/1/N thunk values in one eval-stack slot (`OperationBoxValues`/`OperationUnboxValues`
  in `CompileValidatedDynamicWind`); `procedure-arity` reports `*ComposableContinuation` and
  `*CapturedContinuation` as `(0 . #f)`. Docs: `docs/reference/r7rs-differences.md` → "Value-Count
  at a Single-Value Slot".
- [x] **Single-value resumption contexts raise rather than splice** [Done 2026-08-13,
  `fix/w1-s4-single-value-delivery` (`48a52a3c`)]: delivering ≠1 values into a single-value slot
  raises for `(values …)` and continuation invocation alike, matching Chez and Racket. This
  **reverses** the 2026-06-25 "not pursued" decision, and all four of its objections re-measured
  false: the check sits at the delivery instruction (`OpPush` → `pushSingleValueRegisterTo`), not on
  `RestoreContinuation`; no compile-time single/any classification is needed (which also retires
  §4.3's predicate fork); `(wile control)` variadic resumption still works
  (`(call-with-values (lambda () (reset (shift k (k 1 2)))) list)` is `(1 2)` at `77f23e03` and after), and `call/ec`
  already raised, so `call/cc` splicing was the inconsistency; the Go suite and 63-file Scheme
  corpus stay green. `WithStrictValueArity` not needed, not built. Superseded rationale:
  `memory/2026-06-25-continuation-arity-strictness-design.md`. **Trap:** no test covered this, so
  the suite stayed green through a documented-behaviour reversal;
  `pkg/wile/single_value_delivery_gate_test.go` now pins it.

#### Trampoline continuation invocation to bound Go-stack growth

- [x] **Unified reification + winding-aware resume ("the flip")** [Performance/Correctness, L, Done
  2026-06-28 on `feat/continuation-resume-trampoline`]: resumed continuations ran in a nested
  sub-context and accumulated Go frames (`ctak(18,12,6)` ~40k, Gabriel loop ~525k, against Go's
  ~675k overflow); `ErrResumeContinuation` now resumes on the driver
  (`RunResumable`/`ReinstallSegment`) in O(1) Go frames. `maxContinuationDepth`/`contNestDepth`
  retired, and the `-race` ctak skip with them. A post-landing crosscheck fixed a sticky
  `isolatedMarks` swallowing R7RS §6.11 secondary exceptions (`resumeGeneration` counter). **Why four
  attempts failed is the durable part:** a resume-side-only trampoline is *provably impossible*
  (reinstall-at-nearest breaks escape-past, double-running a `call-with-values` consumer;
  abort-to-top breaks guard); reification is **atomic** across all six boundaries (partial
  reification regresses nested guard, which runs off the chain `FindPrompt` walks); and reification
  ⟺ winding-aware resume is itself one atomic change. `go test ./...` does **not** run
  `control-test.scm`/`exceptions-test.scm`, so `make ci` is the gate. The suite was blind to
  escape-past until `pkg/registry/core/continuation_escape_past_oracle_test.go`, proven non-blind
  against the falsified attempt. `memory/2026-06-2*` (resume-aware-prompt-catches, coupled-fix,
  cluster-reification-impl).

#### Restricted-profile `(scheme base)` export-validation

- [x] **Not a supported combination; the strict eager validation stays** [Done 2026-06-29, #801
  closed by design]: a profile that does not register base's primitives makes `(scheme base)`
  invalid there, even under `(only …)`. That is R7RS §5.6 enforcement and the capability boundary
  asserting itself: `Tiny` chooses what is exposed, orthogonal to the language standard. Tolerating
  it would invert the `machine/compilation`→`registry/` layering or put deny-stubs in every
  namespace, for names (`car`, `cons`) already bound without an import. The diagnostic names both
  causes (`43d7d085`); contract in `docs/embedding/source-loading.md`. `WithStrictNamespace()` does
  not resolve #801, where the primitives are registered nowhere. **Residual:** "start as R5RS or
  R6RS" is a language-standard selector, not a security profile; `(scheme r5rs)` only layers names
  over R7RS, and a first-class `WithDialect` startup point is designed but unstarted under the
  Dialect System in `plans/ARCHITECTURE.md`.

#### `record-modifier` escapes the `NoMutation` dialect (2026-07-25 review)

- [x] **`NoMutation` does not remove `record-modifier`** [Correctness, S, Done 2026-08-09]:
  `mutationPrimitives()` (`pkg/wile/dialect_nomutation.go`) omitted it, and the bang-keyed drift
  guard `TestNoMutationRemovesEveryDestructivePrimitive` could not see a bangless destructive
  primitive. CONFIRMED at `reviews/2026-07-25/VERDICTS.md:180`; filed only when `make indexlint`
  found the unopened row. Fix: `registry.PrimitiveSpec.Mutates`, on the `InvokesProcedure`
  precedent, with 21 specs annotated. The guard's arm A iterates every registered primitive (455;
  35 bang-suffixed, measured 2026-08-09, not the review's 452/42) and reads the annotation; arm B
  requires a reason in `nonDestructiveBangs` for an unannotated bang.
  `TestMutatesMatchesMutationPrimitives` pins the annotation against `RemovedPrimitives()` both ways.
  The list stays a literal because `RemovedPrimitives()` cannot see the registry. The immutability
  plan's Phase 5 ("gate on a record flag") is deleted as unbuildable. **User-visible consequence:**
  `define-record-type` expands to `(record-modifier …)` unconditionally, so under `NoMutation` a
  record type that declares a modifier fails at **definition** time with an unbound identifier;
  modifier-free types are unaffected.

#### `error-object-message` on a compile-originated condition is the whole Go chain (2026-08-15)

- [x] **A caught compile failure's `error-object-message` is the entire wrap chain, not the
  message** [Correctness/conformance, M, Done 2026-08-15 on `fix/n1-condition-message-split`]:
  `(guard … (load "synerr.scm"))` now answers message `"expected a pair"` and irritants
  `(hello 42)`, per R7RS §4.3.3's delegation to §6.11. The site was `machine.ConditionFromError` as
  filed, but the filed rule ("give the innermost message") is **refuted** by
  `TestContractEnforcement_EndToEnd`, which needs `file-exists?` from an outer wrap: a primitive's
  outer wraps are the operation, not breadcrumbs. Three structural cuts, never textual: (1) drop
  every `*werr.StaticError` category (`werr.FailureMessage`; a strict type test, not `errors.As`);
  (2) drop everything above the innermost `*SourcedError` (`machine.failureChain`), the principled
  boundary, which also stops the location appearing twice; (3) a declared message wins
  (`syntax-error` carries both §6.11 halves as values). **Traps:** `ForeignFileError` and
  `ForeignReadError` embed `*ForeignError`, so the walk must use an interface (only
  `TestFailureMessage`'s read-error row catches a concrete assertion); a non-`werr` wrapper is
  descended **only** when its `Error()` equals its cause's. The chain stays on the wrapped error, so
  the CLI's compile line is byte-identical; `NativeError.Error()` deliberately untouched (plan §3).
  Gates: `pkg/werr/failure_message_test.go`, `pkg/wile/condition_message_test.go`.
  `memory/2026-08-15-condition-message-split.md`.

#### A `load`-time parse error's `error-object-source` names the `load` call, not the file (2026-08-18)

- [x] **The Scheme path had no `ParserError` provenance fallback; the Go path did**
  [Correctness/provenance, S, Done 2026-08-24 on `fix/scheme-path-parser-error-provenance`]:
  `innermostCompileLocation` now consults `*parser.ParserError` when the `sourcedError` walk comes up
  empty, in the same precedence as `wrapCompilationError` (compiler location wins). The Scheme path
  never reached that function: `extensions/eval` sits below `pkg/wile` and calls
  `compilation.ExpandAndCompile` directly. This **corrects the N-1 design's premise** that S3 made
  the compile funnel single; it is single for the Go embedder only. It is also the prerequisite for
  any `compile-error?` predicate, which would otherwise answer `#f` for every parse error
  (`memory/2026-08-09-unified-error-representation-design.md` §6.4b; the predicate stays behind
  §6.4's consumer gate). Gate: `TestLoadParseErrorReportsReadSiteOnBothArms`
  (`pkg/wile/compile_error_source_test.go`). **Measurement corrections:** the position is the
  innermost still-open list's token (`bad_parse.scm:2:9`, not `2:0`), and the wrong answer was
  whatever source the loader last stamped, not reliably the `load` call. **Trap:** the fixture's
  leading form must bind nothing; with an immutable top level on one engine, a `define` makes the
  second arm fail with a redefinition error stamped at the define, which reads like a provenance
  answer and produced a false red. **Knock-on, deliberate:** a `read` failure now names the port
  position, not the `(read …)` call (`TestReadErrorReportsPortPositionNotTheReadCall`).
  **Residual:** that answer carries no file name, filed under Tier 2.

#### `force` sub-context truncation (2026-08-18)

- [x] **Convert `force` to a live-chain boundary, on a new `RunBodyUnderGoFrame` seam, with boundary frames returning their env frame to the pool** [Correctness/continuations + Perf, M, Done]:
  a continuation captured inside a delayed thunk ended at the sub-context boundary, so
  `(force (delay (set! captured (call/cc (lambda (r) (r 42))))))` answered `#!void` where the
  `call-with-values` shape answered `(END 42)`; `force` was the one victim the 2026-06-28
  boundary-reification arc had not converted. `stepForcePromise`
  (`pkg/internal/extensions/all/prim_all.go`) runs one link per call under a Go frame whose
  callback carries the promise plus `pendingForce`, so links replace rather than nest (Go stack
  and chain depth O(1) in chain length). `RunBodyUnderGoFrame`
  (`pkg/machine/{operation_go_return.go,run_body_under_frame.go}`) is the general seam for a
  primitive with Go post-work: without it the choice was a `ForeignClosure` finalizer or a
  truncating sub-context. `force` is its only caller.
  **The first leak diagnosis was wrong**: `applyForeign`'s early return on reconfiguration is not a
  leak (the frame is still reachable through the reified frame). The two real losses:
  `NewMachineContinuation` left `envPooled` false, so `RestoreAndRelease` dropped the frame
  (fixed by `transferEnvOwnership`, one owner); and `applyToValuesCode`'s `OpApply` overwrote
  `mc.env` (fixed by `OpReleaseEnvFrame` after `OpPushValues`). Both halves load-bearing.
  Invariant H (`memory/envpooled-clear-is-invariant-h`) survives because the one Go site that
  parents a lasting frame on a primitive's apply frame is now `MachineContext.ClosureEnv`, which
  clears the flag as `OpMakeClosure` does.
  **Measured**, 1M `(force (delay (* n 2)))`: sub-context form 0.48 s / 312 MB / 6.6M objects;
  finalizer form 0.84 s / 1496 MB / 19.7M; Go frame 0.71 s / 1334 MB / 15.7M (co-allocated
  `goFrame` block was −2.3M objects of that); pool release 0.67 s / 1143 MB / 14.5M.
  `make bench-gabriel` flat (no boundary primitive in a loop). The earlier estimate over-predicted
  because the finalizer's apply frame was already recycled. Residual per force: the
  `MachineContinuation` (~265 B, shared with every boundary-reifying primitive) and the `goFrame`
  block (~310 B). Pins: `TestSubContextNonTailEscapeTruncation`, `TestForceDeepDelayForceChain`,
  `TestBoundaryFramesReleaseTheirEnvFrame` (`pkg/wile/tail_call_alloc_test.go`).

## Tier 2 — Embedding API & Product Value

The embedding experience that differentiates Wile.

- [ ] **Extension API contracts Phase 2+** [Embedding, High]: Compile-time (compiler consults `ParamTypes` for static call sites — error before execution, zero runtime cost) and runtime (`buildValidator` wires `ParamTypes` → `SetValidator`). Integration with linter. Prerequisite vocabulary-extension design at `plans/2026-04-21-type-constraint-extension-design.md` (Julia-subset nominal lattice, `OpaqueTypeConstraint`, `Subtype` as primary operation; excludes refinement and union types per invertibility/no-duplication principles). Original parent: `memory/2026-03-26-extension-contracts-design.md`
- [x] **Environment profiles** [Embedding, Done]: Named profiles (Tiny, Console, ConsoleWithLoad, Small, KitchenSink) via `WithProfile`; orthogonal `WithSandbox` modifier; virtual env map (`WithEnv`, `WithEnvMap`); Scheme-level `(environment '(wile <profile>))` support; `SafeExtensions`/`AllExtensions` removed. `memory/2026-03-26-environment-profiles-impl.md`
- [x] **Eager documentation index** [Tooling, Done]: Shipped as lazy-build-and-cache rather than eager scan. `LibraryExportIndex` is built on first `apropos`/`doc` query and cached on `Namespace`; Scheme-level `(apropos)`, REPL `,apropos`, and MCP share the same index, so LLMs can discover unloaded-library procedures from the first query. See PRs #623–625 (`memory/LIBRARY-EXPORT-INDEX.md`) and post-#623 asymmetry fix (`memory/PRIM-APROPOS-EXPORT-INDEX.md`). Original eager-scan design (`2026-04-08-eager-doc-index-design.md`) was superseded before any code shipped.
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
- [ ] **A port carries no name, so everything `read` produces is file-less** [Embedding/provenance, S–M, filed 2026-08-24]: `PrimRead` and `PrimReadSyntax` (`pkg/extensions/io/prim_read_write.go:222,286`) build their parser with `parser.NewParser`, not `NewParserWithFile`, because `values.PortObject` has nothing to hand it — `portBase` is `closed`/`clsr`/`kind`/`datum` and no factory records where the stream came from. Two consequences, and the first predates the second by a long way: **every syntax object a successful `(read p)` returns has `SourceContext.File == ""`**, even from `(open-input-file "f.scm")`, so a program that reads forms and evaluates them gets diagnostics with no file; and a read *error* now reports `2:9` where it should report `f.scm:2:9` (the position half was fixed by the Tier-1 `ParserError` fallback above, and `TestReadErrorReportsPortPositionNotTheReadCall` pins the file-less form as the current answer, so closing this item means updating that assertion on purpose). Fix is a name on `PortObject` threaded from the file-opening factories into both `mkParser` closures; a string or bytevector port has no name and correctly stays `""`. **Decide first:** whether the name is the resolved path or the argument as written (`include`/`load` use the resolver's resolved path — match them), and whether `(read (current-input-port))` names stdin or nothing.
- [ ] **Low-level file predicate reporting *why* a stat failed** [Embedding, POSTPONED 2026-08-11]: Companion to Wave 4 item 6, which makes `file-exists?` answer `#f` for permission errors — authorizer denial and OS `EACCES`/`EPERM` alike — so "absent" and "not allowed to look" become deliberately indistinguishable at that layer (R7RS §6.14 gives the predicate a boolean and no error clause, and the indistinguishability is the confidentiality property Wave 2 item 9c buys elsewhere). An embedder that needs the reason has no way to ask. **Name, signature, error vocabulary and authorizer gating are all UNDETERMINED; design and implementation are both postponed.** Item 6 ships without it and must not block on it. Note the tension to resolve when it is designed: a call that distinguishes denial from absence hands back exactly the oracle item 6 removed, so it needs its own gate — probably a distinct `Action`, not `ActionStat`. See `memory/2026-08-07-review-wave4-embedder-contracts-design.md` §6 for the decided arm table.
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.
- [ ] **MCP triggering rewrite (Lever A)** [Embedding, Text-only]: Rewrite `cmd/wile/mcp.go` `WithInstructions`, 9 tool descriptions, and `prompts/wile-scheme.md` to trigger LLM tool use on algebra/modular/polynomial domains. Correct misleading `libraries` description (currently claims "loaded only" but tool returns full catalog). Validation via `algebra-accuracy` benchmark: closes `powerset_lattice` regression. No code logic changes. `memory/2026-04-18-mcp-triggering-rewrite.md`

### Algebra & Analytics Roadmap

Directions documents — identify prioritized capability extensions. Priority sequence per 2026-04-22 decision: **wile-goast-first** (Tier A — named consumers in wile-goast analysis code, giving wile-goast a complete algebraic palette without digressions into wile), **matching-second** (Tier B — Roth-Sotomayor two-sided matching), then **§5.7 lower-priority** (Tier C).

- [x] **Algebra library roadmap** [Directions]: `plans/2026-04-17-algebra-foundations-directions.md` identifies 6 prioritized directions extending `(wile algebra ...)`. §5.1 `(wile algebra matrix)` shipped via Path D (PRs #684–#691, #695, #696). §5.2 Möbius / incidence algebra — shipped (commit `4ff8a314`, `memory/2026-04-21-incidence-algebra-impl.md`). §5.3 AC-matching shipped via `(wile algebra unification)`, `memory/2026-04-21-ac-matching-impl.md` (Phase 6 closeout). §5.4 Group actions & Burnside shipped as extension of `(wile algebra group)` (`memory/2026-04-22-group-actions-burnside-impl.md`). §5.5 Distributive/modular lattices + Birkhoff shipped as extension of `(wile algebra lattice)` (`memory/2026-04-22-lattice-birkhoff-impl.md`). §5.6–§5.7 broken out as individual items below.
- [ ] **Benchmark statistics (gonum)** [Directions]: `plans/2026-04-18-gonum-integration-directions.md` §5.2 identifies a benchmark-statistics gap in wile. Ships `bench-stats/` ~100–150 LOC; pure Go, no CGo, one `go.mod` entry. Independent track from the companion wile-goast `goastgraph/` work (see wile-goast TODO). Distinct algebraic setting from `(wile algebra matrix)`: gonum is field-valued (ℝ/ℂ), not semiring-parameterized.

#### Tier A — wile-goast-first (named consumers in Appendix A)

- [x] **§5.4 Group actions & Burnside** [Algebra, wile-goast, High, Done]: extends `(wile algebra group)` in place (D1, not a new `group-action` library): `<group-action>`, `orbit`/`stabilizer`/`fixed-points`, `burnside-count`, presets. 124 tests. wile-goast consumers: register renaming (`goastssa/prim_canonicalize.go`), `ssa-rule-commutative`, `boolean-simplify.scm`. `memory/2026-04-22-group-actions-burnside-impl.md`.
- [x] **§5.5 Distributive/modular lattice + Birkhoff** [Algebra, wile-goast, Matching, High, Done]: extends `(wile algebra lattice)` in place: `distributive?`/`modular?`, irreducibles, `birkhoff-representation`/`-reconstruction`, `lattice->locally-finite-poset`, five presets; `<locally-finite-poset>` gains `elements`. Dedekind verified through D(5) = 7581. 155 tests. wile-goast consumers: `dataflow.scm` MOP=MFP certification, `domains.scm`. `memory/2026-04-22-lattice-birkhoff-impl.md`.
- [x] **§5.6 Combinatorial graph** [Algebra, wile-goast, Done]: new `(wile algebra combinatorial-graph)`, distinct from `graph.sld` (semiring Bellman-Ford). 1-WL + individualization-refinement isomorphism, spanning-tree count, chromatic/Tutte polynomials (|V|+|E|≤20 cap), Hopcroft-Karp, presets. 225 tests incl. Petersen and C_6 vs 2K_3 canaries. `memory/2026-04-22-combinatorial-graph-impl.md`.
- [x] **Balanced graph partition (`graph-partition`)** [Algebra, wile-goast, Done]: Kernighan-Lin two-way *balanced* cut in `(wile algebra combinatorial-graph)`. Rejected: the s–t / global min-cut family (Ford-Fulkerson, Dinic, Karger, Stoer-Wagner), which degenerates to isolating one vertex; single-vertex FM, which deadlocks from a balanced seed under a tight tolerance. Consumer: wile-goast `recommend_split`, which mislabels a heuristic as "min-cut". Residual: Phase 2 (rewire `find-split`, recalibrate on `normalized-cut`) is a separate plan in wile-goast; Phase 3 (Shi-Malik normalized cut) deferred, gated on the gonum eigensolver. `memory/2026-06-08-balanced-graph-partition-design.md`, `-impl.md`.
- [x] **§2.2 Free Boolean algebra on atoms** [Algebra, wile-goast, Done]: extracted from wile-goast's `boolean-simplify.scm` as `symbolic-boolean-normalize`/`-equivalent?` in `(wile algebra symbolic)`. Axiom completion 2026-06-09 replaced pairwise commutativity+associativity+idempotence with one AC-normalization axiom (flatten, sort, dedup, fold), fixing a pre-existing non-termination on ≥3-leaf AC terms and making complement detection n-way. Not a decision procedure (no distributivity; use `(wile algebra sat)`). Residual: the same AC fix applies to semiring/ring/field `+` (same latent bug), not migrated. Also shipped `(wile algebra abstract-domain)` and `(wile algebra dataflow)`. `memory/2026-06-09-free-boolean-axiom-completion.md`, `memory/2026-04-22-wile-goast-algebra-extraction-design.md` + `-impl.md`.
- [x] **SAT solver** [Algebra, Done]: `(wile algebra sat)` (`sat?`, `sat-cnf?`, `sat-model`, `boolean-decide-sat?`, `boolean-decide-equivalent?`), CDCL kernel in `extensions/sat/`. Closes the equivalence gaps in `symbolic-boolean-equivalent?`. `memory/2026-05-30-sat-solver-design.md`, `-impl.md`.
- [x] **CFL-reachability path algebra** [Algebra, wile-goast, Done]: `(wile algebra cfl)`: typed CFG production kernels, labeled-edge graph, Reps–Horwitz–Sagiv worklist solver, `dyck-grammar` preset. Not expressible through the semiring API: composition is grammar-constrained, not a free associative `(plus, times, zero, one)`, which is why wile-goast C4's Boolean/tropical sub-items shipped and this one stalled. Context-sensitivity canary proves it strictly more precise than Boolean reachability. `memory/2026-06-05-cfl-reachability-design.md`, `-impl.md`.

#### Tier B — Two-sided matching (Roth-Sotomayor)

- [x] **`(wile algebra matching)` library** [Algebra, Matching, Done]: Two-sided matching per Roth & Sotomayor (1990). Gale-Shapley deferred acceptance (proposer + receiver optimal), hospital/intern many-to-one via Roth's reduction, Conway distributive lattice on stable matchings via Birkhoff (load-tests §5.5), Irving rotations enumeration, egalitarian + sex-equal selectors. Many-to-many (Kelso-Crawford) deferred to follow-up gated on §5.7 matroids (`plans/2026-05-02-algebra-matching-many-to-many.md`). `memory/2026-05-02-algebra-matching-design.md`, `memory/2026-05-02-algebra-matching-impl.md`.
- [x] **§4.2 Tropical permanent / Hungarian primitive** [Algebra, Matching, Done]: `tropical-assignment` shipped in `(wile algebra matching)` — Kuhn-Munkres O(n³) Jonker-Volgenant 1987 form. Returns `(matching . cost)`. Forbidden pairs via `+inf.0`. Unequal sides via padding. Sanity-checked on a 4×4 textbook fixture against brute-force optimum.
- [ ] **§4.2 Maximum common subgraph** [Algebra, Matching]: True code clone detection — bipartite matching between candidate node pairs, branch-and-bound with assignment relaxation. Overlaps §5.6 combinatorial-graph. `plans/2026-04-17-algebra-foundations-directions.md` §4.2.

#### Tier C — §5.7 lower priority

> **Demand note (2026-06-05):** a wile ↔ wile-goast TODO cross-check found **none** of the items below currently have a named wile-goast consumer — the "consumers" cited in each entry (register allocation, `ast-transform` formalization, etc.) appear nowhere in wile-goast's actual TODO. These are completeness-driven, not demand-driven. **CFL-reachability — the one item that had a real wile-goast consumer — has since shipped (`(wile algebra cfl)`)**, so there is currently **no** demand-justified open algebra item. Re-validate demand against `wile-goast/TODO.md` before promoting any item here.

- [ ] **§5.7 Matroids** [Algebra, Low]: `(wile algebra matroid)` — rank function, circuits, duality, Tutte polynomial, matroid intersection. ~300 LOC. Blocks Kelso-Crawford substitutes for many-to-many matching; also unlocks matroid-intersection framing of register allocation and scheduling. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Integer partitions & Young's lattice** [Algebra, Low]: `(wile algebra partition)` — `partitions-of`, conjugate partition, dominance order, Young's lattice as a poset. ~150 LOC. Natural addition given `order.sld`. `plans/2026-04-17-algebra-foundations-directions.md` §2.6 + §5.7.
- [ ] **§5.7 Category theory extensions** [Algebra, Low]: Functors, natural transformations, general adjunctions beyond `galois.sld`'s Galois-connection special case. Formalizes abstract-interpretation composition (Cousot & Cousot 1977). ~400 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Connes-Kreimer Hopf algebra on rooted trees** [Algebra, Low]: Coproduct cuts subtrees — matches `ast-transform`/`ast-splice` primitive operation in wile-goast's `utils.scm`. Formalizes rewrite-rule composition. ~300 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7. **Reassessed 2026-08-03 — do not fund as scoped.** The coproduct/subtree-cut resemblance is vocabulary, not structure: the antipode is what makes it a Hopf algebra and has no refactoring analogue. What `ast-transform`/`ast-splice` actually want is the one-hole context (Huet's zipper), where contexts form a monoid under plugging, and a Lawvere theory for multi-hole templates. Same job, far less machinery. Rewrite-rule *composition* with preconditions is a guarded category, filed separately.
- [ ] **`equal-hash` has no docstring-example harness, and the hashtable family just proved it needs one** [S]: five `Doc` examples in `pkg/registry/core/hashtables.go` called `(make-hashtable)` after the same change made that an arity error, and nothing caught it — `prim_reflection_test.go` string-compares one `Doc` and never RUNS an example. These strings are what `wile doc`, `apropos` and the REPL show. A test that extracts every `Examples:` line from `Registry().Primitives()` and evaluates it would have failed loudly; it would also police the `=> result` claims, which nothing checks today. Scope it to primitives whose examples are self-contained.
- [ ] **Phase asymmetry across the four R6RS hash procedures** [XS]: `equal-hash`, `string-hash` and `symbol-hash` register `PhaseSetRuntime|PhaseSetExpand` (`registry/core/hashes.go`); `string-ci-hash` is `PhaseSetRuntime` only, matching its neighbours in `addMoreStrings` (`internal/extensions/all/register.go`). So a `begin-for-syntax` body can call three of the four. Each file is locally consistent and the split is invisible from either. Decide it on purpose — add `|PhaseSetExpand` for family symmetry, or state that phase parity is deliberately not held — and pin whichever.
- [ ] **`equal-hash`/`string-hash`/`symbol-hash` carry `Category: "hashtables"`** [XS]: so `WithoutCategory("hashtables")` removes two procedures that are not about hashtables, while `string-ci-hash` (`Category: "strings"`) survives — the R6RS quartet splits under a category filter. Either give the four their own category or accept the split explicitly.
- [ ] **`(rnrs hashtables)` should export `equal-hash`, `string-hash`, `string-ci-hash`, `symbol-hash`** [XS]: R6RS lists all four in the library and Wile withholds them. The reason is gone — the export list was a workaround for closure-pointer recognition, which identity-token recognition replaced (`machine.PrimitiveIdentity`), and `TestRnrsHashtablesRecognitionIsIndependentOfTheExportList` pins that the list can no longer affect `make-hashtable`. Verified by hand: a library that re-exports both `equal-hash` and `equal?` no longer breaks the pair. **The one thing to decide first:** `(srfi 13)` exports a DIFFERENT, bounded `string-hash` (a Scheme case-lambda taking an optional bound), so a program importing both libraries would meet the R7RS §5.6 conflict — correct behaviour, newly reachable. Either accept it, or export the other three and leave `string-hash` out with that reason recorded. `docs/reference/r7rs-differences.md` item 16.
- [ ] **A primitive has one identity per environment, and library envs are engine-sized islands** [L]: `Namespace.NewChildRuntime` gives a library env `parent: nil` and a fresh global, so the library env factory (`engine.go:526`) re-runs `LoadBootstrapCore` into it — full registry apply, phase handlers, and the whole bootstrap Scheme — minting a private `*ForeignClosure` for every primitive. Consequences: `eq?` on a primitive is not stable across the import boundary (`(eq? a-car car)` is `#f` when `a-car` came from a library and `car` is ambient), and **each loaded library costs ~7 ms and ~1.7 MB RSS** — measured, 8 `(scheme …)` imports take an engine from 0.01s/23.7MB to 0.07s/42.4MB, so one library load costs more than a whole engine's startup. For names the library imports, its private copy is overwritten by the import and was pure waste. The identity token addresses the ONE consumer that compared pointers (`make-hashtable`); it does not make `eq?` agree, and does not recover the time or the memory. The fix is to stop copying: parent library envs at a full sealed frame instead of re-applying the registry. **Blocked on** the visible-vs-registered split — `WithStrictNamespace` (`engine.go:194`) and a dialect's `PrimitiveRemover` (`engine.go:208`) deliberately give the sealed base a NARROWER registry than library envs get, so a naive reparent starves `(scheme write)` of `display`. Needs a full frame that library envs parent to, with the narrowed top level as a sibling view. **Its price is now paid by every level-2 program**: `WithoutAmbientBindings` binds nothing, so reaching `car` costs one import minimum — re-measured in-process 2026-08-04, `(import (scheme base))` is **9.38 ms** against **3.93 ms / 1.20 MB heap** for a whole `Small` engine, and an eight-library R7RS preamble is **58.57 ms / 7.93 MB heap**. Per-library spread is narrow (6.0–9.4 ms), so the cost is per-import overhead, not library size. This item is what would make level 2 cheap; level 2 shipped without it, tax documented and accepted.
- [ ] **R6RS hashtables Phase 4 — user-supplied hash/equiv** [M]: `make-hashtable` accepts only the built-in `(equal-hash, equal?)` pair; anything else raises `ErrUnsupportedHashtableKind`. Q1-A is pre-decided (widen the `kind` field to hold a `values.Callable`; do NOT reinstate `hashProc`/`equivProc`), but the fork is **not** funded by the Task 4 baseline, which prices the TABLE (hash + bucket scan on symbol/string/fixnum keys) and not the callback. Q1-A turns on the per-element cost of a Go primitive entering the VM to call a Scheme procedure (sub-context + `ApplyCallable` + `RunWithinBoundary`), **never measured in this tree** — the usual precedent, `map` moving out of Go, was a correctness decision with no benchmark. Two benchmarks are the prerequisite: a sub-context callback microbenchmark, and a Scheme-dispatch-wrapper arm over Task 4's shapes. Build those first or the fork is decided on assertion. `docs/reference/r7rs-differences.md` item 12.
- [ ] **`*RecordType` (and identity-`equal?`) keys share one bucket on an `equal` table** [S]: a degradation this work **introduces** — such keys are rejected outright before it. `(*RecordType).EqualTo` is `p == other` with no `EqualComponents`, so it lands in `equalHashStep`'s default arm and every record type in the program hashes as `"*values.RecordType"`. Correct but degenerate: because `equal?` on record types IS identity, `Equal ⟹ same hash` holds trivially and the bucket scan is linear in the number of types. **The obvious repair is barred:** routing identity-`equal?` values through `identityHash` would fix the distribution and break reproducibility, since `identityHash` is an address and `equal-hash` is Scheme-visible. Two things are available — (a) `make-eq-hashtable` already handles such keys correctly and fast, which is what the docs now say; (b) a deterministic `*RecordType` arm (type name + field count, optionally the parent chain) is ~4 lines and discriminates by name. Take (b) when a consumer keys an `equal` table on record types. Same shape covers ports, procedures, native errors and compile-time values, which are filed together because none has a deterministic name worth hashing.
- [ ] **`(wile algebra egraph)`** [Algebra, Transformation]: Congruence closure over a term algebra — union-find + hash-consed e-nodes + deferred `rebuild!`, semiring-parameterized extraction, saturation. Belongs in wile (the definition never mentions Go; the `<term-protocol>` is the seam, Go protocol/theory/cost stay in wile-goast). Plugs into the existing spine: congruence closure is a `<closure-operator>` (`closure.scm:16,90`); extraction is a semiring fold (tropical = min-cost, counting = distinct-term count). **Four findings from the same-day review:** (1) `axiom->rules` (`rewrite.scm:170`) compiles axioms to *procedures*, e-matching needs *pattern pairs*, and `named-axiom-general-form` is a doc string not a parseable pattern (`ssa-normalize.scm:115`) — real e-matching needs a new `axiom->patterns` in shared `rewrite.scm`. (2) `ssa-binop-protocol` has no `make-op-term` (`ssa-normalize.scm:49-68`, the optional 6th arg of `make-term-protocol`), so extraction must rebuild through a per-node **witness term** via `term-make-term`, which also carries metadata and makes the metadata-on-merge policy an explicit choice (Q2). (3) Extraction must gate on `semiring-cycle-safe?` (`semiring.scm:214`): e-graphs are cyclic, so `counting-semiring` does **not** converge; `tropical`/`boolean`/`saturating-counting` are the admissible carriers. (4) ~~Wile hashtables reject pair and vector keys (measured), so the `(operator, child-ids)` hashcons key needs an injective encoding into string/symbol/fixnum/char.~~ **RETIRED 2026-08-04** by the R6RS hashtable work: `make-equal-hashtable` takes pair and vector keys directly, so the hashcons key needs no encoding. Phases 1–3 (~800 LOC est.) avoid the `rewrite.scm` change and already yield the A/B against `discover-equivalences` (`symbolic.scm:577`), whose sole consumer is `../wile-goast/lib/wile/goast/unify.scm:407-408`. Phase 4 (~300 LOC est.) is gated on Q1. `plans/2026-08-03-algebra-egraph-design.md`.
- [ ] **§5.7 Submodular optimization** [Algebra, Low]: Greedy approximation framework. Applies to program slicing, test-suite selection, import minimization (submodular-maximization-under-cardinality). ~200 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Symmetric functions / RSK** [Algebra, Research, Low]: Research-tier. Small consumer: LCS→LIS→RSK connection for statement/parameter-list diff in `unify.scm`. ~500 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.

#### Follow-ups (deferred from shipped plans)

- [ ] **wile-goast AC-match migration** [Algebra, Follow-up]: Migrate `wile-goast/.../unify.scm:421` from `discover-equivalences` to `ac-unify`. Three risks: (1) term-protocol contract compliance, (2) trace-emitting diagnostic paths (`ac-unify` produces no rewrite trace), (3) small-arity benchmark before crossover claim. Scope ~100 LOC. `plans/2026-04-21-wile-goast-ac-match-migration.md`.
- [ ] **AC-matching v2 deferred decisions** [Algebra, Follow-up]: 8 decisions deferred in `memory/2026-04-21-ac-matching-design.md` "Open questions" — non-unit-multiplicity Stickel, sort-typed pattern-vars, E-matching scope. Re-open when a consumer surfaces.
- [ ] **Incidence algebra future extensions** [Algebra, Follow-up]: Items in `memory/2026-04-21-incidence-algebra-impl.md` "Future extensions (deferred)" section.

> Explicitly excluded as Part 7 non-goals in `plans/2026-04-17-algebra-foundations-directions.md` (no prospective consumer; documented here so the exclusion is visible rather than mistaken for oversight): tropical algebraic geometry, simplicial complexes / persistent homology, vector spaces as algebraic objects, holographic algorithms / Pfaffians, spectral graph matching, symmetric-function machinery beyond the LIS connection already tracked above.

### plans/ sweep — feature & new-capability deltas (2026-07-21)

Open feature work found only in `plans/` during the 2026-07-21 triage; recorded here so a TODO
scan sees it. Spans embedding, tooling, macro system, and algebra — kept together by provenance
rather than split across tiers.

- [x] **`--check` compile-only mode + call-site arity checking** [Embedding/Tooling, M, Done]:
  `wile --check` (`Engine.CheckProgram`, `pkg/wile/check.go`) plus static call-site arity against
  primitives, imports, and same-unit defines (`compile_call_arity.go`, `validate.UnitArityOf`).
  `memory/2026-07-19-compile-check-and-call-site-arity.md`. **Five plan claims were wrong**, traps
  for anyone re-reading it: (1) compiling a `define` does register its binding, so "does not
  execute" is proven differentially against `EvalProgram`; (2) `EvalProgram` has three call sites
  in `main.go`, not two, so `--check -f a.scm -f b.scm` would run `a.scm`; (3) globals-only callee
  resolution false-errors on `(define (k car) (car 1 2))`, identity via `GetBinding` is required;
  (4) formals live on the define, not a `SubExp` lambda; (5) the unit arity table cannot live on
  re-entrant `CompileExpression` nor gate on `Binding.IsStable()`, which is stamped only when the
  define compiles and so skipped every forward reference. Arity checking fires in **all**
  compiles; two runtime-arity tests were routed through `apply`
  (`test/scheme/records-test.scm`, `test/scheme/strings-test.scm`), the whole blast radius.
- [ ] **Pipeline seams as parameters** [Embedding, S–M, plan not-started]: expose `current-eval` /
  `current-print` / `current-read` as parameters so embedders can intercept the REPL pipeline.
  `plans/2026-07-11-scheme-pipeline-seams-design.md`.
- [ ] **Climbing-tower Tier 2 — per-phase mutable-state instantiation** [Macro system, L, gated]:
  Tier 1 shipped; Tier 2 (§6) instantiates fresh per-phase mutable state, plus the Boundary-2
  resolution rework (§7.3). Explicitly gated on owner sign-off.
  `plans/2026-07-10-climbing-tower-design.md`.
- [ ] **Bootstrap self-check diagnostics (W2/W3)** [Diagnostic/guard, S]: a load post-condition
  self-check (W2) and a nil-pin census (W3) over the bootstrap core.
  `plans/2026-07-18-bootstrap-core-unification-and-signals.md`.
- [x] **`er-macro-transformer`-equivalent on the sets-of-scopes core** [Macro system, M, Done
  2026-09-04, no code changed]: already built (`compile_er_macro.go`; `identifier?`,
  `syntax->datum`, `datum->syntax`, `generate-temporaries`, `bound-identifier=?`,
  `free-identifier=?` in `pkg/registry/core/syntax.go`); the row and plan status were stale, not
  the code. `plans/2026-07-11-chibi-derived-ergonomics-backlog.md` #5.
- [ ] **MCP LLM support / SOTA server** [Tooling, phased]: LLM-support phases
  (`plans/2026-06-05-mcp-llm-support-design.md`, Phase 1 impl-ready) and the bring-to-SOTA
  design (`plans/2026-04-17-mcp-server-sota-design.md`). Distinct from the "MCP triggering
  rewrite (Lever A)" item in Tier 2 above.
- [ ] **Copilot-review data mining** [Tooling, not-started]: mine Copilot PR-review data (Tier 2
  target; Tiers 3–4 gated on Tier 2). `plans/2026-04-20-copilot-review-data-mining.md`.
- [ ] **All-executed-code coverage tracking** [Tooling, queued]: extend Scheme coverage beyond
  top-level templates to every executed form; blocked on algebra Tier B.
  `plans/2026-04-23-coverage-library-tracking.md`.
- [ ] **`(wile algebra polynomial-ideal)`** [Algebra, plan impl stalled]: univariate polynomial
  ideal abstract domain. Design approved; impl 10/41 boxes ticked but **nothing in git** — likely
  stalled mid-flight, confirm before resuming.
  `plans/2026-06-09-polynomial-ideal-domain-{design,impl}.md`.
- [ ] **`(wile algebra recurrence)` — set-closure & graph-reachability** [Algebra, impl 0/5]:
  `plans/2026-04-16-recurrence-{categories-design,impl-plan}.md`.
- [ ] **SRFI-204 `match`** [Standard library, design draft]:
  `plans/2026-06-04-srfi-204-match-design.md`.
- [ ] **TinyCLOS object system** [Standard library, design only]: classes, MOP, multimethods.
  `plans/2026-06-24-tinyclos-object-system-design.md`.
- [ ] **Kelso-Crawford many-to-many matching** [Algebra, gated]: stub, gated on the
  `(wile algebra matroid)` §5.7 item above. `plans/2026-05-02-algebra-matching-many-to-many.md`.

> Cross-repo (deliverables land in **wile-goast**, not this repo, but tracked here for
> visibility): b3-c2-c6 **C5** Galois auto-lifting + **C6** belief graduation
> (`plans/2026-03-25-b3-c2-c6-design.md`).

---

## Tier 3 — Tooling & Developer Experience

- [ ] **Scheme linter** [Tooling, High, Partially Closed]: Static analysis for Wile Scheme code — catch "plausible but wrong" before execution. **Closed:** unbound bindings and arity mismatches, via `wile --check` and the compile-time call-site arity check (see the `--check` item above). **Still open:** unused bindings, type mismatches, unreachable code, style warnings — plus the two structural limits the shipped work accepted rather than solved: only the *first* error is reported (the compiler stops there, so there is no multi-diagnostic pass), and `--check` is not side-effect-free because `(import ...)` executes library bodies at compile time. Research needed: what do Racket (Check Syntax), Guile, CHICKEN lint tools actually check? How much at expand time vs separate pass? Interaction with type system is a key design question.
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md`
- [x] **Scheme-side line coverage** [Tooling, M, Done]: Shipped and merged to master — `WithCoverage(*coverage.Collector)` engine option (`options.go:443`), `pkg/coverage/` package, `--cover PATH` + `--cover-stdlib` CLI flags (`cmd/wile/main.go:56-57`), Go cover v1 output consumable by `go tool cover -html`, end-to-end `cmd/wile/cover_integration_test.go`. Docs: `docs/coverage/scheme-coverage.md`. `memory/2026-04-18-scheme-line-coverage.md`
- [x] **`--cover` wrote nothing when the program called `(exit)`** [Tooling, S, Done 2026-09-04, `fix/profile-flush-on-exit`]: `exitWithCode` called `os.Exit` before the post-run writers, and every suite ends in `(test-exit)`, so `cover-scm.sh` produced a one-file profile (`sat-test.scm`) while looking like a suite-wide sweep. `extensions/system.SetExitHook` runs before `os.Exit` for both `exit` and `emergency-exit` (the hook is the host's, not the dynamic-wind cleanup emergency-exit may skip); `cmd/wile/main.go` folds all five writers into one `sync.Once`-guarded `writeRunReports`. `system.Exit(code)` is the one path to `os.Exit`; `cmd/wile` exits via `exitUnarmed` or `exitArmed`; the CLI guard is a CAS because a writer failing through the hook re-enters it. **Rejected** option (b), a `process:exit` denial under `--cover`: the gate fires before the status is parsed (the code is lost), and a refused exit is catchable by `guard` and unwinds `dynamic-wind`, so `(exit)` would behave differently only under profiling flags. Trap: regenerating `testdata/axis-b-manifest.scm` (`WILE_AXIS_B_UPDATE=1`) for the `prim_system.go` line shift also rewrites 118 inlining-dependent function-name rows. `make covercheck` now gates the Scheme profile per library directory (exclusions: item below). Pins: `cmd/wile/cover_integration_test.go` (`(exit 0)`, `(exit 3)`, `--cpuprofile`, uncaught-error row), `extensions/system/prim_system_test.go` `TestExitHook`.
- [x] **`--cover` never instrumented imported library bodies** [Tooling, S, Done 2026-09-04, `fix/cover-library-bodies`]: `LibraryRegistry.SetCompileObserver` fires in `compileAndExecuteLibrary` (`library_loader.go`) after the name check and **before** `EvalTemplate`; `setupLibrarySystem` installs `trackTemplateTree` there. A library compiles once per registry, so this covers every importer. **Wrong seam, do not reach for it:** the import observer fires after `LoadLibrary` returns, but the VM records a hit only when the `executed` bitmap exists and `Collector.Track` allocates it, so every top-level library form would read unexecuted. `build/scheme-coverage.out` went from 68 to 116 files. Pins: `TestCLI_CoverFlag_TracksImportedLibraryBodies`, `TestCLI_CoverStdlib_GatesEmbeddedLibraryBodies`, `TestLibraryRegistryCompileObserver`.
- [ ] **Lift the three Scheme `covercheck` exclusions** [Tooling, S, 2026-09-04]: `tools/sh/covercheck.sh` excludes `chibi` (79.8%: 217 of 272 rows in `chibi/test.sld` hit, one row short of the line), `srfi/1` (66.2%), and `srfi/132` (14.2%) from the Scheme gate because they sat below the line when the gate landed. Each is a Scheme suite away: `chibi` needs a handful of `(test …)` forms exercising the reporter's untaken branches; `srfi/1` and `srfi/132` need `test/scheme/` suites over the procedures the existing suites never call (`srfi/132` is the sort library, 1,737 rows, 246 hit). Remove the name from `EXCLUDED_PKGS` as each crosses 80%.
- [ ] **`make doclint` target** [Tooling, S]: Extract `foo.go:N` citations from `docs/**/*.md` and `plans/**/*.md`; assert each file exists and `N` is within `wc -l file`. Cheap version catches the bulk of drift. Existing `check-readme-links.sh` only validates markdown link targets, not prose citations. Past multi-commit doc sweeps (PRs #707, #710, #711, #712, #713) are evidence the check would pay for itself. Stronger form would `go/ast`-parse the cited line and verify the enclosing decl name matches a nearby identifier in the doc.
- [x] **`make planlint` target** [Tooling, S, Done 2026-08-07]: see the Top Priority entry. The central index is TODO.md itself, not `plans/CLAUDE.md`. Residual: the 2026-06-05 header audit left `2026-04-20-axis-b-annotation-bugs` unresolved (cleanup-shipped claim unverifiable from git).
- [ ] **POSIX API / SRFI-170 remaining phases** [Standard library, 9 phases]: Phases 2-10 not started. Phase 1 (directory ops + process extension) completed in PR #565.
- [ ] **REPL tab completion still offers macro-introduced binders** [Tooling + hygiene, S, 2026-07-19]: `Namespace.BoundSymbolNames` (`pkg/environment/namespace.go:315`) now lists only names resolvable under the ambient scope set, via `GlobalEnvironmentFrame.AmbientKeys` (`global_environment_frame.go:267`). The completion path was deliberately left on the unfiltered walk — `Completer.collectBindingNames` (`pkg/repl/completer.go:83`) → `Engine.BoundNames` (`pkg/wile/engine.go:842`) → `Namespace.BoundNamesAcrossPhases` (`namespace.go:342`), which ranges `global.Keys()` at `:353`. The two listings now disagree, and completion can still offer a name that resolves to nothing. **Why it was not filtered alongside:** `BoundNamesAcrossPhases` also walks the expand and compile phase frames, where `define-syntax` keywords live (`compile_define_syntax.go:91`), so an ambient filter would drop any keyword whose binder carries a non-empty scope set. `ee918fd1`'s message states a top-level user binder carries the empty set, which suggests keywords survive — but that is read off a commit message, not measured, and library-defined + imported macros are unchecked. **Measure first:** apply the filter, diff the completion list before/after on a KitchenSink engine; missing macro keywords (`when`, `unless`, stdlib forms) is the disqualifying signal. Not at risk: `let-syntax`/`letrec-syntax` keywords are local bindings (`expander_let_syntax.go:137`), never in `Keys()`. Same read-path family as A1 above, which fixed the sealed-base half of this walk.

---
## Tier 4 — Performance

- [ ] **Recover the 1.3% `MachineClosure` widening cost** [Performance, S-M — **STRUCTURE SHIPPED 2026-08-17, PERF STILL UNMEASURED**; filed 2026-08-01 as "…by splitting the legacy in-place closure into its own type", which is not the route taken]: The closure pair split (`perf(machine,environment): capture closures as shape+parent`) removed an 80-byte frame per evaluated lambda and grew `MachineClosure` 16→24B, because it carries `frame` **and** `parent` where it used to carry one `env`. Measured cost on `BenchmarkParallelScalingCompute` (fib, which never builds a closure in its loop but runs the altered apply path on every recursive call): **+1.25% / +1.56% / +1.17% at P=1/2/4, p=0.002, n=6**; indistinguishable at 8/16 where the spread is ±3%. Deliberately accepted — the same change is −20% to −38% on `…ScalingControl`. Numbers and method are in the `scaling_bench_test.go` header.

  **SHIPPED 2026-08-17 — `MachineClosure` is back to 16B, pinned by `TestMachineClosureIsTwoWords`** (branch `refactor/machine-closure-nil-parent-fold`). Not by splitting a type: `frame` was elided outright. The shape moved to `NativeTemplate.shape` (set by `compileClosureBody`), so `MachineClosure` is `{parent, template}` and `Apply` reads `tpl.Shape()`. `OpMakeClosure` lost an operand — codegen no longer pushes the env literal, so each closure-creation site is `PushLiteral, MakeClosure` (2 ops, 1 literal) instead of `PushLiteral, PushLiteral, MakeClosure` (3 ops, 2 literals). Free side effect: `makeClosureAnnotation` started working — it reads `code[pc-1]` for a template literal, and the env literal used to sit there, so real disassembly showed no `<lambda:name>` while `TestDisassemble_MakeClosureAnnotation` passed on a hand-built sequence codegen never emits. Gates: `go test ./...`, `make lint`, `make covercheck` green. New load-bearing ordering, commented at the site: `MaybeAppendLiteral(tpl)` must stay **before** `compileBody`, because the pool dedups templates through `EqualTo`, which does not compare shape — registering while `tpl` is empty is what makes a match impossible, and moving it later would collapse two identical lambdas onto one shape carrying the loser's binder scope sets.

  **MEASURED 2026-08-17, and the hypothesis this entry was filed under is WRONG.** Interleaved A/B (two prebuilt `extensions/threads` test binaries alternating master/branch within each of 6 rounds, `-benchtime=2s`, benchstat), branch = fold + elide:

  | | P=1 | P=2 | P=4 | P=8 | P=16 |
  |---|---|---|---|---|---|
  | **Control** (3 closures/iter) | −2.73% | −3.20% | −4.27% | −3.94% | −2.47% |
  | **Compute** (fib, 0 closures/iter) | **+1.22%** | ~ | ~ | ~ | ~ |

  Control p=0.015 at P=1 and p≤0.004 elsewhere; Compute p=0.026 at P=1, indistinguishable at 2–16. Geomean −1.60%. Reproduced across two clean runs (Control −2.6/−3.9/−4.4/−4.6/−3.3 and Compute +1.23% p=0.002 in the first).

  **The entry chased the wrong kernel.** Closure size is paid at `OpMakeClosure`, and fib does not execute one in its loop — so 24B→16B cannot recover fib's +1.25%, and did not. The size win lands entirely on Control, which this entry never considered: 3 closures/iteration × 8 bytes off a ~318 B/wind budget, plus 3 fewer instructions and 3 fewer literal loads, against the `mheap`-lock plateau the `scaling_bench_test.go` header attributes Control's ceiling to. The −4.3% peak at P=4–8 is where that allocator is most contended, exactly as the header's byte-volume argument predicts.

  **Compute's +1.22% is unexplained and survived the obvious suspect.** The apply path lost a branch (`ApplyParent` folded) and swapped `mcls.frame` (offset 0 of a 24B object) for `tpl.Shape()` (offset 24 of a 240B `NativeTemplate`, in the same cache line as `parameterCount`/`isVariadic`, both already loaded) — the same load count. A nil-shape guard on the apply path was suspected and moved to `NewClosureCapturing`; the delta did not move (+1.23% before, +1.22% after), so the guard was not it. Remaining candidates, none tested: code-layout/I-cache aliasing from the rebuild (this suite's own header warns it spreads ~2% at P=1, and same-binary `bench-gabriel` drifts ~1.2% geo mean, so a reproducible 1.2% is at the edge of what this method resolves); or a real cost in reaching the shape through the template. **Do not attribute it without a profile.**

  **Gabriel says nothing, and that is itself the answer.** Interleaved A/B of the 16 canonical benchmarks (two CLI binaries alternating per benchmark per round, 10 rounds, benchstat over the per-run totals rather than the runner's own avg/min/max): geomean **+0.34%**, 14 of 16 indistinguishable. The two that cleared α=0.05 — `tak` +1.58% (p=0.004) and `cpstak` −2.31% (p=0.043) — point in opposite directions, and with 16 comparisons the Bonferroni threshold is 0.003, which neither clears. **Do not read either as a result.** For scale, `memory/` records that the *same binary* run twice drifts +1.2% geomean with 14/16 in the same direction; this whole comparison sits below that floor.

  Why the suite is blind here, which is the useful part: Control's win is byte volume into a **contended shared allocator**, and Gabriel is single-threaded, so the `mheap` lock the win acts on is never contended. The remaining lever, one fewer instruction and literal per closure creation, only pays at `OpMakeClosure`, and these are tight loops over closures already built. There is a faint directional echo of the scaling result — the call-bound rows lean slow (`tak`, `fib`, `sumfp`, `divrec` all ≈ +1.3–1.6%) and the allocation-heavy rows lean fast (`cpstak`, `primes`, `nqueens` −1.0 to −2.3%) — but individually none of it resolves. **The conclusion is that Gabriel is the wrong instrument for this change, not that the change is neutral.** Multi-threaded allocation pressure is where it lives; use `extensions/threads`.

  **Trap for the next reader:** an intermediate A/B of this same pair read "regression gone, p=0.818" and was contaminated — a subagent running gopls queries concurrently with the benchmark. Interleaving defends against drift, not against a load spike landing unevenly across arms. The tell was the branch arm spreading ±56% against master's ±2% in the same run. Re-run with nothing else on the machine.

  **Separate what is measured from what is guessed.** Measured: the regression, and (via `-gcflags=-m`) that `ApplyParent` and `InitApplyFrame` both still inline, so it is *not* a lost inline. Guessed: that the 16→24B widening is therefore the cause, and that returning to 16B would recover it. Neither has been tested — 16B now exists, so the test is finally cheap. Do that — a 24B and a 16B struct land in different Go size classes, but so would any other layout change, and 1.3% is close enough to this benchmark's ±1% floor that it needs `-count 6` + benchstat against an adjacent baseline (see the header's replication warning; an earlier cut in this arc first read as +14% and was in fact p=0.699).

  **The lever, and it got simpler.** The two fields were carried to discriminate two representations, but the second one **has no production producer**. Every caller of `NewClosureWithTemplate` — **two** at HEAD (`PrimCompile` in `extensions/eval/prim_eval.go`, `createTransformerClosure` in `pkg/machine/compilation/compile_syntax_rules.go`, which builds every `syntax-rules` transformer); an earlier revision of this paragraph said three by counting `createTransformerClosure` as additional to the two rather than as one of them — passes a frame from `NewEnvironmentFrameWithParent`, which panics on a nil parent. Verified by panicking in that arm and running the whole suite green. So `ApplyParent()` is always non-nil, and `frame` could hold the compile-time frame alone with the parent read from it, or the two could collapse some other way. This entry originally cited "the in-place case must read `frame.Parent()` LATE" as the constraint making a split hard; that requirement came from `(compile …)` capturing a *pooled* frame whose parent went nil, which was the use-after-release bug fixed in the same arc. It no longer binds.

  **Check `frame`, not `parent`, as the field to remove** [2026-08-17, UNVERIFIED]. `parent` cannot go: it is the only *per-activation* word in the struct. `frame` and `template` are both template constants shared by every evaluation of the lambda, and `frame.Parent()` is the **compile-time** parent, whose bindings are placeholders — which is exactly why `NewClosureCapturing` panics on a nil runtime parent instead of falling back to it. Removing `parent` therefore means re-materializing a frame to hold the same pointer, i.e. the 80-byte design this arc reverted. `frame` has the opposite property: it is **1:1 with the template**. `compileClosureBody` appends `childEnv` and `tpl` to the same literal pool adjacent, and both emit sites load them as a pair (`compileClosure`; `CompileValidatedCaseLambda`). Hanging the compile-time frame off `NativeTemplate` leaves `MachineClosure = {template, parent}` = 16B, which is this entry's target, and it deletes a field rather than adding a type — so it does not risk gate (b).

  Two things to settle before prototyping:
  - **Prerequisite SHIPPED 2026-08-17 — the nil-`parent` representation is folded away** (branch `refactor/machine-closure-nil-parent-fold`). `NewClosureWithTemplate` now stores `parent: env.Parent()`, so both constructors capture eagerly and `MachineClosure` has one representation. `ApplyParent` is a field read; `Env` materializes unconditionally and returns nil only for the degenerate no-environment case, which `closureBoundSymbols` already handled. **It was not free.** The old late `frame.Parent()` read doubled as a use-after-release check: a frame released after the closure was built zeroes its own parent, and the closure now keeps the stale pointer instead of faulting at apply. That check only ever covered the two `NewClosureWithTemplate` sites — `OpMakeClosure` has always captured eagerly, and it builds every closure a Scheme program makes — and both sites now build a fresh frame rather than borrowing a pooled one, which is what actually closed the `(compile …)` use-after-release the check stood in for. The live protection is `mc.envPooled = false` at `OpMakeClosure`. Gates: `go test ./...`, `make lint`, `make covercheck` all green, plus both representations exercised at the Scheme level (a `(compile …)` thunk applies and reflects; a `syntax-rules` transformer expands). Not re-run: `make test-race` — no new sharing, both reads are on the owning goroutine.
  - **Unverified: whether `NativeTemplate` can own the frame for the two `NewClosureWithTemplate` sites**, where the env is built by the caller instead of by `compileClosureBody` — `createTransformerClosure` mints one per transformer (`compile_syntax_rules.go:948`) and `PrimCompile` one per `(compile …)` (`prim_eval.go:628`). Both are 1:1 with their template today, but by construction rather than by the compiler's literal-pool pairing, so the invariant has to be re-established there, not assumed.

  **Gate:** only worth doing if (a) a 16B prototype actually recovers the 1.3% under benchstat, and (b) it does not put dynamic dispatch on the apply path — this codebase has a measured preference for switch over table dispatch, and `applyClosure` is the hot path the change is trying to speed up. If (b) eats the gain, close this as WONTFIX and leave the note.

- [x] **`resolveGlobal` re-locks one frame once per lexical depth** [Performance + structure, OBSOLETE 2026-08-05 by the store fold, filed 2026-07-19]: the premise (a walk of the `EnvironmentFrame` chain taking `ge.global.mu.RLock()` and a name lookup at every hop) no longer holds; `resolveGlobal` takes one read lock and runs one ranked probe against the owner's single store (`resolveRankedLocked`). Never measured; nothing to measure now. Rejected alternative, still worth knowing: making `GlobalEnvironmentFrame` satisfy an `EnvironmentFrame` interface with a permanently-nil `Parent()`. `EnvironmentFrame` is a struct, so that means a new interface and dynamic dispatch on the VM's hottest path, and a nil parent erases the sealed-base shadowing the layered carve exists to provide.

- [ ] **Benchmark + profile the cycle-detection and context-poll cost added to `Pair.ForEach`** [Performance, S, UNMEASURED — crosscheck `15b68433..8c297173`, 2026-07-14]: `Pair.ForEach` (`pkg/values/pair.go`) gained Brent's cycle detection (pointer compare + increment + branch, plus a power-of-two checkpoint teleport) **and** an amortized `ctx.Err()` poll, on every walk. The correctness win is real and not in question — it closed the unbounded walk that let `(apply + circular-list)` grow the eval stack past every configured limit. What is in question is the cost, because **this is *the* list walker**: the code's own comment names the blast radius (`ForEachProperList`, `length`, `list-copy`, `append`, `reverse`, and apply's argument spread all funnel through it). No benchmark evidence was produced with the change.
  Work: A/B `make bench-gabriel` and `make bench-extended` across the commit, and profile a list-heavy workload end-to-end (`wile --cpuprofile`). Per `memory/`: micro-benchmarks mislead here — profile end-to-end, and do it *before* deciding anything. If the cost is material, the levers are (a) hoist the cycle check behind a length threshold so short lists (the common case) pay nothing, or (b) split a `ForEachUnchecked` for callers that have already established properness. Do not pre-emptively optimize: measure first, and record the numbers here either way so the next person does not re-ask.

- [ ] **Shrink `Binding` to recover the D2 atomicCell regression** [Performance, M — PUNTED 2026-07-06: too complex for the payoff, don't pick up without new evidence]: The D2 race fix (commit `fbcd7654`) grew `Binding` 32→40B (heap `atomicCell` pointer), inflating the value-embedded local frame slabs (`[]Binding`) and costing **+4.6% geomean on bench-gabriel (15/16 slower)**. Recovery lever: shrink `Binding` back so the local-frame slab footprint returns to baseline while globals keep the atomic cell (e.g. move rarely-used fields off the hot struct, or split local vs global binding representations). Gate on re-running bench-gabriel to confirm the recovery. Pure-perf follow-up; correctness is already banked. **Punt rationale (2026-07-06 analysis):** the recoverable win is *capped at the slab half* of the 4.6% — the other half is the global-read pointer hops (`*Binding → cell → atomic.Load → deref boxed value`), intrinsic to atomically publishing a 2-word `values.Value` and unrecoverable without 1-word NaN-boxing (separate `unsafe`-blocked plan). And getting the local slab to 32B soundly is not cheap: `value`/`cell` are a mutually-exclusive union but Go has no unions; pointer-tagging `bindingType`/`cell` low bits is GC-unsafe; `bindingType int`→`uint8` alone pads back to 40. The only sound mechanism is a cross-package `LocalBinding`/`*Binding` type split (ripples `environment/` + `internal/validate/` + `machine/pool`), whose natural unifier (an interface) adds dispatch to the Apply hot path and can eat the gain. Better lever for the same 4.6% is MORE frame reclamation (remove the slab allocation entirely so its size stops mattering) — but that arc is itself PAUSED (see `plans/2026-06-18-frame-reclaim-precision-coverage.md`: value core A/E shipped, tails B/C/D/F/G stopped under "limited payoff is a valid stop", resume gated on a real workload showing frame-leak pressure). **Resume this only if a representative embedding workload profiles as local-slab-allocation-bound AND the type-split measures net-positive on bench-gabriel.**
- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.md`
- [ ] **B3 effective capture refinement** [Performance, Research]: Propagate B2 escape results back into B1 capture status. A binding marked `Captured` by B1 is effectively non-captured if every lambda that references it is stored in a non-escaping binding (B2). Cross-binding analysis over B1+B2 results.
- [x] **`PrimitiveSpec` capture-safety capability field** [Performance, M, Done, PR #776]: shipped as `PrimitiveSpec.InvokesProcedure` (`pkg/registry/apply.go`); each primitive self-declares, the classifier stamps `Binding.CaptureSafe = !spec.InvokesProcedure`, and extension primitives self-cover. Retired the hand-maintained `captureSafePrimitiveNames` whitelist in `internal/validate/frame_reclaim_build.go`, which silently under-covered extensions. A false positive (a capturing primitive declared safe) is unacceptable, per `feedback_annotation_stability.md`. Q-1 of `memory/2026-06-12-escape-frame-validation-impl.md`.
- [x] **`markCaptured`/`.Captured` is dead code: delete or unify** [Tech debt, S, Done]: deleted (option a) `markCapturedBindings` (`internal/validate/validate_capture.go`), `ValidatedLetBinding.Captured`, and `validate_capture_test.go`; its shared helpers moved to `internal/validate/sharedtest_test.go`. The live `markEscaped`/`.Escapes` path (let-lambda inlining) and `bodyReferencesCaptureOperator` (call/cc detection) are untouched. Unification (option b) rejected: with B1/B3 never built there is no second consumer, and folding a fail-safe predicate into a best-effort one risked the inlining contract.
- [ ] **Benchmark coverage gaps** [Performance, S-M]: No benchmarks for compiler, expander (syntax-rules expansion), library import resolution, or continuation capture/restore cycle. Existing benchmarks cover VM dispatch, fibonacci, tokenizer, parser, environment, and symbol interning.
- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.md`
- [ ] **Inline-budget guard for `checkStackSize` and similar hot-path wrappers** [Performance, S]: `checkStackSize` (`machine/machine_context.go:1185`) is split from `reportStackOverflow` specifically to stay under Go's 80-cost inline budget (currently 67). A future innocuous edit could push it over and silently regress the VM hot path (the Gabriel suite would catch it, but only post-hoc and noisily). Write a test that runs `go build -gcflags='-m=2' ./machine/` and asserts `"can inline (*MachineContext).checkStackSize"` appears in the output. ~30 LOC test infrastructure; reusable for future hot-path wrappers. Surfaced by Finding 5 / PR #734 type-design review.

---

## Tier 5 — Tech Debt

### Sealed axis keyed by `(phase, kind)` — SHIPPED 2026-08-03 (`74c72256`)

- [x] **Sealed-frame routing collapsed into one table** [Tech debt / legibility, M, Done 2026-08-03, `74c72256`]: seven sites hand-unrolled a two-row loop over the phase-0 and phase-1 sealed frames by name. `sealedAxis` (`pkg/environment/sealed_base_frame.go`) is now the single decision site, read by `SealedAt`, `sealedFrameAt`, `SealedFrames`, and `IsSealed`; call sites read as coordinates (`SealedTargetAt(PhaseRuntime, SealKindValue)`, now `SealedWriteViewAt`). `SealedBaseTarget() != env` became `IsNamespaceRuntime()`. No behavior change on any live path. Three `/crosscheck` traps, fixed in the same commit: (1) generalizing the seam turned "there is no seal here" into cover for "the seal is missing", so `SealedAt` returns `(*EnvironmentFrame, bool)` and a declared-but-absent frame panics via `mustSeal`; (2) `phaseParent` runs under the `PhaseRegistry` write lock, where a seam that can re-enter `GetOrCreate` hangs, and its parent link is kind-independent, hence `sealedFrameAt` plus the pure-pointer `IsNamespaceRuntime` guard; (3) `registry.SearchDoc` (`,apropos`) was a second hand enumeration a new row would have silently missed.

- [x] **Expand-phase registry primitives are NOT sealed** [Correctness, S code / M test rewrite, Done 2026-08-10,
  `693ee6f6` (`fix/w1-carve1-seal-phase1-registry`)]: `registry.Apply` bound `PhaseExpand` primitives into the
  mutable expand child, so a top-level `define-for-syntax` of a primitive's name superseded it in place at
  phase 1 (the same program shadows at phase 0) and falsified the `Stable` stamp the frame-reclaim classifier
  trusts. The copies are sealed (`24e08ceb`), and a bootstrap `define-syntax`/`define-for-syntax` over a
  `Stable` phase-1 slot is refused (`0f8ae684`; refuses on `m.Stable`, not `IsStable()`, so an import stays
  supersedable per R7RS §5.3.1). **Do not delete the phase-1 copy** as redundant with the wildcard-phase
  `(ANY, sealed)` one: the two differ in `closureEnv` (`apply.go`), which is load-bearing for `compile`,
  `expand` and `free-identifier=?`. `memory/2026-08-07-review-wave1-silent-corruption-impl.md` Phase 5b.

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

### `PrimitiveSpec.Mutates` has no static guard (2026-08-09)

- [ ] **Derive destructiveness from the code, not only from the annotation**
  [Correctness-adjacent, M]: `TestMutatesMatchesMutationPrimitives` pins the
  annotated set against `NoMutation`'s removal list in both directions, but
  derives NEITHER from what the primitives actually do. A new destructive
  primitive that is neither annotated nor bang-suffixed passes both arms —
  which is precisely how `record-modifier` survived for as long as it did, so
  the hole is demonstrated rather than hypothetical.
  `InvokesProcedure` has the missing analogue: `TestInvokesProcedureStaticGuard`
  walks each `Impl`'s call graph in the AST and fails CI when the annotation is
  absent. The equivalent question here — "does a path exist from this `Impl` to
  a `values`-package mutator?" — is a `go/analysis` SSA pass rather than an AST
  walk, because the reachable mutator is often behind an interface method and
  sometimes inside a closure the `Impl` RETURNS (`record-modifier`'s shape).
  **Known and out of reach even then:** a mutator reachable only by APPLYING a
  returned Scheme object. `make-parameter` is the live case — `(p v)`
  destructively sets a parameter and survives `NoMutation` — and annotating it
  is NOT the fix, since that removes `parameterize`, which is R7RS §4.2.6
  dynamic binding rather than mutation and works on a `NoMutation` engine
  today. That one belongs with the dialect's documented language-surface
  boundary.

### `extensions/gointerop` is named for a story it does not tell (2026-08-09)

- [ ] **Rename or relocate `extensions/gointerop`** [Naming / API, S, **not a defect** — nothing
  misbehaves]: the package registers exactly six primitives, all atomic boxes — `make-atomic`,
  `atomic?`, `atomic-load`, `atomic-store!`, `atomic-swap!`, `atomic-compare-and-swap!` — and its
  implementation file is 85 lines. Nothing in it touches Go values, reflection, or host functions.
  The RWMutex/Once removal already corrected the *description* string ("Go concurrency primitives:
  atomic boxes") and `doc.go` already lists only atomics; the **name** was not corrected, and the
  removal plan declared the rename out of scope and asked that it be raised separately. This is
  that row.
  The actual Go-interop surface is `Engine.RegisterFunc` / `RegisterFuncs` /
  `RegisterPrimitive` plus the reflection bridge in `pkg/wile/ffi_wrapper.go` and the two
  `ffi_*_converters.go` — a Go-host-side API a Scheme program cannot reach at all.
  **Go package and Scheme library rename independently**: the library name `(wile gointerop)`
  comes from the extension NAME STRING (`register.go:24`), not from the package path, and
  `registry.WithLibraryName` is unused here.
  Measured blast radius (excluding `memory/` and `plans/`): 2 import sites + 2 registration-list
  entries; 4 production comment mentions; 17 lines across 10 test files; 6 rows of
  `testdata/axis-b-manifest.scm` carrying the symbol path (regenerate with `WILE_AXIS_B_UPDATE=1`);
  8 doc files. Two ratchets key on literals — `extension_consistency_test.go` on the extension-name
  string and `callable_narrowing_ratchet_test.go` on the source-file PATH — and both **fail loudly**
  under `go test` rather than silently, so the rename cannot rot them unnoticed.

### No memory or CPU custodian at the engine level (2026-08-09)

- [ ] **A short program can consume unbounded memory and time, uninterruptibly** [Design, M,
  engine-level]: `(expt 10 1000000000)` is a 20-character form; measured scaling puts it at
  roughly 16 minutes and 3.7 GB, and `with-timeout` does not bound it — the handler runs only
  after the primitive returns. **Filed against the engine, not against `expt`**, because the
  primitive-surface panic sweep's proposal to cap `exptExact` was refuted on two counts:
  uninterruptibility is a documented VM-wide property of every foreign call
  (`docs/concurrency/cancellation.md:126`, "the same VM-wide cancellation latency every primitive
  has"), and a 60-character loop of `*` with no `expt` anywhere reaches 147 MB and 14.3 s, so a
  ceiling on `expt` alone would refuse `(expt 10 N)` while permitting its own square-and-multiply
  algorithm written out in Scheme — the behavioural asymmetry this project's policy says to remove
  rather than add. `docs/environment/racket-namespaces.md:288-290` already names the gap: resource
  limits exist as engine options (`WithMaxCallDepth`, `WithMaxStackSize`, `WithMaxExpandDepth`) and
  "the custodian half has no counterpart". The existing per-primitive ceilings
  (`MaxReadBytevectorBytes`, `MaxReadStringBytes`, `MaxMakeLength`) are not precedent: each bounds
  an explicit LENGTH ARGUMENT naming one allocation, which is a domain check on a caller-supplied
  value. An exponent is not a length.

### Channel done-channel lifecycle follow-ups (adversarial review 2026-07-16, `fix/channel-lifecycle-ctx`)

Residue from the adversarial review of the done-channel lifecycle rewrite. The
rewrite itself is a real correctness win (closes the `channel-send!` TOCTOU host
panic and the ctx-ignoring parked-goroutine leak) and is `-race`-clean; **no new
correctness bug survived verification** (the `with-timeout ∘ channel-receive`
laundering I hypothesized is blocked by the eager `ErrTimerExpired` check at
`call_foreign_cached.go` after every foreign return: 60/60 handler-runs, 0/40
side-effect leaks). These four items are the leftover design/API/docs/test debt.

- [ ] **Is Option A (launder a cancelled wait as an ordinary result) a committed contract?**
  [Design, S–M]: **REWRITTEN 2026-08-09; the two rows this replaces named code that no longer
  exists.** They were about `PrimChannelSend`/`PrimChannelReceive` and
  `SendOutcome`/`RecvOutcome` in `pkg/values/channel.go` — verified absent from every `.go`
  file in the tree; channels and wait-groups were removed in 1.19.1 by
  `c2959eb1`, `pkg/values/channel.go` does not exist, and
  `TestWithTimeoutInterruptsParkedReceive` (which one row credited itself with) is gone too.
  `extensions/gointerop/prim_gointerop.go` is 85 lines of atomics.
  **The QUESTION survived the code, on the SRFI-18 wait side**, which is why this is a rewrite
  and not a deletion: `mutex-lock!` and `(mutex-unlock! m cv)` each have a free value channel
  and report a ctx-cancelled wait as an ordinary error-free `#f`, indistinguishable from a
  legitimate failed acquire or an unsignalled wait
  (`docs/concurrency/cancellation.md:89-98`). `thread-sleep!` and `thread-join!` do NOT
  launder — they raise `werr.ErrOperationCancelled` carrying the raw ctx cause (:99-112).
  The error-free `#f` is load-bearing rather than sloppy: `callForeignCached`'s eager
  `ErrTimerExpired` recheck fires only on the error-free return path (:121). Only an
  embedder-supplied deadline (`mc.timer == nil`) can observe the laundering, bounded by the
  ~1024-op VM poll (:123, which also records that no test covers it). Decide the contract and
  the asymmetry follows: committed ⇒ document it and stop calling it a seam; placeholder ⇒
  surface the cancellation distinctly on the two laundering waits, as `thread-sleep!` and
  `thread-join!` already do.
- [x] **`RWMutex` and `Once` removed from the Scheme surface** [API/modeling, M, Done, deleted]: 12 primitives, `values.RWMutex`, `values.Once`, `werr.ErrNotARWMutex`, `werr.ErrNotAOnce`, and `finishBlockingSync`; `(wile gointerop)` is now the six `atomic` primitives. **A modeling decision, not a cleanup**: `gointerop` was publishing Go's shared-state model against the project's causal-chain orientation. Public API removal under the zero-consumer rule, as with `ChannelSelect`. Kept: SRFI-18 `mutex-*`/`condition-variable-*` and `atomic`. Traps: the plan's verification grep (hits only in `memory/`/`plans/`/CHANGELOG) was never satisfiable, since ~13 live `sync.RWMutex` struct fields remain; `ErrNotAOnce` was the sole pass-through-article row in `pkg/registry/helpers/args_test.go`, so that sentinel is now built inline to keep `typeNameFromSentinel`'s article path covered. `TestWithTimeoutInterruptsParkedRWMutex` was ported to `mutex-lock!`, not dropped: it is the only test spanning `callForeignCached`'s eager recheck and a primitive's error-free `#f`. Under an embedder deadline `mutex-lock!` returns `#f` and the VM's top-of-loop check surfaces `DeadlineExceeded` (`pkg/machine/machine_context.go:365`); no test covers that `cancellation.md` row. Residual: `werr.ErrOperationCancelled` was left with no producer and a rewritten comment. **DECIDE:** delete it, or keep it for a future primitive that returns a value on success and so cannot borrow the error-free-`#f` convention.
- [x] **`ChannelSelect` was complete, tested, CHANGELOG-cited, and registered nowhere** [API/dead-code, S–M, Done, deleted]: removed `ChannelSelect`, `SelectCase`, `SelectCaseKind`, `firstDeadCase`, and 8 `TestChannelSelect*` functions (~312 lines). Exported from `values/`, so a public API removal under the zero-consumer rule. Wiring it would have needed a ctx arm, not just `done` arms, or it reintroduces the T1.3 leak at a new site (a prototype confirmed 2N+1 arms are `-race`-clean, so the decision was never technical). Released CHANGELOG sections (1.18.0, 1.3.0) were not edited; a removal note and correction went into `[Unreleased]`. If a consumer appears: `reflect.Select` panics past 65536 cases and the list would come from Scheme, so it needs an arity guard.
- [ ] **Stale sub-context comment on `with-timeout`** [Docs, XS]: `PrimWithTimeout` (`pkg/registry/core/prim_timer.go`) header says "The sub-context pattern ... a fresh sub-context isolates the thunk's execution," but the same function's body twelve lines down (and `RunBodyUnderTimer`) says it runs the thunk **INLINE on the live chain, not in a sub-context** (the accurate description). REVIEW.md lists stale comments as a recurring trap; this one misdescribes the isolation model of the code that makes the `with-timeout` cancellation path safe. **Possible actions:** (A) delete the stale sub-context sentence; (B) rewrite it to match the inline model. **Recommendation: A (delete)** — the accurate description already exists in the same comment, so B just duplicates it.
- [x] **Scheme-level cancellation tests added; two real defects found writing them** [Test-coverage + Correctness, S, Done, A+B+C shipped]: `extensions/gointerop/channel_cancellation_test.go` added `TestWithTimeoutInterruptsParkedReceive` and `TestTerminateUnparksBlockedThread`, both mutation-verified. Two defects surfaced:
  - **`thread-terminate!` discarded its own SRFI-18 end-exception** [Correctness, S, Done]: `Thread.Start`'s goroutine unconditionally overwrote the stored `TerminatedThreadException`, and `defer close(p.done)` ran last, so a joiner could never observe it; a thread parked in a tail-position receive was reported as having succeeded. This invalidated the design doc's claim that the ≈1024-op unwind window was itself the protection. Fix: a write-once outcome (`Thread.setOutcome`), first writer wins, which keeps SRFI-18's "if the thread is not already terminated". Prior coverage was vacuous (both tests asserted the `#t` literal they wrote). Guard: `extensions/threads/prim_threads_terminate_outcome_test.go`.
  - **`thread-join!` on a terminated but never-started thread blocked forever** [Correctness, S, Done]: `done` was closed only by the goroutine `Start` spawns. `Terminate` now closes it when ending a `ThreadNew` thread. The two closers are mutually exclusive because `Start` makes `ThreadNew → ThreadRunnable` under `p.mu` and refuses any other state, so no `sync.Once` is needed (a double close is a fatal host panic). Guards: `pkg/values/thread_lifecycle_test.go` (20000-trial `-race`) and `TestThreadTerminateNeverStartedThreadIsJoinable`, which joins with no timeout so a regression is an unbounded park rather than a misleading `JoinTimeoutException`.

**One decision gates the deferred sub-item:** *is Option A a committed contract?* — see the rewritten row above. The second decision this line used to name, *is `channel-select` on the roadmap?*, is spent: `ChannelSelect` was deleted with the rest of the channel surface in 1.19.1.

### FCA-Derived

- [x] **Structural-reduction roadmap** [Planning, Done — closed 2026-07-08]: spent and archived to `memory/2026-05-07-structural-reduction-roadmap.md`. Tier A closed (`values/` PRs #747–#756, `environment/` PR #730, `registry/` PR #728); Tier B closed (`wile/` PR #764; `repl/` + `registry/helpers/` on `refactor/structural-reduction-b2-b3` — low yield as predicted, most findings refuted or declined as churn); Tier C reassigned to `plans/2026-07-01-staff-engineer-sweep.md`.
- [x] **Machine package structural reduction** [Done 2026-05-13]: all 7 findings closed. Shipped — `Stack.Push` max-stack (PR #734), `OpKind()` discriminator (PR #735), vmState value-register consolidation + ruleguard (PR #736), correlated-field sub-records (PRs #742/#743/#745). Declined — syntaxCase marker interface (PR #731), maxCallDepth sentinel removal, tail/non-tail opcode collapse via sign-bit encoding (PR #737: geomean +2.5%, all 16 benches slower), Stage-3 sub-records (field-independence analysis found no co-variance, `9382a3b3`). `memory/2026-05-06-machine-structural-reduction.md`
- [x] **Internal / values / environment structural reduction** [Done]: `internal/` all 7 findings (PRs #739–#741, including the `*SyntaxPair`/`SyntaxEmptyList` duality migration that restores Chez-conformant `(equal? (syntax ()) '())`); `values/` Phases 0–4 (PRs #747–#756 — 9 port types collapsed to one `*Port` with capability slots, ~900 LOC, and a `NumericTypeSpec` registry replacing the 12-step ADDING-A-NEW-NUMERIC-TYPE guide); `environment/` Phases 1–9 (PR #730), Phase 10 (`*LocalIndex` allocation audit) deferred benchmark-gated. Plans archived under `memory/2026-05-0*`.
- [x] **vmCore sub-struct extraction** [High, M, DECLINED on re-evaluation 2026-06-05]: the genuine always-transfer set is only `{env, template, pc}`. `callDepth` is a guarded maintained counter (`SaveContinuation` ++, `PopContinuation` --, derived from the parent by both continuation constructors), so bundling it forces override-after-copy at 4 sites and risks clobbering its guards. The FCA "High" rating rested on the divergent fields (`evals`, `envPooled`, `marks`), none of which a vmCore touches, and drift is already answered by `testVmStateFieldCoverage`. Net ~6 lines saved at 3 sites on the VM's hottest path. Parallels the decline of machine SR Finding 7 Stage 3.
- [ ] **Bidirectional opcode conversion test** [Medium, S]: Verify `operationToInstruction` and `instructionToOperation` cover the same opcode set.
- [ ] **LocalEnvironmentFrame pointer ambiguity** [Low, S]: Doc comment on `NewLocalEnvironment` explaining lifecycle (value-vs-pointer ownership).
- [ ] **Honor `WithInlineThreshold` for imported libraries** [Low, S]: The library import/load chain (`LoadLibrary` → `loadLibraryFromReader` → `compileAndExecuteLibrary`, `machine/compilation/library_loader.go:215,223`) has **no `inlineThreshold` parameter**, so imported libraries always compile at `DefaultInlineThreshold = 5`, ignoring the engine's `WithInlineThreshold(n)` (`pkg/wile/options.go:275`). Every *in-process* child compiler re-threads the parent's value via the two-line `NewCompileTimeContinuation(...)` + `SetInlineThreshold(p.inlineThreshold)` idiom (6 sites: `compile_syntax_case.go:253`, `compile_closure.go:123`, `compile_library_forms.go:109`, `compile_helpers.go:51`, `compile_time_continuation.go:347`, `expand_and_compile.go:53`); the load path is the one site that cannot reach the value. **Not a correctness bug** — inlining here is the behavior-preserving synthetic-let transform (PR #605), so results are unchanged; it is a config-honoring / debuggability inconsistency (disabling inlining, e.g. for predictable stack traces, is silently not honored across the `import` boundary). Fix: thread `inlineThreshold` through the three `LoadLibrary`/`loadLibraryFromReader`/`compileAndExecuteLibrary` signatures (or expose it via `Namespace`/`EngineServices` so the load path can read it) and `SetInlineThreshold` on the library compiler. Discovered during the `CompileTimeContinuation` God-object triage (2026-07-09); the fix also illustrates why the "stable config should be inherited, not hand-copied" refactor (staff sweep tail) has real payoff — a shared services pointer would close this gap by construction.
- [x] **Unified binding reference (`BindingRef`) for local+global** [Medium, M, Done 2026-07-08, `229e0b72`]: `BindingRef` sum type + `ResolveBindingRef` in `environment/`; the validator's mutation set went from 3 maps to 2. **Storage stays split, deliberately**: locals are positionally addressed (`LocalIndex{over,up}`), copied every `Apply`, single-threaded, `[]Binding` by value; globals are symbolically addressed, shared across SRFI-18 threads, and `[]*Binding` pointer-stable because the lock-free `cachedBindings` read cache requires it. Only the reference type unified. **Premise correction**: the validator was not blind to top-level `set!` (the symbolic `mutatedKeys` sidecar compensated and `StableInUnit` was correct), so this was a semantics-preserving tidy, not a bug fix. The conservative over-mark (a `set!` to a local shadow still marks the top-level name non-stable) is the frame-reclaim soundness margin, pinned by `TestStableInUnit_SetToLocalShadowStillMarksTopLevel`.
- [x] **Unify `atan2Operand` with `helpers.ToFloat64`** [Low, S, Done, PR #754]: `atan2Operand` re-implemented the Number-assert → complex-reject → float64-extract sequence just to swap the loss policy from strict to silent-truncate. Extracted shared `screenReal` into `registry/helpers/value_conv.go` and added `helpers.ToFloat64Lossy` as the lossy counterpart to strict `ToFloat64`; `atan2Operand` deleted, both `PrimAtan` sites routed through it. Lossy semantics (`(atan 1/3)`) preserved per R7RS §6.2.6.
### Tech Debt Plan (remaining)

- [ ] **Task 6.2: Replace `context.TODO()` in tests** [Low, S]: 431 occurrences across 39 test files. Mechanical `→ context.Background()`.
- [x] **Task 6.4: Add `typeswitchlint` to value type guide** [Low, S, Done — `a41ec0b7`]: resolved by a mechanism instead of a comment: `typeswitchlint` is opt-in, CI-gating, and drift-guarded (`tools/cmd/typeswitchlint/main_test.go`), so `knownValueTypes` diverging from the value-type set fails CI.
- [x] **Task 8.1: Extract `machine/compilation/resolver/`** [Done]: FileResolver implementations extracted. `LibraryEnumerator` replaced with `FileEnumerator.EnumerateFiles` (returns paths, not `LibraryName`). Type aliases in compilation for backward compat. `memory/2026-04-13-resolver-extraction-impl.md`
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
- [x] **`validate-X` / `assert-X` / `make-X` idioms collapsed to generic helpers** [Medium, S, Done]: `make-violation-reporter` retrofitted across 14 libraries, replacing the `(set! violations (append …))` parent-delegation pattern; `assert-validation` (+1 symbol, not 18 per-structure `assert-X`) keeps the source expression in the error datum; `assert-procedure` covers 11 non-validating `make-X` constructors and names both sides.
- [x] **`combinatorial-graph.scm` monolith — first cut** [Medium, M, Done (partial)]: custom insertion sort replaced by `list-sort` from `(srfi 132)`; 1,787 → 1,726 lines. The remaining `%`-helpers are genuinely WL/isomorphism-specific; splitting into sub-files is deferred until it buys review scope.
- [ ] **Watch `matrix.scm` for split pressure** [Low, S, Deferred]: 1,302 lines with two record types (`<semiring-matrix>` at 839, `<sparse-semiring-matrix>` at 1137) in one file. Shared helpers justify co-location today. Revisit once a third representation (banded, symmetric, etc.) appears — no action needed now.
- [ ] **Harmonize `docs/algebra/reference.md` section template** [Low, M, Deferred; 2026-04-23 crosscheck consistency finding]: First 15 sections use a fixed 5-heading template (Constructors → Predicates → Operations → Validation → Destructuring). The 11 sections added in PR #706 (matrix, polynomial, incidence, interval, graph, combinatorial-graph, unification, fca, pareto, abstract-domain, dataflow) use bespoke headings because their library shapes don't match the 5-part structure pattern (e.g. dataflow has no "law checker"; unification has pattern-vars, substitutions, matching as three parallel concerns). Decision at the time: keep bespoke headings since forcing the template would obscure real structural differences. Revisit if either (a) the template gets extended to cover the new shapes cleanly, or (b) a reader reports navigation trouble across sections.
- [x] **Back-port legacy Sage validators to `check_or_snapshot`** [Low, M, Done 2026-06-09]: 5 of 6 legacy validators route through the shared helper. `powerset-lattice` stays hand-rolled and says why at the function: `lattice-join`/`lattice-meet` return sets in input order, so its live check compares order-insensitively while its snapshot asserts only cardinality, which the single-expression helper cannot express without weakening the membership check. A `('num', token)` sentinel in `to_wile_display`/`to_wile_test_literal` makes rational-field assert exact rationals by `equal?`. Fixtures regenerated under Sage 10.8.

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
- [x] **`read-line` / `peek-char` dropped `UnreadRune` errors and misclassified read failures** [Bug, S, Done 2026-05-06, `460c73a5`]: both use `WrapForeignReadErrorf`, so `(read-error? e)` is `#t` per R7RS §6.11 (it had been `#f`). `io.EOF` after a bare `\r` stays silent. Fault injection in `pkg/internal/extensions/iotest/`; pin `pkg/extensions/io/prim_read_error_test.go`.
- [x] **Library-binding installation swallowed errors silently** [Bug, S, Done 2026-07-01]: two `_ =`-discarded `SetOwnGlobalValue` returns in `machine/compilation/library_bindings.go` now wrap and return, as the sibling base-phase installs already did (a swallowed syntax-binding failure left a macro silently uninstalled). Also guards `targetPhase + sourcePhase` int8 overflow: a `for-meta` target of 127 plus a syntax binding's +1 wrapped to −128 and misrouted the binding. Pin: `TestCopyLibraryBindingsPhaseOverflow`.

### Machine value-register follow-ups (PR #736 deferred items)

Items surfaced by /crosscheck on PR #736 (consolidate value-register
accessors on *vmState — Finding 3 of `memory/2026-05-06-machine-structural-reduction.md`).
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

- [x] **Tier-2: wile-goast capture-site shared-invariant belief** [Tech debt, M, Done, `3ddbe839`, `.goast-beliefs/continuation-capture-marks-shared.scm`]: every function constructing a Captured/Composable continuation must mark the live `mc.cont` chain shared (`MarkChainShared`) before any release path can fire, the `RELEASE_OLD_ENV`/`POOL_FRAME` precondition the field oracle documents but cannot enforce. Both canonical reverts are this class: `RestoreAndRelease` did what the descriptor says while an upstream capture site failed to mark. 5/5 capture sites validated.
- [ ] **Option B — codegen the six save/restore/copy bodies from `contDescriptor`** [Perf/structure, L, Deferred — perf-gated]: The literal "data-driven" half of finding #1. `go:generate` the six method bodies *from* the descriptor so the spec lives in data and the code is emitted, not hand-transcribed — identical runtime (generated Go, not interpreted). **Hard gate:** an end-to-end benchmark proving normal-return-path parity (`memory`: micro-benchmarks mislead; sites #3–#5 are the hot path where table/reflection dispatch loses to a `switch`, and this path is the dominant GC contributor). Promotes `contDescriptor` from a `_test.go` spec to a generator-readable data file — a real restructuring, not a freebie. Do NOT gate #1's drift-catching value on this; Tier-1 already delivered that.

### Internal-SR follow-ups (PR #739 deferred items)

Items surfaced by /crosscheck on PR #739 (internal/ structural reduction
phases 1-5 — Findings 7, 4, 3, 2, 6 of
`memory/2026-05-07-internal-structural-reduction.md`). Deferred per scope.

- [ ] **`*SyntaxObject.Datum()` and `*SyntaxObject.Unwrap()` duplication** [Tech debt, XS, Deferred — pre-existing]: Both methods return `p.datum` with no transformation (`internal/syntax/syntax_value.go:94-96` and `:103-105`). `Unwrap` is the `SyntaxValue` interface method; `Datum` is the historical accessor. Pre-existing; surfaced by but not introduced by PR #739. Fix shape: audit callers (which name does each use?) and delete one. If Unwrap is interface-required, delete Datum or make it a one-line forward; otherwise reverse the choice. Out of scope for the structural-reduction phases; clean-up commit when next touching syntax_value.go.

- [ ] **`qt.Assert(t, ...)` vs `c := qt.New(t); c.Assert(...)` style split in `internal/validate/`** [Tech debt, S, Deferred — pre-existing]: The validate package's test files mix two quicktest invocation styles. Older files (`walk_sub_exprs_test.go`, `validate_capture_test.go`, `validate_escape_test.go`) use the `c := qt.New(t); c.Assert(...)` form; recent additions (`env_helpers_test.go` from PR #739, `walk_binding_refs_test.go` from PR #740) use the package-level `qt.Assert(t, ...)` form. Both are valid quicktest API; the split is purely stylistic. Fix shape: pick one and propagate — likely the package-level `qt.Assert(t, ...)` since it's the more recent precedent and is what other Wile packages use. Out of scope for any one PR.

### Loss-signals API follow-ups (numeric-loss-signals impl)

Items from the numeric loss-signals plan
(`memory/2026-05-14-numeric-loss-signals-design.md` /
`memory/2026-05-14-numeric-loss-signals-impl.md`). Track decisions
that were made on the impl path but warrant revisiting once usage
patterns are visible.

- [ ] **Revisit hybrid return shape if helper set grows** [Tech debt, M, Deferred]: Current API uses a **hybrid** return shape — `ToFloat64WithAccuracy` returns positional 4-tuple `(float64, big.Accuracy, bool, error)`; `ToComplex128WithAccuracy` returns `(Complex128Result, error)` with a named struct (fields `Value`, `RealAcc`, `ImagAcc`). The rule: positional when slot types disambiguate roles; struct when adjacent slots share a type and could be silently swapped. Decision rationale + alternatives (all-positional, all-struct) documented at `memory/2026-05-14-numeric-loss-signals-design.md` § "Decision record: return shape — hybrid (positional + struct)". **Revisit triggers**: (a) a second `WithAccuracy`-shaped helper with a single accuracy signal is added (rationals, intervals, matrix elements with one component) — re-evaluate whether the new helper should follow `ToFloat64WithAccuracy` (positional) or be promoted to struct for consistency with `ToComplex128WithAccuracy`; (b) a third or fourth multi-component helper is added (quaternion, matrix with N≥3 same-type slots) — at that point the asymmetry-as-domain-structure argument weakens, consider a unified struct convention; (c) FFI converter is refactored to consume the struct directly for both helpers (eliminates the discard-idiom advantage motivating positional `ToFloat64WithAccuracy`); (d) a `realAcc/imagAcc` swap bug is reported despite the struct — indicates the safety property failed, revisit whether stricter encoding is warranted (e.g., distinct newtypes `type RealAccuracy big.Accuracy` / `type ImagAccuracy big.Accuracy`).
- [x] **Big-precision numeric sweep: rounding, division, transcendentals** [Correctness, L, Done 2026-07-07; branches `fix/bigcomplex-precision-loss`, `feat/bigcomplex-angle-atan2`, `feat/big-transcendentals`, `feat/big-complex-transcendentals`]: no numeric path round-trips through float64. Rounding and the `floor-`/`truncate-quotient`/`remainder` family take exact `big.Int`/`big.Rat` paths; `pkg/values/big_transcendental{,_complex}.go` adds arbitrary-precision `BigPi`/`BigAtan2`/`BigExp`/`BigLog`/`BigSin`/`BigCos`/`BigAsin` and complex twins, since `math/big` has no transcendentals. `BigSin`/`BigCos` scale working precision to the argument's exponent (the big analogue of Payne–Hanek). **Tier rule**: unbounded-tier input (BigFloat/BigInteger/Rational/BigComplex) yields `*BigFloat`; bounded stays float64, where `math.*` is already correct. Complex kernels run only where `cmplx.*` returns non-finite, so in-range branch-cut behavior is unchanged; `exp` also rescues a bounded operand whose `math.Exp` overflows (~709). Caveat: `BigComplexAtan` on the imaginary-axis branch cut returns `+π/2` where Go gives `−π/2`, reachable only outside float64 range. Pins: `TestRoundingBigPrecision`, `TestBigComplexTranscendentalPrecision`. Designs: `memory/2026-07-07-bigcomplex-angle-atan2-design.md`, `memory/2026-07-07-big-transcendentals-design.md`. Residual: rational-operand remainder still truncates through float64 (`(floor-remainder 7/2 2)` → `1`, exact `3/2`), non-conformant args per R7RS §6.2.6.
- [ ] **Unify `ErrLossyConversion` / `ErrNotAReal` for the imag-drop case** [API design, S, Deferred]: After this PR, two sentinels flag the same underlying condition depending on which surface a caller uses. `ToFloat64Lossless` returns `ErrLossyConversion` (wrapped) for the `!isReal` branch at `values/conversion.go:89-91`; `NumberToFloat64` panics with `ErrNotAReal` at `values/promotion.go:337-338`. The two sentinels carry the same information about the failure but are not interchangeable for `errors.Is` callers. **Options**: (a) reuse `ErrNotAReal` in `ToFloat64Lossless` and reserve `ErrLossyConversion` strictly for rounding loss; (b) accept `ErrLossyConversion` as the canonical sentinel for any precision/component loss and document the historical role of `ErrNotAReal` as a `NumberToFloat64`-only panic discriminator. Revisit when an FFI consumer reports a confusing `errors.Is` mismatch, or when adding a third surface helper that needs to choose between them.
- [ ] **Reconsider `Exact` overload for NaN/Inf identity** [API design, M, Deferred — design choice Q-6]: The `big.Accuracy` slot returned by `ToFloat64WithAccuracy` is overloaded: (a) genuinely lossless rounding, (b) NaN bit-pattern identity, (c) preserved literal infinity. Doc tightening landed in this PR at `values/conversion.go:54-60` (per design Q-6 resolution). Callers screening "is this a meaningful real number?" must use `math.IsNaN` / `math.IsInf` independently. **Trigger to revisit**: a caller reports being unable to distinguish "rounded-but-real" from "NaN-or-Inf" from the tuple alone, OR a fifth `WithAccuracy` helper is added where the overload becomes too costly to maintain. Possible fix: a 4-valued enum `LossKind { Lossless, RoundedBelow, RoundedAbove, NaNOrInf }` replacing `big.Accuracy` at the public surface; would diverge from Go's stdlib vocabulary at the cost of being self-describing.
- [ ] **`ToFloat64Lossless` returns rounded value on error** [API design, S, Deferred]: When the conversion would round, `ToFloat64Lossless` returns `(f, ErrLossyConversion)` where `f` is the lossy float64 result — the caller can use the value if they want; the error is advisory. The nil-input error path returns `(0, ErrNotANumber)`. The asymmetry is real: lossy ⇒ best-effort value preserved; nil-input ⇒ zero. Go convention is "non-nil error ⇒ value is unspecified," which the lossy path softly violates. **Decision deferred**: changing this would force every strict-mode caller to abandon their value on rounding (which they wanted to fail-fast on anyway). Document the contract instead. Revisit if a caller reports relying on the "use the rounded value alongside the error" pattern in a way the API should officially support, OR if the asymmetry causes a bug at an FFI boundary.
- [ ] **`ToFloat64WithAccuracy` nil-defense: error vs panic** [API design, S, Deferred]: The function returns `ErrNotANumber` (wrapped) when `n == nil`. The signature is `n Number`, so a non-Number cannot be passed — the nil case is the only reachable error path. The neighboring `LookupNumericSpec` (`values/numeric_registry.go:163-169`) panics on analogous defensive bugs (out-of-range kind). The split is a style choice: errors-for-FFI-safety vs panic-for-Go-bug. **Revisit when**: (a) the wider codebase converges on one convention for "this should never happen" defensive checks at the `values` boundary, OR (b) an FFI consumer demonstrates a real path where `n == nil` is reachable from outside the type system (unlikely but possible via reflection paths).
- [ ] **File naming: `conversion.go` lacks `numeric_` prefix** [Tech debt, S, Deferred — taste call]: Other numeric-domain files in `values/` use the `numeric_` prefix (`numeric_kind.go`, `numeric_registry.go`, `numeric_tower.go`); test files follow suit (`numeric_dispatch_test.go`, `numeric_lattice_test.go`). The new `conversion.go` / `conversion_test.go` lacks the prefix. Counter-evidence: `promotion.go` is also unprefixed and lives in the numeric domain, so the convention isn't universal. Rename to `numeric_conversion.go` / `numeric_conversion_test.go` if `promotion.go` is also renamed for consistency, or leave both alone. Revisit when a third unprefixed numeric file is added — the convention either solidifies or breaks definitively.
- [ ] **Symbol-singleton location: `symbols_accuracy.go` separate vs co-located** [Tech debt, S, Deferred — taste call]: Prior art for state-symbol singletons in `values/` (`SymbolThreadNew` etc. in `thread.go:54-59`, `SymbolMutexNotOwned` etc. in `mutex.go:30-31`) puts them in the file of the owning type. `SymbolAccuracyBelow`/`Exact`/`Above` live in their own `symbols_accuracy.go`. The split is defensible (the symbols paraphrase `big.Accuracy`, not a Wile type; the natural owner would be `big_float.go` or `numeric_registry.go`, neither of which is a clean fit). Revisit if a third orphan symbol-set appears (then either consolidate all orphans into a single `symbols.go`, or formalize the per-domain-file convention). Update `values/CLAUDE.md` "Sentinel/Singleton Values" inventory either way.

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
  `plans/2026-03-26-extension-contracts-{impl,phase2-design}.md`.
- [ ] **Frame-reclaim precision Phases B/C/D/G** [Perf-precision, PAUSED]: sound-escape sibling (B),
  `OpSelfTailCall` v2 at depth>0 (C), local-recursion release for named-let/letrec (D),
  quasiquote unquote-aware reject (G). Value core (A/E) shipped; arc paused 2026-06-22, resume
  gated on a workload profiling as frame-leak-bound. Superset of the "B3 effective capture" item in
  Tier 4. `plans/2026-06-18-frame-reclaim-precision-coverage.md`.
- [ ] **`iter.Seq` Tier-2 defensive-copy accessors** [Tech debt, M]: 10 steps, sequenced after a
  charsets structural refactor ships. `plans/2026-05-05-iter-seq-cascade.md`.
- [ ] **A cell-yielding form of the canonical list walker** [Refactor, S–M, **deferred on YAGNI —
  needs a second call site**, filed 2026-08-09]: `values.ForEachFunc` yields the ELEMENT, so a
  caller that needs the CELL (the sublist starting at the match) has to advance a shadow cursor
  in its own closure — `MemberLookup` does exactly that, and the failure mode of getting it wrong
  is a **wrong sublist, not an error**. A cell-yielding form would give `memq` the canonical
  traversal (context poll + Brent cycle detection) *and* the cell, with no shadow cursor.
  Structurally the right answer; one call site today, and YAGNI says no new iteration shape until
  there are two. **Not swept for a second** — `list-tail`, `list-set!` and `PrimAppend`'s tail
  re-point are where to look. Recorded by review wave 4 §8 item 5 as shape (c), out of scope for
  that phase.

  > **RECONSTRUCTED 2026-08-09 and not verbatim.** The original wording of this row was lost to a
  > `git checkout master -- .` that overwrote uncommitted work; only its first two lines survived.
  > The substance above is rebuilt from `memory/2026-08-07-review-wave4-embedder-contracts-impl.md`
  > §8 item 5's "shape (c)" paragraph, which is what the row was filed from. Check it against your
  > intent before trusting the phrasing.
- [ ] **Staff-sweep structural residuals** [Refactor, M]: `CompileTimeContinuation` God-object,
  `engine.go` facade, "N parallel tables" (ValueType ×5, docparse ×3), parser cluster. The `[S]`
  findings + Tier-1 already shipped. `plans/2026-07-01-staff-engineer-sweep.md`.
- [ ] **Complexity inventory (measured)** [Refactor, unscheduled]: cognitive+cyclomatic complexity over
  all non-test functions, ranked by *density* not size. Four candidates, none started:
  `MatchSyntaxWithLiterals` (`match.go:303`, cog 155, nesting depth 6 — the worst genuine function in
  the tree), `CompileTimeContinuation` (**131 methods / 21 fields / 26 files**, the method count the
  staff sweep's `[L]` row lacked), the syntax-rules/quasiquote cluster, `fuseCallForeignCached`.
  **Read §3 first:** `MachineContext.Run` tops every raw metric (cog 305) and is **disqualified** —
  extract its 78 arms and the total collapses to 98, 77/78 arms scoring under 5. Wide ≠ complex, and
  §5 files the eight other non-candidates so they aren't re-derived. No forcing goal, so §0 requires
  each item to pay rent before execution. Two corrections are recorded in the plan rather than
  buried: C1's original "no dispatch switch" justification was **false** (it has an 18-arm switch,
  70% of its score; it survives on the 4 arms still scoring ≥5), and the `-arms` verdict itself had a
  defect that called `cmd/wile/main.go:main` wide off a switch worth 1% of it. Measurement tool at
  `tools/cmd/cxmeasure` (`make complexity`, `make complexity-arms ARMS=…`), a reporter not a gate.
  C1 is also the named blocker on the `NEST_MAX=6` ratchet in `tools/Makefile`: dropping it to 5
  needs that function flattened. `plans/2026-08-22-complexity-inventory-refactor.md`.
- [x] **Dead exported-surface census: `tools/cmd/deadscan`** [Chore, Done 2026-08-29]: a reporter, not a gate (`make deadscan`, `make deadscan-json`), because `deadcode` does not run on this toolchain (built against go1.26, tree targets go1.27) and `unused` skips exported identifiers. The analysis is the importable `tools/deadscan` package (`Load` -> `Result`) so wile-goast can join it against call graph and CFG; `tools/cxmeasure` split the same way. Four PINS a reference count cannot see (a `var _ I = (*T)(nil)` assertion, an interface owned outside the module, the universe `error` protocol, anonymous-interface dispatch) make a symbol a live ROOT; rows carry `iotagroup`/`clusterwith` because iota members and leaves reachable only from dead symbols are not standalone deletions. At filing: 4207 exported, 385 dead, 353 standalone / 2212 LOC; an earlier hand-run's 462 dead / 149 "safe to remove" was four defects, each with an instance in `memory/`. **Pass every workspace module** or the ext column is zero and the list is overstated.
- [x] **Tool layout: `tools/cmd` + `tools/Makefile`** [Chore, Done 2026-08-22]: every Go tool `main` lives under `tools/cmd/` (`cxmeasure`, `nestinglint`, `typeswitchlint`, `singlelinefunclint`); `tools/Makefile` owns their invocations and the root delegates recursively (not `include`d: `build`/`test`/`clean` mean different things in each). `TOP=`/`NEST_MAX=` are defaulted in **both** makefiles on purpose: a bare `TOP=` on a sub-make command line overrides its `?=` with the empty string. Gates run from `$(ROOT)` with `.` so findings cite `pkg/foo.go:42`. The `tools/sh` delegations stay in the root Makefile because `ci` and `cd` depend on 4 each.
- [ ] **Unscheduled design-only notes** [Refactor/architecture]: engine-services generic keyed slot
  (`plans/2026-07-10-engine-services-generic-keyed-slot-design.md`), layered-environment
  architecture direction (`memory/2026-06-13-layered-environment-architecture.md`),
  data-driven promoted-primitive inline registry
  (`plans/2026-06-26-promoted-primitive-inline-registry.md`, DRAFT v2 awaiting review).
- [ ] **`docs/` audit sweep** [Docs/verification]: the docs-subsystem sweep
  (`plans/2026-04-23-docs-sweep-impl.md`, follows algebra-docs). The §4 review audit AU.1
  that shared this row was run 2026-08-09 with a negative verdict, the plan's own header says all
  23 items are closed, and it is archived: `memory/2026-07-15-review-2026-07-13-sec4-remediation.md`.

> **Stale-status housekeeping (planlint evidence), DONE 2026-09-04.** `2026-07-17-review-remediation.md` + `-impl.md` and `2026-07-12-numeric-zero-and-tier2-fold.md` showed unchecked boxes while fully shipped; all three archived to `memory/` in the 2026-09-04 pass with their Plan Index rows, alongside eight whose headers said shipped and the lingering rows of three earlier archives (r6rs hashtables, ambient keywords, literal-pin ambiguity).

### Closed in Tier 5

Fully closed sections, collapsed to the archive form on 2026-09-15 and moved below the
open work. Headings kept so old references still find them.

#### List/Pair Primitive Cleanup (from inline annotations)

- [x] **List/pair primitive cleanup** [Low, XS–S, Done]: relocated from inline `// CLAUDE:` source annotations, removed because inline comments shift primitive line numbers and break `TestBuildAxisBManifest`. `(*Pair).Append` and the `Tuple.Append` interface method deleted as dead (superseded by `PrimAppend`). `PrimListCopy` moved to `PairBlock`, reversing the earlier tail-pointer choice: `Tuple.ForEach` yields the count and the terminating cdr, so an improper tail survives by re-pointing the block's last cdr after `LinkSpine` (63→18 allocs, ~10% faster on 50 elements). `PrimAppend` stays the sole tail-pointer exemplar, since multi-arg concat cannot pre-count.

#### `syntax-rules` derived over `syntax-case`: candidate (2026-09-04)

- [x] **Derive `syntax-rules` in Scheme over `syntax-case`** [Macro system / dedup, M, Done, superseded 2026-09-04 by `plans/2026-09-04-scheme-specified-syntax-forms-design.md`, which makes this its P2 and adds `syntax-case` itself]: kept for the sizing. The R6RS definition (~40 lines, core forms only) at the top of `pkg/registry/core/bootstrap_macros.scm` retires `operation_syntax_rules_transform.go` (358) and the syntax-rules-only parts of `compile_syntax_rules.go` (934; `collectFreeIdentifiersWithEllipsis` and `collectPatternVariablesWithEllipsis` are shared with `syntax-case` and `(syntax …)` and stay). Template instantiation is already one path (`match.SyntaxMatcher.Expand`). Two gates: (1) reliability inversion, since syntax-rules carries 13 archived fix plans and syntax-case had silent failures as late as PR #732, so verify use-site scopes (Design B) and pattern-variables-as-bindings (Design C) apply on the syntax-case path; (2) startup, one closure call + fender per clause per derived-form expansion against ~4.3 ms / 159k allocs, interleaved A/B on `BenchmarkEngineStartup`. `plans/2026-09-04-expander-in-scheme-assessment.md` §2.

## Tier 6 — Nice-to-Haves

No demand signal. Speculative or research-only.

### Tooling
- [ ] **Hygiene debugging** [Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Programmatic tokenization/parsing**: Expose tokenizer/parser to Scheme. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Event callbacks**: Hooks for expansion, compilation, debugging. IDE integration, profiling.
- Update skills to explicitly state where wile-goast is a fit for refactoring.  Add guidance.
- Add guidance to skills where Serena use makes sense.

### Standard Library
- [x] **Hashtables → R6RS `(rnrs hashtables)`** [Done]: SHIPPED on `feat/r6rs-hashtables`. The spine was one inversion: the hash moved from the KEY (`values.Hashable.HashCode`, which gated admission) to the TABLE (`values.HashtableKind`, a write-once discriminant selecting one of three (hash, equality) pairs), which makes pair, vector, record, and nested-table keys legal. Findings the plan did not predict: (1) the first cut broke its ~10% gate (`Get/symbol/n=10` +76.5%); a `hashKey` leaf fast path and `keyEqual`'s default arm spelled `a.EqualTo(b)` restored flat-or-better except `Get/{symbol,string}/n=10` at +17.4%/+12.8% (~2ns/lookup), accepted. (2) `string-ci-hash` ships in `pkg/internal/extensions/all` beside `string-ci=?`, which folds with x/text `cases.Fold` (FULL folding, `(string-ci=? "ß" "SS")` is `#t`); go.mod confines x/text to internal packages, and a `strings.ToLower` stand-in would break on `"ß"`. (3) `hashtable-update!` has its own bootstrap procedure source: the mutable map fragment is SWAPPED for a mutation-free variant, this one has none and must be DROPPED, and separability keeps `WithoutCategory("hashtables")` working (it already fails on master for vectors/strings/pairs/lists). (4) **Trap**: `(make-hashtable equal-hash equal?)` raised after an import, and the cause is not that an import copies the binding (`installImportedBinding` copies the value by reference). A library env is a flat island (`NewChildRuntime` parents it at nil), so the env factory mints a second `*ForeignClosure` per primitive there and pointer recognition of the pair saw two objects; `(import (scheme base))` alone broke it. FIXED by identity-token recognition (`machine.PrimitiveIdentity`), shared by every copy of a primitive, which fails CLOSED where a `.Name()` compare would fail open; the export list is free to grow back. (5) `(rnrs hashtables (6))` resolves; the version is parsed and dropped. (6) the SRFI-13 shadowing guard moved to `pkg/wile` (`pkg/registry/core` has no library registry). Also retires `graph.scm`'s atomic-key restriction entirely, so a pair-keyed graph reaches the Go counting kernel. `docs/reference/r7rs-differences.md` items 12–15; `memory/2026-08-03-r6rs-hashtables-design.md`, `memory/2026-08-03-r6rs-hashtables-impl.md`, `memory/2026-08-03-r6rs-hashtables-baseline.md`.
- [ ] **`assert` is bound by nothing, at any strictness level** [XS]: surfaced by the level-2 stdlib export sweep (`WithoutAmbientBindings`, 2026-08-04), where `(import (scheme base))` restores 17 of 18 probed forms and `assert` is the miss. It is **not** a level-2 gap: `assert` fails identically on a non-strict engine, no `(scheme …)` library exports it, and no bootstrap macro or primitive defines it. It is R6RS (`(rnrs base)`), absent from R7RS-small, so the omission is conformant and this is a feature request, not a defect. Cheapest shape is a bootstrap macro raising an error naming the source expression — the `assert-validation` idiom under Algebra library consistency already does this at the Scheme level and is the model. No demand signal; filed so the next sweep does not re-derive it.
- [ ] **`setRecognizedPrimitive` still reads the sealed base, which is empty at level 2** [S]: `pkg/registry/core/prim_hashtables.go` resolves a name through the mutable top level and falls back to `runtime.Namespace().SealedBase()`. Under `WithoutAmbientBindings` that fallback is empty, so `hashtable-equivalence-function` answers only when the program has `equal?` bound under **that spelling**. A renaming import (`(rename (scheme base) (equal? my-eq))`) already defeats it on a non-strict engine, so level 2 widens an existing window rather than opening one. It is not an identity bug — identity already DECIDES (`recognizedBinding` checks the token and discards a same-named shadow); the name only LOCATES a candidate, and level 2 removes the environment that made a name-keyed probe reliable. A real fix needs a canonical `*ForeignClosure` reachable from a `PrimitiveIdentity` without going through any environment, which is the un-copying item's territory (see "A primitive has one identity per environment" under Tier 2). Last sealed-base reader in the tree.
- [ ] **Logging library**: Levels, structured output, handlers.
- [ ] **Go AST Phase 3 — Comments & generics** [S]: `Comment`/`CommentGroup` attachment, `BadExpr`/`BadStmt`/`BadDecl` error recovery, `IndexListExpr` for generics. Owned by [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans); the former `plans/GO-AST.md` moved with it.
- [ ] **`continuation-mark-set-first` accepts `#f` for mark-set** [XS, Racket-compat]: Racket lets `#f` stand in for "current continuation's marks" as the first argument; Wile's `PrimContinuationMarkSetFirst` (`registry/core/prim_cont_marks.go:54`) hard-requires `*machine.ContinuationMarkSet` via `RequireType`. One-branch fix: check `values.FalseValue` before the type check and substitute `mc.CollectContinuationMarks(machine.DefaultPromptTag)`. Surfaced by the audit findings crosscheck on PR #673; no demand signal yet. Defer until the audit's Phase 4 (axis C — Racket compliance sweep) or a real consumer asks.

### Core Language
- [ ] **Type system**: Covers base types, expandable. Discover useful type properties. Types as distinct top-level concept.
- [ ] **let-syntax*** [S]: Implement `let-syntax*`.
- [ ] **Scribble-style `@` reader notation** [Reader extension]: Racket-style at-expressions for rich documentation markup. `@cmd[datum ...]{text ...}` desugars to S-expressions.

### Architecture
- [~] **Dialect system** [In progress]: forms layer SHIPPED (SP1 per-engine codegen fork, `WithDialect`, `DefaultDialect`). Primitive-level control SHIPPED (`PrimitiveRemover` + `BootstrapProcedureRewriter` capabilities; `NoMutation` removes all 21 destructive primitives genuinely, keyed off `PrimitiveSpec.Mutates` rather than the `!` spelling — mutating `vector-map`/`string-map` swapped for a mutation-free bootstrap fragment; inline-HOF optimizer gated on `requires` so removal deopts cleanly). `NoMutation` is the one shipped leaf dialect; it exercises the forms seam (removes `set!`) plus both cross-ceiling capabilities. NoMutation import-reexpose remains a documented language-surface boundary (dialect ≠ sandbox), not a gap. The demo leaves `R5RSStrict` and `R7RSMinimal` — and the `DisableExpandForm` expander gate that only R5RSStrict used — were pruned once no product consumer wanted restricted-surface engines; the seam + `NoMutation` remain. `plans/ARCHITECTURE.md`
- [~] **`FormSpec.Expand` consolidation** [P1 shipped, rest deferred]: `primitive_expanders_registry.go` and `syntax_compilers_registry.go` look like a phase split and are not — they split by handler *signature*, and phase is a constant column with one value per table. The genuine gap: `forms.FormSpec` carries `Validate`+`Compile` but no `Expand`, so compile dispatch reads the forms registry while expand dispatch reads the environment. **P1 shipped** (`48f6fa25`): `RegisterValidator`/`RegisterCompiler` route through one copy-on-write `update(name, mutate)`, so field preservation stops costing a line per registrar per field. **P2–P5 deferred as marginal** — the motivating `fr.Remove` expander leak was already closed by the `compileTimeHandler` marker (`compile_time_continuation.go:452`), and package layering (`validate` and `machine/compilation` both register into `forms`, which imports neither) caps the outcome at one record populated by three passes, never one table. Wiring expand *dispatch* to the field rebuilds the pruned `DisableExpandForm` gate (see Dialect system above) — do not, absent a named consumer. `plans/2026-08-03-formspec-expand-consolidation-sizing.md`
- [ ] **Plugin shadowing** [Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined.
- [ ] **User labels/tags for FS resolvers**: Distinguish bootstrap from include/library loaders in fileResolver.
- [ ] **Important refactoring**
    - When few fields are referenced from a struct within a function, pass in the field - do not pass in the struct or a reference to the struct

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

- [x] **Promoted op table**: Table-driven dispatch regressed ~1.5% geo mean (15/16 Gabriel benchmarks slower). Go compiles contiguous-integer switches to jump tables; table-driven adds overhead. `memory/2026-04-05-structural-reduction.md`
- [x] **PrimitiveSpec dead fields (D1)**: Originally 5%/2% usage. Extension contracts Phase 1 populated both broadly. No longer dead.
- [x] **ForeignClosure redundant fields (D2)**: `doc` duplicates `PrimitiveSpec.Doc` but costs ~3.2KB total, set once, cannot diverge. Removing requires circular import workarounds.
- [x] **Namespace root/child state waste (D3)**: ~6 nil fields in children. Not worth splitting — zero-value costs nothing, children are rare.
- [x] **LocalIndex / BindingID overlap (D4)**: `LocalIndex` is relative (slot+depth), `BindingID` is absolute (frame pointer + slot). Both needed.
- [x] **Binding/BindingMeta (FCA)**: Clean lazy-initialization pattern. FCA false positive.
- [x] **PrimitiveRegistration/PrimitiveSpec (FCA)**: Orthogonal concerns properly separated. FCA false positive.
- [x] **CompileTimeCallContext (FCA)**: 2-field value type parameter, not coupling. FCA false positive.
- [x] **Opcode resource limits**: Per-category limits (match steps, expand steps, continuation copy depth). Existing mechanisms sufficient: `WithMaxCallDepth` bounds recursion, `WithMaxStackSize` bounds stack growth, `context.WithTimeout` checked every 1024 ops in VM and match loops. Deterministic per-category budgets are niche; timeout is an adequate proxy.
- [x] **Core macro expander in Scheme (psyntax / Racket style)**: declined 2026-09-04. Wile has no image format (the stdlib recompiles on every `wile.New()`), so a Scheme-hosted expander needs template serialization first or a second, Go, bootstrap expander; expansion would run as bytecode on the embedding product's headline metric, engine startup; `*Binding` handles held from Scheme need a frame-layout change in `pkg/environment`; coverage, debugger, and provenance would see expander frames. Not smaller either (psyntax ≈10k, Racket >30k lines vs ≈5.6k Go core). The extension benefit is already served by `WithDialect` + open `FormSpec.Compile` and the registered procedural surface. The middle path, a small Go kernel with every `syntax-*` form specified in Scheme, is the live design (`plans/2026-09-04-scheme-specified-syntax-forms-design.md`); it is not this proposal. `plans/2026-09-04-expander-in-scheme-assessment.md` §3.

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
- [x] **Peephole optimizer double-restore** [Fixed]: `savedCont` pointer-identity guard. `memory/OPTIMIZER-FIX.md`
- [x] **Degenerate form pipeline tests** [Done]: Full-pipeline tests for all core special forms. PR #571.
- [x] **Sub-context winding stack inheritance hazard** [Fixed]: Constructor parameter requirement. `machine/machine_context_subcontext.go`.
- [x] **`cond-expand (library ...)` bypasses FileResolver** [Fixed]: `machine/compilation/features.go`.
- [x] **syntax-rules ellipsis and hygiene bugs** [Fixed]: Three bugs — scope-aware duplicate binding detection (PR #607), cross-group ellipsis zipping, nested ellipsis depth tracking (PR #606).

### Refactoring
- [x] **`WalkSubExprs` for validated expression traversal** [Done]: `ChildRole` enum, B1 capture analysis migrated.
- [x] **Extract interface types from `environment/` `any` fields** [Done]: 15 type assertions removed across 7 files. `memory/2026-03-31-environment-any-fields.md`
- [x] **`Stack.Pull()` O(1) replacement** [Done]: `PullDrain()` in `OpPullApply`. `memory/2026-03-31-pulldrain-design.md`
- [x] **Split `ffi.go` by concern** [Done]: 1010 lines → 4 files. PR #599.
- [x] **Engine initialization order invariant** [Done]: 6-step DAG documented. `memory/2026-04-01-engine-init-order.md`
- [x] **`machine/` mega-package decomposition** [Done]: PRs #592, #593. `memory/2026-03-30-machine-decomposition-design.md`
- [x] **`file_resolver.go` chain of responsibility** [Done]: 541 → 469 lines.
- [x] **Timing-dependent concurrency tests** [Done]: PR #602. `memory/2026-04-01-timing-dependent-tests.md`
- [x] **ExpanderTimeContinuation convention deviations** [Done]: 18 deviations fixed.
- [x] **Opcode metadata consolidation (D5)** [Done]: `OperandKind` enum. `memory/2026-04-05-structural-reduction.md`

### Tech Debt
- [x] Task 1.1: `uint16` source table index overflow → `uint32`
- [x] Task 1.2: Opcode round-trip exhaustiveness test (already existed)
- [x] Task 1.3: Extension list consistency test (already existed)
- [x] Task 1.4: Eval stack size limit — `WithMaxStackSize(n)`. `memory/2026-04-11-eval-stack-limit-design.md`
- [x] Task 4.2: Security gate integration tests (already existed)
- [x] Task 5.1: `NamedCallable` interface
- [x] Task 5.2: `StringOrFalse` helper. PR #609.
- [x] Task 5.3: `ForEachList` for proper-list enforcement. PR #609.
- [x] Task 5.4: `requireSourceContext` helper. PR #609.
- [x] Task 5.5: `RequireArg[T]` migration (5 sites, 3 intentional deviations). PR #609.
- [x] Task 6.1: Delete `runtime/` package
- [x] Task 6.3: Receiver naming normalized. PR #609.
- [x] Task 7.1: Unified `machine/testutil` into `registry/testhelpers`. PR #609.
- [x] Task 8.3: REPL decoupled from `machine/compilation`. PR #639. `memory/2026-04-11-repl-decoupling-design.md`
- [x] Task 8.5: `prim_eval.go` funneled through `NewSubContext`. PR #637. `memory/2026-04-11-eval-subcontext-design.md`

### Performance
- [x] **GC pressure reduction** [Done]: -8.9% geo mean. PRs #562-563. `memory/GC-PRESSURE-REDUCTION.md`
- [x] **Core-let compilation** [Done]: PR #570. `memory/CORE-LET-IMPL.md`
- [x] **Procedure inlining** [Done]: PR #605. `memory/PROCEDURE-INLINING.md`
- [x] **B2 escape analysis** [Done]: PR #604. `memory/ESCAPE-ANALYSIS.md`

### Features
- [x] **Algebra library** [Done]: `(wile algebra)`. 158 tests. `memory/2026-03-25-algebra-library-design.md`
- [x] **`(wile algebra polynomial)` library** [Done]: ring-parameterized univariate polynomials; `polynomial-ring` is itself a ring, so R[x][y] recurses. Formal derivative is characteristic-safe; divmod requires a field. Commits `69b98203`..`78bb7e2f`. `memory/2026-04-18-polynomial-library.md`
- [x] **`(wile algebra matrix)` library** [Done]: semiring-parameterized, Path D (sparse/dense reps, dispatch-table rep-tags, bang-first arithmetic, aliasing enforcement). PRs #684–#691, #695, #696. `memory/2026-04-20-algebra-matrix-impl.md`, `memory/2026-04-21-matrix-path-d-impl.md`.
- [x] **`(wile algebra incidence)` library** [Done]: Möbius/incidence algebra on locally-finite posets (Rota 1964), unifying direct-vs-transitive handling across four wile-goast posets. Commit `4ff8a314`. `memory/2026-04-21-incidence-algebra-impl.md`.
- [x] **`(wile algebra unification)` library** [Done]: AC-matching and AC-unification (Eker/Stickel/Contejean–Devie); `ac-unify` returns a CSU, since AC is finitary, not unitary (Fages–Huet 1986). PR #698. `memory/2026-04-21-ac-matching-design.md`, `memory/2026-04-21-ac-matching-impl.md`.
- [x] **SRFI-14 + `(wile charsets)`** [Done, including the Track A4 completeness follow-up]: inversion-list char-sets from Go's `unicode` tables, fully immutable; char-set criteria across seven SRFI-13 procedures; zero-arg identities, cursors, `char-set-hash`, `char-set-diff+intersection`. Residual: the surrogate construction-time invariant (6D) is deferred; only iteration skips U+D800–U+DFFF. `memory/2026-05-04-srfi-14-design.md`, `memory/2026-05-04-srfi-14-impl.md`.
- [x] **SRFI-13 + `(wile strings)`** [Done]: 60 procedures plus five extras, all pure Scheme; `(wile strings)` resolves the SRFI-13 vs R7RS `string-map` clash with `(except (scheme base) string-map)`. Deferred: FFI promotion (profile-driven, design §6) and `string-titlecase`, `string-hash`, `string-unfold`, `xsubstring`, `*/shared` (design §11). PR #721. `memory/2026-05-03-string-primitives-design.md`, `memory/2026-05-03-string-primitives-impl.md`.
- [x] **Documentation system** [Done]: Full infrastructure — `,doc`, `,apropos`, `,topics`, `,topic`, library descriptions, docstring examples. PRs #579-591.
- [x] **MCP server** [Done]: `wile --mcp`. PR #588. `memory/2026-03-26-wile-mcp-server-design.md`
- [x] **`(available-libraries)` primitive** [Done]: PR #590. `memory/AVAILABLE-LIBRARIES.md`
- [x] **OpaqueValue type** [Done]: Generic opaque wrapper for Go objects in Scheme.
- [x] **Disassembler** [Done]: `(disassemble proc)`, `,dis`, MCP tool. PR #603.
- [x] **Go AST Phase 2** [Done]: 13 node types. PR #480. Plan moved to [wile-goast](https://github.com/aalpar/wile-goast/tree/master/plans).
- [x] **Climbing macro tower — Tier 1** [Done 2026-07-10, `f92568a8`, branch `feat/climbing-tower-tier1`]: a phase-*N* transformer body that defines and uses macros climbs to *N+1*, … via `EnvironmentFrame.NextPhase()`; phase 0 is byte-identical to pre-tower. **It affects procedural macro-writing macros only**: declarative inner macros live in expansion output, at the use phase, so the plan's corpus is green with and without the tower. Bindings are shared across phases, not instantiated per phase (Tier 2, gated). (Q4 mutation boundary, `ErrCrossPhaseMutation`, deferred: mutation reachability is a whole-unit validator property not queryable at the cross-phase resolution site, and a flag approximation false-positives on the unmutated `jabberwocky`/`march-hare` carve-out. Ships as silent share, not a regression; natural home is the design §7.3 phase-precise use-time resolution rework. `memory/2026-07-10-climbing-tower-q4-mutation-boundary-note.md`.) `docs/compiler/macro-system.md` §Phase Tower. `plans/2026-07-10-climbing-tower-design.md`, `memory/2026-07-10-climbing-tower-impl.md`.

### Other
- [x] **Promote `eval` extension to public** [Done]: Moved `internal/extensions/eval/` → `extensions/eval/`, importable as `github.com/aalpar/wile/extensions/eval`. Required by wile-goast and any embedder wanting sandboxed `(eval ...)` / `(load ...)`. The naive composition `WithProfile(Console) + WithExtension(eval.Extension)` does **not** work — `ConsoleAuthorizer` denies `code:load`, so `(load ...)` fails. The fix is a baked `ConsoleWithLoad` profile (extensions + matching authorizer that allows `code:load` under `/tmp`), now part of `memory/2026-03-26-environment-profiles-impl.md`.

- [x] **Reader fixes: `#z`/`#m` as datum introducers, readable boxes, radix floats, precision exponent markers** [Done 2026-07-31, branch `feat/reader-hash-dispatch`]: the rule is the `#` Reader Dispatch invariant in `CLAUDE.local.md` (detail in `pkg/internal/tokenizer/CLAUDE.local.md`, `pkg/parser/CLAUDE.local.md`); pinned by `pkg/parser/{bigint_radix,box_read,float_radix,precision_marker}_test.go`. Decisions D1-a, D2-a, D3-a. `memory/2026-07-31-reader-hash-dispatch-model.md`, `memory/2026-07-31-reader-hash-dispatch-impl.md`.
  - **Prerequisite the design missed**: an out-of-radix digit was a token boundary (`(#b19)` read as `(1 9)`), which made both digit-validation tests unreachable. A radix-prefixed numeral now requires a delimiter; unprefixed numerals still split (open item under Tier 1).
  - **Four defects fixed along the way**: `#e#x1.8` was 9/5 while `#x#e1.8` was 3/2 (`#e` now defers a non-decimal literal to the value, `makeExactLiteral`); `#x.f` scanned as the identifier `.f`; `read()`'s leading-dot arm leaked a radix, so in `(#x.8 19)` 19 read as 25; `#0=#&#0#` was an undefined label (`readLabelAssignment` now pre-registers a box placeholder).
  - **Deliberate behaviour changes**: `#x1f#t` is an error (`1f#` is an R7RS §7.1.1 inexact-digit token, leaving `t` adjacent); a `BigFloat` writes `l` (`1e+1000` → `1l+1000`, `#m1.5` → `1.5l0`), marker omitted inside a complex, pinned as the property `TestBigFloatWriteReadRoundTrip`.
  - **Divergences, chosen**: `#&` must read behind a datum label because Wile's writer emits `#0=#&…` (Racket accepts, Chez does not; matching Chez leaves Wile's own output unreadable). `l` selects a different representation, which no reference Scheme does. **Exponent markers stay decimal-only**: `#x1e2` is 482 (R7RS §7.1.1), where Racket, Chez and MIT read `#x1s3` as 4096.0 with a radix-based exponent. Radix floats are an extension (R7RS defines `⟨decimal R⟩` only for R = 10).
  - Not adopted, recorded: R6RS mantissa width `x|p` (Chez and MIT), which maps onto `big.Float`'s precision parameter more directly than a four-letter code and would complement `l`.

</details>
