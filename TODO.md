TODO
----

**Last Updated**: 2026-10-17: split into `TODO.md` (hub) plus `todo/*.md` (one file per tier, plus
`exceptions-and-rejected.md` and `closed.md`); every row moved verbatim, see **Layout** below. Previous: 2026-10-16: every open row and the Plan Index re-verified against `956604f0`
(repros rerun, citations resolved, fixing commits found): 13 rows closed that had already shipped,
the `if` section and Top Priority table updated, drifted citations repointed, two unindexed plans
added, five archived plans dropped from the index (citations repointed to `memory/`). Previous: 2026-09-20: full `plans/` sweep (72 files) plus `WORK.local.md`; items not
previously filed added under the three `plans/ sweep … (2026-09-20)` headings in Tiers 1, 4 and 5;
the three `Redefining if` entries rewritten with **verified** mechanisms (the 2026-09-19 hypotheses
were wrong on both counts); priorities restated against the four current design goals. Previous:
2026-09-15, closed items collapsed to the archive form again (the 2026-07-27 pass, `2187a734`, had
left the file at 167 KB; it had regrown to 476 KB); `plans/` and `memory/` citations repointed after
the 2026-09-14 `.local.md` → `.md` rename. Per-edit history is `git log -p TODO.md`; release history
is [`CHANGELOG.md`](CHANGELOG.md).

## Layout — where the rows live (split 2026-10-17)

This file is the hub: status, ordering, conventions, the Plan Index (the index into `plans/`)
and the Top Priority table. Every work row lives in exactly one file under `todo/`, split by
the tier it was filed under. Cite a row by its bold text, never by line number (the line
anchors cited by older plans were already untrustworthy; see fault C1 in
`plans/2026-09-22-defect-closeout-audit.md`). `make indexlint` and `make planlint` read this
file and `todo/*.md` as one corpus. Per-edit history before the split is `git log -p TODO.md`;
after it, `git log -p todo/<file>`.

| File | Holds | Open | Closed | Lines |
|---|---|---|---|---|
| [`todo/tier1-security-correctness.md`](todo/tier1-security-correctness.md) | Conformance, hygiene, binding identity, phases, reader; blocks production embedded use | 57 | 78 | 2016 |
| [`todo/tier2-embedding-api.md`](todo/tier2-embedding-api.md) | Embedding API, algebra & analytics roadmap, feature deltas | 33 | 20 | 145 |
| [`todo/tier3-tooling-dx.md`](todo/tier3-tooling-dx.md) | Linter, tooling, developer experience | 5 | 5 | 12 |
| [`todo/tier4-performance.md`](todo/tier4-performance.md) | Measured perf levers; unboxed arithmetic is the largest | 12 | 8 | 133 |
| [`todo/tier5-tech-debt.md`](todo/tier5-tech-debt.md) | Simplification, dedup, crosscheck-filed residuals, postponed items | 85 | 54 | 1213 |
| [`todo/tier6-nice-to-haves.md`](todo/tier6-nice-to-haves.md) | Unranked wants | 17 | 4 | 50 |
| [`todo/exceptions-and-rejected.md`](todo/exceptions-and-rejected.md) | Documented R7RS exceptions, rejected proposals, the FCA coupling assessment | 0 | 10 | 82 |
| [`todo/closed.md`](todo/closed.md) | Archive form of closed rows; read for traps and deciding measurements | 0 | 90 | 808 |

Rows are ranked by the four goals under **Ordering** below, not by file; the tier files are
the older, coarser axis kept because rows are cited by tier.

---

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

**Four design goals, in rank order (restated 2026-09-20).** Everything below is ranked against
these; the tier headings are the older, coarser axis and are kept because the file is cited by
tier, not because they outrank this list.

1. **Conformance bug fixes** — a measured divergence from R7RS, Racket, Chez or Chibi. Ranked
   first because a silent wrong answer costs more than anything else here. Wile's own Scheme
   failing to be portable counts.
2. **Phase separation and load-order independence** — a unit's compiled output must depend only on
   its own source and its declared imports. The largest single body of work
   (`2026-07-10-climbing-tower-design.md` Tier 2) and the only one that is gated on a decision
   rather than on effort.
3. **Implementation simplification** — deduplication, dead-code deletion, structural reduction.
4. **Performance** — measured, interleaved A/B, against a named baseline.

Off-rank: embedding-API surface, tooling/DX, and feature work, which keep their tiers below.

Tiers: Security/Correctness → Embedding API → Tooling/DX → Performance → Tech Debt → Deferred →
Nice-to-Haves. Rows live in `todo/` (see **Layout** above); closed rows in archive form are in
`todo/closed.md`.

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

Every file in `plans/`, with the status recorded in the file itself. The `todo/` files hold the
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
| `2026-07-28-alocal-capture-safe-proof-for-locals.md` | Phases 1–4 **DONE** (1–3 committed on `feat/callback-conditional-capture`); 19/68 → 42/68 capture-safe stamps, reclaim verdicts unchanged at 19/68 as predicted. Phase 5 (Lever B) open. Prerequisite `47c27f72` landed directly on master, 2026-07-28 |
| `2026-08-01-scheme-deadline-and-cancellation-cause.md` | Phase 1 **impl-ready**; Phase 2 designed and **gated** on the plan's scoping decision. Exposes the ambient `context.Context`'s deadline and cancellation cause to Scheme |
| `2026-09-20-defect-closeout-design.md` | Design behind the defect-closeout arc; status lives in the `-impl` hub and phase files |
| `2026-09-22-defect-closeout-impl.md` | **HUB** of the defect-closeout arc, derived from `2026-09-20-defect-closeout-design.md`. **Split into 11 files 2026-10-02** (one per phase, plus the audit) because the single file had reached 5,220 lines; task IDs, branch names and every word unchanged. Carries the method, the phase map and the first branch. **48 of 97 tasks landed**: P1 all ten, P2 all five, P3 all eighteen (P3.6 `c83e32c1`, P3.14 `dbb82082`, P3.12 `455ee5ed` and P3.15 `921a53ea` merged 2026-10-03; P3.16 `f94806a8` merged 2026-10-04; P3.17 `4ea9b5b5` and `231d3d95` merged 2026-10-10; P3.18 `24e3b23e`, reduced to a delete, merged 2026-10-14), P4.1 (`669c7b23`, merged 2026-10-14), P4.2 (`3721b540`, merged 2026-10-14), P4.3 (`35e8866a` + `e6ee81c9`, merged 2026-10-14), P4.4 (`9f3f5f1d` + `b5ac075d`, merged 2026-10-15), P4.5 (merged `956604f0`, 2026-10-15), P4.6 (merged `cb4a74f3`, 2026-10-16), P5.9 (merged `775ba045`, 2026-10-16), P4.8 (`2904d82b`, merged `6eed0be2`, 2026-10-16), P7.5 (`9301850b`, merged `05830a9f`, 2026-10-16), P4.7 (`50bebc5f` + `41a1fd37` + `c9bbc0c0` + `ed4597bd`, merged `9e93ca3e`, 2026-10-16), P6.11 (`fddda8c2`, merged `c291cebf`, 2026-10-16), P5.1 (`35958b0f` + `1f414464` + `9f6a7670`, merged `5cb05fc6`, 2026-10-17), P5.2 (`68918fe1` + `ae825aa5` + `6cfef797`, merged `e0024774`, 2026-10-17), P5.3 (`7154f0a4`, 2026-10-17), P5.4 (`d6a85301` + `8514acba`, merged `a1a297e9`, 2026-10-17). The plan's own task records are the status of record — P1's and P2's landings are in the tree but NOT written into their task records, and P1's branch names differ from its table |
| `2026-09-22-defect-closeout-audit.md` | **Read once, before the first branch.** The cross-phase audit, five topics: what serializes behind P2's merge (52 of the 82 tasks in P3–P9, measured), which edits two phases both claim, the six fault settlements (A2, C6, D1–D4), the citations and anchors that do not resolve (X1–X6, E1/E1b), and which gates go green without a fix. Fault labels are cited from the phase files, so they must not be renumbered |
| `2026-09-22-defect-closeout-P5.md` | The unit boundary — an included file belongs to the enclosing expansion and validation unit — plus engine-scoped state and the library-env costs. **5/13** (P5.9 `f3c0910b` + `323f2580` + `70211cca`; P5.1 `35958b0f` + `1f414464` + `9f6a7670`, merged `5cb05fc6`; P5.2 `68918fe1` + `ae825aa5` + `6cfef797`, merged `e0024774`, 2026-10-17; P5.3 `952d05fc` + `d4710f7f` + `c9bbbdb4`, merged `7154f0a4`, 2026-10-17; P5.4 `d6a85301` + `8514acba`, merged `a1a297e9`, 2026-10-17), all after P2's window closes (unconditional). P5.9 done ahead of P4's close, because P4.6 depends on it. P5.7 is a decision |
| `2026-09-22-defect-closeout-P6.md` | The value surface answers at its edges instead of raising, splitting or silently rounding. **1/15** (P6.11 `fddda8c2`, merged `c291cebf`: the differential row only, the code change being P4.7's spine); five window-constrained, ten free after the registry/core carve-out. P6.7, P6.10, P6.13, P6.15 are decisions. I172's rename residual is fixed (`12edd154` + `fb7f346a` + `5ea457d2`, merged `76df7d19`), under "Filed by the P4.7 crosscheck" |
| `2026-09-22-defect-closeout-P7.md` | Provenance: four places that discard a fact the pipeline was already holding stop discarding it (port name, port read position, intro-scope label, origin stamp). **1/7** (P7.5 `9301850b`); T1–T5 before-or-after the window, T6 after, T7 unconstrained |
| `2026-09-22-defect-closeout-P8.md` | No hand-maintained list stands in for something the tree can derive, and a stale citation is caught by a gate rather than by a reader. **0/14**; four window-constrained, ten free. P8.3 is NOT done — its **Sites** paragraph names `tools/sh/planlint.sh`, and the script has no reverse arm |
| `2026-09-22-defect-closeout-P9.md` | The never-fork-dependent cleanups: retire what nothing production calls. **0/7**; three window-constrained, four free, and the phase never picks a side on before-or-after |

### Design-only — approved or drafted, no code

| Plan | Status |
|---|---|
| `2026-04-17-mcp-server-sota-design.md` | **DEAD by decision, not by deletion** (2026-07-09): MCP-server work targets wile-goast's server, not `wile --mcp`. `cmd/wile/mcp.go` still builds; the plan does not. Row corrected 2026-09-20 |
| `2026-04-21-type-constraint-extension-design.md` | Design draft; impl deferred to follow-ups |
| `2026-06-04-srfi-204-match-design.md` | Design draft; `-impl` to follow |
| `2026-06-05-mcp-llm-support-design.md` | **DEAD by the same 2026-07-09 decision.** "Phase 1 implementation-ready" was stale in the looks-open direction; corrected 2026-09-20 |
| `2026-06-09-polynomial-ideal-domain-design.md`<br>`2026-06-09-polynomial-ideal-domain-impl.md` | Design approved; impl **0/30** checkboxes, zero commits. The "10/41" figure quoted in Tier 2 was never true; corrected 2026-09-20 |
| `2026-06-24-tinyclos-object-system-design.md` | Design proposal, opt-in object system |
| `2026-06-24-unboxed-scalar-arithmetic-design.md` | Design **A rejected**; Design B + Phase 4 is the live plan |
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
| `ARCHITECTURE.md` | Header says 1/4 sections complete; the Dialect System section it marks Proposed has shipped (`WithDialect`, `pkg/wile/dialect.go`), others unverified |
| `MACRO_SYSTEM.md` | Both sections unstarted (re-verified 2026-09-20: zero `ScopeReason` / `syntax-origin` symbols in tree) |
| `2026-09-20-core-form-override-and-import-portability-design.md` | **DESIGN 2026-09-20; Part B SHIPPED**: defect 3 by P3.3 (`6f5b9f6a`), defects 1+2 by P4.3 (`35e8866a`). Parts A (stdlib self-violations), C (§5.2 opt-in check), D (superlinear expansion cost) open. As designed: successor to `2026-09-19-open-directions.local.md` §§2–3, which it supersedes on five points. Root causes for all three `if`-capture defects **verified by instrumentation**, not hypothesised: defect 1+2 are `lookupMacroBinding`'s D2 arm discarding a resolved pin because it names a `BindingTypePrimitive` (`expander_time_continuation.go:393-401`); defect 3 is `findLibraryBinding` exporting the inherited core `if` instead of the library's own Expand-phase macro (`library_bindings.go:554-557`). Two independent sites, **not** one root cause as the recap said. Both candidate fixes measured: each reproduces Racket 9.2 and Chez byte for byte, `go test ./...` green with both active. Also: the §5.2 self-violations are **three** edits not four (`(wile strings)` already excepts `string-map`; schelog's `_` shadows a startup binding, which §5.2 does not reach), the A1 stdlib edit is applied and green in a worktree, and the superlinear expander cost is reachable with **no shadowing at all** |
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
| `2026-09-19-open-directions.local.md` | Recap of the 2026-09-16→19 sessions; §§2–3 superseded by `2026-09-20-core-form-override-…-design.md` |
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
## Top Priority — Retriaged 2026-09-20

Ranked against the four goals in **Ordering** above. Each row names where the detail lives; none of
them blocks another except where stated.

| # | Goal | Item | Where | Size |
|---|---|---|---|---|
| 1 | 1 | ~~The three `if`-capture defects~~ **Done**: 1 and 2 by P4.3 (`35e8866a`/`c4442f23`, 2026-10-14), 3 by P3.3 (`6f5b9f6a`/`1f380c15`, 2026-09-30) | Tier 1 "Redefining `if`"; `plans/2026-09-20-core-form-override-and-import-portability-design.md` §3 | M |
| 2 | 1 | Expander cost is superlinear in recursion depth, with **no shadowing involved**. Cut ~8x by `7d4192b7` (depth 2000: 9.4 s → 1.2 s), now quadratic (depth 4000: 3.9 s); the DoS framing is retracted (expansion polls `ctx.Done()`), so this is a constant to cut | Tier 1, same section | M |
| 3 | 1 | Wile's own Scheme is not portable: `(srfi 13)` redefines an unexcepted import; two `test/wile` files do the same | same plan §2 | S |
| 4 | 2 | Climbing-tower Tier 2. Gated on Q5 sign-off (Q7 gates T2.3–T2.5), **not** on effort; T2.0a and T2.0b's deletion landed. The 33-row corpus is in the plan at §8.2, not perishable | Tier 2 "Climbing-tower Tier 2"; `plans/2026-07-10-climbing-tower-design.md` §6 | L |
| 5 | 2 | The §6.8 double-evaluated `define-syntax` RHS (climbing-tower T2.0c). The other three §6.8 rows are closed | Tier 1 "plans/ sweep … (2026-09-20)" | M |
| 6 | 3 | `WithStrictImports()` as an opt-in §5.2 check. Costs nothing when off, needs no program-vs-REPL decision in the binding model | same plan §4 | M |
| 7 | 4 | Two full library environments are built per file-library load, ≈5 ms each, ~99.5% of import time | Tier 4 "plans/ sweep … (2026-09-20)" | M |
| 8 | 4 | Unboxed scalar/float arithmetic, Design B. Corrected ceiling ~16–20% on the most float-heavy benchmark; still the largest single perf lever | below | L |

Below: the 2026-07-09 triage, kept because item 8 is still its conclusion. Unboxed arithmetic was
the sole real perf lever left once escape-gated frame reclaim and the layered-environment carve
were found already shipped.

- [x] **`make planlint`: flag plan headers whose Status is stale vs reality** [Done 2026-08-07]:
  `tools/sh/planlint.sh`. A status keyword alone is never a finding; an open-sounding Status line
  must coincide with a signal that the work landed: an `-impl` twin archived to `memory/`, a
  TODO.md row reporting it shipped, or a merged PR cited **on that Status line**. The PR scan is
  confined to the Status line because the any-`PR #N` form's 8 first hits were all a
  *neighbouring* plan's PR. Not in `make ci`: its inputs are gitignored. Motivated by three triage
  hits (escape-gated, layered-env, frame-reclaim) where a stale `-design` note in `plans/` hid an
  `-impl` twin archived as COMPLETE.
- [ ] **Unboxed scalar/float arithmetic — kill per-op `*values.Float` heap alloc.**
  **Phase 0 SHIPPED 2026-08-07** (the `Cell`/`Box`/`Unbox` carrier in `pkg/environment/cell.go`,
  since deleted unused by `301c8506` because Design B does not build on it, plus the probe benchmarks the plan's acceptance section cited but which had
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
