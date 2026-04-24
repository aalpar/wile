+++
title = "Implementation plan — docs/ subsystem audit sweep"
date  = "2026-04-23"
status = "Planned — not started; follows 2026-04-23-algebra-docs-impl.md"
parent = "2026-04-23-algebra-docs-impl.md"
+++

# Implementation Plan — `docs/` Subsystem Audit Sweep

Verify that every subsystem document in `docs/` (excluding `docs/algebra/`,
which is covered by Plan A) matches the current code. Fix drift where
found; defer structural/stylistic work to follow-up plans.

The sweep is a disciplined, phased follow-up — not a rewrite. Each phase
audits one subsystem directory, produces findings, and applies non-style
fixes. Phases are independent; they can run serially, in parallel, or
pause between them.

## Scope

### In scope

- Read every `.md` file under:
  - `docs/numeric/`
  - `docs/reference/`
  - `docs/environment/`
  - `docs/compiler/`
  - `docs/continuations/`
  - `docs/extensions/`
  - `docs/security/`
  - `docs/embedding/`
  - `docs/types/`
  - `docs/dev/`
  - `docs/learn/`
  - `docs/coverage/`
- For each file, verify claims against current code.
- Record findings categorized as `stale`, `missing`, `drift`, or `style`.
- Apply fixes for `stale`, `missing`, `drift`. Defer `style`.

### Explicitly not in scope

- `docs/algebra/` — Plan A.
- `docs/INDEX.md`, `docs/TOC.md`, `docs/CLAUDE.md` — umbrella docs;
  audited only to the extent they reference files we touch.
- Subsystem-level `CLAUDE.md` files — those are for Claude, not end users.
  Different audit.
- `BIBLIOGRAPHY.md`, `PRIMITIVES.md`, `CODING_STYLE.md`, `TODO.md` —
  top-level references, different artifact class.
- `plans/` — ephemeral, no audit needed.
- **Net-new documentation.** If we find a missing subsystem doc that
  *should* exist, file a follow-up plan. Do not expand scope mid-sweep.
- **Structural reorganization.** If a doc is organized wrong, note it in
  findings and defer.
- **Go source changes** of any kind.

## Per-phase workflow (applies to every phase)

Every phase follows the same four-step loop. Do not skip steps — even a
phase that finds no drift still goes through the loop so the audit record
is uniform.

### Step 1 — Inventory

- `git log --since="2026-01-01" -- <subsystem>/` for the subsystem's
  source code (not docs).
- `git log -- docs/<subsystem>/` to see when docs were last touched.
- Produce a list of code-side changes that happened after the last
  doc-side touch. This is the "surface area that could have drifted."

### Step 2 — Verify claims

For each `.md` file under the subsystem directory:

- Read the file.
- For every specific claim — file path, function name, type name,
  architectural statement, code snippet — verify against current code.
- Pin each finding to `file.go:line` or `file.md:line` per
  `CLAUDE.md` Verification principle.

### Step 3 — Findings record

Append to this plan file under the phase's heading. Format per finding:

    - **<category>** `docs/<subsystem>/<file>.md:<line>`
      <what the doc claims> → <what the code actually does>
      Evidence: `<path>:<line>`

Categories:

- `stale` — doc claim is wrong. The most important category; must fix.
- `missing` — doc omits something important that exists in code. Must fix
  unless defer is explicitly justified.
- `drift` — doc is right in spirit but wrong in specifics (renamed
  function, moved file, slightly different signature). Must fix.
- `style` — formatting, heading ordering, wording. Deferred.
- `clean` — a file was audited and no drift was found. Record it so the
  audit trail shows the file was not simply skipped.

Phase 1 established the per-phase section template: **Inventory**,
**Findings**, **Fixes**. Later phases follow the same three-heading
shape so the plan reads uniformly top-to-bottom.

If findings for a single phase exceed ~30 entries, spin them off into
`plans/2026-04-XX-<subsystem>-findings.md` and reference from this plan.

### Step 4 — Apply fixes

- One commit per subsystem. Large commit per user preference.
- Commit message: `docs(<subsystem>): sync with current code (<N> findings)`.
- Skip `style` findings unless the fix is trivially inline.
- Mark the phase complete in this plan file with commit SHA.

## Phase exit criteria (applies to every phase)

A phase closes when:

- Findings record is present under the phase heading (or in a sidecar
  findings file).
- Non-style findings are fixed and committed.
- Phase heading is marked `Completed — <SHA>` or `Completed — no changes
  needed` in this plan file.

## Phases

Phases are ordered by dependency — subsystems whose concepts feed into
others are audited first, so later phases can trust the foundations.

### Phase 1 — `numeric/` (3 files)

Files:

- `docs/numeric/tower.md`
- `docs/numeric/precision-guarantees.md`
- `docs/numeric/nan-boxing.md`

Code under verification:

- `values/numeric_*.go`
- `values/integer.go`, `values/rational.go`, `values/float.go`,
  `values/bigcomplex.go`.

Specific concerns:

- NaN-boxing doc is flagged as educational — verify it still describes
  the implementation, not a considered-and-rejected alternative.
- Tower doc predates any NaN work; verify promotion/contagion rules.

### Phase 2 — `reference/` (3 files)

Files:

- `docs/reference/scheme.md`
- `docs/reference/r7rs-differences.md`
- `docs/reference/implementation-notes.md`

Code under verification:

- Wherever specific language-reference claims anchor — `PRIMITIVES.md`,
  `internal/forms/`, `registry/core/`, selected special forms in
  `machine/`.

Specific concerns:

- `r7rs-differences.md` is the conformance-record doc. Every listed
  deviation must match current behavior.

### Phase 3 — `environment/` (3 files)

Files:

- `docs/environment/system.md`
- `docs/environment/diagram.md`
- `docs/environment/racket-namespaces.md`

Code under verification:

- `environment/` package.
- Post-PR-#544 namespace refactor (`TopLevelEnvironment` → `Namespace`,
  registry relocation, module instance caching).

Specific concerns:

- Any reference to `TopLevelEnvironment` is stale.
- Registry and authorizer location is on `Namespace`, not where older
  docs might say.

### Phase 4 — `compiler/` (6 files)

Files:

- `docs/compiler/macro-system.md`
- `docs/compiler/peephole-optimizer.md`
- `docs/compiler/core-let.md`
- `docs/compiler/inlining.md`
- `docs/compiler/ssa.md`
- `docs/compiler/anf-and-cps.md`

Code under verification:

- `machine/compile_*.go`, `machine/expand_*.go`, `machine/compilation/`,
  `machine/peephole*.go`.

Specific concerns:

- Peephole doc mentions savedCont invariant — verify PR #573 fix is
  reflected.
- Core-let doc is post-PR-#570; verify against current `core-let`
  handling.
- SSA doc is speculative — verify it's still marked as "would it help?"
  rather than implying SSA exists.

### Phase 5 — `continuations/` (8 files)

Files:

- `docs/continuations/concepts.md`
- `docs/continuations/marks.md`
- `docs/continuations/implementation.md`
- `docs/continuations/delimited.md`
- `docs/continuations/escape-design.md`
- `docs/continuations/prompt-abort.md`
- `docs/continuations/optimizations.md`
- `docs/continuations/racket-primitives.md`

Code under verification:

- `machine/machine_continuation.go`, `machine/machine_context.go`,
  `machine/continuation_marks.go`, `machine/prompt*.go`.

Specific concerns:

- Marks doc is post-PR-#542; verify `with-continuation-mark` semantics
  and `isolatedMarks` flag.
- Prompt/abort doc must match current `%` form handling.

### Phase 6 — `extensions/` (2 files)

Files:

- `docs/extensions/architecture.md`
- `docs/extensions/libraries.md`

Code under verification:

- `registry/`, `extensions/`, `internal/extensions/`.

Specific concerns:

- Extension API contract (Phase 1 shipped PRs #577-578; Phases 2-4 not
  started). Doc must not over-claim what's implemented.
- Library integration: verify post-PR-#553/#554 FileResolver chain is
  documented correctly.

### Phase 7 — `security/` (2 files)

Files:

- `docs/security/sandboxing.md`
- `docs/security/blog-sandboxing.md`

Code under verification:

- `security/` package, `wile/` engine options (`WithProfile`,
  `WithAuthorizer`, `WithSandbox`).

Specific concerns:

- Profile-based API (PR #662) must be the primary documented entry
  point; ad-hoc `WithExtension` secondary.
- Authorizer interface signature must match current code.

### Phase 8 — `embedding/` (3 files)

Files:

- `docs/embedding/api-design.md`
- `docs/embedding/source-loading.md`
- `docs/embedding/mcp.md`

Code under verification:

- `wile/` package (public API), `machine/file_resolver.go`,
  `cmd/wile/mcp.go`.

Specific concerns:

- `api-design.md` must reflect post-PR-#555 `Parse` / `Eval` /
  `EvalMultiple` split.
- `source-loading.md` must match the four FileResolver implementations
  (`OSFileResolver`, `FSFileResolver`, `EmbedFileResolver`,
  `ChainFileResolver`) and the embed-FS `fs.Sub` lesson.
- `mcp.md` must match current tool inventory.

### Phase 9 — `types/` (4 files)

Files:

- `docs/types/records-as-formal-types.md`
- `docs/types/abstract-data-types.md`
- `docs/types/scheme-types-records-mop.md`
- `docs/types/racket-structs.md`

Code under verification:

- `values/record*.go`, record-related primitives.

Specific concerns:

- Primarily conceptual docs; verify code snippets still compile against
  the current value type.

### Phase 10 — `dev/` (4 files)

Files:

- `docs/dev/debug-methodology.md`
- `docs/dev/foreign-closure-design.md`
- `docs/dev/pooling.md`
- `docs/dev/project-board-setup.md`

Code under verification:

- `machine/foreign_closure.go`, `values/freelist*.go`,
  `values/pool*.go`.

Specific concerns:

- Foreign-closure design doc must reflect post-PR-#573 savedCont fix.
- Pooling doc must match current `FreeList` contract.
- Project-board-setup is operational — may be stale if workflow changed.

### Phase 11 — `learn/` (2 files)

Files:

- `docs/learn/python-vs-scheme-for-algebra.md`
- `docs/learn/scheme-debugging-primitives.md`

Code under verification:

- Debugging primitives — `apropos`, `doc`, `disassemble`, any REPL
  commands.

Specific concerns:

- Light — mostly educational prose. Verify primitive names and
  arg shapes in any example blocks.

### Phase 12 — `coverage/` (1 file)

Files:

- `docs/coverage/scheme-coverage.md`

Code under verification:

- `coverage/` package, `cmd/wile/main.go` `--cover` flag.

Specific concerns:

- Coverage work is recent (per `2026-04-18-scheme-line-coverage.md`).
  Verify doc reflects final shipped API.

## Estimated size (rough, not a promise)

- Small phases (11, 12): 1 file each; half a day.
- Medium phases (1, 2, 3, 6, 7, 10): 2–4 files; full day each.
- Large phases (4, 5, 8, 9): 3–8 files; up to two days each.

Total: ~8–12 working days if sequential. Independent phases can overlap.

## Success criteria

- All 12 phase headings marked `Completed — <SHA>` or
  `Completed — no changes needed`.
- Every non-style finding fixed.
- `make lint` clean after each phase's commit.
- No `docs/` content change outside the audited subsystems.

## Risks and mitigations

- **Scope creep into rewrites.** Easiest failure mode. Mitigation: strict
  categorization — `style` findings are deferred, full stop. If a finding
  requires restructuring the doc, it goes to a follow-up plan.
- **Verification-task underestimation.** Reading-and-diffing is
  deceptively slow. Mitigation: per-phase size estimates above are
  pessimistic; if a phase exceeds its estimate by 2×, split into
  sub-phases and re-estimate.
- **Findings pile-up across subsystems.** If Phase 1 discovers a pattern
  of drift that suggests systematic issues (e.g., many docs reference
  pre-namespace-migration symbols), the sweep plan itself may need
  revision. Mitigation: after three phases complete, take stock — if the
  finding rate is much higher than expected, re-scope.
- **Code changes during the sweep.** The repo is not frozen. Mitigation:
  each phase's inventory step is re-run at phase start; if significant
  code changes land during the sweep, update the finding list.

## Findings log

Per-phase findings records accumulate below as phases execute.

### Phase 1 — `numeric/` — Completed (PRs #707, #708)

**Inventory** (2026-04-23):

- Last doc touch of `docs/numeric/`: `bf83fa43` (2026-04-15) — reorganize by topic with INDEX.md and TOC.md.
- Code changes to `values/` since that date: one commit `852926fa feat: add error diagnostics via continuation marks and NativeError enrichment`. Not numeric-related.
- However, the precision-guarantees.md doc has file/line claims that pre-date the scope of this Phase 1 window. The parser refactor that moved `numberToInexact`/`makeInexact` out of `parser.go` happened before 2026-04-15 as well — the doc's line references were never updated to the new locations.

**Findings**:

- **drift** `docs/numeric/tower.md:96`
  Claims `Promote` was "Deleted (2026-02-05)". `Promote` still exists at `values/promotion.go:303`, exported.
  Evidence: `values/promotion.go:303` (`func Promote(n Number, target NumericKind) Number`).

- **missing** `docs/numeric/tower.md` § "Current API"
  Doc lists `Simplify` and `ExactnessOf` as the only utility functions, but the actual architecture uses dispatch tables indexed by `NumericKind` (41 tables, 294 closures) populated at `init()` by generators in `values/promotion.go` (`makeArithmeticDispatch`, `makeLessThanDispatch`, `makeCompareDispatch`). This is a real architectural fact the doc should mention.
  Evidence: `values/promotion.go` — `ensurePromotionInit` and the three `make*Dispatch` generators; `values/big_complex.go:148` ("BigComplex has 5 dispatch tables (no bigComplexLessThan)").
  Revision (PR #708): evidence pointer corrected from `values/CLAUDE.md` (has no such section) to `values/promotion.go` + `big_complex.go:148`.

- **drift** `docs/numeric/precision-guarantees.md:93`
  Claims `toExactPart` is at `values/big_complex.go:493`. Actual location is `values/big_complex.go:379`. Line 493 is now the `EqualTo` method.
  Evidence: `values/big_complex.go:379` (`func toExactPart(n Number) (Number, error)`).

- **drift** `docs/numeric/precision-guarantees.md:99`
  Claims `numberToInexact` is at `internal/parser/parser.go:1722-1727`. Actual location is `internal/parser/parser_number.go:627-644`. The numeric parsing functions were moved out of `parser.go` (now 877 lines total) into a dedicated `parser_number.go` during a prior refactor.
  Evidence: `internal/parser/parser_number.go:627`.

- **drift** `docs/numeric/precision-guarantees.md:99`
  Claims `makeInexact` is at `internal/parser/parser.go:1758-1761`. Actual location is `internal/parser/parser_number.go:649-682`.
  Evidence: `internal/parser/parser_number.go:649`.

- **drift** `docs/numeric/precision-guarantees.md:215` (Audit Checklist)
  References `ffi.go:convertArg`. No function named `convertArg` exists in `ffi.go`. The FFI entry points are now `RegisterFunc`, `RegisterFuncs`, `buildFFISpec`, etc. Per-argument conversion is distributed across those functions; no single `convertArg` exists to audit.
  Evidence: `ffi.go` (no match for `convertArg`).

- **drift** `docs/numeric/precision-guarantees.md:208` (Audit Checklist)
  The exported `NumberToFloat64` (`values/promotion.go:327`) and `NumberToComplex128` (`values/promotion.go:352`) are the Tier 3 precision-dropping helpers that belong in the checklist. The previous entry used a lowercase symbol name and was not tracking them.
  Evidence: `values/promotion.go:327` `NumberToFloat64`, `values/promotion.go:352` `NumberToComplex128`.
  Revision (PR #708): an earlier revision of this finding (PR #707) stated no such function existed; that was a lowercase-grep miss. PR #708 replaces the unactionable entry with two real audit targets.

- **clean** `docs/numeric/nan-boxing.md`
  Educational doc. Structural claims (Wile uses Go interfaces for `Value`; `unsafe` constraint rules out NaN-boxing) are still accurate per `values/CLAUDE.md`. Profiling numbers quoted are historical / illustrative; no code-verification action needed.

**Fixes** (to be committed):

- Update the three file:line references in precision-guarantees.md to match current code.
- Remove `Promote` from the "Deleted (2026-02-05)" list in tower.md.
- Add a short "Dispatch Table Architecture" subsection to tower.md mentioning the `NumericKind`-indexed table layout (per `values/CLAUDE.md`).
- Revise the two unactionable audit-checklist entries (`ffi.go:convertArg`, `values/promotion.go:numberToFloat64`): either remove them or replace with the current symbols they were intended to track.

Style findings deferred per plan scope controls (no architectural reorganization, no adding missing docs). The "Status: Stable (2026-02-05)" date stamp is old but fine as-is until something substantive changes.

### Phase 2 — `reference/` — Completed (branch `feat/docs-sweep-reference`)

**Inventory** (2026-04-23):

- Last doc touch of `docs/reference/`: `c1000595` (2026-04-20) — R7RS §6.13.2 fix for mid-parse EOF. Subsequent doc-side changes: none.
- Code changes since then touching areas the docs reference: several audit commits on primitives (H.1 numeric widening, F.1/F.2 chars, E.1 lists, etc.); no structural changes to the language surface.
- The bigger concern is the gap *before* that date — the reference has accumulated drift across PR #570 (CORE-LET made let/letrec/letrec* core compiled forms), various primitive additions, and the entire `(wile algebra)` library tree that was added in 2026-04 and isn't mentioned.

**Findings**:

- **stale** `docs/reference/implementation-notes.md:9,29,42-51`
  Entire document claims `letrec` and `letrec*` are macro-defined in `registry/core/bootstrap_macros.scm`. Since PR #570 (CORE-LET), `let`/`let*`/`letrec`/`letrec*` are core compiled forms handled directly by the expander/validator/compiler pipeline, not macros. The bootstrap_macros.scm file at lines 44-46 explicitly says so. The entire "letrec* Implementation" section is obsolete — the semantic claim (sequential evaluation) is still true, but the mechanism description is wrong.
  Evidence: `registry/core/bootstrap_macros.scm:44-46` ("Binding forms ... are now core compiled forms"); `plans/CORE-LET.md` (shipped PR #570).

- **drift** `docs/reference/scheme.md:5`
  `**Version**: v1.5.0` is many minor versions behind the implementation. Either the version tag means "Scheme language reference revision" and should be interpreted independently, or it should track implementation. Since the doc has no other language-revision versioning, this reads as a frozen/forgotten stamp. Fix by either removing the version line or clarifying what it tags.
  Evidence: `VERSION` file (current implementation version), `scheme.md:5`.

- **missing** `docs/reference/scheme.md:1584-1590` ("Wile Scheme Libraries")
  Table lists `(wile control)`, `(wile kanren)`, `(wile microkanren)` only. Missing: the entire `(wile algebra)` library tree — umbrella `(wile algebra)` plus 26 sub-libraries shipped during 2026-03..2026-04 (setoid, monoid, group, ring, lattice, polynomial, matrix, incidence, symbolic, rewrite, combinatorial-graph, etc.). Given algebra is one of Wile's signature features and was the subject of PR #706's entire tutorial, omitting it from the reference libraries section is a substantial gap.
  Evidence: `stdlib/lib/wile/algebra.sld` (umbrella); 26 `.sld` files under `stdlib/lib/wile/algebra/`; `docs/algebra/reference.md` (separate, comprehensive).

- **drift** `docs/reference/scheme.md:1354` ("Reflection" table)
  Claims `procedure-type` returns "Type tag string". Actual returns are symbols, and the full set is wider than two values: `closure` (Scheme lambda), `foreign` (Go primitive), `case-lambda` (case-lambda closure), `parameter` (parameter object), `continuation` (composable continuation), with `unknown` as a fallback for any other callable.
  Evidence: `registry/core/prim_reflection.go:246-259`; docstring at `registry/core/reflection.go` for `procedure-type` enumerates all five named cases; `registry/core/prim_reflection_test.go:311-354` (`TestProcedureType`) asserts all five return symbols.

- **clean** `docs/reference/r7rs-differences.md`
  All four documented semantic differences verify against current code:
  - `char-ready?` / `u8-ready?` always #t — `internal/extensions/io/prim_read_write.go:346-354`.
  - `parameterize` uses `with-continuation-mark` — `registry/core/bootstrap_macros.scm:121-137`.
  - `set-current-directory!` uses `os.Chdir` — `extensions/files/prim_directory.go:146`.
  - Pair/vector literals mutable — `values/pair.go` / `values/vector.go` have no immutability flag (per `values/CLAUDE.md`).

**Fixes** (to be committed):

- `implementation-notes.md`: rewrite to reflect current reality — `let`/`letrec`/`letrec*` are core compiled forms, not macros. Either remove the letrec* section entirely (the implementation note is historical) or restructure the document around current implementation choices worth documenting.
- `scheme.md:5`: remove the v1.5.0 version line; the doc stands on its own without a stamp.
- `scheme.md:1584-1590`: add a row for `(wile algebra)` (umbrella + pointer to sub-library tree) so the reference inventory matches what ships.
- `scheme.md:1354`: fix `procedure-type` return type ("Type tag symbol (`foreign` or `closure`)" instead of "Type tag string").

Style / layout findings deferred per plan scope controls.

### Phase 3 — `environment/` — Completed (branch `feat/docs-sweep-environment`)

**Inventory** (2026-04-23):

- Last doc touch of `docs/environment/`: `bf83fa43` (2026-04-15) — topic reorganization.
- Code changes to `environment/` since then: `envmap` additions (`WithEnv`, `WithEnvMap`), `envBindingChecker` Copilot fixes, `apropos` built-sentinel work, and the earlier refactor that moved `LoadPathStack` out of `environment/` to `machine/compilation/sourceload/`.

**Findings**:

- **drift** `docs/environment/system.md:272` (Load-Path Stack diagram)
  Doc labels the stack file as `environment/load_path_stack.go`. File does not exist in `environment/`. The type now lives at `machine/compilation/sourceload/load_stack.go` (type name `LoadStack`, not `LoadPathStack`). The interface exposed in `environment/` is `PathTracker` in `environment/file_resolver.go:55`.
  Evidence: `ls environment/*.go` (no `load_path_stack.go`); `machine/compilation/sourceload/load_stack.go:22` (`func NewLoadStack() *LoadStack`); `environment/file_resolver.go:55` (`type PathTracker interface`).

- **drift** `docs/environment/system.md:285`
  References `ResolveFile` in `environment/resolve.go`. Neither the function nor the file exists. Current architecture resolves files through the `FileResolver` interface in `environment/file_resolver.go` and concrete implementations in `machine/compilation/sourceload/` (`ResolveLibraryFile` at `machine/compilation/library_registry.go:309`, etc.).
  Evidence: no `environment/resolve.go`; no top-level `ResolveFile` function (grep).

- **drift** `docs/environment/diagram.md:25` (Ownership Hierarchy)
  Diagram labels the Namespace's load-path field as `*LoadPathStack`. Actual field type is the `PathTracker` interface (`environment/namespace.go:64`); the concrete value is `*sourceload.LoadStack` set by `engine.go:125`.
  Evidence: `environment/namespace.go:64` (`loadPathStack PathTracker`); `engine.go:125` (`ns.SetLoadPathStack(sourceload.NewLoadStack())`).

- **clean** `docs/environment/racket-namespaces.md`
  Conceptual/comparative document introducing Racket's namespace model. No Wile-specific code claims to verify. Reads correctly against current implementation.

**Fixes** (to be committed):

- `system.md` § "Load-Path Stack": correct the file reference from `environment/load_path_stack.go` to `machine/compilation/sourceload/load_stack.go`; rename `LoadPathStack` to `LoadStack` where it refers to the concrete type; note the interface split (`PathTracker` in `environment/file_resolver.go`, concrete `LoadStack` in `machine/compilation/sourceload/`).
- `system.md` § "Resolution Strategy": drop the `environment/resolve.go` path; describe resolution as happening via the `FileResolver` interface with concrete implementations in `machine/compilation/resolver/` (`os_file_resolver.go`, `fs_file_resolver.go`, `embed_file_resolver.go`, `chain_file_resolver.go`), backed by `sourceload.Finder` for file search. (Revision note: the first cut of this fix said implementations live in `sourceload/`; that was wrong — `sourceload/` holds `LoadStack`, `Finder`, `walk.go`, while the concrete `FileResolver` types live in the sibling `resolver/` package. Copilot + errors-lens flagged this convergently.)
- `diagram.md`: update the ownership-hierarchy box to show `loadPathStack ─── PathTracker` instead of `*LoadPathStack`, matching the current field type.

### Phase 4 — `compiler/` — Pending

### Phase 5 — `continuations/` — Pending

### Phase 6 — `extensions/` — Pending

### Phase 7 — `security/` — Pending

### Phase 8 — `embedding/` — Pending

### Phase 9 — `types/` — Pending

### Phase 10 — `dev/` — Pending

### Phase 11 — `learn/` — Pending

### Phase 12 — `coverage/` — Pending

## References

- Sibling plan: `2026-04-23-algebra-docs-impl.md` (algebra-specific docs
  work; must ship first).
- Doc conventions: `docs/CLAUDE.md`.
- Plan conventions: `plans/CLAUDE.md`.
