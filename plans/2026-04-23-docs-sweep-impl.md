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

### Phase 4 — `compiler/` — Completed (branch `feat/docs-sweep-compiler`)

**Inventory** (2026-04-24):

- Last doc touch of `docs/compiler/`: `bf83fa43` (2026-04-15) — topic reorganization.
- Code changes to `machine/` since then: ~25 commits including the `machine/` → `machine/compilation/` refactor (`02dd8b39`, `75767751`, `33ae0c6a`), the `bootstrap.go` rename (`72faae34 refactor(bootstrap): rename environment_tiny to bootstrap`), coverage hooks (`396ea6b7`, `b4c8ac8e`), timer interrupts (`0f766afd`), error diagnostics (`852926fa`, `af94d2f2`, `b46dcb5e`), and sourceload extraction (`8838409b`, `318a0992`, `02dd8b39`).

**Findings**:

- **drift** `docs/compiler/macro-system.md:89,246,283,301-315` (File Reference + inline citations)
  Four files moved during the `machine/compilation/` extraction refactor (late 2026-04). Doc still cites the old locations:
  - `machine/compile_syntax_rules.go` → `machine/compilation/compile_syntax_rules.go`
  - `machine/operation_syntax_rules_transform.go` → `machine/compilation/operation_syntax_rules_transform.go`
  - `machine/expander_time_continuation.go` → `machine/compilation/expander_time_continuation.go`
  - `internal/bootstrap/environment_tiny.go` → `internal/bootstrap/bootstrap.go` (per commit `72faae34`, the file was renamed; the package and type names stayed the same).
  Evidence: `find machine -name compile_syntax_rules.go` yields only `machine/compilation/compile_syntax_rules.go`; `find internal/bootstrap` shows `bootstrap.go` (no `environment_tiny.go`).

- **clean** `docs/compiler/core-let.md`
  Conceptual doc about compiler design (not specific implementation). Opcode claims (`OpPushEnv`, `StoreLocal`, `OpPopEnv`) verify against `machine/opcode.go:44,57,61`.

- **clean** `docs/compiler/peephole-optimizer.md`
  File-reference table at the bottom (10+ entries) verifies — every cited file exists at its cited path (`machine/peephole.go`, `machine/edit_plan.go`, `machine/instruction.go`, `machine/opcode.go`, `machine/native_template.go`, `machine/call_promoted.go`, `machine/call_promoted_arithmetic.go`, `machine/call_foreign_cached.go`, `machine/machine_context_apply.go`, `machine/peephole_test.go`, top-level `opcode_fusion_test.go`, `callcc_engine_test.go`).

- **clean** `docs/compiler/inlining.md`
  Describes optimization opportunities, not concrete code locations. Opcode names and `BindingType*` values mentioned (`BindingTypeVariable`, `Syntax`, `Primitive`, `Unknown`) match `environment/binding_type.go`.

- **clean** `docs/compiler/anf-and-cps.md`
  Pure conceptual doc. No file references to verify.

- **clean** `docs/compiler/ssa.md`
  Pure conceptual doc. No file references to verify.

**Fixes** (committed):

- `macro-system.md`: updated the four moved-file references (inline citations and the File Reference table) to point at the current locations (`machine/compilation/...` and `internal/bootstrap/bootstrap.go`).
- `macro-system.md`: added the `Label` field to the `Scope` struct snippet at line 83, matching `internal/syntax/syntax_value.go:30-47`.
- `macro-system.md`: corrected inline file references at the Implementation in Code section (`machine/compilation/operation_syntax_rules_transform.go:193`, `internal/match/syntax_expand.go:293-294`, `internal/syntax/scope_utils.go:58`) — the initial fix updated section headers and the File Reference table but missed the inline code pointers.
- `macro-system.md`: Syntax Adapter section now notes the concrete implementation lives in `internal/match/syntax_expand.go` (not the old `syntax_adapter.go`), with `capturedValueToSyntax` cited at `syntax_expand.go:332`.
- `macro-system.md`: Bootstrap Macros table rewritten to match what's actually in `registry/core/bootstrap_macros.scm`. Removed `let`, `let*`, `letrec` (now core compiled), removed `do`'s old row with stale sketch, and added the forms the old table omitted (`delay`, `delay-force`, `parameterize`, `guard-aux`, `define-opaque-record-type`, `define-record-type-impl`, `let-values`, `let*-values`, `define-values`, `with-continuation-barrier`, `with-baffle`).

### Phase 5 — `continuations/` — Split into 5a/5b/5c sub-phases (per plan's "too large to review in one PR" guidance)

#### Phase 5a — `concepts.md` + `implementation.md` + `marks.md` — Completed (PR #712)

#### Phase 5b — `delimited.md` + `prompt-abort.md` + `escape-design.md` — Completed (PR #713)

#### Phase 5c — `optimizations.md` + `racket-primitives.md` — Completed (awaiting PR)

**Inventory** (2026-04-24):

- Last doc touch of `docs/continuations/`: same as Phase 5a/5b (`2785298c` 2026-04-17).
- Relevant code changes since: the `machine_context.go` split (`RestoreAndRelease` moved to `machine_context_continuation.go:79`; `Apply` moved to `machine_context_apply.go:27`); the `internal/extensions/` → `extensions/` reorg (public extension packages live at `extensions/eval/`, `extensions/files/`, etc.); the `machine/compile_validated.go` → `machine/compilation/compile_validated.go` move from the compilation subpackage split.

**Findings**:

- **drift** `docs/continuations/optimizations.md:189`
  Optimization 5 "Files:" header cites `machine/machine_context.go` for `RestoreAndRelease`. Function now at `machine/machine_context_continuation.go:79`.
  Evidence: `grep -n "^func.*RestoreAndRelease" machine/*.go`.

- **drift** `docs/continuations/optimizations.md:103`
  Optimization 3 "Files:" header cites `machine/machine_context.go` for the `Apply` consumer of `NewApplyFrame`. `Apply` is now at `machine/machine_context_apply.go:27`.
  Evidence: `grep -n "^func (p \*MachineContext) Apply" machine/*.go`.

- **drift** `docs/continuations/optimizations.md:399` (References section)
  Cites `machine/machine_context.go — RestoreAndRelease with shared-flag branching`. Same drift as #1; should be `machine_context_continuation.go`.

- **drift** `docs/continuations/racket-primitives.md:220`
  `with-continuation-mark` compilation cited at `machine/compile_validated.go`. File is now at `machine/compilation/compile_validated.go` after the compilation/ subpackage split.

- **drift** `docs/continuations/racket-primitives.md:334-335,452-453`
  Four entries cite `registry/core/prim_eval.go` for `eval`, `environment`, `expand`, `expand-once`. File no longer exists. These primitives now live in `extensions/eval/prim_eval.go` + `extensions/eval/register.go` (the eval extension was moved from the internal layout to the public `extensions/` package).
  Evidence: `ls registry/core/prim_eval.go` → missing; `grep -n "PrimEval\|PrimExpand\|PrimEnvironment" extensions/eval/*.go` locates them.

- **drift** `docs/continuations/racket-primitives.md:343,637`
  Two entries cite `internal/extensions/eval/prim_eval.go` for `syntax-local-value/immediate`. Path no longer exists; actual location is `extensions/eval/prim_eval.go:603` (PrimSyntaxLocalValueImmediate).

- **drift** `docs/continuations/racket-primitives.md:624`
  "Go primitives are in `registry/core/` and `internal/extensions/eval/`" — `internal/extensions/` path is gone; public extensions live at `extensions/`.

- **clean** `docs/continuations/optimizations.md:65`
  Optimization 1 "Files:" cites `machine/machine_context.go` for the OpLoadLocal/OpStoreLocal callers of `GetLocalBindingBySlotDepth`/`SetLocalValueBySlotDepth`. Verified: those call sites remain in `machine/machine_context.go` at lines 519 and 1112.

- **clean** `docs/continuations/optimizations.md:265` (historical Opt 6)
  Cites `machine/compile_validated.go` in "Files (historical):" for the REMOVED noCopyApply optimization. This is historically correct — at the time of PR #561, the file did live at that path. The compilation/ subpackage split came later. Annotation left as-is.

**Fixes** (committed in this PR):

- `optimizations.md:103`: `machine/machine_context.go` → `machine/machine_context_apply.go` (Opt 3 Apply consumer)
- `optimizations.md:189`: `machine/machine_context.go` → `machine/machine_context_continuation.go` (Opt 5 RestoreAndRelease)
- `optimizations.md:399`: same fix in References section
- `racket-primitives.md:220`: `machine/compile_validated.go` → `machine/compilation/compile_validated.go`
- `racket-primitives.md:334-335,452-453`: `registry/core/prim_eval.go` → `extensions/eval/prim_eval.go` (4 entries)
- `racket-primitives.md:343,637`: `internal/extensions/eval/prim_eval.go` → `extensions/eval/prim_eval.go`
- `racket-primitives.md:624`: strip `internal/` prefix

**Additional fixes from crosscheck code-lens review** (commit 2):

- `racket-primitives.md:324-327`: four rows (`syntax-local-value`, `make-compile-time-value`, `syntax-local-introduce`, `syntax-local-identifier-as-binding`) were cited at `registry/core/syntax.go` but actually live in `extensions/eval/prim_eval.go`. The `registry/core/syntax.go` file only registers 6 primitives (`identifier?`, `syntax->datum`, `datum->syntax`, `generate-temporaries`, `bound-identifier=?`, `free-identifier=?`) — none of the `syntax-local-*` ones. Remaining 6 citations at that path verified correct.
- `racket-primitives.md:534`: `syntax-local-introduce` second occurrence (Phase Introspection section) — same fix.

**Crosscheck findings NOT actioned** (with rationale):

- [code] `optimizations.md:265` "Files (historical):" still cites `machine/compile_validated.go`. Code-lens flagged as miss; consistency-lens explicitly concurred with leaving as-is. The section describes the REMOVED noCopyApply optimization as it existed pre-PR #561. At that time, the file DID live at `machine/compile_validated.go`; the compilation/ subpackage split happened later. Retargeting to `machine/compilation/compile_validated.go` would be factually wrong — the noCopyApply code never lived at that path. Historical annotation preserved.
- [tests] precision gap: `machine_context_apply.go` is cited as the `NewApplyFrame` consumer, but `Apply` actually calls `InitApplyFrame` (the pooling-friendly counterpart). The "Files:" header is a coarse two-file index; both files contain relevant code (`NewApplyFrame` defined in `environment/environment_frame.go`; Apply consumer in `machine_context_apply.go`). Minor precision, not drift.
- [tests] precision gap: primitive registrations live in `extensions/eval/register.go` while implementations live in `extensions/eval/prim_eval.go`. Tables cite only the impl file per existing doc convention (sibling rows in the same tables follow the same pattern).

### Phase 6 — `extensions/` — Completed (awaiting PR)

**Inventory** (2026-04-24):

- Last touch of `docs/extensions/`: pre-dates the `internal/extensions/` → `extensions/` public-API reorg that moved `eval` to the public surface and added a new `envvars` package.
- Relevant code structure changes since: `eval` moved from `internal/extensions/eval` to `extensions/eval` (part of public embedding API); `internal/extensions/envvars` package added (hosts `get-environment-variable`/`get-environment-variables` with sandbox-awareness — moved out of `system`); several primitives migrated between extensions (`features` → `introspection`, env vars → `envvars`); various extensions grew primitives (math +5, files +5, introspection +3).

**Findings**:

- **drift** `docs/extensions/architecture.md:469-487` (Public + Internal Extensions tables)
  - `extensions/eval` missing from Public Extensions table — it's now public (moved from `internal/extensions/eval`).
  - `internal/extensions/envvars` missing from Internal Extensions table — this package exists and hosts `get-environment-variable`/`get-environment-variables`.
  - `internal/extensions/eval` listed as internal — no longer exists at that path.
  - `extensions/system` primitive list includes `get-environment-variable`, `get-environment-variables`, `features` — these moved: env vars to `internal/extensions/envvars`, `features` to `extensions/introspection`. Actual `system` list has 6 primitives.
  - `extensions/files` list has 10 primitives — actual register.go has 13, missing `create-directory`, `delete-directory`, `directory-files`, `current-directory`, `set-current-directory!`.
  - `extensions/introspection` list has 5 primitives — actual register.go has 8, missing `features`, `available-libraries`, `disassemble`.
  - `extensions/math` list includes `square` — `square` is a core bootstrap procedure (`bootstrap_procedures.scm:321`), not a math extension primitive.
  Evidence: `ls extensions/ internal/extensions/`; `grep -c "Name:" extensions/*/register.go`.

- **drift** `docs/extensions/libraries.md:62-69` (Available Extension Libraries table)
  - `(wile eval)` missing entirely.
  - `(wile math)` claims "30 math primitives" — actual 35.
  - `(wile system)` claims "9 system primitives" — actual 6 (3 moved as above).
  - `(wile introspection)` description is narrow ("Environment introspection") — extension actually covers features and disassembler too.

- **clean** `docs/extensions/libraries.md:239-248` (`LibraryEnvFactory` narrative)
  `LibraryEnvFactory` type is at `environment/namespace.go:36`; `SetLibraryEnvFactory` on `Namespace` at line 250. Doc description of isolated library environment creation matches implementation.

- **clean** `docs/extensions/architecture.md:152-160` (Optional Interfaces table)
  `Describer`, `LibraryNamer`, `Closeable` all verified at `registry/extension.go:28-45`. `NewDescribedExtension` at `registry/extension.go:61`.

- **clean** ASCII diagrams (architecture.md:21-37, libraries.md:80-96)
  Primitive counts in diagrams (`~80`, `+30`, `80 → 110`) are pedagogical approximations, intentionally vague; not treating as drift.

**Fixes** (committed in this PR):

- `architecture.md:469-487`: add `extensions/eval` row to Public Extensions; remove `get-environment-variable`/`get-environment-variables`/`features` from `extensions/system` row (6 primitives now); expand `extensions/files` row to 13 primitives; expand `extensions/introspection` row to 8 primitives (add `features`, `available-libraries`, `disassemble`); remove `square` from `extensions/math` row; swap `internal/extensions/eval` entry for `internal/extensions/envvars` in Internal Extensions table.
- `libraries.md:62-69`: add `(wile eval)` row; fix primitive counts for `(wile math)` (30 → 35), `(wile system)` (9 → 6), `(wile files)` ("File I/O primitives" → "13 file/directory primitives"), `(wile introspection)` description expanded.

### Phase 7 — `security/` — Completed (PR #716)

**Inventory** (2026-04-24):

- Last touch of `docs/security/` predates the `internal/extensions/eval` → `extensions/eval` reorg and the `envvars` extension extraction.
- Profile definitions in `profile.go` + `internal/bootstrap/bootstrap.go:ProfileExtensions` are the source of truth for profile composition.

**Findings**:

- **drift** `docs/security/sandboxing.md:21`
  Extension security classification table lists `eval` at `internal/extensions/eval`. Actual path is `extensions/eval` (public, per the reorg documented in Phase 6).

- **drift** `docs/security/sandboxing.md:22`
  `system` row lists `get-environment-variable` as one of its primitives. That primitive moved to `internal/extensions/envvars` (sandbox-aware). Same drift as Phase 6.

- **missing** `docs/security/sandboxing.md:14-25` (classification table)
  Table lists 11 extensions but `allExtensions` in `bootstrap.go:70-83` has 12. Missing `envvars` and `namespace` rows. Both are privileged.

- **stale** `docs/security/sandboxing.md:241`
  References `plans/SECURITY.md`. File was rejected (per `memory/MEMORY.md` note — "existing limits sufficient") and doesn't exist in repo.

- **clean** `docs/security/sandboxing.md:35-41` (profile composition table)
  Verified each profile's extension list matches `ProfileExtensions` in `internal/bootstrap/bootstrap.go:93-134`: Tiny = none; Console = io, files, math, all.SafeExtension, envvars (+ core always); ConsoleWithLoad = Console + eval; Small = io, files, math, introspection, eval, all, system, envvars; KitchenSink = allExtensions (12).

- **clean** `docs/security/sandboxing.md:107-119` (Enforcement mechanism)
  Narrative matches `registry/apply.go` + `machine/compilation/compile_time_continuation.go:201` error path (`"no such local or global binding %q"` matches doc's fail-fast claim).

- **clean** `docs/security/blog-sandboxing.md`
  Blog post with narrative content about Scheme's design. Error message claim at line 70 (`expand/compile error: no such local or global binding "open-input-file"`) verified against `compile_time_continuation.go:201`. Other claims are editorial/historical; no Wile-specific drift.

**Fixes** (committed in this PR):

- `sandboxing.md:21`: `internal/extensions/eval` → `extensions/eval`; updated primitive list to include `syntax-local-*`.
- `sandboxing.md:22`: removed `get-environment-variable` from `system` row; replaced with current system primitives (`current-second`, `current-jiffy`, `jiffies-per-second`). Kept `exit`, `emergency-exit`, `command-line`.
- `sandboxing.md:14-25`: added `envvars` and `namespace` rows (both Privileged).
- `sandboxing.md:241`: removed `plans/SECURITY.md` reference.

### Phase 8 — `embedding/` — Completed (awaiting PR)

**Inventory** (2026-04-24):

- Last touch of `docs/embedding/`: `2785298c` (post-PR-#662 migration of `SafeExtensions`/`AllExtensions` references to `WithProfile`).
- Relevant code changes since: `99e82370` (replace `SafeExtensions`/`AllExtensions` with `WithProfile` — partially synced), `05da58f2` (`WithInstructions` in mcp-go library, not a Wile EngineOption), `61fb8a78` (MCP `libraries`/`reset` tools + prompts + session hardening — doc partially synced), plus the `disassemble` MCP tool added in the v1.8+ bytecode disassembler work and `ReadExpression` / `MustParseWithSource` added alongside the `Expression` type API (PR #555).

**Findings**:

- **stale** `docs/embedding/api-design.md:212`
  Claim: "The `Engine` uses plain `Run()` internally. The `repl` package uses `RunWithEscapeHandling` for full R7RS continuation escape support. This is a deliberate simplification for the embedding case." Reality: Both entry paths use `RunWithEscapeHandling`. `runCompiled` (used by `Engine.Run` and `Engine.Eval`) calls it at `engine.go:732`; `callCallable` (used by `Engine.Call`) calls it at `engine.go:544`. The claimed "deliberate simplification" never existed.
  Evidence: `engine.go:544`, `engine.go:732`.

- **drift** `docs/embedding/api-design.md:146`
  Claim: "The `ForeignFunction` receives a `MachineContext` and unwrapped arguments." Reality: `type ForeignFunction func(mc CallContext) error`. `CallContext` is the interface (`machine/call_context.go`), `MachineContext` is one concrete implementation. Primitives type-assert to `*MachineContext` only when they need VM-internal operations.
  Evidence: `machine/foreign_closure.go:19`; CallContext interface has 7 methods (`Arg`, `SetValue`, `SetValues`, `Authorizer`, `Context`, `EnvironmentFrame`, `Thread`) per `machine/CLAUDE.md`.

- **drift** `docs/embedding/api-design.md:57`
  Claim: "Parse first expression to `*Expression`." Reality: `Parse` enforces *exactly one* expression — it errors if empty, malformed, OR contains more than one expression. "First expression" misleads into thinking it skips trailing input.
  Evidence: `expression.go:50-53` docstring ("Parse returns a CompilationError if the input is empty, malformed, or contains more than one expression").

- **missing** `docs/embedding/api-design.md:55-66` (Evaluation Methods table)
  Missing entries: `MustParseWithSource`, `ReadExpression`. `ReadExpression` is the REPL-oriented reader variant that accepts `io.Reader` and (unlike `Parse`) allows trailing input; pairs with `IsIncompleteInput` for interactive input handling.
  Evidence: `expression.go:83` (`ReadExpression`), `expression.go:102` (`MustParseWithSource`).

- **missing** `docs/embedding/api-design.md:109-116` (Constructors table)
  Table lists 6 constructors but the public API exposes 14+: `NewBigInteger`/`…FromInt64`/`…FromString`, `NewBigFloat`/`…FromFloat64`/`…FromString`, `NewRational`/`NewRationalFromBigInt`, `NewComplex`/`NewComplexFromParts`, `NewVector`, plus `WrapValue` as an escape hatch for pre-constructed internal values.
  Evidence: `value.go:80-185`.

- **missing** `docs/embedding/api-design.md:157-171` (Options table)
  Missing: `WithContractEnforcement`, `WithMaxCallDepth`, `WithMaxStackSize`, `WithInlineThreshold`, `WithImportObserver`, `WithCoverage`. These are real engine-configuration knobs an embedder should know about.
  Evidence: `options.go:103,113,124,134,169,285`.

- **missing** `docs/embedding/api-design.md:181` (built-in authorizers)
  Claim lists `DenyAll`, `ReadOnly`, `FilesystemRoot`, `All`. Missing: `ConsoleAuthorizer`, `ConsoleWithLoadAuthorizer`, `SandboxAuthorizer(envPrefix)` — which are the authorizers used by the profiles (`Console`, `ConsoleWithLoad`) and by `WithSandbox`.
  Evidence: `security/console_authorizer.go:26`, `security/console_with_load_authorizer.go:28`, `security/sandbox_authorizer.go:26`.

- **missing** `docs/embedding/api-design.md:181` (action vocabulary)
  Claim lists actions `read`, `write`, `delete`, `stat`, `load`, `exit`. Missing: `exec` (structured process execution) and `exec-shell` (shell command execution).
  Evidence: `security/access.go:45-54`.

- **drift** `docs/embedding/api-design.md:166`
  Claim: "`WithSandbox()` | Compose the sandbox env-prefix wrapper with the current authorizer." Reality: `WithSandbox(opts ...SandboxOption) EngineOption` is variadic; takes optional `SandboxEnvPrefix(prefix)` to customize the env var prefix (default `"WILE_"`). Doc should note the variadic option shape and ordering constraint (must appear after `WithProfile`/`WithAuthorizer`).
  Evidence: `sandbox.go:46`.

- **missing** `docs/embedding/api-design.md:138-143` (PrimitiveSpec struct)
  Struct is shown with 4 fields. Actual has 10: also `Doc`, `ParamNames`, `Category`, `ParamTypes`, `ReturnType`, `Keywords`. Documentation should note that extra optional fields exist for documentation, type contracts, and discoverability.
  Evidence: `registry/registry.go:26-42`.

- **missing** `docs/embedding/api-design.md:217-222` (File Reference table)
  Missing files: `expression.go` (Expression/Parse/MustParse/ReadExpression), `profile.go` (Profile + WithProfile), `sandbox.go` (WithSandbox + SandboxOption), `debugger.go` (Debugger type), `error.go` (CompilationError + RuntimeError).
  Evidence: `ls wile root` — all five files present alongside engine.go/value.go/options.go/compiled.go/doc.go.

- **drift** `docs/embedding/source-loading.md:54-65` (OSFileResolver resolution order)
  Claim: order is 1. library registry, 2. SCHEME_INCLUDE_PATH, 3. CWD. Reality: `LoadPathStack.CurrentDir()` is searched FIRST, before library registry paths. Mirrors `FSFileResolver` behavior (which the doc documents correctly at lines 67-79) and is essential for relative `include` from an OS-loaded file. Absolute paths bypass the search list, open directly, still subject to authorization.
  Evidence: `machine/compilation/resolver/os_file_resolver.go:66-68` ("Current load directory from the load path stack (stack-relative, highest priority)") and `os_file_resolver.go:114-128` (`osFSSearchDirs`).

- **drift** `docs/embedding/source-loading.md:36-52` (ASCII diagram — Resolver Implementations overview)
  Diagram's OSFileResolver column shows only 3 steps starting at "LibraryRegistry paths". Same drift as above — needs "LoadPath dir" as step 1 to mirror FSFileResolver.

- **drift** `docs/embedding/source-loading.md:234-255` (ASCII diagram — Resolution Priority)
  Same drift a third time: OSFileResolver subtree starts at "LibraryRegistry search paths"; should start at "LoadPathStack.CurrentDir() + path".

- **missing** `docs/embedding/mcp.md:73-139` (Tools section)
  Missing tool: `disassemble`. Registered at `cmd/wile/mcp.go:180-192`, takes a `name` string parameter (procedure name bound in the session), returns bytecode disassembly text. Part of the bytecode-disassembler work introduced alongside the REPL `,disasm` meta-command.
  Evidence: `cmd/wile/mcp.go:180-192` (`AddTool(NewTool("disassemble"...))`), `handleDisassemble` at `cmd/wile/mcp.go:421-440`.

- **clean** `docs/embedding/source-loading.md:14-31` (FileResolver interface + chain protocol)
  Interface signature at `environment/file_resolver.go:31-34` matches. `ErrFileNotFound`/sourceload-`ErrNotFound` convention verified against `ChainFileResolver.ResolveAndOpen` at `machine/compilation/resolver/chain_file_resolver.go:46-59` (matches the code snippet in the doc nearly byte-for-byte).

- **clean** `docs/embedding/source-loading.md:177-217` (Embedded Standard Library)
  `fs.Sub(rawFS, "lib")` init pattern verified at `stdlib/stdlib.go:44-49`. `//go:embed lib` directive at `stdlib/stdlib.go:36-37`. `DefaultLibraryPaths = [".", "./stdlib/lib"]` verified at `machine/compilation/library_registry.go:153-156`.

- **clean** `docs/embedding/source-loading.md:134-140` (Bootstrap Isolation)
  `core.BootstrapFS` at `registry/core/bootstrap.go:25-31` with `//go:embed bootstrap.scm bootstrap_macros.scm bootstrap_procedures.scm`. Bootstrap resolver instantiated separately at `engine.go:685` (`compilation.NewEmbedFileResolver(core.BootstrapFS)`), not threaded into the user-visible chain.

- **clean** `docs/embedding/source-loading.md:142-162` (Library Import Resolution)
  `.sld`-then-`.scm` extension order verified at `machine/compilation/resolver/helpers.go:35` (`libraryExtensions = []string{".sld", ".scm"}`) consumed by `ResolveLibraryFile` in `machine/compilation/library_registry.go:309-323`. The doc's hand-written example illustrates the behavior without citing the actual function — acceptable as pedagogy.

- **clean** `docs/embedding/source-loading.md:299-318` (Code Locations table)
  All 15 file paths verified present at the cited locations.

- **clean** `docs/embedding/mcp.md:141-186` (Resources section)
  Three resources registered at `cmd/wile/mcp.go:574-601`: `wile://session`, `wile://libraries`, `wile://primitives`. JSON shapes (`sessionState`, `libraryInfo`, `primitiveInfo`) at `cmd/wile/mcp.go:551-571` match the documented examples.

- **clean** `docs/embedding/mcp.md:188-194` (Prompts section)
  Single prompt `wile-scheme` with required `task` argument, registered at `cmd/wile/mcp.go:488-500`.

- **clean** `docs/embedding/mcp.md:14-20` (Flag table + mutual exclusion)
  `--mcp` mutual exclusion with `-e`/`-f`/`-i` enforced at `cmd/wile/main.go:202-205`. `--mcp-timeout` non-negative check at `main.go:206-209`.

**Fixes** (committed in this PR):

- `api-design.md:55-66`: rewrite `Parse` row to clarify "exactly one expression" (not "first"); add `ReadExpression` and `MustParseWithSource` rows.
- `api-design.md:109-116`: expand Constructors table to cover big numerics, rationals, complex numbers, vectors, and `WrapValue`.
- `api-design.md:138-143`: expand `PrimitiveSpec` struct snippet with all 10 fields (4 required + 6 optional) and an inline comment.
- `api-design.md:146`: `MachineContext` → `CallContext` with a sentence pointing at the type-assert escape for VM-internal access.
- `api-design.md:157-171`: expand Options table with `WithContractEnforcement`, `WithMaxCallDepth`, `WithMaxStackSize`, `WithInlineThreshold`, `WithImportObserver`, `WithCoverage`; fix `WithSandbox` row to note variadic options and ordering constraint.
- `api-design.md:181`: add `exec` and `exec-shell` to the action vocabulary; add `ConsoleAuthorizer`, `ConsoleWithLoadAuthorizer`, `SandboxAuthorizer` to the built-in authorizer list with a sentence on how profiles and `WithSandbox` compose them.
- `api-design.md:212`: rewrite the "Design Decisions" entry — the engine *does* use `RunWithEscapeHandling` in both entry paths, with pointer references to the two call sites.
- `api-design.md:217-222`: expand File Reference table with `expression.go`, `profile.go`, `sandbox.go`, `debugger.go`, `error.go`.
- `source-loading.md:36-52`: correct the ASCII diagram — add "LoadPath dir" as step 1 of OSFileResolver, renumber subsequent steps to 4.
- `source-loading.md:54-65`: rewrite OSFileResolver resolution order to list 4 steps with `LoadPathStack.CurrentDir()` first; note absolute-path bypass.
- `source-loading.md:234-255`: correct the Resolution Priority ASCII summary — same fix as the diagram.
- `mcp.md:124-131`: add `disassemble` tool section with parameter table and error semantics, placed between `libraries` and `reset` to mirror the registration order in `cmd/wile/mcp.go`.

**Additional fixes from Copilot + crosscheck review** (commit 2):

Three-lens-converging **Critical**:

- **[types/code/Copilot]** `api-design.md:189` — Options table cited a nonexistent `DefaultMaxStackSize` constant. `WithMaxStackSize` has no default; it is opt-in and `0` also means unlimited. Rewrote the row to match the option's own docstring at `options.go:120-123`.
- **[errors/tests/Copilot]** `mcp.md:136-139` — The added `disassemble` section claimed a Go primitive raises an MCP tool error for "no disassembly available", but `disassemble.go:83-95` explicitly returns a one-line text summary for `*machine.ForeignClosure`. Rewrote to describe the actual behavior (Scheme procs → full bytecode; foreign prims → one-line summary; errors reserved for unbound or non-procedure).
- **[consistency/code]** `source-loading.md:49` — Pre-existing: the EmbedFileResolver row in the resolver-implementations diagram was 60 display columns while every other row was 59. Removed one trailing space per CLAUDE.md "Box-drawing alignment" rule.

**Notable unambiguous**:

- **[types/code]** `api-design.md:236` / `:243` — File Reference table listed `IsIncompleteInput` under `expression.go`; it actually lives at `error.go:134`. Moved to the `error.go` row; added `MustParseWithSource` to the `expression.go` row to match the evaluation-methods table.
- **[Copilot]** `api-design.md:114` — Constructors row listed `NewBigIntegerFromString` without its `base int` parameter. Expanded to `NewBigIntegerFromString(s, base)` so embedders see the required second argument.
- **[Copilot]** `api-design.md:161` — "receives a `CallContext` and unwrapped arguments" — arguments are not received as separate parameters; they are accessed positionally via `mc.Arg(i)`. Rewrote the sentence to describe the actual access pattern (including the variadic-rest convention).
- **[Copilot]** `api-design.md:233` — The escape-handling claim "translates them into the returned error" overstates what `RunWithEscapeHandling` does. Per `machine_context.go:1278-1350`, it installs `DefaultPromptTag` as a top-level prompt, catches `ErrPromptAbort` for *that* tag, restores to the prompt frame, and resumes — the payload becomes the returned value, `err == nil`. Only aborts whose tag has no matching prompt escape as runtime errors. Rewrote accordingly.
- **[errors]** `api-design.md:181` + new "Option ordering" paragraph — The original row said "Must appear after `WithProfile`/`WithAuthorizer`" without naming the consequence. Moved the ordering constraint out of the table into a dedicated paragraph below the Options table that names the silent-overwrite hazard explicitly: `WithAuthorizer` assigns, `WithSandbox` composes via `security.All(...)` only when an authorizer is already set, so a later `WithAuthorizer(...)` silently drops the sandbox with no diagnostic. Embedders reading this now have the failure mode spelled out.

**Findings NOT actioned** (with rationale):

- **[consistency]** Cross-doc authorizer inventory drift between `api-design.md` and `docs/security/sandboxing.md` — `sandboxing.md` doesn't enumerate `WithSandbox`, `SandboxAuthorizer`, or `SandboxEnvPrefix`. This is Phase 7 territory (shipped via PR #716) and would require a separate doc-sweep touch. Out of Phase 8 scope. Tracked for potential Phase 9+ revisit.
- **[tests]** Test coverage gaps — (1) `sandbox_test.go` and `TestProfile_WithSandbox_Composition` are tautological (assert only that construction succeeds; would pass if `WithSandbox` were a no-op); (2) no test exercises the ordering-constraint foot-gun; (3) no test verifies a user-provided virtual FS with `bootstrap.scm` cannot shadow the embedded bootstrap; (4) no test for `--mcp` + `-e/-f/-i` mutual-exclusion error. These are genuine code-change follow-ups, not docs-sweep fixes. Out of Phase 8 scope (the sweep plan excludes Go source changes of any kind).
- **[consistency]** `WithSandbox` row style (3-sentence cell vs 1-clause neighbors) — addressed by the "Option ordering" paragraph move above; the row is now 2 clauses like its neighbors.
- **[consistency/tests]** `mc` receiver identifier ambiguity after changing the documented type to `CallContext` — the name `mc` is conventional in the codebase (`machine/CLAUDE.md`), so this is idiomatic rather than drift. Left as-is.

### Phase 9 — `types/` — Completed (awaiting PR)

**Inventory** (2026-04-24):

- Last doc touch of `docs/types/`: `bf83fa43` (2026-04-15) — topic reorganization with INDEX.md and TOC.md.
- Relevant code changes since: `c717862f` (feat: add opaque record types for abstract data type support), `8b4f5627` (fix: harden opaque record types with validation and doc corrections), `fa600645` (fix: guard NewRecord against nil record type), `5b99df75` (extensible type constraints — TypeConstraint interface). All post-date the doc's last touch.
- The `types/` directory is primarily conceptual: `records-as-formal-types.md` (type theory / product types), `abstract-data-types.md` (existential types / ADTs), `scheme-types-records-mop.md` (SRFI landscape + Wile's position), `racket-structs.md` (Racket comparison). The per-phase concern from the plan is "verify code snippets still compile against the current value type" — which mostly holds. The bigger gaps are *status* claims ("Wile is at Level 1", "Sealed/opaque/nongenerative | Not implemented") that pre-date the opaque-record work.

**Findings**:

- **drift** `docs/types/records-as-formal-types.md:197`
  In-prose companion-doc reference points to `scheme-types-records-and-mop.md` (with "-and-"). Actual filename is `scheme-types-records-mop.md`. Line 310's footer reference uses the correct filename, but the inline mention at line 197 is broken. Cross-doc link drift.
  Evidence: `ls docs/types/` and `grep -n "scheme-types-records" docs/types/`.

- **stale** `docs/types/scheme-types-records-mop.md:292`
  "The Practical Landscape for Wile" table row: `| Sealed/opaque/nongenerative | Not implemented |`. Opacity IS implemented — `NewOpaqueRecordType` at `values/record_type.go:48`, exposed to Scheme as `make-opaque-record-type` (`internal/extensions/all/register.go:84`) and the `define-opaque-record-type` macro (`registry/core/bootstrap_macros.scm:199-202`). Opaque record types return `#f` from `record?` and error on `record-type` per `record_type.go:74-76` + `record.go:131`. The row lumps three independent features together; need to split.
  Evidence: commit `c717862f feat: add opaque record types for abstract data type support`.

- **missing** `docs/types/scheme-types-records-mop.md:286-293` (table)
  No row for `define-opaque-record-type` / `make-opaque-record-type` even though they exist and are user-facing. Should be added with Status: Implemented.

- **stale** `docs/types/scheme-types-records-mop.md:383-385`
  "Wile is at Level 1 with a procedural/inspection layer. The question for Wile is whether to climb to Level 2 (SRFI-99 inheritance) — which is a modest, well-understood extension — or leap further." With opacity shipped, Wile has adopted one Level-4 feature (opacity is R6RS territory per the same doc's "Standards Staircase" at lines 376-380). Calling Wile "Level 1" now understates its position.
  Evidence: Standards Staircase cites "R6RS / SRFI-240" at Level 4 with "sealed, opaque, nongenerative". Opacity is one of three — Wile has it.

- **missing** `docs/types/abstract-data-types.md:209-224` (R6RS Opaque Records section)
  The section frames opacity as an R6RS feature with R6RS `(opaque #t)` syntax, implying R7RS readers lack it. Wile has native `define-opaque-record-type` with R7RS-compatible syntax. Without a "Where Wile is" note, an embedder reading this doc will conclude opacity is unavailable to them. Added a sidebar block with Wile's syntax and semantics.

- **drift** `docs/types/racket-structs.md:271`
  "See also" link caption: `records-as-formal-types.md — Records as existential types and ADTs`. But `records-as-formal-types.md` is about nominally-typed labeled product types (introduction/elimination rules). Existential types / ADTs are the subject of `abstract-data-types.md`. Reworded the `records-as-formal-types.md` caption and added a separate line for `abstract-data-types.md`.

- **clean** `docs/types/records-as-formal-types.md:38-44` (define-record-type example)
  Standard R7RS syntax `(define-record-type <name> (constructor field...) predicate (field accessor)...)` verified against `registry/core/bootstrap_macros.scm:194-197`. Disjointness and generativity claims match the codebase.

- **clean** `docs/types/scheme-types-records-mop.md:87-100` (procedural layer example)
  All six primitives cited (`make-record-type`, `record-constructor`, `record-predicate`, `record-accessor`, `record-modifier`, `record?`, `record-type`, `record-type?`) verified at `internal/extensions/all/register.go:81-110`.

- **clean** `docs/types/racket-structs.md:63-67` ("Where Wile is" callout for opacity)
  Claim that "Opaque records print as `#<point>` instead of `#<record:point>`, and they're invisible to `record?`" matches `values/record.go:124-134` (SchemeString) and `values/record_type.go:74-76` (IsOpaque) + `record_type.go:44-46` comment. Callout is current.

- **clean** All four docs' cross-references to each other (excluding the two drifts above): `abstract-data-types.md:3` → `records-as-formal-types.md`; `abstract-data-types.md:266,268` → sibling docs; `records-as-formal-types.md:197,310` → `scheme-types-records-mop.md` (the inline mention at :197 is the drift above; :310 footer link is already correct); `racket-structs.md:270,272,273` → sibling docs. All resolve.

**Fixes** (committed in this PR):

- `records-as-formal-types.md:197`: fix broken companion-doc reference — `scheme-types-records-and-mop.md` → `scheme-types-records-mop.md`.
- `scheme-types-records-mop.md:286-293`: split the Sealed/opaque/nongenerative row; add "Opaque record types" row as Implemented; clarify Record inheritance row (internal `NewDerivedRecordType` exists but is not exposed at Scheme level).
- `scheme-types-records-mop.md:383-385`: rewrite the "Wile is at Level 1" paragraph to acknowledge opacity as a Level-4 feature already adopted, with sealed/nongenerative still missing.
- `abstract-data-types.md:224` (end of R6RS Opaque Records section): add a "Where Wile is" callout noting that Wile supports opacity with R7RS-compatible syntax (`define-opaque-record-type`), not R6RS's `(opaque #t)` clause; describe the `record?` / `record-type` sealing semantics.
- `racket-structs.md:271`: fix miscaptioned cross-reference. `records-as-formal-types.md` is about product types, not existential types. Added a separate line pointing to `abstract-data-types.md` for "Records as existential types and ADTs".

Style / conceptual prose deferred per plan scope controls.

**Additional fixes from crosscheck review** (commit 2):

- **[tests]** `scheme-types-records-mop.md:292` — reworded "internal `NewDerivedRecordType` exists but is not exposed" to "Go-level `values.NewDerivedRecordType` exists but is not wired to any Scheme primitive". The original "internal" was imprecise because `NewDerivedRecordType` is Go-exported (capital N); the intended meaning was "not reachable from Scheme."
- **[code]** `plans/2026-04-23-docs-sweep-impl.md` clean-cross-references bullet — self-reference typo: cited `scheme-types-records-mop.md:197` but the inline-mention drift is at `records-as-formal-types.md:197`. Fixed.

**Findings NOT actioned** (with rationale):

- **[types]** Optional "— opacity-aware" addendum on the Inspection-layer row of the landscape table. The adjacent "Opaque record types" row already signals opacity-awareness; adding the addendum would be redundant.
- **[tests]** `evalExpectError` weak-assertion improvement — code change out of docs-sweep scope. Logged for a future test-hardening follow-up.
- **[tests]** Link-checker covers only `README.md`, not `docs/` — the very link broken in this PR's finding #1 would have been caught if the checker globbed `docs/**/*.md`. CI/tooling change out of docs-sweep scope. Logged for a follow-up.
- **[errors]** `equal?` on opaque records does structural comparison (bypasses opacity in a certain sense) — not a doc-vs-code mismatch since no doc currently claims otherwise. Potential design-conversation item; not a docs-sweep fix.

### Phase 10 — `dev/` — Completed (PR #719)

**Inventory.** Code-side surface that could have drifted (since 2026-01-01):
`machine/foreign_closure.go` (last touched in PR #335, 2026-02-25); pooling
plumbing in `machine/pool.go`/`machine/pool_generic.go`; `machine/closure.go`
(post-#335 introduction of the `Closure` interface and `NamedCallable`
embedding); the savedCont double-restore fix in PR #573 (touches
`applyForeign` and `callForeignCached`); the migration of `match/` to
`internal/match/`. No `values/freelist*.go`/`values/pool*.go` exist in the
tree — the plan's "Code under verification" list at L321-324 names paths
that never existed; pooling lives in `machine/pool*.go`. Noted; not a docs
finding (the plan itself is ephemeral per its own scope at L49).

**Findings.**

- **drift** `docs/dev/foreign-closure-design.md:33-38` — `Closure` interface
  declaration is incomplete. Doc shows only `values.Callable` + `closureMarker()`;
  code (`machine/closure.go:17-21`) embeds `values.Callable`, `NamedCallable`,
  and `closureMarker()`. The `NamedCallable` embedding (Name + Doc) is consumed
  by `(procedure-name)` and `(procedure-documentation)` at
  `registry/core/prim_reflection.go:136,275`.
  Evidence: `machine/closure.go:17-21`, `machine/machine_closure.go:51`,
  `machine/foreign_closure.go:79`.
- **stale** `docs/dev/foreign-closure-design.md:27-29` — Claim that
  `applyForeign` does "panic recovery" is wrong. `applyForeign`
  (`machine/machine_context_apply.go:89-181`) has no `defer recover()`. Panic
  recovery exists only in `OperationForeignFunctionCall`
  (`machine/operations_call.go:66-101`), the bytecode path used by
  `NewVMForeignClosure` — itself documented at L150 of the same doc as having
  "zero callers in production code." The fast-path callers (`applyForeign`,
  `callForeignCached`) propagate panics through the Go stack; primitives
  reachable via these paths must return errors, not panic. Doc-only fix; the
  underlying behavioral asymmetry (fast path lacks recovery for performance)
  is acknowledged in the new doc text but not enforced by test/lint and is
  logged below as a deferred follow-up.
  Evidence: `machine/machine_context_apply.go:89-181` (no recover);
  `machine/operations_call.go:70-101` (recover present).
- **drift** `docs/dev/foreign-closure-design.md:67-98` (Edge Case 1) — Section
  describes only the `savedTemplate` guard; code
  (`machine/machine_context_apply.go:123-179`) has both `savedTemplate` AND
  `savedCont` guards. The savedCont guard handles a distinct case (foreign
  closure invoking `ApplyCallable` on another `*ForeignClosure` via
  `PrimCallCC` inline mode, where the nested `applyForeign` already consumed
  the saved continuation). Per `MEMORY.md` "savedCont Double-Restore Fix
  (PR #573)" and the analogous (non-identical) guard in
  `machine/call_foreign_cached.go:83-126`.
  Evidence: `machine/machine_context_apply.go:129-130, 165-179`.
- **drift** `docs/dev/pooling.md:14` — Cross-reference to
  `CONTINUATION_WORKLOAD_OPTIMIZATIONS.md` is a stale path. The actual document
  is `docs/continuations/optimizations.md` (verified content matches: opens
  with "Continuation-Heavy Workload Optimizations").
- **drift** `docs/dev/debug-methodology.md:145` — File path
  `match/syntax_adapter.go` is stale. The match package was moved into
  `internal/`; current path is `internal/match/syntax_adapter.go`.
- **clean** `docs/dev/project-board-setup.md` — Operational guide for GitHub
  Projects v2 UI workflow; no code-side claims to verify. The plan's prep note
  flagged this as potentially stale-if-workflow-changed, but the workflow
  described matches current GH Projects v2.

**Fixes.**

- `docs/dev/foreign-closure-design.md:27-33`: rewrote the `applyForeign`
  capability sentence — replaced "panic recovery" with "error conversion",
  added explicit note that recovery remains in `OperationForeignFunctionCall`
  only, and named the genuine panic sources (`values/promotion.go`,
  `values/numeric_tower.go` panicking on `ErrNotANumber`/`ErrNotAPair`).
  Added "code-review-enforced contract; no test/lint backing" sentence to
  acknowledge the behavioral hazard explicitly.
- `docs/dev/foreign-closure-design.md:38-42`: added `NamedCallable` line to
  the `Closure` interface code block with corrected gloss
  (`(procedure-name)` and `(procedure-documentation)` only — not stack
  traces, which use `*NativeTemplate.Name()` directly per
  `machine/machine_context.go:1002,1015`).
- `docs/dev/foreign-closure-design.md:50-66`: added two paragraphs explaining
  the `NamedCallable` consumer surface (citing
  `registry/core/prim_reflection.go:136,275`), distinguishing it from the
  stack-trace mechanism, and noting that `closureMarker()` enforces package
  locality while "direct invocation" is convention enforced at
  `ApplyCallable`'s dispatch.
- `docs/dev/foreign-closure-design.md:78-156`: rewrote Edge Case 1 from a
  single template-pointer guard to a Case A (template change) + Case B
  (continuation already consumed) treatment with the dual-guard code snippet
  (`savedTemplate` + `savedCont`). Cited the matching but non-identical
  guard structure in `machine/call_foreign_cached.go:83-126` (analogous, not
  symmetric — tail path calls `returnImmediate()` unconditionally). Added a
  closing **Generality** paragraph noting the guards fire defensively for any
  callable in `ApplyCallable`'s dispatch table, not only the named cases.
- `docs/dev/pooling.md:14`: retargeted cross-reference from
  `CONTINUATION_WORKLOAD_OPTIMIZATIONS.md` to
  `docs/continuations/optimizations.md`.
- `docs/dev/debug-methodology.md:145`: corrected file path from
  `match/syntax_adapter.go` to `internal/match/syntax_adapter.go`.

**Findings NOT actioned (logged for follow-up).**

- **[errors]** Behavioral asymmetry: `applyForeign` and `callForeignCached`
  (the fast paths) lack `defer recover()`, while `OperationForeignFunctionCall`
  (the dead bytecode path used by `NewVMForeignClosure`, which has zero
  production callers) has it. The new doc text acknowledges this, but the
  underlying contract — "primitives reachable via the fast path must not
  panic" — is enforced only by code review. Options for a follow-up plan:
  (a) reinstate `defer recover()` in the two fast-path entry points and
  benchmark the cost; (b) add a `wile-goast` belief that flags primitives
  transitively calling panic-prone helpers in `values/promotion.go` and
  `values/numeric_tower.go`. Out of docs-sweep scope per plan L54
  ("Go source changes of any kind").
- **[errors]** `machine/CLAUDE.md` "Error priority in `OperationForeignFunctionCall`"
  block describes a path no production primitive takes anymore (per
  `NewVMForeignClosure` having zero callers, doc L150). Subsystem `CLAUDE.md`
  files are out of docs-sweep scope per plan L46. Logged for a separate
  `CLAUDE.md` audit.
- **[tests]** No unit test in `machine/*_test.go` pins the savedCont guard in
  either `applyForeign` or `callForeignCached`. The PR #573 fix is currently
  protected only by `integration/testdata/r7rs-tests.scm:1650`
  (`(call-with-current-continuation procedure?)`), which is coarse-grained.
  Out of docs-sweep scope (Go test changes); logged for the same follow-up
  plan as the panic-recovery audit above.
- **[tests]** No affirmative negative test for the "panicking `*ForeignClosure`"
  contract; `TestApplyForeign_PanicRecovery` was removed at
  `machine/foreign_closure_apply_test.go:124-127` without a replacement.
- **[plan]** "Code under verification" list at L321-324 names
  `values/freelist*.go` and `values/pool*.go` — neither path exists; pooling
  lives in `machine/pool*.go`. Plan files are ephemeral per the sweep's own
  scope (L49); not a docs finding.

### Phase 11 — `learn/` — Pending

### Phase 12 — `coverage/` — Pending

## References

- Sibling plan: `2026-04-23-algebra-docs-impl.md` (algebra-specific docs
  work; must ship first).
- Doc conventions: `docs/CLAUDE.md`.
- Plan conventions: `plans/CLAUDE.md`.
