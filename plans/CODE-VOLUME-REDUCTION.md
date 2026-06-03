# Code Volume Reduction

**Date:** 2026-06-01
**Source:** Multi-agent workflow audit (`code-volume-reduction-audit`) — 18 discovery
units across all production packages + cross-package duplication + dead-code triage,
each candidate adversarially verified by an independent skeptic (reachability,
public-API safety, semantic-equivalence, perf hot-path). 71 raw candidates → **52
confirmed**.
**Status:** **Phase 1 (dead code) SHIPPED** (`cleanup/remove-dead-code`, merged to master).
**Phase 2 (cross-extension helpers) SHIPPED** (`cleanup/code-volume-phase2`, commit `369c4f98`;
net −74 prod LOC; `make lint` 0 issues + `make covercheck` 41/41 ≥80% green). Phases 3-5 pending
(Phase 3/4 scope reconciled 2026-06-03 — see below).
**Related plans:**
- `2026-05-08-dispatch-axis-as-data.md` — names the "parallel-variant" pattern (the
  factory theme). The factory work below is its continuation. See `FACTORY-AUDIT.md`.
- `2026-05-29-repeated-stanzas-audit.md` — earlier sweep for repeated 3-10 line stanzas;
  several boilerplate findings here are its descendants.
- `2026-05-29-car-cdr-consolidation.md` — COMPLETE exemplar of factory/helper
  consolidation (~600 LOC); the structural template for the factory work.
- `2026-05-18-registry-structural-reduction.md` — Phases 0-3 shipped; deferred Phases 5
  (ArgShape) and 6 (unification) overlap the registry boilerplate findings here.

## Candid framing

645 LOC over 84k production lines is **~0.77%**. This codebase is *tight*, not bloated.
The value here is **structural** — one recurring "N near-identical primitives that
should be one factory call" pattern — and the **pure dead-code deletions** (~31% of the
total, near-zero risk). Treat this as a cleanup map, not a fat-trimming emergency. Do not
chase the long tail (ranks 45-51 net ≤2 LOC each, some zero/negative) except as
ride-alongs when already editing the file.

## By kind (skeptic-adjusted)

| Kind | Count | LOC |
|------|------:|----:|
| dead-code | 6 | 198 |
| duplicate | 17 | 189 |
| parallel-variant | 9 | 120 |
| boilerplate | 11 | 99 |
| over-abstraction | 7 | 36 |
| unrolled-loop | 1 | 3 |
| **Total** | **51** | **645** |

---

## Phase 1 — Dead code (✅ SHIPPED, branch `cleanup/remove-dead-code`)

Pure deletions, near-zero behavioral risk. ~215 production LOC + 122 test LOC.
`make lint` (0 issues) and `make covercheck` (exit 0) both green.

| Item | Location | Note |
|------|----------|------|
| `PrimDynamicWind` | `registry/core/prim_control.go` | Never registered; dynamic-wind is compiled to bytecode (`machine/dynamic_wind.go`). Doc already omitted it. |
| `AnalyzePatternWithLiterals` + `collectPatternVariables` + `collectPatternVariablesWithEllipsis` + 2 tests | `internal/match/pattern_analyzer*.go` | All three orphan together (forwarder's worker dies with it). `AnalyzePattern` stays (used by `syntax_compiler.go`). |
| `WalkFSSchemeFiles` | `machine/compilation/resolver/helpers.go` | Orphaned after `FSFileResolver` switched to `sourceload.Walk`. Sibling `WalkOSSchemeFiles` stays. |
| `exponentMarkerStrength` + unreachable `if !ok` branch + orphaned `MessageExpectingExponentMarker` | `internal/tokenizer/tokenizer_{predicates,numbers}.go`, `tokenizer.go` | The line-493 `isExtendedExponentMarker` guard accepts the exact same char set, so `ok` is always true → branch provably dead. |
| `io.StringValue` | `internal/extensions/io/state.go` | Exported but zero callers; internal package, not embeddable. |
| `isListOpener` | `internal/parser/parser.go` | Was `//nolint:unused` (already known dead). |

**Excluded false positives** (deadcode tool flagged, triage rejected): `ruleguard/*` rules
(reflection-invoked by the ruleguard framework), `repl`/`options.go` `With*` options (public
embedder API), `registry/testhelpers` exports (cross-package test use), `machine.NewVMForeignClosure`
(intentionally kept zero-caller design point, documented in `machine/CLAUDE.local.md`).

**Gotcha encountered:** deleting code shifts `file:line` numbers, staling
`plans/axis-b-manifest.scm` (golden manifest of registered primitives). Regenerate with
`WILE_AXIS_B_UPDATE=1 go test -run TestBuildAxisBManifest .`. Verify the diff shows only
`loc=` shifts, not added/removed primitives (set stays 465).

---

## Cross-cutting themes (the real takeaway)

1. **Extension/registry primitive registration is pervasively factory-able.** Findings 6,
   8, 30, 38, 43 (and more uncaught) all reduce to "N near-identical primitives → one
   factory call." The pattern **already exists** (`registry/helpers.MakeNumericPredicate`/
   `MakeCharTransform`, `extensions/math.makeRealNumberPrimitive`) — these are unconverted
   call sites, not missing abstractions. **→ `FACTORY-AUDIT.md`** treats this as its own
   sweep; it will find more than the individually-listed items.
2. **Cross-extension copy-paste** between `threads`↔`gointerop` (findings 10, 22) and
   `eval`↔`namespace` (finding 4). Natural sink: `registry/helpers` (above `values`, below
   `extensions`, already imported by both).
3. **Numeric/parse formatting duplication** — float-to-string and exponent-marker logic
   copied across `values/float.go`↔`complex.go` (11), `parser`↔`math` (`normalizeExponentMarker`
   byte-identical, 25), and reinlined helpers (`cmpFloat64` ×2, finding 19). **No single home**
   (verified 2026-06-03): only F25 (a pure string fn) lands in `internal/schemeutil`; F11/F19
   operate on `values` types and stay in `values/`; F12/F28/F39 stay in `internal/parser`. See
   the Phase 3 reconciliation table.
4. **The no-single-line-function rule is the dominant LOC-deflator.** Every "extract a
   helper" finding loses lines to the mandatory multi-line helper body. Net savings are
   already adjusted down for this, but it caps the realistic yield of the duplicate/boilerplate
   tiers.

---

## Confirmed findings (ranks 4-51; rank 1-3, 7, 27, 34 are Phase 1 dead code)

Ranked by (LOC × confidence), low-risk first. Phase grouping below the table.

> **Line numbers in this table are as-of the 2026-06-01 audit and have drifted**
> (Phase 1+2 deletions + the May structural-reduction wave shifted them). For Phase 3
> and Phase 4 — the next phases up — use the **verified locations** in the
> "Phase 3 & 4 scope reconciliation" section below, not this table.

| # | Kind | LOC | Conf | Title | Locations |
|--:|------|----:|------|-------|-----------|
| 4 | duplicate | 36 | high | ✅ **DONE (P2)** Import-spec load/apply/copy loop dup ×3 → `compilation.ImportSpecInto` | `extensions/eval/prim_eval.go:376`; `internal/extensions/namespace/prim_namespace.go:75,251` |
| 5 | duplicate | 30 | high | Foreign-error triage block ×3 → **reuse existing `applyCallableError`** | `machine/operations_call.go:85,105`; `machine/machine_context_apply.go:135` |
| 6 | parallel-variant | 30 | high | char-set union/intersect/diff/xor → `makeCharSetFold` factory | `extensions/charsets/charsets.go:313,326,339,352` |
| 8 | parallel-variant | 20 | high | write/display/write-simple/write-shared → one factory | `internal/extensions/io/prim_write.go:48,85,118,138` |
| 9 | parallel-variant | 18 | high | `LocalIndex` accessors dup slot/depth parent-walk | `environment/environment_frame.go:618,644,660,677` |
| 10 | boilerplate | 16 | high | ✅ **DONE (P2)** nil-to-Void guard ×6 across threads + gointerop → `values.ValueOrVoid` | `extensions/threads/prim_threads.go:170,308,358,554`; `extensions/gointerop/prim_gointerop.go:485,517` |
| 11 | duplicate | 15 | high | `Float.SchemeString` dup `formatComplexComponent` float fmt | `values/float.go:327`; `values/complex.go:375` |
| 12 | boilerplate | 15 | high | "optionally-inexact number → syntax wrap" tail dup | `internal/parser/parser_number.go:110,132,145`; `parser.go:760,796,811` |
| 13 | duplicate | 14 | high | Splice-segment machinery dup quasiquote vector↔list | `machine/compilation/compile_time_continuation_quasiquote.go:135`; `quasi_expand.go:330` |
| 14 | boilerplate | 13 | high | Named-let should reuse `createBindingEnv` | `machine/compilation/expander_let.go:159,366` |
| 15 | parallel-variant | 13 | high | `markCapturedBindings`/`markEscapedBindings` share scaffolding | `internal/validate/validate_capture.go:39`; `validate_escape.go:40` |
| 16 | duplicate | 12 | high | Inline docstring render dup `tryStructuredBindingDoc` | `repl/meta.go:477,516` |
| 17 | boilerplate | 12 | high | ✅ **DONE (P2)** `PrimChannelTryReceive` hand-codes 3 bool→Boolean → `values.BoolToBoolean` | `extensions/gointerop/prim_gointerop.go:135` |
| 18 | boilerplate | 18 | med | Per-type identity-object scaffolding ×8 concurrency types | `values/{atomic,channel,condition_variable,mutex,once,rw_mutex,thread,wait_group}.go` |
| 19 | duplicate | 10 | high | Two complex real-part compares reinline `cmpFloat64` | `values/complex.go:106,254`; `values/promotion.go:355` |
| 20 | duplicate | 10 | high | `NewApplyFrame` dups `InitApplyFrame` parent-copy | `environment/environment_frame.go:165,187` |
| 21 | duplicate | 10 | high | Phase-env binding lookup walk dup cmdDoc↔DisassembleBinding | `repl/meta.go:298,781` |
| 22 | duplicate | 10 | high | ✅ **DONE (P2)** Optional resource-name extraction dup threads↔gointerop → `helpers.OptionalName` | `extensions/threads/prim_threads.go:58`; `extensions/gointerop/prim_gointerop.go:280` |
| 23 | over-abstraction | 9 | high | Three scope-set wrappers, pure pass-through, no prod callers | `internal/syntax/scope_utils.go:37,42,46` |
| 24 | duplicate | 9 | high | Doc-only PrimitiveSpec construction dup ×2 | `wile/engine.go:886,936` |
| 25 | duplicate | 9 | high | `normalizeExponentMarker` byte-identical parser↔math | `internal/parser/parser_number.go:189`; `extensions/math/prim_conversion.go:297` |
| 26 | boilerplate | 8 | high | `PrimErrorContextSource/ObjectSource` reimplement `StringOrFalse` | `registry/core/prim_error_context.go:44,90` |
| 28 | parallel-variant | 13 | med | `parseComplex` dups `ParseComplexStringNumber` sign-scan | `internal/parser/parser_number.go:416`; `number_string.go:89` |
| 29 | over-abstraction | 7 | high | `NewTokenizerWithComments` pure pass-through, stale doc | `internal/tokenizer/tokenizer.go:287`; `internal/parser/parser.go:141` |
| 30 | over-abstraction | 6 | high | `makeCharComparePrimitive`/`makeStringComparePrimitive` single-use forwarders | `registry/core/prim_characters.go:69`; `prim_strings.go:302` |
| 31 | boilerplate | 6 | high | ✅ **DONE (P2)** Repeated `MachineContext` checked-cast prologue ×7 → `machine.RequireMachineContext` | `extensions/eval/prim_eval.go:52,116,417,456,547,634,684` |
| 32 | over-abstraction | 5 | high | `collectPatternVariables` thin wrapper (machine/compilation — distinct from match) | `machine/compilation/compile_syntax_rules.go:392`; `compile_syntax_case.go:186` |
| 33 | boilerplate | 5 | high | Fixed-arity validators hand-roll instead of `validateBodySlice` | `internal/validate/validate_dynamic_wind.go:36`; `validate_cont_mark.go:31` |
| 35 | duplicate | 5 | high | ✅ **DONE (P2)** `environment-bound-names`/`namespace-bound-names` identical bodies → `(*Namespace).BoundSymbolNames` | `extensions/introspection/prim_introspection.go:47`; `internal/extensions/namespace/prim_namespace.go:222` |
| 36 | parallel-variant | 14 | low | `fuseCallForeignCached`/`fuseCallGeneric` share call-site scan skeleton | `machine/peephole.go:208,415` |
| 37 | duplicate | 4 | high | `compileValidatedCall`/`CompileValidatedApply` dup emit loop | `machine/compilation/compile_validated.go:608,864` |
| 38 | parallel-variant | 4 | high | `eq?`/`eqv?`/`equal?` identical except comparator | `registry/core/prim_equality.go:26,41,50` |
| 39 | over-abstraction | 4 | high | `parseRealPart` single-caller pass-through | `internal/parser/parser_number.go:535` |
| 40 | over-abstraction | 4 | high | `containsString` pass-through over `slices.Contains` | `repl/meta.go:264` |
| 41 | duplicate | 4 | high | `EvalIn` re-inlines `compileExpr` (hardcodes `p.env`) | `wile/engine.go:381,709` |
| 42 | unrolled-loop | 3 | high | Max-name-width compute ×3 in cmdApropos/Topic/Libraries | `repl/meta.go:620,685,717` |
| 43 | parallel-variant | 5 | med | `PrimSyntaxLine/Column/Position` parallel int accessors | `registry/core/prim_syntax_loc.go:66,78,90` |
| 44 | duplicate | 4 | med | Captured-field child-namespace literal dup ×2 | `environment/namespace.go:634,661` |
| 45 | boilerplate | 2 | high | Macro-binding lookup dup ExpandSyntaxExpression↔ExpandOnce | `machine/compilation/expander_time_continuation.go:213,425` |
| 46 | boilerplate | 2 | high | Intro-scope trailer dup in `applyHygieneToSymbol` | `internal/match/syntax_expand.go:251,290` |
| 47 | duplicate | 2 | high | Command-alias help render dup meta.go↔debug.go | `repl/meta.go:242`; `repl/debug.go:300` |
| 48 | parallel-variant | 3 | med | Hashtable `Keys()`/`Values()` identical except projection | `values/hashtable.go:188,202` |
| 49 | duplicate | 5 | low | `IntegerEqualsFloat`/`BigIntegerEqualsFloat` share exact-compare tail | `values/numeric_tower.go:154,180` |
| 50 | over-abstraction | 1 | high | `readBigNum` `isExpMarker` param single-valued | `internal/tokenizer/tokenizer_numbers.go:115`; `tokenizer_hash.go:199,203` |
| 51 | boilerplate | 2 | low | "advance current entry + record last" idiom ×4 | `internal/match/syntax_compiler.go:222,230,254,269` |

---

## Phase 3 & 4 scope reconciliation (verified 2026-06-03)

Every Phase 3 and Phase 4 finding was re-checked against current `master` (post Phase 1+2
and post PRs #739–#741). This section supersedes the table's `file:line` cells for these two
phases and resolves the cross-phase overlaps so the phases are **independent and accurate**.

### Two findings change phase or drop

- **F39 `parseRealPart` moves Phase 4 → Phase 3.** It lives in `internal/parser/parser_number.go`
  (verified `:536`) — the *same file* as Phase 3's F12, F25, F28. Splitting that file across two
  phases guarantees a rebase conflict. F39 is also thematically numeric-parse, not a generic
  intra-package cleanup. Moving it makes `parser_number.go` a single-phase file.
- **F41 dropped — already resolved.** The finding ("`EvalIn` re-inlines `compileExpr`,
  hardcodes `p.env`") no longer holds. The shared compile core **already exists** as
  `expandAndCompileOptimized` (`engine.go:700`) and is reused at `:387`, `:710`, `:821`.
  `EvalIn` (`:381`) deliberately threads a caller-supplied env, so it *cannot* call the
  `p.env`-hardcoded `compileExpr` (`:709`) — that is correct, not duplication. No action.
- **F47 demoted to tail.** 2 LOC alias-help render dup (`repl/meta.go:244`; `repl/debug.go:302`,
  byte-identical) — below the plan's own ≤2-LOC ride-along bar (matches F45, F46). Ride-along
  only, when `meta.go` is already open for F16/F21/F40/F42.

### Phase 3 — verified locations + sink (NOT a single `schemeutil` sink)

The plan header says "Theme 3 → `internal/schemeutil`." Verified: **only F25 lands there.**
Per the FACTORY-AUDIT "lowest package owning the operand type" rule (the same deviation Phase 2's
commit already established), the rest land beside their operands:

| # | Symbol(s) | Verified location | Sink |
|--:|-----------|-------------------|------|
| 11 | `Float.SchemeString` ↔ `formatComplexComponent` | `values/float.go:327`; `values/complex.go:375` | **`values/`** |
| 12 | `makeExact`/`makeInexact` convert-and-rewrap pair | `values`… → `internal/parser/parser_number.go:555,655` (call sites `:250,:256`; `parser.go:825,827`) | **`internal/parser/`** |
| 19 | real-part compare reinlines `cmpFloat64` | `values/complex.go:106,254` (helper already at `values/promotion.go:355`) | **`values/`** (reuse existing) |
| 25 | `normalizeExponentMarker` — **byte-identical** (verified `diff`) | `internal/parser/parser_number.go:189`; `extensions/math/prim_conversion.go:297` | **`internal/schemeutil/`** |
| 28 | `parseComplex` ↔ `ParseComplexStringNumber` sign-scan | `internal/parser/parser_number.go:416`; `number_string.go:89` | **`internal/parser/`** |
| 39 | `parseRealPart` single-caller pass-through (moved from P4) | `internal/parser/parser_number.go:536` | **`internal/parser/`** (inline) |

Net Phase 3 ≈ **67 LOC**. Files touched: `values/` (F11, F19), `internal/parser/parser_number.go`
(F12, F28, F39), `extensions/math` + `internal/schemeutil` (F25). No file shared with Phase 4.
Watch tail-F49 (`values/numeric_tower.go:154,182`) — only if `numeric_tower.go` is opened (bench-gated).

### Phase 4 — verified locations (intra-package, after F39 out / F41 dropped / F47 → tail)

| # | Symbol(s) | Verified location | Notes |
|--:|-----------|-------------------|-------|
| 5 | foreign-error triage → reuse `applyCallableError` | `operations_call.go:86,106` (2 inline); `machine_context_apply.go:135` (1 inline) | helper at `foreign_closure.go:48`, **already reused** at `machine_context_apply.go:119,258,280` — 3 inline blocks remain |
| 13 | quasiquote splice-segment machinery | `compile_time_continuation_quasiquote.go` (`expandQuasiquoteVector`, `segSplice` ~`:96–145`); `quasi_expand.go:260` (`expandQuasiListWithSplice`) | |
| 14 | named-let should reuse `createBindingEnv` | `machine/compilation/expander_let.go:366` (def); callers `:115,:239` | |
| 16 | inline docstring render `tryStructuredBindingDoc` | `repl/meta.go:478`; callers `:504,:510` | |
| 20 | `NewApplyFrame` dups `InitApplyFrame` parent-copy | `environment/environment_frame.go:165,187` | concurrent-refactor caveat now STALE (lines stable) |
| 21 | phase-env binding walk cmdDoc ↔ DisassembleBinding | `repl/meta.go:308`; `repl/meta.go:777` (walk `:789`) | |
| 23 | 3 scope-set pass-throughs to `values.*`, no prod callers | `internal/syntax/scope_utils.go:37,42,47` | callers are **test-only** (`coverage_test.go`, `coverage_extra_test.go`, `syntax_vector_test.go`) — delete + redirect tests to `values.*` |
| 24 | doc-only `PrimitiveSpec` construction dup ×2 | `engine.go:896,946` (`AddDocOnlyPrimitive`) | |
| 29 | `NewTokenizerWithComments` pure pass-through, stale doc | `internal/tokenizer/tokenizer.go:289`; sole prod caller `internal/parser/parser.go:135` | |
| 30 | `makeChar`/`makeStringComparePrimitive` single-use forwarders | `registry/core/prim_characters.go:69`; `prim_strings.go:302` | |
| 32 | `collectPatternVariables` thin wrapper (compilation, distinct from match) | `compile_syntax_rules.go:392`; `compile_syntax_case.go:187` | |
| 37 | `compileValidatedCall` ↔ `CompileValidatedApply` emit loop | `compile_validated.go:592,858` | |
| 40 | `containsString` pass-through over `slices.Contains` | `repl/meta.go:265` | |
| 42 | max-name-width compute ×3 | `repl/meta.go:622,687,716` (`cmdApropos`/`cmdTopic`/`cmdLibraries`) | 3 LOC — ride-along |
| 48 | hashtable `Keys()`/`Values()` projection | `values/hashtable.go:188,202` | |

Net Phase 4 ≈ **120 LOC**. **Intra-phase sequencing** (each file, one pass, to avoid self-rebasing):
`repl/meta.go` = F16+F21+F40+F42 (+F47 ride-along, +debug.go); `machine/compilation` = F13+F14+F32+F37;
`engine.go` = F24; `values/` = F48. None of these files overlap Phase 3.

## Corrections the skeptics found (MUST apply at implementation time)

1. **Finding 5 (foreign-error triage):** the discovery agent prescribed adding a *new*
   `triageForeignError` helper, but `applyCallableError` (`machine/foreign_closure.go:48-62`)
   **already exists** with an identical body. Reuse it — adding a new one creates fresh
   duplication. Keep the defer-site preamble (`rmc=nil`, panic→error) intact; replace only the
   triage tail with `rerr = applyCallableError(mc, err)`.
2. **Finding 2 (Phase 1, already handled):** the discovery agent said the worker
   `collectPatternVariablesWithEllipsis` "stays, still used" — gopls proved it orphans. All
   three were deleted. (Already done; noted for the record.)

---

## Caveats before acting

- **Preserve sentinels, not message strings.** Every helper extraction must keep the same
  `werr` sentinel; per-site message text may drift (tests use `errors.Is`). Same rule
  `car-cdr-consolidation.md` followed.
- **Hot-path: only 2 items.** Finding 49 (numeric-tower `EqualsFloat` tail — run
  `make bench-gabriel`) and finding 36 (peephole scanner — compile-time, but assert
  byte-identical bytecode across the 23 peephole tests). The VM dispatch loop and
  `BySlotDepth` hot targets are untouched. Finding 9's `LocalIndex` delegation leaves the
  VM-hot path alone.
- **Public-API `func→var` (findings 38, 43)** are theoretically observable to embedders
  taking `&PrimX`; precedent exists in-package (`PrimBoxQ` etc.) so accepted — flag for
  maintainer.
- **~~`environment_frame.go` concurrent modification~~** (STALE 2026-06-03): the
  `NewApplyFrame`/`InitApplyFrame` refactor is no longer in progress; both sit at
  `environment_frame.go:165,187`. F20 may be implemented directly. (F9's `LocalIndex`
  accessors are unassigned to any phase — separate decision.)
- **Findings 45-47, 49-51 net ≤2 LOC**, some zero/negative. Single-source-of-truth/readability
  wins, not volume wins. Do only as ride-alongs. (F47 demoted here on 2026-06-03.)
- **F5 is partly done** (verified 2026-06-03): `machine_context_apply.go` already routes 3 sites
  through `applyCallableError`. Three inline triage blocks remain — `operations_call.go:86,106`
  and `machine_context_apply.go:135`. Per Correction 1 below, reuse the existing helper; do
  **not** add a new one.

---

## Recommended phasing

Each phase = its own branch off `master`, one commit per logical group, `make lint &&
make covercheck` green before claiming done. Preserve sentinels; tests via `errors.Is`.

- **Phase 1 — Dead code.** ✅ SHIPPED (`cleanup/remove-dead-code`). ~337 LOC (215 prod).
- **Phase 2 — Cross-extension helpers.** ✅ SHIPPED (`cleanup/code-volume-phase2`, commit
  `369c4f98`). Findings 4, 10, 17, 22, 31, 35. Net −74 prod LOC. Sinks deviated from the
  planned `registry/helpers` per FACTORY-AUDIT "lowest package owning the operand type":
  `compilation.ImportSpecInto` (F4), `values.ValueOrVoid` (F10), `values.BoolToBoolean` (F17),
  `helpers.OptionalName` (F22), `machine.RequireMachineContext` (F31),
  `(*environment.Namespace).BoundSymbolNames` (F35). +`threads` coverage 79.2%→90.2%.
- **Phase 3 — Numeric/parse formatting.** Findings 11, 12, 19, 25, 28, **+39 (moved in from
  Phase 4)**. ~67 LOC. **Split sink** (verified — see reconciliation): F11/F19 → `values/`,
  F12/F28/F39 → `internal/parser/`, F25 → `internal/schemeutil/`. *Not* a single `schemeutil`
  sink. Owns `parser_number.go` exclusively. Watch tail-F49 (bench).
- **Phase 4 — Intra-package dedup/over-abstraction.** Findings 5 (reuse!), 13, 14, 16, 20,
  21, 23, 24, 29, 30, 32, 37, 40, 42, 48. ~120 LOC. Per-package, low risk. **Changes from the
  original list (see reconciliation):** F39 moved to Phase 3; **F41 dropped** (already resolved —
  `expandAndCompileOptimized` is the shared core); **F47 demoted to tail** (2 LOC). F42 (3 LOC)
  is a ride-along. Sequence by file: `repl/meta.go` (16/21/40/42), `machine/compilation`
  (13/14/32/37), `engine.go` (24), `values/` (48).
- **Phase 5 — Factory sweep.** Findings 6, 8, 38, 43 + the broader audit. → **`FACTORY-AUDIT.md`.**
  Defer until Phases 2-4 settle (they reshape `registry/helpers`, the factory sink).

Tail items (18, 33, 36, 44, 45, 46, 47, 49, 50, 51) ride along when their file is already open.
