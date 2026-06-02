# Code Volume Reduction

**Date:** 2026-06-01
**Source:** Multi-agent workflow audit (`code-volume-reduction-audit`) — 18 discovery
units across all production packages + cross-package duplication + dead-code triage,
each candidate adversarially verified by an independent skeptic (reachability,
public-API safety, semantic-equivalence, perf hot-path). 71 raw candidates → **52
confirmed**.
**Status:** **Phase 1 (dead code) SHIPPED** on branch `cleanup/remove-dead-code`
(this session). Phases 2-5 pending.
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
   byte-identical, 25), and reinlined helpers (`cmpFloat64` ×2, finding 19). Home:
   `internal/schemeutil`.
4. **The no-single-line-function rule is the dominant LOC-deflator.** Every "extract a
   helper" finding loses lines to the mandatory multi-line helper body. Net savings are
   already adjusted down for this, but it caps the realistic yield of the duplicate/boilerplate
   tiers.

---

## Confirmed findings (ranks 4-51; rank 1-3, 7, 27, 34 are Phase 1 dead code)

Ranked by (LOC × confidence), low-risk first. Phase grouping below the table.

| # | Kind | LOC | Conf | Title | Locations |
|--:|------|----:|------|-------|-----------|
| 4 | duplicate | 36 | high | Import-spec load/apply/copy loop dup ×3 → `compilation.ImportSpecInto` | `extensions/eval/prim_eval.go:376`; `internal/extensions/namespace/prim_namespace.go:75,251` |
| 5 | duplicate | 30 | high | Foreign-error triage block ×3 → **reuse existing `applyCallableError`** | `machine/operations_call.go:85,105`; `machine/machine_context_apply.go:135` |
| 6 | parallel-variant | 30 | high | char-set union/intersect/diff/xor → `makeCharSetFold` factory | `extensions/charsets/charsets.go:313,326,339,352` |
| 8 | parallel-variant | 20 | high | write/display/write-simple/write-shared → one factory | `internal/extensions/io/prim_write.go:48,85,118,138` |
| 9 | parallel-variant | 18 | high | `LocalIndex` accessors dup slot/depth parent-walk | `environment/environment_frame.go:618,644,660,677` |
| 10 | boilerplate | 16 | high | nil-to-Void guard ×6 across threads + gointerop | `extensions/threads/prim_threads.go:170,308,358,554`; `extensions/gointerop/prim_gointerop.go:485,517` |
| 11 | duplicate | 15 | high | `Float.SchemeString` dup `formatComplexComponent` float fmt | `values/float.go:327`; `values/complex.go:375` |
| 12 | boilerplate | 15 | high | "optionally-inexact number → syntax wrap" tail dup | `internal/parser/parser_number.go:110,132,145`; `parser.go:760,796,811` |
| 13 | duplicate | 14 | high | Splice-segment machinery dup quasiquote vector↔list | `machine/compilation/compile_time_continuation_quasiquote.go:135`; `quasi_expand.go:330` |
| 14 | boilerplate | 13 | high | Named-let should reuse `createBindingEnv` | `machine/compilation/expander_let.go:159,366` |
| 15 | parallel-variant | 13 | high | `markCapturedBindings`/`markEscapedBindings` share scaffolding | `internal/validate/validate_capture.go:39`; `validate_escape.go:40` |
| 16 | duplicate | 12 | high | Inline docstring render dup `tryStructuredBindingDoc` | `repl/meta.go:477,516` |
| 17 | boilerplate | 12 | high | `PrimChannelTryReceive` hand-codes 3 bool→Boolean | `extensions/gointerop/prim_gointerop.go:135` |
| 18 | boilerplate | 18 | med | Per-type identity-object scaffolding ×8 concurrency types | `values/{atomic,channel,condition_variable,mutex,once,rw_mutex,thread,wait_group}.go` |
| 19 | duplicate | 10 | high | Two complex real-part compares reinline `cmpFloat64` | `values/complex.go:106,254`; `values/promotion.go:355` |
| 20 | duplicate | 10 | high | `NewApplyFrame` dups `InitApplyFrame` parent-copy | `environment/environment_frame.go:165,187` |
| 21 | duplicate | 10 | high | Phase-env binding lookup walk dup cmdDoc↔DisassembleBinding | `repl/meta.go:298,781` |
| 22 | duplicate | 10 | high | Optional resource-name extraction dup threads↔gointerop | `extensions/threads/prim_threads.go:58`; `extensions/gointerop/prim_gointerop.go:280` |
| 23 | over-abstraction | 9 | high | Three scope-set wrappers, pure pass-through, no prod callers | `internal/syntax/scope_utils.go:37,42,46` |
| 24 | duplicate | 9 | high | Doc-only PrimitiveSpec construction dup ×2 | `wile/engine.go:886,936` |
| 25 | duplicate | 9 | high | `normalizeExponentMarker` byte-identical parser↔math | `internal/parser/parser_number.go:189`; `extensions/math/prim_conversion.go:297` |
| 26 | boilerplate | 8 | high | `PrimErrorContextSource/ObjectSource` reimplement `StringOrFalse` | `registry/core/prim_error_context.go:44,90` |
| 28 | parallel-variant | 13 | med | `parseComplex` dups `ParseComplexStringNumber` sign-scan | `internal/parser/parser_number.go:416`; `number_string.go:89` |
| 29 | over-abstraction | 7 | high | `NewTokenizerWithComments` pure pass-through, stale doc | `internal/tokenizer/tokenizer.go:287`; `internal/parser/parser.go:141` |
| 30 | over-abstraction | 6 | high | `makeCharComparePrimitive`/`makeStringComparePrimitive` single-use forwarders | `registry/core/prim_characters.go:69`; `prim_strings.go:302` |
| 31 | boilerplate | 6 | high | Repeated `MachineContext` checked-cast prologue ×7 | `extensions/eval/prim_eval.go:52,116,417,456,547,634,684` |
| 32 | over-abstraction | 5 | high | `collectPatternVariables` thin wrapper (machine/compilation — distinct from match) | `machine/compilation/compile_syntax_rules.go:392`; `compile_syntax_case.go:186` |
| 33 | boilerplate | 5 | high | Fixed-arity validators hand-roll instead of `validateBodySlice` | `internal/validate/validate_dynamic_wind.go:36`; `validate_cont_mark.go:31` |
| 35 | duplicate | 5 | high | `environment-bound-names`/`namespace-bound-names` identical bodies | `extensions/introspection/prim_introspection.go:47`; `internal/extensions/namespace/prim_namespace.go:222` |
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
- **`environment_frame.go` (findings 9, 20) is under concurrent modification** by an
  in-progress `NewApplyFrame`/`InitApplyFrame` refactor. Rebase before touching.
- **Findings 45-51 net ≤2 LOC**, some zero/negative. Single-source-of-truth/readability
  wins, not volume wins. Do only as ride-alongs.

---

## Recommended phasing

Each phase = its own branch off `master`, one commit per logical group, `make lint &&
make covercheck` green before claiming done. Preserve sentinels; tests via `errors.Is`.

- **Phase 1 — Dead code.** ✅ SHIPPED (`cleanup/remove-dead-code`). ~337 LOC (215 prod).
- **Phase 2 — Cross-extension helpers → `registry/helpers`.** Findings 4, 10, 17, 22, 31,
  35. Theme 2. ~75 LOC. Self-contained; no hot paths.
- **Phase 3 — Numeric/parse formatting → `internal/schemeutil`.** Findings 11, 12, 19, 25,
  28. Theme 3. ~63 LOC. Watch finding 49 (bench).
- **Phase 4 — Intra-package dedup/over-abstraction.** Findings 5 (reuse!), 13, 14, 16, 20,
  21, 23, 24, 29, 30, 32, 37, 39, 40, 41, 42, 47, 48. ~130 LOC. Per-package, low risk.
- **Phase 5 — Factory sweep.** Findings 6, 8, 38, 43 + the broader audit. → **`FACTORY-AUDIT.md`.**
  Defer until Phases 2-4 settle (they reshape `registry/helpers`, the factory sink).

Tail items (18, 33, 36, 44, 45, 46, 49, 50, 51) ride along when their file is already open.
