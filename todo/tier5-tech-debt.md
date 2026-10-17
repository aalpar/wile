# Tier 5 — Tech Debt

> One of the files [`TODO.md`](../TODO.md) is split into (2026-10-17). `TODO.md` holds the
> status, ordering, conventions, Plan Index and Top Priority table; this file holds rows.
> Cite a row by its bold text, never by line number. `make indexlint` and `make planlint`
> read `TODO.md` and `todo/*.md` as one corpus.

## Tier 5 — Tech Debt

### Filed by the P3.15 crosscheck (2026-10-03)

Three findings from the five-lens review of `fix/hashtable-remembers-its-procedures` that are real
but out of that task's scope. Each was verified by the reviewing lens; none is a defect in shipped
behaviour.

- [ ] **An out-of-range `HashtableKind` is representable, and two subsystems answer it
  differently** [Small, needs a decision first]: `values.NewHashtable` is exported and accepts all
  256 values of a `uint8` kind. `hashKey` and `keyEqual` route an unlisted kind through `default:`
  to the EQUAL behaviour (their `//nolint:exhaustive` comments say so), while `String()` answers
  `"unknown"` and both inspection accessors now REFUSE it. So `NewHashtable(HashtableKind(7))`
  stores and retrieves as an `equal?` table but cannot say what it keys by. The fork, and it is a
  fork rather than a fix: **normalize** at the constructor (matching the storage path, which makes
  both refusal arms unreachable and requires deleting
  `TestHashtableAccessorsRefuseAnUnknownKind`), or make the type **opaque** so only the three
  constants inhabit it. Sizing measured: 2 production `NewHashtable(` call sites (4 at filing), and every
  `HashtableKind(…)` conversion in the tree is in a test or a comment. Costs of the opaque form:
  `uint8(ht.Kind())` in two error messages becomes `Kind().String()` (an improvement), and
  `for i := range HashtableKindCount` in `hashtable_test.go` becomes a loop over a slice. Do not
  implement either without picking one — the two have opposite consequences for a test that
  currently passes.

- [ ] **`setRecognizedPrimitive`'s `name` parameter duplicates `identity.Name()`** [Small]:
  `setRecognizedPrimitive(mc, identity, name, site)` takes the probe spelling beside the token that
  already carries it, and all four call sites pair them consistently
  (`identityEqualQ`/`"equal?"`, `identityEqQ`/`"eq?"`, `identityEqvQ`/`"eqv?"`,
  `identityEqualHash`/`"equal-hash"`). A mismatched pair would locate the wrong binding, fail the
  token check, and raise instead of answering — a silent, plausible refactor fault. Two readings
  with different fixes: if the spelling is DERIVED from the identity, drop the parameter and read
  `identity.Name()` — but `PrimitiveIdentity.Name()`'s doc currently disclaims any semantic role
  ("diagnostic only — never compared") and would have to be amended, and `equivIdentity` collapses
  to returning just the identity since `ok ⟺ identity != nil`. If it is genuinely a distinct fact,
  nothing documents what would make the two differ. Also: `name, site string` are adjacent and
  same-typed, so at one call site two literals sit side by side and are transposable in silence.

- [ ] **`TestRegisterFuncCallbackExceptionToError` failed once under heavy concurrent load, and
  did not reproduce** [Small, investigation]: two independent reviewers each saw one failure
  (`expected no-error-expected but got boom`, `pkg/wile/ffi_test.go`) while several agents were
  concurrently building and editing this tree and shared worktrees; the same window produced a
  spurious `TestBuildAxisBManifest` staleness hint, which that load explains. Not reproduced since:
  20 sequential runs, six concurrent 3× runs, and a further 5× run are all green, and the failure is
  NOT attributable to the P3.15 change (it is in the FFI callback path, which that change does not
  touch). Filed rather than dropped because of the standing orientation in CLAUDE.md: a test that
  fails once under load is nondeterminism that was never routed through an injectable, not something
  to retry away. What to look for first: whether the callback path reads a clock, a map order, or a
  shared registry entry that a second engine in the same process can perturb — a `-count` loop is
  the wrong instrument, since the reproducing condition was CPU contention, not repetition.

- [ ] **`pkg/wile`'s test engines are handed the parent `t` from inside a subtest** [Small, decide
  the convention]: `newSRFITestEngine`'s doc says the `testing.TB` parameter exists so a caller
  inside a `c.Run` hands over the SUBTEST's handle, because a helper given the parent's handle
  "calls FailNow from a goroutine that is not running that test". Practice contradicts the doc: of
  32 call sites, 14 are inside a `c.Run` and 6 of those pass the subtest's handle (recounted
  2026-10-16). Either the doc is wrong and should be cut, or 8 sites are. Not a
  live failure — the engines construct successfully — so this is a convention decision, not a bug.

### plans/ sweep — simplification deltas not previously filed (2026-09-20)

Same sweep as the Tier-1 and Tier-4 headings of this date. The structural-reduction roadmap stays
CLOSED for `pkg/repl` and `registry/helpers` (5 of 7 refuted); nothing below re-proposes those.

- [ ] **Delete the dead per-namespace module cache** [Tech debt, S, climbing tower Q13]:
  `Namespace.moduleInstances`, `ModuleInstance` and `AttachModule` have no production caller, and
  the docs claim a per-namespace cache exists on the strength of them. Deleting the three is
  cheaper than correcting the docs, and it removes a false answer to "does Wile already have
  per-namespace instances?" — it does not.
- [ ] **`NewSyntaxSymbolForSyntaxSymbol` silently drops `ResolvedBinding`** [Tech debt, S]:
  `pkg/syntax/syntax_symbol.go:65`, zero production callers, the gap recorded only in a test
  comment (`coverage_extra_test.go:260`). The 2026-07-18 Stage C review said "delete it or fix it";
  neither happened. Given the D2 pin work above, a constructor that drops a pin is a live hazard,
  not a dormant one.
- [x] **`MaybeCreateLocalBinding` still says "nil means match any"** [Tech debt, S, Done
  2026-09-27, `97af01a6`]: `pkg/environment/local_environment_frame.go` kept
  `matchAny := scopes == nil` and the doc comment, both of which the `ScopeSet` migration eliminated
  on the read surface. Same ambiguity one layer down, contradicting the nil-means-NONE rule — and
  fail-open, since a nil create reused whichever slot came first whatever scope set it carried, so a
  ∅-scoped binder and a `{s}`-scoped binder of one name collapsed onto one slot. **Fix**: delete the
  two lines. The loop now reuses a slot only under `scopeSetsEqual`, which for a nil argument against
  a meta-less slot is `len 0 == len 0` plus two vacuous `ScopesMatch` calls — still true — so every
  reuse the suite takes keeps its answer. **Creation and lookup have opposite polarities and that is
  the point**: "any binding of this name" is askable on the query side with `AllScopes`, and
  meaningless on the creation side, where a binder's own scope set IS its identity.
  Latent, and measured rather than assumed: an instrumented counter over the whole Go suite plus
  `./integration/...` and `./test/...` found **zero** reuses that happened only because `scopes` was
  nil. That is why the suite cannot supply the ratchet, and why this lands with three tests —
  `TestNilCreateIsNotWildcard` (the gate: red at `a7a3461e` logging `nil slot=0 created=false`, green
  after at `nil slot=1 created=true`), `TestLocalBindingCreateHasNoWildcard` (a `go/parser` walk
  keeping the identifier `matchAny` out of the file, because the wildcard could return under another
  spelling while the probe's answer stayed the same), and `TestNilCreateReusesAnEmptyScopedSlot` (the
  control — green before AND after, pinning that this is a bug fix, not a behaviour change). Five
  stale doc claims went with it. `AmbientScopes()` survives with its rationale rewritten: it is now
  vacuous as documented, but it states intent where a bare nil reads as an oversight and it has a
  live caller. `local_environment_frame.go`'s SECOND nil-vs-∅ discrimination — the
  `if scopes != nil || source != nil` guarding the `BindingMeta` allocation — is deliberately
  untouched and belongs to the scope-set type flip, which must re-check that no reader distinguishes
  "meta present with ∅ scopes" from "no meta".
- [x] **Stale doc: `processLibraryImport` claims `AtPhase` routes to the parent's phase registry**
  [Tech debt, S, Done 2026-10-04, `e01f79ab`]: the sentence is gone; the doc now names
  `planLibraryImport` and `planCopyDirect`.
- [ ] **`TestDenotedForm`'s Primitive+Void case is unreachable** [Tech debt, S]: after the
  keyword-denotation Task-6 ratchet, `TestDenotedForm` (`pkg/environment/form_keyword_test.go`) pins a state
  production cannot produce.
- [ ] **`RequireArg[T]` and `BuildValidator` type-check the same argument twice** [Tech debt, M,
  from `2026-03-26-extension-contracts-phase2-design.md`]: with `WithContractEnforcement()` on,
  `BuildValidator` (`pkg/registry/contract.go`, wired in `pkg/wile/engine.go`) checks from
  `ParamTypes`, and 100 call sites in 26 files (2026-10-16) then call `helpers.RequireArg` inside the `Prim*`
  body. The plan calls it "redundant but harmless" and gates de-duplication on full `ParamTypes`
  coverage, which is not reached (core ~197/208, several extension packages partial or zero).
  Enforcement is opt-in, so this costs nothing by default — file it, do not schedule it.
- [ ] **`pkg/values/utils.go` is a 603-line grab-bag** [Tech debt, M]: equality, list helpers,
  formatting and predicates in one file. Distinct from the `schemeutil` grab-bag already filed.
- [ ] **`resolveLocal` / `resolveGlobal` take `func(...) any` visitors** [Tech debt, M]:
  `environment_frame.go`. Generics retire the `any`.
- [ ] **Adding a primitive still touches four files with escaped-string docstrings** [Tech debt, L]:
  `pkg/registry/core/register.go`'s own comment lists the ritual verbatim. Related to, but larger
  than, the chibi-ffi-style codegen item parked in the plan index.
- [ ] **Coverage-campaign test files break the 1:1 source/test mapping** [Tech debt, M]:
  `coverage_improvement_test.go`, `near_threshold_coverage_test.go`, `expander_coverage_test.go`
  are named for the campaign that produced them, not for what they test.
- [ ] **Rehost `nestinglint` / `typeswitchlint` / `singlelinefunclint` as wile-goast beliefs**
  [Tech debt, M, from `2026-07-31-go-volume-reduction-findings.md` M5]: still plain Go under
  `tools/cmd/`. The ruleguard rehost was measured and **rejected**; these three were not, and they
  are exactly the shape the belief DSL exists for. Cross-repo.
- [ ] **Cold `NewCons` sites that should be `PairBlock`** [Tech debt, S, from
  `2026-07-17-pair-gc-investigation.md` Recommendation 3]: e.g.
  `pkg/machine/exception_raise.go:268,280-282`. The investigation makes **no perf claim** for these;
  it is consistency with the batch-construction rule, nothing more.
- [ ] **`extensions/process` hardcodes `/bin/sh` and a POSIX signal table** [Tech debt, M]:
  `prim_process.go:102`, no GOOS gate. A portability gap in a pure-Go, no-CGo product.

### Stale rows in this file, corrected 2026-09-20

- The frame-reclaim row that reads "Phases B/C/D/G … PAUSED" is stale: B, C and D all shipped; only
  F and G remain, which the plan index row already says correctly.
- `makeDocRegistrationObserver` looked exports up at phase 0 by local name. Fixed 2026-10-01,
  `edb0a1c6`: it keys on the install map and resolves at the export's own phase.


### Sealed axis keyed by `(phase, kind)` — SHIPPED 2026-08-03 (`74c72256`)

- [x] **Sealed-frame routing collapsed into one table** [Tech debt / legibility, M, Done 2026-08-03, `74c72256`]: seven sites hand-unrolled a two-row loop over the phase-0 and phase-1 sealed frames by name. `sealedAxis` (`pkg/environment/sealed_write_view.go` since `efd58d2a`) is now the single decision site, read by `SealedAt`, `sealedFrameAt`, `SealedFrames`, and `IsSealed` (names as of `74c72256`); call sites read as coordinates (`SealedTargetAt(PhaseRuntime, SealKindValue)`, now `SealedWriteViewAt`). `SealedBaseTarget() != env` became `IsNamespaceRuntime()`. No behavior change on any live path. Three `/crosscheck` traps, fixed in the same commit: (1) generalizing the seam turned "there is no seal here" into cover for "the seal is missing", so `SealedAt` returns `(*EnvironmentFrame, bool)` and a declared-but-absent frame panics via `mustSeal`; (2) `phaseParent` runs under the `PhaseRegistry` write lock, where a seam that can re-enter `GetOrCreate` hangs, and its parent link is kind-independent, hence `sealedFrameAt` plus the pure-pointer `IsNamespaceRuntime` guard; (3) `registry.SearchDoc` (`,apropos`) was a second hand enumeration a new row would have silently missed.

- [x] **Expand-phase registry primitives are NOT sealed** [Correctness, S code / M test rewrite, Done 2026-08-10,
  `693ee6f6` (`fix/w1-carve1-seal-phase1-registry`)]: `registry.Apply` bound `PhaseExpand` primitives into the
  mutable expand child, so a top-level `define-for-syntax` of a primitive's name superseded it in place at
  phase 1 (the same program shadows at phase 0) and falsified the `Stable` stamp the frame-reclaim classifier
  trusts. The copies are sealed (`24e08ceb`), and a bootstrap `define-syntax`/`define-for-syntax` over a
  `Stable` phase-1 slot is refused (`0f8ae684`; refuses on `m.Stable`, not `IsStable()`, so an import stays
  supersedable per R7RS §5.3.1). **Do not delete the phase-1 copy** as redundant with the wildcard-phase
  `(ANY, sealed)` one: the two differ in `closureEnv` (`apply.go`), which is load-bearing for `compile`,
  `expand` and `free-identifier=?`. `memory/2026-08-07-review-wave1-silent-corruption-impl.md` Phase 5b.

- [ ] **The seal is exactly one level deep, and the climb exits silently** [Design, S, deliberate —
  decide before Tier 2 of the climbing tower]: the compile-time writers reach their target phase
  through `AtPhase`: `define-syntax`, `define-for-syntax`, `begin-for-syntax`, `eval-when`
  (expand/compile) and a `define` in any transformer right-hand side (`compileTransformerValue`) via
  `NextPhase()`, and `meta` via `Expand()`, an ABSOLUTE phase 1. `AtPhase`
  stays sealed only when the receiver IS a sealed-write view and the target is above it, on a
  `sealedAxis` row; everywhere else it falls through to the mutable view, and nothing says so. Three
  exits follow. `TestSealedClimbStopsAboveExpand` pins the environment-layer fall-through of each
  (through `AtPhase` and the writer's own entry, `NextPhase` or `Expand`), including the
  transformer-body case, which goes red only when both of its fixes land. It does not pin the
  compiler's write sites:
  - **From a lexical child of a sealed view.** `NewEnvironmentFrameWithParent` does not copy
    `sealed`, so these forms in any body under a sealed view climb unsealed even onto a row.
    Measured by probe (2026-10-04): a `define-syntax` in a procedure body compiled against the
    sealed root lands in mutable phase 1, the onto-a-row case. A `define-syntax` inside an
    `er-macro-transformer` body runs in a lexical child at phase 1 and lands in mutable phase 2:
    this exit and the off-axis one at once, so it needs both fixes.
  - **From a sealed view itself, off the axis.** The phase-1 view climbs into mutable phase 2,
    because phase 2 has no row.
  - **`meta` from the phase-1 sealed view.** Phase 1 is not above the receiver, so there is no
    climb and it resolves to the mutable phase-1 view: a downgrade on the axis itself.

  Latent today: no bootstrap source nests a macro-defining form in a body (the only nested
  `define-syntax` in `bootstrap_syntax.scm` are docstring examples), and none uses
  `begin-for-syntax`, `define-for-syntax`, `eval-when` or `meta`. A logging probe at the climb sites
  recorded no write through any exit across `go test ./...` and `test/run-all.sh`. (A panicking probe
  is the wrong instrument here: the engine's compile boundary recovers panics into compile errors.)
  `compileTransformerValue` did climb from phase-1 lexical children to mutable phase 2 thousands of
  times, only under `WithSchemeSyntaxForms` (none on a default engine), for `let-syntax` right-hand
  sides inside bootstrap transformers; phase 2 holds no slots after boot, so those were reads. No
  compiler-level pin exists: a probe loading `(define (probe-f) (define-syntax probe-m …) …)` through
  `LoadBootstrapSources` against the sealed root reads the slot unsealed today, and sealed under the
  decoupling or under a write-site fix to the two `define-syntax` writers (measured); a phase-2 row
  alone leaves it unsealed. It is not committed. It becomes live the first time a bootstrap source
  does one of these: that macro's own compile-time helper would be user-overwritable, the property D3
  exists to prevent one phase lower.

  **A phase-2 row alone would not fix it.** Measured: with `Phase(2)` added to `sealedAxis`,
  `NextPhase()` from a lexical child of the phase-1 seal still returned the mutable phase-2 view. The
  lexical-child exit needs "this chain writes sealed" (`TopLevel().sealed`), a DIFFERENT predicate
  from "this frame is a sealed-write view" (`p.sealed`), which `SealedWriteViewAt`'s admission and
  its non-inheritance pins depend on. They cannot share the one flag: keying `AtPhase` on
  `TopLevel().sealed` alone admits a lexical child through `SealedWriteViewAt`'s fallback, which
  routes through `AtPhase` (measured: it reds the "not admitted" row). So either decouple the two (a
  chain predicate for the climb, and `unsealedTargetAt` taken off the sealed branch: measured at the
  environment layer, it reds only the lexical-child pin and passes every suite), or fix at the write
  sites (`compile_define_syntax.go`, `expander_body.go`, `compile_define_for_syntax.go`,
  `executeFormsAtCompileTime` in `compile_helpers.go`, which `begin-for-syntax` and `eval-when` run,
  `CompileMeta`, and `compileTransformerValue`, whose climb compiles a transformer's right-hand side
  and whose defines land one phase above the defining frame); end to end, only the two
  `define-syntax` writers were measured. The decoupling closes neither the `meta` exit nor the
  phase-2 exit. A structural alternative (hypothesis): move `AtPhase`'s sealed branch into a
  write-target method used only by the writing climb sites, so the writer set is a call graph rather
  than a hand-kept list. A row also forces an unanswered question: is the new frame's parent the
  phase-0 seal (flat, mirroring the mutable axis's no-phase→phase-edge rule) or the phase-1 seal
  (chained, mirroring the phase tower)? The construction site is `newPhaseRegistry` (`namespace.go`).

  **A different exit is closed** (2026-10-03, P3.16/I045, merged `f94806a8`): `SealedWriteViewAt` no
  longer hands back a mutable view. Asked for a phase with no `sealedAxis` row by an owner root or by
  a sealed-write view, it panics with a wrapped `werr.ErrNoSealedWriteView`; an unsealed non-owner
  falls through by its documented precondition. That is not this row's climb, and this row was first
  closed as `[x]` on the P3.16 branch by calling the climb a "read". Crosscheck refuted it: the climb
  is a write path, and the row's own premise (env == the phase-1 view) was wrong too, which a later
  round measured. **The refusal condition is `sealedViewAt`'s miss; do not replace it with a ceiling
  comparison:** `PhaseTemplate` (-1) is a named phase below the first axis row, so `phase > ceiling`
  falls through there and reproduces the defect. Pinned by `TestSealedWriteOffTheAxisIsRefused`,
  including negative phases and both sealed receivers. Unreachable from production: all five non-test
  callers pass a literal `PhaseRuntime` or `PhaseExpand`. History:
  `memory/2026-10-03-p3-16-first-cuts-refuted.md`.

- [ ] **`SealedWriteViewAt` still answers an unsealed non-owner with a mutable view** [Low, S, filed
  2026-10-04 from the P3.16 crosscheck; deferred]: the refusal covers owner roots and sealed-write
  views. An inner lexical frame with a registry still gets its own mutable view, with no error.
  Nothing checks ownership, `sealed` is unexported, and the exported `IsOwnerRoot` answers a
  different question; see the next row. `registerCompileTimeBinding`'s doc (`apply.go`) names the
  harm: keywords land at (0, mutable) and a later user define reuses the slot. Two lenses
  mutation-tested "refuse there too": only the tests that pin the fall-through on purpose went red
  (`TestSealedWriteViewAtRequiresAnOwnerRoot`, the non-owner rows of
  `TestSealedWriteOffTheAxisIsRefused`); `pkg/...` and `extensions/...` stayed green. Deferred in
  favor of the narrow fix; the rule would become "a sealed view or a panic", with the own-phase
  answer kept only for a frame with no registry.

- [ ] **`registry.Apply` does not check its owner-root precondition, and "owner root" names three
  predicates** [Low, S, filed 2026-10-04 from the P3.16 crosscheck]: `Apply`'s doc states the
  precondition; nothing enforces it, and a wrong receiver misplaces bindings or closure envs, at one
  phase or both depending on the receiver (measured per receiver kind). A guard on the predicate
  itself (an exported `OwnsPhaseRegistry` wrapping `ownsSealedAxis`, returning a wrapped
  `werr.ErrInvalidArgument`) passed `./pkg/...` and `./extensions/...` (measured in a copy, not
  committed). `env.Runtime() == env` agrees with it wherever there is a registry, but panics on a
  frame without one, so it is the wrong guard. "Owner root" also means three things: `ownsSealedAxis`
  (every owner's runtime, a library env's included), `IsOwnerRoot` (the namespace runtime and its
  sealed root; no non-test caller), and `WritesOwnerRootCoordinates` (the define gate's predicate,
  true for a lexical child of the root). Renaming `IsOwnerRoot` alone leaves the collision in the one
  predicate with a caller; deleting `IsOwnerRoot` and renaming `WritesOwnerRootCoordinates` would
  leave "owner root" meaning the registry owner only. Stale citations of `IsOwnerRoot` as the define
  gate's predicate: its own doc, `IsNamespaceRuntime`'s, `Namespace.sealedWriteRoot`'s,
  `NewChildRuntime`'s (which credits it with R2, now kept by `WritesOwnerRootCoordinates`'s
  same-store check) and `initOwner`'s; and two tests assert it as the gate's predicate
  (`sealed_write_view_test.go`, `library_seal_test.go`). Separately, `phaseTargets`' stated reason
  that a primitive's closureEnv must be the mutable runtime (capturing the sealed view "would hide
  every user define") did not hold in a probe: reads ignore `sealed`, so the sealed root saw a user
  define. The dependency, if any, is on writes through that env; unverified.

### `predeclareBinding` leaves an unwritten `#!void` twin slot per library-body define (2026-07-19)

- [x] **Orphan slot per library-body `define`** [Tech debt / allocation, S,
  **count refuted, closed 2026-09-26 — no ratchet; the measurement is recorded**]: the row
  reported 104 orphans in `(srfi 1)` alone and asked for the count to be verified before
  sizing the work. Verified 2026-09-26, and there is no work to size: the define path leaves
  zero orphans. An out-of-tree probe (a two-define library through `fstest.MapFS` +
  `stdlib.FS` + `WithLibraryPaths()`, then `eng.Namespace().Store().LiveSlots()` bucketed by
  name) gives live slots=779, sealed=779, distinct live names=587, and **f=1, g=1** —
  exactly one live slot per library-body `define`, no `#!void` twin. Against `(srfi 1)`
  itself, which is the library the row names: baseline live=777 / distinct=585, after
  `(import (srfi 1))` live=926 / distinct=684 (delta 149), with
  `fold`/`fold-right`/`delete-duplicates`/`iota`/`last`/`any`/`every` at one slot each and
  **zero `#!void`-valued slots in the store, before or after**. The close is about the
  OUTCOME, not the mechanism's removal: `predeclareBinding` is still live
  (`pkg/machine/compilation/letrec_semantics.go:112`, called from `expander_body.go:105`,
  `compile_time_continuation_include.go:223`, `compile_let.go:469`), but its global arm goes
  through `MaybeCreateOwnGlobalBinding`, which keys the slot by its scope set **at creation**,
  so a second predeclare of the same name lands on the slot the first one minted instead of
  minting a twin. C3 (scope-keyed export resolution) is what made that true; it did not merely
  make the twins unreachable. **The 192 names carrying more than one slot are a different
  phenomenon and this close does not deny them**: they are stdlib import/re-export duplicates
  under different scope sets — the head of that list is core primitives at exactly 2 slots
  each (`%syntax-spine`, `%syntax-violation`, `*`, `+`, `-`, `/`, `<`, `<=`) — not unwritten
  define twins. A number inside a closed row is documentation and nothing fails when it is
  contradicted, so a third filing argues against these figures or re-measures them.

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
  (the poll-period latency; the sentence this row quoted left `cancellation.md` with `d7723ea7`),
  and a 60-character loop of `*` with no `expt` anywhere reaches 147 MB and 14.3 s, so a
  ceiling on `expt` alone would refuse `(expt 10 N)` while permitting its own square-and-multiply
  algorithm written out in Scheme — the behavioural asymmetry this project's policy says to remove
  rather than add. `docs/environment/racket-namespaces.md:293-296` already names the gap: resource
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

- [x] **Is Option A (launder a cancelled wait as an ordinary result) a committed contract?**
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
  (`docs/concurrency/cancellation.md`, "The primitive-layer policy: wait side vs held side"). `thread-sleep!` and `thread-join!` do NOT
  launder — they raise `werr.ErrOperationCancelled` carrying the raw ctx cause (same section).
  The error-free `#f` is load-bearing rather than sloppy: `callForeignCached`'s eager
  `ErrTimerExpired` recheck fires only on the error-free return path ("Who cancels the ctx", `with-timeout` row). Only an
  embedder-supplied deadline (`mc.timer == nil`) can observe the laundering, bounded by the
  ~1024-op VM poll (the embedder-deadline row; uncovered then, covered now). Decide the contract and
  the asymmetry follows: committed ⇒ document it and stop calling it a seam; placeholder ⇒
  surface the cancellation distinctly on the two laundering waits, as `thread-sleep!` and
  `thread-join!` already do.
  **RE-OPENED 2026-10-05 by a measurement (P3.17 owns it).** The poll bound holds only when a
  poll period of work remains: when the cancelled wait is the unit's LAST form, `EvalProgram`
  returns `#f` with a nil error after the deadline expired, for both `(mutex-lock! m)` and
  `(mutex-unlock! m cv)`. With work after the wait, `context.DeadlineExceeded` surfaces at ~50 ms
  under a 50 ms ctx, as documented. Options (a) commit and document the edge, (b) VM-wide
  end-of-run ctx check, (c) source-discriminate via the existing `waitCancelled`.
  **DECIDED (c), amended, and Done 2026-10-06 in P3.17, merged `231d3d95`.** The source
  discrimination (c) asked for lives in the VM's owner-side rule, not in `waitCancelled`. No:
  every cancelled wait, the
  two lock waits included, raises `ErrOperationCancelled` with the ctx cause (`waitCancelled` no
  longer asks what cancelled it). When a `with-timeout` armed on the waiting chain fired, the VM's
  owner-side rule (`supersedingTimerInterrupt`) turns the failure into the timeout before any
  exception handler on the chain sees it: at the owning context's foreign-call bridge and
  drivers, with a sub-context below the owner passing the failure up undispatched. (A root not
  linked to the chain, such as a procedure run through `Engine.Call`, a macro transformer or a
  library body, sees it first; it becomes the timeout when it returns.) A thread started inside a timed
  thunk owns no timer and simply raises. Reached in three rounds, each refuting the previous one's
  timer path (the P3.17 record in `memory/2026-09-22-defect-closeout-P3-landed.md` has them): round 1 kept the
  timer case error-free and let a started thread and eval'd code run past an unacquired lock, as
  HEAD does;
  round 2 routed a parent timer's interrupt across contexts, and round 3 found it double-resolving
  through a copied boundary frame, losing the handler under `Engine.Call`, and leaking a raw
  interrupt. The owner-side rule has no cross-context interrupt at all, and also makes compute and
  macro expansion inside `eval`, waits in host callbacks, and escapes' after thunks run the
  handler. Round 4 found the abort route missing from `RunWithinBoundary` (a `with-timeout`
  inside `eval` whose guard escape ran a waiting after thunk lost the handler) and a stale timer
  claiming failures; both fixed and pinned. Pinned by `TestCancelledLockWaitRaisesOperationCancelled`,
  `TestCaughtCancellationCleanupUnlockReturns`, `TestTimerCancelledWaitInChildThreadRaises`,
  `TestTimerCancelledWaitInSubContextRunsHandler`, `TestTimerCancelledWaitIsNeverCaught`,
  `TestTimerCancelledWaitInRootlessCallCCRunsHandler`, `TestTimerCancelledWaitInHostCallbackRunsHandler`,
  `TestTimerExpiryDuringEvalComputeRunsHandler`, `TestTimerSupersedesFailureButNotDenial`,
  `TestInnerTimerExpiryRunsTheInnerHandler`, `TestJoinReportsJoineeCancellationAsUncaught`,
  `TestTimerCancelledWaitInAbortAfterThunkRunsHandler`, `TestOrdinaryErrorUnderArmedTimerIsCaught`,
  `TestEmbedderDeadlineInsideArmedTimerRaises` and
  `TestCancelledWaitInChildThreadIsCatchableThere`, `TestTimerClaimsAHandlerFailure` and
  `TestTimerClaimsAnSRFI18ConditionBelowIt`, `TestTimerDoesNotClaimAControlTransfer` and
  `TestEscapePastATimerLeavesItsExtent`, `TestReentryUnderATimerRewindsOutsideIt`,
  `TestReentryIntoAnEndedTimerKeepsTheEnclosingOne`,
  `TestAfterThunkEnteredBelowTheTimerStaysInsideIt`, `TestAfterThunkOutsideAFiredTimerKeepsItsGuard`,
  `TestWindingThunkFailureOutsideAFiredTimerIsItsOwn`, `TestUnclaimedFailureIsClaimedByAnotherOwner`,
  `TestEscapedTimerDoesNotHideALiveOne`, `TestEscapedTimerLeavesTheHandlersContinuation`,
  `TestTimerClaimsAnApplySpreadFailureBelowIt`, `TestEscapeFromATimerEndsIt` and
  `TestTimerExpiryNamesTheTimerThatFired`. **Simplified 2026-10-10**
  (`fix/timer-identity-and-teardown`): with the two structural rows below fixed (an escape ends
  its timer; an expiry names its timer), the code that worked around them is deleted: the
  stale-timer skip, `entryCtx` and `windingThunkContext`, the unclaimed-failure marks and the
  timer's own context type, with `TestStaleTimerClaimsNothing` and the four `pkg/machine` unit
  tests that pinned them. Round 11 found no defect in behaviour; it corrected the record of the
  open decision below and pinned the mark walk, the mark's timer and the deadline caching.
  Round 10 found the unclaimed mark honoured by every context,
  so a joiner's guard caught a joinee's end and a host's caller reported a failure as itself, but
  only when the failing code's own timer had fired; it now answers for the marking context's
  timers only. It also found the round-9 value layer doubling the deadline walk of every nested
  `with-timeout` (2 to 4 times on loops leaving stale timers); the timer's context now answers its
  own deadline, which also removes HEAD's walk. Nine mutations of round 7 to 9 code survived
  every suite and are now pinned, and three new tests flaked under `-race` and load and now have
  wide margins. **Decided 2026-10-10, by position** (the first of round 10's two options), as a
  consequence of putting the timer's extent on the winding stack and of a reinstatement composing
  its extents onto those outside its boundary: a frame re-entered inside a `with-timeout` by a
  composable continuation, or by a `call/cc` continuation reinstated at an inner prompt, sits
  above the timer's frame, so all its thunks run under the timer, wherever the continuation is
  applied; a full `call/cc` continuation invoked inside the `with-timeout` leaves the extent first,
  so its thunks run outside. Before, the answer depended on which driver ran the reconcile (the
  review of round 11's fixes found the record had described even that by the wrong rule).
  Round 9 found a `dynamic-wind`
  entered inside `load` matching no timer (`load` wraps the timed ctx, and the match was by
  identity), so its after thunk ran outside the timer and could hang, now found through a
  context value; an after thunk's uncaught failure outside a fired timer claimed by the driver
  still inside that timer's extent (the handler ran, the escape was lost, the after thunk ran
  twice; HEAD reported the failure), now marked unclaimed where it occurred; and the claim's
  chain walk paid on every failure under an armed timer (about 30% on a guard loop), now only
  once a timer fired. Round 8 found round 7's fix recording the entering context's own timer, which is
  nil inside `eval` below the owner, so such a `dynamic-wind`'s after thunk ran outside the
  timer whose extent it is in and its wait could hang; the frame now records the entry context
  and resolves it against the armed timers. It also found a stale inner timer hiding a live
  outer one (the outer handler never ran, and a handler below saw the cancellation; HEAD
  reported `ErrInternal`), now skipped, and `apply`'s spread failures raised in place past the
  rule, now bridged. Round 7 found an escape leaving a `with-timeout` through an enclosing `dynamic-wind`
  running that `dynamic-wind`'s after thunk under the timer's context, where the rule lost the
  escape to the timeout; winding thunks now run outside any timer armed since their
  `dynamic-wind` was entered. Round 6 found `timerAbove` counting a
  timer that had not fired (a stale timer below then drew an ordinary failure past the guards
  between); fixed and pinned. Round 5 also found the drivers'
  timer arms and `RunWithinBoundary`'s abort arm returning a handler's interrupt instead of
  dispatching it, and the SRFI-18 raise-in-place sites (join condition, abandoned mutex)
  bypassing the rule; all fixed and pinned.
  **Still open, documented in `cancellation.md` *Boundaries*:** a `guard` that catches the raise
  lets a cancelled program finish with a nil error, as does work that ends before the next VM
  poll, and a timed `mutex-lock!` can acquire after a cancellation. Only a VM-wide end-of-run ctx
  check (option (b)) closes the first two. The pre-existing defects the rounds found are filed
  below, under *Cancellation and with-timeout residue*.
- [x] **`RWMutex` and `Once` removed from the Scheme surface** [API/modeling, M, Done, deleted]: 12 primitives, `values.RWMutex`, `values.Once`, `werr.ErrNotARWMutex`, `werr.ErrNotAOnce`, and `finishBlockingSync`; `(wile gointerop)` is now the six `atomic` primitives. **A modeling decision, not a cleanup**: `gointerop` was publishing Go's shared-state model against the project's causal-chain orientation. Public API removal under the zero-consumer rule, as with `ChannelSelect`. Kept: SRFI-18 `mutex-*`/`condition-variable-*` and `atomic`. Traps: the plan's verification grep (hits only in `memory/`/`plans/`/CHANGELOG) was never satisfiable, since ~13 live `sync.RWMutex` struct fields remain; `ErrNotAOnce` was the sole pass-through-article row in `pkg/registry/helpers/args_test.go`, so that sentinel is now built inline to keep `typeNameFromSentinel`'s article path covered. `TestWithTimeoutInterruptsParkedRWMutex` was ported to `mutex-lock!`, not dropped: it is the only test spanning `callForeignCached`'s eager recheck and a primitive's error-free `#f`. Under an embedder deadline `mutex-lock!` returns `#f` and the VM's top-of-loop check surfaces `DeadlineExceeded`; no test covers that `cancellation.md` row. (**Both sentences superseded 2026-10-06 by P3.17:** no wait returns an error-free `#f` any more; a cancelled wait raises `ErrOperationCancelled` carrying the cause, pinned by `TestCancelledLockWaitRaisesOperationCancelled`, and the ported test now proves only the end-to-end composition.) Residual: `werr.ErrOperationCancelled` was left with no producer and a rewritten comment. **DECIDE:** delete it, or keep it for a future primitive that returns a value on success and so cannot borrow the error-free-`#f` convention. **MOOT (noted 2026-10-05): kept, and it has producers again**: `thread-sleep!` (`489cb89b`) and `thread-join!`, timed and untimed (`be0c6369`), raise it through `waitCancelled` (`extensions/threads/prim_threads.go`) and `Thread` join (`pkg/values/thread.go`); since P3.17 so do `mutex-lock!` and `(mutex-unlock! m cv)`.
- [x] **`ChannelSelect` was complete, tested, CHANGELOG-cited, and registered nowhere** [API/dead-code, S–M, Done, deleted]: removed `ChannelSelect`, `SelectCase`, `SelectCaseKind`, `firstDeadCase`, and 8 `TestChannelSelect*` functions (~312 lines). Exported from `values/`, so a public API removal under the zero-consumer rule. Wiring it would have needed a ctx arm, not just `done` arms, or it reintroduces the T1.3 leak at a new site (a prototype confirmed 2N+1 arms are `-race`-clean, so the decision was never technical). Released CHANGELOG sections (1.18.0, 1.3.0) were not edited; a removal note and correction went into `[Unreleased]`. If a consumer appears: `reflect.Select` panics past 65536 cases and the list would come from Scheme, so it needs an arity guard.
- [x] **Stale sub-context comment on `with-timeout`** [Docs, XS, **already fixed, closed
  2026-09-26**]: the row said `PrimWithTimeout`'s header described a fresh sub-context isolating
  the thunk while the body twelve lines down ran it inline, and recommended option A, deleting the
  stale sentence. A is what the tree now has. The header of `PrimWithTimeout`
  (`pkg/registry/core/prim_timer.go`) reads "The thunk runs inline on the live continuation chain
  under a finalizer prompt frame (RunBodyUnderTimer), mirroring call-with-continuation-barrier
  (prim_barrier.go): a continuation captured inside the thunk spans the finalizer frame and the
  rest of the program rather than being truncated at a sub-context boundary." Measured 2026-09-26:
  `grep -rn 'sub-context pattern\|fresh sub-context isolates' --include=*.go .` is empty, and the
  callee the header names is live — `(*MachineContext).RunBodyUnderTimer`
  (`pkg/machine/run_body_under_timer.go:38`), called at `prim_timer.go:100`.
- [x] **Scheme-level cancellation tests added; two real defects found writing them** [Test-coverage + Correctness, S, Done, A+B+C shipped]: `extensions/gointerop/channel_cancellation_test.go` added `TestWithTimeoutInterruptsParkedReceive` and `TestTerminateUnparksBlockedThread`, both mutation-verified. Two defects surfaced:
  - **`thread-terminate!` discarded its own SRFI-18 end-exception** [Correctness, S, Done]: `Thread.Start`'s goroutine unconditionally overwrote the stored `TerminatedThreadException`, and `defer close(p.done)` ran last, so a joiner could never observe it; a thread parked in a tail-position receive was reported as having succeeded. This invalidated the design doc's claim that the ≈1024-op unwind window was itself the protection. Fix: a write-once outcome (`Thread.setOutcome`), first writer wins, which keeps SRFI-18's "if the thread is not already terminated". Prior coverage was vacuous (both tests asserted the `#t` literal they wrote). Guard: `extensions/threads/prim_threads_terminate_outcome_test.go`.
  - **`thread-join!` on a terminated but never-started thread blocked forever** [Correctness, S, Done]: `done` was closed only by the goroutine `Start` spawns. `Terminate` now closes it when ending a `ThreadNew` thread. The two closers are mutually exclusive because `Start` makes `ThreadNew → ThreadRunnable` under `p.mu` and refuses any other state, so no `sync.Once` is needed (a double close is a fatal host panic). Guards: `pkg/values/thread_lifecycle_test.go` (20000-trial `-race`) and `TestThreadTerminateNeverStartedThreadIsJoinable`, which joins with no timeout so a regression is an unbounded park rather than a misleading `JoinTimeoutException`.

**The decision that gated the deferred sub-item is taken:** Option A is NOT the contract (P3.17, (c), 2026-10-06); see the row above. The second decision this line used to name, *is `channel-select` on the roadmap?*, is spent: `ChannelSelect` was deleted with the rest of the channel surface in 1.19.1.


### Cancellation, with-timeout and continuation residue (found by the P3.17 verification rounds, 2026-10-06 and 07)

Each open row was reproduced on HEAD `4c695231` as well as on the P3.17 branch, so none is caused by
P3.17, though P3.17 changes the symptom of some (each such row says how). The shapes below are
enough to rebuild the probes.

- [x] **An escape past a `with-timeout` leaves its timer armed** [Correctness, M, Done 2026-10-10 on
  `fix/timer-identity-and-teardown`: the timer's extent is a winding-stack frame (`newTimerFrame`)
  above its boundary, and every unwind through it ends the timer (`exitTimer`); pinned by
  `TestEscapeFromATimerEndsIt`. The record below is as filed]: only the finalizer
  frame and `resolveTimerInterrupt` restore `p.timer`/`p.ctx` (`run_body_under_timer.go`); a `guard`
  catching a raise outside the thunk, or a `call/cc` escape, discards the finalizer without running
  it. When the stale timer fires, the next poll, recheck or wait emits an interrupt whose boundary is
  gone and the root driver returns `ErrInternal` ("no boundary frame found for fired timer").
  `(define r (guard (e (#t 'caught)) (with-timeout 100 (lambda (k) 'h) (lambda () (raise 'x)))))`
  then any 200 ms of work, in the same evaluation unit (one form, or an `EvalProgram` unit such as
  a file run; separate `EvalMultiple` forms and REPL inputs each start a fresh context). Fix shape: tie the teardown to the boundary frame leaving the chain.
  Since P3.17 a stale timer claims no failure and hides no live timer outside it
  (`liveFiredTimer`, `TestStaleTimerClaimsNothing`, `TestStaleTimerDoesNotHideALiveOne`; the first two
  since deleted, the third renamed `TestEscapedTimerDoesNotHideALiveOne`), so a
  primitive's own failure is reported as itself; but the stale timer's done ctx still cancels
  every later wait in the unit, which now raises a cancellation carrying `ErrTimerExpired` where
  HEAD reported `ErrInternal`, and a poll or recheck still reports `ErrInternal`. An `eval` that
  expands after the stale timer fired reports a raw "context deadline exceeded" from the expander.
- [x] **The VM poll, both eager rechecks and the expander read only the running context's timer**
  [Correctness, M, Done 2026-10-06 in P3.17, merged `231d3d95`]: a with-timeout expiry during compute
  or macro expansion inside `eval`/`load` (a sub-context: timed ctx, no timer) surfaced as a raw,
  catchable `context.DeadlineExceeded` and the handler never ran. Those checks still read only the
  running context's timer, and that is now enough: the raw error returns through `eval` to the
  context that owns the timer, whose owner-side rule (`supersedingTimerInterrupt`) turns it into the
  timeout. Pinned by `TestTimerExpiryDuringEvalComputeRunsHandler`.
- [ ] **Resuming a `with-timeout` handler's `k` re-dispatches a foreign call with drained operands,
  and after a poll interrupt overwrites a live value** [Correctness, S–M]: a foreign-call interrupt
  leaves `callForeignCached` before the call's frame is restored, with `evals` drained and pc on the
  call, so `(k v)` fails with "expected 1 arguments, got 0" (intermittently, inside `eval` or out, a
  stack underflow reported as `ErrInternal`). After a POLL interrupt `(k v)` resumes, but `v`
  replaces the operand the next instruction consumes and `(k)` clears it: `resolveTimerInterrupt`
  does not stash the value register, which `resolveBreakInterrupt` does (`breakResumeValues`).
  `(with-timeout 20 (lambda (k) (k 0)) (lambda () (let loop ((i 0) (acc 0)) (if (< i 3000000)
  (loop (+ i 1) (+ acc 1)) acc))))` returns a wrong `acc` with no error in up to 1 run in 4; HEAD
  too. No test resumes `k`. Decide: deliver `v` as the call's result and replay the stashed values
  after a poll, or document `k` as not resumable and raise a clear error.
- [x] **Which timer fired is not recorded** [Design, M, Done 2026-10-10 on
  `fix/timer-identity-and-teardown`: the cause names its timer (`timerExpiry`), answered by the
  context that arms it (`firedTimer`); pinned by `TestTimerExpiryNamesTheTimerThatFired`. A started
  thread's busy loop under its starter's timer still dies with the poll's raw context error, as one
  with no `with-timeout` of its own does. The record below is as filed]: every `with-timeout` cancels with the one
  `ErrTimerExpired`, so the innermost context that owns a timer answers any expiry: an outer
  timer's runs an inner handler early, and a starter thread's runs a started thread's own
  (unexpired) `with-timeout` handler. (In a busy loop the poll that interrupts runs before
  `OpsExecuted` advances, so that handler is cut off at its first instruction instead, and a
  started thread dies with a raw "context deadline exceeded".) Documented in `cancellation.md`. Fix shape: a per-timer cause
  carrying the boundary `Tag` (`errors.Is(ErrTimerExpired)` still true), matched in
  `supersedingTimerInterrupt`, the poll, the rechecks and `resolveTimerInterrupt`.
- [ ] **A joinee killed at the VM poll is classified differently from one killed in a wait, and the
  uncaught-exception carrier hides the cause** [Correctness, S–M]: a busy joinee dies with the raw
  ctx error, which `joinConditionFor` does not map, so the joiner's guard sees an ordinary error
  object (the arm for the JOINER's cancellation); a parked joinee yields `uncaught-exception`. And
  for every joinee error, `ErrExceptionEscape.Unwrap` returns nil for a `values.UncaughtException`,
  so `errors.Is(err, werr.ErrOperationCancelled)` / `context.Canceled` fail at the embedder (Error
  Chains: Lossless and Traversable). Fix the chain first, then map every remaining
  `UncaughtThreadException` to `uncaught-exception`; doing it in the other order loses the cause.
- [ ] **A zero or elapsed timeout under cancellation picks at random in `thread-sleep!` and
  `thread-join!`** [Correctness, S]: both `select` a ready timer arm against a ready ctx arm, so
  `(thread-join! t 0 'tv)` in a cancelled thread returns `'tv` about half the time; the lock waits
  now raise every time. Fix shape: check `mc.Context().Err()` before honouring the timer arm.
- [ ] **Joining a thread terminated inside a timed `mutex-lock!` blocks for up to the timeout**
  [Correctness, S]: `Join`'s fast path watches only `done`, which closes when the goroutine exits,
  and the timed `LockContext` path does not watch ctx. Fix shape: make the timed path ctx-aware
  (the timed-`mutex-lock!` row below). Returning a stored `TerminatedThreadException` before the `select` is not
  enough: it misses a joiner already parked, and breaks the "a join implies the goroutine exited"
  guarantee `TestTerminateUnparksBlockedSyncPrimitive` relies on.
- [ ] **A cancelled waiter can swallow a wakeup meant for another** [Correctness, S]: `waitOnCondCtx`
  returns false when the cancel lands just after `cond.Wait` woke it for an unlock's `Signal`, and
  `LockContext` returns without passing the signal on, so another untimed waiter stays parked on an
  unlocked mutex. Window is a few instructions (1 hit in 20 000 stress trials). Fix: re-`Signal`
  when returning false with the mutex free. Coordinate with `plans/2026-08-01-srfi18-sequencer-design.md`,
  which keeps a `Signal` lost wakeup in the same lines unfixed as its Phase-4 acceptance test.
- [x] **An escape reconciled by the top driver runs a sub-context timer's after thunks outside its
  timed ctx** [Correctness, M, Done 2026-10-12, `b39a6329`, merged `8ae377b3`: the
  repro returns the handler's value in under 0.1 s; at `49ad4fc2` a 10 s `thread-sleep!` after thunk
  still ran in full. The record below is as filed]: with the `with-timeout` itself inside `eval`, an escape inside the
  thunk is re-raised to the top `RunResumable`, whose winding reconcile runs the after thunks on
  the top context's ctx, not the timed one, so a wait in an after thunk is never cancelled by the
  timer and hangs (a 10 s `thread-sleep!` sleeps its full 10 s).
  `(eval '(with-timeout 50 h (lambda () (call/cc (lambda (k0) (dynamic-wind (lambda () #f)
  (lambda () (k0 'escaped)) (lambda () (mutex-lock! m)))))))) env)` with `m` held.
- [x] **A sub-context or thread root runs its parent's dynamic-wind after thunks when it
  completes** [Correctness, S–M, Done 2026-10-10 on
  `fix/timer-identity-and-teardown`, by the fix shape: a context records the depth it inherited
  (`MachineContext.windingBase`) and its completion, or a thread's exit, unwinds to it
  (`UnwindOwn`), as does an abort to its own boundary (`resolveAbort` reconciles to
  `windingOutside`); pinned by `TestRootlessSubContextLeavesInheritedExtents` and
  `TestThreadExitLeavesTheStartersExtents`. Found again as a regression of the timer frame: the
  thread's exit ended its starter's timer. The record below is as filed]: `NewSubContext` copies the
  parent's winding stack and `RunResumable`'s normal completion runs `UnwindTo(0)`, so every
  enclosing extent's after thunk fires there and again when the parent exits. A thread root, which
  copies its starter's winding stack, does the same at thread exit. Fix shape: on a sub-context or
  thread root, unwind on completion only to the inherited depth. (Filed as "A rootless `call/cc`
  or `call-with-composable-continuation` sub-context runs its parent's dynamic-wind after thunks
  when it completes": the sub-context those ran their procedure on in tail position. It no longer
  exists since 2026-10-11; the procedure runs in place.)
- [ ] **A call/cc continuation reinstated by an escape leaves its parameterization sticky**
  [Correctness, M]: `reinstallSegment` sets the driver's `capturedMarks`/`isolatedMarks` and nothing
  clears them when the reinstalled frames return, so a `parameterize` value outlives its extent for
  the rest of the evaluation. Since P3.17 a `with-timeout` handler that runs because a wait in an
  escape's after thunk was cancelled sees the escaping continuation's parameterization (before, the
  handler never ran in that shape). Fix shape: scope the snapshot to the reinstalled segment.
- [x] **A library instantiation interrupted by a cancellation is memoized** [Correctness, S,
  **Done 2026-10-12, duplicate**]: fixed under its later row "A library body cancelled by a timeout
  fails every later import" (`fix/library-abort-replay`), by the other fix shape: the cancellation
  is still remembered (the body runs at most once) but as `ErrLibraryInstantiationCancelled`, which
  a guard catches, not as the stale ctx error.
- [x] **A guard catching a raise from a rootless `call/cc` inside an escape's after thunk fails
  with "exit procedure called outside dynamic extent"** [Correctness, S–M, **Done 2026-10-11**]:
  no timer needed.
  `(guard (e (#t 'caught)) (call/cc (lambda (k0) (dynamic-wind (lambda () #f) (lambda () (k0 'esc))
  (lambda () (call/cc (lambda (k) (raise 'boom))))))))`; without the inner `call/cc` the guard
  works. Cause: the inner `call/cc` is rootless, so it runs under `RunWithEscapeHandling`
  (`RunResumable`), whose not-found abort arm mints "no prompt found" for the guard's exit tag, which
  lives on the outer chain, instead of re-raising it as `RunWithinBoundary` does; the exit closure
  has already set `valid=false` (`prim_exit.go`), so the guard's handler, catching the minted
  condition, calls exit again and gets this message. Any abort to an outer tag from there fails the
  same way: `(ex 'x)` under `call-with-exit` and `(abort-current-continuation t 'v)` under an
  enclosing prompt report "no prompt found". Fixed with the resume row below: the tail `call/cc`
  applies its procedure in place, so the abort reaches the thunk's own `RunWithinBoundary`, which
  re-raises it to the enclosing driver. Pinned by `TestCallCCInAWindingThunkIsNotAPrompt` (guard
  and abort rows). A stale timer's cancelled wait used to surface here as this message.
- [x] **Applying a composable continuation runs the invoker's `dynamic-wind` after thunks**
  [Correctness, M, Done 2026-10-10 on `fix/timer-identity-and-teardown` for the winding half: a
  continuation records the depth of its delimiting prompt on the winding stack
  (`ComposableContinuation.windingBase`), and a reinstatement re-enters the extents it carries on
  top of those outside its boundary (`reinstallSegment`), so the invoker's are kept, and the prompt
  frames it carries are rebased onto them (`rebasePrompts`); pinned by
  `TestReentryInsideAFrameKeepsIt` and `TestReentryInsideATimerStaysUnderIt`. The handlers and
  `parameterize` half below is STILL OPEN. The record below is as filed]: no timer involved, same on HEAD. `reinstallSegment` reconciles the winding
  stack to the captured one as an absolute target, so frames the invoker is inside are unwound.
  With `kc` captured under a top-level prompt, `(dynamic-wind (lambda () (note 'in)) (lambda ()
  (note (list 'kc-returned (kc 7))) (note 'body-done)) (lambda () (note 'out)))` logs
  `(in out (kc-returned 8) body-done)` and then fails with `ErrInternal` "pop-wind: winding stack
  is empty"; Racket logs `(in (kc-returned 8) body-done out)`. A `call/cc` continuation reinstated
  at an inner prompt likewise unwinds the frames outside that prompt. Fix shape: compose the
  segment's frames onto the invoker's stack, as composable resume composes marks. Re-entered
  thunks also see neither the invoker's handlers nor its `parameterize`: a `raise` in a before
  thunk re-run by `(kc 1)` escapes a `guard` around `(kc 1)` (Racket catches it). The open timer
  decision in the I050 row sits on this.
- [ ] **`resolveTimerInterrupt` does not reset the live marks** [Correctness, S]: the hard
  suspension restores the chain and winding stack to the boundary's but leaves `p.marks`, so a
  `parameterize` in tail position of the thunk is visible to the handler, and so is a
  `with-exception-handler`: one in tail position of the thunk catches the timeout handler's own
  `raise-continuable` (`(inner-weh from-handler)`; non-tail, the outer guard does). HEAD too. Fix
  shape: `p.marks = nil` after `p.windingStack = boundary.windingStack`. `resolveBreakInterrupt`
  leaves them too, which a debugger handler may want.
- [ ] **A cancelled program can still finish with a nil error** [Correctness, M]: a `guard` that
  catches a cancelled wait's raise and finishes, work that ends before the next 1024-op poll, and
  an uncontended `mutex-lock!` (which acquires without waiting) all return normally after the
  embedder cancelled. The residual of I050 option (b): only a VM-wide end-of-run ctx check closes
  it (`cancellation.md`, *Boundaries*).
- [ ] **The timed `mutex-lock!` path ignores its context** [Correctness, S–M]: `Mutex.LockContext`'s
  timed branch (`pkg/values/mutex.go`) never reads ctx, so `(mutex-lock! m T)` parks for up to `T`
  after a cancellation and can acquire and answer `#t`; under a 50 ms `with-timeout` the handler
  runs only when a 2 s timed wait ends, and under an embedder cancel the wait raises after 2 s.
  Below the timer's owner the code after a late acquire runs until the next poll
  (`cancellation.md`, *Boundaries*). Fix shape: select the timed wait against `ctx.Done()`; this
  also fixes the join stall above.
- [ ] **`error`'s own argument failure below a fired `with-timeout` reaches a `guard`**
  [Correctness, S]: `PrimError`'s non-string-message branch (`pkg/registry/core/prim_exceptions.go`)
  calls `RaiseInPlace` with the `ErrNotAString` condition without asking `FailureClaimedByTimer`,
  so inside `eval`/`load` after the timer fired a guard sees `(error 5)`, while the same class of
  failure from `car` or `apply` is claimed. Unchanged from HEAD, where both reached the guard;
  P3.17 claims every other primitive failure there. `raise`, `raise-continuable` and a well-formed
  `error` are exempt by design (`FailureClaimedByTimer`'s doc). Fix shape: return the condition,
  or ask `FailureClaimedByTimer` first as `thread-join!` does; pin beside
  `TestTimerClaimsAnApplySpreadFailureBelowIt`.
- [ ] **A handler the driver applies itself escapes every `guard` when its application fails**
  [Correctness, S]: `resolveTimerInterrupt` and `resolveAbort` (`pkg/machine/machine_context.go`)
  apply the `with-timeout` handler and the prompt handler with `ApplyCallable`; its arity failure
  never reaches `bridgeForeignError`, and the timer and abort arms of `RunResumable` return it raw
  unless it is a control signal or a fired outer timer claims it. `(guard (e (#t 'caught))
  (with-timeout 20 (lambda () 'x) (lambda () (expt 3 6000000))))` and `(guard (e (#t 'caught))
  (call-with-continuation-prompt (lambda () (abort-current-continuation t 1 2)) t (lambda (x) x)))`
  reach the embedder as "expected N arguments, got M" with no location; inside `eval` both are
  caught. HEAD too. Fix shape: send the arms' non-control failure through `bridgeForeignError`.
- [ ] **The VM poll returns `ctx.Err()`, dropping the cause and the location** [Correctness, S]:
  `MachineContext.Run`'s top-of-loop check returns the bare `mc.ctx.Err()`, where a parked wait wraps
  `context.Cause` in `ErrOperationCancelled`. Under a ctx cancelled by `WithCancelCause` with
  `context.DeadlineExceeded`, a parked `(mutex-lock! m)` reports "...cancelled before the wait
  completed: context deadline exceeded" at its source position, while `(let loop ((i 0)) (loop (+ i
  1)))` reports "runtime error: context canceled" with `errors.Is(DeadlineExceeded)` false and no
  position. HEAD too. Fix shape: wrap the cause and `ctx.Err()` through `WrapError`, located.
- [ ] **`condition-variable-signal!` right after `thread-terminate!` of a waiter wakes no one**
  [Correctness, S]: `Thread.Terminate` (`pkg/values/thread.go`) cancels the waiter's ctx and returns
  while its channel is still in the cv's wait set, so `ConditionVariable.Signal` closes the dead
  waiter's channel and the live waiter behind it stays parked (`blockOnWaiter`'s ctx arm then
  answers `#t` to the dead thread). Waiters `a` then `b` in a `(mutex-unlock! m cv)` loop; set the
  predicate, `(thread-terminate! a)`, `(condition-variable-signal! cv)`: `b` parked 20/20, woken
  20/20 with a 10 ms sleep before the signal. HEAD too. Deterministic, unlike the swallowed-wakeup
  row. Fix shape: `Signal` skips a waiter whose ctx is done.
- [ ] **`process-wait` ignores its context's cancellation** [Correctness, S–M]: `PrimProcessWait`
  (`extensions/process/prim_process.go`) blocks in `values.Process.Wait` and never reads
  `mc.Context()`. A child spawned under the same ctx is killed by `exec.CommandContext` and the wait
  answers `-1` with no error (a `guard` never fires; a program ending in it finishes nil after its
  deadline). A child spawned under another ctx (an earlier `Eval`, or before a `with-timeout`) is
  not killed, so the wait parks until it exits: 3 s under a 300 ms deadline, and a `with-timeout 50`
  handler runs after 2 s. HEAD too. Fix shape: select against `ctx.Done()` and raise
  `ErrOperationCancelled` with the cause, as the SRFI-18 waits do; decide whether cancellation
  kills the child.
- [ ] **A thread started inside a `with-timeout` thunk is cancelled when the thunk returns**
  [Design, S]: `Thread.Start` (`pkg/values/thread.go`) derives the thread's ctx from the timer's,
  and the finalizer's `cancel()` ends it, so a thread that outlives a thunk that returned normally
  dies with "uncaught exception in thread: context canceled". HEAD too; `cancellation.md` lists it
  as a cancellation source without saying it is intended. Decide: keep it and pin it, or give the
  thread a ctx the expiry cancels but a normal return does not.
- [x] **A continuation captured and invoked inside `eval`, `load`, a `call-with-*-file` procedure or
  a library body discards the caller** [Correctness, M, Done 2026-10-12, `b39a6329`, merged
  `8ae377b3`]: fixed by the rootless-resume rule (a resume delimited at a sub-context's own boundary
  is resolved by that sub-context's driver). Re-measured 2026-10-16: the `eval` form below answers
  `101`; `load` runs the rest of the file and the caller; a library body imported through `eval`
  answers `142`; a thread thunk keeps its tail; `with-timeout` around the `eval` times out cleanly.
  Pinned by `TestCallCCInsideEvalResumesInsideEval`, `TestCallCCInsideLoadResumesInsideLoad`
  (`extensions/eval/prim_eval_test.go`). The stale-continuation case stays open under the
  rootless-resume row below. As filed: no timer involved, same on HEAD. `eval` and
  `load` drive their sub-context with `RunWithinBoundary`, which re-raises every
  `ErrResumeContinuation`, and a library body runs under a bare `mc.Run()`
  (`vmMacroEvaluator.EvalTemplate`); the top `RunResumable` then installs the captured segment,
  which ends at that boundary, as its whole chain. `(+ 100 (eval '(call/cc (lambda (k) (k 1)))
  (interaction-environment)))` answers `1` (Chez, Racket: `101`); in `load` the rest of the file and
  the caller are skipped; a library body `(define z (+ 1 (call/cc (lambda (k) (k 41)))))` imported
  through `eval` makes the program answer `#<void>`; a thread thunk loses its tail.
  `call-with-exit`, and a `k` captured outside `eval` and invoked inside, are correct.
  `TestSubContextNonTailEscapeTruncation` (the 2026-06 boundary-reification arc) has no row for
  these primitives. Since 2026-10-10 the resume keeps the winding stack around the `eval` call
  (`ResumeWindingDepth`), so a `with-timeout` around it stays armed while its boundary frame is
  discarded with the chain: when its timer fires, the driver reports "with-timeout: no boundary frame
  found for fired timer" (an internal error) where it raised a plain cancellation before.
  Fix shape: resolve a resume rooted on the sub's own chain in its driver, or run
  them on the live chain as `force` now is.
- [ ] **Code in a sub-context cannot see its parent's prompts** [Correctness, M]: `FindPrompt`
  (`pkg/machine/machine_context_continuation.go`) walks the running context's chain and its own
  `promptTag`, never `parentMC`, so `eval`'d code and the winding thunks a reconcile runs on a
  sub-context find no enclosing prompt: `(eval '(continuation-prompt-available?
  (default-continuation-prompt-tag)) (interaction-environment))` is `#f` (`#t` outside), and
  `call-with-composable-continuation` there or in an escape's after thunk fails "no prompt found
  for tag"; Racket answers `#t` and captures. `abort-current-continuation` works. HEAD too. Fix
  shape: continue the lookup through `parentMC`; capturing across the boundary is the harder half.
- [x] **A continuation invoked from a rootless `call/cc` in an escape's after thunk runs the rest of
  the program, then the escape runs it again** [Correctness, M, **Done 2026-10-11**]: a `call/cc`
  in tail position of a winding thunk saw `mc.Parent() == nil`, so `PrimCallCC`
  (`pkg/registry/core/prim_control.go`) ran its lambda on a fresh sub-context under
  `RunWithEscapeHandling`, whose resume arm grafted the outer continuation onto the sub's own chain
  and ran it to completion; the escape in flight then delivered too. `(note (call/cc (lambda (ex)
  (call/cc (lambda (k0) (dynamic-wind (lambda () #f) (lambda () (k0 'escaped)) (lambda () (call/cc
  (lambda (k) (ex 'exited))))))))))` logged `(exited escaped)`; Chez logs `exited`. Fix: the lambda
  runs in place always, and a resume is resolved by the driver that owns where the continuation
  was delimited: a default-tag frame on its chain, or its own boundary (boundary token, stamped by
  `DelimitAt` when `FindPrompt` found no frame; `resumePrompt`). `RunWithinBoundary`
  re-raises every other resume, so the sub-context's edge is never a prompt the program did not
  write; a root resolves whatever reaches it. The same rule fixed the other face of the gap, not
  filed before: a continuation captured at a sub-context's own boundary and resumed by an outer
  driver replaced that driver's chain, so a non-tail `call/cc` inside `eval` or inside an after
  thunk silently dropped the rest of the program. Racket agrees on both probes. Pinned by
  `TestCallCCInAWindingThunkIsNotAPrompt`, `TestCallCCInsideEvalResumesInsideEval`,
  `TestThreadRootIsTheThreadsOnlyImplicitPrompt`; design in
  `memory/2026-10-11-rootless-subcontext-winding-decision.md`. Still open: a continuation captured
  in a sub-context and invoked after it returned resolves at the root (the sub-context truncation
  class; the root replaces its chain with the segment and the rest of the program is dropped
  without an error). The token makes that case detectable: a token whose context has been
  released could be marked closed in `ReleaseSubContext` and a root could raise a located error
  instead of replaying. Not done, because a continuation captured inside one `Engine.Call` and
  invoked from the next (a host-driven generator) takes the same route today and would start
  failing.
- [ ] **An expansion-time context carries neither thread nor barrier identity** [Correctness,
  S–M]: `acquireMacroContext` and `EvalTemplate`'s `NewMachineContext` leave `threadID` at 0 and
  `barrierValid` nil on every goroutine, so the checks in `applyCapturedContinuation` are wrong in
  both directions on the pass-up route: a continuation captured on the main thread and invoked
  from a transformer under `eval` in another thread passes the cross-thread check and is replayed
  by that thread's root (the per-thread allocator invariant is broken), while a thread's own
  continuation invoked the same way is refused ("captured in thread 1, invoked from thread 0");
  a continuation captured outside `call-with-continuation-barrier` escapes it through a
  transformer, and one captured inside fails from a transformer inside the same barrier. A
  phase-1 continuation captured in one thread's expansion and invoked in another's is not refused
  either, and which root resolves it depends on whether the capturing root's run has ended (its
  boundary token's `closed`, atomic for this reason). Fix shape: hand the expander the invoking
  context's thread and barrier identity (the `eval`/`load` primitive and the engine entry know
  them) and stamp them on the contexts expansion creates. Related: with no runtime driver above
  the expansion, the barrier error for a passed-up resume is raised at the engine's compile
  entry, after the transformer's extents were left and out of any `guard`'s reach; raising it at
  the `(k v)` site needs a "runtime driver above" fact the expander would carry. Found by the
  types and errors lenses of the rootless fix's crosscheck (rounds 3 and 4); pre-existing.
- [ ] **An abort passed up from a transformer reconciles the runtime's winding from the
  transformer's** [Correctness, S]: `RunWithinBoundary` re-raises a not-found `ErrPromptAbort`
  unchanged, so an abort from a transformer (run by a runtime `eval` inside a `dynamic-wind`
  under a runtime prompt) to that prompt carries the transformer root's `SourceWinding`, which
  started empty, and the runtime `out` thunk is skipped: `((handled from-macro) (in) ...)` where
  the same abort from eval'd runtime code gives `(in out)`. The resume signal got this treatment
  on 2026-10-11 (`passUpResume`, `rebaseResumeWinding`); the abort needs the same, best by
  putting the stack and the rebase mark on one small type both signals embed. Found by the types
  lens of the rootless fix's crosscheck (round 4); pre-existing.
- [ ] **A continuation captured in a phase-0 library body is replayed by a transformer's root**
  [Correctness, S]: a library body instantiated at expansion time runs on an `EvalTemplate` root,
  whose boundary token says "expansion-time" (it records the driver, not the phase), so a stale
  continuation from it invoked in a transformer under a runtime `eval` is resolved there and the
  body's remainder runs on the phase-1 root ("syntax transformer returned non-syntax value").
  A continuation from a top-level runtime form in the same position passes up to the runtime
  root. The bootstrap's root (`runBootstrapStx`) is classified the same way, and the guard runs
  one way only: a phase-1 continuation shared through a library and invoked at runtime is
  resolved by the runtime root, which runs the transformer's remainder at phase 0 without an
  error. Fix shape: mint the token's kind from the instantiation's phase (the library registry
  knows it) rather than from the driver, then refuse in both directions. Same crosscheck, types
  and code lenses; pre-existing shape.
- [ ] **A non-continuable `raise`'s secondary exception is swallowed when a handler-captured
  continuation is invoked from a sub-context** [Correctness, S]: `applyCapturedContinuation`
  computes the escalator revivals against the invoking context's own chain only
  (`pendingEscalatorRevivals(capt.cc, p.cont)`; `chainCarriesEscalatorArm` never walks
  `parentMC`), so a continuation captured inside a `with-exception-handler` handler and invoked
  through `eval` reads its finalizer frame's arm as revived although the jump never left the
  handler, and the handler's return is forwarded instead of raising "handler returned from
  non-continuable exception" (the direct invocation raises, as Chez does). Fix shape: filter
  the revivals against every chain up the `parentMC` walk; for the transformer route, which has
  no parent, recompute them at the first driver above the expansion, as the winding is rebased
  there. Found by the errors lens of the rootless fix's crosscheck (round 3); the `eval` route
  is pre-existing on master.
- [ ] **A thread's outcome forwards a control signal raw to the joining thread** [Correctness, S]:
  `raiseJoinCondition` (`extensions/threads/prim_threads.go`) returns the thread's error unchanged
  and `UncaughtThreadException.Unwrap` exposes it, so a control signal that ends a thread's run
  (today: `(make-thread exit)`, whose exit signal makes the joining program exit silently on
  `thread-join!`; until 2026-10-11 also an abort out of an inherited after thunk at thread exit,
  now dispatched in the thread) is acted on by the joiner's driver. Fix shape: turn a control
  signal in a thread's outcome into a located error that is not a control signal; whether `exit`
  in a thread should end the program is the open semantics question. Found by the errors lens of
  the rootless fix's crosscheck; pre-existing.
- [ ] **A procedure a host function runs through `Engine.Call` is a default prompt nobody
  wrote** [Correctness, S–M]: `callCallable` (`pkg/wile/engine.go`) drives its sub-context under
  `RunWithEscapeHandling`, a root, so a continuation captured in the calling program and invoked
  inside the callback is resolved there: the rest of the program runs inside the callback, the
  host returns, and it runs again, the shape the rootless rows above had at the thunk edge. The
  same program through a func-typed `RegisterFunc` callback (`RunWithinBoundary`,
  `ffi_arg_converters.go`) answers as Chez and Racket do. Two fixes, the choice is the
  embedder-facing contract: mint a `BarrierToken` for the call (Racket puts a continuation barrier
  on every callback; a continuation crossing `Engine.Call` in either direction then fails with
  `ErrContinuationBarrier`, including one captured in an earlier `Engine.Call`), or have the root
  re-raise a resume `resumePrompt` does not own so a host that returns the error forwards it to the
  outer driver (a stale continuation invoked from top-level Go then surfaces as an error). Found by
  three crosscheck lenses of the rootless fix, 2026-10-11; pre-existing.
- [x] **A library body that exited through an abort replays the abort on every later import**
  [Done 2026-10-12, branch `fix/library-abort-replay`]: `CompiledLibrary.memoizableInstantiateError`
  (`pkg/machine/compilation/library_registry.go`) now converts a remembered `ErrPromptAbort`, like a
  resume, into a continuation barrier error for later importers; the first importer still gets
  the abort. Pinned by `TestLibraryBodyThatExitedThroughAnAbortIsNotReplayedByLaterImports`
  (`pkg/wile/transformer_callcc_test.go`): before the fix, each later import reran the handler
  and the body's after thunk. The filed fix shape (convert everything `isControlSignal` matches)
  was wrong: `ErrExceptionEscape` is an uncaught raise, the body's ordinary failure, which later
  importers must keep seeing as the same raise.
- [x] **A library body cancelled by a timeout fails every later import** [Done 2026-10-12,
  branch `fix/library-abort-replay`]: `CompiledLibrary.memoizableInstantiateError`
  (`pkg/machine/compilation/library_registry.go`) now remembers a body whose ctx ended while it
  ran as `werr.ErrLibraryInstantiationCancelled`, located at the library's form, with no ctx error
  in the chain; the first importer still gets the cancellation, which the owner's driver makes the
  timeout. Before, later imports failed with the stale cancellation (`context deadline exceeded`
  from the VM loop, or a SRFI-18 wait's `ErrOperationCancelled`) with no deadline in force, which
  a fresh `with-timeout` passed through uncaught. Cancelled means the ctx ended and the failure is
  that ending (`ctx.Err()` or `context.Cause(ctx)`), not a ctx error found anywhere in the chain: a
  primitive's own `context.DeadlineExceeded` under a live ctx, or a failure of the body's own that
  lands after the ctx ended, is an ordinary raise and stays one.
  Decided: remember (the body runs at most once), not re-run (Chez re-runs the body; Racket counts
  it instantiated with the definitions that ran). Pinned in `pkg/wile/transformer_callcc_test.go`
  by `TestLibraryBodyCancelledByATimeoutIsNotReportedAsADeadlineByLaterImports`,
  `TestLibraryBodyCancelledByTheEmbedderIsNotReportedAsCancelledByLaterImports`,
  `TestLibraryBodyFailingWithItsOwnDeadlineIsRememberedAsIs`,
  `TestLibraryBodyFailingAfterItsCtxEndedIsRememberedAsIs` and
  `TestLibraryBodyThatRaisedIsRememberedAsTheSameRaise`. Timer and break interrupts themselves do
  not replay: neither is emitted on a library body's root (`vmMacroEvaluator.EvalTemplate` builds
  it with no timer armed and no `brk`).
- [ ] **A top-level import of an aborting library body reports two sentinels** [Cleanup, S]: the
  first `(import …)` of a body that aborts to a tag no expansion-time chain holds fails through
  `controlSignalEscapedExpansion` (`pkg/wile/engine.go`) with `ErrInvalidArgument` ("no prompt
  found … at expansion time"); later imports get the remembered `ErrContinuationBarrier` ("exited
  through an abort"), and nothing crossed a barrier. Before `fix/library-abort-replay` both got
  the first form, because the replayed abort hit the same conversion. Fix shape, undecided: one
  sentinel for "this body's control-signal exit is remembered", used by the resume and abort cases.
  Found by the code lens of that branch's crosscheck, 2026-10-12.
- [ ] **Rootless fix, round-4 leftovers** [Cleanup, S]: Minors the round-5 crosscheck found in the
  round-4 remediation, filed under the scoped re-review rule. (1) `controlSignalEscapedExpansion`
  (`pkg/wile/engine.go`) formats the cut chain into its message with `%v`, so the location is
  printed twice, and its `wrapCompilationError("compile error", …)` adds a layer `compileExpr`
  wraps again ("expand/compile error: compile error: …"); its name also reads like a predicate.
  (2) `TestAbortAtTopLevelExpansionIsAnError` (`pkg/wile/transformer_callcc_test.go`) asserts
  only the sentinel, not the message, the `CompilationError` type, or the location. (3) In the
  same file, a comment in `TestControlInACompileTimeBody` says n = 10 where the row expects 11,
  and the `probe/user.sld` fixture is used by no row. (4) The doc of
  `TestThreadRootIsTheThreadsOnlyImplicitPrompt` (`extensions/threads/prim_threads_test.go`)
  opens with the test's old name. (5) `boundaryToken.runtime`'s doc
  (`pkg/machine/boundary_token.go`) records that a capture under a Go-written transformer reads
  as a runtime one rather than preventing it; fix shape: set `expansionRoot` where expansion
  contexts are created, not at the driver's entry.
  (6) `CompiledLibrary.memoizableInstantiateError` (`pkg/machine/compilation/library_registry.go`)
  pastes the body's failure into both of its conversions with `%v` and no Unwrap edge, which is
  deliberate (a reachable signal or ctx error is what later importers must not see) but is an
  unrecorded exception to the lossless-chain rule, and it prints the cause's location and stack
  trace a second time; same fix shape as (1). Found by the `fix/library-abort-replay` re-review.
- [x] **A `guard` that catches a raise fails in a library body or a procedural macro transformer**
  [Correctness, S–M, **Done 2026-10-11**]: both ran a root context under a bare `mc.Run()`
  (`EvalTemplate`, `invokeTransformerClosure` in `pkg/machine/macro_evaluator.go`), which resolves
  no prompt abort, so `guard`'s exit failed with "abort-current-continuation: no prompt found for
  tag #<continuation-prompt-tag:exit>". Shapes: a body `(define g (guard (e (#t 'guarded)) (raise
  'boom)))`; `(er-macro-transformer (lambda (f r c) (guard (e (#t 42)) (raise 'boom))))` with
  `(for-syntax (scheme base))` imported. Fixed with the resume row above: these roots and the
  bootstrap's (`runBootstrapStx`) run under `RunWithinBoundary`, which also resumes a `call/cc`
  captured in the body; a tail `call/cc` there used to work only through the deleted sub-context
  mode, a non-tail one failed. Pinned by `TestControlInACompileTimeBody`.
- [x] **A library body's uncaught raise escapes a `guard` around the import** [Correctness, S,
  **Done 2026-10-12**, branch `fix/library-raise-escapes-guard`]: `EnsureInstantiated` marks a
  body's uncaught raise that is not a cancellation (`machine.MarkUnlinkedRootEscape`), and the
  foreign-call bridge (`applyCallableError`) raises its condition in the importer's handlers, the
  raised object itself, as Chez and Racket do (the mechanism `thread-join!` uses for a joinee).
  With no handler it goes on uncaught, its full chain under a SETTLED mark, so a bridge below a
  running handler (which still sees that handler installed) does not hand it back: under that
  mutant the handler ran twice.
  Pinned by `TestLibraryBodyRaiseIsCaughtByAGuardAroundTheImport` and
  `TestLibraryBodyRaiseInAHandlerReachesTheOuterHandlersOnce` (`pkg/wile/transformer_callcc_test.go`).
- [ ] **A guard whose clauses miss reports the guard's location, not the raise's** [Diagnostics,
  S]: `(guard (e ((string? e) 'no)) (f))` with `f` = `(car 5)` reports the guard's line; the
  re-raise's carrier records its own site and the `NativeError`'s own location is not what
  renders. Since `fix/library-raise-escapes-guard` an import whose body raised takes this path
  too, so a missed guard around `(environment '(lib))` reports the import site where master
  reported `lib.sld:L:C`, and since `fix/transformer-raise-escapes-guard` so does a missed guard
  around an `eval` whose transformer raised. Found by crosscheck 2026-10-13.
- [x] **A macro transformer's uncaught raise escapes a `guard` around `eval`** [Correctness, S,
  Done 2026-10-13]: every root `vmMacroEvaluator` runs (a transformer, a `define-syntax`
  expression, a library body) is unlinked, so its escape is marked there (`markExpansionRootEscape`)
  rather than per caller; `EnsureInstantiated`'s own mark is gone, its library body reaching it
  already marked through `EvalTemplate`. A cancellation (`machine.CancelledBy`) stays unmarked.
  Absorbs the duplicate row "A raise escaping a procedural transformer under a runtime `eval`
  cannot be guarded", whose proposed demotion to an ordinary error would have lost the raised
  object and the non-continuable re-raise. A primitive used as a transformer is marked too: a
  failure `applyForeign`'s bridge converts is an escape on its handlerless sub-context (an arity
  mismatch is not, and stays an ordinary error the `eval` primitive raises). Pinned by
  `TestExpansionRootRaiseIsCaughtByAGuardAroundEval` and
  `TestExpansionRootCancelledInAWaitIsNotRaisedInTheCallersHandlers`.
- [ ] **A caught raise out of an expansion-time root skips the root's `dynamic-wind` after thunks**
  [Correctness, S–M]: a transformer running `(dynamic-wind void (lambda () (raise 'boom)) (lambda ()
  (display "AFTER")))`, with a `guard` around the `eval` that expands it, prints `GUARD` and never
  `AFTER`; leaving the same extent through `call/cc` runs it. A root that ends in an uncaught raise
  never unwinds its winders (`RunWithinBoundary` → `closeBoundary` only closes the token), which
  was unobservable while that raise ended the program; since `fix/library-raise-escapes-guard` and
  `fix/transformer-raise-escapes-guard` the caller's handlers take it and the program goes on, so
  library bodies show it too. Fix shape, unverified against the winding API: unwind the root's
  winders before the escape leaves `RunWithinBoundary`'s expansion-root arm. That runs the after
  thunk BEFORE a `with-exception-handler` handler in the caller, where Racket runs the handler
  first; inherent to raising after the root ended, so decide the order before fixing. Found by the
  code lens of this fix's crosscheck, 2026-10-13.
- [ ] **A `raise-continuable` out of an expansion-time root reports a non-continuable raise**
  [Diagnostics, S]: `(define-syntax m (lambda (s) (raise-continuable 'boom)))` under a returning
  `with-exception-handler` around `eval` reports "exception handler returned from non-continuable
  exception" at the `eval` site; `unlinkedRootEscape.raiseIn` always raises non-continuable. It
  cannot resume (the root is gone), but the message names a raise the user did not write and the
  transformer's raise site is no longer in the error. Fix shape: carry continuability on
  `ErrExceptionEscape` and give this case its own message. Found by the code and errors lenses of
  this fix's crosscheck, 2026-10-13.
- [ ] **Every transformer failure is labelled a `syntax-rules` failure** [Diagnostics, S]:
  `expandMacroInvocation` (`expander_time_continuation.go`) wraps any `InvokeTransformer`
  error as "syntax-rules transformer invocation failed", so a lambda, ER or primitive transformer's
  raise reads as a `syntax-rules` one. Fix: drop "syntax-rules" from the wrap; check for tests
  matching the text first. Found by the errors lens of this fix's remediation crosscheck, 2026-10-14.
- [ ] **A host function that forwards an `Engine.Call` failure makes it uncatchable**
  [Correctness, S–M]: an uncaught failure in the callee root leaves `RunWithEscapeHandling` as an
  `*ErrExceptionEscape`; `Engine.callCallable` wraps it in a `RuntimeError` whose `Unwrap` keeps it,
  and the caller's `applyCallableError` passes it through as a control signal. `(guard (e (#t
  'caught)) (host-call (lambda () (raise 'boom))))`, with `host-call` returning `engine.Call`'s
  error, ends the evaluation with `boom` (likewise for `car`, `error` and `with-exception-handler`);
  a host returning its own error is caught. HEAD too; `thread-join!` already turns a joinee's
  uncaught condition into a catchable one. Fix shape: re-raise `escape.Condition` in the caller.
  Since `fix/library-raise-escapes-guard` one form is caught by accident: a host forwarding
  `EvalMultiple("(import (lib))")` of a library whose body raised hands back an error whose mark
  is still live (no bridge on the callee root settled it), so the caller's guard catches it, while
  forwarding `(environment '(lib))` does not. The fix here should make the two agree.

### FCA-Derived

- [x] **Structural-reduction roadmap** [Planning, Done — closed 2026-07-08]: spent and archived to `memory/2026-05-07-structural-reduction-roadmap.md`. Tier A closed (`values/` PRs #747–#756, `environment/` PR #730, `registry/` PR #728); Tier B closed (`wile/` PR #764; `repl/` + `registry/helpers/` on `refactor/structural-reduction-b2-b3` — low yield as predicted, most findings refuted or declined as churn); Tier C reassigned to `plans/2026-07-01-staff-engineer-sweep.md`.
- [x] **Machine package structural reduction** [Done 2026-05-13]: all 7 findings closed. Shipped — `Stack.Push` max-stack (PR #734), `OpKind()` discriminator (PR #735), vmState value-register consolidation + ruleguard (PR #736), correlated-field sub-records (PRs #742/#743/#745). Declined — syntaxCase marker interface (PR #731), maxCallDepth sentinel removal, tail/non-tail opcode collapse via sign-bit encoding (PR #737: geomean +2.5%, all 16 benches slower), Stage-3 sub-records (field-independence analysis found no co-variance, `9382a3b3`). `memory/2026-05-06-machine-structural-reduction.md`
- [x] **Internal / values / environment structural reduction** [Done]: `internal/` all 7 findings (PRs #739–#741, including the `*SyntaxPair`/`SyntaxEmptyList` duality migration that restores Chez-conformant `(equal? (syntax ()) '())`); `values/` Phases 0–4 (PRs #747–#756 — 9 port types collapsed to one `*Port` with capability slots, ~900 LOC, and a `NumericTypeSpec` registry replacing the 12-step ADDING-A-NEW-NUMERIC-TYPE guide); `environment/` Phases 1–9 (PR #730), Phase 10 (`*LocalIndex` allocation audit) deferred benchmark-gated. Plans archived under `memory/2026-05-0*`.
- [x] **vmCore sub-struct extraction** [High, M, DECLINED on re-evaluation 2026-06-05]: the genuine always-transfer set is only `{env, template, pc}`. `callDepth` is a guarded maintained counter (`SaveContinuation` ++, `PopContinuation` --, derived from the parent by both continuation constructors), so bundling it forces override-after-copy at 4 sites and risks clobbering its guards. The FCA "High" rating rested on the divergent fields (`evals`, `envPooled`, `marks`), none of which a vmCore touches, and drift is already answered by `testVmStateFieldCoverage`. Net ~6 lines saved at 3 sites on the VM's hottest path. Parallels the decline of machine SR Finding 7 Stage 3.
- [x] **Bidirectional opcode conversion test** [Medium, S, **ill-posed as worded, closed
  2026-09-26**]: the row asked for a test that `operationToInstruction` and
  `instructionToOperation` cover the *same* opcode set. They cannot, and the asymmetry is the
  design. `operationToInstruction` (`pkg/machine/native_template.go:411`) emits
  `Instruction{Op: op.OpKind()}`, so its range is exactly the opcodes some `Operation` type names
  as its kind; measured 2026-09-26, none of the 41 `OpKind` implementations returns a fused or
  promoted opcode, and `grep -E 'OpPullApply|OpPushLiteral|OpPushGlobal|OperandCachedBinding'` over
  its body is empty. Those opcodes exist only downstream of the peephole pass (`OpPullApply` is
  synthesized at `pkg/machine/peephole.go:152`), yet `instructionToOperation` must still accept
  them, mapping each back to the first operation of the sequence it replaced — the lossiness its
  own doc comment records at `native_template.go:214-223`. So `operationToInstruction`'s range is a
  strict subset of `instructionToOperation`'s domain and a same-set assertion fails by
  construction. Each direction is separately pinned instead: `TestOpcodeRoundTrip`
  (`pkg/machine/native_template_test.go:259`) walks every opcode from 1 to `opCount`, skipping only
  `OpComplex`, and asserts `instructionToOperation` is non-nil;
  `TestOperationToInstruction_AllDirectDispatch` (`pkg/machine/instruction_test.go:149`) and
  `…_SideTableReturnsFalse` (`:203`) cover the other. This is a scope judgment, not a
  non-reproduction: nothing was fixed, and no existing test asserts the row's literal ask, because
  the property it names is false.
- [ ] **LocalEnvironmentFrame pointer ambiguity** [Low, S]: Doc comment on `NewLocalEnvironment` explaining lifecycle (value-vs-pointer ownership).
- [x] **Honor `WithInlineThreshold` for imported libraries** [Low, S, **already fixed, closed
  2026-09-26**]: the row said the library load path had no `inlineThreshold` parameter, so an
  imported library always compiled at `DefaultInlineThreshold = 5` and silently ignored the
  engine's `WithInlineThreshold(n)`. The gap is closed, and by the second of the two options the
  row named — exposing the value through `Namespace` rather than threading it through three
  signatures. The whole chain resolves, measured 2026-09-26: `pkg/wile/engine.go:595`
  `ns.SetInlineThreshold(cfg.inlineThreshold)`; `pkg/machine/compilation/library_loader.go:291`
  reads `libEnv.Namespace().InlineThreshold()`, falls back to `DefaultInlineThreshold` when unset
  (a namespace not built by an Engine, e.g. a direct `LoadLibrary` in a unit test), and calls
  `compiler.SetInlineThreshold(...)` at `:295`; the accessors are
  `(*Namespace).SetInlineThreshold` and `.InlineThreshold`
  (`pkg/environment/namespace.go:472`, `:481`). Three tests in
  `pkg/wile/library_inline_threshold_test.go` cover it — `TestInlineThresholdPopulatedOnNamespace`
  (`:34`), whose table carries the explicit-zero row that distinguishes "disabled" from "unset",
  `TestInlineThresholdHonoredForImportedLibrary` (`:73`) and
  `TestInlineThresholdBoundaryForImportedLibrary` (`:147`).
- [x] **Unified binding reference (`BindingRef`) for local+global** [Medium, M, Done 2026-07-08, `229e0b72`]: `BindingRef` sum type + `ResolveBindingRef` in `environment/`; the validator's mutation set went from 3 maps to 2. **Storage stays split, deliberately**: locals are positionally addressed (`LocalIndex{over,up}`), copied every `Apply`, single-threaded, `[]Binding` by value; globals are symbolically addressed, shared across SRFI-18 threads, and `[]*Binding` pointer-stable because the lock-free `cachedBindings` read cache requires it. Only the reference type unified. **Premise correction**: the validator was not blind to top-level `set!` (the symbolic `mutatedKeys` sidecar compensated and `StableInUnit` was correct), so this was a semantics-preserving tidy, not a bug fix. The conservative over-mark (a `set!` to a local shadow still marks the top-level name non-stable) is the frame-reclaim soundness margin, pinned by `TestStableInUnit_SetToLocalShadowStillMarksTopLevel`.
- [x] **Unify `atan2Operand` with `helpers.ToFloat64`** [Low, S, Done, PR #754]: `atan2Operand` re-implemented the Number-assert → complex-reject → float64-extract sequence just to swap the loss policy from strict to silent-truncate. Extracted shared `screenReal` into `registry/helpers/value_conv.go` and added `helpers.ToFloat64Lossy` as the lossy counterpart to strict `ToFloat64`; `atan2Operand` deleted, both `PrimAtan` sites routed through it. Lossy semantics (`(atan 1/3)`) preserved per R7RS §6.2.6.
### Tech Debt Plan (remaining)

- [ ] **Task 6.2: Replace `context.TODO()` in tests** [Low, S]: 522 occurrences across 61 test files (2026-10-16); none in production code. Mechanical `→ context.Background()`.
- [x] **Task 6.4: Add `typeswitchlint` to value type guide** [Low, S, Done — `a41ec0b7`]: resolved by a mechanism instead of a comment: `typeswitchlint` is opt-in, CI-gating, and drift-guarded (`tools/cmd/typeswitchlint/main_test.go`), so `knownValueTypes` diverging from the value-type set fails CI.
- [x] **Task 8.1: Extract `machine/compilation/resolver/`** [Done]: FileResolver implementations extracted. `LibraryEnumerator` replaced with `FileEnumerator.EnumerateFiles` (returns paths, not `LibraryName`). Type aliases in compilation for backward compat. `memory/2026-04-13-resolver-extraction-impl.md`
- [ ] **Task 8.2: Evaluate `wile.Value` wrapper** [Low, M]: Wrapper provides minimal methods beyond `Internal()` escape hatch.
- [ ] **Task 8.4: Make `DefaultBigFloatPrecision` configurable** [Low, M]: 256-bit precision hardcoded across 31 references (2026-10-16). No engine option.
- [ ] **Error sentinel grouping** [Low, S]: 131 sentinels in `werr.go` plus 23 package-local under `pkg/` and `extensions/` (2026-10-16), ~154 in all: past the ~150 trigger. Consider category-specific files now.
- [ ] **Namespace registry typing** [Low, S]: Namespace's registry should have a type instead of `any`.
- [ ] **ValueType refactoring** [Low]: ValueType doesn't have grounding in Scheme or Go — determine use and scope of type domains.
- [ ] **Evaluate need for Primitive Annotation Enforcement** [Low]: Enforcement may not be needed.

### Algebra library consistency (2026-04-23 staff-engineer audit)

- [x] **Shared helpers promoted into `(wile algebra setoid)`** [High, S, Done]: `setoid-member?`, `setoid-assoc`, `setoid-dedup`, `assv-or`, and `validate-opts-keys` are public there; the private `%`-copies are deleted from group, combinatorial-graph, incidence, and lattice — four libraries, not the two the audit found.
- [x] **Drift-check test for umbrella `algebra.sld`** [High, S, Done — option (c)]: `algebra_umbrella_drift_test.go` parses every leaf `.sld` export clause and asserts umbrella coverage. First run caught real drift (`rewrite.sld`, `semiring.sld`). Deleting or generating the umbrella stays deferred until measured drift frequency justifies either.
- [x] **Structure-API convention documented instead of abstracted** [Medium, S, Done]: a `define-with-binder` meta-macro would have saved ~10 lines across 15 libraries at the cost of indirection — declined. `stdlib/lib/wile/algebra/CLAUDE.md` documents the five-part structure API (`make-X` / `X?` / accessors / `with-X` / `validate-X`), the shared plumbing, options-alist discipline, and validator shape instead; duplication stays mechanical.
- [x] **`validate-X` / `assert-X` / `make-X` idioms collapsed to generic helpers** [Medium, S, Done]: `make-violation-reporter` retrofitted across 14 libraries, replacing the `(set! violations (append …))` parent-delegation pattern; `assert-validation` (+1 symbol, not 18 per-structure `assert-X`) keeps the source expression in the error datum; `assert-procedure` covers 11 non-validating `make-X` constructors and names both sides.
- [x] **`combinatorial-graph.scm` monolith — first cut** [Medium, M, Done (partial)]: custom insertion sort replaced by `list-sort` from `(srfi 132)`; 1,787 → 1,726 lines. The remaining `%`-helpers are genuinely WL/isomorphism-specific; splitting into sub-files is deferred until it buys review scope.
- [ ] **Watch `matrix.scm` for split pressure** [Low, S, Deferred]: 1,313 lines with two record types (`<semiring-matrix>` at 841, `<sparse-semiring-matrix>` at 1148) in one file. Shared helpers justify co-location today. Revisit once a third representation (banded, symmetric, etc.) appears — no action needed now.
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

- [x] **`SetValues(sub.GetValues()...)` nil-vs-empty ambiguity** [Tech debt, M, **closed 2026-09-26 by option (b): the collapse is intentional and is now documented**]: taking the row's own second option. `nil` IS the zero-values encoding, per auto-memory `feedback-nil-means-none-not-wildcard` (nil ≡ NONE; a wildcard, or any other distinguished state, needs an explicit named value). One sentence saying so was added to `(*vmState).GetValues` (`pkg/machine/vm_state.go`), which is the function whose return value the row reads as ambiguous. The semantics are unambiguous where it matters: `(call-with-values (lambda () (call/cc (lambda (k) (values)))) list)` round-trips to `()` in Wile, in `racket` and in `petite` (verified three ways 2026-09-26; note `racket -I r5rs -e …` fails on this program with "cannot reference an identifier before its definition" — use plain `racket -e`). **The original filing's 13-site list, quoted at the end of this row, is stale — record the pattern, not the spelling.** Four sites match `X.GetValues()...` today: `extensions/files/prim_files.go:226`, `extensions/eval/prim_eval.go:155`, `pkg/registry/core/prim_control.go:227`, `pkg/registry/core/prim_prompt.go:251`. Two of the four spell the receiver `target`, not `sub`, so a bare `sub.GetValues()` grep finds only two of them; grep the pattern. No ratchet: a Go test asserting `GetValues()` returns nil after a zero-arg `SetValues()` is green by construction (`SetValues` nils both fields on `len == 0`, `GetValues` returns nil when both are nil), so it would be a pin, not a gate. Original filing, for the record: Silent-failure-hunter flagged 13 call sites that propagate a sub-context's value register into the parent via `mc.SetValues(sub.GetValues()...)`. `GetValues()` returns `nil` for an empty register (both fields nil); spreading `nil...` calls `SetValues()` with zero args, which now canonicalizes to (nil, nil) post-Q-e. Sub-contexts that exited abnormally without writing a value, sub-contexts that returned `(values)` (R7RS zero-value return), and sub-contexts that returned a real value all collapse into indistinguishable parent-side state. Call sites: `extensions/eval/prim_eval.go:104`, `extensions/files/prim_files.go:179`, `registry/core/prim_timer.go:127`, `registry/core/prim_barrier.go:72`, `registry/core/prim_cont_marks.go:187`, `registry/core/prim_prompt.go:135,149`, `registry/core/prim_control.go:87,200,365`, `registry/core/prim_exit.go:105`. Pre-existing; surfaced by but not introduced by PR #736. Fix shape: distinguish "no value produced" from "(values) zero-return" at each call site, or document the collapse as intentional R7RS behavior.

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

- [ ] **`*SyntaxObject.Datum()` and `*SyntaxObject.Unwrap()` duplication** [Tech debt, XS, Deferred — pre-existing]: Both methods return `p.datum` with no transformation (`pkg/syntax/syntax_value.go`, `Datum` and `Unwrap`). `Unwrap` is the `SyntaxValue` interface method; `Datum` is the historical accessor. Pre-existing; surfaced by but not introduced by PR #739. Fix shape: audit callers (which name does each use?) and delete one. If Unwrap is interface-required, delete Datum or make it a one-line forward; otherwise reverse the choice. Out of scope for the structural-reduction phases; clean-up commit when next touching syntax_value.go.

- [ ] **`qt.Assert(t, ...)` vs `c := qt.New(t); c.Assert(...)` style split in `internal/validate/`** [Tech debt, S, Deferred — pre-existing]: The validate package's test files mix two quicktest invocation styles. Older files (`walk_sub_exprs_test.go`, `validate_escape_test.go`; `validate_capture_test.go` was deleted in `a8a266d4`; 7 files vs 8, 2026-10-16) use the `c := qt.New(t); c.Assert(...)` form; recent additions (`env_helpers_test.go` from PR #739, `walk_binding_refs_test.go` from PR #740) use the package-level `qt.Assert(t, ...)` form. Both are valid quicktest API; the split is purely stylistic. Fix shape: pick one and propagate — likely the package-level `qt.Assert(t, ...)` since it's the more recent precedent and is what other Wile packages use. Out of scope for any one PR.

### Loss-signals API follow-ups (numeric-loss-signals impl)

Items from the numeric loss-signals plan
(`memory/2026-05-14-numeric-loss-signals-design.md` /
`memory/2026-05-14-numeric-loss-signals-impl.md`). Track decisions
that were made on the impl path but warrant revisiting once usage
patterns are visible.

- [ ] **Revisit hybrid return shape if helper set grows** [Tech debt, M, Deferred]: Current API uses a **hybrid** return shape — `ToFloat64WithAccuracy` returns positional 4-tuple `(float64, big.Accuracy, bool, error)`; `ToComplex128WithAccuracy` returns `(Complex128Result, error)` with a named struct (fields `Value`, `RealAcc`, `ImagAcc`). The rule: positional when slot types disambiguate roles; struct when adjacent slots share a type and could be silently swapped. Decision rationale + alternatives (all-positional, all-struct) documented at `memory/2026-05-14-numeric-loss-signals-design.md` § "Decision record: return shape — hybrid (positional + struct)". **Revisit triggers**: (a) a second `WithAccuracy`-shaped helper with a single accuracy signal is added (rationals, intervals, matrix elements with one component) — re-evaluate whether the new helper should follow `ToFloat64WithAccuracy` (positional) or be promoted to struct for consistency with `ToComplex128WithAccuracy`; (b) a third or fourth multi-component helper is added (quaternion, matrix with N≥3 same-type slots) — at that point the asymmetry-as-domain-structure argument weakens, consider a unified struct convention; (c) FFI converter is refactored to consume the struct directly for both helpers (eliminates the discard-idiom advantage motivating positional `ToFloat64WithAccuracy`); (d) a `realAcc/imagAcc` swap bug is reported despite the struct — indicates the safety property failed, revisit whether stricter encoding is warranted (e.g., distinct newtypes `type RealAccuracy big.Accuracy` / `type ImagAccuracy big.Accuracy`).
- [x] **Big-precision numeric sweep: rounding, division, transcendentals** [Correctness, L, Done 2026-07-07; branches `fix/bigcomplex-precision-loss`, `feat/bigcomplex-angle-atan2`, `feat/big-transcendentals`, `feat/big-complex-transcendentals`]: no numeric path round-trips through float64. Rounding and the `floor-`/`truncate-quotient`/`remainder` family take exact `big.Int`/`big.Rat` paths; `pkg/values/big_transcendental{,_complex}.go` adds arbitrary-precision `BigPi`/`BigAtan2`/`BigExp`/`BigLog`/`BigSin`/`BigCos`/`BigAsin` and complex twins, since `math/big` has no transcendentals. `BigSin`/`BigCos` scale working precision to the argument's exponent (the big analogue of Payne–Hanek). **Tier rule**: unbounded-tier input (BigFloat/BigInteger/Rational/BigComplex) yields `*BigFloat`; bounded stays float64, where `math.*` is already correct. Complex kernels run only where `cmplx.*` returns non-finite, so in-range branch-cut behavior is unchanged; `exp` also rescues a bounded operand whose `math.Exp` overflows (~709). Caveat: `BigComplexAtan` on the imaginary-axis branch cut returns `+π/2` where Go gives `−π/2`, reachable only outside float64 range. Pins: `TestRoundingBigPrecision`, `TestBigComplexTranscendentalPrecision`. Designs: `memory/2026-07-07-bigcomplex-angle-atan2-design.md`, `memory/2026-07-07-big-transcendentals-design.md`. Residual: rational-operand remainder still truncates through float64 (`(floor-remainder 7/2 2)` → `1`, exact `3/2`), non-conformant args per R7RS §6.2.6.
- [ ] **Unify `ErrLossyConversion` / `ErrNotAReal` for the imag-drop case** [API design, S, Deferred]: After this PR, two sentinels flag the same underlying condition depending on which surface a caller uses. `ToFloat64Lossless` returns `ErrLossyConversion` (wrapped) for its `!isReal` branch (`values/conversion.go`); `NumberToFloat64` panics with `ErrNotAReal` (`values/promotion.go`). The two sentinels carry the same information about the failure but are not interchangeable for `errors.Is` callers. **Options**: (a) reuse `ErrNotAReal` in `ToFloat64Lossless` and reserve `ErrLossyConversion` strictly for rounding loss; (b) accept `ErrLossyConversion` as the canonical sentinel for any precision/component loss and document the historical role of `ErrNotAReal` as a `NumberToFloat64`-only panic discriminator. Revisit when an FFI consumer reports a confusing `errors.Is` mismatch, or when adding a third surface helper that needs to choose between them.
- [ ] **Reconsider `Exact` overload for NaN/Inf identity** [API design, M, Deferred — design choice Q-6]: The `big.Accuracy` slot returned by `ToFloat64WithAccuracy` is overloaded: (a) genuinely lossless rounding, (b) NaN bit-pattern identity, (c) preserved literal infinity. Doc tightening landed in this PR at `values/conversion.go:54-60` (per design Q-6 resolution). Callers screening "is this a meaningful real number?" must use `math.IsNaN` / `math.IsInf` independently. **Trigger to revisit**: a caller reports being unable to distinguish "rounded-but-real" from "NaN-or-Inf" from the tuple alone, OR a fifth `WithAccuracy` helper is added where the overload becomes too costly to maintain. Possible fix: a 4-valued enum `LossKind { Lossless, RoundedBelow, RoundedAbove, NaNOrInf }` replacing `big.Accuracy` at the public surface; would diverge from Go's stdlib vocabulary at the cost of being self-describing.
- [ ] **`ToFloat64Lossless` returns rounded value on error** [API design, S, Deferred]: When the conversion would round, `ToFloat64Lossless` returns `(f, ErrLossyConversion)` where `f` is the lossy float64 result — the caller can use the value if they want; the error is advisory. The nil-input error path returns `(0, ErrNotANumber)`. The asymmetry is real: lossy ⇒ best-effort value preserved; nil-input ⇒ zero. Go convention is "non-nil error ⇒ value is unspecified," which the lossy path softly violates. **Decision deferred**: changing this would force every strict-mode caller to abandon their value on rounding (which they wanted to fail-fast on anyway). Document the contract instead. Revisit if a caller reports relying on the "use the rounded value alongside the error" pattern in a way the API should officially support, OR if the asymmetry causes a bug at an FFI boundary.
- [ ] **`ToFloat64WithAccuracy` nil-defense: error vs panic** [API design, S, Deferred]: The function returns `ErrNotANumber` (wrapped) when `n == nil`. The signature is `n Number`, so a non-Number cannot be passed — the nil case is the only reachable error path. The neighboring `LookupNumericSpec` (`values/numeric_registry.go`) panics on analogous defensive bugs (out-of-range kind). The split is a style choice: errors-for-FFI-safety vs panic-for-Go-bug. **Revisit when**: (a) the wider codebase converges on one convention for "this should never happen" defensive checks at the `values` boundary, OR (b) an FFI consumer demonstrates a real path where `n == nil` is reachable from outside the type system (unlikely but possible via reflection paths).
- [x] **File naming: `conversion.go` lacks `numeric_` prefix** [Tech debt, S, **premise refuted, closed 2026-09-26 — leave both alone**]: the row read the prefix as marking the numeric domain and found one exception (`promotion.go`). Counted 2026-09-26, the ratio inverts the reading: `ls pkg/values/numeric_*.go | grep -v _test` gives exactly **4** files (`numeric_kind.go`, `numeric_registry.go`, `numeric_repr.go`, `numeric_tower.go`) against **12** unprefixed numeric-domain files (`big_complex.go`, `big_float.go`, `big_integer.go`, `big_transcendental.go`, `big_transcendental_complex.go`, `complex.go`, `conversion.go`, `exact_zero.go`, `float.go`, `integer.go`, `promotion.go`, `rational.go`). The prefix marks **tower machinery** — the kind enum, the spec registry, the representation, the tower itself — not the numeric domain, so `conversion.go` and `promotion.go` are correctly unprefixed and unprefixed is the majority spelling, not the exception. The row's "revisit when a third unprefixed numeric file is added" trigger fired long ago at 12 and the convention resolved the other way. If the rule is written down at all it goes in a **tracked** file (`CODING_STYLE.md`), never `pkg/values/CLAUDE.local.md`, which `.gitignore:48` (`*.local.md`) excludes — `git ls-files pkg/values/ | grep -i claude` is empty, so CI never reads it and neither does a fresh checkout.
- [ ] **Symbol-singleton location: `symbols_accuracy.go` separate vs co-located** [Tech debt, S, Deferred — taste call]: Prior art for state-symbol singletons in `values/` (`SymbolThreadNew` etc. in `thread.go`, `SymbolMutexNotOwned` etc. in `mutex.go`) puts them in the file of the owning type. `SymbolAccuracyBelow`/`Exact`/`Above` live in their own `symbols_accuracy.go`. The split is defensible (the symbols paraphrase `big.Accuracy`, not a Wile type; the natural owner would be `big_float.go` or `numeric_registry.go`, neither of which is a clean fit). Revisit if a third orphan symbol-set appears (then either consolidate all orphans into a single `symbols.go`, or formalize the per-domain-file convention). Update `values/CLAUDE.md` "Sentinel/Singleton Values" inventory either way.

### Postponed

Items deferred for stated reasons. Re-evaluate when preconditions change.

- [x] **F11: Promote internal extensions** [Postponed, **premise stale, closed 2026-09-26**]: two of the three named packages are already public and the third is reachable. Measured 2026-09-26: `ls pkg/extensions/` → `io`; `eval` is public at `extensions/eval`; `ls pkg/internal/extensions/` → `all envvars iotest namespace`, so of the `{io,eval,all}` triple this row names, only `all` is still internal (`envvars`, `iotest` and `namespace` are internal too, but outside the row's scope). "Invisible to embedders" survives only for the ad-hoc `WithExtension()` route: `all.Extension` and `all.SafeExtension` are reachable through `ProfileExtensions` (`pkg/internal/bootstrap/bootstrap.go:82`, with `all.Extension` in `allExtensions` at `:65` and `all.SafeExtension` at `:91` and `:100`), which is the profile API embedders are pointed at. The narrower rewrite — promote `all` out of `pkg/internal/extensions/` — is **declined**: `ls extensions/` gives 12 entries, 11 of them extension packages and all already public, so `all` is a bundle of public parts and exporting the bundle adds surface without adding reach. Its stated precondition ("external consumers exist") is also unmet — see auto-memory `deadscan-ext-refs-are-mostly-first-party` (95 of 102 external references are first-party, so "wile-goast builds against it" is not independent-consumer evidence) and `embedding-size-cuts-deferred` (trim the surface via profiles now; defer the structural moves until a user asks).
- [x] **Parser: unify readList + readLabeledList** [Postponed, **blocker refuted, closed
  2026-09-26**]: the row postponed the unification as high risk because "structural difference is
  semantic, not accidental." It was accidental, and the difference was one parameter. Both readers
  now delegate to a single `readListInto(head *syntax.SyntaxPair, opener)`
  (`pkg/parser/parser.go:643`), which holds the list grammar once: `readList` (`:632`) passes a
  fresh head (`p.wrapSyntaxPair(nil, nil, p.cur)`) and `readLabeledList` (`:317`) passes the
  already-registered placeholder pair. The in-place mutation the row feared is exactly what
  passing the placeholder as `head` buys: the shared loop fills that exact pair, so a `#n#`
  encountered mid-list already resolves, and no traversal patches anything afterward. What is left
  in the wrapper is one label re-store, for the case where the list turns out empty (`#0=()`, or
  one whose elements were all elided by `#;`) and the result is the empty-list singleton rather
  than the placeholder. Pinned by
  `TestReader_CircularListResolvesSelfReference` (`pkg/parser/datum_label_test.go:100`) and
  `TestReader_LabeledListElidedElementIsEmptyList` (`:125`), whose comment names the
  `readList`/`readLabeledList` unification as what it guards.
- [ ] **VM dispatch loop extraction** [Postponed]: `MachineContext.Run()` is 829 lines with 77 inlined opcode arms (measured 2026-10-16). Go has no computed goto; method dispatch adds measurable overhead on hot path. Intentional performance-over-readability trade-off.
- [ ] **Match: consolidate bytecode type files** [Postponed]: Pure cosmetic reorganization.
- [x] **Extensions: standardize registration patterns** [Postponed, **precondition met, closed 2026-09-26**]: the design decision the row waits on was made, and the sweep is already uniform. Per-directory measurement under `extensions/` (2026-09-26): 11 of 12 top-level entries register through `NewDescribedExtension`; the twelfth, `extensions/algebra/`, holds no `.go` file at top level. It is a container for `graph`, a pure graph-kernel package (`monotone.go`, `scc.go`) whose only non-test consumer is `extensions/algebragraph/prim_count_paths.go:20` — not an extension package, so `NewDescribedExtension` does not apply to it. State the denominator rather than an unqualified ratio: **11 of 12 entries, 11 of 11 extension packages**.
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
  `MatchSyntaxWithLiterals` (`match.go`, cog 155, nesting depth 6 — the worst genuine function in
  the tree), `CompileTimeContinuation` (**131 methods / 21 fields / 26 files** at filing, 137 methods / 28 files on 2026-10-16; the method count the
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
  (`memory/2026-06-26-promoted-primitive-inline-registry.md`, archived 2026-10-16).
- [x] **`docs/` audit sweep** [Docs/verification, **goal met by another route, closed
  2026-09-26**]: the docs-subsystem sweep (`memory/2026-04-23-docs-sweep-impl.md`, follows
  algebra-docs). The §4 review audit AU.1 that shared this row was run 2026-08-09 with a
  negative verdict, that plan's own header says all 23 items are closed, and it is archived:
  `memory/2026-07-15-review-2026-07-13-sec4-remediation.md`. The sweep's own goal was met by
  `99b8c99d` instead. Coverage at close, measured 2026-09-26: **53 `.md` across 15
  directories** — `docs/` plus 14 documentation subdirectories. (A bare `find docs -type d`
  reports 16; the extra one is `docs/.claude`, an empty Claude Code config directory holding
  no files at all. Cite 15, or the next re-measurement contradicts this row.)
  **RISK NOTE, and it is the load-bearing line here: do NOT schedule re-measuring
  `99b8c99d`'s benchmark and profile figures as part of a documentation sweep.** That commit
  deliberately labels un-re-measured figures with their measurement context rather than
  presenting them as current, and re-measuring them means an interleaved A/B against a named
  baseline — same-binary drift is +1.2% geometric mean, so a sequential before/after cannot
  see the delta at all (auto-memory `interleaved-ab-required-for-vm-microdeltas`). That work
  belongs to the perf wave, not here.
  Correcting the design's "nothing cheap can gate accuracy", which is too strong:
  `TestCorrectedDocClaimsStayCorrected` (`cmd/wile/docs_refs_test.go:229`) and
  `TestProseCountsMatchTheirSource` (`cmd/wile/docs_counts_test.go:60`) are exactly cheap
  accuracy gates, both already reached by `make ci` through `test` (`Makefile:146`, `:180`).
  What they cannot do is enumerate: each gates **one named claim**, which is the honest scope
  — this closeout phase uses six rows of the first. What is genuinely unstartable in CI is
  the plan-header check, because `plans/` lives in the separate `wile-memoplans` repo and is absent from a CI checkout of `wile`: `head -5
  plans/2026-04-23-docs-sweep-impl.md` (since archived to `memory/`) still reads `status = "Planned — not started"` while
  the goal it names has shipped. That is `plans/`-header rot, and it is the same asymmetry
  that correctly keeps `planlint` (`tools/sh/planlint.sh`, `Makefile:685`) out of the `ci`
  target.

> **Stale-status housekeeping (planlint evidence), DONE 2026-09-04.** `2026-07-17-review-remediation.md` + `-impl.md` and `2026-07-12-numeric-zero-and-tier2-fold.md` showed unchecked boxes while fully shipped; all three archived to `memory/` in the 2026-09-04 pass with their Plan Index rows, alongside eight whose headers said shipped and the lingering rows of three earlier archives (r6rs hashtables, ambient keywords, literal-pin ambiguity).

### Closed in Tier 5

Moved to [`closed.md`](closed.md) (2026-10-17 split).
