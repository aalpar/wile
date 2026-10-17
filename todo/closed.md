# Closed rows — Closed in Tier 1, Closed in Tier 5, Completed

> One of the files [`TODO.md`](../TODO.md) is split into (2026-10-17). `TODO.md` holds the
> status, ordering, conventions, Plan Index and Top Priority table; this file holds rows.
> Cite a row by its bold text, never by line number. `make indexlint` and `make planlint`
> read `TODO.md` and `todo/*.md` as one corpus.

## Closed in Tier 1

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
  one hung package, not the run. `-timeout 30m` per package and `timeout-minutes: 40` (step plus
  ONE per-package firing; widened from 8m/25m in `8f323b4f`, rationale in `race.yml`), because `go test`'s own timeout goroutine dump is the entire
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
  `newSealedAxisFrames` (now `newPhaseRegistry`, since `c6eb4302`) is the one builder for every owner, and **an owner does not pick a subset
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
  (`TestFindLibraryBindingPrefersRuntimeOverExpand`; superseded for `findLibraryBinding` on
  2026-09-16 by `372d703c`, which probes the export's phase, pinned by
  `TestFindLibraryBindingAtExportPhase`) and a phase-3 define is exportable
  (`TestLibraryExportsPhaseThreeBinding`); `PhaseTemplate` (-1) is excluded deliberately.
  Residual: Q3, a predeclared-but-unwritten slot reads as `#!void` rather than raising; the
  `predeclareBinding` twin-slot entry was separately closed 2026-09-26 as count-refuted (Tier 5), and closing this did not require it, which shows
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
    ~~`findLibraryBinding` takes the requesting phase (entry above, decided (a))~~ declared
    per-phase exports (done 2026-09-16; fork (a) withdrawn), making `syntax-rules` importable →
    the phase-1 vocabulary becomes declarable → only then is zero reachable.

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
  passes it to `copyLibraryBindingsDirect`'s new `targetPhase` (`copyLibraryBindingsDirect` was replaced by `planLibraryImport` in `4fab03c3`); syntax lands at a phase composed
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
  R7RS §5.2; same-kind clashes still are.
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
  (`RunResumable`/`reinstallSegment`) in O(1) Go frames. `maxContinuationDepth`/`contNestDepth`
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
  Invariant H (auto-memory `envpooled-clear-is-invariant-h`) survives because the one Go site that
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

---
## Closed in Tier 5

Fully closed sections, collapsed to the archive form on 2026-09-15 and moved below the
open work. Headings kept so old references still find them.

#### List/Pair Primitive Cleanup (from inline annotations)

- [x] **List/pair primitive cleanup** [Low, XS–S, Done]: relocated from inline `// CLAUDE:` source annotations, removed because inline comments shift primitive line numbers and break `TestBuildAxisBManifest`. `(*Pair).Append` and the `Tuple.Append` interface method deleted as dead (superseded by `PrimAppend`). `PrimListCopy` moved to `PairBlock`, reversing the earlier tail-pointer choice: `Tuple.ForEach` yields the count and the terminating cdr, so an improper tail survives by re-pointing the block's last cdr after `LinkSpine` (63→18 allocs, ~10% faster on 50 elements). `PrimAppend` stays the sole tail-pointer exemplar, since multi-arg concat cannot pre-count.

#### `syntax-rules` derived over `syntax-case`: candidate (2026-09-04)

- [x] **Derive `syntax-rules` in Scheme over `syntax-case`** [Macro system / dedup, M, Done, superseded 2026-09-04 by `plans/2026-09-04-scheme-specified-syntax-forms-design.md`, which makes this its P2 and adds `syntax-case` itself]: kept for the sizing. The R6RS definition (~40 lines, core forms only) at the top of `pkg/registry/core/bootstrap_macros.scm` retires `operation_syntax_rules_transform.go` (358) and the syntax-rules-only parts of `compile_syntax_rules.go` (934; `collectFreeIdentifiersWithEllipsis` and `collectPatternVariablesWithEllipsis` are shared with `syntax-case` and `(syntax …)` and stay). Template instantiation is already one path (`match.SyntaxMatcher.Expand`). Two gates: (1) reliability inversion, since syntax-rules carries 13 archived fix plans and syntax-case had silent failures as late as PR #732, so verify use-site scopes (Design B) and pattern-variables-as-bindings (Design C) apply on the syntax-case path; (2) startup, one closure call + fender per clause per derived-form expansion against ~4.3 ms / 159k allocs, interleaved A/B on `BenchmarkEngineStartup`. `plans/2026-09-04-expander-in-scheme-assessment.md` §2.

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
