TODO
----

**Last Updated**: 2026-07-20 (Tier 1: transcribed the scope-keyed-globals arc's open successors out of `plans/2026-07-18-scope-keyed-global-bindings-design.md`, where they were invisible to a TODO scan — `freeIds` name-keyed collapse and the `BindingID` scope-discriminator prerequisite joined the name-keyed-consumers section; new "successor work" section holds the two shorthand-`define` defects, export phase-probe order, no-sealed-base-above-phase-0, and the missing CHANGELOG for 16 commits of user-visible semantic change. Tier 5: `predeclareBinding` orphan slots. Two of the successors carry **unverified repros** and say so. Prior: 2026-07-14 — Tier 1: recorded the 2026-07-13-review remediation resolved on 2026-07-14 — numeric-lattice `eqv?`/`equal?` F1/F2/F3 + Value Go-comparability (`c302b702`); macro-introduced top-level binder hygiene + `define-values` under NoMutation (`d594beeb`, general form-removal `*PrimitiveExpander` leak STILL OPEN); export supersets + `(description)` documented (`cc3c48bb`); `GlobalIndex` env-literal identity (`fa9804d6`). Opaque-subtree over-marking finding remains open. Prior: 2026-07-01 — D2 thread-shared-global race / `fbcd7654`; stale `peek-char`/`read-line` read-error bugs / `460c73a5`; Scheme-side line coverage, `PrimitiveSpec` capture-safety field / PR #776, Task 6.4 typeswitchlint drift-guard)

### Current Project Status

> Authoritative status lives elsewhere — this header is orientation only and
> drifts. For the current version see [`VERSION`](VERSION) / `wile --version`
> and [`CHANGELOG.md`](CHANGELOG.md); for the documentation map see
> [`docs/INDEX.md`](docs/INDEX.md). Counts below are approximate.

**Version**: 1.17.x line (see `CHANGELOG.md` for the latest release)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower, core-compiled let forms
**Extensions**: 12 extension packages — 8 public (files, math, process, system, threads, gointerop, introspection, charsets), 4 internal (io, eval, namespace, all); all importable as R7RS `(wile <name>)` libraries. Extension API contracts (ValueType enum, PrimitiveSpec type declarations) in Phase 1. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Embedding**: CLI uses public Engine API; embedded stdlib via `stdlib.FS` (`go:embed` + `fs.Sub`); named profiles (`Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink`) via `WithProfile`; orthogonal `WithSandbox` modifier; virtual env map (`WithEnv`, `WithEnvMap`); `Engine.AvailableLibraries()` API for library discovery.
**Documentation**: Complete documentation system — `procedure-documentation`, `,doc`, `,apropos`, `,topics`, `,topic`, library-level `(description)` clause, docstring examples, examples filtering. All 397 primitive specs + ~243 stdlib procedures + 29 special forms + 15 macros + 11 syntax compilers + 34 library descriptions documented.
**MCP Server**: Built-in MCP server mode (`wile --mcp`) with eval, doc, apropos, topic, libraries, and reset tools.
**Examples**: 75 examples across 13 categories, 23 benchmarks (16 Gabriel + Larceny R7RS + Schelog + miniKanren)
**Tests**: Go test suite comprehensive; Scheme test suite: 3,852 lines across 19 files (11 scheme + 8 algebra) + R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1), (srfi 13), (srfi 14), (srfi 132), (wile strings), (wile charsets); `(wile algebra)` umbrella with 27 sub-libraries — setoid/monoid/group/ring/field/lattice/boolean/heyting/category/closure/differential/galois/semiring/order, plus matrix (Path D, sparse+dense), polynomial (ring-parameterized), incidence (Möbius), unification (AC-matching), combinatorial-graph (1-WL + isomorphism + Kernighan-Lin balanced partition), matching (Roth-Sotomayor + Hungarian), symbolic/abstract-domain/dataflow (wile-goast extraction), fca, pareto, interval, rewrite, graph; stdlib embedded in binary

### Ordering

Items ordered by perceived priority for the project's success as an embedding product. Tiers: Security/Correctness → Embedding API → Tooling/DX → Performance → Tech Debt → Deferred → Nice-to-Haves. Completed items at the bottom for reference.

### Conventions

- **Completed items** are marked `- [x]` and include `Done` (or `Done — note`) in the brackets after the difficulty estimate. Example: `[Medium, Done]` or `[Medium, Done — P3 deferred]`. The bracket marker makes completion machine-grep-able alongside the Markdown checkbox; the bracket note may carry a one-line deferral fact when a sub-item is intentionally postponed.
- **Deferred sub-items within a completed parent** are noted parenthetically in the entry body (e.g., *"Phase 10 deferred — benchmark-gated"*) rather than spawned as a separate `[ ]` entry. Re-open as a top-level entry only if the deferral becomes the active work.

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

### Ambiguous binding references resolve silently instead of erroring (2026-07-18)

- [ ] **`bestOf` picks arbitrarily on a scope-set weight tie** [Medium, S]: when two candidate
  bindings have **incomparable, equal-cardinality** scope sets that are both subsets of the
  reference's scope set, Flatt/Racket raise an *ambiguous binding* error. Wile raises nothing:
  `pkg/environment/best_of.go:60-68` keeps the **first** candidate on a tie, so the reference
  resolves silently innermost-first. The macro-introduced-top-level-binder rename pass
  documented the same arbitrariness independently — ties "resolve arbitrarily to the first
  collected". That pass was **deleted** in `a60e32e1` once scope-keyed globals made it
  redundant, so read it at `git show a60e32e1^:pkg/machine/compilation/toplevel_binder_hygiene.go`
  (lines 249-254). Its deletion does not close this item: `bestOf` is untouched and still
  first-wins on a tie.

  **The fix belongs in `bestOf`, not at a call site.** The gap is uniform across local and
  global resolution; fixing one path alone would make it stricter than the other and create a
  new inconsistency. Landing it in `bestOf` covers both at once.

  Deliberately scoped **out** of two plans that each touch adjacent code, so it does not get
  bundled and lost: `plans/2026-07-18-scope-keyed-global-bindings-design.md` (Part III) and
  `plans/2026-07-18-load-order-independent-binding-resolution-design.md` (Fork C — same
  conclusion, reached independently). Both are prerequisites in practice, since scope-keyed
  globals make equal-cardinality ties reachable at top level for the first time.

### Name-keyed identity survives in consumers of scope-keyed bindings (2026-07-19)

All are consequences of `8afeb66a`/`a60e32e1` making a name own several slots. Each is
filed here because it existed only inside a `plans/` section — several of them marked
RESOLVED — where anyone scanning for open work will skip it. Written for three consumers;
`freeIds` and `BindingID` were added 2026-07-20 from the same Stage C review, which raises
the count to five and is itself the argument for not trusting a hardcoded one.

- [ ] **Frame-reclaim's verdict domain is name-keyed** [Medium, M]: `buildReclaimGraph`'s
  `byName` is `map[string]*reclaimNode` keyed on `Sym.Key`, and `frameReuseForDefine`
  (`pkg/machine/compilation/compile_define.go`) reads the verdict back by bare key, so two
  hygiene-distinct top-level defines of one name share a verdict. The **unsoundness is
  closed** — `390e1a35` stamps `reclaimNode.collided` and `nodeSafe` treats a collided node
  as unsafe, verified load-bearing by neutralizing the flag and reproducing `got["f"] == true`.
  What remains is that the fix is collision-*conservative*: any name defined twice in a unit
  forfeits reclamation. Making the verdict domain itself scope-aware is the general fix, and
  it waits on a scope-discriminated binding identity (the shipped
  `BindingID{*LocalEnvironmentFrame, slot}` is a physical local slot and is **not** the
  vehicle). Recorded at `plans/2026-07-18-scope-keyed-global-bindings-design.md` under
  "BLOCKER (RESOLVED)".

  - [x] **Sub-hole closed: the verdict leaked below top level** [Correctness, S,
    Done 2026-07-20, `82046952`]: `collided` closes the same-`Key` hole for **top-level**
    binders, which is all `collectTopLevelDefines` visits. An **internal** define can share
    the `Key` too, and reached `frameReuseForDefine` with the map still live:
    `CompileValidatedLet` compiles a let body on the *same* compiler with `p.env` swapped,
    the only such swap in the package (every other body compile builds a child continuation,
    where the map is nil). It collected the top-level verdict — a false positive, which this
    subsystem defines as silent state corruption rather than lost reclamation, and one the
    classifier cannot see to guard. Fixed by reading through `unitFrameReclaimVerdict`, which
    re-tests the `ns.Runtime() == p.env` condition the map was armed under; it can withhold a
    verdict, never grant one. Guarded over all four binder kinds
    (`pkg/wile/framereclaim_letbody_leak_test.go`). **The parent item stands**: this narrows
    *who may read* the verdict, it does not make the domain scope-aware.

- [x] **`MaybeCreateLocalBinding` uses `ScopesCompatible` where exact equality is correct**
  [Medium, S, Done 2026-07-20]: creation reused a slot when
  `ScopesCompatible(existing.Scopes(), newScopes)`
  (`pkg/environment/local_environment_frame.go`), and that predicate returns true whenever
  the *existing* set is empty — so a `{m}`-scoped binder could reuse a `{}`-scoped slot and
  clobber it. This is the same hole the global path deliberately avoided by using exact
  scope-set equality at creation (design D3 of the scope-keyed plan).

  **Fix**: the predicate now calls `scopeSetsEqual` — the global creation predicate *itself*,
  not a copy of it. Same package, already unexported there, so the two creation paths cannot
  drift. Lookup keeps `ScopesCompatible`, which is correct for lookup: a pre-hygiene binding
  with no scopes really is visible to every reference. The bug was using a visibility
  predicate to decide identity.

  **Latency confirmed, not assumed.** The obvious repro — a macro-introduced `(define x 42)`
  spliced into a body that already has a user `(define x 1)` — returns the hygienic `1` on
  `4f73936d`. The masking is not "a fresh frame per binding form" as filed: the user's binder
  in a body *carries the body's scope*, so `len(bindingScopes) != 0` and the short-circuit
  never fires. Reaching it needs a genuinely **empty** scope set sharing a frame with a scoped
  binder; the nil-passing callers (`compile_syntax_case.go:347`,
  `operation_syntax_case.go:219`) use dedicated frames. So the guard is a unit test, not an
  integration test — there is no Scheme program known to reach it.

  Guard: `TestMaybeCreateLocalBinding_EmptyScopedSlotNotReused` (`environment_frame_test.go`),
  RED before / GREEN after, over both `nil` and empty-non-nil existing sets. It also asserts
  the existing binding was not retroactively re-scoped. The sibling
  `TestMaybeCreateLocalBinding_ScopeDistinctKeys` cannot see this hole — it covers `{A}` vs
  `{B}`, where `ScopesCompatible` already returns false. Only one direction was ever broken
  (`{m}`-then-empty already split correctly), so a fix keyed on the *new* binder's scopes
  alone would pass a one-directional test; the test comment records this.

  - [x] **Residual: the `Scopes` backfill in the reuse branch was dead — deleted**
    [Tech debt, XS, Done 2026-07-20]: the reuse branch filled `m.Scopes` in when an existing
    binding had none. Under exact equality that branch needed `scopeSetsEqual(nil, scopes)`
    true *and* `scopes != nil` — i.e. an empty-but-non-nil set, where the write is a semantic
    no-op. It was half the clobber mechanism (the reuse aliased the slot, the backfill
    rewrote its identity in place). Deleted rather than pinned: the dead case is a no-op, so
    there was nothing to pin. `Source` still backfills, and the now-visible asymmetry is
    documented on `MaybeCreateLocalBinding` so it does not get "restored" for symmetry.
    `TestMaybeCreateLocalBinding_EmptyScopedSlotNotReused` keeps its
    not-retroactively-re-scoped assertion as a guard against either half returning.

- [x] **`DeleteBinding` is name-keyed while the namespace read surface is scope-exact**
  [Medium, S, issue #805, Done 2026-07-20 — sealed-base probe left wildcard, see below]:
  `namespace-ref`/`namespace-bound?`/`namespace-bound-names` (and their
  `environment-*` twins) resolve under the ambient (empty) scope set; `namespace-undefine!` still
  resolves by bare name. `GlobalEnvironmentFrame.DeleteBinding`
  (`pkg/environment/global_environment_frame.go:500`) removes **every** slot a name owns, so
  `(namespace-undefine! ns 'x)` destroys a macro-introduced `x` that `(namespace-ref ns 'x)`
  reports as unbound: you can destroy a binding you cannot read. Pre-existing; before the read
  side became scope-exact the whole surface was uniformly coarse, so the diff promoted this from
  coarseness to a hole.

  **Fix**: give `DeleteBinding` a scope-set parameter and resolve through
  `bestSlotLocked(sym, scopes, false)`, the literal call the read makes, so delete cannot drift
  from ref; nil that one slot and prune its index from `keys[sym]`. Leaving dead indices in place
  strands the two consumers that treat `slots[0]` as the name's representative
  (`pkg/wile/engine.go:1293`, `pkg/registry/search.go:210`); both nil-check, so no panic, but
  they would drop a name whose slot 0 died while slot 1 lives. One production caller
  (`pkg/internal/extensions/namespace/prim_namespace.go:216`), so change the signature rather
  than add a variant; the wildcard sealed-base probe at `:219` needs the same treatment.

  **Cost is the tests, not the code.** `TestGlobalFrame_DeleteRemovesEveryHygieneDistinctSlot`
  (`global_environment_frame_test.go:363`) and the `DeleteBinding` doc (`:512-515`) pin
  delete-all deliberately, by name and by comment; both get **inverted**, not updated. Unmeasured
  risk: a *wildcard* stale index pinned to the deleted slot now falls through to a surviving macro
  slot where delete-all made it miss. That is the C2b scope-blindness at
  `global_environment_frame.go:372-384` showing through rather than something the change invents,
  but it cannot be ruled out by reading and needs coverage.

  **As landed.** `DeleteBinding(sym, scopes)` resolves through `bestSlotLocked(*sym, scopes,
  false)`, nils that one slot, prunes its index, and drops the map entry when the name owns no
  more. `matchAny` is hardcoded false, mirroring `AmbientKeys` and `GetGlobalIndexWithScopes`
  — the two read entry points delete must not drift from — so delete has **no wildcard mode**
  and `nil` here means the empty set, diverging from this file's nil-means-MATCH-ANY
  convention. That divergence is deliberate and documented at the function: routing the
  delete-all operation through the nil case would make the `AmbientScopes` footgun fire
  destructively.

  The delete-all test was inverted into
  `TestGlobalFrame_DeleteClearsMultiSlotNameOneScopeSetAtATime` (multi-slot coverage kept, the
  delete-all contract dropped), plus a scope-matched-only case, a macro-only no-op case, and
  `TestNamespaceUndefine_RemovesAmbientAndSparesMacroBinder` at the primitive layer. All four
  were verified discriminating by mutating `matchAny` to true and confirming they go red.

  **Sealed-base probe made scope-exact too** (`prim_namespace.go`, `GetGlobalIndexWithScopes`
  under `AmbientScopes()`). Initially left wildcard on the argument that no scope-carrying
  sealed binding exists, so no test could fail first; reversed under the nil-means-NONE
  convention. A wildcard probe answers a different question than the one asked: "is *some*
  binding of this name sealed" rather than "is the binding I just failed to delete sealed",
  raising `ErrImmutableBinding` for a name the ambient read calls unbound. Same drift class the
  delete side just closed. Note the effect here is a spurious *denial*, not a permission —
  wildcard widens the candidate set, and which way that lands on the caller depends on the
  call. Tightening a wildcard default outweighs "no failing test" when existing coverage
  (`TestNamespaceUndefineSealedRejected`) guards the reachable path.

- [ ] **`namespace-undefine!` does not stop compiled code from reading the binding**
  [Correctness, S, 2026-07-20]: found while measuring #805, **pre-existing on master**, not
  introduced by it. After `(define v 7)` `(define (get-v) v)` `(namespace-undefine! ns 'v)`,
  `namespace-bound?` correctly answers `#f` but `(get-v)` still returns `7`. The closure reads
  through a cached/pinned binding that nil-ing the slot does not invalidate. Not macro-specific
  — plain ambient bindings show it. Cost me a test: the obvious "macro-only binder survives"
  assertion at the primitive layer measures this cache rather than the delete policy, and so
  passes under any policy. **Measure first:** determine whether the stale read comes from the
  global binding cache (`memory/global-binding-cache-already-exists.md`) or from a pinned
  `GlobalIndex` whose re-resolution succeeds; the two want different fixes.

- [ ] **Scope-set resolution lets the zero value answer an unanswerable question**
  [Correctness + API, M, 2026-07-20, follow-up to #805]: convention is **nil means NONE**;
  "All" must be an explicit special value. The environment read surface does the opposite —
  `GetBinding`, `GetLocalIndex` and `GetGlobalIndex` read a nil scope set as MATCH ANY. Nil is
  indistinguishable from an uninitialized value, so a caller that merely forgot to thread its
  scopes silently gets a *wider* resolution, with nothing in the signature to flag it.

  **What the wider resolution actually returns is the defect.** `bestSlotLocked` with
  `matchAny` true does not return a union; it returns `slots[0]`, the first live slot. That is
  arbitrary selection from a wider candidate set, and slot order is an expansion-order
  artifact. `AmbientScopes`' own doc states it exactly: a wildcard "resolves by slot order — an
  expansion-order artifact, not an answer to the caller's question." So the harm is an
  arbitrary binding, not a granted permission.

  **Corrected 2026-07-20 — this entry previously called the behavior "fail-open" and a
  "security posture question."** Both were overstated, and the term was introduced in
  `4f73936d` rather than inherited. Nothing fails: no fault occurs, a legal value is simply
  ambiguous between "unset" and "all". And "open" implies permission, while the effect can land
  either way (the sealed-base probe above widens the match and produces a spurious *denial*).
  No case has been shown crossing the sandbox boundary (`security.Authorizer`, sealed base);
  every measured consequence is a wrong binding. Hygiene is a correctness boundary, not an
  authorization one. The item stands on correctness and API hygiene, and has no claim to
  priority over demonstrated defects.

  **Three symptoms of the same undersized domain**, all in `pkg/environment`:
  - `AmbientScopes()` (`global_environment_frame.go`) returns a non-nil empty slice for no
    reason other than to route around the nil default. Its own doc comment documents the
    footgun. A named workaround is the tell that the default is wrong.
  - `bestSlotLocked(key, scopes, matchAny bool)` — a bool riding alongside the value because
    the value could not carry the state. 17 `matchAny` references across
    `global_environment_frame.go`, `environment_frame.go`, `local_environment_frame.go`.
  - `GlobalIndex.matchAny()` = `Scopes == nil && !scopeKeyed`, plus the `scopeKeyed` field (4
    references) that exists *only* because nil cannot distinguish "matched the empty set" from
    "no key at all".

  **Fix**: a `ScopeSet` type carrying three named states (All / empty / specific) collapses all
  three into one value. This is `state-trace` shaped — bounded state split across a slice and
  two bools, where the distributed comparisons reduce to one scalar.

  **Blast radius is smaller than it looks.** 13 non-test call sites pass nil into the scoped
  reads: 9 in `pkg/machine/compilation`, 2 in `pkg/registry/apply.go`, 1 each in
  `pkg/wile/engine.go` and `pkg/repl/meta.go`. The wildcard-by-name `GetGlobalIndex` now has
  exactly **one** non-test caller left (`machine/compilation/er_macro_rename.go`) — the
  `namespace-undefine!` probe was its other one until #805 moved it to the scoped form.

  **Triage each nil site, don't mass-rewrite.** Some genuinely mean All (introspection,
  `er_macro_rename`); most are probably unthreaded scopes. The whole point is that the current
  encoding makes those two indistinguishable, so each site needs its intent decided and then
  written down — a mechanical `nil` → `AmbientScopes()` sweep would just freeze today's
  accidents into explicit form. Expect some to be latent hygiene bugs.

  Supersedes the nil-encoding half of the older "use nil to mean unconstrained" preference,
  which is plausibly where this design came from.

- [x] **`freeIds` collapses scope-verified answers into a name-keyed map**
  [Correctness, S, Done 2026-07-20]: `collectFreeIdentifiersWithEllipsis` resolved each
  template identifier under its own scope set, then stored the result in a
  `map[string]*FreeIdResolution` keyed on `symVal.Key` and overwritten unconditionally
  (`compile_syntax_rules.go`; threaded on through `operation_syntax_rules_transform.go` and
  `operation_syntax_case.go`). Two same-named template identifiers carrying different scope
  sets resolved individually and correctly, then the second silently discarded the first —
  the exact shape C1/C2 fixed on the *lookup* side, surviving one layer out in *storage*.

  **Reachable from surface Scheme, not latent.** A macro-generating macro whose generated
  template holds one literal `mh` (its own intro scope) beside a pattern-var substituted with
  a user identifier also named `mh` gave `(99 99)` where hygiene demands `(1 99)`; a
  distinct-name control gave `(1 99)`, isolating the collapse. Expands at top level, so it
  dodges the internal-define-syntax visibility limitation. Guarded by
  `TestMacroGeneratingMacro_SameNameFreeIdsDoNotCollapse` (`pkg/wile`).

  **Fix**: a scope-discriminated key end to end — `match.FreeIdKey(name, scopes) =
  syntax.ScopeFingerprint(scopes) + "|" + name` — at the one writer (collector) and one
  reader (`applyHygieneToSymbol`); the two verbatim-copy transfer sites needed no change
  (key is still a string). `ScopeFingerprint` is the consolidation of validate's private
  `scopeFingerprint` (impl now in `values` with `Scope`, wrapper in `syntax`, validate
  delegates). Exact-scope-set keying, not Flatt-maximal: this replays a per-occurrence
  pre-resolved answer keyed by the same immutable template symbol at both ends, verified
  (`Scopes()` reads the same `SourceContext.Scopes` field at storage and consumption).
  `FreeIdKey` contract (incl. the `|`-delimiter unambiguity) pinned by
  `TestFreeIdKey_DiscriminatesScopeAndName`.

- [ ] **`BindingID` must carry a scope discriminator before the load-order plan ships**
  [Correctness + sequencing, M, 2026-07-19]: Part III of
  `plans/2026-07-18-load-order-independent-binding-resolution-design.md` defines
  `BindingID{Origin, Phase, Sym, Local}` with no scope component. That was sound only while
  the (now-deleted, `a60e32e1`) rename pass guaranteed one global per
  `(frame, phase, symbol)`. **After Stage B it is not**: two hygiene-distinct bindings
  produce equal `BindingID`s. Since that struct exists specifically to replace three
  disagreeing notions of "same binding" with one, shipping it name-keyed yields a fourth
  wrong answer instead of a fix. Not a defect in today's tree — `BindingID` is unbuilt — so
  this is a **prerequisite recorded against the successor plan**, not a bug. The scope-keyed
  arc was sequenced first precisely so `BindingID` gets defined once, correctly, after it.
  Related: the shipped `BindingID{*LocalEnvironmentFrame, slot}` is a physical local slot and
  is **not** the vehicle for this (see the frame-reclaim entry above).

### Scope-keyed globals — successor work not built in the arc (2026-07-19)

Surfaced by the Stage C adversarial review and deliberately left out of scope so the arc
could land. Recorded here 2026-07-20; previously these lived only in
`plans/2026-07-18-scope-keyed-global-bindings-design.md` under "Successor work (do not build
here)" and "Found by the Stage C adversarial review", which is not where open work is looked
for. **Two of the three defects below are marked in the plan as NOT verified first-hand —
re-confirm each repro before designing a fix.**

- [ ] **Two shorthand-`define` defects adjacent to the C1-local fix** [Correctness, S,
  2026-07-19]: a silent `#!void`, and a hard `no such local or global binding` for a
  macro-generating macro defined by an internal `define-syntax`. Both survived `2a0b2941`
  unchanged. The second is REAL and reproduces in the shorthand `(define (f) …)` body form;
  it does **not** reproduce under `(let () …)` or `(lambda () …)`, which is why an earlier
  review pass wrongly concluded it was not a defect — check the shorthand form specifically.
  The plan's instruction was "file separately; do not bundle," and no issue was ever opened.

- [ ] **Export phase-probe ORDER is a second wrong-binding axis, not closed by C3**
  [Correctness, M, 2026-07-19, **repro unverified**]: `findLibraryBinding` probes runtime,
  then expand, then compile, returning on first hit, while `define-syntax` stores into Expand
  and the import path *also* mirrors imported syntax bindings into the library's RUNTIME frame
  (`library_bindings.go`, the `copyLibraryBindingsDirect` syntax branch). A library that
  imports a macro and then defines its own of that name exports the **imported** one.
  Reported repro: library `(mc)` imports `(mb)` (exporting macro `twice`) then defines its own
  `twice`; `(import (mc)) (twice 5)` yields mb's transformer. Scope keying does not catch it —
  the runtime copy sits at `{}`, a subset of `{libScope}`, so a scoped probe still matches and
  probe 1 still short-circuits. Fix is either best-across-all-three-probes instead of
  first-hit, or not mirroring syntax bindings into the library runtime frame.

- [ ] **No sealed base above phase 0** [Correctness, M, 2026-07-19, **repro unverified**]: a
  library's expand-phase install overwrites a bootstrap macro's binding in place (reported:
  `guard-aux`, same `*Binding` pointer, `(guard …)` compromised). Both binders carry `{}`, so
  scope keying is structurally blind to it — this is not something the arc could have fixed.
  Options the codebase already names (`bootstrap_nilpin_test.go`): carve an immutable expand
  base, or move bootstrap helpers out of user reach.

- [ ] **CHANGELOG says nothing about scope-keyed global storage** [Docs, S, 2026-07-19]:
  16 commits from `8afeb66a` (Stage A, vacuous) to `4f73936d`, 14 of them fixes, and
  `grep -c "scope-key" CHANGELOG.md` returns **0**. Several change user-visible R7RS
  semantics — a macro-generating macro expanded twice now gets two binders instead of
  sharing one (`c9b6b90c`), template-introduced library exports are now rejected eagerly
  (C3), and `namespace-undefine!` now deletes one scope-matched slot instead of every slot a
  name owns (`4f73936d`). The deferral was deliberate and correct at the time: an entry
  written before C2 landed would have described half a change. C2 landed 2026-07-19, so this
  is now unblocked and is the cheapest open item in this tier. `docs/` was already brought
  current (Invariant 5 in `docs/environment/system.md`); the CHANGELOG is the remaining gap.

### ~~Opaque-subtree over-marking may loosen the immutable-top-level check (crosscheck `15b68433..HEAD`, 2026-07-14)~~ RESOLVED

- [x] **The acceptance was real; the diagnosis was not. Fixed at the cause.** (2026-07-16, `fix/opaque-subtree-quasi-depth`, `57973333` + `3cce6754`)

  **(1) Confirmed empirically** — the gate. Cross-unit, under the default immutable top level: `(define x 1)` then `(set! x 2)` is rejected, but ``(begin (define x 1) `(x))`` then `(set! x 2)` **compiled**. Pinned by `TestImmutableTopLevel_OpaqueSubtreeOverMark` (`pkg/wile`). The item's own repro ``(begin `(x) (set! x 1))`` demonstrates nothing: it has no `define` to stamp, and the in-unit `set!` marks `x` by itself, so the quasiquote contributes nothing. The effect only isolates across compilation units.

  **(2) NO separate mutation set — the premise is false.** The item called this "two consumers wanting opposite error directions from the same data." They want the *same* direction. Both enforcement sites key on the **same `Stable` flag the optimizer anchors on**: the `set!` rejection (`compile_validated.go`, `binding.IsStable()`) and the redefine guard (`compile_define.go`, `Meta().Stable`). The `set!` rejection exists *to protect* the frame-reclaim anchor. So an over-mark withdraws the optimization and both enforcements **together**; no consumer treats a binding as immutable without reading `Stable`, so nothing is left holding an assumption the admitted `set!` could falsify. There was no soundness hole, and a second map would have bought nothing.

  **(3) Comment corrected, then obsoleted by the fix.** "An over-mark costs an optimization" undersold it: it costs the `Stable` stamp, and top-level immutability rides on that stamp — so it silently turned immutability off for any name a template mentioned. Recorded in `57973333`, then fixed at the cause in `3cce6754`.

  **The defect was imprecision, not direction.** `forEachRawSymbol` marked template *data* no unquote can reach. It now threads quasiquote depth and marks only evaluated positions, matching the compiler's own walk (`quasiquoteNeedsRuntime`/`expandQuasi`) and the shipped `renamePair` prior art. Agreement with the compiler is the soundness argument: that walk decides what is live, this one only predicts it. `markOpaqueSubtree` split into `markOpaqueCode` (depth 0, ordinary code) and `markOpaqueTemplate` (depth 1, already stepped into) so the callers' real difference is API, not convention.

  Three shapes decide correctness, all pinned: dotted unquote (``` `(a . ,x) ``` parses as `(a unquote x)`, a bare unquote in the **spine** the keyword dispatch cannot see); `quote` as a barrier at depth 0 **only** (its nested unquotes stay live inside a template, R7RS §4.2.6 — the one mistake that fails silently); and nested depth (``` `(a `(b ,(set! x 9))) ``` lands at depth 1, still data). The miscompile the file exists to prevent is re-verified end to end: ``(let ((f (lambda () 7))) `(,(set! f (lambda () 99))) (f))`` still returns 99. Gates: lint 0 issues, covercheck 41/41, full suite green.
### ~~`Value` Go-comparability is an unenforced invariant, already violated in-tree (2026-07-14)~~ RESOLVED

- [x] **Decided: Option 1 — require comparability. Documented and enforced.** (2026-07-14, `fix/value-comparability-contract`)

  The requirement is now stated on the `Value` doc comment (`pkg/values/values.go`), including the rule that actually decides it: **the RECEIVER, not the underlying type**. `Vector` is `[]Value` and is safe because its methods take pointer receivers.

  Enforced by `reflect.TypeOf(v).Comparable()` over rosters, since Go comparability has no method set and cannot be asserted at compile time:
  - `TestValue_ImplementorsAreGoComparable` (`pkg/values`, over `allValueExemplars`)
  - `TestDeepEqualer_ImplementorsAreGoComparable` (hashing is a stricter demand than `==`)
  - `TestMachineValues_AreGoComparable` (`pkg/machine`, new roster)
  - `TestMachineValueExemplars_CoverPackage` (pins that `Operations`/`MultipleValues` do not silently re-acquire `Value`)

  **THREE violators, not two.** The audit above listed `Operations` and `MultipleValues`. It missed `machine.boxedValuesType` — a `struct{[]values.Value}` with value receivers — and that was the only one of the three *reachable from Scheme*: `OperationBoxValues` puts it in the value register, so `dynamic-wind` gets there. Found by `reflect`, not by eye, which is the argument for the roster tests.
  - `Operations` and `MultipleValues` — **no longer `values.Value`**. Neither is a Scheme datum; both took the conformance for container convenience. Production code built untouched, only tests referenced the interface methods. They keep a concrete `EqualTo(T)`: having an equality method is not the same as being a `Value`.
  - `boxedValues` — genuinely IS a `Value`, so it became **pointer-shaped**.

  **A live host-crash bug was fixed on the way.** `equalWorklist.step` compared `a == b` BEFORE establishing both sides were `DeepEqualer`s, so two same-typed non-comparable leaves meeting as components panicked. The regression test merged earlier that day only paired a leaf against a `*Pair` — differing dynamic types, which Go answers `false` for without faulting — so the hazard sat unmeasured in the one shape that actually faults. `step()` now reaches `==` and the visited-set key only once both operands are `DeepEqualer`s. Guarded by `TestEqual_SameTypedNonComparableLeavesDoNotPanic`.

  **A `SchemeComparable` interface was considered and REJECTED.** It is the intuitive fix and it is the wrong one: it would give `Operations` and `MultipleValues` a *supported* way to be non-comparable `Value`s, ratifying the free ride instead of ending it. Also, identity may not be delegated to a method — R7RS §6.1 defines `eq?`/`eqv?` on aggregates as "denote the same location in the store", and `eq?` is the FINEST equivalence in the lattice: a type that computes its own identity can lie about it, and `eq? ⊆ eqv? ⊆ equal?` stops being structurally guaranteed. The contract, and why it has no compile-time expression, is on `values.Value`'s doc comment; it is enforced module-wide by `TestValue_AllImplementorsAreGoComparable`.

### ~~`eqv?`/`equal?` numeric-lattice nonconformances (F1/F2/F3, from 2026-07-13 review `numeric-tower.md` + Chez probe)~~ RESOLVED

- [x] **One owner for numeric equivalence; contagion lattice and NaN reflexivity fixed.** (2026-07-14, `fix/value-comparability-contract`, `c302b702`)

  `EqvNumber` (`pkg/values/eqv.go`) is now the single authority the three F6 sites consume: two exact numbers compare across representations, so `(eqv? (+ 1/2 1/2) 1) ⇒ #t` — closing the `numeric-tower.md` rational SPEC finding (`rational.go:168`), which the tower also fixes upstream by canonicalizing denom-1 results to `*Integer`. Exactness contagion corrected (`Float ⊕ exact → Float`, F2); NaN is now reflexive (`(eqv? +nan.0 +nan.0) ⇒ #t`, matching Chez, F3), deliberately finer than the literal pool's `literalIdentical`. F4/F5 (records, hashtables/boxes) documented as conformant divergences. Shipped as the same branch as the Value-comparability contract above. Design + per-phase commit map: `memory/2026-07-14-equivalence-predicate-divergence.md` (archived).

### ~~`define-values` broken under NoMutation; no macro could introduce a top-level temporary; general form-removal `*PrimitiveExpander` leak (from 2026-07-13 review `dialects.md`)~~ RESOLVED

- [x] **Macro-introduced top-level binders are now hygienically renamed; `define-values` survives NoMutation.** (2026-07-14, `fix/toplevel-macro-binder-hygiene`, `d594beeb`)

  > **Superseded 2026-07-18 (`a60e32e1`).** The rename pass described below no longer
  > exists. Macro-introduced top-level binders are now separated by **scope-keyed global
  > storage** (a name owns one slot per binder scope set) rather than by rewriting the
  > binder to a crypto-random name. The outcome is unchanged and `TestNoMutationKeepsDefineValues`
  > still guards it; only the mechanism changed. See
  > `plans/2026-07-18-scope-keyed-global-bindings-design.md`.

  A new expander pass (`pkg/machine/compilation/toplevel_binder_hygiene.go`) gives each macro-introduced top-level `(define …)` binder a fresh crypto-random name and rewrites the references that resolve to it — quote/quasiquote-aware, since a quoted or quasiquote-literal symbol is data, not an identifier (R7RS §4.3.2 / §4.1.2). `define-values` was rewritten `set!`-free with a template temporary, so it now works under the immutable top level, across compilation units, and under the NoMutation dialect (a definition, not a mutation — R7RS §5.3.3). `TestNoMutationKeepsDefineValues` is now an enforced guard, not a gated RED test. A subsequent quote-in-quasiquote corruption (`` `(quote ,tmp) `` rewriting the temp) was fixed with a quasi-depth barrier (`ba283e86`).

- [x] **RESOLVED — the general form-removal leak.** (2026-07-14, `fix/form-removal-expander-leak`)

  A **user** macro whose template references a *removed* form leaked that form's expand-phase `*PrimitiveExpander` into runtime — `(define-syntax my-set (syntax-rules () ((_ v e) (set! v e)))) (let ((x 1)) (my-set x 2) x)` on a NoMutation engine applied `#<primitive-expander:set!>` instead of failing with `ErrNoSuchBinding`. Root cause: `fr.Remove` drops only the compiler `FormSpec`; the expand-phase `PrimitiveExpander` survives and gets pinned onto the introduced identifier, which `tryResolvedBinding` then materialized as a runtime load. Fixed via the review's second direction (`tryResolvedBinding` refuses a compile-time-only handler): a `compileTimeHandler` marker on `namedHandlerBase` (satisfied by both `PrimitiveExpander` and `SyntaxCompiler`) lets `tryResolvedBinding` (`compile_time_continuation.go`) fall through to `ErrNoSuchBinding` — the documented removed-form contract — for *any* removed form carrying an expander, not just `set!`. Guard `TestNoMutationRemovedFormInMacroTemplateIsUnbound`. The cosmetic residue (the removed form's compile-time `BindingSpec` still listed in `Registry().Bindings()`) is unchanged — it is `dialects.md`'s separate open question, not this correctness leak.

### ~~`(scheme base)`/`(scheme eval)`/`(scheme cxr)` export supersets + non-R7RS `(description)` declaration (from 2026-07-13 review `sld-libraries.md`)~~ RESOLVED

- [x] **Documented as deliberate deviations, not deleted.** (2026-07-14, `docs/r7rs-library-export-supersets`, `cc3c48bb`)

  Deleting exports is a user-visible API break, so the supersets were recorded rather than removed: two new sections in `docs/reference/r7rs-differences.md` (*Standard-Library Export Supersets*, *`(description <string>)` Library Declaration*), pinned by `TestLibraryExportSupersets` — it imports each binding through `(only (scheme …) id)`, so narrowing any library back to the strict R7RS surface fails the test and forces a deliberate doc update rather than a silent API break.

### ~~`GlobalIndex` literal identity must include `Env` (from 2026-07-13 review `codegen.md`, §2c)~~ RESOLVED

- [x] **`GlobalIndex.EqualTo` now compares `Env`, not just `Index`.** (2026-07-14, `fix/globalindex-env-literal-identity`, `fa9804d6`, merged to master)

  A literal-pool collision: two distinct globals with the same `Index` symbol but different `Env` deduped to one literal slot. `EqualTo` (`pkg/environment/global_environment_frame.go`) now includes `Env`. Phase 1 shipped + merged; Phase 2 deferred by design. `memory/2026-07-13-globalindex-env-literal-identity.md` (archived).

### Continuation multiple-values follow-ups (from PR #800 crosscheck, 2026-06-25)

Deferred items surfaced while shipping the multi-value continuation re-invocation
fix (PR #800). The continuation value-count behavior itself is documented in
`docs/reference/r7rs-differences.md` → "Continuation Value-Count".

- ~~**`dynamic-wind` does not preserve multiple values from its thunk**~~ FIXED
  on `fix/dynamic-wind-multiple-values`: box/unbox the thunk result so 0/1/N
  values occupy exactly one eval-stack slot (`OperationBoxValues` /
  `OperationUnboxValues` in `CompileValidatedDynamicWind`). A call/cc
  *multiple-value* escape observed through `call-with-values` is still blocked by
  the paused sub-context truncation bug, not by dynamic-wind.
- ~~**`procedure-arity` reports continuations as `1`**~~ FIXED on
  `fix/procedure-arity-continuations`: both `*ComposableContinuation` and (newly
  handled) `*CapturedContinuation` now report `(0 . #f)` (variadic-from-0,
  matching their `AcceptsArity` and Racket's arity-at-least-0); docstring updated.
- **(Investigated 2026-06-25 — NOT pursued)** single-value resumption contexts
  splice multiple values instead of raising an arity error. Design pass concluded
  it is not worth it: strictness needs a value-count check on the
  `RestoreContinuation` hot path + a compile-time single/any classification, and
  breaks `(wile control)` variadic resumption and pervasive normal-return splices
  — all to enforce behavior R7RS leaves unspecified (current splice already
  conforms, documented in r7rs-differences). If ever needed, do it as an opt-in
  `WithStrictValueArity` engine option, not a default change. Full rationale:
  `plans/2026-06-25-continuation-arity-strictness-design.local.md`.

### Trampoline continuation invocation to bound Go-stack growth (2026-06-26) — SHIPPED 2026-06-28

**STATUS: SHIPPED** on `feat/continuation-resume-trampoline`. The unified reification +
winding-aware resume ("the flip") landed: `applyCapturedContinuation` emits
`ErrResumeContinuation`, and the resume runs on the driver (`RunResumable` /
`ReinstallSegment`) instead of nesting a fresh `Run()` — O(1) Go frames. Consequently
the `maxContinuationDepth` bound and its `threadPools.contNestDepth` tracking were
RETIRED (the resource they guarded no longer exists), and the `-race` ctak skip plus
`pkg/wile/raceflag_*_test.go` were removed — `TestDeepConvergingContinuationConverges`
(ctak 18/12/6) now runs under `-race`. A post-landing A/B `/crosscheck` then found and
fixed one escalation regression (sticky context-global `isolatedMarks` swallowed R7RS
§6.11 secondary exceptions after any resume; fixed path-precisely via a
`resumeGeneration` counter — `pkg/registry/core/continuation_noncontinuable_after_resume_test.go`).
The chronological design/falsification log below is retained as history; the
"current mitigation" and "until the trampoline lands" passages in it are SUPERSEDED.

`applyCapturedContinuation` (`machine/captured_continuation.go`) invokes a captured
continuation by running the resumed computation in a *nested* sub-context
(`sub.Run()` then abort to the prompt) rather than tail-replacing the current
continuation. So a continuation-heavy program accumulates LIVE Go-goroutine-stack
frames across its dynamic extent — `ctak`, which a proper Scheme runs in bounded
space, peaks ~40k live frames for a single `(ctak 18 12 6)` and ~525k for the
Gabriel benchmark's warmup + 10-iteration loop, approaching Go's ~675k fatal
stack-overflow point.

The interim mitigation (**RETIRED** when the trampoline landed — see STATUS above) was
a dedicated, live-nesting bound `maxContinuationDepth` (default
`DefaultMaxContinuationDepth = 600000`, tracked as `threadPools.contNestDepth`,
decremented on unwind) that surfaced a runaway `call/cc` loop as a catchable
`ErrCallDepthExceeded` before the Go fatal overflow. **The margin was necessarily thin**
(benchmark ~525k vs overflow ~675k):
a sufficiently long continuation-heavy program can still approach the Go stack
limit, and on a platform whose overflow point sits below the bound a true runaway
could fatally crash before the catchable bound trips.

- **Proper fix [Performance/Correctness, L]:** trampoline continuation invocation so
  a resume does not nest a fresh `Run()` Go frame — replace the current continuation
  in place instead of running the future nested-then-aborting. This bounds Go-stack
  growth to the genuine live continuation depth and lets the depth bound be set with
  comfortable margin (or retired). High-risk VM/continuation work — see the
  memory's `subcontext-continuation-truncation-redesign` and tail-frame-recycling
  cautions before attempting; gate on the full continuation/`-race` suite.
- **ATTEMPTED & FALSIFIED (2026-06-27):** a *resume-side-only* segment-reinstall
  trampoline (`ErrResumeContinuation` signal → `ReinstallSegment` → resume-aware
  `sub.Run()` drivers `DriveResume`/`RunResumable`) was built and reverted. It DID
  fix the `-race` ctak overflow and the dynamic-wind after-thunk double-fire, but
  **reinstall-at-nearest is unsound**: an outer continuation invoked to *escape past*
  a `call-with-values` producer double-executes the consumer (base: `n2 done`; the
  flip: `n2 done CONSUMER`). The whole test suite was BLIND to this class — `make ci`
  was green; a multi-agent crosscheck caught it. A correct flip needs the Go-frame
  prompt catches (`call-with-exit`, `call-with-continuation-prompt`, `RaiseInPlace`)
  reified as continuation-chain frames (archaeology §5) — i.e. it is COUPLED to the
  sub-context truncation open problem, not separable. The behavior-preserving
  groundwork (`ReinstallSegment`, `RunResumable` extraction, the inert
  `ErrResumeContinuation` type) is kept. Recoverable at tag
  `attempt-resume-aware-catches-falsified`; full design + kill-conditions + crosscheck
  in `memory/2026-06-27-resume-aware-prompt-catches-design.local.md` (falsified/reverted; moved to memory/ 2026-06-29).
- **ORACLE + SEPARATELY-FALSIFIED DESIGN LANDED (2026-06-27, re-take):** the prior
  revert happened because the suite was **blind to escape-past**. That gap is now
  closed by a committed non-blind oracle,
  `pkg/registry/core/continuation_escape_past_oracle_test.go`: escape-past guards for
  every boundary (call-with-values / prompt / exit / apply), nested-guard,
  normal-completion, consumer-captured, escape-past multi-shot+mutation (all GREEN on
  master), plus the two open bugs (truncation, dynamic-wind double-fire) each with an
  always-on "documents-current-bug" tripwire and a `WILE_RUN_RED_CONTINUATION`-gated
  target acceptance cell. **Proven non-blind:** run against the falsified flip (tag
  `attempt-resume-aware-catches-falsified`) the call-with-values row returns
  `CONSUMER-WRONGLY-RAN` and the oracle FAILS — it catches the exact regression that
  shipped `make ci`-green. The coupled-fix design (adversarially reviewed + corrected)
  is `memory/2026-06-27-continuation-resume-trampoline-coupled-fix-design.local.md`. It
  PROVES no resume-side-only fix can exist (the resume needs a chain-resident boundary
  to place itself; reinstall-at-nearest breaks escape-past, abort-to-top breaks guard)
  and specifies the coupled fix: a new `RunBodyUnderFrame` VM primitive (the missing
  normal-completion frame-execution mechanism — build+spike FIRST), reify the four
  boundaries as chain frames (migrating `call-with-exit`'s Go-local tag to
  chain-carried), then flip `applyCapturedContinuation` to bounce through the single
  `RunResumable` driver. Verified corrections vs older docs: the exception handler is
  ALREADY marks-based (`exceptionHandlerParam`), and the `call-with-values` consumer is
  ALREADY inline — only the producer is a sub-context. **Gate: RED — implementation
  NOT started; needs human go + the §6 kill-conditions (incl. KC-9..11 to be added
  before the flip).**
- **SPIKE EXECUTED — mechanism PROVEN, change is ATOMIC (2026-06-27):** built
  `RunBodyUnderPrompt` (`pkg/machine/run_body_under_frame.go`) — a transparent
  chain prompt frame (`returnTemplate` = one `OpRestoreContinuation`) with the body
  applied INLINE. Wiring it into `call-with-exit` made the oracle's escape-past rows
  + a SINGLE guard GREEN. But it tripped the design's own STOP condition: reifying ONE
  boundary while the others stay sub-contexts regresses NESTED guard + call-with-exit-
  in-barrier — the inner construct runs inside the OUTER guard's `call-with-values`
  PRODUCER sub-context, so its prompt frame is off the main chain and the driver's
  `FindPrompt` can't reach it. The reification is **ATOMIC** across {call-with-exit,
  call-with-continuation-prompt, call-with-values producer, RaiseInPlace handler,
  with-continuation-barrier} + the flip — not incremental. `call-with-exit` was
  REVERTED to its sub-context form (tree non-regressing); `RunBodyUnderPrompt` is kept
  as proven groundwork. **De-risked the hardest piece:** call-with-values' consumer-
  apply needs NO hot-path change — a normal frame whose template is
  `[OpPush, OpLoadLocal(consumer), OpApply]` (capturable via `Copy`), and RaiseInPlace
  is a marks-carrying transparent frame. Full findings + revised atomic execution
  order: design doc § "Spike outcome".
- **cwv REIFICATION PROVEN + COUPLING MAPPED, reverted to clean (2026-06-28):** built
  `RunBodyUnderFrame` + `RunBodyUnderConsumer` (kept as proven groundwork in
  `run_body_under_frame.go`; consumer template `[OpPush, OpLoadLiteral 0, OpApply]`,
  verified for 0/1/N values; lint-clean unwired). Wiring it into `PrimCallWithValues`
  turned the oracle truncation target GREEN and kept escape-past/nested-guard/multishot/
  machine all GREEN — **the consumer-frame mechanism works.** But it does NOT stand
  alone, so it was REVERTED: (1) the real `guard` wraps cwv inside `call-with-exit`
  (bootstrap_macros.scm:175), so reifying only cwv MOVES guard's truncation out to the
  call-with-exit boundary (`TestGuardCoupling_C2_RealGuard`: `"2"` → leaked
  `#<machine-closure>`) — empirically CONFIRMS the atomic coupling for guard; (2) it
  breaks the frame-reclaim `InvokesProcedure` analyzer (`TestInvokesProcedureStaticGuard`)
  — the `ApplyCallable` moved behind machine helpers it can't see (must learn
  `RunBodyUnder*` as sinks). call-with-exit reification blockers: one-shot `valid`
  invalidation has no home in the transparent-frame model (primitive returns before proc
  runs); must drop exitFn's `UnwindTo` (driver double-fires otherwise). **Risk #1 (the
  blind spot):** reifying tag-bearing boundaries under nest-then-abort is BLIND-unsound —
  a `k` captured inside a reified prompt/exit, re-invoked, re-aborting to the tag escapes
  the plain `sub.Run()` in `applyCapturedContinuation` → "no prompt found". Commit a
  replay-reabort oracle cell BEFORE wiring exit/prompt; green oracle ≠ correct for them.
  Full map + corrected coupled plan: design doc § 8c; memory
  `continuation-cwv-reification-validated-coupling-mapped`.
- **FULL CLUSTER (Phase 1) IMPLEMENTED then A/B-FALSIFIED, reverted to clean (2026-06-28):**
  reified all 6 boundaries (cwv/exit/prompt as chain frames; barrier/RaiseInPlace/composable +
  ~13 sweep sites routed via a new `RunWithinBoundary` driver) + the call/cc-delimiting fix
  (PrimCallCC slices at `FindPrompt(DefaultPromptTag)`, not nil). Genuine FIXES confirmed by
  A/B crosscheck (truncation, marks-survive-exit, delimiting). **But the crosscheck FALSIFIED
  Phase-1-alone: 4 CRITICALs + `make ci` RED** (control-test.scm + exceptions-test.scm go red;
  `go test ./...` does NOT run them — MUST run `make ci`). C1 boundary-after-resume crash/silent
  `#<machine-closure>`; C2 boundary-in-after-thunk forward-escape crash; C3 after-thunk silently
  skipped through a deeper sub (leak); C4 handler-runs-boundary-then-reraise HANG. **DECISIVE:
  reification ⟺ winding-aware resume (the flip) are ONE atomic change** — 4 paths still run
  boundary code under plain Run/non-reconciling re-raise, and routing them all overflows ctak
  under nest-then-abort. The 4 CRITICALs are the spec for the unified change. Full detail: plan
  `memory/2026-06-28-continuation-cluster-reification-impl.md` § 7 (OUTCOME); memory
  `continuation-cwv-reification-validated-coupling-mapped` (ATTEMPT 2026-06-28c). Helpers kept
  as proven substrate in `run_body_under_frame.go`.
- **CI mitigation (2026-06-27 — RETIRED when the trampoline landed; the skip + `raceflag_*_test.go` were removed):** the `-race` detector
  inflates per-Go-frame cost several-fold, so `TestDeepConvergingContinuationConverges`
  (ctak(18,12,6), ~40k live re-invocation frames) overflowed the 1 GB goroutine stack
  under `-race` — a *fatal* abort below the `maxContinuationDepth` bound, which turned
  the whole `race` CI job red from PR #794 (2026-06-25) onward. ctak is single-threaded,
  so `-race` adds no race-detection value for it; the test now skips under `-race`
  (`raceEnabled` flag, `pkg/wile/raceflag_*_test.go`) and still runs in the non-race
  job, which validates the depth-bound + convergence semantics. This is a
  test/environment mitigation, **not** the fix — the O(depth) Go-stack continuation
  model above is the root cause, removed only by the trampoline.

### Restricted-profile `(scheme base)` export-validation (from PR #795–#799 crosscheck, 2026-06-25) — RESOLVED by-design 2026-06-29

**Maintainer decision (2026-06-29): NOT a supported combination. Keep the strict eager
validation; resolved by-design (won't-fix), documented as an embedding contract. #801 closed.**
Rationale: (1) strict §5.6 enforcement is the *conformant* behavior — a `(scheme base)` that
cannot define its I/O exports under Tiny is genuinely invalid in that configuration; (2) the error
is the **security/capability boundary** asserting itself — `Tiny` is a capability/sandbox choice
(which primitives are *exposed*), **orthogonal** to the language-standard/dialect axis; subset-
importing R7RS's `(scheme base)` under a sandbox is not the mechanism for "I want a smaller
standard" (that is the Dialect System — see the follow-on note below); (3) the "yes" path has
near-zero value (the names that *do* resolve under such a subset, `car`/`cons`, are core primitives
already bound under Tiny with no import at all); (4) both "yes" fixes are disproportionate-cost —
tolerating profile-gated primitives inverts the `machine/compilation`→`registry/` layering (see
`compile_library_forms.go:307-310`), and deny-stubs pollute every namespace; (5) the shipped
diagnostic (`43d7d085`) already names both causes, so the failure is actionable (pick a profile
that provides the primitives, or import only what the profile has). Contract documented in
`docs/embedding/source-loading.md`.

**Follow-on (distinct axis — non-R7RS standard at startup):** the legitimate "configure as R5RS
(or R6RS) on non-default startup" need is a *language-standard/dialect* selector, NOT a security
profile. Today `(scheme r5rs)` only layers R5RS names over the full R7RS core (it `(import (scheme
base) …)`), so there is no non-R7RS baseline; a first-class `WithDialect(r7rs.Dialect)`/`r6rs.Dialect`
startup point is designed but **unstarted** — tracked under the Dialect System in
`plans/ARCHITECTURE.local.md` (all 4 phases incomplete). That is the home for this feature, separate
from #801.

Surfaced by the post-merge crosscheck of the #795–#799 conformance arc.
`validateLibraryExports` (added in #799) runs eagerly and requires every export
to resolve, so importing even a subset of `(scheme base)` via `(only …)` now
fails under the `Tiny`/default profile (its ~64 I/O+numeric primitives are
unregistered). Verified base-vs-HEAD differential: regression for Tiny/default
subset import only; Console and full-base import were already failing; no shipped
consumer affected (CLI/MCP use `KitchenSink`). Arguably *more* R7RS §5.6-conformant
(a library that can't define its exports is invalid). Diagnostic improved to name
both causes (merge `43d7d085`).

- **Maintainer decision (#801) — DECIDED 2026-06-29: NO, not supported.** The current
  stricter behavior + improved diagnostic IS the resolution (the "no" branch). A profile
  that does not register base's primitives makes `(scheme base)` an invalid library in that
  configuration, including under `(only …)`; this is correct §5.6 enforcement, not a defect.
  The "yes" branch (tolerate profile-gated primitives / deny-stubs) is declined — vacuous use
  case, layering-inverting cost. Full differential in
  [#801](https://github.com/aalpar/wile/issues/801) (closed by-design).
- **Note (strict-namespace mode):** `WithStrictNamespace()` lets an embedder run a
  bare top level over a profile whose extensions *are* registered (e.g.
  `WithProfile(Small) + WithStrictNamespace()`), so `(import (scheme r5rs))` layers
  cleanly on a bare baseline. This sidesteps the #801 subset-import friction for that
  use case (the primitives exist in the importable registry); it does **not** resolve
  #801 itself, which concerns genuinely-`Tiny` profiles where the primitives are
  registered nowhere.

### Layered-environment carve regressions (review `d8911c15..HEAD`, 2026-06-15)

Findings from the `/code-review` of the sealed-base carve + immutable-top-level-default
arc on `feat/layered-environment`. Empirically verified against a fresh build unless
marked otherwise. Two root patterns: (1) own-frame `Keys()` iterators not extended to
span the sealed base; (2) the immutable default reaching entry points/scopes the design
intended to stay mutable. **Fix before this branch merges.**

**Scope decision (2026-06-15, user):** immutability is scoped to **compilation units only**
(Chez model) — immutable for files / `-e` batches (preserves the frame-reclaim GC win);
mutable for all interactive/eval contexts (REPL, `--mcp`, `(environment …)`,
`scheme-report-environment`). Implemented via `ImmutableTopLevel()` being a root-namespace
property (child namespaces always mutable) + the `set!`-gate keying on `IsStable()`
directly (anchors stay protected in mutable children) + the CLI/MCP engines opting into
mutable for interactive sessions. 10/15 items done; A4/B4/D1/D2/E1/E2 are follow-ups below.

**A. Read-path regressions (own-frame `Keys()`, no parent/sealed-base walk) — UNAMBIGUOUS**

- [x] **A1 — REPL tab-completion drops every sealed-base name** [High, S, Done 2026-06-15 — `collectBindingNames` now also walks `topLevel.SealedBase()`]: `repl/completer.go:113` `collectBindingNames` walks `phases.Get(phase).GlobalEnvironment().Keys()` (own-frame only). Post-carve primitives + bootstrap procedures live in the sealed base (not a phase entry), so `caar`, `map`, `zero?`, `not`, all 28 cxr accessors, `call/cc` vanish from completion; only dual-registered expand-phase prims survive. Fix: also walk the sealed base (mirror `Namespace.BoundSymbolNames`, which was already patched). **Verified: `caar`/`map`/`zero?`/`call-cc` absent.**
- [x] **A2 — weak completer test masks A1** [Low, XS, Done 2026-06-15 — asserts `caar`/`cadr` (sealed-base-only) present]: `repl/completer_test.go:122` asserts only `car` present (survives via the expand phase). Strengthen to assert a sealed-base-only name (`caar`/`map`).
- [x] **A3 — `,apropos` env-binding search drops sealed-base names** [Medium, S, Done 2026-06-15 — `searchEnvironmentBindings` collects the sealed base via a shared closure]: `registry/search.go:210` `searchEnvironmentBindings` has the same own-frame-`Keys()` walk. Largely masked by the registry `DocPrimitives` index, but binding-level docs on sealed entries absent from the doc tables are missed. Fix: span the sealed base.
- [x] **A4 — `namespace-undefine!` silent no-op for sealed-base names** [Medium, S, Done 2026-06-15 — checks `DeleteBinding`'s bool; if nothing was removed from the mutable runtime AND the name is owned by the sealed base, raises `ErrImmutableBinding` ("cannot undefine sealed binding") instead of silently succeeding. User shadows still removable; unbound names still a no-op. Test `TestNamespaceUndefineSealedRejected` + doc updated]: `internal/extensions/namespace/prim_namespace.go:195` deletes only from `ns.Runtime()`'s own frame; primitives/bootstrap procedures live in the parent sealed base. **Verified: `(namespace-undefine! (interaction-environment) 'caar)` returns success yet `caar` stays bound.** Decide: error on undefining a sealed name, or document the no-op (do not mutate the shared sealed base).

**B. Immutable-default scope — entry points the scoping plan intended mutable — NEEDS DECISION**

- [x] **B1 — interactive REPL, MCP server, and `-e` inherit the immutable default with no opt-out** [High, M, Done 2026-06-15 — CLI adds `WithMutableTopLevel()` when `enterREPL`; `mcp.go` always mutable]: `cmd/wile/main.go:287` and `cmd/wile/mcp.go:235` build engines with no `WithMutableTopLevel()`, no CLI/env flag. Each REPL line / `-e` expr is a separate unit, so a first `(define x 1)` is stamped `Stable` and a later `(define x 2)` is rejected. **Verified on built binary:** `printf '(define x 1)\n(define x 2)\nx\n' | wile` → `Exception: cannot redefine immutable top-level binding "x"`. The scoping plan (`plans/2026-06-13-immutable-toplevel-by-default-scoping.local.md:357-370`) explicitly models the user/REPL top level as Chez's *mutable* `interaction-environment` (`define`-shadows-`define` permitted; never `Stable`). Default resolution: REPL + MCP opt into mutable; `-e` matches the file path (one begin-wrapped unit). **Confirm with user before changing the deliberately-designed enforcement.**
- [x] **B2 — `-e` diverges from file mode on redefine** [Medium, S, Done 2026-06-15 — `runEval` begin-wraps into one unit like `runFile`]: file mode (`runFile`, single `begin`-wrap) allows `(define w 1)(define w 2)` → `2`; `-e` (`runEval`/`EvalMultipleWithSource`, per-form units) rejects it. **Verified.** Same-batch entry points should behave identically. Folds into B1's resolution.
- [x] **B3 — redefine into a first-class `(environment …)` / `scheme-report-environment` rejected** [Medium, S, Done 2026-06-15 — child namespaces now mutable; regression test `TestSealedBase_B3_*`]: `machine/compilation/compile_validated.go:277` gate fires for any namespace where `ns.Runtime()==p.env`, including eval-environment children that inherit immutability via root delegation. **Verified: `(eval '(define zz 1) e)(eval '(define zz 2) e)` raises.** Undocumented for first-class envs. Tie to B1.
- [x] **B4 — split-brain default: internal bootstrap API mutable vs public `NewEngine` immutable** [Medium, S, Done 2026-06-15 — resolved as DOCUMENTATION: this is mechanism-vs-policy separation, not a bug. `internal/bootstrap` is the policy-free building block; the immutable default is a PRODUCT policy applied by the public Engine, and `internal/` is not an external embedder API. Made internal default immutable was rejected (would churn redefine-heavy testhelpers tests for no external benefit). Comment added to `NewTopLevelWithRegistry`]: `internal/bootstrap/bootstrap.go:246` `NewNamespaceFrame`/`NewTopLevelWithRegistry` never call `SetImmutableTopLevel`. **Verified: `bootstrap.NewNamespaceFrame(ctx).Namespace().ImmutableTopLevel()==false`.** Embedders on the internal API + `WithNamespace` get mutable; via `NewEngine` get immutable. Also makes `environment/sealed_base_frame_test.go` run its "immutable-default" characterizations under mutable mode.

**C. `Stable`-stamping scope — two divergent mechanisms — NEEDS DECISION**

- [x] **C1 — profile children diverge from the engine root on `Stable`** [Medium-High, M, Done 2026-06-15 — mutable child ⇒ bootstrap procs not stamped Stable; `set! car`/`set! caar` both permitted; regression test `TestSealedBase_C1_*`]: `internal/bootstrap/bootstrap.go` ~175 `initializeEnvironmentWithRegistry` (the `(environment '(wile kitchen-sink))` path) omits `WithStableBasePrimitives`, but the child still inherits `ImmutableTopLevel()==true` and the gate still stamps bootstrap procedures `Stable`. **Verified live: in a profile child `(set! car …)` is permitted but `(set! caar …)` is rejected — opposite of the engine root (both rejected).** Capture-safe prims aren't `Stable` in profile children, so the reclaim classifier can't trust them there (silent optimization loss).
- [x] **C2 — non-capture-safe bootstrap procedures frozen, contradicting docs** [Medium, S, Done 2026-06-15 — DOC FIX: behavior is intentional (anchors); docs now state bootstrap procs ARE frozen in the compiled program, mutable in interaction/eval contexts. NOT a code change: narrowing the stamp would lose the user-recursion GC win]: `machine/compilation/compile_validated.go:318` stamps `m.Stable = v.StableInUnit` with no capture-safety check, so `(set! map/assoc/caar/list? …)` is rejected under the default. **Verified.** But `docs/reference/r7rs-differences.md` says "the sealed base's own bootstrap procedures are not frozen by the stamp." The freeze buys no optimization (the classifier only trusts capture-safe names). Root cause shared with C1: two `Stable`-stamping mechanisms (capture-safe filter in `apply.go` vs `StableInUnit` here). Decide: unify behind one "trusted anchor" predicate, or fix the docs.
- [x] **C3 — `set!`-gate is not frame-scoped** [Low, S, Done 2026-06-15 — gate now keys on `IsStable()` directly (decoupled from `ImmutableTopLevel()`); a Stable anchor is never `set!`-able, even in a mutable child — sound + documented]: `machine/compilation/compile_validated.go:582` lacks the library exemption the define gate (`:277`) has, so a library `(set! <Stable-sealed-name>)` is rejected. Defensible (anchors must stay frozen) but undocumented as intentional vs the "libraries stay mutable" claim. Decide + document.

**D. Concurrency / isolation (lower incidence)**

- [ ] **D1 — sub-context/thread capture can leak library-eval defines** [Low, S — DEFERRED 2026-06-15, arguably-not-a-bug]: `machine/machine_context_subcontext.go` now captures `MutableRuntime()` (named) which returns the engine-root mutable global even from a library frame. For SRFI-18 THREADS this is correct by design (threads share the engine global). The only edge is a sub-context (`load`/`call-with-exit`) spawned *during a library's own load* landing defines in the engine global rather than the library frame — an extreme, untested case. Revisit only if a concrete isolation bug surfaces; not worth a speculative fix.
- [x] **D2 — lock asymmetry on thread-shared global** [Low, M, pre-existing — Done 2026-07-01, commit `fbcd7654`]: global bindings now publish their value through an `atomicCell` (atomic publish, lock-free load); the `noCopy` `atomic.Pointer` lives in the heap cell so `Binding` stays copylocks-clean for the value-embedded local frame (`[]Binding`). Locals keep the plain field (no atomic op, no box alloc on the hot Apply arg-bind path). Every global-frame entry point (`CreateGlobalBinding`, `Copy`, `SetGlobalBindingByIndex`) establishes the cell, so "in a global frame ⇒ has a cell" is structural. `binding_race_test.go` reproduces the tear (RED under `-race` pre-fix; clean post-fix). **Cost: +4.6% geomean on bench-gabriel, 15/16 slower** (`Binding` 32→40B inflates local slabs; global reads gain 2 pointer hops) — accepted, correctness over performance. Recovery lever (shrink `Binding`) tracked as a pure-perf follow-up (Tier 4). Original note: out-of-diff root cause (`environment/binding.go`), present before the layered-environment branch; `Value()`/`SetValue()` were unsynchronized while `set!` writes locked the frame mutex.

**E. Altitude / cleanup / efficiency**

- [x] **E1 — `.Namespace().Runtime()` ×8 Demeter band-aid; `TopLevel()` now lies** [Medium, M, Done 2026-06-15 — added `EnvironmentFrame.MutableRuntime()` (wraps `p.namespace.Runtime()`, behavior-identical to the chain — NOT `Runtime()`, which diverges for library frames); migrated all 8 sites + trimmed redundant comments; unit test pins the `TopLevel()` distinction; axis-b manifest regenerated for the line shifts]: the carve left `TopLevel()` returning the sealed base while 8 production call sites migrated to `.Namespace().Runtime()`, each with the same explanatory comment. Add a named `EnvironmentFrame.MutableRuntime()` (or `UserGlobal()`); the next contributor reaching for `TopLevel()` silently gets the frozen base. Altitude root of A4/B1/B3/D1.
- [x] **E2 — `loadBootstrapSources` duplicated AND behaviorally diverged** [Medium, M, Done 2026-06-15 — extracted `compilation.LoadBootstrapSources` (optimized + pooled, the production behavior, with `kind` error context); engine.go + bootstrap.go reduced to thin wrappers; deleted both diverged copies + `runBootstrapMacroStx`. Divergence fixed: the internal path now optimizes too. ALSO deduped `NewSchemeReportNamespace`/`initRuntimeFrame`: extracted `wireRuntimeFrames` as the single source of truth for the two-frame topology + phase wiring (they differ only in fresh-vs-copied globals)]: `pkg/wile/engine.go:853` vs `internal/bootstrap/bootstrap.go` — one path optimizes templates + pools contexts, the other doesn't, so bootstrap procedures loaded via the internal path run un-optimized. Dedup behind one pipeline; decide the optimize/no-optimize question. (Related: `NewSchemeReportNamespace` re-hand-builds the two-frame stack `initRuntimeFrame` exists to centralize.)
- [x] **E3 — `registerSchemeDocstrings` re-walks the root sealed base on every `(import …)`** [Low, S, Done 2026-06-15 — guarded with `env.SealedBaseTarget() == env` to skip flat library frames]: `pkg/wile/engine.go:736` is called from `applyBaseEnvironment`, which the library-env factory also invokes; for a library env `Namespace()` is the shared root, so each import re-parses ~500 root docstrings and discards them via dedup. Run once at bootstrap, or guard with `env.SealedBaseTarget()==env`.

---

- [x] **Data race: error/backtrace capture vs. concurrent VM mutation under SRFI-18 thread-terminate** [High, M, Done]: `go test -race ./extensions/threads/` reports a `DATA RACE` (reproduces deterministically; `TestMutexAbandonedOnTermination` is the trigger). **Pre-existing** — present on `origin/master`, verified by reproducing it at the merge-base independent of any in-flight work. **Not surfaced by `make ci`**, because CI does not run `-race` on the threads package and the happy path never enters the error/teardown path that conflicts.

  **The race.** When one SRFI-18 thread *terminates* another mid-execution, the terminator captures a Scheme exception + backtrace via `goErrorToSchemeException` (`machine/machine_context.go`), whose stack-trace walk *reads* the victim's `mc.template`, `mc.pc`, and continuation chain (`mc.cont` → `cont.template.Name()`, `cont.pc`; ~lines 1087/1097/1125). Concurrently the victim's own `Run` loop *writes* those same fields as it advances — `mc` is reassigned by `pullDrainAndApply` (`OpPullApply`, ~line 600) and `callForeignCached` (`OpCallForeignCached`, ~line 642), driven by the per-thread `RunWithEscapeHandling` loop (`err := p.Run()`, ~line 1438). Reader and writer touch the **same `*MachineContext` / continuation graph** with no happens-before edge: thread-terminate does not quiesce the victim before its context is inspected from the terminator's goroutine.

  **Why it matters.** Reading a torn `mc.pc` / `mc.template` / `cont` during teardown can yield a corrupt backtrace or, under the wrong interleaving, a nil-deref / out-of-bounds in `SourceAt`. It is a correctness hazard in the SRFI-18 termination path, not merely a `-race` tooling warning.

  **Precedent.** Same *family* as **PR #561**, which removed `NoCopyApply` / `computeNoCopyApply` precisely because it was "unsafe under concurrent invocation" (`docs/continuations/optimizations.md`). That fix covered concurrent *apply*; this instance is the concurrent *error / terminate* path and was never recorded.

  **Root cause (narrower than the sketch above).** Not "terminate doesn't quiesce" per se — `NewThreadSubContext` set the thread's `parentMC` to the *live* spawning context. That `parentMC` link is for *synchronous* sub-contexts (eval/apply, parent paused on the same goroutine), but a thread's parent runs concurrently, so every `parentMC` walk — `CaptureStackTrace`, `findParameterInMarks`, the `subContextPool` release counter — crossed the goroutine boundary into the mutating parent. The earlier T4 fix snapshotted the parent's *fields* at spawn but left the *pointer* to be dereferenced later.

  **Fixed.** Sever `parentMC` for thread contexts (a thread is an independent root, not a sub-context): removed the `ParentMC` field from `SubContextParams`/`CaptureSubContextParams` and left `parentMC` nil in `NewThreadSubContext` (`machine/machine_context_subcontext.go`). One change fixes all three `parentMC`-walk races. Confirmed safe: `ParentMC()` has no callers, `CaptureSubContextParams` is thread-only, and the parameter inheritance the live walk provided was untested *and* not SRFI-18-correct (a racy live read, not a creation snapshot). `go test -race ./extensions/threads/` passes; full suite green. Trade-off: cross-thread dynamic-parameter inheritance is dropped (it was unsound) — restoring it correctly via a creation-time parameter snapshot is a separate, properly-scoped enhancement if needed.

  **Discovered:** 2026-06-13, running the Phase-5 continuation / `-race` gate of the escape-gated frame-reclamation work (`plans/2026-06-11-escape-gated-frame-allocation.local.md`); orthogonal to that work.

- [x] **vmState field coverage test** [High, S]: Reflection-based test enumerating vmState fields, asserting each appears in a coverage table keyed by operation. Prevents silent state corruption when fields are added. See [FCA Assessment](#fca-assessment) below.
- [x] **Error type identity** [Medium]: Determined: `CompilationError` and `RuntimeError` are **public boundary types** — they translate internal errors (`werr.ForeignError`, `machine.SchemeError`, `machine.ErrExceptionEscape`) to the embedder API. They should NOT implement `SchemeError` or `ForeignError`. Embedders use `errors.As` to match them. `RuntimeError` already has `Source`/`StackTrace`; `CompilationError` lacks source because the compiler doesn't propagate `SourceContext` into its errors — fix belongs in "error stack traces" below.
- [x] **Exceptions and error stack traces** [Medium, Done]: `SourcedError` type in `compilation/`, `CompileExpression` wraps errors with source context, `CompilationError.Source` field populated from cause chain. Phases 1-4 complete (PR #657 + precision fix in `processLibraryImport`). Datum-level functions (`import_set_datum.go`, `library_bindings.go`) operate on `values.Value` without syntax context — callers wrap. Foreign stack trace entries for Native → Foreign → Native callback crossings (P3) remain deferred — design doc moved to `memory/2026-04-14-error-stack-traces-design.local.md` (§P3).
- [x] **MCP eval fails on schelog `include`** [Not a bug]: Original report was missing `puzzle.scm` include and `(set! *schelog-use-occurs-check?* #t)`. Without occurs check, the puzzle infinite-loops and hits MCP timeout. With correct setup, MCP eval produces the correct answer.
- [x] **`read` mid-parse EOF should raise read-error, not return EOF** [Done]: Phase 4 exceptions audit G.1 — `(read "(foo")` returned `#!eof` instead of raising. Fixed by `wrapMidParseEOF` helper in `internal/parser/parser.go` that converts `io.EOF` to a `ParserError` wrapping `io.ErrUnexpectedEOF` at all mid-parse sites (readList, readLabeledList, readVector, readByteVector). `PrimRead`/`PrimReadSyntax`/`PrimReadToken` unchanged — the existing `errors.Is(err, io.EOF)` check correctly rejects the new `io.ErrUnexpectedEOF` and falls through to `WrapForeignReadErrorf`, producing a `ForeignReadError` that `goErrorToSchemeException` maps to `NativeErrorKindRead`, making `(read-error? e) → #t`. Test in `registry/core/prim_read_mid_parse_eof_test.go` covers 6 mid-parse cases + 2 clean-EOF regressions.
- [x] **Silent failures in `compilation/operation_syntax_case.go`** [Medium-High, S–M, Done — PR #732]: Four error-handling defects surfaced by `pr-review-toolkit:silent-failure-hunter` during PR #731 crosscheck (pre-existing — not introduced by that PR). Fix as one cohesive sweep. (1) **HIGH** — `operation_syntax_case.go:95-103` swallows all `matcher.Match` errors via `nolint:errcheck, nilerr`, on the premise that "match failed = normal control flow." `match.ErrNotAMatch` already exists (`internal/match/syntax_compiler.go` re-exports `werr.ErrNotAMatch`); the call site just doesn't use `errors.Is` to discriminate. Context cancellation (`ctx.Err()` returned at `match.go:267`), malformed input, and ellipsis-depth violations all collapse to "set #f and continue", masking real failures as "no matching clause". Fix: gate the swallow on `errors.Is(err, match.ErrNotAMatch)`; propagate other errors via `mc.WrapError`. The `nolint:nilerr` comment is the smoking gun — the linter detected exactly this hazard. (2) **HIGH** — `operation_syntax_case.go:154-165` (the `MaybeCreateLocalBinding` bind loop) discards the error return with `_`; further, when a pattern var is in `p.PatternVars` but missing from `sc.bindings`, falls through to `SetLocalValue(li, nil)` with no diagnostic. Three branches conflated: "binding creation failed", "binding already exists at outer scope" (skip), "value missing for declared pattern var" (currently silent corruption). Fix: separate the three. (3) **MED** — `operation_syntax_case.go:82, 143, 221` use `sc, _ := mc.SyntaxCaseState().(*syntaxCaseState)` then collapse "field nil" and "type mismatch" into the same "no input available" message. The marker-interface revert (PR #731 Q-c) means a wrong type *can* be stored without compile-time rejection; readers should distinguish. Fix: `raw := mc.SyntaxCaseState(); if raw == nil { ... }; sc, ok := raw.(*syntaxCaseState); if !ok { ... unexpected type %T ... }`. (4) **MED** — Generic `mc.Error` strings at lines 84, 145, 196, 223 lack input/source-location context; macro debugging is hard enough without these. Fix: include input syntax (and source location if available via `mc.SourceLocation()`) in each message. **Reference**: silent-failure-hunter agent output captured in PR #731 review aggregation (2026-05-10).
- [x] **Audit PrimitiveSpec `ReturnType` and `ParamTypes` annotations** [High, L] — **complete**: Phase 1 axis-A (docs ↔ ReturnType) clean. Phase 3 axis-B (ReturnType ↔ impl, 5 tightened in PR #675). Phase 4 axis-C R7RS sweep clean. Phase 5 ParamTypes audit: 5.A (manifest, PR #678), 5.B (analyzer + inventory, PR #679), 5.C (sidecar, `plans/2026-04-20-paramtypes-annotation-bugs.md`), 5.D (one partial narrowing: `get-output-bytevector` → `TypeBinaryOutputPort`), 5.E (`plans/2026-04-20-paramtypes-axis-c-findings.md` R7RS sweep) all complete. **Finding**: declared-too-narrow bucket empty (3 FPs confirmed); declared-too-wide dominated by TypeConstraint-vocabulary gaps (~85 entries, cross-referenced with axis-B Category C's 28 return-side gaps); ~25 sub-domain refinement-type candidates (R7RS "exact non-negative integer", "byte in [0,255]", etc.) below ValueType granularity. Four-axis framework closed. Next forward work: vocabulary-extension design (Extension API contracts Phase 2+), separately scoped. Original scoping rationale below:
  1. **Annotation vs implementation** (mechanical, tool-assistable via `wile-goast` belief or fuzz harness): for each primitive, verify the annotation is the narrowest *sound* type covering every return path; flag dead branches.
  2. **Implementation vs published standard** (R7RS-small first, then R6RS, SRFI, Racket, Chibi/Guile/Chicken where applicable): for each primitive whose name appears in a standard, verify the Wile implementation's domain (accepted params) and codomain (return shape) match the spec. Catches non-standard extensions masquerading as standard primitives (the open-output-bytevector one-arg branch).
  3. **Annotation vs standard**: cross-check ensures we don't document non-standard behavior as if it were standard.
  Wile-specific primitives (no entry in any adopted standard) need a **local spec** written before they can be audited — name, intended domain, intended codomain, error cases, any invariants. Without a spec there is nothing to drift *from*. Produce the spec inventory as a deliverable; treat missing specs as a debt sub-item. This audit becomes load-bearing the moment "Extension API contracts Phase 2+" (Tier 2) ships compile-time checking — unsound annotations then turn into wrongly-rejected programs, and the R7RS-compliance-as-baseline product claim starts to depend on evidence rather than assertion.
- [x] **Bound expander recursion depth** [Medium, Done]: The parser depth limit
      (memory/2026-06-04-parser-depth-limit-impl.md) closes the textual-input
      stack-overflow surface, but programmatically-constructed deep syntax (macro
      output, `datum->syntax`, quasiquote) could still overflow the expander with a
      fatal, unrecoverable Go stack overflow. **Fixed**: added `werr.ErrExpandDepthExceeded`
      and a shared `expandDepthGuard` on `machine/compilation`'s `ExpanderTimeContinuation`,
      incremented/decremented at the single recursion chokepoint `ExpandExpression`
      (which every descent — nested cars, argument lists, primitive-form bodies via
      child expanders, and macro re-expansion — funnels through). Unlike the parser
      (one object per parse), the expander spawns child expanders for lambda/let/
      let-syntax bodies, so the guard is **shared by pointer** across a run via
      `newChildExpander` (which otherwise reproduces the prior construction exactly —
      `libraryScope` deliberately stays nil to preserve hygiene behavior). Default
      `DefaultMaxExpandDepth = 50000` (0 = unlimited), chosen empirically: the
      expander overflows the Go stack between ~400k (heavy macro-re-expansion) and
      ~800k (light call nesting) levels, so 50000 leaves an order-of-magnitude margin
      while sitting far above any practical program (flat recursive macros like
      `and`/`or` are O(N²) to expand and unusable well before 50000 clauses).
      Configurable via `WithMaxExpandDepth` (threaded engine→`ExpandAndCompile` like
      `inlineThreshold`) and `ExpanderTimeContinuation.SetMaxDepth`. Mirrors the VM's
      `DefaultMaxCallDepth` and the parser's `DefaultMaxParseDepth` triad. Tests:
      `machine/compilation/expander_depth_test.go` (trip, default-protects,
      within-limit, shared-across-child-expanders, unlimited, decrement-on-return),
      `engine_expand_depth_test.go` (end-to-end via recursive macro + the option).
- [x] **Bound writer recursion depth** [Medium, Done]: The fourth and final leg
      of the recursion-depth quad (VM `DefaultMaxCallDepth`, parser
      `DefaultMaxParseDepth`, expander `DefaultMaxExpandDepth`, now writer
      `DefaultMaxWriteDepth`). `(write (make-list 10000000))` overflowed the host
      Go stack with a fatal, unrecoverable crash. **Root cause was two distinct
      surfaces, fixed separately** (the guiding invariant: *anything the writer
      emits must be valid on read*): (1) **Length ≠ depth** — `SchemeWriter`'s two
      analysis passes (`findShared`, `filterToCircular`) recursed once per
      cdr-spine element while the output pass (`writePairContents`) already
      iterated it, so a *flat* list of any length (nesting depth 1, perfectly
      re-readable) overflowed the analysis passes. Both now walk the cdr-spine
      iteratively, recursing only into cars/elements (genuine nesting). (2)
      **Nesting bound** — car/element recursion is now capped at
      `DefaultMaxWriteDepth = 10000`, counted identically to the parser's
      `readSyntax` (root = 1, +1 per container descent) so the write and read
      limits trip on exactly the same structures. Pass 1 (`findShared`) enforces
      the bound, so passes 2–3 run only on depth-valid structure and stay within
      `maxDepth` Go frames. Added `werr.ErrWriteDepthExceeded`; the three writer
      entry points (`WriteValueToString`/`WriteSharedValueToString`/
      `DisplayValueToString`) and `SchemeWriter.WriteString` now return
      `(string, error)`, so `write`/`display`/`write-shared` raise a catchable
      Scheme condition rather than emit unreadable output. Configurable via
      `SchemeWriter.SetMaxDepth` (0 = unlimited). **No `WithMaxWriteDepth` engine
      option**: unlike parse, the writer has no engine-owned entry point — it is
      reached only through the io primitives — so per the parser's documented
      `(read ...)` limitation the primitives use the default. `write-simple`
      bypasses `SchemeWriter` (it uses `Value.SchemeString`, which was a separate
      unbounded recursion — **now closed by the SchemeString depth bound below**).
      Tests:
      `pkg/values/scheme_writer_test.go` (default-protects, configurable boundary,
      unlimited, flat-not-bounded-by-depth, long-flat-no-overflow),
      `pkg/wile/engine_write_depth_test.go` (end-to-end long-flat + deep-nested
      raise).
- [x] **Bound `SchemeString` recursion depth (staff-sweep #3)** [Medium, Done]: The
      fifth leg of the recursion-depth quad. `Value.SchemeString()` — the non-writer
      render path reached by `write-simple`, error messages, and Go-side `%v`/`%s` —
      recursed one Go frame per nesting level and **host-crashed** (fatal, unrecoverable
      `stack overflow`) on a deeply nested *acyclic* value: `(write-simple (deep-nest N))`
      overflowed at ~10⁶ levels. **Root cause identical to the writer's surface 2**: the
      cdr-spine walk was already iterative (a flat list of any length is depth 1, safe),
      but car/element descent recursed unboundedly. Path-scoped *cycle* detection does not
      bound *depth* — the two guarantees are orthogonal (an acyclic chain never re-hits a
      marked node). **Fix**: thread a `depth` counter through the single chokepoint
      `schemeStringChild` (`pkg/values/utils.go`) — the one function all compound descent
      (Pair car / improper cdr, Vector element, Hashtable key/value) flows through — plus
      the three `schemeStringWithVisited` methods + `formatIndexable`; guard at
      `depth > DefaultMaxWriteDepth` (reuse the writer's single host-safe nesting number,
      counted root = 1 / +1 per container descent, so write and SchemeString trip on the
      same structures). **Diverges from the writer on failure semantics**: `SchemeString()
      string` is the `Value` interface contract and cannot raise, so it **degrades** to a
      distinct marker `#<deep>` (vs. the cycle marker `...`) rather than returning
      `ErrWriteDepthExceeded`. The `Pair.String()` `fmt.Stringer` twin shared the same
      defect (`fmt.Sprintf("%v", deepPair)`) and got the same bound (same-pattern-everywhere
      discipline). Tests: `pkg/values/pair_test.go` (deep-bounded SchemeString + String,
      exact `DefaultMaxWriteDepth` boundary, flat-list-not-bounded, cross-type
      pair→vector→pair). Doc: `pkg/extensions/io/CLAUDE.local.md`. Plan:
      `plans/2026-07-01-staff-engineer-sweep.md` #3.
- [x] **Unify complex/imaginary number parsing (staff-sweep #5)** [Medium, Done]: The
      reader (`parser_number.go`) and `string->number` (`extensions/math/prim_conversion.go`
      via `parser/number_string.go`) implemented the same rectangular-complex / pure-imaginary
      grammar **twice**, and had already **drifted into two different wrong answers** on the
      same input: `+3/4i` (pure imaginary, rational coefficient) was *rejected* by the reader
      (`malformed input`) but accepted by `string->number` as *inexact* `0.0+0.75i` — while
      R7RS §6.2.5 makes it exact `0+3/4i` (as `0+3/4i` and `3/4+1/2i` already parsed). Root
      cause: the pure-imaginary path gated its exact branch on `isIntegerString`, so a rational
      coefficient fell through to the inexact `parseFloatOrInfnan`; the reader's twin used a
      bare `strconv.ParseFloat` that rejected `3/4` outright. **Fix** (two moves): (1) correct
      the shared grammar — `ParseImaginaryStringNumber` gates on `isExactPartString` (handles
      rationals via `parseExactPart`), so a rational coefficient stays exact; (2) unify — the
      reader's `parseImaginary`/`parseComplex` now **delegate** to
      `ParseImaginaryStringNumber`/`ParseComplexStringNumber` (the single grammar source of
      truth), adding only the reader's source-located `NewParserErrorf` on reject. Deleted the
      now-dead `parseImagPart` (folded into the pure function's inline sign switch). Left
      `parseImaginaryInf`/`parseImaginaryNan` — tokenizer-driven dedicated infnan tokens, a
      separate concern from the duplicated grammar. Tests: `pkg/parser/parser_number_test.go`
      (`TestParseNumber_PureImaginaryRationalIsExact` + `TestParseNumber_ReaderAgreesWithStringParsers`,
      a reader-vs-pure-function parity guard that pins the single-source-of-truth invariant);
      `extensions/math/prim_conversion_test.go` (`+3/4i` exact cases); redirected
      `TestParseImagPart` through the unified `ParseComplexStringNumber`. Plan:
      `plans/2026-07-01-staff-engineer-sweep.md` #5.
- [x] **Parser fuzz targets + reader crash-safety hardening** [Medium, Done]: Added
      the first Go native fuzz targets in the repo — `FuzzReadSyntax` (untrusted-input
      contract: never panic/overflow the host; every non-EOF error is a located
      `*ParserError`) and `FuzzReadWriteRoundTrip` (write output must re-read) in
      `pkg/parser/reader_fuzz_test.go`, seeded from `reader_robustness_test.go`. The
      example-based reader tests enforced the contract only on inputs someone wrote
      down; fuzzing found **8 pre-existing reader bugs in ~2 min, 5 of them host
      panics**, all now fixed with committed regression corpus under
      `pkg/parser/testdata/fuzz/`: (1) invalid UTF-8 and (3) bad datum-label numbers
      leaked raw `*tokenizer.TokenizerError`/`*strconv.NumError` — closed as a CLASS by
      a boundary catch-all `locateReaderErr` that lifts any non-EOF/non-`*ParserError`
      to a located error; (2) `#0=)` silently mis-parsed to a nil-datum label; (4)
      `' )` panicked (nil interface conversion in `readQuoteForm`); (5) `#\<NUL>`
      panicked (`rs[0]` index in `parseCharacter`); (6) `#e)` panicked (nil deref in
      `readExactnessMarker`); (7) `#b0/0` panicked (`big.Rat` div-by-zero); (8)
      `#0=(#d)` panicked (nil list element in `readLabeledList`). Each
      unguarded-`readSyntax`-caller fix follows the existing nil-at-delimiter guard
      pattern; also made `ParserError.SchemeString` nil-token-safe. `FuzzReadSyntax`
      now runs clean (180s, ~20M execs). Round-trip target additionally fixed: string
      escaping (`String.SchemeString` used Go `%q` → proper R7RS `\xHH;`/mnemonics, a
      real conformance bug) and `#0=()` empty labeled list (→ `()` not `(#<void>)`).
      **Deferred** to #781: the numeric-tower external-representation round-trip tail
      (`#m` big floats write without prefix; audit `BigComplex`/`BigInteger`/etc.) —
      a distinct numeric-formatting conformance pass. **Partially closed 2026-07-09:**
      the `1e+700`-rejected-on-read half is fixed — scientific/decimal notation whose
      magnitude overflows float64 now promotes to `BigFloat` (shared
      `parser.ParseRealFloatString`, mirroring int64→BigInteger) across the reader and
      `string->number`, so out-of-range bigfloats round-trip. The `#m`
      write-without-prefix half (in-range bigfloats lose their type on read) remains.
- [x] **Stable-matching selectors fail + matching tests don't gate CI** [High, M, Done]:
      **Root cause** (single bug, two symptoms): `walk-for-cycle` in
      `stdlib/lib/wile/algebra/matching.scm` stored each rotation cycle in
      *newest-first* (reversed) order because an extra `(reverse …)` undid the
      oldest-first ordering the cons-accumulation already produced. `apply-rotation`
      reads a cycle as "proposer mᵢ → receiver of m_{i+1}" (= successor(mᵢ)), which
      only holds in oldest-first order. A 2-cycle is its own inverse, so the 2×2
      fixtures masked it; for length-≥3 cycles the reversed traversal ran the
      rotation backwards, collapsing M_top straight to M_bot and hiding the interior
      matching. That made `rotations` return 1 instead of 2 ("expected 2 but got 1")
      and made `enumerate-stable-matchings` (hence the Conway lattice) miss interior
      stable matchings, so `sex-equal-stable-matching` couldn't find the |Δ|=0
      optimum M₁ ("expected #t but got #f" / "expected #f but got #t"). **Fix**:
      removed the stray `reverse` so the cycle is oldest-first. Verified `rotations`
      now returns both ρ₁ (M_top→M₁) and ρ₂ (M₁→M_bot). **CI-gate gap**: added
      `(test-exit)` to `algebra-matching-test.scm` and swept every `*-test.scm` under
      `test/` and `stdlib/lib/` — 12 chibi-test files lacked `(test-exit)`
      (characters, control, eval, exceptions, lazy, macros, numbers, ports, records,
      smoke, strings scheme tests + algebra-unification); all now gated.
      `stdlib/lib/wile/algebra/sat-test.scm` uses a custom `check` harness (not chibi
      test) that displayed "FAIL:" but exited 0 — same silent-failure class, fixed by
      raising `(error …)` in its FAIL branch. `stdlib/lib/wile/er-macro-test.scm` is a
      macro fixture with no assertions (left as-is). `make lint` + `make covercheck`
      green (53/53 scheme files, 0 failed). Provenance: PR #767 crosscheck, 2026-06-06.

---

## Tier 2 — Embedding API & Product Value

The embedding experience that differentiates Wile.

- [ ] **Extension API contracts Phase 2+** [Embedding, High]: Compile-time (compiler consults `ParamTypes` for static call sites — error before execution, zero runtime cost) and runtime (`buildValidator` wires `ParamTypes` → `SetValidator`). Integration with linter. Prerequisite vocabulary-extension design at `plans/2026-04-21-type-constraint-extension-design.md` (Julia-subset nominal lattice, `OpaqueTypeConstraint`, `Subtype` as primary operation; excludes refinement and union types per invertibility/no-duplication principles). Original parent: `plans/2026-03-26-extension-contracts-design.md`
- [x] **Environment profiles** [Embedding, Done]: Named profiles (Tiny, Console, ConsoleWithLoad, Small, KitchenSink) via `WithProfile`; orthogonal `WithSandbox` modifier; virtual env map (`WithEnv`, `WithEnvMap`); Scheme-level `(environment '(wile <profile>))` support; `SafeExtensions`/`AllExtensions` removed. `plans/2026-03-26-environment-profiles-impl.md`
- [x] **Eager documentation index** [Tooling, Done]: Shipped as lazy-build-and-cache rather than eager scan. `LibraryExportIndex` is built on first `apropos`/`doc` query and cached on `Namespace`; Scheme-level `(apropos)`, REPL `,apropos`, and MCP share the same index, so LLMs can discover unloaded-library procedures from the first query. See PRs #623–625 (`memory/LIBRARY-EXPORT-INDEX.md`) and post-#623 asymmetry fix (`memory/PRIM-APROPOS-EXPORT-INDEX.md`). Original eager-scan design (`2026-04-08-eager-doc-index-design.md`) was superseded before any code shipped.
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.
- [ ] **MCP triggering rewrite (Lever A)** [Embedding, Text-only]: Rewrite `cmd/wile/mcp.go` `WithInstructions`, 9 tool descriptions, and `prompts/wile-scheme.md` to trigger LLM tool use on algebra/modular/polynomial domains. Correct misleading `libraries` description (currently claims "loaded only" but tool returns full catalog). Validation via `algebra-accuracy` benchmark: closes `powerset_lattice` regression. No code logic changes. `plans/2026-04-18-mcp-triggering-rewrite.md`

### Algebra & Analytics Roadmap

Directions documents — identify prioritized capability extensions. Priority sequence per 2026-04-22 decision: **wile-goast-first** (Tier A — named consumers in wile-goast analysis code, giving wile-goast a complete algebraic palette without digressions into wile), **matching-second** (Tier B — Roth-Sotomayor two-sided matching), then **§5.7 lower-priority** (Tier C).

- [x] **Algebra library roadmap** [Directions]: `plans/2026-04-17-algebra-foundations-directions.md` identifies 6 prioritized directions extending `(wile algebra ...)`. §5.1 `(wile algebra matrix)` shipped via Path D (PRs #684–#691, #695, #696). §5.2 Möbius / incidence algebra — shipped (commit `4ff8a314`, `plans/2026-04-21-incidence-algebra-impl.md`). §5.3 AC-matching shipped via `(wile algebra unification)`, `plans/2026-04-21-ac-matching-impl.md` (Phase 6 closeout). §5.4 Group actions & Burnside shipped as extension of `(wile algebra group)` (`plans/2026-04-22-group-actions-burnside-impl.md`). §5.5 Distributive/modular lattices + Birkhoff shipped as extension of `(wile algebra lattice)` (`plans/2026-04-22-lattice-birkhoff-impl.md`). §5.6–§5.7 broken out as individual items below.
- [ ] **Benchmark statistics (gonum)** [Directions]: `plans/2026-04-18-gonum-integration-directions.md` §5.2 identifies a benchmark-statistics gap in wile. Ships `bench-stats/` ~100–150 LOC; pure Go, no CGo, one `go.mod` entry. Independent track from the companion wile-goast `goastgraph/` work (see wile-goast TODO). Distinct algebraic setting from `(wile algebra matrix)`: gonum is field-valued (ℝ/ℂ), not semiring-parameterized.

#### Tier A — wile-goast-first (named consumers in Appendix A)

- [x] **§5.4 Group actions & Burnside** [Algebra, wile-goast, High]: Shipped as extension of `(wile algebra group)` in place (D1 — not a new `(wile algebra group-action)` library). Extends `<group>` record with optional metadata (element?, setoid, order, elements, generators); adds `<group-action>` record, BFS-from-generators `orbit`/`stabilizer`/`fixed-points`, `orbit-representative` with documented tie-breaker, `burnside-count` with divisibility validation, presets (`trivial`/`cyclic`/`symmetric`/`product` groups; `trivial`/`permutation`/`regular`/`conjugation`/`product` actions). 124 tests; end-to-end verified via Burnside on conjugation-action of S_3 = 3 conjugacy classes. Available for wile-goast migration of register-renaming (`goastssa/prim_canonicalize.go`), binop commutativity (`ssa-normalize.scm` `ssa-rule-commutative`), and `boolean-simplify.scm`. Plan: `plans/2026-04-22-group-actions-burnside-impl.md`.
- [x] **§5.5 Distributive/modular lattice + Birkhoff** [Algebra, wile-goast, Matching, High]: Shipped as extension of `(wile algebra lattice)` in place (not a new library). Extends `<lattice>` record with three optional metadata fields (setoid, cardinality, elements); ships `distributive?` / `modular?` exhaustive axiom-check predicates + sample-based `validate-*[/setoid]` siblings; `join-irreducibles` / `meet-irreducibles` via lower/upper cover counting; `birkhoff-representation` / `birkhoff-reconstruction` roundtrip with smart O(|downsets(P)|) enumerator; `lattice->locally-finite-poset` projection; five presets (chain, boolean, diamond/M3, pentagon/N5, free-distributive). Also extends `<locally-finite-poset>` with optional `elements` field + `lf-poset-elements` accessor. Dedekind numbers verified through D(5) = 7581 (~1.5s). 155 tests. Available for wile-goast migration of `dataflow.scm` `run-analysis` MOP=MFP certification and `domains.scm` precision annotations. Plan: `plans/2026-04-22-lattice-birkhoff-impl.md`.
- [x] **§5.6 Combinatorial graph** [Algebra, wile-goast]: Shipped as new `(wile algebra combinatorial-graph)` — distinct from `graph.sld` (which remains as semiring-Bellman-Ford). 1-WL color refinement + individualization-refinement backtracking for complete graph isomorphism (Weisfeiler–Leman 1968; McKay–Piperno 2014), spanning-tree count via deletion-contraction with fast paths (Cayley, C_n, tree, empty), chromatic and Tutte polynomials via deletion-contraction (Read 1968, Tutte 1954) with |V|+|E|≤20 size cap, Hopcroft-Karp O(E·√V) bipartite matching, six preset fixtures (K_n, C_n, P_n, K_{m,n}, empty, Petersen). Setoid-carried vertex equality, tier-1/tier-2/tier-3 finiteness per §5.4 pattern. 225 tests including Petersen backtracking-correctness canary and C_6 vs 2K_3 cospectral non-iso canary. `plans/2026-04-22-combinatorial-graph-impl.md`.
- [x] **Balanced graph partition (`graph-partition`)** [Algebra, wile-goast]: Shipped as an extension of `(wile algebra combinatorial-graph)` — a two-way *balanced* cut (NOT a global min-cut, which degenerates to isolating one vertex) via Kernighan-Lin pair-swaps. Holds the seed's A/B ratio; `balance` bounds the seed ratio; returns `group-a`/`group-b`/`cut-weight`/`sizes`/`normalized-cut`. The s–t and global min-cut family (Ford-Fulkerson, Dinic, Karger, Stoer-Wagner) was rejected with rationale; single-vertex FM was tried and dropped (deadlocks from a balanced seed under a tight tolerance). 4 test groups (K(3,3) optimal-cut + star degeneracy-guard canaries). Motivating consumer is wile-goast `recommend_split`, which currently mislabels a heuristic as "min-cut". **Phase 2** (wile-goast): build the import-signature affinity graph, rewire `find-split` to call `graph-partition`, recalibrate confidence off `normalized-cut`, fix the docstrings — separate plan in the wile-goast repo. **Phase 3** (deferred): Shi-Malik normalized-cut `'method`, gated on the gonum eigensolver. Design+impl: `plans/2026-06-08-balanced-graph-partition-design.md`, `-impl.md`.
- [x] **§2.2 Free Boolean algebra on atoms** [Algebra, wile-goast, Done]: Shipped via extraction from wile-goast's `boolean-simplify.scm` L23-69. Named entry points `symbolic-boolean-normalize` / `symbolic-boolean-equivalent?` in `(wile algebra symbolic)` — normalize under `boolean->theory`. **Axiom completion (2026-06-09)**: De Morgan, complement laws (x ∧ ¬x ⇒ ⊥), and bound identities are now applied. `boolean->theory` was re-architected from 11 pairwise axioms to 7, replacing pairwise commutativity+associativity+idempotence with a single AC-normalization axiom (flatten → sort → dedup → fold). This fixed a **pre-existing** associativity+commutativity non-termination (≥3-leaf AC terms fuel-exhausted) and made complement detection n-way (`a ∧ b ∧ ¬a ⇒ ⊥`). Not a decision procedure (no distributivity — use `(wile algebra sat)`). Follow-up: the same AC fix is available for semiring/ring/field `+` (same latent bug, not migrated this task). `plans/2026-06-09-free-boolean-axiom-completion.md`. Also shipped Tier 2+3 of the same extraction plan: `(wile algebra abstract-domain)` with `sign-lattice` + `abstract-sign` + `sign-binop`; `(wile algebra dataflow)` with `<cfg-protocol>` record + `run-analysis` MFP worklist solver + `reverse-postorder` + `analysis-in/out/states`. `plans/2026-04-22-wile-goast-algebra-extraction-design.md` + `-impl.md`.
- [x] **SAT solver** [Algebra, Done]: `(wile algebra sat)` ships `sat?`, `sat-cnf?`, `sat-model`, `boolean-decide-sat?`, `boolean-decide-equivalent?`. CDCL kernel in `extensions/sat/` (watched-literal propagation, 1-UIP analysis, VSIDS, Luby restarts). Closes De Morgan, complement-law, distributivity, bound-identity gaps in `symbolic-boolean-equivalent?`. `memory/2026-05-30-sat-solver-design.md`, `-impl.md`.
- [x] **CFL-reachability path algebra** [Algebra, wile-goast, Done]: **Shipped** — `(wile algebra cfl)`: general CFG engine (typed production kernels `cfl-epsilon`/`-terminal`/`-unary`/`-binary`, normalized by construction) + labeled-edge graph + Reps–Horwitz–Sagiv worklist solver (`cfl-solve` + `cfl-reachable?`/`-from`/`-pairs`/`cfl-derives?`) + `dyck-grammar` preset for interprocedural/field-sensitive analysis + validators. Context-sensitivity canary proves it is strictly more precise than Boolean (semiring) reachability. Design+impl: `plans/2026-06-05-cfl-reachability-design.md`, `-impl.md`. Original entry: New `(wile algebra cfl)` (or a `semiring.scm` extension) for context-free-language reachability — the path-algebra generalization where edge labels compose under a context-free grammar rather than a free semigroup. **The single open wile-side algebra gap with a named, current consumer**: wile-goast TODO C4 ("CFL-reachability — context-sensitive analysis") tags it explicitly as a *wile-side gap*. It is **not** expressible through the existing semiring API — the composition rule is grammar-constrained, not associative/free, so it cannot be parameterized from `semiring.scm`'s `(plus, times, zero, one)` shape (this is why C4's Boolean/tropical sub-items shipped but this one stalled). Algorithm: Reps–Horwitz–Sagiv (1995) "Precise interprocedural dataflow analysis via graph reachability" — worklist over (node, grammar-symbol) pairs. **Demand-audit note (2026-06-05)**: a wile ↔ wile-goast TODO cross-check found this is the *only* algebra item with a real downstream consumer; the entire §5.7 Tier C menu below (matroids, Hopf, submodular, RSK, category extensions, partitions) currently has **no** wile-goast consumer, despite the "wile-goast-first" priority principle. Scope: design doc first (grammar representation, productive-cycle termination, demand vs exhaustive evaluation), then `-impl.md`. Consumer: wile-goast interprocedural/field-sensitive context-sensitive analysis.

#### Tier B — Two-sided matching (Roth-Sotomayor)

- [x] **`(wile algebra matching)` library** [Algebra, Matching, Done]: Two-sided matching per Roth & Sotomayor (1990). Gale-Shapley deferred acceptance (proposer + receiver optimal), hospital/intern many-to-one via Roth's reduction, Conway distributive lattice on stable matchings via Birkhoff (load-tests §5.5), Irving rotations enumeration, egalitarian + sex-equal selectors. Many-to-many (Kelso-Crawford) deferred to follow-up gated on §5.7 matroids (`plans/2026-05-02-algebra-matching-many-to-many.md`). `plans/2026-05-02-algebra-matching-design.md`, `plans/2026-05-02-algebra-matching-impl.md`.
- [x] **§4.2 Tropical permanent / Hungarian primitive** [Algebra, Matching, Done]: `tropical-assignment` shipped in `(wile algebra matching)` — Kuhn-Munkres O(n³) Jonker-Volgenant 1987 form. Returns `(matching . cost)`. Forbidden pairs via `+inf.0`. Unequal sides via padding. Sanity-checked on a 4×4 textbook fixture against brute-force optimum.
- [ ] **§4.2 Maximum common subgraph** [Algebra, Matching]: True code clone detection — bipartite matching between candidate node pairs, branch-and-bound with assignment relaxation. Overlaps §5.6 combinatorial-graph. `plans/2026-04-17-algebra-foundations-directions.md` §4.2.

#### Tier C — §5.7 lower priority

> **Demand note (2026-06-05):** a wile ↔ wile-goast TODO cross-check found **none** of the items below currently have a named wile-goast consumer — the "consumers" cited in each entry (register allocation, `ast-transform` formalization, etc.) appear nowhere in wile-goast's actual TODO. These are completeness-driven, not demand-driven. **CFL-reachability — the one item that had a real wile-goast consumer — has since shipped (`(wile algebra cfl)`)**, so there is currently **no** demand-justified open algebra item. Re-validate demand against `wile-goast/TODO.md` before promoting any item here.

- [ ] **§5.7 Matroids** [Algebra, Low]: `(wile algebra matroid)` — rank function, circuits, duality, Tutte polynomial, matroid intersection. ~300 LOC. Blocks Kelso-Crawford substitutes for many-to-many matching; also unlocks matroid-intersection framing of register allocation and scheduling. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Integer partitions & Young's lattice** [Algebra, Low]: `(wile algebra partition)` — `partitions-of`, conjugate partition, dominance order, Young's lattice as a poset. ~150 LOC. Natural addition given `order.sld`. `plans/2026-04-17-algebra-foundations-directions.md` §2.6 + §5.7.
- [ ] **§5.7 Category theory extensions** [Algebra, Low]: Functors, natural transformations, general adjunctions beyond `galois.sld`'s Galois-connection special case. Formalizes abstract-interpretation composition (Cousot & Cousot 1977). ~400 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Connes-Kreimer Hopf algebra on rooted trees** [Algebra, Low]: Coproduct cuts subtrees — matches `ast-transform`/`ast-splice` primitive operation in wile-goast's `utils.scm`. Formalizes rewrite-rule composition. ~300 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Submodular optimization** [Algebra, Low]: Greedy approximation framework. Applies to program slicing, test-suite selection, import minimization (submodular-maximization-under-cardinality). ~200 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Symmetric functions / RSK** [Algebra, Research, Low]: Research-tier. Small consumer: LCS→LIS→RSK connection for statement/parameter-list diff in `unify.scm`. ~500 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.

#### Follow-ups (deferred from shipped plans)

- [ ] **wile-goast AC-match migration** [Algebra, Follow-up]: Migrate `wile-goast/.../unify.scm:421` from `discover-equivalences` to `ac-unify`. Three risks: (1) term-protocol contract compliance, (2) trace-emitting diagnostic paths (`ac-unify` produces no rewrite trace), (3) small-arity benchmark before crossover claim. Scope ~100 LOC. `plans/2026-04-21-wile-goast-ac-match-migration.md`.
- [ ] **AC-matching v2 deferred decisions** [Algebra, Follow-up]: 8 decisions deferred in `plans/2026-04-21-ac-matching-design.md` "Open questions" — non-unit-multiplicity Stickel, sort-typed pattern-vars, E-matching scope. Re-open when a consumer surfaces.
- [ ] **Incidence algebra future extensions** [Algebra, Follow-up]: Items in `plans/2026-04-21-incidence-algebra-impl.md` "Future extensions (deferred)" section.

> Explicitly excluded as Part 7 non-goals in `plans/2026-04-17-algebra-foundations-directions.md` (no prospective consumer; documented here so the exclusion is visible rather than mistaken for oversight): tropical algebraic geometry, simplicial complexes / persistent homology, vector spaces as algebraic objects, holographic algorithms / Pfaffians, spectral graph matching, symmetric-function machinery beyond the LIS connection already tracked above.

---

## Tier 3 — Tooling & Developer Experience

- [ ] **Scheme linter** [Tooling, High, Needs Scoping]: Static analysis for Wile Scheme code — catch "plausible but wrong" before execution. Potential checks: unused bindings, arity mismatches, type mismatches, unreachable code, style warnings. Research needed: what do Racket (Check Syntax), Guile, CHICKEN lint tools actually check? How much at expand time vs separate pass? Interaction with type system is a key design question.
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md`
- [x] **Scheme-side line coverage** [Tooling, M, Done]: Shipped and merged to master — `WithCoverage(*coverage.Collector)` engine option (`options.go:443`), `pkg/coverage/` package, `--cover PATH` + `--cover-stdlib` CLI flags (`cmd/wile/main.go:56-57`), Go cover v1 output consumable by `go tool cover -html`, end-to-end `cmd/wile/cover_integration_test.go`. Docs: `docs/coverage/scheme-coverage.md`. `plans/2026-04-18-scheme-line-coverage.md`
- [ ] **Source file tracking in Syntax Objects** [Tooling]: Utilities for finding source locations and providing source lines.
- [ ] **`make doclint` target** [Tooling, S]: Extract `foo.go:N` citations from `docs/**/*.md` and `plans/**/*.md`; assert each file exists and `N` is within `wc -l file`. Cheap version catches the bulk of drift. Existing `check-readme-links.sh` only validates markdown link targets, not prose citations. Past multi-commit doc sweeps (PRs #707, #710, #711, #712, #713) are evidence the check would pay for itself. Stronger form would `go/ast`-parse the cited line and verify the enclosing decl name matches a nearby identifier in the doc.
- [ ] **`make planlint` target** [Tooling, S]: Flag plan files whose header status is stale. A plan's status lives in two places — its own `**Status:**`/`status =` header and the central `plans/CLAUDE.md` index — and only the central one is on the post-merge checklist, so per-file headers rot. Cheap version: for each `plans/*.md` whose header matches `not started|design only|design draft|ready to implement|pending`, extract any cited `PR #N` / `#N` and assert it is *not* merged (`gh pr view N`); a merged PR under a "not started" header is the drift signal. Evidence the check pays for itself: a 2026-06-05 audit found **10** plan headers claiming not-done for work merged on master (interval-dataflow-widening, sat-solver, numeric-registry, values-SR, approximate-counting-semirings, bignum-allocation-reduction, algebra-docs). Stronger form: cross-check each header against its `plans/CLAUDE.md` row and flag mismatches. Companion to `make doclint` above. `1` lone candidate left unresolved by that audit: `2026-04-20-axis-b-annotation-bugs` (cleanup-shipped claim unverifiable from git).
- [ ] **POSIX API / SRFI-170 remaining phases** [Standard library, 9 phases]: Phases 2-10 not started. Phase 1 (directory ops + process extension) completed in PR #565.
- [ ] **REPL tab completion still offers macro-introduced binders** [Tooling + hygiene, S, 2026-07-19]: `Namespace.BoundSymbolNames` (`pkg/environment/namespace.go:315`) now lists only names resolvable under the ambient scope set, via `GlobalEnvironmentFrame.AmbientKeys` (`global_environment_frame.go:267`). The completion path was deliberately left on the unfiltered walk — `Completer.collectBindingNames` (`pkg/repl/completer.go:83`) → `Engine.BoundNames` (`pkg/wile/engine.go:842`) → `Namespace.BoundNamesAcrossPhases` (`namespace.go:342`), which ranges `global.Keys()` at `:353`. The two listings now disagree, and completion can still offer a name that resolves to nothing. **Why it was not filtered alongside:** `BoundNamesAcrossPhases` also walks the expand and compile phase frames, where `define-syntax` keywords live (`compile_define_syntax.go:91`), so an ambient filter would drop any keyword whose binder carries a non-empty scope set. `ee918fd1`'s message states a top-level user binder carries the empty set, which suggests keywords survive — but that is read off a commit message, not measured, and library-defined + imported macros are unchecked. **Measure first:** apply the filter, diff the completion list before/after on a KitchenSink engine; missing macro keywords (`when`, `unless`, stdlib forms) is the disqualifying signal. Not at risk: `let-syntax`/`letrec-syntax` keywords are local bindings (`expander_let_syntax.go:137`), never in `Keys()`. Same read-path family as A1 above, which fixed the sealed-base half of this walk.

---

## Tier 4 — Performance

- [ ] **`resolveGlobal` re-locks one frame once per lexical depth** [Performance + structure, S for the guard / L for the carve, 2026-07-19]: `NewEnvironmentFrameWithParent` (`pkg/environment/environment_frame.go:152`) sets `global: parent.global`, so every lexically-nested frame shares one `*GlobalEnvironmentFrame`. But `resolveGlobal` (`environment_frame.go:498`) walks the **EnvironmentFrame** chain, taking `ge.global.mu.RLock()` and running `bestSlotLocked` at *every* hop. A 12-deep closure nest therefore does 12 RLocks and 12 map lookups to answer a question with at most 2 distinct answers — only a hop where `ge.global` actually changes can differ. **Inferred from those two lines; UNMEASURED.** Measure before acting (per `memory/`: profile end-to-end, micro-benchmarks mislead here).

  **Cheap lever:** track the previous `*GlobalEnvironmentFrame` in the walk and skip a hop whose global pointer is unchanged. No structural or semantic change.

  **Structural lever (the real fix, deferred):** give `GlobalEnvironmentFrame` its own parent so the global chain is walked directly and the EnvironmentFrame walk for globals disappears. Today `EnvironmentFrame` owns the local chain *and*, transitively, the global chain; splitting them is a separation of concerns, not a duplicated chain. **This is a migration, not a field addition:** the parent relation is *computed*, not static — `phase_registry.go:142` derives it via `SealedBaseTarget()`, which routes to the frozen sealed base for the layered main namespace (the hermeticity cut) but to the frame itself for a flat `NewChildRuntime` library frame (`namespace.go:976`). Moving that decision into global-frame construction goes through the hermetic-phases work. Distinct globals in a chain today: `wireRuntimeFrames` builds `sealedBase(sealedGlobal) ← runtime(mutableGlobal)` (`namespace.go:1063-1073`), and each phase frame owns one parented to `SealedBaseTarget()`.

  **Rejected alternative:** make `GlobalEnvironmentFrame` satisfy an `EnvironmentFrame` interface with `Parent()` always nil. There is no such interface — `EnvironmentFrame` is a struct (`environment_frame.go:95`) — so this means introducing one and putting dynamic dispatch on the VM's hottest path, against this codebase's measured preference for switch over table dispatch. It is also false to the structure: there are genuinely ≥2 ordered global frames, and a permanently-nil parent erases the sealed-base shadowing the layered carve exists to provide. Neighborhood is flagged in `memory/` (cross-engine sealed-base sharing SHELVED at D4).

- [ ] **Benchmark + profile the cycle-detection and context-poll cost added to `Pair.ForEach`** [Performance, S, UNMEASURED — crosscheck `15b68433..HEAD`, 2026-07-14]: `Pair.ForEach` (`pkg/values/pair.go`) gained Brent's cycle detection (pointer compare + increment + branch, plus a power-of-two checkpoint teleport) **and** an amortized `ctx.Err()` poll, on every walk. The correctness win is real and not in question — it closed the unbounded walk that let `(apply + circular-list)` grow the eval stack past every configured limit. What is in question is the cost, because **this is *the* list walker**: the code's own comment names the blast radius (`ForEachProperList`, `length`, `list-copy`, `append`, `reverse`, and apply's argument spread all funnel through it). No benchmark evidence was produced with the change.
  Work: A/B `make bench-gabriel` and `make bench-extended` across the commit, and profile a list-heavy workload end-to-end (`wile --cpuprofile`). Per `memory/`: micro-benchmarks mislead here — profile end-to-end, and do it *before* deciding anything. If the cost is material, the levers are (a) hoist the cycle check behind a length threshold so short lists (the common case) pay nothing, or (b) split a `ForEachUnchecked` for callers that have already established properness. Do not pre-emptively optimize: measure first, and record the numbers here either way so the next person does not re-ask.

- [ ] **Shrink `Binding` to recover the D2 atomicCell regression** [Performance, M — PUNTED 2026-07-06: too complex for the payoff, don't pick up without new evidence]: The D2 race fix (commit `fbcd7654`) grew `Binding` 32→40B (heap `atomicCell` pointer), inflating the value-embedded local frame slabs (`[]Binding`) and costing **+4.6% geomean on bench-gabriel (15/16 slower)**. Recovery lever: shrink `Binding` back so the local-frame slab footprint returns to baseline while globals keep the atomic cell (e.g. move rarely-used fields off the hot struct, or split local vs global binding representations). Gate on re-running bench-gabriel to confirm the recovery. Pure-perf follow-up; correctness is already banked. **Punt rationale (2026-07-06 analysis):** the recoverable win is *capped at the slab half* of the 4.6% — the other half is the global-read pointer hops (`*Binding → cell → atomic.Load → deref boxed value`), intrinsic to atomically publishing a 2-word `values.Value` and unrecoverable without 1-word NaN-boxing (separate `unsafe`-blocked plan). And getting the local slab to 32B soundly is not cheap: `value`/`cell` are a mutually-exclusive union but Go has no unions; pointer-tagging `bindingType`/`cell` low bits is GC-unsafe; `bindingType int`→`uint8` alone pads back to 40. The only sound mechanism is a cross-package `LocalBinding`/`*Binding` type split (ripples `environment/` + `internal/validate/` + `machine/pool`), whose natural unifier (an interface) adds dispatch to the Apply hot path and can eat the gain. Better lever for the same 4.6% is MORE frame reclamation (remove the slab allocation entirely so its size stops mattering) — but that arc is itself PAUSED (see `plans/2026-06-18-frame-reclaim-precision-coverage.local.md`: value core A/E shipped, tails B/C/D/F/G stopped under "limited payoff is a valid stop", resume gated on a real workload showing frame-leak pressure). **Resume this only if a representative embedding workload profiles as local-slab-allocation-bound AND the type-split measures net-positive on bench-gabriel.**
- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.md`
- [ ] **B3 effective capture refinement** [Performance, Research]: Propagate B2 escape results back into B1 capture status. A binding marked `Captured` by B1 is effectively non-captured if every lambda that references it is stored in a non-escaping binding (B2). Cross-binding analysis over B1+B2 results.
- [x] **`PrimitiveSpec` capture-safety capability field** [Performance, M, Done — PR #776]: Shipped as `PrimitiveSpec.InvokesProcedure` (`pkg/registry/apply.go`); each primitive self-declares, and the classifier stamps `Binding.CaptureSafe = !spec.InvokesProcedure` (`apply.go:240`) — extension primitives self-cover, no central list. The hand-maintained `captureSafePrimitiveNames` whitelist was retired (`frame_reclaim_build_test.go:58`). Below: original scoping. The escape-frame classifier's Layer C (`internal/validate/frame_reclaim_build.go`) decides whether a *primitive* callee is capture-safe via a hand-maintained, sound-by-default name whitelist (`captureSafePrimitiveNames` — `+`, `cons`, `<`, … contribute no edge; unlisted ⇒ unsafe). A primitive is capture-*unsafe* iff it can invoke a Scheme procedure that captures (`apply`, `map`, `for-each`, `call-with-values`, `dynamic-wind`, `with-exception-handler`, `sort`, …). The principled replacement is a `PrimitiveSpec.InvokesProcedure` (or `CapturesContinuation`) capability field so each primitive — including extension primitives — self-declares, instead of a central name list that silently under-covers extensions. **Gate:** only worth building once the sibling escape-gated plan's Phase 2 measurement shows the classifier's precision matters (the whitelist is sound regardless; this is a coverage/scalability dial). Weight false-positives (declaring a capturing primitive safe) as unacceptable per `feedback_annotation_stability.md`. Q-1 of `plans/2026-06-12-escape-frame-validation-impl.md`.
- [x] **`markCaptured`/`.Captured` is dead code — delete or unify** [Tech debt, S, follow-up to escape-frame-validation, Done]: Resolved via option (a) — deleted `markCapturedBindings` (`internal/validate/validate_capture.go`), the `ValidatedLetBinding.Captured` field, its 5 call sites in `validate_let.go`, and `validate_capture_test.go`. Shared test helpers from that file (`call`/`symRef`/`lam`/`makeTestEnvAndBindings`/… still used by sibling test files) were relocated to `internal/validate/sharedtest_test.go`. The live `markEscaped`/`.Escapes` path (read at `compile_let.go:240` for let-lambda inlining) and the unrelated live `bodyReferencesCaptureOperator` (call/cc detection, read by `frame_reclaim_build.go`) were left untouched. Unification (option b) was rejected: with B1/B3 never built there is no second consumer to unify against, and folding a fail-safe predicate in with the best-effort one risked regressing the inlining contract.
- [ ] **Benchmark coverage gaps** [Performance, S-M]: No benchmarks for compiler, expander (syntax-rules expansion), library import resolution, or continuation capture/restore cycle. Existing benchmarks cover VM dispatch, fibonacci, tokenizer, parser, environment, and symbol interning.
- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.md`
- [ ] **Inline-budget guard for `checkStackSize` and similar hot-path wrappers** [Performance, S]: `checkStackSize` (`machine/machine_context.go:1185`) is split from `reportStackOverflow` specifically to stay under Go's 80-cost inline budget (currently 67). A future innocuous edit could push it over and silently regress the VM hot path (the Gabriel suite would catch it, but only post-hoc and noisily). Write a test that runs `go build -gcflags='-m=2' ./machine/` and asserts `"can inline (*MachineContext).checkStackSize"` appears in the output. ~30 LOC test infrastructure; reusable for future hot-path wrappers. Surfaced by Finding 5 / PR #734 type-design review.

---

## Tier 5 — Tech Debt

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
- [x] **`ChannelSelect` is complete, tested, CHANGELOG-cited — and registered nowhere; half-migrated to the old lifecycle** [API/dead-code, S–M, Done]: **Resolved via (A) — deleted.** Removed `ChannelSelect`, `SelectCase`, `SelectCaseKind` + its 3 constants, and `firstDeadCase` (`pkg/values/channel.go`, 124 lines — the whole tail), the now-unused `reflect` import, and the 8 `TestChannelSelect*` functions (`channel_test.go`, ~188 lines, about half the file). ~312 lines total; no consumer anywhere, so nothing else changed. Three corrections to the framing below, found while scoping it: (1) it was **exported from `values/`, a public embedding package** — reachable from Go even though no Scheme program could call it, so this is a public API removal, not internal cleanup, taken under the zero-consumer rule; (2) wiring it would have needed a **ctx arm, not just `done` arms** — `ChannelSelect` took no `context.Context` while `Send`/`Receive` both do, so exposing it as-is would have reintroduced T1.3 at a new site; a throwaway prototype confirmed 2N+1 arms (data + `done` per channel, one ctx) work, `-race` clean, so the decision was never technical; (3) **the CHANGELOG line was not deleted** — 1.18.0 and 1.3.0 are released sections and deleting from them rewrites shipped history; a removal note plus a correction went in `[Unreleased]` instead. That 1.18.0 entry announced `channel-select` as a Scheme primitive that never existed, which needed correcting either way. If a `channel-select` consumer ever appears, note `reflect.Select` panics past 65536 cases and the list would come from Scheme, so it needs an arity guard. Original scoping below.
- [ ] **~~`ChannelSelect` dead code~~ (original scoping, retained for context)** [API/dead-code, S–M]: No `PrimitiveSpec` names `values.ChannelSelect` (`pkg/values/channel.go`); only tests reference it. It builds `reflect.SelectCase` on the never-closed data channel `ch` directly, so a peer closing a channel mid-block is invisible to a blocked `ChannelSelect` — its own doc admits this and says the fix is to add each channel's `done` arm to the `reflect.Select` set, exactly what the lifecycle rewrite added to `Send`/`Receive` but not here. **Possible actions:** (A) delete it (plus its tests + the CHANGELOG line); (B) wire it as `channel-select`, first adding a `<-done` reflect case per channel so it observes closure mid-block; (C) leave + annotate with an explicit "do-not-expose-until-done-arm-added" marker. **Recommendation: A (delete)** unless `channel-select` has a concrete near-term consumer; choose **B** only if it is actually on the roadmap, and then the done-arm migration is mandatory. Reject C (latent-buggy dead code is what the rubric already flagged). Gated on the open question "is `channel-select` on the roadmap?".
- [ ] **Stale sub-context comment on `with-timeout`** [Docs, XS]: `PrimWithTimeout` (`pkg/registry/core/prim_timer.go`) header says "The sub-context pattern ... a fresh sub-context isolates the thunk's execution," but the same function's body twelve lines down (and `RunBodyUnderTimer`) says it runs the thunk **INLINE on the live chain, not in a sub-context** (the accurate description). REVIEW.md lists stale comments as a recurring trap; this one misdescribes the isolation model of the code that makes the `with-timeout` cancellation path safe. **Possible actions:** (A) delete the stale sub-context sentence; (B) rewrite it to match the inline model. **Recommendation: A (delete)** — the accurate description already exists in the same comment, so B just duplicates it.
- [x] **Tests validate the Go layer, not the Scheme integration that makes it safe** [Test-coverage, S, Done]: Shipped A+B+C. `extensions/gointerop/channel_cancellation_test.go`: `TestWithTimeoutInterruptsParkedReceive` (A — asserts the handler value; mutation-verified: disabling the eager `ErrTimerExpired` recheck in `callForeignCached` makes it fail with the laundered `Void`) and `TestTerminateUnparksBlockedThread` (B — `thread-join!` doubles as the goroutine-exit handshake, since `Thread.Join` blocks on the `done` channel closed by `Start`'s last-run defer; mutation-verified: disabling `Receive`'s ctx arm makes it fail with `JoinTimeoutException`). Go-level tests retained (C). **Writing B found a real defect** — see the `Terminate` item below. Original scoping preserved below.
- [x] **`thread-terminate!` discarded its own SRFI-18 end-exception** [Correctness, S, Done]: Found while writing the test item above. `Thread.Terminate` stored a `TerminatedThreadException` (`pkg/values/thread.go`) and `Thread.Start`'s goroutine then unconditionally overwrote it when the thunk returned; `defer close(p.done)` is registered first and so runs last, guaranteeing the overwrite landed before any joiner was released. A started thread's joiner could therefore *never* observe the exception. Worst case: a thread parked in `channel-receive` in **tail position** of its thunk — the cancelled receive's laundered `Void` (Option A) returned as the thunk's ordinary result with no VM op following it to trip the top-of-loop ctx check, so `(thread-terminate! t)` + `(thread-join! t)` reported a terminated thread as having *succeeded* with `Void`. Not a regression (pre-`fix/channel-lifecycle-ctx` that thread hung forever), and it invalidated the `thread-terminate!` row of the design doc's three-sources table, which argued the ≈1024-op unwind window was itself the protection. **Fix:** the outcome is now write-once (`Thread.setOutcome`/`setOutcomeLocked`) — first writer wins, so `Terminate` beats the completion path, while SRFI-18's "if the thread is not already terminated" clause still holds (a completed thread keeps its result). Existing coverage was vacuous: `prim_threads_test.go`'s "terminate thread" case never started the thread and `prim_thread_test.go`'s `TestThreadTerminate` never joined — both asserted the `#t` literal they wrote. Now guarded channel-free by `extensions/threads/prim_threads_terminate_outcome_test.go`.
- [x] **`thread-join!` on a terminated but never-started thread blocks forever** [Correctness, S, Done]: Found while probing the SRFI-18 surface for the `Terminate` fix above; was pre-existing. `Thread.Terminate` on a `ThreadNew` thread stored the terminated-thread exception and set `state = ThreadTerminated`, but `done` is closed only by the goroutine `Thread.Start` spawns — which never ran, and never could (`Start` rejects `state != ThreadNew`). So `Thread.Join(nil)` parked on `<-p.done` forever while the exception the joiner wanted sat in the outcome field; with a timeout it raised `JoinTimeoutException` instead. The exception was never the missing piece, only the `done` signal. **Fixed via (A)**: `Terminate` closes `done` when it is the one ending a `ThreadNew` thread. The two closers are mutually exclusive because `Start` makes the `ThreadNew → ThreadRunnable` transition under `p.mu` before spawning and refuses any other state — so `done` closes exactly once, without a `sync.Once`. That mattered: a double close is a fatal host panic, the same hazard class as the `channel-send!` TOCTOU. Guarded by `pkg/values/thread_lifecycle_test.go` (20000-trial `-race` no-double-close race with a starting gate, plus a deterministic test per arm) and `TestThreadTerminateNeverStartedThreadIsJoinable`, which joins with *no* timeout so the regression is an unbounded park rather than a misleading `JoinTimeoutException`.
- [ ] **~~Tests validate the Go layer~~ (original scoping, retained for context)** [Test-coverage, S]: `pkg/values/channel_lifecycle_test.go` drives cancellation with a raw `context.WithCancel` at the Go layer; nothing exercises `thread-terminate!` or `with-timeout` reaching a parked channel op — the actual integration this branch enables. In particular the `with-timeout ∘ channel-receive` path is safe *only* because of the eager `ErrTimerExpired` recheck in `callForeignCached`; no channel or timer test would fail if that recheck regressed. Also, per the concurrency rubric, no test starts a real SRFI-18 thread that blocks in a cross-thread rendezvous and is then terminated. **Possible actions (do all three):** (A) add a Scheme-level `(with-timeout T handler (lambda () (channel-receive empty-ch)))` test asserting the handler value, not `Void` — the regression guard for the eager recheck; (B) add a cross-thread rendezvous-under-terminate test (park a thread in `channel-receive`, `thread-terminate!`, assert the goroutine exits and the joiner sees `TerminatedThreadException`); (C) keep the existing Go-level cancellation tests (they still prove `Send`/`Receive` honor a raw ctx). **Recommendation: A + B + C** — A/B are additive coverage of the SRFI-18/timer wiring the Go-level tests can't reach; C is retained, not replaced. **Sequence these FIRST**, before touching the comment/seam items above, to lock in current-correct behavior.

**Two decisions gate the deferred sub-items:** *is Option A a committed contract?* (drives item 1's B-vs-C) and *is `channel-select` on the roadmap?* (drives item 2's A-vs-B). Everything else (comment fixes + the tests) is safe to do now regardless. Suggested order: tests → delete stale comment → tighten seam rationale → resolve `ChannelSelect` → resolve the seam.

### List/Pair Primitive Cleanup (from inline annotations)

- [x] **List/pair primitive cleanup notes** [Low, XS–S, Done]: Relocated from inline `// CLAUDE:` source annotations (removed to keep the axis-B manifest stable — inline comments shift primitive line numbers and break `TestBuildAxisBManifest`). Four items:
  - [x] **`Pair.Append` removal** (`pkg/values/pair.go`, `(*Pair).Append`): **removed.** Confirmed dead — zero production callers (the only `.Append(` in non-test code was `reflect.Append`); superseded by the Scheme `append` primitive (`PrimAppend`), which builds its own spine. Deleted the `Tuple.Append` interface method, both implementers (`*Pair`, `emptyListType`), and the 6 `*_Append*` unit tests.
  - [x] **`PrimReverse` allocation** (`pkg/registry/core/prim_lists.go`, `PrimReverse`): already `PairBlock` (count pass + back-to-front fill).
  - [x] **`PrimListCopy` allocation** (`pkg/registry/core/prim_lists.go`, `PrimListCopy`): **converted to `PairBlock`.** Reversed the earlier tail-pointer choice — `Tuple.ForEach` gives both the count and the terminating cdr, so an improper tail is preserved by re-pointing the block's last cdr after `LinkSpine`; the tail-pointer form was not load-bearing. Measured: 50-element copy 63→18 allocs / ~10% faster (O(N) cons → 1 block). CLAUDE.local.md allocation guidance updated to move list-copy to the `PairBlock` example and keep `PrimAppend` as the sole tail-pointer exemplar (multi-arg concat genuinely can't pre-count).
  - [x] **`PrimListCopy` loop shape** (`pkg/registry/core/prim_lists.go`, `PrimListCopy`): explicit `for` spine-walk replaced by two `Tuple.ForEach` passes (mirrors `PrimList`/`PrimReverse`).

### FCA-Derived

- [x] **Structural reduction roadmap** [Top priority, planning-only, Done — closed 2026-07-08]: Selects the next packages to subject to `/structural-reduction` and **gates** the implementation plans below. Tier A targets in priority order: `values/` (Ca=33, 11K LOC, numeric tower + port hierarchy), `environment/` (Ca=16, binding-resolution algebra, recent namespace migration seams), `registry/` (Ca=19, contract surface for ~500 primitives). Tier B: root `wile/` (API design quality), `repl/`, `registry/helpers/`. Tier C uses different lenses (`scheme-conformance` for `registry/core`, `extensions/math`; `signals-engineer` for `security/`; batch `staff-engineer` sweep for `extensions/{eval,files,threads,gointerop,charsets,system,process,introspection}`). Why gating: `internal/` Phase 7 references `values/` as migration precedent; `machine/` Phase 7 boundaries depend on `environment/` frame shape. Run Tier A analyses BEFORE implementing the plans below. `memory/2026-05-07-structural-reduction-roadmap.local.md` (moved to `memory/` on close). **Tier A status (2026-05-13)**: A.2 (`environment/`) shipped via PR #730 (`memory/2026-05-09-environment-structural-reduction.md`). A.3 (`registry/`) cross-cutting findings consolidated in `plans/2026-05-08-dispatch-axis-as-data.md` — Phase unification shipped (PR #728); remaining instances feed per-package plans. **A.1 (`values/`) complete — Phases 0–4 shipped via PRs #747–#756 (`memory/2026-05-13-values-structural-reduction.md`). Tier A closed.** **Tier B closed**: B.1 `wile/` (PR #764); B.2 `repl/` + B.3 `registry/helpers/` SR passes run 2026-07-08 (branch `refactor/structural-reduction-b2-b3` — one state-tightness fix, dead-code removal, and a dispatch drift-guard test; low yield as predicted, most candidate findings refuted/declined as churn). **Tier C** batch reassigned to `2026-07-01-staff-engineer-sweep.md`. Roadmap spent — closed and archived to `memory/`.**
- [x] **vmCore sub-struct extraction** [High, M, Considered and declined on re-evaluation (2026-06-05)]: Original proposal — extract always-transfer fields (env, template, pc, callDepth) into a sub-struct so 4 hand-copied assignments collapse to 1 struct assignment at each of 6 copy sites. **Declined** after reading all transfer sites (`machine_context_continuation.go`, `machine_continuation.go`, `machine_context.go`). Four reasons: (1) **Field list wrong** — `callDepth` is not an always-transfer datum but a *guarded maintained counter*: `SaveContinuation` `++` (maxCallDepth guard), `PopContinuation` `--` (underflow guard), both continuation constructors compute `depth` from the parent pointer. It transfers verbatim at only 3 of 8 sites; bundling it forces override-after-copy at 4 sites and risks clobbering its guards. (2) **Wrong target** — the genuine always-transfer triple is just `{env, template, pc}`, but those are the *trivial* fields. The FCA assessment's own "High" rating rests on the divergent fields (`evals` 4 ownership modes, `envPooled` 4 behaviors, `marks` clone-vs-direct), none of which a `vmCore` of always-transfer fields touches. (3) **Safety net already exists** — the FCA's "no compile-time guard when fields are added" concern is already answered by `testVmStateFieldCoverage` (`machine/vm_state_test.go:277`), which fails the build if any `vmState` field is missing from any operation's coverage table. (4) **Cost/benefit** — net ~6 lines saved at 3 sites (`Restore`/`RestoreAndRelease`/`Copy`) on the hottest path in the VM (per-call/per-return), requiring a doc-table reorg and bench gate; no realistic future always-transfer CESK register exists to amortize it. Parallels the prior decline of machine SR Finding 7 Stage 3 (sub-record extraction that didn't reduce real risk). See [FCA Assessment](#fca-assessment).
- [x] **Machine package structural reduction** [High, mixed S/M/L, Done] — **all 7 findings closed (2026-05-13).** 7 findings + 3 opportunities from `/structural-reduction ./machine` (2026-05-06). Status: (1) syntaxCase any → marker interface — **considered and declined** (PR #731); (2) maxStackSize → Stack.Push — **shipped** (PR #734, Finding 5); (3) maxCallDepth type unification — partially shipped (commit `7dc2511c`), sentinel-removal half **declined** (Finding 4); (4) Operation empty contract → OpKind() discriminator — **shipped** (PR #735, Finding 6); (5) singleValue/multiValues accessors → vmState consolidation + ruleguard — **shipped** (PR #736, Finding 3); (6) tail/non-tail opcode collapse via sign-bit encoding — **considered and declined** (PR #737, 2026-05-11; geomean +2.5% regression, all 16 benches slower; see `memory/finding2-collapse-revert.md`); (7) named sub-records for correlated `MachineContext` fields — **shipped at stages 1–2** (PR #742 expansion sub-record, PR #743 timer sub-record, PR #745 cumulative crosscheck followups); **Stage 3 declined** (field-independence analysis showed no co-variance between sub-context fields, commit `9382a3b3`). `plans/2026-05-06-machine-structural-reduction.md`
- [x] **Internal package structural reduction** [Medium-High, mixed XS/S/M, Done] — **all 7 findings shipped.** 7 findings + 4 opportunities from `/structural-reduction ./internal` (2026-05-07). Dependency graph is a clean DAG (0 cycles). Phasing: (7) delete dead `SyntaxObject.IsPair()`/`IsEmptyList()`, (4) extract `bindLocalSymbol`+`extendEnvWithSymbols`, (3) extract `detectDuplicateSymbols` fold, (2) extract `parseLetBindingPairs` helper, (6) collapse 4 `match.NewMatcher*` telescoping ctors → 1 + N options — all batched in **PR #739**. (5) generalize binding-reference walker (`WalkBindingRefs` higher-order traversal collapsing `markCaptured` + `markEscaped`) shipped in **PR #740**. (1) finish `*SyntaxPair`/`SyntaxEmptyList` empty-list duality migration (restores Chez-conformant `(equal? (syntax ()) '())`) shipped in **PR #741**. `plans/2026-05-07-internal-structural-reduction.md`
- [x] **Values package structural reduction** [High, mixed S/M/L, Done] — **Tier A.1 complete; Phases 0–4 shipped (PRs #747–#756).** 9 findings + 4 opportunities from `/structural-reduction ./values` (2026-05-13). Phase 0 quick wins (PR #747): `TypeExactInteger` alias deleted, `makeInterfaceCheck` folded into `makeCheck`, `goTypeToValueType` reverse map replaces the `SchemeTypeName` switch. Phase 1 (PR #748): mutex state tightness. Phase 2 **Port unification** (PR #749): ~900 LOC across 9 port types collapsed to one `*Port` struct with capability slots. Phase 3 **NumericTypeSpec registry** (PR #752): the 12-step ADDING-A-NEW-NUMERIC-TYPE guide collapses to one record; numeric loss signals follow-up (PRs #753–#756). Phase 4: `Datum()` deletion + `IsVoid` convention test (commits `d7112b0c`, `e93448c4`). Finding 1 (IsVoid convention) recast after design-intent review — original "delete 51 methods, use reflection" framing retracted; shipped as additive convention test + `allValueExemplars` roster. `memory/2026-05-13-values-structural-reduction.md`
- [x] **Environment package structural reduction** [Medium-High, mixed XS/S/M, Done — Phase 10 deferred] — **Phases 1–9 shipped (PR #730, 2026-05-10).** Closed Tier A.2 of the roadmap. 10 findings + 4 opportunities from `/structural-reduction ./environment` (2026-05-09). Findings 1, 2, 3, 4, 5, 6, 7, 8, 9 implemented (dead-code drops + `Namespace.root()` extraction + `bestOf[T]` reducer + `Binding` accessor collapse + 5 Namespace constructors → `NewChildNamespace` + options + `BindingTypeUnknown` documented + `EnvironmentFrame` delegation surface documented). Phase 10 (Finding 10 — `*LocalIndex` allocation audit across 40 sites; unboxed `slot, depth int` fast path already exists) **deferred — benchmark-gated** per the recommended phasing; re-open if a measured allocation win surfaces. `memory/2026-05-09-environment-structural-reduction.md`
- [ ] **Bidirectional opcode conversion test** [Medium, S]: Verify `operationToInstruction` and `instructionToOperation` cover the same opcode set.
- [ ] **LocalEnvironmentFrame pointer ambiguity** [Low, S]: Doc comment on `NewLocalEnvironment` explaining lifecycle (value-vs-pointer ownership).
- [ ] **Honor `WithInlineThreshold` for imported libraries** [Low, S]: The library import/load chain (`LoadLibrary` → `loadLibraryFromReader` → `compileAndExecuteLibrary`, `machine/compilation/library_loader.go:215,223`) has **no `inlineThreshold` parameter**, so imported libraries always compile at `DefaultInlineThreshold = 5`, ignoring the engine's `WithInlineThreshold(n)` (`pkg/wile/options.go:275`). Every *in-process* child compiler re-threads the parent's value via the two-line `NewCompileTimeContinuation(...)` + `SetInlineThreshold(p.inlineThreshold)` idiom (6 sites: `compile_syntax_case.go:253`, `compile_closure.go:123`, `compile_library_forms.go:109`, `compile_helpers.go:51`, `compile_time_continuation.go:347`, `expand_and_compile.go:53`); the load path is the one site that cannot reach the value. **Not a correctness bug** — inlining here is the behavior-preserving synthetic-let transform (PR #605), so results are unchanged; it is a config-honoring / debuggability inconsistency (disabling inlining, e.g. for predictable stack traces, is silently not honored across the `import` boundary). Fix: thread `inlineThreshold` through the three `LoadLibrary`/`loadLibraryFromReader`/`compileAndExecuteLibrary` signatures (or expose it via `Namespace`/`EngineServices` so the load path can read it) and `SetInlineThreshold` on the library compiler. Discovered during the `CompileTimeContinuation` God-object triage (2026-07-09); the fix also illustrates why the "stable config should be inherited, not hand-copied" refactor (staff sweep tail) has real payoff — a shared services pointer would close this gap by construction.
- [x] **Unified binding reference (`BindingRef`) for local+global** [Medium, M, Done — structural tidy, not the bug fix implied]: `BindingID{*LocalEnvironmentFrame, slot}` names only local bindings; `GlobalIndex{*Symbol, *GlobalEnvironmentFrame}` names only globals. The asymmetry forces every cross-cutting "name any binding" consumer to special-case the two — most visibly the validator's mutation set (`mutatedBindings map[BindingID]bool`, `validate_set.go`), which is structurally blind to top-level `set!` because `ResolveBindingID` walks `resolveLocal` only. This surfaced while implementing define-immutability constant tracing (`plans/2026-06-12-define-immutability-and-constant-tracing-impl.md`), where the in-unit-`set!` predicate for top-level defines had to fall back to symbol-key tracking precisely because no unified reference exists. **Do NOT fold the two storage structures** — they are tuned for opposite access patterns and folding regresses the VM hot path: locals are positionally-addressed (`LocalIndex{over,up}`, O(1) array index), copied every `Apply`, single-threaded, multi-slot-per-name (hygiene), `[]Binding` by value; globals are symbolically-addressed, shared across SRFI-18 threads (`sync.RWMutex`), `[]*Binding` pointer-stable (required by the lock-free `cachedBindings`/promoted-ops read cache — see `memory/global-binding-cache-already-exists.md`), multi-slot-per-name since `8afeb66a` (scope-keyed, like locals — this clause formerly read "single-slot" and was the one premise below that scope-keying invalidated; the other four still hold and the directive stands), deletable. The productive unification is the **reference/identity type, not the storage**: a `BindingRef` sum (local-or-global) resolvable at the `EnvironmentFrame` layer (which already unifies `GetBinding`/`ResolveBindingID`/`GetGlobalIndex` over both frames). Gate behind `/structural-reduction ./environment` measurement; verify by substitution that the validator and any other consumer can name either binding kind through one type. Sibling-of: line 188 (LocalEnvironmentFrame pointer lifecycle doc). **Done (2026-07-08, master `229e0b72`)**: `BindingRef` sum type + `ResolveBindingRef` added to `environment/`; the validator's mutation set collapsed from 3 maps (`mutatedBindings`/`mutatedKeys`/`definedKeyCount`) to 2. **Correction to the premise above**: on inspection the validator was NOT structurally blind to top-level `set!` — the symbolic `mutatedKeys` sidecar already compensated, and `StableInUnit` was correct. So this shipped as a **semantics-preserving structural tidy** (one "name any binding" type, hand-marked duplication removed), not a bug fix. The precise-vs-conservative split was found to be principled (two consumers: per-let-binding `Mutable` vs top-level `StableInUnit`), so storage stayed split as warned; only the reference type unified. The conservative over-mark (a `set!` to a local shadow still marks the top-level name non-stable) is the frame-reclaim soundness margin — now guarded by `TestStableInUnit_SetToLocalShadowStillMarksTopLevel` (verified non-tautological). `/structural-reduction ./environment` gate skipped: the two-consumer pattern was already located. Compiler-side sibling (`inlineCandidates` + `Sym.Key`) left as opportunistic follow-up.
- [x] **Unify `atan2Operand` with `helpers.ToFloat64`** [Low, S, Done]: PR #754 surfaced 3-lens convergence on a duplication. `extensions/math/prim_transcendental.go::atan2Operand` re-implemented the Number-assertion → ComplexNumber-rejection → float64-extraction sequence that `helpers.ToFloat64` performs, just to swap the loss-policy knob from "strict" to "silent truncate." **Resolved**: extracted shared `screenReal` screening in `registry/helpers/value_conv.go`; added `helpers.ToFloat64Lossy` (screening + `values.ToFloat64WithAccuracy` discard) as the lossy-policy counterpart to strict `ToFloat64`; deleted `atan2Operand` and routed both `PrimAtan` call sites through `helpers.ToFloat64Lossy`. Lossy semantics (`(atan 1/3)` etc.) preserved per R7RS §6.2.6.

### Tech Debt Plan (remaining)

- [ ] **Task 6.2: Replace `context.TODO()` in tests** [Low, S]: 431 occurrences across 39 test files. Mechanical `→ context.Background()`.
- [x] **Task 6.4: Add `typeswitchlint` to value type guide** [Low, S, Done — `a41ec0b7`]: Resolved by a stronger mechanism than the guide comment — `a41ec0b7` made `typeswitchlint` opt-in, CI-gating, and **drift-guarded** (`cmd/typeswitchlint/main_test.go`), so `knownValueTypes` diverging from the actual value-type set now fails CI mechanically rather than relying on a human reading a comment.
- [x] **Task 8.1: Extract `machine/compilation/resolver/`** [Done]: FileResolver implementations extracted. `LibraryEnumerator` replaced with `FileEnumerator.EnumerateFiles` (returns paths, not `LibraryName`). Type aliases in compilation for backward compat. `plans/2026-04-13-resolver-extraction-impl.md`
- [ ] **Task 8.2: Evaluate `wile.Value` wrapper** [Low, M]: Wrapper provides minimal methods beyond `Internal()` escape hatch.
- [ ] **Task 8.4: Make `DefaultBigFloatPrecision` configurable** [Low, M]: 256-bit precision hardcoded across 12 call sites. No engine option.
- [ ] **Error sentinel grouping** [Low, S]: ~109 sentinels in flat list. Consider category-specific files if count exceeds ~150.
- [ ] **Namespace registry typing** [Low, S]: Namespace's registry should have a type instead of `any`.
- [ ] **ValueType refactoring** [Low]: ValueType doesn't have grounding in Scheme or Go — determine use and scope of type domains.
- [ ] **Evaluate need for Primitive Annotation Enforcement** [Low]: Enforcement may not be needed.

### Algebra library consistency (2026-04-23 staff-engineer audit)

- [x] **Promote setoid collection helpers** [High, S, Done]: `setoid-member?`, `setoid-assoc`, `setoid-dedup` now public in `(wile algebra setoid)`; private `%`-copies deleted from `group.scm` and `combinatorial-graph.scm`. All call sites updated.
- [x] **Promote options-alist helpers** [High, S, Done]: `assv-or` and `validate-opts-keys` folded into `(wile algebra setoid)` as public helpers (noted as "plumbing, not setoid ops" with a section comment; move to a dedicated module later if a third category accumulates). Scope was larger than audit showed: duplication existed in FOUR libraries (group, combinatorial-graph, **incidence, lattice**). All four call sites updated; `incidence.sld` gained a `(wile algebra setoid)` import.
- [x] **Drift-check test for umbrella `algebra.sld`** [High, S, Done — option (c)]: `algebra_umbrella_drift_test.go` parses every leaf `.sld` export clause and asserts umbrella coverage. First run caught real drift (`rewrite.sld: associativity-axiom-op commutativity-axiom-op`; `semiring.sld: tropical-inf`) — added to umbrella. Deferred decisions (a) delete and (b) generate until test-driven drift frequency justifies either path.
- [x] **Documented convention for structure API** [Medium, S, Done — supersedes "extract with-X macro skeleton"]: Reviewing the audit's own premise: a `define-with-binder` meta-macro would save ~10 lines across 15 libraries at the cost of indirection. Not worth it. Replaced with `stdlib/lib/wile/algebra/CLAUDE.md` documenting the five-part structure API (`make-X` / `X?` / accessors / `with-X` / `validate-X`), the shared plumbing in `(wile algebra setoid)`, options-alist discipline, validator body shape, and the `with-X` skeleton. New structures and reviewers have an anchor; duplication stays mechanical.
- [x] **Extract `validate-X` violation-collector idiom** [Medium, S, Done]: `make-violation-reporter` added to `(wile algebra setoid)` — two-mode procedure (call with type + args to record, call with no args to finalize). Retrofitted every `validate-X` across 14 libraries (setoid, monoid, group, ring, field, semiring, lattice + /distributive + /modular, boolean, heyting, category, closure, differential, partial-order + /setoid, galois's gc-sound?, combinatorial-graph). Parent-validator delegation patterns (field→ring, boolean→lattice, heyting→lattice, partial-order/setoid→partial-order, differential→ring) now use `(for-each (lambda (v) (apply fail! v)) parent-result)` instead of `(set! violations (append ...))`.
- [x] **Resolve `assert-X` asymmetry** [Medium, S, Done — dissolved via generic helper]: Rather than add 18 `assert-X` symbols, added `assert-validation` as a syntax-rules macro in `(wile algebra setoid)`. `(assert-validation (validate-group G s))` raises if the result isn't `#t`, with the source expression preserved in the error datum. Existing `assert-group`/`assert-graph` kept as thin conveniences; new structures should use the generic helper. Net API-surface cost: +1 symbol instead of +18.
- [x] **Refactor `combinatorial-graph.scm` monolith — first cut** [Medium, M, Done (partial)]: Replaced `%list-sort`/`%insert` (custom insertion sort) with `list-sort` from `(srfi 132)` — 14 lines removed, new library import added. File is now 1,726 lines (down from 1,787 including the setoid/options helper promotion earlier this session). Remaining `%`-prefixed helpers (`%sig<`, `%key<`, `%refine-step`, `%nat-*`, `%backtrack-canonical`, etc.) are genuinely graph-specific or WL/isomorphism-specific — splitting into sub-files (`-isomorphism`, `-polynomials`, `-matching`) is deferred until that provides tangible review-scope benefit.
- [x] **Normalize `make-X` constructor validation discipline** [Low, S, Done]: Added `assert-procedure` macro to `(wile algebra setoid)` — uses `syntax-rules` to capture the source identifier, so `(assert-procedure "make-ring" plus)` raises `"make-ring: plus must be a procedure"` on non-procedure input. Retrofitted 11 non-validating constructors: `make-setoid`, `make-monoid`, `make-partial-order`, `make-closure-operator` (split into `make-X*` record-type ctor + wrapper), `make-semiring`, `make-ring`, `make-field`, `make-boolean-algebra`, `make-heyting-algebra`, `make-category`, `make-differential-ring`, `make-galois-connection`. The audit noted `make-lattice` as non-validating — it was already validating. Convention documented in `stdlib/lib/wile/algebra/CLAUDE.md` "Procedural-argument discipline for `make-X`" section.
- [ ] **Watch `matrix.scm` for split pressure** [Low, S, Deferred]: 1,302 lines with two record types (`<semiring-matrix>` at 839, `<sparse-semiring-matrix>` at 1137) in one file. Shared helpers justify co-location today. Revisit once a third representation (banded, symmetric, etc.) appears — no action needed now.
- [ ] **Harmonize `docs/algebra/reference.md` section template** [Low, M, Deferred; 2026-04-23 crosscheck consistency finding]: First 15 sections use a fixed 5-heading template (Constructors → Predicates → Operations → Validation → Destructuring). The 11 sections added in PR #706 (matrix, polynomial, incidence, interval, graph, combinatorial-graph, unification, fca, pareto, abstract-domain, dataflow) use bespoke headings because their library shapes don't match the 5-part structure pattern (e.g. dataflow has no "law checker"; unification has pattern-vars, substitutions, matching as three parallel concerns). Decision at the time: keep bespoke headings since forcing the template would obscure real structural differences. Revisit if either (a) the template gets extended to cover the new shapes cleanly, or (b) a reader reports navigation trouble across sections.
- [x] **Back-port legacy Sage validators to `check_or_snapshot`** [Low, M, Done 2026-06-09]: 5 of the 6 legacy structure validators (integer-ring, rational-field, modular-ring, boolean-semiring, tropical-semiring) now route through the shared `check_or_snapshot` helper and emit flat top-level `(test …)` fixtures, matching the 6 newer validators. `powerset-lattice` is intentionally exempt and stays hand-rolled (commented at the function): Wile's `lattice-join`/`lattice-meet` return sets in input-order, not a canonical order, so its live check compares the full set (order-insensitive) while its snapshot asserts only cardinality — a live/snapshot divergence the single-expression `check_or_snapshot` cannot express without weakening the membership check. The back-port added a `('num', token)` sentinel to `to_wile_display`/`to_wile_test_literal` for bare self-evaluating number literals, so rational-field now asserts exact rationals by `equal?` (`(test 1/3 (field-plus …))`) instead of string-matching `number->string`. Regenerated 5 fixtures under Sage 10.8 + a built binary; all pass.

### Helpers TypeName Encoding (PR #725 deferred items)

Items surfaced by /crosscheck adversarial review on PR #725 (helpers
typeName encoding refactor). Deferred per scope or design choice.

- [ ] **Distinct `*TypeSentinel` type for compile-time enforcement** [Tech debt, M, Deferred per Q1=A]: Type-design analyzer recommended splitting `*StaticError` into two types: `*StaticError` for non-type sentinels and `*TypeSentinel` for type-mismatch sentinels (embedding or wrapping `*StaticError`). Helpers like `RequireArg`/`RequireType` would take `*TypeSentinel` directly, making "passing a non-type sentinel to a type helper" a compile error. Current design uses runtime sum-as-struct discriminant (empty `expectedType` = non-type) plus `TestTypeSentinelsCarryTypeName` allowlist as the guard. Future cleanup once a real misuse incident motivates the rename across the codebase. See PR #725 review.
- [ ] **Store bare noun in `expectedType`, apply `articleFor` at format time** [Tech debt, S, Deferred]: Currently `NewTypeSentinel("string")` stores `expectedType: "a string"` (with article baked in). Type analyzer recommended storing `noun: "string"` and applying `articleFor` during `Error()`/`TypeName()`. Would let the article rule evolve (e.g., switch to phonetic) without regenerating sentinels, and would isolate the orthographic rule from the data. Pass-through irregulars ("a once") would need a separate `irregularArticle` field or override map.
- [ ] **`TypeNamer` interface for `typeNameFromSentinel`** [Tech debt, S, Deferred]: Currently `typeNameFromSentinel` matches on concrete `*werr.StaticError` via `errors.As`. Type analyzer recommended an open-extensible `interface { TypeName() string }` so any future error type could opt in. Trade-off: opens to accidental participation by unrelated types adding `TypeName() string`. Address when a second carrier of TypeName actually appears.
- [ ] **`Lengthable` rename to `IndexedSequence`** [Bikeshed, S, Deferred]: Type analyzer noted the helpers use the constraint as "indexed finite sequence" but the name `Lengthable` promises only `Length() int`. `*String`, `*Pair`, and `emptyListType` accidentally satisfy `Lengthable` but cannot meaningfully participate in `SequenceRef`/`SequenceSet`. Rename when the asymmetry causes real confusion.
- [ ] **Reflection-based `TestTypeSentinelsCarryTypeName`** [Test debt, S, Deferred]: Currently the inventory test enumerates ~55 type sentinels by hand. Test analyzer recommended a reflection-based variant that walks all exported `*StaticError` vars in `werr/` and asserts any whose `Error()` starts with `"not "` has a non-empty `TypeName()`. Self-maintaining, ~20 lines replacing ~60. Add when a contributor adds a new sentinel and forgets the inventory entry.
- [ ] **Extension-level message-content tests for new sentinels** [Test debt, M, Deferred]: Test analyzer flagged that no extension-level test asserts the user-visible "expected an integer/namespace/once" message content. Helper-level tests in `registry/helpers/args_test.go` pin the plumbing end-to-end through `TestRequireType_ErrorMessageContainsTypeName`, but a regression that, say, swaps `ErrNotAnInteger` back to `ErrNotANumber` in `make-vector` would not be caught by a test. Belt-and-suspenders coverage; add per primitive when message wording becomes load-bearing for users.
- [x] **`ParseOptionalStartEnd` / `ParseOptionalArg` literal phrases** [Tech debt, S, Done 2026-07-01 — comment path]: Silent-failure hunter flagged that these helpers hardcode "improper argument list" / "too many arguments" rather than reading from sentinels. These are *shape* errors (proper-list, arity), not type errors, so skipping TypeName plumbing is deliberate — but the file mixed two conventions with nothing explaining why. **Resolved via the comment path** (not migration): both `ParseOptionalStartEnd` and `ParseOptionalArg` doc comments in `pkg/registry/helpers/args.go` now state that shape errors carry literal phrases because there is no expected-type noun to plumb through a `*TypeSentinel`, while the per-argument type checks (start/end must be an integer) still draw their noun from a sentinel. Migration to a parallel mechanism was declined — shape errors genuinely have no type noun.
- [x] **`read-line` / `peek-char` `UnreadRune` errcheck** [Bug, S, Done 2026-05-06 — commit `460c73a5`]: `read-line`'s `UnreadRune` error (and the non-EOF error from its inner `\r` lookahead `ReadRune`) were dropped via `//nolint:errcheck` / silent fallthrough, masking I/O failures. **Fixed**: both are now captured via `WrapForeignReadErrorf` (`pkg/extensions/io/prim_read_write.go:388-397`); `io.EOF` after `\r` stays silent (line ends with a bare `\r`), any other error propagates as a read-error. Fault-injection test infra added: `pkg/internal/extensions/iotest/` (`FailingTextualInputPort` + `make-failing-unread-port`/`make-failing-read-after-port` primitives), asserted in `pkg/extensions/io/prim_read_error_test.go`. Was a surfaced-but-deferred item from PR #725.
- [x] **`peek-char` error classification** [Bug, S, Done 2026-05-06 — commit `460c73a5`]: `peek-char`'s `UnreadRune` (and read) failures were wrapped with `WrapForeignErrorf`, so `goErrorToSchemeException` classified them as `NativeErrorKindGeneric` and `(read-error? e)` returned `#f` — a direct R7RS §6.11 violation. **Fixed**: both sites now use `WrapForeignReadErrorf` (`pkg/extensions/io/prim_read_write.go:341,349`), so the condition satisfies `(read-error? e)`. Same commit as the `read-line` fix above.
- [x] **Library-binding installation swallows errors silently** [Bug, S, Done 2026-07-01]: two `SetOwnGlobalValue` return values in `machine/compilation/library_bindings.go` were discarded via `_ =` — the source-phase propagation branch in `CopyLibraryBindingsToEnvAtPhase` and the syntax-binding branch in `copyLibraryBindingsDirect`; a swallowed failure in the latter means a macro is silently not installed in the expand environment and later macro expansion mysteriously fails. The sibling base-phase installs already wrapped-and-returned — the asymmetry was "evolved separately." **Fixed**: both branches now wrap-and-return per the local convention. While there, added the requested `targetPhase + sourcePhase` int8-overflow guard (`Phase` is `int8`; a `for-meta` target of 127 — permitted by the parse-time `composePhaseShift` guard — plus a syntax binding's source-phase +1 wraps to −128 and silently misroutes the propagated binding). The sum is now checked at int width against `math.MaxInt8` before narrowing, mirroring `composePhaseShift`, returning `ErrInvalidArgument`. The `MaybeCreateOwnGlobalBinding` returns cited in the original note are no longer discarded (PR #793 wired the `created` bool into the conflict guard). Regression test `TestCopyLibraryBindingsPhaseOverflow` (constructs a syntax-only-export library, installs at phase 127, asserts the overflow diagnostic). `make lint` + `make covercheck` green. Pre-existing; surfaced by PR #728 crosscheck.

### Machine value-register follow-ups (PR #736 deferred items)

Items surfaced by /crosscheck on PR #736 (consolidate value-register
accessors on *vmState — Finding 3 of `plans/2026-05-06-machine-structural-reduction.md`).
Deferred per scope or design choice.

- [ ] **`SetValues(sub.GetValues()...)` nil-vs-empty ambiguity** [Tech debt, M, Deferred — pre-existing]: Silent-failure-hunter flagged 13 call sites that propagate a sub-context's value register into the parent via `mc.SetValues(sub.GetValues()...)`. `GetValues()` returns `nil` for an empty register (both fields nil); spreading `nil...` calls `SetValues()` with zero args, which now canonicalizes to (nil, nil) post-Q-e. Sub-contexts that exited abnormally without writing a value, sub-contexts that returned `(values)` (R7RS zero-value return), and sub-contexts that returned a real value all collapse into indistinguishable parent-side state. Call sites: `extensions/eval/prim_eval.go:104`, `extensions/files/prim_files.go:179`, `registry/core/prim_timer.go:127`, `registry/core/prim_barrier.go:72`, `registry/core/prim_cont_marks.go:187`, `registry/core/prim_prompt.go:135,149`, `registry/core/prim_control.go:87,200,365`, `registry/core/prim_exit.go:105`. Pre-existing; surfaced by but not introduced by PR #736. Fix shape: distinguish "no value produced" from "(values) zero-return" at each call site, or document the collapse as intentional R7RS behavior.

### Continuation vmState descriptor follow-ups (#1 Tier-1 shipped `834b2db7`)

Follow-ups from the staff-sweep #1 lever — `vmState` save/restore descriptor +
oracle. Tier-1 (descriptor + driven oracle + completeness ratchet across all six
save/restore/copy sites) shipped to master `834b2db7` with bodies unchanged.
Design: `memory/2026-07-02-continuation-vmstate-descriptor-oracle.md` (archived;
Decisions D-c, Option B). **Do NOT touch continuation method bodies without the red-suite +
A/B `/crosscheck` gate** (most-reverted neighborhood; `memory/tail-frame-recycling-unsound.md`,
`memory/c1-continuation-not-frame-reclaim.md`).

- [x] **Tier-2 — wile-goast capture-site shared-invariant belief** [Tech debt / soundness guard, M, Done — `.goast-beliefs/continuation-capture-marks-shared.scm`, commit `3ddbe839`; reaches-call checker added in wile-goast; validated 5/5 capture sites reach `MarkChainShared`]: The field-oracle verifies each method matches its declared per-field discipline, but by construction it **cannot** catch the *upstream-invariant-violated* class — the two canonical continuation reverts (`tail-frame-recycling-unsound`, `c1-continuation-not-frame-reclaim`) both had `RestoreAndRelease` doing exactly what the descriptor says, while a frame a captured continuation still reached was released because a capture site failed to mark the live chain shared. Deliverable: a `wile-goast` belief (belief DSL the project already uses) asserting *every function that constructs a `Captured`/`Composable` continuation marks the live `mc.cont` chain shared (via `MarkChainShared`) before any release path can fire* — the structural guard for the `RELEASE_OLD_ENV`/`POOL_FRAME` precondition the oracle documents but can't enforce. Known capture sites to cover: `Copy`, `SliceContinuationAt`→`MarkChainShared`, `CurrentContinuation`. **Supplement to, not a replacement for, the red-suite** (per plan D-c). Gated on Tier-1 only (now merged) — no perf measurement needed. Watch its false-positive profile (belief DSL); ship as its own scoped item, don't bundle.
- [ ] **Option B — codegen the six save/restore/copy bodies from `contDescriptor`** [Perf/structure, L, Deferred — perf-gated]: The literal "data-driven" half of finding #1. `go:generate` the six method bodies *from* the descriptor so the spec lives in data and the code is emitted, not hand-transcribed — identical runtime (generated Go, not interpreted). **Hard gate:** an end-to-end benchmark proving normal-return-path parity (`memory`: micro-benchmarks mislead; sites #3–#5 are the hot path where table/reflection dispatch loses to a `switch`, and this path is the dominant GC contributor). Promotes `contDescriptor` from a `_test.go` spec to a generator-readable data file — a real restructuring, not a freebie. Do NOT gate #1's drift-catching value on this; Tier-1 already delivered that.

### Internal-SR follow-ups (PR #739 deferred items)

Items surfaced by /crosscheck on PR #739 (internal/ structural reduction
phases 1-5 — Findings 7, 4, 3, 2, 6 of
`plans/2026-05-07-internal-structural-reduction.md`). Deferred per scope.

- [ ] **`*SyntaxObject.Datum()` and `*SyntaxObject.Unwrap()` duplication** [Tech debt, XS, Deferred — pre-existing]: Both methods return `p.datum` with no transformation (`internal/syntax/syntax_value.go:94-96` and `:103-105`). `Unwrap` is the `SyntaxValue` interface method; `Datum` is the historical accessor. Pre-existing; surfaced by but not introduced by PR #739. Fix shape: audit callers (which name does each use?) and delete one. If Unwrap is interface-required, delete Datum or make it a one-line forward; otherwise reverse the choice. Out of scope for the structural-reduction phases; clean-up commit when next touching syntax_value.go.

- [ ] **`qt.Assert(t, ...)` vs `c := qt.New(t); c.Assert(...)` style split in `internal/validate/`** [Tech debt, S, Deferred — pre-existing]: The validate package's test files mix two quicktest invocation styles. Older files (`walk_sub_exprs_test.go`, `validate_capture_test.go`, `validate_escape_test.go`) use the `c := qt.New(t); c.Assert(...)` form; recent additions (`env_helpers_test.go` from PR #739, `walk_binding_refs_test.go` from PR #740) use the package-level `qt.Assert(t, ...)` form. Both are valid quicktest API; the split is purely stylistic. Fix shape: pick one and propagate — likely the package-level `qt.Assert(t, ...)` since it's the more recent precedent and is what other Wile packages use. Out of scope for any one PR.

### Loss-signals API follow-ups (numeric-loss-signals impl)

Items from the numeric loss-signals plan
(`memory/2026-05-14-numeric-loss-signals-design.md` /
`memory/2026-05-14-numeric-loss-signals-impl.md`). Track decisions
that were made on the impl path but warrant revisiting once usage
patterns are visible.

- [ ] **Revisit hybrid return shape if helper set grows** [Tech debt, M, Deferred]: Current API uses a **hybrid** return shape — `ToFloat64WithAccuracy` returns positional 4-tuple `(float64, big.Accuracy, bool, error)`; `ToComplex128WithAccuracy` returns `(Complex128Result, error)` with a named struct (fields `Value`, `RealAcc`, `ImagAcc`). The rule: positional when slot types disambiguate roles; struct when adjacent slots share a type and could be silently swapped. Decision rationale + alternatives (all-positional, all-struct) documented at `memory/2026-05-14-numeric-loss-signals-design.md` § "Decision record: return shape — hybrid (positional + struct)". **Revisit triggers**: (a) a second `WithAccuracy`-shaped helper with a single accuracy signal is added (rationals, intervals, matrix elements with one component) — re-evaluate whether the new helper should follow `ToFloat64WithAccuracy` (positional) or be promoted to struct for consistency with `ToComplex128WithAccuracy`; (b) a third or fourth multi-component helper is added (quaternion, matrix with N≥3 same-type slots) — at that point the asymmetry-as-domain-structure argument weakens, consider a unified struct convention; (c) FFI converter is refactored to consume the struct directly for both helpers (eliminates the discard-idiom advantage motivating positional `ToFloat64WithAccuracy`); (d) a `realAcc/imagAcc` swap bug is reported despite the struct — indicates the safety property failed, revisit whether stricter encoding is warranted (e.g., distinct newtypes `type RealAccuracy big.Accuracy` / `type ImagAccuracy big.Accuracy`).
- [x] **BigComplex precision-loss bugs in math primitives & methods** [Correctness, M, Done — sites 1,2,3,5 on `fix/bigcomplex-precision-loss`; site 4 (angle) split out below]: Crosscheck on the PR 1 (Go infrastructure) branch surfaced precision losses on BigComplex that the `Float64Truncated` rename made *visible* but did not introduce (pre-existing, not regressions). **Fixed:** (1) `PrimMagnitude` BigComplex now routes through `(*BigComplex).Magnitude()` (`pkg/values/big_complex.go`) instead of truncating to float64 — `(magnitude 10^400+10^400i)` returns a finite big value, not `+inf.0`. (2) `PrimSqrt` BigComplex routes through the new `(*BigComplex).Sqrt()` — numerically-stable closed form on `big.Float` (larger component from `|z|`, smaller by division), no `+inf.0+inf.0i` overflow. (3) `floor`/`ceiling`/`truncate`/`round` no longer round-trip through float64: `Integer`/`BigInteger` are identity+exact (BigInteger previously collapsed to inexact `Float`), `Rational` rounds exactly (previously `int64(f(Float64Truncated()))` **overflowed** to `MaxInt64` on large magnitudes), and `BigFloat` rounds at its own precision — all via `roundRatToInt` on the exact `big.Rat`. (5) `(*BigComplex).EqualTo(*Complex)` promotes the Complex's float64 components to `big.Float` and compares at full precision instead of truncating the BigComplex first. **Note:** the TODO's site-5 premise was inaccurate — Scheme `=` uses `NumericEquals` (`a.Subtract(b).IsZero()`, already precise), and `equal?`/`eqv?` short-circuit on the exact-vs-inexact mismatch; the truncating `EqualTo` is reached only via the `Value.EqualTo` interface (hashtable keys, cross-type `equal?`), which the fix hardens. Tests: `TestRoundingBigPrecision`, `TestBigComplexTranscendentalPrecision` (Scheme-level), `TestBigComplex_EqualTo_StrictBeyondFloat64`, `TestBigComplex_Sqrt` (values-level).
- [x] **`floor/`, `truncate/` and the `*-quotient`/`*-remainder` family overflow int64 on large exact operands** [Correctness, M, Done 2026-07-07]: `realDivision` (`extensions/math/prim_rounding.go`) computed the exact path as `int64(roundFn(n0/n1))` on float64s from `helpers.ExtractReal` — the same float64 round-trip + int64-cast overflow class removed from `floor`/`ceiling`/`truncate`/`round` in `#679`. **Verified pre-fix:** `(floor-quotient (expt 10 30) 7)` → `9223372036854775807` (int64 saturation), `(floor-remainder (expt 10 30) 7)` → `52776558133248` (garbage). **Fix:** exact-integer operands (both `Integer`/`BigInteger`) now take a `big.Int` path — `q = roundRatToInt(new(big.Rat).SetFrac(b0,b1), mode)` reusing the rounding family's sign-correct rounder, `r = b0 - q*b1` exact by construction; multi-value return preserved. Signature changed `roundFn func(float64)float64` → `mode roundMode` (single floor/truncate source). Inexact + non-integer(rational) operands keep the float64 path unchanged. Guarded by `TestIntegerDivisionBigPrecision` (14 cases: positive/negative divisor, floor≠truncate divergence, division identity). **Scope note:** the rational-operand remainder is a *separate* pre-existing float64-truncation bug (`(floor-remainder 7/2 2)` → `1`, exact is `3/2`); left untouched — rationals are non-conformant args per R7RS §6.2.6 (integers only).
- [x] **BigComplex `angle`/`Phase` big-precision atan2** [Correctness, L, Done 2026-07-07 — `feat/bigcomplex-angle-atan2`]: New `pkg/values/big_transcendental.go` — arbitrary-precision `BigPi`/`BigAtan`/`BigAtan2` on `*big.Float` (Go's `math/big` has native `Sqrt` but no transcendentals): argument-reduction Taylor for `atan` (reciprocal to `(0,1]` then halve to <2⁻⁸, bounded ~8 `Sqrt` regardless of magnitude), Machin `π = 16·atan(1/5) − 4·atan(1/239)` cached per precision, quadrant logic on the big-precision ratio `y/x` (finite for any finite operands → no overflow). `(*BigComplex).Phase()` now routes through `BigAtan2`; `PrimAngle` big-complex case → `v.Phase()`; `PrimAtan` 2-arg atan2 and 1-arg real big paths → `BigAtan2`/`BigAtan`. **Return-type change (user-approved):** unbounded-tier input (BigComplex / BigFloat / BigInteger / Rational) now yields `*BigFloat` not `*Float` — coherent precision-tier rule (matches existing big-real `angle`), Scheme numeric behavior unchanged; `TestAngle` + trig-rational tests updated. **Also closed the two residual overflow holes** (2026-07-07, same branch): (1) `*Rational` operands now take the big path (`isUnboundedReal` includes Rational — unbounded range), so a huge rational no longer overflows atan2; (2) 1-arg **complex** atan on a BigComplex whose components exceed float64 range now routes through new `values.BigLog` + `values.BigComplexAtan` (natural log via Machin-style `ln(mant)+e·ln2` from an atanh series; complex atan via `(i/2)[ln(1−iz)−ln(1+iz)]`) instead of `cmplx.Atan` returning NaN. In-float64-range BigComplex still uses branch-cut-correct `cmplx.Atan` (gated on `math.IsInf`), so no regression. **Sole remaining caveat (documented, not a silent hole):** `BigComplexAtan` on the imaginary-axis branch cut (`Re z=0`, `|Im z|>1`) returns the principal `+π/2` vs Go's signed-zero `−π/2` — only reachable out of float64 range where `cmplx.Atan` is NaN anyway (an improvement, not a regression). Tests: precision-honesty (`4·atan(1)=π`, `BigLog(2)=ln2` to 60+ digits) + overflow canaries (`atan2(1e401,1e400)=atan(10)`; huge rational; huge complex real part → π/2) at values + extension levels. Design: `plans/2026-07-07-bigcomplex-angle-atan2-design.local.md`. All gates green (lint/covercheck/ci).
- [x] **Big-precision real transcendentals sweep (exp/log/sin/cos/tan/asin/acos)** [Correctness/Precision, L, Done 2026-07-07 — `feat/big-transcendentals`]: Extends the atan/angle arc to the remaining transcendentals. New kernels in `pkg/values/big_transcendental.go`: `BigExp` (range-reduce `x=k·ln2+r`, Taylor, rescale `2ᵏ` — overflow-safe), `BigSin`/`BigCos` (reduce mod 2π to `[−π/2,π/2]` with **working precision scaled to the argument's exponent** = the big analogue of Payne–Hanek, so large exact args reduce correctly, not "best-effort"), `BigTan`=sin/cos, `BigAsin`=`BigAtan(x/√(1−x²))`/`BigAcos`=π/2−asin (decline→nil for `|x|>1`, the complex domain), `BigLog`-backed real `log`/`log-base-b`. Wiring: `makeComplexPrimitive(name, cmplxFn, bigRealFn)` prepends an `isUnboundedReal → *BigFloat` guard (nil-return declines); `log` guarded inline; **`exp` is custom** — besides the tier path it rescues a *bounded* operand whose `math.Exp` over/underflows float64 to a finite BigFloat (user-approved: `(exp 1000)` → 1.97e434, not +inf.0 — exp's overflow threshold ~709 sits inside float64 range, unlike the others). Tier rule: unbounded-tier real (BigFloat/BigInteger/Rational) → `*BigFloat`; bounded Integer/Float → float64 (`math.*` is already Payne–Hanek-correct for large trig args); out-of-domain (`asin`/`acos` `|x|>1`, `log` `x≤0`) → complex path. Updated trig-rational tests to `assertBigFloatResult` + added tier-agnostic `assertRealResult` for mixed-tier identity loops. Canaries: `exp(1000)` finite + `log(exp(1000))=1000` round-trip, `log(10^400)≈921`, `sin`/`cos` vs `math.*` incl. `1e15` (validates reduction against Payne–Hanek), `BigExp(1)=e`/`BigLog(2)=ln2` to 60+ digits. **Follow-on shipped same day — see next entry.** Design: `plans/2026-07-07-big-transcendentals-design.local.md`. All gates green.
- [x] **BigComplex transcendentals (exp/log/sin/cos/tan/asin/acos)** [Correctness, L, Done 2026-07-07 — `feat/big-complex-transcendentals`]: Follow-on to the real sweep. New `pkg/values/big_transcendental_complex.go`: `BigComplexExp` (exp(re)·(cos im+i·sin im)), `BigComplexLog` (½ln(re²+im²)+i·atan2, principal branch), `BigComplexSin`/`Cos` (via `sinhCoshAt`), `BigComplexTan` (complex division), `BigComplexAsin` (−i·ln(iz+√(1−z²)) reusing `BigComplex.Sqrt`)/`BigComplexAcos` — composed from the real kernels + `BigAtan2`. **Gating (user chose overflow-gate all)**: consistent with the shipped complex `atan`. A `rescueBigComplex` helper recomputes at big precision only when the float64 `cmplx.*` result is non-finite **and** the big result is finite (so `log(0)=−∞` and genuine infinities stay float64 `*Complex`; NaN/Inf operands aren't rescued — `math/big` has no NaN). In-float64-range complex operands keep branch-cut-exact `cmplx.*` (the big kernels run only where `cmplx.*` is NaN, so principal-value is an improvement, not a divergence). Also hardened `numberToBigFloat` against NaN/Inf `*Float`. Tests: agreement with `cmplx.*` at in-range off-cut points for all seven; finiteness/round-trip canaries where `cmplx.*` overflows (exp re>709, sin |im|>709, log component>1.8e308) + Scheme-level extension canaries. Design: `plans/2026-07-07-big-transcendentals-design.local.md`. All gates green.
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

---

## Tier 6 — Nice-to-Haves

No demand signal. Speculative or research-only.

### Tooling
- [ ] **Hygiene debugging** [Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Programmatic tokenization/parsing**: Expose tokenizer/parser to Scheme. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Event callbacks**: Hooks for expansion, compilation, debugging. IDE integration, profiling.

### Standard Library
- [ ] **Hashtable SRFI compliance**: Current custom API (10 primitives) doesn't conform to any SRFI. Gaps vs SRFI-125: no custom hash/equality, no `hash-table-update!`, no fold/map, no immutable variant, naming uses `hashtable-*` not `hash-table-*`. Decide: SRFI-125 or keep custom.
- [ ] **Logging library**: Levels, structured output, handlers.
- [ ] **Go AST Phase 3 — Comments & generics** [S]: `Comment`/`CommentGroup` attachment, `BadExpr`/`BadStmt`/`BadDecl` error recovery, `IndexListExpr` for generics. `plans/GO-AST.md`
- [ ] **`continuation-mark-set-first` accepts `#f` for mark-set** [XS, Racket-compat]: Racket lets `#f` stand in for "current continuation's marks" as the first argument; Wile's `PrimContinuationMarkSetFirst` (`registry/core/prim_cont_marks.go:54`) hard-requires `*machine.ContinuationMarkSet` via `RequireType`. One-branch fix: check `values.FalseValue` before the type check and substitute `mc.CollectContinuationMarks(machine.DefaultPromptTag)`. Surfaced by the audit findings crosscheck on PR #673; no demand signal yet. Defer until the audit's Phase 4 (axis C — Racket compliance sweep) or a real consumer asks.

### Core Language
- [ ] **Type system**: Covers base types, expandable. Discover useful type properties. Types as distinct top-level concept.
- [ ] **let-syntax*** [S]: Implement `let-syntax*`.
- [ ] **Scribble-style `@` reader notation** [Reader extension]: Racket-style at-expressions for rich documentation markup. `@cmd[datum ...]{text ...}` desugars to S-expressions.

### Architecture
- [~] **Dialect system** [In progress]: forms layer SHIPPED (SP1 per-engine codegen fork, `WithDialect`, `DefaultDialect`). Primitive-level control SHIPPED (`PrimitiveRemover` + `BootstrapProcedureRewriter` capabilities; `NoMutation` removes ALL 13 mutators genuinely — mutating `vector-map`/`string-map` swapped for a mutation-free bootstrap fragment; inline-HOF optimizer gated on `requires` so removal deopts cleanly). `NoMutation` is the one shipped leaf dialect; it exercises the forms seam (removes `set!`) plus both cross-ceiling capabilities. NoMutation import-reexpose remains a documented language-surface boundary (dialect ≠ sandbox), not a gap. The demo leaves `R5RSStrict` and `R7RSMinimal` — and the `DisableExpandForm` expander gate that only R5RSStrict used — were pruned once no product consumer wanted restricted-surface engines; the seam + `NoMutation` remain. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
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

- [x] **Promoted op table**: Table-driven dispatch regressed ~1.5% geo mean (15/16 Gabriel benchmarks slower). Go compiles contiguous-integer switches to jump tables; table-driven adds overhead. `plans/2026-04-05-structural-reduction.md`
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
- [x] **Peephole optimizer double-restore** [Fixed]: `savedCont` pointer-identity guard. `plans/OPTIMIZER-FIX.md`
- [x] **Degenerate form pipeline tests** [Done]: Full-pipeline tests for all core special forms. PR #571.
- [x] **Sub-context winding stack inheritance hazard** [Fixed]: Constructor parameter requirement. `machine/machine_context_subcontext.go`.
- [x] **`cond-expand (library ...)` bypasses FileResolver** [Fixed]: `machine/compilation/features.go`.
- [x] **syntax-rules ellipsis and hygiene bugs** [Fixed]: Three bugs — scope-aware duplicate binding detection (PR #607), cross-group ellipsis zipping, nested ellipsis depth tracking (PR #606).

### Refactoring
- [x] **`WalkSubExprs` for validated expression traversal** [Done]: `ChildRole` enum, B1 capture analysis migrated.
- [x] **Extract interface types from `environment/` `any` fields** [Done]: 15 type assertions removed across 7 files. `plans/2026-03-31-environment-any-fields.md`
- [x] **`Stack.Pull()` O(1) replacement** [Done]: `PullDrain()` in `OpPullApply`. `plans/2026-03-31-pulldrain-design.md`
- [x] **Split `ffi.go` by concern** [Done]: 1010 lines → 4 files. PR #599.
- [x] **Engine initialization order invariant** [Done]: 6-step DAG documented. `plans/2026-04-01-engine-init-order.md`
- [x] **`machine/` mega-package decomposition** [Done]: PRs #592, #593. `plans/2026-03-30-machine-decomposition-design.md`
- [x] **`file_resolver.go` chain of responsibility** [Done]: 541 → 469 lines.
- [x] **Timing-dependent concurrency tests** [Done]: PR #602. `plans/2026-04-01-timing-dependent-tests.md`
- [x] **ExpanderTimeContinuation convention deviations** [Done]: 18 deviations fixed.
- [x] **Opcode metadata consolidation (D5)** [Done]: `OperandKind` enum. `plans/2026-04-05-structural-reduction.md`

### Tech Debt
- [x] Task 1.1: `uint16` source table index overflow → `uint32`
- [x] Task 1.2: Opcode round-trip exhaustiveness test (already existed)
- [x] Task 1.3: Extension list consistency test (already existed)
- [x] Task 1.4: Eval stack size limit — `WithMaxStackSize(n)`. `plans/2026-04-11-eval-stack-limit-design.md`
- [x] Task 4.2: Security gate integration tests (already existed)
- [x] Task 5.1: `NamedCallable` interface
- [x] Task 5.2: `StringOrFalse` helper. PR #609.
- [x] Task 5.3: `ForEachList` for proper-list enforcement. PR #609.
- [x] Task 5.4: `requireSourceContext` helper. PR #609.
- [x] Task 5.5: `RequireArg[T]` migration (5 sites, 3 intentional deviations). PR #609.
- [x] Task 6.1: Delete `runtime/` package
- [x] Task 6.3: Receiver naming normalized. PR #609.
- [x] Task 7.1: Unified `machine/testutil` into `registry/testhelpers`. PR #609.
- [x] Task 8.3: REPL decoupled from `machine/compilation`. PR #639. `plans/2026-04-11-repl-decoupling-design.md`
- [x] Task 8.5: `prim_eval.go` funneled through `NewSubContext`. PR #637. `plans/2026-04-11-eval-subcontext-design.md`

### Performance
- [x] **GC pressure reduction** [Done]: -8.9% geo mean. PRs #562-563. `plans/GC-PRESSURE-REDUCTION.md`
- [x] **Core-let compilation** [Done]: PR #570. `plans/CORE-LET-IMPL.md`
- [x] **Procedure inlining** [Done]: PR #605. `plans/PROCEDURE-INLINING.md`
- [x] **B2 escape analysis** [Done]: PR #604. `plans/ESCAPE-ANALYSIS.md`

### Features
- [x] **Algebra library** [Done]: `(wile algebra)`. 158 tests. `plans/2026-03-25-algebra-library-design.md`
- [x] **`(wile algebra polynomial)` library** [Done]: Ring-parameterized univariate polynomials. 12/12 tasks. 60 tests passing. poly-plus/negate/minus/times, Horner evaluation, formal derivative (characteristic-safe, O(n) via accumulator threading), divmod (field-required), GCD (Euclidean, monic-normalized), polynomial-ring capstone (enables recursive rings R[x][y]), `with-polynomial` macro. Commits `69b98203`..`78bb7e2f`. `plans/2026-04-18-polynomial-library.md`
- [x] **`(wile algebra matrix)` library** [Done]: Semiring-parameterized matrix algebra (§5.1 of foundations). Path D implementation across 10 phases: sparse/dense representations, dispatch-table rep-tags, bang-first arithmetic, aliasing enforcement. Test count 112→303. PRs #684–#691 (P2–P10), #695 (error attribution), #696 (N1–N9 crosscheck follow-ups). `plans/2026-04-20-algebra-matrix-impl.md`, `plans/2026-04-21-matrix-path-d-impl.md`.
- [x] **`(wile algebra incidence)` library** [Done]: Möbius/incidence algebra on locally-finite posets per Rota (1964) (§5.2 of foundations). Formalizes ad-hoc direct-vs-transitive handling across four wile-goast posets (dominator trees, subtype lattices, call-graph reachability, import DAGs) and belief-DSL overlap normalization. `<locally-finite-poset>` with `(leq? interval-proc)`, ring-parameterized with `(integer-ring)` default, lazy memoization via `equal?`-keyed hashtable. ~200 LOC, ~25 tests. Commit `4ff8a314`. `plans/2026-04-21-incidence-algebra-impl.md`.
- [x] **`(wile algebra unification)` library** [Done]: AC-matching and AC-unification per Eker/Stickel/Contejean–Devie (§5.3 of foundations). `ac-match`, `ac-unify`, `<pattern-var>` records, substitution suite, `diophantine-basis`. `ac-unify` returns CSU (finitary-not-unitary per Fages–Huet 1986). PR #698 (30 commits on `feat/algebra-unification`). `plans/2026-04-21-ac-matching-design.md`, `plans/2026-04-21-ac-matching-impl.md`.
- [x] **SRFI-14 + `(wile charsets)`** [Done]: 17 FFI primitives + 23 derived Scheme procedures + 17 named char-sets (`char-set:letter`, `char-set:digit`, `char-set:whitespace`, etc.) sourced from Go's `unicode` tables; inversion-list representation, fully immutable. Char-set criteria enabled across 7 SRFI-13 procedures (`string-index`, `string-skip`, `string-count`, `string-trim*`, `string-tokenize`, `string-filter`, `string-delete`). `(wile charsets)` exposes `char-set-ranges` for efficient iteration. PR #TBD. `plans/2026-05-04-srfi-14-design.md`, `plans/2026-05-04-srfi-14-impl.md`.
  - **Completeness follow-up (Track A4) [Done]:** n-ary zero-arg identities (6F) + the previously-deferred cursor protocol, hash, and diff+intersection (6G). `char-set-union`/`-intersection`/`-xor` now return their identity element on zero args (empty/full/empty) via Scheme wrappers over `%char-set-*` folds; `char-set-difference` keeps its ≥1-arg arity (no identity per SRFI-14). All seven formerly-deferred names now implemented and exported: `char-set-hash` (content-stable, bounded, O(#ranges) over the canonical inversion list), `char-set-cursor`/`char-set-ref`/`char-set-cursor-next`/`end-of-char-set?` (cursor walks the inversion-list ranges; defensively skips the U+D800–U+DFFF surrogate block that `integer->char` rejects), and `char-set-diff+intersection`/`!`. `pkg/stdlib/lib/srfi/14/{algebra,cursor}.scm`, tests in `integration/testdata/srfi-14-tests-{algebra,cursor}.scm` + `extensions/charsets/charsets_test.go`. NOTE: the surrogate construction-time invariant (6D) remains deferred — only iteration is guarded.
- [x] **SRFI-13 + `(wile strings)`** [Done]: 60 SRFI-13 procedures + `string-trim-left` alias + 5 `(wile strings)` extras (`string-split`, `string-replace-all`, `string-byte-length`, `string-blank?`, `string-repeat`); 309 integration tests across 8 phases. All pure Scheme; FFI promotion deferred (profile-driven §6 of design). `(wile strings)` resolves the SRFI-13 vs R7RS `string-map` shadowing via `(except (scheme base) string-map)`. Char-set criteria enabled by SRFI-14; `string-titlecase`, `string-hash`, `string-unfold`, `xsubstring`, `*/shared` forms also deferred per §11 of design. PR #721. `memory/2026-05-03-string-primitives-design.md`, `memory/2026-05-03-string-primitives-impl.md`.
- [x] **Documentation system** [Done]: Full infrastructure — `,doc`, `,apropos`, `,topics`, `,topic`, library descriptions, docstring examples. PRs #579-591.
- [x] **MCP server** [Done]: `wile --mcp`. PR #588. `plans/2026-03-26-wile-mcp-server-design.md`
- [x] **`(available-libraries)` primitive** [Done]: PR #590. `plans/AVAILABLE-LIBRARIES.md`
- [x] **OpaqueValue type** [Done]: Generic opaque wrapper for Go objects in Scheme.
- [x] **Disassembler** [Done]: `(disassemble proc)`, `,dis`, MCP tool. PR #603.
- [x] **Go AST Phase 2** [Done]: 13 node types. PR #480. `plans/GO-AST.md`
- [x] **Climbing macro tower — Tier 1** [Done 2026-07-10, branch `feat/climbing-tower-tier1`]: A phase-*N* macro whose transformer body defines and uses further macros climbs to *N+1*, *N+2*, … via relative phase accessors (`EnvironmentFrame.NextPhase()`) at the four macro-resolution sites (transformer-body compilation, define-syntax storage ×2 incl. the internal-body path, macro lookup ×2, begin/define-for-syntax + import placement). `phaseLevel 0` is byte-for-byte identical to pre-tower (level-0 identity). Pinning RED→GREEN: `TestClimbingTower_CrossPhaseCollision` (a name reused at two phases no longer collapse-clobbers). **Finding:** the feature affects *procedural* macro-writing-macros, NOT the declarative majority (declarative inner macros live in expansion *output* = same phase as use, always consistent — the plan's two/three-storey corpus is green with and without the tower, kept as level-0-identity guards). Bindings are shared, not per-phase-instantiated (Tier 2, gated). Task 6 (`GetGlobalIndexAcrossPhases` climbed-band) was a **no-op** — the R7RS `jabberwocky`/`march-hare` carve-out survives the climb unchanged. `docs/compiler/macro-system.md` §Phase Tower. `plans/2026-07-10-climbing-tower-{design,impl}.local.md`.
  - [ ] **Q4 mutation boundary (`ErrCrossPhaseMutation`) — BLOCKED, deferred**: Rejecting a `set!`-mutated binding shared across a climb (design option (b)) is blocked at the impl plan's Task 7 Step-0 gate: mutation-reachability is a whole-unit property computed in the validator pass and is NOT queryable at the cross-phase resolution site (`GetGlobalIndexAcrossPhases` / `compile_syntax_rules.go:342`), and a flag-based approximation false-positives on the (unmutated) carve-out (top-level defines aren't `Stable`). Ships as option (a) (silent share) — not a regression (master had no tower). Reachability of a genuine cross-phase mutable-sharing footgun is itself unproven. Natural home for the check: the Boundary-2 phase-precise use-time resolution rework (design §7.3), where the mutation gate and the `[0,1,2]` sweep would retire together. `plans/2026-07-10-climbing-tower-q4-mutation-boundary-note.local.md`.

### Other
- [ ] **Important refactoring**
    - When few fields are referenced from a struct within a function, pass in the field - do not pass in the struct or a reference to the struct

- [x] **Promote `eval` extension to public** [Done]: Moved `internal/extensions/eval/` → `extensions/eval/`, importable as `github.com/aalpar/wile/extensions/eval`. Required by wile-goast and any embedder wanting sandboxed `(eval ...)` / `(load ...)`. The naive composition `WithProfile(Console) + WithExtension(eval.Extension)` does **not** work — `ConsoleAuthorizer` denies `code:load`, so `(load ...)` fails. The fix is a baked `ConsoleWithLoad` profile (extensions + matching authorizer that allows `code:load` under `/tmp`), now part of `plans/2026-03-26-environment-profiles-impl.md`.

</details>


- Update skills to explicitly state where wile-goast is a fit for refactoring.  Add guidance.
- Add guidance to skills where Serena use makes sense.
