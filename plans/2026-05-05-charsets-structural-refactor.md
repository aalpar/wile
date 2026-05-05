# Charsets Structural Refactoring Plan

> **Status: ✅ SHIPPED.** All 5 phases + Phase 0 + planning artifacts complete on branch `feat/charsets-structural-refactor` (8 commits past master). `make ci` passes. Per-phase summary:
>
> | Phase | Status | Commit | Notes |
> |---|---|---|---|
> | docs | ✅ | `760d1e0c` | Plan + cascade plan + iteration-idioms doc + CLAUDE.md ref |
> | 0 — RequireArg position info | ✅ | `2ffb1fb1` | Pre-work commit (was already in working tree) |
> | 1 — F1 + F2 (RequireArg + OptionalArg) | ✅ | `0f74f24c` | 11 sites migrated (matched estimate); −36 LOC |
> | 2 — VariadicArgs[T] helper | ✅ | `ee978561` | +250 LOC (helper + 9 table-driven tests) |
> | 3 — F3 migration | ✅ | `bc33ef58` | **Scope grew**: bonus `CompareVariadic[T,V]` extraction triggered by `dupl` lint hit; −61 LOC net |
> | 4 — F4 + F5 (iter.Seq accessors) | ✅ | `3456545f` | `All() iter.Seq[CharSetRange]` + `Codepoints() iter.Seq[rune]`; 8 new tests; −12 LOC charsets |
> | 5 — F6 + F7 (panic style + cache doc) | ✅ | `abb36a4b` | 4 panic sites wrapped; cache doc explicit re Go-unicode compatibility |
> | chore — manifest regen | ✅ | `e17ace33` | Unanticipated: `plans/axis-b-manifest.scm` line drift caught by `TestBuildAxisBManifest`; auto-regenerated via `WILE_AXIS_B_UPDATE=1` |
>
> **Net effect on `extensions/charsets/charsets.go`:** 646 → 489 lines (−157, ~24% smaller). 7 of 7 findings resolved. 1 sort-package opportunity (`VariadicArgs[T]`) shipped with 8 reuse sites; 1 unanticipated extraction (`CompareVariadic[T,V]`) shipped with 2 reuse sites.
>
> Below is the implementation plan as it actually ran. Original drafting style retained for traceability.

**Goal:** Resolve the 7 structural findings from the post-v1.15.0 review of `extensions/charsets/` + `values/char_set.go`. The new SRFI-14 code shipped with hand-rolled patterns that bypass project-level helpers (`registry/helpers`) and an internal API surface (`CharSet.Ranges()`) that pays for a defensive copy on every call. Bring the package into line with the rest of the extension layer and surface one new sort-package abstraction (`helpers.VariadicArgs[T]`) that already has 8+ reuse sites in the codebase.

**Architecture:** Three migration phases against existing code; one new helper added to `registry/helpers/` (became two helpers — see Phase 3 deviation note). No new packages, no new value types. The `CharSet` value type gains two iteration methods (`All`, `Codepoints` returning `iter.Seq[T]`, per Q-b resolution) that close the immutability invariant the type already documents.

**Source review:** `plans/2026-05-05-charsets-refactor-review.md` (this file's predecessor — captures the structural-reduction analysis that generated each finding).

**Target files (actual):**

- `registry/helpers/args.go` — Phase 0: `RequireArg` includes 1-indexed argument position in error messages
- `registry/helpers/variadic.go` — NEW: `VariadicArgs[T any]` helper
- `registry/helpers/variadic_test.go` — NEW: 9 table-driven test cases for `VariadicArgs[T]`
- `registry/helpers/char.go` — migrate `CharCompareVariadic` via `CompareVariadic[*values.Character, rune]`; **also** hosts the new `CompareVariadic[T,V]` helper (extracted in Phase 3 due to `dupl` lint hit)
- `registry/helpers/string.go` — migrate `StringCompareVariadic` via `CompareVariadic[*values.String, string]`
- `values/char_set.go` — add `All() iter.Seq[CharSetRange]` and `Codepoints() iter.Seq[rune]`; replace 4 `panic(fmt.Sprintf(...))` sites in `NewCharSetFromRanges` with `werr.WrapForeignErrorf` panics
- `values/char_set_test.go` — 8 new tests covering visit order, early-exit, nil receiver, empty set for both iter.Seq accessors
- `extensions/charsets/charsets.go` — applied F1, F2, F3, F4, F5; bulk of LOC reduction (646 → 489, −157 lines / 24% smaller)
- `extensions/charsets/charsets_test.go` — no changes required; zero tests asserted on helper message text
- `plans/axis-b-manifest.scm` — auto-regenerated for line-number drift after the refactor

**LOC budget (actual):** `extensions/charsets/charsets.go` shrank by 157 lines (vs. estimated ~85). New helpers and iter.Seq accessors added ~310 lines including tests. Whole-branch delta: ~+750 (mostly tests + 549 lines of plan/idioms docs). Qualitative outcome: 4 anticipated sort-package opportunities resolved + 1 unanticipated (`CompareVariadic[T,V]`).

**Workflow per phase:** red → green → refactor → `make lint && make covercheck` → commit. Each phase ends with a single conventional-commit-style commit (`refactor(charsets): adopt helpers.RequireArg [F1]`). Tests must pass before moving to the next phase.

**Order rationale:** Phases are strictly ordered by dependency:
1. **Phase 1** depends only on the already-edited `RequireArg`. Lowest risk.
2. **Phase 2** introduces the new `VariadicArgs[T]` helper that Phase 3 needs.
3. **Phase 3** is the bulk migration of charsets to the new helper plus other internal helper migrations.
4. **Phase 4** touches the value-type API (`CharSet.All/Codepoints` returning `iter.Seq[T]`, per Q-b resolution) and is the only phase that changes external API surface. Goes last because it's the largest review burden and is independent of phases 1-3.
5. **Phase 5** addresses style/invariant issues (panic format, named-sets cache). Independent; could be split off.

---

## Design Decisions (resolved)

### Q-a. `VariadicArgs[T]` error-message format → **A (positional)**

Type errors mid-rest-list use the same `"argument N:"` format established by `RequireArg` in Phase 0. Treats fixed + rest as one logical argument vector with 1-indexed positions. Example:

```
char-set-union: argument 3: expected a char-set but got *values.Pair
```

Migrating-away from `"rest argument:"` wording in the 6 hand-rolled charsets sites is part of Phase 3.

### Q-b. Iteration protocol → **`iter.Seq[T]` (Go 1.23+ stdlib)**

`*CharSet` exposes its iteration via stdlib `iter.Seq`, matching `slices.Values`, `maps.Keys`, `strings.SplitSeq`. The existing `Ranges() []CharSetRange` is preserved for the dual-cursor merge algorithms (`unionTwo`, `intersectTwo`, `differenceTwo`, `isSubset`) where slice-form is materially simpler than `iter.Pull`-bridged dual iteration.

```go
// Preserved — used by mutating/dual-cursor algorithms (~4 internal sites)
func (p *CharSet) Ranges() []CharSetRange

// New — primary iteration API, range-over-func
func (p *CharSet) All() iter.Seq[CharSetRange]
func (p *CharSet) Codepoints() iter.Seq[rune]
```

Call-site shape:

```go
for r := range cs.All()         { /* ranges */ }
for c := range cs.Codepoints()  { /* codepoints */ }
```

Naming matches Go stdlib convention: `slices.All(s)`, `maps.All(m)` use bare `All` for "iterate everything"; `slices.Values(s)`/plural-of-thing for typed iteration. `cs.All()` and `cs.Codepoints()` mirror this.

**Cost note:** `iter.Seq[T]` allocates the closure on the heap per call (the iterator function captures `p`). The current `Ranges() []CharSetRange` allocates an `O(n)` slice copy per call. `iter.Seq` is strictly cheaper for any `n ≥ 1`.

**Constraint:** Do not migrate the dual-cursor merges to `iter.Pull`. Each `iter.Pull` spins up a goroutine; for binary set-algebra ops on small inputs that's a real cost. Slice-form is the right shape there; that's why `Ranges()` stays.

### Q-c. `namedCharSets` cache → **document-global**

Keep the process-global `sync.Mutex`-protected cache. Add a doc comment explaining it is safe and intentional:

- Inputs are `unicode.RangeTable` values from Go's stdlib — immutable.
- Outputs are deterministic functions of inputs.
- Compatibility with `unicode.*` is preserved by construction (the cache stores results *derived* from `unicode.L`, `unicode.Ll`, etc.).

Per-Engine isolation matters for *mutable* state; the named-set cache has none. The global is correct; the comment makes the design choice explicit instead of surprising a future reader.

---

## Phases

### Phase 0 — Pre-work ✅ (commit `2ffb1fb1`)

- [x] Updated `registry/helpers/args.go:RequireArg` to include 1-indexed argument position in the wrapped error message. Comment updated to document the new format. Verified: zero tests in the codebase match the helper's exact message string; `go test ./registry/... ./extensions/...` clean.

Commit: `refactor(helpers): include argument position in RequireArg error messages`.

### Phase 1 — Charsets adopts existing helpers [F1, F2] ✅ (commit `0f74f24c`)

Pure migration. No new helpers introduced. Drove the bulk of the LOC reduction.

- [x] **F1**: Replaced 11 hand-rolled type-assertion sites in `extensions/charsets/charsets.go` with `helpers.RequireArg[T any](mc, idx, sentinel, name)`:

  | Function | Lines | Sentinel | Type T |
  |----------|-------|----------|--------|
  | `primCharSetQ` | 28 | (no error path; 0-arg pred — leave as-is) | n/a |
  | `primCharSetContains` | 34, 39 | `ErrNotACharSet`, `ErrNotACharacter` | `*CharSet`, `*Character` |
  | `primCharSetSize` | 49 | `ErrNotACharSet` | `*CharSet` |
  | `primCharSetCtor` | 59 | `ErrNotACharacter` | `*Character` |
  | `primCharSetCopy` | 96 | `ErrNotACharSet` | `*CharSet` |
  | `primStringToCharSet` | 109 | `ErrNotAString` | `*String` |
  | `primListToCharSet` | 123 | (Tuple — keep manual: helper requires a concrete type, Tuple is an interface) | n/a |
  | `primUcsRangeToCharSet` | 175, 180 | (uses `ExactInteger` — int64 path, not `RequireArg`'s shape) | n/a |
  | `primCharSetToList` / `ToString` / `Ranges` / `Complement` | 265, 281, 297, 434 | `ErrNotACharSet` | `*CharSet` |
  | `primMakeNamedCharSet` | 635 | `ErrNotASymbol` | `*Symbol` |

  **Migration count: 11 sites** (matched estimate; 4 special-shape sites stayed manual: `Tuple` interface, `ExactInteger` decoder, 0-arg predicate, the rest-list walks inside variadic primitives).

- [x] **F2**: Deleted `optionalBaseCharSet` (charsets.go:151-172, 22 lines). Replaced its 2 call sites with `helpers.OptionalArg[*values.CharSet](mc.Arg(1), nil, werr.ErrNotACharSet, "<site>")`. `primUcsRangeToCharSet`'s rest-parsing kept its inline implementation (two-optional shape doesn't fit `OptionalArg`).

- [x] `go test ./extensions/charsets/... ./integration/...`: all pass with no message-text changes.

- [x] `make lint`: 0 issues.

- [x] Commit: `refactor(charsets): adopt helpers.RequireArg + OptionalArg [F1, F2]` (`0f74f24c`).

**Actual delta:** −36 LOC in `charsets.go` (vs. estimated ~50). Error messages strictly equivalent.

### Phase 2 — Add `helpers.VariadicArgs[T]` [F3 prerequisite] ✅ (commit `ee978561`)

New helper. Error-message format follows Q-a (positional, 1-indexed).

- [x] Added `registry/helpers/variadic.go`:

  ```go
  // VariadicArgs gathers a "first fixed arg + variadic rest" call into a typed
  // []T. fixedCount is the ParamCount declared in the PrimitiveSpec; the helper
  // expects exactly that many arguments where Arg(fixedCount-1) is the rest list.
  // For the common shape ParamCount=2, IsVariadic=true: Arg(0) is fixed, Arg(1)
  // is the rest list, and the returned slice has length 1+|rest|.
  //
  // All elements (fixed + rest) are type-checked against T. On type mismatch,
  // the error names the offending argument's 1-indexed position.
  func VariadicArgs[T any](
      mc machine.CallContext,
      fixedCount int,
      sentinel error,
      name string,
  ) ([]T, error)
  ```

- [x] Added `registry/helpers/variadic_test.go` with 9 table-driven cases (broader than the 6 originally listed):
  - Single fixed arg, empty rest
  - Single fixed arg, two rest elements
  - Two fixed args, one rest element
  - Type mismatch on fixed arg position 1
  - Type mismatch on rest element 1 (overall position 2)
  - Type mismatch on rest element 2 (overall position 4) with 2 fixed args
  - Improper rest list — error wraps `ErrNotAList`
  - Non-list rest argument — same
  - `fixedCount=0` rejected with `ErrInvalidArgument`

- [x] `go test ./registry/helpers/...`: pass.

- [x] `make lint`: 1 issue auto-fixed (`goimports` on test file ordering); re-run clean.

- [x] Commit: `feat(helpers): add VariadicArgs[T] for first-fixed + variadic-rest primitives` (`ee978561`).

**Actual delta:** +250 LOC including 169 lines of tests (vs. estimated ~50). Strictly additive — no existing call sites changed.

### Phase 3 — Migrate to `VariadicArgs[T]` [F3] ✅ (commit `bc33ef58`)

**Scope deviation:** the original plan called for a straightforward migration of three call sites. The migration of `helpers.{Char,String}CompareVariadic` exposed structural duplication between `char.go` and `string.go` that the `dupl` linter flagged (lines 15-55 byte-identical between the two files). Resolved by extracting a new generic `CompareVariadic[T values.Value, V any]` helper and routing both `*Variadic` wrappers through it. The doubly-generic `variadicCompare` (which previously masked the duplication) was deleted in the process.

This was an unanticipated extraction — but it shipped a *second* sort-package opportunity (`CompareVariadic[T,V]`) that was not in the original analysis. Single-purpose layered helpers (`VariadicArgs[T]` does the gather; `CompareVariadic[T,V]` does the chain-fold) ended up cleaner than the original `variadicCompare` blob.

- [x] In `extensions/charsets/charsets.go`: deleted `charSetVariadicArgs` (28 LOC). Updated its 6 callers (`primCharSetEqual`, `primCharSetSubset`, `primCharSetUnion`, `primCharSetIntersection`, `primCharSetDifference`, `primCharSetXor`) to call `helpers.VariadicArgs[*values.CharSet](mc, 2, werr.ErrNotACharSet, "<name>")`.

- [x] In `registry/helpers/char.go`: rewrote `CharCompareVariadic` to delegate to `CompareVariadic` (the new generic helper, also defined in this file). Tests pass.

- [x] In `registry/helpers/string.go`: same migration. Wrapper now 3 lines.

- [x] **Bonus: deleted `variadicCompare`** — it was the doubly-generic helper that previously housed the chain-compare logic. Both its callers now go through `CompareVariadic` instead, which builds on `VariadicArgs[T]` for the gather step.

- [x] `go test ./registry/... ./extensions/charsets/... ./integration/...`: pass.

- [x] `make lint`: 1 issue (`dupl` between `char.go` and `string.go`) — fixed by the `CompareVariadic` extraction. 1 hint-level (`infertypeargs`) — fixed by dropping unnecessary type arguments. Re-run clean.

- [x] Commit: `refactor(charsets,helpers): migrate variadic-arg gathering to helpers.VariadicArgs[T] [F3]` (`bc33ef58`).

**Actual delta:** −61 LOC net (−39 charsets + −22 helpers). Cleaner than estimated.

### Phase 4 — `CharSet` iteration protocol [F4, F5] ✅ (commit `3456545f`)

- [x] Added to `values/char_set.go`:

  ```go
  import "iter"

  // All returns an iter.Seq that yields each canonical range in codepoint order.
  // Caller breaks the loop with `break` to early-exit. Safe because *CharSet is
  // immutable; the closure reads directly from the internal slice with no copy.
  func (p *CharSet) All() iter.Seq[CharSetRange] {
      return func(yield func(CharSetRange) bool) {
          if p == nil {
              return
          }
          for _, r := range p.ranges {
              if !yield(r) {
                  return
              }
          }
      }
  }

  // Codepoints returns an iter.Seq that yields every codepoint in the set, in
  // codepoint-ascending order.
  func (p *CharSet) Codepoints() iter.Seq[rune] {
      return func(yield func(rune) bool) {
          if p == nil {
              return
          }
          for _, r := range p.ranges {
              for c := r.Lo; c <= r.Hi; c++ {
                  if !yield(c) {
                      return
                  }
              }
          }
      }
  }
  ```

  **Did NOT remove or modify `Ranges() []CharSetRange`** — `unionTwo` relies on the slice copy for `append` safety, and `intersectTwo`/`differenceTwo`/`isSubset` use the slice form for dual-cursor merges where `iter.Pull` would cost a goroutine per binary op. Doc comment expanded to direct read-only callers to `All` / `Codepoints` instead.

- [x] Tests in `values/char_set_test.go` — 8 new (4 per accessor):
  - `TestCharSet_All_VisitOrder`, `TestCharSet_Codepoints_VisitOrder` — order matches `Ranges()`
  - `TestCharSet_All_EarlyExit`, `TestCharSet_Codepoints_EarlyExit` — `break` exits the iter
  - `TestCharSet_All_NilReceiver`, `TestCharSet_Codepoints_NilReceiver` — zero iterations on `nil`
  - `TestCharSet_All_EmptySet`, `TestCharSet_Codepoints_EmptySet` — zero iterations on `&CharSet{}`

  **Mid-implementation correction:** the first draft of `TestCharSet_All_EarlyExit` used unsorted ranges as direct `NewCharSetFromRanges` input (`{'a','c'}, {'x','z'}, {'0','9'}`), which panicked on the canonical-form invariant. Fixed by sorting the input. Useful sanity check that the panic path actually fires.

- [x] In `extensions/charsets/charsets.go`, migrated the 3 simple-traversal `cs.Ranges()` callers to the iter forms:
  - `primCharSetToList`: nested range/codepoint loop → `for c := range cs.Codepoints()`
  - `primCharSetToString`: same shape → `for c := range cs.Codepoints()`
  - `primCharSetRanges`: pre-sized slice + index loop → `for r := range cs.All()` with `append`

  **Migration count: 3 sites.** The 4 dual-cursor sites (`unionTwo`, `intersectTwo`, `differenceTwo`, `isSubset`) kept `cs.Ranges()` by design.

- [x] `go test ./values/... ./extensions/charsets/... ./integration/...`: pass.

- [x] `make lint`: 0 issues.

- [x] Commit: `feat(values): add CharSet.All and Codepoints iter.Seq accessors [F4, F5]` (`3456545f`).

**Actual delta:** +43 LOC in `values/char_set.go` + 96 LOC tests = +139. −12 LOC in `charsets.go`. 3 fewer `O(n)` slice copies per call on the `char-set->list`/`->string`/`-ranges` paths.

### Phase 5 — Style + invariant cleanup [F6, F7] ✅ (commit `abb36a4b`)

Independent of all prior phases.

- [x] **F7**: Replaced `panic(fmt.Sprintf(...))` in `values/char_set.go:NewCharSetFromRanges` (4 sites) with `panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument, "...", ...))` per CLAUDE.md imperative. **Panic-shape change**: still panics on internal contract violation, but the panic value is now a project error type — observable to a deferred `recover()` which previously saw a `string` and now sees an `error` matchable via `errors.Is(_, werr.ErrInvalidArgument)`. Pinned by `TestNewCharSetFromRanges_PanicWrapsSentinel`.

- [x] **F6**: Expanded the doc comment on `var namedCharSets` to spell out three things: (1) inputs are immutable `unicode.RangeTable` values, (2) outputs are deterministic functions of inputs, (3) maintenance constraint: keep inputs sourced from `unicode.*` only, or per-Engine caching becomes necessary. Per Q-c resolution.

- [x] `go test ./values/... ./extensions/charsets/... ./integration/...`: pass.

- [x] `make lint`: 0 issues.

- [x] Commit: `style(charsets): wrap panic values + clarify named-charset cache scope [F6, F7]` (`abb36a4b`).

### Phase chore — `axis-b-manifest.scm` regen ✅ (commit `e17ace33`)

**Unanticipated:** `make ci` failed on `TestBuildAxisBManifest`. The audit manifest (`plans/axis-b-manifest.scm`) records primitive locations including `file:line` — line numbers shifted across most charsets primitives due to the refactor. Fixed by running `WILE_AXIS_B_UPDATE=1 go test -run TestBuildAxisBManifest .` to regenerate the manifest, then committing the diff.

This step should be added to any future plan that touches a file with primitives that appear in the audit manifest. The test's failure message (`run: WILE_AXIS_B_UPDATE=1 go test -run TestBuildAxisBManifest .`) is self-explanatory.

---

## Verification (actual)

- Every phase ended green on `go test ./registry/... ./extensions/charsets/... ./integration/...` plus `make lint`.
- Final `make ci` on the branch: passed (after the manifest regen). Includes the full test matrix (`go test ./...`), `golangci-lint run`, README Go-snippet compile check, `examples/embedding/...` build, and `go mod verify`.
- Self-review pass on the diff confirmed:
  - No tests asserted on `RequireArg`'s old message format (verified before Phase 0; held throughout).
  - `optionalBaseCharSet` had no callers outside `extensions/charsets`.
  - All non-charsets uses of `CharSet.Ranges()` are inside `values/` tests and are benign reads.
- Did not run a charsets benchmark; none exists in the tree.

## Out of scope (unchanged from draft)

- **Generic `VariadicMonoidFold[T]`**: tempting to fold the 4 set-algebra ops (union/intersection/difference/xor) into one `helpers.VariadicMonoidFold[T any](mc, op func(T,T)T) error`. Defer until a second monoid domain (bit-vectors? string-set?) needs it. YAGNI.
- **Replace numeric helpers with VariadicArgs[T]**: `NumericFoldVariadic` + `NumericFoldWithFirst` could in principle reuse `VariadicArgs[values.Number]`, but they have NaN/exactness contagion logic that doesn't fit a pure gather-and-fold shape. Leave alone.
- **Move named-charsets to per-Engine state**: rejected per Q-c. Cache is referentially transparent; no per-Engine state to isolate.

## Risk assessment vs. outcome

| Phase | Predicted risk | Actual outcome |
|-------|---------------|----------------|
| 1 (F1, F2) | Low | No tests broke; messages unchanged or better. As predicted. |
| 2 (helper add) | Very low | One `goimports` auto-fix. As predicted. |
| 3 (F3 migration) | Low | **Higher than predicted**: `dupl` lint forced extraction of `CompareVariadic[T,V]` mid-phase. Outcome was net-positive (one extra sort-package opportunity shipped) but warrants noting that "mechanical migration" can surface latent duplication that the refactor either resolves or hides. |
| 4 (F4, F5 API) | Medium | Test fixture used non-canonical input on first draft, panicked. Caught by the test itself. Otherwise as predicted. |
| 5 (F6, F7) | Low | As predicted. |
| chore | (not predicted) | `TestBuildAxisBManifest` failure. Worth adding a "regenerate audit manifest" checkpoint to any plan that touches files containing primitive registrations. |

## Outcomes

| Metric | Estimated | Actual |
|---|---|---|
| `extensions/charsets/charsets.go` LOC | 646 → ~560 (−85) | 646 → 489 (−157) |
| Helpers/values added | ~70 LOC | ~310 LOC including tests |
| Sort-package opportunities shipped | 1 (`VariadicArgs[T]`, 8 reuse sites) | 2: `VariadicArgs[T]` (8) + `CompareVariadic[T,V]` (2) |
| Findings resolved | 7 of 7 | 7 of 7 |
| Net branch LOC | ~−15 | ~+750 (mostly tests + 549 lines of plan/idioms docs) |

The LOC budget overshoot is entirely tests + docs. Production-code LOC is net negative as planned, deeper than estimated. The overshoot reflects that this plan also shipped its own design context (idioms doc + cascade plan) which the original LOC budget didn't account for.
