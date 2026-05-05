# Charsets Structural Refactoring Plan

> **Status:** Draft. Pre-work landed: `helpers.RequireArg` now reports `argument N:` position info (commit pending). All other phases unstarted.

**Goal:** Resolve the 7 structural findings from the post-v1.15.0 review of `extensions/charsets/` + `values/char_set.go`. The new SRFI-14 code shipped with hand-rolled patterns that bypass project-level helpers (`registry/helpers`) and an internal API surface (`CharSet.Ranges()`) that pays for a defensive copy on every call. Bring the package into line with the rest of the extension layer and surface one new sort-package abstraction (`helpers.VariadicArgs[T]`) that already has 8+ reuse sites in the codebase.

**Architecture:** Three migration phases against existing code; one new helper added to `registry/helpers/`. No new packages, no new value types. The `CharSet` value type gains two iteration methods (`ForEachRange`, `ForEachCodepoint`) that close the immutability invariant the type already documents.

**Source review:** `plans/2026-05-05-charsets-refactor-review.md` (this file's predecessor — captures the structural-reduction analysis that generated each finding).

**Target files:**

- `registry/helpers/args.go` — already edited: `RequireArg` includes 1-indexed argument position in error messages
- `registry/helpers/variadic.go` — NEW: `VariadicArgs[T any]` helper
- `registry/helpers/variadic_test.go` — NEW: table-driven tests for `VariadicArgs[T]`
- `registry/helpers/char.go` — migrate `CharCompareVariadic` to use `VariadicArgs[*values.Character]`
- `registry/helpers/string.go` — migrate `StringCompareVariadic` to use `VariadicArgs[*values.String]`
- `values/char_set.go` — add `ForEachRange(fn)` and `ForEachCodepoint(fn)`; replace `panic(fmt.Sprintf(...))` in `NewCharSetFromRanges` with `werr.WrapForeignErrorf` panic
- `values/char_set_test.go` — tests for new iteration methods
- `extensions/charsets/charsets.go` — apply F1, F2, F3, F4, F5 (the bulk of the LOC reduction)
- `extensions/charsets/charsets_test.go` — verify message-text deltas if any tests assert on them (currently zero)

**LOC budget:** Net reduction of ~85 LOC in `charsets.go` (646 → ~560). New helpers add ~50 LOC (`VariadicArgs[T]` + tests). New iteration methods on `CharSet` add ~20 LOC. Total: ~−15 LOC, but the qualitative win is structural: 4 sort-package opportunities resolved, 0 invented.

**Workflow per phase:** red → green → refactor → `make lint && make covercheck` → commit. Each phase ends with a single conventional-commit-style commit (`refactor(charsets): adopt helpers.RequireArg [F1]`). Tests must pass before moving to the next phase.

**Order rationale:** Phases are strictly ordered by dependency:
1. **Phase 1** depends only on the already-edited `RequireArg`. Lowest risk.
2. **Phase 2** introduces the new `VariadicArgs[T]` helper that Phase 3 needs.
3. **Phase 3** is the bulk migration of charsets to the new helper plus other internal helper migrations.
4. **Phase 4** touches the value-type API (`CharSet.ForEachRange/ForEachCodepoint`) and is the only phase that changes external API surface. Goes last because it's the largest review burden and is independent of phases 1-3.
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

### Phase 0 — Pre-work (✅ complete)

- [x] Update `registry/helpers/args.go:RequireArg` to include 1-indexed argument position in the wrapped error message. Comment updated to document the new format. Verified: zero tests in the codebase match the helper's exact message string; `go test ./registry/... ./extensions/...` clean.

Commit: `refactor(helpers): include argument position in RequireArg error messages` (pending).

### Phase 1 — Charsets adopts existing helpers [F1, F2]

Pure migration. No new helpers introduced. Drives the bulk of the LOC reduction.

- [ ] **F1**: Replace 15 hand-rolled type-assertion sites in `extensions/charsets/charsets.go` with `helpers.RequireArg[T any](mc, idx, sentinel, name)`:

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

  **Migration count: 11 sites** (not the 15 originally claimed — 4 sites use shapes `RequireArg` doesn't support: `Tuple` interface, `ExactInteger` decoder, 0-arg predicate). Those 4 stay manual.

- [ ] **F2**: Delete `optionalBaseCharSet` (charsets.go:151-172). Replace its 2 call sites with `helpers.OptionalArg[*values.CharSet](mc.Arg(1), nil, werr.ErrNotACharSet, "<site>")`. `primUcsRangeToCharSet`'s rest-parsing keeps its inline implementation (two-optional shape doesn't fit `OptionalArg`).

- [ ] Run: `go test ./extensions/charsets/...`. Expected: all pass with no message-text changes (charsets had its own format that produced `"argument 1: expected ..."`; new format from `RequireArg` produces the same).

- [ ] Run: `make lint && make covercheck`.

- [ ] Commit: `refactor(charsets): adopt helpers.RequireArg + OptionalArg [F1, F2]`.

**Expected delta:** ~50 LOC removed from `charsets.go`. Error messages strictly equivalent or one-line shorter.

### Phase 2 — Add `helpers.VariadicArgs[T]` [F3 prerequisite]

New helper. Error-message format follows Q-a (positional, 1-indexed).

- [ ] Add `registry/helpers/variadic.go`:

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

- [ ] Add `registry/helpers/variadic_test.go` with table-driven cases:
  - Single fixed arg, no rest (length 1 result)
  - Single fixed arg, 2 rest elements (length 3 result)
  - Type mismatch on Arg(0) — error names "argument 1"
  - Type mismatch on rest element 2 — error names "argument 4" (fixed=2, rest[1])
  - Improper rest list — error wraps `ErrNotAList`
  - Empty rest list with fixedCount=2 → length 1 result

- [ ] Run: `go test ./registry/helpers/...`.

- [ ] Run: `make lint && make covercheck`.

- [ ] Commit: `feat(helpers): add VariadicArgs[T] for first-fixed + variadic-rest primitives`.

**Expected delta:** +50 LOC including tests. Strictly additive — no existing call sites change yet.

### Phase 3 — Migrate to `VariadicArgs[T]` [F3]

- [ ] In `extensions/charsets/charsets.go`: delete `charSetVariadicArgs` (lines 324-349). Update its 6 callers (`primCharSetEqual`, `primCharSetSubset`, `primCharSetUnion`, `primCharSetIntersection`, `primCharSetDifference`, `primCharSetXor`) to call `helpers.VariadicArgs[*values.CharSet](mc, 2, werr.ErrNotACharSet, "<name>")`.

- [ ] In `registry/helpers/char.go`: rewrite `CharCompareVariadic` to use `VariadicArgs[*values.Character]`. The current implementation walks the rest list manually (the comment explicitly notes it doesn't use `ForEach`); the new shape gathers into a slice and folds. Verify: tests in `registry/helpers/char_test.go` still pass.

- [ ] In `registry/helpers/string.go`: same migration for `StringCompareVariadic`.

- [ ] Run: `go test ./registry/... ./extensions/...`.

- [ ] Run: `make lint && make covercheck`.

- [ ] Commit: `refactor(charsets,helpers): migrate variadic-arg gathering to helpers.VariadicArgs[T] [F3]`.

**Expected delta:** −26 LOC in charsets. ~−15 LOC across char.go + string.go (manual walks → slice fold). Net negative.

### Phase 4 — `CharSet` iteration protocol [F4, F5]

- [ ] Add to `values/char_set.go`:

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

  **Do NOT remove or modify `Ranges() []CharSetRange`** — `unionTwo` relies on the slice copy for `append` safety, and `intersectTwo`/`differenceTwo`/`isSubset` use the slice form for dual-cursor merges where `iter.Pull` would cost a goroutine per binary op.

- [ ] Tests in `values/char_set_test.go`:
  - `All()` visit order matches `Ranges()` order
  - `Codepoints()` visits every codepoint of every range, in order
  - Early-exit via `break` works for both
  - Nil-receiver safety: `for r := range (*CharSet)(nil).All() { ... }` produces zero iterations
  - Empty char-set: zero iterations for both methods

- [ ] In `extensions/charsets/charsets.go`, migrate the 3 simple-traversal `cs.Ranges()` callers to the iter forms:
  - `primCharSetToList:271` (currently nested loop expanding ranges into chars) → `for c := range cs.Codepoints()`
  - `primCharSetToString:287` (same shape) → `for c := range cs.Codepoints()`
  - `primCharSetRanges:302` (yields range pairs to Scheme) → `for r := range cs.All()`

  **Migration count: 3 sites.** The 4 dual-cursor sites (`unionTwo:447`, `intersectTwo:452`, `differenceTwo:472`, `isSubset:514-515`) **keep `cs.Ranges()`** by design.

- [ ] Run: `go test ./values/... ./extensions/...`.

- [ ] Run: `make lint && make covercheck`. Spot-check `make bench-extended` if a charset benchmark exists; otherwise skip.

- [ ] Commit: `feat(values): add CharSet.All and Codepoints iter.Seq accessors [F4, F5]`.

**Expected delta:** +25 LOC in `values/char_set.go` (incl. tests). ~−10 LOC in `charsets.go`. 3 fewer `O(n)` slice copies per call on the `char-set->list`/`->string`/`-ranges` paths; the `iter.Seq` closure cost is `O(1)` per call vs the previous `O(n)` defensive copy.

### Phase 5 — Style + invariant cleanup [F6, F7]

Independent of all prior phases.

- [ ] **F7**: Replace `panic(fmt.Sprintf(...))` in `values/char_set.go:NewCharSetFromRanges` (4 sites: lines 56, 59, 62, 65) with `panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument, "...", ...))` per CLAUDE.md imperative. **No behavior change** — it's still a panic for an internal contract violation; the panic value just becomes a project error type.

- [ ] **F6**: Add a doc comment to `var namedCharSets` at line 533 explaining why the process-global cache is safe and intentional: inputs are immutable `unicode.RangeTable` values from Go's stdlib, outputs are deterministic, and compatibility with `unicode.*` is preserved by construction. Per Q-c resolution.

- [ ] Run: `go test ./values/... ./extensions/...`.

- [ ] Commit: `style(charsets): wrap panic values + clarify named-charset cache scope [F6, F7]`.

---

## Verification

- After each phase: `go test ./... && make lint && make covercheck`.
- After Phase 4: confirm `char-set->list` and `char-set->string` benchmarks (if any) show no regression.
- After all phases: read the full diff against `master` as a code reviewer (Copilot-hat pass per `feedback-copilot-self-review.md`). Specifically look for:
  - Error-message drift in any tests that DID hit `RequireArg`'s old format (re-grep after migration).
  - Any caller of `optionalBaseCharSet` outside charsets (none expected — package-private).
  - Any external dependency on `CharSet.Ranges()` in `values/` tests — all should be benign reads.

## Out of scope

- **Generic `VariadicMonoidFold[T]`**: tempting to fold the 4 set-algebra ops (union/intersection/difference/xor) into one `helpers.VariadicMonoidFold[T any](mc, op func(T,T)T) error`. Defer until a second monoid domain (bit-vectors? string-set?) needs it. YAGNI.
- **Replace numeric helpers with VariadicArgs[T]**: `NumericFoldVariadic` + `NumericFoldWithFirst` could in principle reuse `VariadicArgs[values.Number]`, but they have NaN/exactness contagion logic that doesn't fit a pure gather-and-fold shape. Leave alone.
- **Move named-charsets to per-Engine state**: rejected per Q-c. Cache is referentially transparent; no per-Engine state to isolate.

## Risk assessment

| Phase | Risk | Mitigation |
|-------|------|------------|
| 1 (F1, F2) | Low — verified zero tests match `RequireArg`'s old format | If a test does break, fix the test; the new format is strictly more informative |
| 2 (helper add) | Very low — strictly additive | n/a |
| 3 (F3 migration) | Low — each call site is mechanical | Per-site review during commit |
| 4 (F4, F5 API) | Medium — touches the public `*CharSet` API | Tests cover early-return + nil-receiver; nothing existing breaks |
| 5 (F6, F7) | Low — cosmetic + invariant | n/a |

Total LOC delta across all phases: approximately −90 LOC removed from `charsets.go`, +70 LOC added to `helpers/` and `values/`. Net: ~−20 LOC. Qualitative: 7 findings resolved, 1 sort-package opportunity (`VariadicArgs[T]`) shipped with 8 reuse sites.
