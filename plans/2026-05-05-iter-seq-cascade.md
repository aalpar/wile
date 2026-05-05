# `iter.Seq` Cascade — Tier 2 Defensive-Copy Accessors

> **Status:** Draft. **Sequenced after** `2026-05-05-charsets-structural-refactor.md` ships, since that plan establishes the `iter.Seq` precedent for the project.

**Goal:** Migrate the two cleanest defensive-copy iteration accessors in the codebase from `[]T` return to `iter.Seq[T]`, establishing `iter.Seq` as the project's default for Go-internal iteration accessors.

**Architecture context:** See `docs/dev/iteration-idioms.md` for the four-tier iteration vocabulary. This plan executes a Tier-2 migration on two accessors that match the CharSet pattern. Three other Tier-2 candidates are explicitly **out of scope** and listed below with reasons.

## In scope

| Accessor | Current | Target |
|---|---|---|
| `coverage.Collector.Entries() []Entry` | slice copy under mutex | `Collector.All() iter.Seq[Entry]` |
| `machine/compilation.LibraryExportIndex.Entries() []*LibrarySummary` | sorted slice copy | `LibraryExportIndex.All() iter.Seq[*LibrarySummary]` |

Both accessors materialize a sorted slice eagerly during construction (mutex in `Collector`'s case). After migration the slice is still materialized internally; the change is in the *return shape*, not the materialization strategy.

## Out of scope (and why)

| Accessor | Tier | Reason for exclusion |
|---|---|---|
| `environment.GlobalEnvironmentFrame.Bindings` | 3 (mutex-protected) | Snapshot IS the safety mechanism; `iter.Seq` cannot replace it without lock-leak hazards |
| `environment.GlobalEnvironmentFrame.Keys` | 3 (mutex-protected) | Same |
| `environment.LocalEnvironmentFrame.Keys` | 3 (deep-copy semantics) | Each yielded slice value is currently deep-copied; converting forces a "yield-by-reference" decision that would tighten the accessor's contract beyond what callers expect |
| `registry.Registry.Bindings` | 2 (slice copy) | Single internal caller, low leverage; defer until a 2nd caller emerges |
| `values.Hashtable.Entries(fn) error` | 4 (error-propagating callback) | Error-channel asymmetry; single caller (`ffi_arg_converters.go:216`) propagates errors mid-iteration |
| All `*.ForEach(ctx, fn)` on Scheme types | 1 (Scheme/Go boundary) | The `(ctx, error, hasNext, tail)` channels are load-bearing for Scheme semantics |

## Rationale

The CharSet refactor establishes `iter.Seq` as the project default for new Go-internal iteration. The two accessors above match the same shape as `CharSet.Ranges()` did:

- Defensive slice copy whose every caller is a `for ... range` loop
- No mutex outside the constructor (or, in `Collector`'s case, mutex held only during materialization, not iteration)
- No error-channel requirement in callers

| Accessor | Caller sites |
|---|---|
| `coverage.Collector.Entries` | `coverage/summary.go:61`, `coverage/gocover.go:49` |
| `machine/compilation.LibraryExportIndex.Entries` | `library_info.go:86`, `registry/search.go:258` |

**Performance:** approximately net-zero. The slice is still materialized; the closure adds one allocation per accessor call but saves the slice-clone allocation. Real win is convention-setting, not byte-level.

**Why ship this:** without the cascade, the codebase ends up with `CharSet.All() iter.Seq[T]` as a one-off. Two more migrations bring the convention to four call sites and three packages, making the pattern recognizable rather than novel.

## Phases

### Phase 1 — `coverage.Collector.All`

- [ ] In `coverage/coverage.go`, replace `Entries() []Entry` (line 82) with `All() iter.Seq[Entry]`. Internal materialization (templates loop + hit map + sort) stays identical; the closure yields from the materialized slice. Mutex is acquired during materialization, released before the closure returns. Document this in the doc comment.

  ```go
  // All returns an iter.Seq yielding one Entry per unique SourceContext seen
  // across all tracked templates, sorted lexicographically by
  // (File, StartLine, StartCol, EndLine, EndCol). The collector's mutex is
  // held only during materialization, not during caller iteration.
  func (p *Collector) All() iter.Seq[Entry]
  ```

- [ ] Migrate the 2 call sites:
  - `coverage/summary.go:61`: `for _, e := range c.Entries()` → `for e := range c.All()`
  - `coverage/gocover.go:49`: same shape

- [ ] Update `coverage/coverage_test.go` if any tests call `Entries()` (grep before; rename or rewrite as needed).

- [ ] `make lint && make covercheck`.

- [ ] Commit: `refactor(coverage): replace Collector.Entries with All() iter.Seq[Entry]`.

### Phase 2 — `machine/compilation.LibraryExportIndex.All`

- [ ] In `machine/compilation/library_export_index.go`, replace `Entries() []*LibrarySummary` (line 61) with `All() iter.Seq[*LibrarySummary]`. Internal sort (line 69-71) stays; closure yields from the sorted slice.

  ```go
  // All returns an iter.Seq yielding all indexed summaries in library-key
  // sort order. The internal sort is performed once per call.
  func (p *LibraryExportIndex) All() iter.Seq[*LibrarySummary]
  ```

- [ ] Migrate the 2 call sites:
  - `library_info.go:86`: `for _, summary := range idx.Entries()` → `for summary := range idx.All()`
  - `registry/search.go:258`: same shape

- [ ] Update `machine/compilation/library_export_index_test.go` if any tests call `Entries()`.

- [ ] `make lint && make covercheck`.

- [ ] Commit: `refactor(compilation): replace LibraryExportIndex.Entries with All() iter.Seq[*LibrarySummary]`.

## Risk

Both phases are mechanical renames + signature changes. Surface area:

- **Tests asserting on the literal name `Entries`.** Grep before each phase; rename usages.
- **External embedders importing `LibraryExportIndex.Entries`.** Per CLAUDE.md ("v1.x with zero consumers — break freely") this is acceptable. Add a CHANGELOG entry noting the rename so any future adopter sees the precedent.
- **Mutex semantics in `Collector`.** Verify the materialized slice is built fully before the iterator function returns. The closure must not re-acquire the mutex — that would change the concurrency contract.

## LOC delta

Approximately net-zero per migration:
- Removes `Entries() []T` (~8-12 lines including materialization)
- Adds `All() iter.Seq[T]` (~10-14 lines including the closure)
- Each call site is one keyword shorter (`for _, e := range` → `for e := range`)

Cumulative across both phases: ~+3 LOC, +1 import (`iter`) in two packages, 4 call sites tightened.

## Conventions established

When this plan ships, the following are project-canonical:

- **`All() iter.Seq[T]`** is the canonical name for primary Go-internal iteration accessors (mirrors `slices.All`, `maps.All`).
- **`iter` package** is approved for new Go-internal iteration code. Existing slice-returning accessors are migrated only when friction surfaces, per the policy in `docs/dev/iteration-idioms.md`.
- **Tier-2 vs Tier-3 distinction** is the primary screening question: if the snapshot exists for safety (concurrency, deep-copy invariants), keep the slice form; if it exists purely for caller convenience, migrate to `iter.Seq`.

Future iteration accessors on Go-internal types should follow this shape unless they fall into Tier 1 / 3 / 4 of `docs/dev/iteration-idioms.md`.
