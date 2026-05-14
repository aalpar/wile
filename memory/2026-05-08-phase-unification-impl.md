# Phase unification — Instance B implementation plan

**Date**: 2026-05-08
**Type**: Implementation plan
**Source**: `plans/2026-05-08-dispatch-axis-as-data.md` Instance B
**Branch (proposed)**: `refactor/phase-unification`
**Status**: ✅ **SHIPPED** — PR #728 (`refactor/phase-unification`), merge commit `2b271a41`.

## Why

Two `Phase` types exist with conflicting values for the same name:

- `registry.Phase` — bit-flag (`type Phase int`, values 1, 2, 4 via `1 << iota`)
- `environment.PhaseRuntime` etc. — untyped `int` constants (values -1, 0, 1, 2)

Public API at `wile/options.go:36-37` re-exports the *environment* values
(0, 1, 2). Embedders writing extension code see `wile.PhaseExpand = 1`
while `registry.PhaseExpand = 2`. Same name, two values, both load-bearing.

This plan splits the concept along its real axes:

- **`environment.Phase`**: a typed enum naming a phase of evaluation.
  Used for indexing into `PhaseRegistry.envs`, for the `phaseLevel` field
  on `EnvironmentFrame`, for public API (`wile.PhaseExpand` etc.).
- **`registry.PhaseSet`**: a bitset over non-negative `environment.Phase`
  values. Used for declaring "this primitive is available at these
  phases" in `PrimitiveRegistration.Phases`.

After this change, the two abstractions stop sharing names. The names
that remain — `environment.PhaseRuntime`, `registry.PhaseSetRuntime` —
are unambiguous about what they encode.

## Decisions (per `2026-05-08-dispatch-axis-as-data.md` brainstorm)

| Q | Decision | Rationale |
|---|---|---|
| Q1 | **Two types**: `environment.Phase` (enum) + `registry.PhaseSet` (bitset) | Operations differ — indexing vs set construction. Two types means each operation has one valid encoding. Matches `time.Weekday` (enum) vs `os.FileMode` (bits) split in stdlib. |
| Q2 | **`environment/` owns canonical `Phase`** | Already exposed via public API; phase is fundamentally an environment concept (it names a *time/scope*). Registry depends on environment, never the reverse. |
| Q3 | **Match `environment/` values** (Template=-1, Runtime=0, Expand=1, Compile=2). Bit-flag derives via `1 << uint(phase)` for non-negative phases. | Avoids breaking the public API constants. PhaseSet bits derive from indices, not the other way around. |
| Q4 | **Skip Template registration vocab in this plan** | `PhaseTemplate = -1` is currently defined but unused — already a deferred concern. Template can't appear in a `PhaseSet` (bitset over non-negative phases only). If Template-phase registration is ever needed, that's a separate plan. Not adding the asymmetry here keeps scope narrow. |
| Q5 | **Big-bang migration**, single PR | Compile-error sweep is bounded (~83 callsites, mostly mechanical). Staged migration would require a third transitional encoding — strictly worse during the window. v1.x with no consumers permits this. |

## Target API

### environment/

```go
// Phase identifies a stage of compilation/evaluation.
// Indexes into PhaseRegistry.envs and EnvironmentFrame.phaseLevel.
type Phase int8

const (
    PhaseTemplate Phase = -1  // for-template (currently unused; see TODO)
    PhaseRuntime  Phase = 0   // runtime execution
    PhaseExpand   Phase = 1   // macro expansion
    PhaseCompile  Phase = 2   // compile-time bindings
)

func (p Phase) String() string { ... }  // "runtime", "expand", "compile", "template"
```

Existing signatures change `int → Phase`:

```go
// Was:
func (p *PhaseRegistry) Get(phase int) *EnvironmentFrame
func (p *PhaseRegistry) GetOrCreate(phase int) *EnvironmentFrame
func (p *PhaseRegistry) Phases() []int
type PhaseRegistry struct { envs map[int]*EnvironmentFrame; ... }
type EnvironmentFrame struct { phaseLevel int; ... }
func (p *EnvironmentFrame) PhaseLevel() int

// Now:
func (p *PhaseRegistry) Get(phase Phase) *EnvironmentFrame
func (p *PhaseRegistry) GetOrCreate(phase Phase) *EnvironmentFrame
func (p *PhaseRegistry) Phases() []Phase
type PhaseRegistry struct { envs map[Phase]*EnvironmentFrame; ... }
type EnvironmentFrame struct { phaseLevel Phase; ... }
func (p *EnvironmentFrame) PhaseLevel() Phase
```

### registry/

```go
// PhaseSet is a bitset over non-negative environment.Phase values,
// used to declare which phases a primitive is registered for.
//
// PhaseTemplate (-1) cannot appear in a PhaseSet.
type PhaseSet uint8

const (
    PhaseSetRuntime PhaseSet = 1 << 0  // matches environment.PhaseRuntime
    PhaseSetExpand  PhaseSet = 1 << 1  // matches environment.PhaseExpand
    PhaseSetCompile PhaseSet = 1 << 2  // matches environment.PhaseCompile
)

// Has reports whether p is in the set. Returns false for negative phases.
func (s PhaseSet) Has(p environment.Phase) bool {
    if p < 0 {
        return false
    }
    return s&(1<<uint(p)) != 0
}

// With returns a new PhaseSet with p added.
// Panics if p is negative (PhaseTemplate cannot be set).
func (s PhaseSet) With(p environment.Phase) PhaseSet {
    if p < 0 {
        panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
            "registry.PhaseSet.With: negative phase %d not supported", p))
    }
    return s | (1 << uint(p))
}

func (s PhaseSet) String() string { ... }  // "runtime|expand", etc.

// Replaces the deleted methods:
//   HasRuntime()  → Has(environment.PhaseRuntime)
//   HasExpand()   → Has(environment.PhaseExpand)
//   HasCompile()  → Has(environment.PhaseCompile)
```

Type rename in `PrimitiveRegistration`:

```go
// Was:
type PrimitiveRegistration struct {
    Spec   PrimitiveSpec
    Phases Phase            // the bit-flag type
}

// Now:
type PrimitiveRegistration struct {
    Spec   PrimitiveSpec
    Phases PhaseSet
}
```

Method-receiver registry methods change `Phase → PhaseSet`:

```go
func (p *Registry) AddPrimitive(spec PrimitiveSpec, phases PhaseSet)
func (p *Registry) AddPrimitives(specs []PrimitiveSpec, phases PhaseSet)
func (p *Registry) FindPrimitive(name string, phase PhaseSet) (PrimitiveRegistration, bool)
func (p *Registry) HasPrimitive(name string, phase PhaseSet) bool
```

### Init-time assertion

```go
// In registry/phase.go init():
func init() {
    if PhaseSetRuntime != 1<<uint(environment.PhaseRuntime) ||
       PhaseSetExpand  != 1<<uint(environment.PhaseExpand)  ||
       PhaseSetCompile != 1<<uint(environment.PhaseCompile) {
        panic("registry: PhaseSet bit values out of sync with environment.Phase")
    }
}
```

This catches future drift if anyone reorders or renumbers
`environment.Phase` constants without updating `PhaseSet`.

### Public API (`wile/options.go`)

```go
// Was:
const (
    PhaseExpand  = environment.PhaseExpand   // untyped int 1
    PhaseCompile = environment.PhaseCompile  // untyped int 2
)

// Now (these become typed environment.Phase values):
const (
    PhaseRuntime  = environment.PhaseRuntime   // typed Phase 0
    PhaseExpand   = environment.PhaseExpand    // typed Phase 1
    PhaseCompile  = environment.PhaseCompile   // typed Phase 2
    PhaseTemplate = environment.PhaseTemplate  // typed Phase -1
)

// Type alias for embedders:
type Phase = environment.Phase
```

`PhaseRuntime` is added to the public re-exports (it's currently absent
— another mild asymmetry this plan closes).

## Phases

### Phase 1 — Add typed `environment.Phase`

Touches: `environment/phase_registry.go`, `environment/environment_frame.go`,
`environment/namespace.go`.

**Tasks:**

1. Define `type Phase int8` in `environment/phase_registry.go`. Type the
   four existing constants. Add `String()` method.
2. Change `PhaseRegistry.envs map[int]*EnvironmentFrame` →
   `map[Phase]*EnvironmentFrame`. Update `Get(phase int) →
   Get(phase Phase)` and `GetOrCreate(phase int) → GetOrCreate(phase Phase)`.
   Update `Phases() []int → Phases() []Phase`. Update `createPhaseEnv`
   signature.
3. Change `EnvironmentFrame.phaseLevel int → phaseLevel Phase`. Update
   `PhaseLevel() int → PhaseLevel() Phase`. Update the
   `[3]int{PhaseRuntime, PhaseExpand, PhaseCompile}` literal at
   `environment_frame.go` to `[3]Phase{...}`.
4. Update internal callers within `environment/` — `Runtime()`,
   `Expand()`, `Compile()`, `AtPhase(int)` → `AtPhase(Phase)`.
5. Update tests in `environment/phase_registry_test.go` and others as
   needed for the typed signatures.
6. `make lint && go test ./environment/...` — must pass.

**Commit**: `refactor(environment): introduce typed Phase enum`

### Phase 2 — Update direct environment.Phase consumers in machine/

Touches: `machine/compilation/library_bindings.go`,
`machine/compilation/compile_import.go`,
`machine/compilation/expander_primitive_forms.go`,
`machine/compilation/library_registry.go`.

These pass phases to `env.AtPhase(...)` or compare to the constants.
With the typed `environment.Phase` they should auto-convert (typed
constant context); raw int callers (if any) get a compile error.

**Tasks:**

1. Compile and fix any errors in `machine/compilation/`.
2. `go test ./machine/...` — must pass.

**Commit**: `refactor(machine/compilation): adopt typed environment.Phase`

### Phase 3 — Rename registry.Phase → PhaseSet, replace HasX with Has(phase)

Touches: `registry/phase.go`, `registry/apply.go`, `registry/registry.go`,
`registry/builder.go`, `registry/extension.go`, `registry/search.go`,
`registry/doc.go`, plus tests.

**Tasks:**

1. Rename `registry.Phase` type → `registry.PhaseSet`.
2. Rename constants: `PhaseRuntime → PhaseSetRuntime`,
   `PhaseExpand → PhaseSetExpand`, `PhaseCompile → PhaseSetCompile`.
   Keep the same values (1, 2, 4).
3. Replace methods `HasRuntime/HasExpand/HasCompile` with single
   `Has(p environment.Phase) bool`. Add `With(p environment.Phase) PhaseSet`.
4. Update `PhaseSet.String()` to iterate
   `[]environment.Phase{PhaseRuntime, PhaseExpand, PhaseCompile}`.
5. Add `init()` assertion that bit values match environment indices.
6. Update `Registry.AddPrimitive`, `AddPrimitives`, `FindPrimitive`,
   `HasPrimitive`, `RuntimePrimitiveNamesSince/Range`, `PrimitiveRegistration.Phases`
   — all `Phase → PhaseSet`.
7. Update `apply.go`'s 4 phase-check sites: `reg.Phases.HasX()` →
   `reg.Phases.Has(environment.PhaseX)`. Add the import.
8. Update `registry/phase_test.go` and other tests.
9. `make lint && go test ./registry/...` — must pass.

**Commit**: `refactor(registry): rename Phase → PhaseSet, replace HasX with Has(phase)`

### Phase 4 — Update all callers of registry.PhaseRuntime etc.

Touches: 83 callsites across `extensions/*/register.go`,
`internal/extensions/*/register.go`, `registry/core/*.go`,
`internal/testutil/ready_extension.go`, root `engine_library_test.go`.

This is the mechanical sweep.

**Tasks:**

1. Search/replace `registry.PhaseRuntime` → `registry.PhaseSetRuntime`
   (and Expand/Compile equivalents) across the codebase.
2. Update any composed expressions:
   `registry.PhaseRuntime|registry.PhaseExpand` →
   `registry.PhaseSetRuntime|registry.PhaseSetExpand`.
3. `make lint && make test` — full suite must pass.

**Commit**: `refactor: rename callsites for registry.PhaseSet*`

### Phase 5 — Public API alignment

Touches: `options.go` (root), `engine_sandbox_test.go` (uses `wile.PhaseExpand`).

**Tasks:**

1. Add `PhaseRuntime` and `PhaseTemplate` re-exports to `options.go`.
   These were absent.
2. Add `type Phase = environment.Phase` alias for ergonomic embedder use.
3. Verify `engine_sandbox_test.go` compiles unchanged
   (`wile.PhaseExpand` is now a typed `Phase` rather than untyped int —
   the equality check should still type-check).
4. `make ci` — full lint + test + cover.

**Commit**: `refactor(wile): expose typed Phase + add missing constants`

### Phase 6 — Final lint and cross-cutting verification

**Tasks:**

1. `make lint && make covercheck && make ci` — all green.
2. Walk the diff with `crosscheck:crosscheck all` and address findings
   (per `plans/CLAUDE.md` § "Implementation Completion Workflow").
3. Address any findings from `crosscheck`. Push fixup commit if needed.

**Commit (if needed)**: `fix: address crosscheck findings on phase-unification PR`

## Test plan

- All existing tests pass.
- `registry/phase_test.go` updated to test:
  - `PhaseSet.Has(p)` returns the same answers as the old
    `HasRuntime`/`HasExpand`/`HasCompile`.
  - `PhaseSet.Has(environment.PhaseTemplate)` returns false (negative
    phase rejected).
  - `PhaseSet.With(p)` is monotonic and idempotent.
  - The init-time assertion fires if the bit values are tampered with
    (white-box: temporarily reassign and call `init()` — or simply
    document the assertion).
- `environment/phase_registry_test.go` updated to use typed `Phase`
  values. Existing assertions hold.
- Sandbox sandbox tests in `engine_sandbox_test.go` still type-check
  with `wile.PhaseExpand` as a typed `Phase`.

## Risk and rollback

**Risks:**

- *Subtle implicit conversions* in callers passing raw `int` to APIs
  that now want `Phase`. These should be caught at compile time but
  may show up in code that uses `int` arithmetic on phase values
  (e.g., `phase + 1`). Mitigation: walk the diff for any `int(...)`
  conversions to `Phase` and verify each is intentional.
- *Test infrastructure* using raw integer constants. Likely small;
  fix in Phase 1 or Phase 4.
- *Public API ergonomics*: Embedders comparing `wile.PhaseExpand` with
  `int` literals will now fail to compile. Fix is `int(wile.PhaseExpand)`
  or update to use the typed value. v1.x permits this break.

**Rollback**: revert the entire branch. The change is structured as
phase-by-phase commits; `git revert <branch>` returns to current
master cleanly.

## Done criteria

Per the dispatch-axis-as-data document's "done criteria" section:

- [ ] **The axis is named in code with one type definition.** ✓ Once
      `environment.Phase` is the single canonical type.
- [ ] **No second package declares constants at conflicting values for
      the same conceptual position.** ✓ Once `registry.PhaseRuntime`
      etc. are renamed to `PhaseSetRuntime` etc.
- [ ] **Coverage of the axis is enforced** — `init()` assertion in
      `registry/phase.go` catches drift. The Go compiler enforces
      `Phase` typing on indexing.
- [ ] **Adding an axis-point requires changes at one site.** Adding a
      new phase: extend `environment.Phase` constant list, add a
      matching `PhaseSet*` constant in registry — two sites for two
      type-distinct uses, but no replication across N callers.
- [ ] **The "ADDING A NEW PHASE" guide comment, if any, references
      these two sites and nothing else.**

## Cross-references

- `plans/2026-05-08-dispatch-axis-as-data.md` — the synthesis document
  that identified Instance B and proposed this plan as the first move.
- `plans/2026-05-07-internal-structural-reduction.md` — its Phase
  references (validators, expanders) will silently use the new typed
  `Phase` once this plan lands; no plan changes required.
- `plans/2026-05-06-machine-structural-reduction.md` — likewise.
- Once Instance B (this plan) lands, Instance C (collapse the four
  phase loops in `registry/apply.go`) becomes a small follow-up:
  the `phaseTable` literal can use `environment.Phase` values directly,
  and the per-phase target functions can be method values on
  `*EnvironmentFrame` (`Compile`, `Runtime`-via-identity, `Expand`).
  That follow-up is a separate impl plan.

## Open questions (none expected; surface here if any arise during impl)

_None at this time. Decisions Q1–Q5 settled per the synthesis brainstorm._
