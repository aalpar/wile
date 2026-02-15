# External Extensions Plan

**Status:** PROPOSED — Revised 2026-02-14

> **Cross-reference**: Plugin architecture (Phases 1-3, 5-6) is complete. This plan covers the remaining Phase 4: external extraction.

## Problem

Extensions live in `internal/extensions/`, blocking external repos from linking. External extensions can't be `go get`-installed.

## Audit: What Actually Blocks Extraction

The original plan identified `ApplyContext.Environment()` as the blocker and proposed an `EnvironmentAccess` interface. A code audit found the real blockers are **`internal/` package imports**, not the `ApplyContext` interface.

### Actual `internal/` Dependencies by Extension

| Extension | `internal/schemeutil` | `internal/parser` | `internal/syntax` | `internal/tokenizer` | Environment access |
|-----------|:---:|:---:|:---:|:---:|---|
| system | — | — | — | — | `InternSymbol` only |
| math | predicate | — | — | — | None |
| gointerop | predicate | — | — | — | None |
| exceptions | predicate | — | — | — | None |
| files | predicate | — | — | — | None |
| threads | predicate | — | — | — | None |
| all | predicate | — | — | — | `TopLevel`, closures |
| io | predicate | ✓ | ✓ | ✓ | Pass env to parser |
| eval | predicate+syntax | ✓ | ✓ | — | Full pipeline |

**Key finding**: `schemeutil.BoolToBoolean` (and friends) depend only on `values/`. Moving them to a public package unblocks 7 of 9 extensions. No new interface needed.

### `schemeutil` Function Usage

| Function | Depends on | Used by |
|----------|-----------|---------|
| `BoolToBoolean(bool)` | `values` only | all, math, io, files, threads, exceptions, gointerop |
| `ValueToBool(Value)` | `values` only | threads |
| `BooleanToBool(*Boolean)` | `values` only | (internal callers) |
| `ValueToBoolean(Value)` | `values` only | (internal callers) |
| `DatumToSyntaxValue(...)` | `values`, `syntax` | eval only |
| `SyntaxValueToDatum(...)` | `values`, `syntax` | (internal callers) |
| `IsSyntaxComment(...)` | `syntax` | (internal callers) |

The predicate functions (`BoolToBoolean`, `ValueToBool`, `BooleanToBool`, `ValueToBoolean`) depend only on `values/` and are the sole `internal/` import for 6 extensions. The syntax functions are used only by `eval`, which stays in-tree regardless.

### Extractability Tiers

```
Tier 1 — Extractable after moving predicate utils:
  ┌──────────────────────────────────────────────────┐
  │ system      zero internal/ imports               │
  │ math        schemeutil.BoolToBoolean only         │
  │ gointerop   schemeutil.BoolToBoolean only         │
  │ exceptions  schemeutil.BoolToBoolean only         │
  │ files       schemeutil.BoolToBoolean only         │
  │ threads     schemeutil.BoolToBoolean+ValueToBool  │
  └──────────────────────────────────────────────────┘

Tier 2 — Requires parser/tokenizer publicity decisions:
  ┌──────────────────────────────────────────────────┐
  │ io          parser, tokenizer, syntax             │
  │ all         schemeutil + environment (closures)   │
  └──────────────────────────────────────────────────┘

Tier 3 — Stays in-tree (full compiler coupling):
  ┌──────────────────────────────────────────────────┐
  │ eval        parser, syntax, machine expander,     │
  │             machine compiler, environment         │
  └──────────────────────────────────────────────────┘
```

## Why the Original `EnvironmentAccess` Interface Was Wrong

The original plan proposed a new `EnvironmentAccess` interface with methods (`DefineVariable`, `DefineParameter`, `Runtime()`/`Expand()`/`Compile()`). This was misaligned:

1. **`DefineVariable`/`DefineParameter`** — Only needed by `io`'s `registerPortParameters`, which is the sole `ApplyContext.Environment()` call site. Refactoring that one function eliminates the need.
2. **`Runtime()`/`Expand()`/`Compile()`** — Called by `Registry.Apply()` (registry infrastructure), not by extensions.
3. **Missing actual operations** — Extensions use `TopLevel()`, `TopLevelEnv()`, `GetLocalBindingByIndex()`, none of which were in the proposed interface.
4. **`MachineContext` already provides the real API** — Extensions access the environment through `mc.EnvironmentFrame()`, which is already public. Adding a parallel access path creates confusion.

## Revised Plan

### Phase 1: Move Predicate Utilities to `values/`

Move the four predicate functions from `internal/schemeutil/predicate.go` to `values/bool.go`:

```go
// values/bool.go
func BoolToBoolean(b bool) *Boolean
func BooleanToBool(b *Boolean) bool
func ValueToBool(v Value) bool
func ValueToBoolean(v Value) *Boolean
```

These depend only on types already in `values/`. Update all call sites (`internal/schemeutil.BoolToBoolean` → `values.BoolToBoolean`).

Leave `syntax.go` functions in `internal/schemeutil` — only `eval` uses them, and `eval` stays in-tree.

**Files changed:**
| File | Change |
|------|--------|
| `values/bool.go` | New — predicate functions |
| `values/bool_test.go` | New — moved tests |
| `internal/schemeutil/predicate.go` | Remove moved functions (keep if syntax funcs need them, otherwise delete) |
| `internal/extensions/*/prim_*.go` | Update imports (8 extensions) |
| `machine/compile_*.go` etc. | Update any internal callers |

**Outcome:** 6 of 9 extensions have zero `internal/` imports.

### Phase 2: Refactor Port Parameter Registration

`io`'s `registerPortParameters` manually calls `InternSymbol`, `MaybeCreateOwnGlobalBinding`, `SetOwnGlobalValue` — reimplementing what `Registry.Apply()` already does. This is the only consumer of `ApplyContext.Environment()`.

Add a `Registry.AddGlobalValue(name string, value values.Value)` method that defers the intern+bind+set to `Apply()` time. Then `io` registers port parameters like:

```go
func Register(r *registry.Registry) error {
    r.AddGlobalValue("current-input-port", GetCurrentInputPortParam())
    r.AddGlobalValue("current-output-port", GetCurrentOutputPortParam())
    r.AddGlobalValue("current-error-port", GetCurrentErrorPortParam())
    // ...
}
```

This eliminates `addPortState`'s `InitFunc`, which eliminates the only `ApplyContext.Environment()` call site.

**Files changed:**
| File | Change |
|------|--------|
| `registry/registry.go` | Add `AddGlobalValue` method + storage |
| `registry/apply.go` | Process global values during `Apply()` |
| `internal/extensions/io/register.go` | Replace `addPortState`/`registerPortParameters` with `AddGlobalValue` calls |

**Outcome:** `ApplyContext.Environment()` has zero extension callers.

### Phase 3: Deprecate `ApplyContext.Environment()`

With no extension callers, `ApplyContext.Environment()` can be deprecated. Options:

- **Option A**: Remove `Environment()` from the `ApplyContext` interface entirely. `InitFunc` still receives the context but can't reach the raw environment.
- **Option B**: Keep the method but document it as internal-only. External extensions should not depend on it.

Either way, external extensions can only use `MachineContext` (for primitives) and `Registry` (for registration) — both already public with stable APIs.

**Files changed:**
| File | Change |
|------|--------|
| `registry/apply.go` | Remove or deprecate `Environment()` from `ApplyContext` |
| `registry/apply_test.go` | Update tests |

### Phase 4: Move Tier 1 Extensions Out of `internal/`

After Phases 1-3, these extensions import only public packages (`values/`, `registry/`, `registry/helpers/`, `machine/`). Move them from `internal/extensions/` to `extensions/` (still in-tree but importable):

```
internal/extensions/system     →  extensions/system
internal/extensions/math       →  extensions/math
internal/extensions/gointerop  →  extensions/gointerop
internal/extensions/exceptions →  extensions/exceptions
internal/extensions/files      →  extensions/files
internal/extensions/threads    →  extensions/threads
```

`internal/extensions/all` must be updated to import from the new locations.

**Outcome:** 6 extensions are now importable by external Go code.

### Phase 5 (Future): Extract to Separate Repos

Once Phase 4 is stable and the API boundary is proven, individual extensions can migrate to separate repos. This is a distribution concern, not an architecture concern — Phase 4 already achieves the importability goal.

| In-tree (`extensions/`) | Future external repo |
|-------------------------|---------------------|
| `extensions/system` | `github.com/aalpar/wile-system` |
| `extensions/math` | `github.com/aalpar/wile-math` |
| `extensions/gointerop` | `github.com/aalpar/wile-gointerop` |
| `extensions/exceptions` | `github.com/aalpar/wile-exceptions` |
| `extensions/files` | `github.com/aalpar/wile-files` |
| `extensions/threads` | `github.com/aalpar/wile-threads` |

### Tier 2 Extensions (Deferred)

`io` and `all` require further design decisions:

- **`io`**: Needs `internal/parser`, `internal/tokenizer`, `internal/syntax` for `read`/`read-syntax`/`read-token`. Extracting `io` requires either making the parser public or splitting `io` into extractable port primitives vs. in-tree read primitives.
- **`all`**: Needs environment frame for record closures via `machine.NewForeignClosure`. The environment package is already public, but the usage pattern ties closures to `EnvironmentFrame` internals.

Both are deferred until Tier 1 extraction proves the pattern.

### `eval` (Stays In-Tree Permanently)

`eval` imports the full compiler pipeline: parser, syntax, machine expander, machine compiler, environment. It IS the compiler exposed to Scheme. Extracting it would require making half of `machine/`'s internals public. Cost-benefit is clearly negative.

## Open Questions (Resolved)

| Question | Resolution |
|----------|-----------|
| `syntax.Scope` public vs opaque? | Not needed — only `eval` uses scopes, stays in-tree |
| `registry.Parameter` interface vs concrete? | Moot — port params use `AddGlobalValue`, no `Parameter` type needed |
| `DefineVariable` error vs panic? | Moot — `DefineVariable` method no longer proposed |
| Extension inter-dependencies? | `files` → `io` is test-only, not production. Tier 1 has zero inter-dependencies |

## Open Question (New)

1. **Where to put predicate functions**: `values/bool.go` vs `registry/helpers/bool.go`? `values/` is lower-level and semantically correct (Go↔Scheme value conversion). `registry/helpers/` is where extension helpers already live. Recommendation: `values/` — it has no dependency implications and is the most natural location.

## Summary

```
Phase 1: Move BoolToBoolean et al. to values/     → unblocks 7/9 extensions
Phase 2: Refactor io port param registration       → eliminates ApplyContext.Environment() usage
Phase 3: Deprecate ApplyContext.Environment()       → clean API boundary
Phase 4: Move Tier 1 to extensions/ (public)        → 6/9 extensions importable
Phase 5: Extract to separate repos (future)         → distribution concern only
```

No new interfaces. No new abstraction layers. The existing public API (`MachineContext`, `Registry`, `values/`, `registry/helpers/`) is sufficient.
