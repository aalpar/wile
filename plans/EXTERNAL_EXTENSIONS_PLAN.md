# External Extensions Plan

**Status:** PROPOSED — Not implemented

> **Cross-reference**: Plugin architecture (Phases 1-3, 5-6) is complete. This plan covers the remaining Phase 4: external extraction.

## Problem

Extensions live in `internal/extensions/`, blocking external repos from linking. External extensions can't be `go get`-installed.

## Audit: What Extensions Need

Extensions use **14 distinct operations** from the environment package:

| Category | Operations |
|----------|-----------|
| Symbol interning | `InternSymbol` |
| Global bindings | `MaybeCreateOwnGlobalBinding`, `SetOwnGlobalValue`, `NewGlobalIndex` |
| Phase access | `Runtime()`, `Expand()`, `Compile()` |
| Scope/frame navigation | `TopLevel()`, `TopLevelEnv()` |
| Hygienic lookup | `GetBindingWithScopes` |
| First-class environments | `NewChildTopLevelEnvironment` |
| Constants | `BindingTypeVariable`, `BindingTypePrimitive` |

## Solution: Widen `ApplyContext` Interface

Add `EnvironmentAccess` interface to `registry/` providing only the operations extensions need:

| Method | Purpose |
|--------|---------|
| `InternSymbol(sym)` | Symbol interning |
| `DefineVariable(name, value)` | Register runtime values |
| `DefineParameter(name, param)` | Register parameter objects (current-input-port, etc.) |
| `Runtime()` / `Expand()` / `Compile()` | Phase accessors |

Advanced (Phase 4): `LookupWithScopes`, `NewChildEnvironment`, `TopLevel()`, `Frame()`.

## Migration Phases

| Phase | Description |
|-------|-------------|
| 1 | Define `EnvironmentAccess` interface in `registry/` |
| 2 | Implement adapter wrapping `*environment.EnvironmentFrame` |
| 3 | Migrate internal extensions to new interface |
| 4 | Handle advanced use cases (hygienic lookup, first-class environments) |
| 5 | Deprecate `ApplyContext.Environment()`, promote `Env()` |
| 6 | Extract extensions to separate repos |

## Future Repo Structure

| Current | External |
|---------|----------|
| `internal/extensions/io` | `github.com/aalpar/wile-io` |
| `internal/extensions/files` | `github.com/aalpar/wile-files` |
| `internal/extensions/eval` | `github.com/aalpar/wile-eval` |
| etc. | `github.com/aalpar/wile-all` (meta-package) |

## Open Questions

1. Should `syntax.Scope` be public or wrapped in opaque `ScopeSet` type?
2. Should `registry.Parameter` be interface or concrete type?
3. Should `DefineVariable` return error or panic on failure?
4. Extension inter-dependencies (e.g., kubernetes depends on io)?

## Files Changed

| File | Change |
|------|--------|
| `registry/apply_context.go` | New — `EnvironmentAccess` interface |
| `registry/env_access.go` | New — adapter implementation |
| `registry/apply.go` | Add `Env()` method |
| `internal/extensions/io/register.go` | Migrate to `Env().DefineParameter()` |
| `internal/extensions/eval/prim_eval.go` | Migrate to `EnvironmentAccess` |
| + 5 more extension files | Similar migration |
