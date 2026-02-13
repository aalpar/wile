# Plan: Authorization Framework

**Status**: Planned — Not started

## Motivation

Reddit feedback: embedded scripting languages for Go lack fine-grained resource access control. Wile has coarse-grained control (extensions are opt-in), but once an extension is loaded it's all-or-nothing. The gap is **fine-grained policy within enabled extensions**.

## Architecture

K8s-style `Check(AccessRequest) error` with an open vocabulary of resources and actions. One `Authorizer` interface, stable forever. Extensions define their own resource/action vocabulary without changing the security package.

```
AccessRequest { Resource string, Action string, Target string }
Authorizer    { Authorize(AccessRequest) error }
```

### Context propagation via `context.Context`

`context.Context` is the only vehicle available at all three sites (runtime primitives, compile-time `include`, library loading) without structural changes.

```
Engine.Eval(ctx) → security.WithAuthorizer(ctx, authorizer) → flows to all sites
```

### Package: `security/` (new, public, zero deps on wile)

| File | Purpose |
|------|---------|
| `access.go` | `AccessRequest`, resource/action constants |
| `authorizer.go` | `Authorizer` interface, `AuthorizerFunc`, `ErrAccessDenied` |
| `context.go` | `WithAuthorizer`, `FromContext`, `Check` |
| `filesystem_root.go` | Restricts file/code ops to a directory tree |
| `read_only.go` | Allows reads/stats, denies writes/deletes |
| `deny_all.go` | Blocks everything |
| `composite.go` | AND combinator (all must allow) |

### Well-known constants

Core: `file`, `code`, `env`, `process` resources; `read`, `write`, `delete`, `stat`, `load`, `exit` actions. Extensions define their own (e.g., `net`/`connect`).

## Integration Points

| File | Primitives Gated | Resource | Action |
|------|-----------------|----------|--------|
| `internal/extensions/files/prim_files.go` | open-input/output-file, file-exists?, delete-file, call-with-*-file | `file` | read/write/stat/delete |
| `internal/extensions/system/prim_system.go` | get-environment-variable(s), exit, emergency-exit | `env`/`process` | read/exit |
| `internal/extensions/eval/prim_eval.go` | `load` | `code` | `load` |
| `machine/compile_time_continuation.go` | `include`, `include-ci` | `code` | `load` |
| `machine/library_loader.go` | library `import` | `code` | `load` |

All sites already have `context.Context`. No plumbing changes needed.

## Design Decisions

- **One sentinel** (`ErrAccessDenied`) — `AccessRequest` fields carry domain info
- **K8s two-state** (allow/deny) instead of three-state — embedders configure one authorizer
- **Immutable after construction** — no `SetAuthorizer()`, prevents TOCTOU
- **Denied `include` paths skip** (not error) — no information leakage about sandbox contents
- **No `Extra` field** on `AccessRequest` — three fields cover all foreseeable needs
- **Go 1.24 upgrade path** — `FilesystemRoot` can use `os.Root` internally without API changes

## Phases

| Phase | Description | Deps |
|-------|-------------|------|
| 1 | `security/` package — interface, context, sentinels, constants | None |
| 2 | Built-in authorizers (FilesystemRoot, ReadOnly, DenyAll, Composite) | 1 |
| 3 | Engine integration — `WithAuthorizer` option, wrap ctx in public methods | 1 |
| 4 | Gate runtime primitives (files, system, eval) | 3 |
| 5 | Gate compile-time code loading (include, import) | 3 |
| 6 | Integration tests | 4, 5 |

Phases 4 and 5 are independent of each other.

## Scope Boundaries

- **Not covered**: Load path resolution (see `LOAD_PATH_STACK.md`), network primitives (not yet), resource limits (CPU/memory/stack — separate concern)
- **Composes with load-path stack**: path → [resolve] → [authorization check] → os.Open
