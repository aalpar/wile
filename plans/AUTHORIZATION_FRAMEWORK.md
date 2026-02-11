# Plan: Authorization Framework

**Status**: Planned

## Motivation

Reddit user feedback: embedded scripting languages for Go lack resource access control, making them less useful for untrusted code. Specifically requested: restricting file access to a directory (Go's `os.Root`), restricting or disabling network access.

Wile already has coarse-grained capability control -- extensions are opt-in. `NewEngine()` with no extensions cannot touch the filesystem or call `exit`. But once `files.Ext` is loaded, it's all-or-nothing. The gap is **fine-grained policy within enabled extensions**.

### Why not method-per-domain

The previous `SECURITY_POLICY.md` plan defined a `SecurityPolicy` interface with one method per protected domain: `CheckFile`, `CheckCodeLoad`, `CheckEnvAccess`, `CheckProcessExit`. This design has a structural flaw: **extensions cannot define new protected operations**. When a network extension arrives, the interface needs `CheckNetConnect`. When an FFI extension arrives, `CheckForeignCall`. Each new domain is a breaking change.

Both Kubernetes RBAC and Java's SecurityManager (Java 2 onward) solved this same problem by collapsing all authorization into a single check that takes a **descriptor** of the requested operation:

- **Kubernetes**: `Can <subject> perform <verb> on <resource>?` -- one `SubjectAccessReview` struct checked against any number of authorization backends. Adding a CRD doesn't change the authorization interface.
- **Java 2**: `AccessController.checkPermission(Permission)` -- one method, extensible via `Permission` subclasses. Extensions define new permission types without touching the security framework.

This plan adopts the Kubernetes model: a single `Check(AccessRequest) error` method with an open vocabulary of resources and actions.

## Scope

This plan covers the authorization interface, context propagation, built-in authorizers, and integration with all security-sensitive primitives. It does NOT cover:

- Load path resolution mechanics (see `LOAD_PATH_STACK.md` -- in progress, separate feature)
- Network primitives (don't exist yet -- plan provides the extension point)
- Resource limits (CPU time, memory, stack depth -- separate concern)

## Interaction with LOAD_PATH_STACK.md

The load-path stack determines **where** files are found. The authorizer determines **whether** access is allowed. They compose sequentially:

```
path -> [resolve to absolute path] -> [authorization check] -> os.Open
              load-path stack              this plan
```

No changes to LOAD_PATH_STACK.md are required. The two features are independently implementable.

## Architecture

### The access request model

Every protected operation is described by the same struct:

```go
package security

// AccessRequest describes a protected operation.
// Resource and Action form an open vocabulary -- extensions define their
// own resource/action pairs without changing this type.
type AccessRequest struct {
    // Resource identifies the kind of thing being accessed.
    // Examples: "file", "env", "process", "code"
    // Extensions define their own: "net", "ffi", etc.
    Resource string

    // Action identifies the operation being performed on the resource.
    // Examples: "read", "write", "delete", "stat", "load", "exit"
    Action string

    // Target identifies the specific instance being accessed.
    // For files: the path. For env vars: the variable name.
    // For process exit: empty. For network: "host:port".
    Target string
}
```

Three fields. No `Extra map[string]string` -- if a future extension needs structured metadata beyond a single target string, it can encode it in target or we revisit then. Minimalism now; extend under pressure later.

### The authorizer interface

```go
// Authorizer decides whether an operation is allowed.
//
// Implementations must be safe for concurrent use.
// Return nil to allow, non-nil error to deny.
// The error propagates to Scheme as a ForeignError.
type Authorizer interface {
    Authorize(AccessRequest) error
}
```

One method. Stable forever. Extensions declare their vocabulary; the authorizer decides based on the resource/action/target triple.

### The AuthorizerFunc adapter

```go
// AuthorizerFunc adapts a function to the Authorizer interface.
type AuthorizerFunc func(AccessRequest) error

func (f AuthorizerFunc) Authorize(req AccessRequest) error {
    return f(req)
}
```

Enables inline authorizers for simple cases:

```go
wile.NewEngine(
    wile.WithExtension(files.Ext),
    wile.WithAuthorizer(security.AuthorizerFunc(func(req security.AccessRequest) error {
        if req.Resource == "file" && req.Action == "write" {
            return security.ErrAccessDenied
        }
        return nil
    })),
)
```

### Kubernetes-style response semantics

K8s authorizers return one of: **allow**, **deny**, or **no opinion**. This plan simplifies to two: **allow** (`nil`) or **deny** (non-nil error). The `Composite` authorizer provides the "chain of authorizers" pattern if needed.

The K8s three-state model exists because K8s chains multiple independent authorizers (RBAC, webhook, node). Wile embedders configure a single authorizer per engine. If they need composition, `Composite` handles it. Adding a third state adds complexity without clear benefit for the embedding use case.

### Where the authorizer lives

The authorizer is an engine-level concern -- set once at construction, immutable thereafter. It flows to primitives via `context.Context`:

```
+-------------------------------------------------------------+
| Engine                                                       |
|   authorizer Authorizer (set via WithAuthorizer)             |
|                                                              |
|   Eval(ctx, code) {                                          |
|       ctx = security.WithAuthorizer(ctx, e.authorizer)       |
|       ...                                                    |
|   }                                                          |
+-------------------+------------------------------------------+
                    | ctx flows through
        +-----------+---------------+
        v           v               v
  MachineContext  CompileTime    LoadLibrary
  mc.Context()   ctctx.ctx      ctx param
        |           |               |
        v           v               v
   PrimOpenFile  findFile     loadLibraryFromFile
   -----------------------------------------
   err := security.Check(ctx, security.AccessRequest{
       Resource: security.ResourceFile,
       Action:   security.ActionRead,
       Target:   filename.Value,
   })
```

Why `context.Context`:

| Approach | Runtime | Compile-time (`include`) | Library loading |
|----------|---------|--------------------------|-----------------|
| `MachineContext` field | Yes | **No** | **No** |
| `context.Context` | Yes | Yes | Yes |

`context.Context` is the only vehicle available at all three sites without structural changes.

### Package placement

```
security/                    NEW -- public, zero deps on wile packages
+-- access.go               AccessRequest, well-known constants
+-- authorizer.go            Authorizer interface, AuthorizerFunc, sentinel errors
+-- context.go               WithAuthorizer / FromContext / Check
+-- filesystem_root.go       FilesystemRoot authorizer
+-- read_only.go             ReadOnly authorizer
+-- deny_all.go              DenyAll authorizer
+-- composite.go             Composite authorizer
+-- *_test.go

wile/ (root)
+-- options.go               WithAuthorizer option  (modify)
+-- engine.go                wrap ctx in Eval/Run    (modify)

internal/extensions/files/   Check before os.Open/Create/Remove  (modify)
internal/extensions/system/  Check before os.LookupEnv, os.Exit  (modify)
internal/extensions/eval/    Check before os.Open in load        (modify)
machine/                     Check in findFile, loadLibraryFromFile (modify)
```

`security/` is a new **public** package. Zero imports from other wile packages -- only stdlib (`context`, `errors`, `fmt`, `filepath`, `os`, `path/filepath`, `strings`). Embedders implement custom authorizers:

```go
import "github.com/aalpar/wile/security"

type myAuthorizer struct{ ... }
func (a *myAuthorizer) Authorize(req security.AccessRequest) error { ... }
```

## Well-Known Resource and Action Constants

Extensions declare their vocabulary as constants. The `security` package defines the core vocabulary; extension packages define their own.

### In `security/access.go`

```go
package security

// Well-known resources. Extensions define additional resources
// in their own packages.
const (
    ResourceFile    = "file"    // filesystem operations
    ResourceCode    = "code"    // code loading (load, include, import)
    ResourceEnv     = "env"     // environment variables
    ResourceProcess = "process" // process control (exit)
)

// Well-known actions. Extensions define additional actions
// in their own packages.
const (
    ActionRead   = "read"
    ActionWrite  = "write"
    ActionDelete = "delete"
    ActionStat   = "stat"
    ActionLoad   = "load"   // code loading (implies execution)
    ActionExit   = "exit"   // process exit
)
```

### In extension packages (future example)

```go
// In a hypothetical internal/extensions/net/ package
const (
    ResourceNet   = "net"
    ActionConnect = "connect"
    ActionListen  = "listen"
)
```

The authorizer receives these as strings. Unknown resource/action pairs are the authorizer's decision -- deny-unknown-by-default is trivial for embedders to implement.

## Sentinel Errors

```go
package security

import "errors"

var (
    // ErrAccessDenied is the root sentinel for all authorization denials.
    // Match with errors.Is(err, security.ErrAccessDenied).
    ErrAccessDenied = errors.New("security: access denied")
)
```

One sentinel. The `AccessRequest` fields provide the context (what resource, what action, what target). Call sites wrap with `values.WrapForeignErrorf` to add primitive-specific context.

The previous plan had `ErrFileNotAllowed`, `ErrLoadNotAllowed`, `ErrEnvNotAllowed`, `ErrExitNotAllowed`. With the unified model, the resource/action in the request provides this information. One sentinel is sufficient for programmatic matching.

Note: `errors.New` is used here, not `values.StaticError`. The security package has zero deps on wile packages. When these errors reach primitive functions, the primitive wraps them in `values.WrapForeignErrorf`.

## Context Propagation

```go
package security

import "context"

type contextKey struct{}

// WithAuthorizer returns a new context carrying the given authorizer.
func WithAuthorizer(ctx context.Context, a Authorizer) context.Context {
    if a == nil {
        return ctx
    }
    return context.WithValue(ctx, contextKey{}, a)
}

// FromContext extracts the authorizer from ctx, or nil if none is set.
func FromContext(ctx context.Context) Authorizer {
    a, _ := ctx.Value(contextKey{}).(Authorizer)
    return a
}

// Check authorizes an operation against the policy in ctx.
// Returns nil if no authorizer is set (backward compatible -- no authorizer
// means everything is allowed, matching current behavior).
func Check(ctx context.Context, req AccessRequest) error {
    a := FromContext(ctx)
    if a == nil {
        return nil
    }
    return a.Authorize(req)
}
```

Call sites become:

```go
if err := security.Check(ctx, security.AccessRequest{
    Resource: security.ResourceFile,
    Action:   security.ActionRead,
    Target:   filename.Value,
}); err != nil {
    return values.WrapForeignErrorf(err, "open-input-file: %s", filename.Value)
}
```

## Built-In Authorizers

### FilesystemRoot

Restricts file and code-loading operations to a directory tree. Addresses the commenter's primary request.

```go
type FilesystemRoot struct {
    root string // cleaned absolute path
}

func NewFilesystemRoot(dir string) (*FilesystemRoot, error)
```

**Authorize logic**:

```
1. If req.Resource is not "file" and not "code" -> return nil (not our concern)
2. Clean the target path: filepath.Clean(req.Target)
3. If relative, join with root: filepath.Join(root, target)
4. Resolve symlinks: filepath.EvalSymlinks(absPath)
5. Check prefix: strings.HasPrefix(resolved, root + string(filepath.Separator))
   OR resolved == root
6. If not contained -> return ErrAccessDenied
```

Only gates `ResourceFile` and `ResourceCode`. Passes through everything else (env, process, unknown future resources). This is intentional -- `FilesystemRoot` is a filesystem policy, not a general-purpose deny-all.

**Known limitation**: `filepath.EvalSymlinks` is TOCTOU-vulnerable. Documented. When Go 1.24 is adopted, `FilesystemRoot` can use `os.Root` internally without interface changes.

**Constructor validates**: `NewFilesystemRoot` calls `filepath.Abs` and `filepath.EvalSymlinks` on `dir` at construction time. Returns error if `dir` doesn't exist or isn't a directory.

### ReadOnly

Allows reads and stats, denies writes and deletes. Passes non-file resources through.

```go
type ReadOnly struct{}
```

```
Authorize(Resource="file", Action="read")   -> nil
Authorize(Resource="file", Action="stat")   -> nil
Authorize(Resource="file", Action="write")  -> ErrAccessDenied
Authorize(Resource="file", Action="delete") -> ErrAccessDenied
Authorize(Resource="code", Action=*)        -> nil  (code loading is a read)
Authorize(Resource=other, Action=*)         -> nil  (not our concern)
```

### DenyAll

Blocks everything. Useful as a baseline for composition or as a fallback.

```go
type DenyAll struct{}

func (DenyAll) Authorize(AccessRequest) error {
    return ErrAccessDenied
}
```

### Composite

Combines multiple authorizers. All must approve -- first denial wins.

```go
type Composite struct {
    authorizers []Authorizer
}

func Combine(authorizers ...Authorizer) *Composite
```

Each `Authorize` call iterates in order, returning the first non-nil error. This is an AND combinator (all must allow).

Nil authorizers in the input slice are silently skipped.

### Typical embedder configurations

```go
// Read-only access within /data/config only
engine := wile.NewEngine(
    wile.WithExtension(files.Ext),
    wile.WithAuthorizer(security.Combine(
        security.NewFilesystemRoot("/data/config"),
        &security.ReadOnly{},
    )),
)

// Full file access, no env/exit (custom inline)
engine := wile.NewEngine(
    wile.WithExtension(files.Ext),
    wile.WithExtension(system.Ext),
    wile.WithAuthorizer(security.AuthorizerFunc(func(req security.AccessRequest) error {
        switch req.Resource {
        case security.ResourceEnv, security.ResourceProcess:
            return security.ErrAccessDenied
        default:
            return nil
        }
    })),
)

// Deny by default, allow only specific operations
engine := wile.NewEngine(
    wile.WithExtension(files.Ext),
    wile.WithAuthorizer(security.AuthorizerFunc(func(req security.AccessRequest) error {
        if req.Resource == security.ResourceFile &&
           req.Action == security.ActionRead &&
           strings.HasPrefix(req.Target, "/safe/") {
            return nil
        }
        return security.ErrAccessDenied
    })),
)
```

## Integration Points

### Summary of changes per file

| File | Primitives gated | Resource | Action |
|------|-----------------|----------|--------|
| `internal/extensions/files/prim_files.go` | `open-input-file`, `open-binary-input-file` | `file` | `read` |
| | `open-output-file`, `open-binary-output-file` | `file` | `write` |
| | `file-exists?` | `file` | `stat` |
| | `delete-file` | `file` | `delete` |
| | `call-with-input-file`, `with-input-from-file` | `file` | `read` |
| | `call-with-output-file`, `with-output-to-file` | `file` | `write` |
| `internal/extensions/system/prim_system.go` | `get-environment-variable` | `env` | `read` |
| | `get-environment-variables` | `env` | `read` |
| | `exit`, `emergency-exit` | `process` | `exit` |
| `internal/extensions/eval/prim_eval.go` | `load` | `code` | `load` |
| `machine/compile_time_continuation.go` | `include`, `include-ci` | `code` | `load` |
| `machine/library_loader.go` | library `import` | `code` | `load` |

### Primitive modification pattern

Every security-sensitive primitive follows the same pattern -- a `security.Check` call before the first syscall:

```
BEFORE (current):
    filename := mc.Arg(0)
    file, err := os.Open(...)

AFTER:
    filename := mc.Arg(0)
    if err := security.Check(ctx, security.AccessRequest{
        Resource: security.ResourceFile,
        Action:   security.ActionRead,
        Target:   filename.Value,
    }); err != nil {
        return values.WrapForeignErrorf(err, "open-input-file: %s", filename.Value)
    }
    file, err := os.Open(...)
```

Note: primitives currently ignore their `ctx` parameter (signature is `func(_ context.Context, mc *MachineContext)`). After this change, they use it.

### Context availability at each site

| Site | How ctx is obtained |
|------|-------------------|
| Runtime primitives (files, system, eval) | First parameter: `func(ctx context.Context, mc *MachineContext)` |
| `findFile` (compile-time `include`) | `ctctx.ctx` field on `CompileTimeCallContext` |
| `LoadLibrary` / `loadLibraryFromFile` | `ctx` parameter |

All sites already have `context.Context`. No plumbing changes needed.

### files/prim_files.go -- concrete example

`PrimOpenInputFile`:
```go
func PrimOpenInputFile(ctx context.Context, mc *machine.MachineContext) error {
    filename, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "open-input-file")
    if err != nil {
        return err
    }
    // NEW: authorization check
    if err := security.Check(ctx, security.AccessRequest{
        Resource: security.ResourceFile,
        Action:   security.ActionRead,
        Target:   filename.Value,
    }); err != nil {
        return values.WrapForeignErrorf(err, "open-input-file: %s", filename.Value)
    }
    file, err := os.Open(filename.Value)
    // ... rest unchanged
```

Same pattern for all 10 file primitives. The `callWithFile` helper gets an `action` parameter:

```go
func callWithFile(ctx context.Context, mc *machine.MachineContext, name string,
    opener func(string) (*os.File, error), portCreator func(*os.File) values.Value,
    action string,  // NEW parameter: security.ActionRead or security.ActionWrite
) error {
    // ...
    if err := security.Check(ctx, security.AccessRequest{
        Resource: security.ResourceFile,
        Action:   action,
        Target:   filename.Value,
    }); err != nil {
        return values.WrapForeignErrorf(err, "%s: %s", name, filename.Value)
    }
    // ...
```

### Denied `include` paths: skip vs error

When `findFile` encounters a denied path during its search loop, it should **skip** (continue to next search path), not error immediately. Only after exhausting all paths should it error. This prevents information leakage -- a denied path behaves identically to a nonexistent path from the Scheme program's perspective.

### Engine integration

In `engine.go`, every public method that takes `ctx` wraps it once:

```go
func (p *Engine) Eval(ctx context.Context, code string) (Value, error) {
    ctx = p.wrapContext(ctx)  // adds authorizer if configured
    compiled, err := p.Compile(ctx, code)
    // ...
}

func (p *Engine) wrapContext(ctx context.Context) context.Context {
    if p.authorizer != nil {
        ctx = security.WithAuthorizer(ctx, p.authorizer)
    }
    return ctx
}
```

Methods that need wrapping: `Eval`, `EvalWithSource`, `EvalMultiple`, `EvalMultipleWithSource`, `Run`, `Call`, `evalMultiple`, `compile`, `runCompiled`, `callClosure`, `callCaseLambda`, `callParameter`.

Actually -- the wrapping should happen at the top-level public methods only (`Eval`, `EvalWithSource`, `EvalMultiple`, `EvalMultipleWithSource`, `Run`, `Call`). Internal methods receive the already-wrapped ctx. This avoids double-wrapping.

## Extensibility Model

The key property of this design: **extensions define their own resource/action vocabulary without changing the `security` package or the `Authorizer` interface**.

### How a future network extension would work

```go
// internal/extensions/net/constants.go
package net

const (
    ResourceNet   = "net"
    ActionConnect = "connect"
    ActionListen  = "listen"
)

// internal/extensions/net/prim_net.go
func PrimTCPConnect(ctx context.Context, mc *machine.MachineContext) error {
    host := // ... extract from args
    port := // ... extract from args
    target := fmt.Sprintf("%s:%d", host, port)

    if err := security.Check(ctx, security.AccessRequest{
        Resource: ResourceNet,
        Action:   ActionConnect,
        Target:   target,
    }); err != nil {
        return values.WrapForeignErrorf(err, "tcp-connect: %s", target)
    }
    // ... proceed with net.Dial
}
```

The authorizer the embedder configures can handle `"net"` resources:

```go
engine := wile.NewEngine(
    wile.WithExtension(net.Ext),
    wile.WithAuthorizer(security.AuthorizerFunc(func(req security.AccessRequest) error {
        if req.Resource == net.ResourceNet {
            if req.Action == net.ActionListen {
                return security.ErrAccessDenied  // no listening
            }
            // allow outbound to *.example.com only
            if !strings.HasSuffix(req.Target, ".example.com:443") {
                return security.ErrAccessDenied
            }
        }
        return nil
    })),
)
```

No changes to `security/`, no changes to `Authorizer`, no new interface methods. The extension and the authorizer agree on the vocabulary via constants.

### How an embedder-defined extension works

Embedders who add custom primitives via `Engine.RegisterPrimitive` can use the same pattern:

```go
engine.RegisterPrimitive(wile.PrimitiveSpec{
    Name:       "my-dangerous-op",
    ParamCount: 1,
    Impl: func(ctx context.Context, mc *machine.MachineContext) error {
        if err := security.Check(ctx, security.AccessRequest{
            Resource: "my-extension",
            Action:   "dangerous",
            Target:   mc.Arg(0).SchemeString(),
        }); err != nil {
            return err
        }
        // ... do dangerous thing
        return nil
    },
})
```

## Comparison with Previous Plan

| Aspect | SECURITY_POLICY.md | This plan |
|--------|-------------------|-----------|
| Interface | 4 methods, grows per domain | 1 method, stable forever |
| Extension permissions | Not possible | Extensions define own vocabulary |
| Breaking changes | Every new domain | Never |
| Type safety | High (Go method signatures) | Lower (string resource/action) |
| Discoverability | IDE shows methods | Constants + docs |
| Unknown operations | Compile error (forces impl) | Authorizer's policy decision |
| Error sentinels | 4 (one per domain) | 1 (`ErrAccessDenied`) |
| `BasePolicy` needed? | Yes (forward compat hack) | No |

The trade-off is type safety vs extensibility. String-typed resource/action pairs can have typos. Mitigations: well-known constants, and tests that verify extensions use the documented vocabulary.

## Phases

### Phase 1: `security/` package -- interface, context, sentinels, constants

New files:
- `security/access.go` -- `AccessRequest`, well-known resource/action constants
- `security/authorizer.go` -- `Authorizer` interface, `AuthorizerFunc`, `ErrAccessDenied`
- `security/context.go` -- `WithAuthorizer`, `FromContext`, `Check`
- `security/access_test.go`
- `security/authorizer_test.go`
- `security/context_test.go`

Zero deps on other wile packages. Pure stdlib.

Tests:
- `AccessRequest` construction and field access
- `AuthorizerFunc` adapter
- Context round-trip: `WithAuthorizer` -> `FromContext`
- `Check` with nil authorizer returns nil
- `Check` with denying authorizer returns error
- `errors.Is(err, ErrAccessDenied)` works

### Phase 2: Built-in authorizers

New files:
- `security/filesystem_root.go` + `security/filesystem_root_test.go`
- `security/read_only.go` + `security/read_only_test.go`
- `security/deny_all.go` + `security/deny_all_test.go`
- `security/composite.go` + `security/composite_test.go`

Tests:
- `FilesystemRoot`: containment, symlink handling, `..` traversal blocked, relative paths, non-file resources pass through
- `ReadOnly`: reads/stats allowed, writes/deletes denied, non-file resources pass through
- `DenyAll`: everything denied
- `Composite`: AND semantics, first denial wins, empty composite allows all, nil authorizers skipped

### Phase 3: Engine integration

Modify:
- `options.go` -- add `WithAuthorizer` option, add `authorizer` field to `engineConfig`
- `engine.go` -- add `authorizer` field to `Engine`, `wrapContext` helper, wrap ctx in all public entry points

Tests:
- Engine with authorizer rejects operations
- Engine without authorizer (backward compat) allows everything
- Authorizer set at construction, not changeable after

### Phase 4: Gate runtime primitives

Modify (add `security.Check` calls):
- `internal/extensions/files/prim_files.go` -- all 10 file primitives
- `internal/extensions/system/prim_system.go` -- env var and exit primitives
- `internal/extensions/eval/prim_eval.go` -- `PrimLoad`

Tests:
- Scheme-level tests: `(open-input-file "/forbidden")` with denying authorizer raises error
- Scheme-level tests: same operations succeed with no authorizer
- Scheme-level tests: `(exit)` denied by authorizer -> error, process stays alive
- Scheme-level tests: `(get-environment-variable "SECRET")` denied -> error

### Phase 5: Gate compile-time code loading

Modify:
- `machine/compile_time_continuation.go` -- `findFile`: check before `os.Open`, skip denied paths
- `machine/library_loader.go` -- `loadLibraryFromFile`: check before `os.Open`

Tests:
- `(include "forbidden.scm")` with denying authorizer -> compilation error
- `(import (forbidden lib))` with denying authorizer -> load error
- Denied paths in `findFile` skip silently (no information leakage)

### Phase 6: Integration tests

New:
- `security_integration_test.go` in root `wile/` package

Tests:
1. Engine with `FilesystemRoot` -- allowed file opens succeed, disallowed fail with `ErrAccessDenied`
2. Engine with `ReadOnly` -- reads succeed, writes fail
3. Engine with `DenyAll` -- everything fails
4. Engine with `Composite` -- combined policies enforce all constraints
5. Engine with no authorizer -- backward compatible, everything succeeds
6. `(load ...)` blocked by authorizer -- returns Scheme error, doesn't crash
7. `(include ...)` blocked by authorizer -- compilation error, doesn't crash
8. `(exit)` blocked by authorizer -- returns error, process stays alive
9. `(get-environment-variable "SECRET")` blocked -- returns error
10. Custom embedder authorizer -- verify interface works for external implementations
11. Extension-defined resource/action pairs work with authorizer (simulated future extension)

## Phase Dependencies

```
Phase 1 --> Phase 2
    |
    +---> Phase 3 --+--> Phase 4
                    +--> Phase 5
                              |
                              +--> Phase 6
```

Phases 4 and 5 are independent of each other. Phase 6 requires all prior phases.

## Files Summary

| File | Change | Phase |
|------|--------|-------|
| `security/access.go` | **New** | 1 |
| `security/authorizer.go` | **New** | 1 |
| `security/context.go` | **New** | 1 |
| `security/access_test.go` | **New** | 1 |
| `security/authorizer_test.go` | **New** | 1 |
| `security/context_test.go` | **New** | 1 |
| `security/filesystem_root.go` | **New** | 2 |
| `security/filesystem_root_test.go` | **New** | 2 |
| `security/read_only.go` | **New** | 2 |
| `security/read_only_test.go` | **New** | 2 |
| `security/deny_all.go` | **New** | 2 |
| `security/deny_all_test.go` | **New** | 2 |
| `security/composite.go` | **New** | 2 |
| `security/composite_test.go` | **New** | 2 |
| `options.go` | Modify -- `WithAuthorizer` option | 3 |
| `engine.go` | Modify -- `authorizer` field, `wrapContext`, wrap public methods | 3 |
| `internal/extensions/files/prim_files.go` | Modify -- add checks to 10 primitives | 4 |
| `internal/extensions/system/prim_system.go` | Modify -- add checks to 3 primitives | 4 |
| `internal/extensions/eval/prim_eval.go` | Modify -- add check to `PrimLoad` | 4 |
| `machine/compile_time_continuation.go` | Modify -- add check in `findFile` | 5 |
| `machine/library_loader.go` | Modify -- add check in `loadLibraryFromFile` | 5 |
| `security_integration_test.go` | **New** | 6 |

## Design Decisions

### Why verb+resource, not typed permissions (Java-style)

Java's `Permission` subclass model (e.g., `FilePermission`, `SocketPermission`) is more type-safe -- each permission carries exactly the right fields, and policies can type-switch. But Go doesn't have class hierarchies, and the overhead of defining a new type + interface implementation for each permission domain is disproportionate for Wile's scale. The K8s model (flat struct, string-typed) is simpler, equally extensible, and more idiomatic in Go where interfaces are narrow and data flows through structs.

### Why not `io/fs.FS`

Go's `io/fs.FS` is read-only. No `Create`, `Remove`, or write operations. Wile needs write support. The authorizer pattern is also more general -- it gates non-filesystem operations too.

### Why `context.Context`, not a `MachineContext` field

`MachineContext` only exists at runtime. Compile-time `include` and library loading have no `MachineContext` -- they use `CompileTimeCallContext` and raw `ctx`. `context.Context` is the only vehicle at all three sites.

### Why check-before-syscall, not wrapping `os.Open`

The authorizer is a gate, not a replacement. It answers "is this operation allowed?" but doesn't perform the operation. Primitives retain full control over how they open/create/delete files.

### Denied `include` paths: skip vs error

When `findFile` encounters a denied path, it skips to the next search path. A denied path behaves identically to a nonexistent path -- no information leakage about what exists outside the sandbox.

### Immutability after construction

The authorizer is set once via `WithAuthorizer` and never changed. No `Engine.SetAuthorizer()`. Prevents TOCTOU where code runs under one policy then the policy changes mid-execution.

### No `Extra` field on `AccessRequest`

Considered adding `Extra map[string]string` for extension-specific metadata. Omitted: three fields cover all current and foreseeable needs. The `Target` field is flexible enough for structured data (e.g., `"host:port"` for network). If a future use case genuinely needs structured metadata, we can add it then -- it's an additive change to the struct (not a breaking change to the interface).

### Why one error sentinel, not four

The previous plan had `ErrFileNotAllowed`, `ErrLoadNotAllowed`, `ErrEnvNotAllowed`, `ErrExitNotAllowed`. With the unified model, the `AccessRequest` fields carry the domain information. One sentinel `ErrAccessDenied` is sufficient for `errors.Is` matching. The wrapping message at each call site provides human-readable context:

```go
values.WrapForeignErrorf(err, "open-input-file: %s", filename.Value)
// -> "open-input-file: /etc/passwd: security: access denied"
```

## Go 1.24 `os.Root` Upgrade Path

When Wile adopts Go 1.24+, `FilesystemRoot` can use `os.Root` internally for kernel-level path containment. The `Authorizer` interface doesn't change. `FilesystemRoot.Authorize` would use `os.Root.Open` for the containment check instead of `filepath.EvalSymlinks` + prefix comparison. No changes to call sites or the security package API.

## Verification

1. `make test` -- all existing tests pass (no authorizer = no restrictions = backward compatible)
2. `make lint` -- no lint issues
3. `security/` package tests -- interface compliance, context round-trip, all built-in authorizers
4. `FilesystemRoot` tests -- containment, symlink handling, `..` traversal, relative paths
5. `Composite` tests -- AND semantics, empty composite, ordering
6. Integration tests -- full engine with authorizer, Scheme code denied/allowed
7. No policy regression -- engine without `WithAuthorizer` behaves identically to today

## References

- [Kubernetes RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/) -- verb + resource + group model
- [Kubernetes SubjectAccessReview API](https://kubernetes.io/docs/reference/kubernetes-api/authorization-resources/subject-access-review-v1/) -- `ResourceAttributes` struct
- [kubernetes/kubernetes authorization types.go](https://github.com/kubernetes/kubernetes/blob/master/pkg/apis/authorization/types.go) -- Go type definitions
- [Java SE Platform Security Architecture](https://docs.oracle.com/en/java/javase/11/security/java-se-platform-security-architecture.html) -- Permission model, `checkPermission(Permission)`
- [JEP 411: Deprecate the Security Manager for Removal](https://openjdk.org/jeps/411) -- why method-per-domain failed
- [jGuard](https://github.com/jguard-io/jguard) -- capability-based post-SecurityManager framework
- [Oracle: Security and Sandboxing Post SecurityManager](https://inside.java/2021/04/23/security-and-sandboxing-post-securitymanager/) -- defense-in-depth philosophy
