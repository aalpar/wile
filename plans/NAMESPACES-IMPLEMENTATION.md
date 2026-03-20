# Namespace Migration — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** Complete (PR #544, merged 2026-03-20)

**Goal:** Migrate `TopLevelEnvironment` to `Namespace`, unifying registry, authorizer, and module instances under one type. Five independently mergeable PRs.

**Architecture:** `TopLevelEnvironment` → `Namespace` rename + extension. Registry and authorizer move from Engine to Namespace. Module instance caching added. Scheme API exposed. Zero runtime regression — namespace resolution is compile-time only.

**Tech Stack:** Go 1.24, `gopls` for rename tooling, `goimports` for import cleanup. No new dependencies.

**Design doc:** `plans/NAMESPACES.md`

---

## PR 1: Mechanical Rename `TopLevelEnvironment` → `Namespace`

~122 files, ~900 references. Zero behavior change. Every change is a find/replace or `gopls rename`.

### Task 1.1: Rename the Core Type and Constructors

**Files:**
- Modify: `environment/top_level_environment.go`
- Rename to: `environment/namespace.go`

**Step 1: Rename the type**

Use `gopls rename` or manual rename:

```
TopLevelEnvironment         → Namespace
NewTopLevelEnvironment()    → NewNamespace()
NewTopLevelEnvironmentFrame() → NewNamespaceFrame()
```

In `environment/namespace.go` (was `top_level_environment.go`):

```go
// Line 45: type definition
type Namespace struct {
    // ... all existing fields unchanged ...
}

// Line 94: constructor
func NewNamespace() *Namespace {

// Line 381: child constructor
func (p *Namespace) NewChildNamespace() *Namespace {

// Line 399: scheme-report constructor
func (p *Namespace) NewSchemeReportNamespace() *Namespace {
```

`NewChildRuntime()` stays — it's internal and already well-named.

**Step 2: Update method receivers**

All 18 methods on `TopLevelEnvironment` change receiver type name. Bodies are unchanged.

**Step 3: Rename the file**

```bash
git mv environment/top_level_environment.go environment/namespace.go
git mv environment/top_level_environment_test.go environment/namespace_test.go
```

### Task 1.2: Rename Accessor Methods on Dependent Types

**Files:**
- Modify: `environment/environment_frame.go`
- Modify: `environment/phase_registry.go`
- Modify: `environment/global_environment_frame.go`

**Step 1: `EnvironmentFrame` changes**

In `environment/environment_frame.go`:

```go
// Line 106: field rename
type EnvironmentFrame struct {
    // ...
    namespace *Namespace    // was: topLevel *TopLevelEnvironment
}

// Line 858: accessor rename
func (p *EnvironmentFrame) Namespace() *Namespace {    // was: TopLevelEnv()
    return p.namespace
}

// Line 117: constructor wrapper rename
func NewNamespaceFrame() *EnvironmentFrame {           // was: NewTopLevelEnvironmentFrame()
    return NewNamespace().Runtime()
}
```

**Step 2: `PhaseRegistry` changes**

In `environment/phase_registry.go`:

```go
// Line 47: field rename
type PhaseRegistry struct {
    owner *Namespace    // was: *TopLevelEnvironment
}

// Line 121: accessor rename
func (p *PhaseRegistry) Namespace() *Namespace {    // was: TopLevelEnv()
```

**Step 3: `GlobalEnvironmentFrame` changes**

In `environment/global_environment_frame.go`:

```go
// Line 87: field rename
type GlobalEnvironmentFrame struct {
    // ...
    namespace *Namespace    // was: topLevel *TopLevelEnvironment
}
```

### Task 1.3: Rename Engine Accessor

**Files:**
- Modify: `engine.go`

**Step 1: Rename field and method**

```go
// Line 53: field rename
type Engine struct {
    namespace *environment.Namespace    // was: topLevel *environment.TopLevelEnvironment
    // ...
}

// Line 428: public accessor rename
func (p *Engine) Namespace() *environment.Namespace {    // was: TopLevelEnvironment()
```

**Step 2: Update all internal references to `p.topLevel` → `p.namespace`**

~6 references in `engine.go`: constructor, `setupLibrarySystem`, `WithLoadPath`, `PushLoadPath`, `PopLoadPath`, etc.

### Task 1.4: Propagate Rename Across All Downstream Files

**Process:**

```bash
# 1. Find all remaining references
grep -rn 'TopLevelEnvironment\|TopLevelEnv\|topLevel\|NewTopLevelEnvironmentFrame' \
  --include='*.go' . | grep -v vendor

# 2. Fix each file — most are type annotations and constructor calls in tests
# Pattern: s/TopLevelEnvironment/Namespace/g
# Pattern: s/TopLevelEnv()/Namespace()/g
# Pattern: s/NewTopLevelEnvironmentFrame/NewNamespaceFrame/g
# Pattern: s/\.topLevel/\.namespace/g  (only the field, not local vars)

# 3. Fix imports
goimports -w .

# 4. Verify
make lint && make test
```

**Highest-impact packages** (by reference count):

| Package | Files | Approx references |
|---------|-------|-------------------|
| `machine/` (tests) | ~40 | ~300 |
| `internal/parser/` (tests) | ~7 | ~190 |
| `environment/` (tests) | ~9 | ~130 |
| `internal/bootstrap/` | ~7 | ~95 |
| `registry/` (tests) | ~10 | ~50 |
| `engine.go` | 1 | 6 |
| `extensions/` | 2 | 8 |

### Task 1.5: Update Documentation

**Files:**
- Modify: `docs/dev/ENVIRONMENT_SYSTEM.md` — replace all `TopLevelEnvironment` references
- Modify: `CLAUDE.md` — update architecture section
- Modify: `CLAUDE.local.md` — update invariants table
- Modify: `environment/doc.go` — update package doc
- Modify: `runtime/doc.go` — update references
- Modify: `internal/bootstrap/doc.go` — update references

### Task 1.6: Verify and Commit

**Step 1: Run full test suite**

```bash
make test
```

Expected: all tests pass with no changes to test logic (only type/method names changed).

**Step 2: Run linter**

```bash
make lint
```

**Step 3: Commit**

```bash
git add -A
git commit -m "refactor: rename TopLevelEnvironment to Namespace

Mechanical rename with zero behavior change. Namespace is a more
accurate name — the type already owns bindings, phases, library
registry, and syntax interning.

Renames:
- TopLevelEnvironment → Namespace
- NewTopLevelEnvironment() → NewNamespace()
- NewTopLevelEnvironmentFrame() → NewNamespaceFrame()
- env.TopLevelEnv() → env.Namespace()
- NewChildTopLevelEnvironment() → NewChildNamespace()
- NewSchemeReportEnvironment() → NewSchemeReportNamespace()
- Engine.TopLevelEnvironment() → Engine.Namespace()"
```

---

## PR 2: Move Registry from Engine to Namespace

### Task 2.1: Add Registry Field to Namespace

**Files:**
- Modify: `environment/namespace.go`
- Test: `environment/namespace_test.go`

**Step 1: Write the failing test**

In `environment/namespace_test.go`:

```go
func TestNamespace_RegistryField(t *testing.T) {
    ns := NewNamespace()
    // Registry is nil by default
    if ns.Registry() != nil {
        t.Error("expected nil registry on fresh namespace")
    }
}
```

**Step 2: Run test to verify it fails**

```bash
go test -v -run TestNamespace_RegistryField ./environment/...
```

Expected: FAIL — `Registry()` method does not exist.

**Step 3: Add field and accessor**

In `environment/namespace.go`:

```go
type Namespace struct {
    // ... existing fields ...
    registry any    // *registry.Registry — use any to avoid import cycle
}

func (p *Namespace) Registry() any {
    return p.registry
}

func (p *Namespace) SetRegistry(reg any) {
    p.registry = reg
}
```

Note: `environment/` cannot import `registry/` (would create a cycle). The field is typed `any`; callers assert to `*registry.Registry`. This matches the existing `libraryRegistry any` pattern on the same struct.

**Step 4: Run test to verify it passes**

```bash
go test -v -run TestNamespace_RegistryField ./environment/...
```

**Step 5: Commit**

```bash
git commit -m "feat(environment): add registry field to Namespace"
```

### Task 2.2: Move buildRegistry to Namespace Construction

**Files:**
- Modify: `engine.go`
- Create: `namespace_options.go` (or extend `options.go`)
- Test: `engine_unit_test.go`

**Step 1: Create NamespaceOption type**

In `options.go` (or new `namespace_options.go`):

```go
// NamespaceOption configures a Namespace.
type NamespaceOption func(*namespaceConfig)

type namespaceConfig struct {
    registry   *registry.Registry
    extensions []registry.Extension
    skipCore   bool
    authorizer security.Authorizer    // for PR 3, placeholder for now
}
```

**Step 2: Create NewNamespace constructor on the wile package**

In `engine.go` (or new `namespace.go` at package root):

```go
func NewNamespace(ctx context.Context, opts ...NamespaceOption) (*environment.Namespace, error) {
    cfg := &namespaceConfig{}
    for _, opt := range opts {
        opt(cfg)
    }

    ns := environment.NewNamespace()

    reg, snapshots, closers, err := buildRegistry(cfg)
    if err != nil {
        // CONTEXT PRESERVATION: buildRegistry's error must carry the
        // extension name and primitive count at the point of failure.
        // After this return, cfg.extensions (which extensions were
        // requested), reg (partial registry state), and snapshots
        // (which extensions succeeded) are all lost. buildRegistry
        // itself must wrap errors with the failing extension's Name()
        // and the current primitive index so the caller can diagnose
        // which extension broke and how far registration got.
        //
        // Inside buildRegistry, each extension loop iteration should:
        //   werr.WrapForeignErrorf(err, "buildRegistry: extension %q at index %d",
        //       ext.Name(), len(reg.Primitives()))
        //
        // Do NOT wrap here — the context is already gone. The wrap
        // must happen at the site inside buildRegistry where the
        // error originates and the state is still live.
        return nil, err
    }

    ns.SetRegistry(reg)

    env := ns.Runtime()
    err = applyBaseEnvironment(ctx, env, reg, macroSources, bootstrapResolver)
    if err != nil {
        // CONTEXT PRESERVATION: applyBaseEnvironment runs a 4-step
        // sequence (Apply, RegisterSyntaxCompilers, RegisterPrimitiveExpanders,
        // loadBootstrapMacros). The error must identify which step failed.
        // reg and env are still live here but will be lost after return.
        // The wrap should happen inside applyBaseEnvironment at each step.
        return nil, err
    }

    // ... library system setup ...

    return ns, nil
}
```

This extracts the registry-building and environment-populating logic from `NewEngine` into `NewNamespace`.

**Error context rule:** When `buildRegistry` or `applyBaseEnvironment` returns an error, the objects that explain *why* it failed (which extension, how far registration progressed, which step of the 4-step sequence) go out of scope at the `return`. These functions must wrap errors internally at the point of failure, capturing state that would otherwise be lost. See `REVIEW.local.md` §Error Context at Return Boundaries.

**Step 3: Add WithNamespace option to Engine**

In `options.go`:

```go
func WithNamespace(ns *environment.Namespace) EngineOption {
    return func(cfg *engineConfig) {
        cfg.namespace = ns
    }
}
```

**Step 4: Modify NewEngine to use namespace**

In `engine.go`, `NewEngine` checks for a pre-built namespace:

```go
func NewEngine(ctx context.Context, opts ...EngineOption) (*Engine, error) {
    cfg := defaultConfig()
    for _, opt := range opts {
        opt(cfg)
    }

    var ns *environment.Namespace
    if cfg.namespace != nil {
        ns = cfg.namespace
    } else {
        // Build namespace from engine options (backward compat)
        nsOpts := engineConfigToNamespaceOpts(cfg)
        var err error
        ns, err = NewNamespace(ctx, nsOpts...)
        if err != nil {
            return nil, err
        }
    }

    reg := ns.Registry().(*registry.Registry)

    q := &Engine{
        namespace: ns,
        env:       ns.Runtime(),
        registry:  reg,
        // ...
    }
    return q, nil
}
```

**Step 5: Write backward-compat test**

In `engine_unit_test.go`:

```go
func TestNewEngine_BackwardCompat(t *testing.T) {
    ctx := context.Background()
    // Old style: options on NewEngine
    eng, err := NewEngine(ctx, WithExtension(math.Extension))
    qt.Assert(t, qt.IsNil(err))
    result, err := eng.Eval(ctx, "(+ 1 2)")
    qt.Assert(t, qt.IsNil(err))
    qt.Assert(t, qt.Equals(result.SchemeString(), "3"))
}

func TestNewEngine_WithNamespace(t *testing.T) {
    ctx := context.Background()
    // New style: pre-built namespace
    ns, err := NewNamespace(ctx, WithExtension(math.Extension))
    qt.Assert(t, qt.IsNil(err))

    eng, err := NewEngine(ctx, WithNamespace(ns))
    qt.Assert(t, qt.IsNil(err))
    result, err := eng.Eval(ctx, "(+ 1 2)")
    qt.Assert(t, qt.IsNil(err))
    qt.Assert(t, qt.Equals(result.SchemeString(), "3"))
}
```

**Step 6: Run tests**

```bash
make test
```

**Step 7: Commit**

```bash
git commit -m "feat: move registry ownership from Engine to Namespace

NewNamespace(ctx, opts...) builds and owns the registry.
NewEngine accepts WithNamespace(ns) to use a pre-built namespace.
Backward compat: NewEngine without WithNamespace creates namespace
from its own options."
```

### Task 2.3: Add Namespace.Derive

**Registry copy policy:** The registry is **immutable after construction**. `buildRegistry` populates it, `Apply` writes its bindings into the environment, and no code path mutates it after that. This means:

- **No options → pointer share.** Parent and child reference the same `*Registry`. Safe because neither mutates it. No allocation cost.
- **With restriction options → clone-and-filter.** `Registry.Without()` (`registry/registry.go:308`) returns a new `*Registry` with the specified primitives removed. The parent's registry is unchanged.
- **Never deep-copy without reason.** `Registry.Clone()` exists (used by `Engine.Registry()` to give embedders a safe copy to filter), but `Derive` should not clone unless the caller requests restrictions.

**Files:**
- Modify: `environment/namespace.go`
- Test: `environment/namespace_test.go`

**Step 1: Write the failing tests**

```go
func TestNamespace_Derive_SharesRegistry(t *testing.T) {
    parent := NewNamespace()
    parent.SetRegistry("parent-registry")

    child := parent.Derive()

    // Same pointer — immutable registry is shared, not cloned
    if child.Registry() != parent.Registry() {
        t.Error("derived namespace should share parent registry (pointer equality)")
    }
    if child == parent {
        t.Error("derived namespace should be a new object")
    }
}

func TestNamespace_Derive_OverrideRegistry(t *testing.T) {
    parent := NewNamespace()
    parent.SetRegistry("parent-registry")

    child := parent.DeriveWith(func(cfg *NamespaceDeriveConfig) {
        cfg.Registry = "restricted-registry"
    })

    if child.Registry() == parent.Registry() {
        t.Error("derived namespace with override should have different registry")
    }
    if child.Registry() != "restricted-registry" {
        t.Error("derived namespace should use overridden registry")
    }
}
```

**Step 2: Implement Derive and DeriveWith**

In `environment/namespace.go`:

```go
// NamespaceDeriveConfig holds options for DeriveWith.
// Zero value means "inherit everything from parent."
type NamespaceDeriveConfig struct {
    Registry   any    // if non-nil, overrides parent's registry
    Authorizer any    // if non-nil, overrides parent's authorizer (PR 3)
}

// NamespaceDeriveOption configures a derived namespace.
type NamespaceDeriveOption func(*NamespaceDeriveConfig)

// Derive creates a child namespace that shares syntax interning with
// the parent but has isolated bindings. The parent's registry is shared
// by pointer — safe because registries are immutable after construction.
func (p *Namespace) Derive() *Namespace {
    child := p.NewChildNamespace()
    child.registry = p.registry       // pointer share: immutable
    child.authorizer = p.authorizer   // pointer share: immutable
    return child
}

// DeriveWith creates a child namespace with option overrides.
// Use this when the child needs a restricted registry or different
// authorizer. The parent's registry is NOT shared when overridden.
func (p *Namespace) DeriveWith(opts ...NamespaceDeriveOption) *Namespace {
    cfg := &NamespaceDeriveConfig{}
    for _, opt := range opts {
        opt(cfg)
    }

    child := p.NewChildNamespace()

    if cfg.Registry != nil {
        child.registry = cfg.Registry     // caller-provided (typically cloned+filtered)
    } else {
        child.registry = p.registry       // pointer share: immutable
    }

    if cfg.Authorizer != nil {
        child.authorizer = cfg.Authorizer
    } else {
        child.authorizer = p.authorizer
    }

    return child
}
```

At the `wile` package level (Go embedding API), `Derive` wraps this and handles `Registry.Without()`:

```go
// In wile package (engine.go or namespace.go):
func (ns *environment.Namespace) Derive(opts ...NamespaceOption) *environment.Namespace {
    cfg := &namespaceConfig{}
    for _, opt := range opts {
        opt(cfg)
    }

    // If no restriction options, fast path: pointer share
    if len(cfg.extensions) == 0 && cfg.registry == nil && !cfg.skipCore {
        return ns.Derive()
    }

    // Restriction requested: clone and filter
    parentReg := ns.Registry().(*registry.Registry)
    restricted := parentReg.Clone()
    // Apply Without/WithoutCategory based on options...

    return ns.DeriveWith(func(dcfg *environment.NamespaceDeriveConfig) {
        dcfg.Registry = restricted
    })
}
```

**Step 3: Run tests, commit**

```bash
go test -v -run TestNamespace_Derive ./environment/...
git commit -m "feat(environment): add Namespace.Derive and DeriveWith

Derive() shares the parent registry by pointer (safe: registries
are immutable after construction). DeriveWith() accepts overrides
for cases that need a restricted registry or different authorizer."
```

### Task 2.4: Add Engine.EvalIn

**Files:**
- Modify: `engine.go`
- Test: `engine_unit_test.go`

**Step 1: Write the failing test**

```go
func TestEngine_EvalIn(t *testing.T) {
    ctx := context.Background()
    eng, _ := NewEngine(ctx)

    // Define x in main namespace
    eng.Eval(ctx, "(define x 42)")

    // Create isolated namespace
    ns2, _ := NewNamespace(ctx)
    // x should not be visible
    _, err := eng.EvalIn(ctx, "x", ns2)
    qt.Assert(t, qt.IsNotNil(err))
}
```

**Step 2: Implement EvalIn**

In `engine.go`:

```go
func (p *Engine) EvalIn(ctx context.Context, code string, ns *environment.Namespace) (values.Value, error) {
    reg, ok := ns.Registry().(*registry.Registry)
    if !ok {
        return nil, werr.WrapForeignErrorf(werr.ErrEngineInit, "EvalIn: namespace has no registry")
    }
    // Compile and run using the target namespace's environment
    env := ns.Runtime()
    // ... expand, compile, run in env ...
}
```

**Step 3: Run tests, commit**

```bash
make test
git commit -m "feat: add Engine.EvalIn for evaluating in alternate namespaces"
```

### Task 2.5: Verify and Commit PR 2

```bash
make lint && make test
```

---

## PR 3: Move Authorizer from Context to Namespace

### Task 3.1: Add Authorizer Field to Namespace

**Files:**
- Modify: `environment/namespace.go`
- Test: `environment/namespace_test.go`

**Step 1: Write test**

```go
func TestNamespace_AuthorizerField(t *testing.T) {
    ns := NewNamespace()
    if ns.Authorizer() != nil {
        t.Error("expected nil authorizer on fresh namespace")
    }
}
```

**Step 2: Add field and accessors**

In `environment/namespace.go`:

```go
type Namespace struct {
    // ... existing fields ...
    authorizer any    // security.Authorizer — any to avoid import cycle
}

func (p *Namespace) Authorizer() any {
    return p.authorizer
}

func (p *Namespace) SetAuthorizer(auth any) {
    p.authorizer = auth
}
```

**Step 3: Update Derive to inherit authorizer**

```go
func (p *Namespace) Derive() *Namespace {
    child := p.NewChildNamespace()
    child.registry = p.registry
    child.authorizer = p.authorizer    // inherit by default
    return child
}
```

**Step 4: Run tests, commit**

```bash
go test -v ./environment/...
git commit -m "feat(environment): add authorizer field to Namespace"
```

### Task 3.2: Wire Authorizer Through Namespace Instead of Context

**Files:**
- Modify: `engine.go` — remove `withAuth()`, store authorizer on namespace
- Modify: `security/context.go` — add `CheckWithAuthorizer(auth, req)` function
- Modify: 4 gate-site files (9 call sites total)

**Step 1: Add CheckWithAuthorizer to security package**

In `security/context.go`:

```go
// CheckWithAuthorizer checks authorization using an explicit authorizer.
// Returns nil if auth is nil (open by default).
func CheckWithAuthorizer(auth Authorizer, req AccessRequest) error {
    if auth == nil {
        return nil
    }
    err := auth.Authorize(req)
    if err == nil {
        return nil
    }
    return werr.WrapForeignErrorf(err, "%s %s %q", req.Action, req.Resource, req.Target)
}
```

**Step 2: Add authorizer accessor to MachineContext**

The gate sites use `mc.Context()` today. They need access to the namespace's authorizer. `MachineContext` already has `EnvironmentFrame()`, which has `Namespace()`.

In each gate site, change:

```go
// Before:
err = security.Check(mc.Context(), security.AccessRequest{...})

// After:
auth, _ := mc.EnvironmentFrame().Namespace().Authorizer().(security.Authorizer)
err = security.CheckWithAuthorizer(auth, security.AccessRequest{...})
```

Or add a helper method to reduce boilerplate:

In `machine/machine_context.go`:

```go
func (p *MachineContext) Authorizer() security.Authorizer {
    auth, _ := p.EnvironmentFrame().Namespace().Authorizer().(security.Authorizer)
    return auth
}
```

Then gate sites become:

```go
err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{...})
```

**Step 3: Update all 9 gate sites**

| File | Line | Change |
|------|------|--------|
| `extensions/files/prim_files.go` | 38 | `security.Check(mc.Context(), ...)` → `security.CheckWithAuthorizer(mc.Authorizer(), ...)` |
| `extensions/files/prim_files.go` | 93 | Same |
| `extensions/files/prim_files.go` | 113 | Same |
| `extensions/files/prim_files.go` | 151 | Same |
| `extensions/system/prim_system.go` | 63 | Same |
| `extensions/system/prim_system.go` | 108 | Same |
| `extensions/system/prim_system.go` | 128 | Same |
| `machine/file_resolver.go` | 86 | Uses `ctx`, not `mc` — needs authorizer passed via resolver or context |
| `machine/library_loader.go` | 88 | Same as file_resolver — uses `ctx` |

**Note:** The two `machine/` gate sites (`file_resolver.go:86`, `library_loader.go:88`) don't have a `MachineContext` — they receive a plain `context.Context`. These need the authorizer passed differently:

Option A: Keep `context.WithValue` for these two compile-time sites only.
Option B: Pass authorizer explicitly through file resolver and library loader.

Option B is cleaner. The `OSFileResolver` can hold an `Authorizer` field, set when the resolver is created. The `LoadLibrary` function can take an authorizer parameter or read it from the `EnvironmentFrame` it already receives.

**Step 4: Remove withAuth from Engine**

In `engine.go`, delete the `withAuth()` method and all calls to it. Remove the `authorizer` field from `Engine` (it now lives on `Namespace`).

**Step 5: Remove context-based authorizer plumbing**

In `security/context.go`:
- Keep `Check(ctx, req)` as deprecated (or remove entirely if no other consumers)
- Keep `WithAuthorizer(ctx, auth)` and `FromContext(ctx)` only if needed by the two compile-time sites

**Step 6: Update TODO.md**

Remove the item: "Security context — Authorizer rides on `context.valueContext`"

**Step 7: Run tests, commit**

```bash
make lint && make test
git commit -m "refactor: move authorizer from context to Namespace

Gate sites now read authorizer from mc.Authorizer() which delegates
to mc.EnvironmentFrame().Namespace().Authorizer(). Removes the
context.WithValue plumbing.

Closes TODO.md: 'Security context — Authorizer rides on
context.valueContext'"
```

---

## PR 4: Module Instance Tracking

### Task 4.1: Add ModuleInstance Type and Storage

**Files:**
- Modify: `environment/namespace.go`
- Test: `environment/namespace_test.go`

**Step 1: Write the failing test**

```go
func TestNamespace_ModuleInstances(t *testing.T) {
    ns := NewNamespace()

    // No instance initially
    _, ok := ns.ModuleInstance("(scheme base)")
    if ok {
        t.Error("expected no module instance")
    }

    // Register an instance
    inst := &ModuleInstance{
        exports: make(map[string]*GlobalIndex),
    }
    ns.SetModuleInstance("(scheme base)", inst)

    got, ok := ns.ModuleInstance("(scheme base)")
    if !ok || got != inst {
        t.Error("expected to retrieve registered instance")
    }
}
```

**Step 2: Add types and methods**

In `environment/namespace.go`:

```go
// ModuleInstance represents a loaded and initialized library.
type ModuleInstance struct {
    Env     *EnvironmentFrame
    Exports map[string]*GlobalIndex
}

func (p *Namespace) ModuleInstance(path string) (*ModuleInstance, bool) {
    if p.moduleInstances == nil {
        return nil, false
    }
    inst, ok := p.moduleInstances[path]
    return inst, ok
}

func (p *Namespace) SetModuleInstance(path string, inst *ModuleInstance) {
    if p.moduleInstances == nil {
        p.moduleInstances = make(map[string]*ModuleInstance)
    }
    p.moduleInstances[path] = inst
}
```

**Step 3: Run tests, commit**

```bash
go test -v -run TestNamespace_ModuleInstances ./environment/...
git commit -m "feat(environment): add module instance tracking to Namespace"
```

### Task 4.2: Cache Module Instances in Library Loader

**Files:**
- Modify: `machine/library_loader.go`
- Test: `machine/library_test.go`

**Step 1: Write the failing test**

```go
func TestLibraryLoader_CachesInstance(t *testing.T) {
    // Load a library, verify it's cached on the namespace
    // Load the same library again, verify it reuses the cached instance
    // (no re-initialization)
}
```

**Step 2: Modify LoadLibrary to check cache and store results**

In `machine/library_loader.go`, after successful library initialization:

```go
// After library init completes, cache the instance
ns := callerEnv.Namespace()
ns.SetModuleInstance(resolvedPath, &environment.ModuleInstance{
    Env:     libEnv,
    Exports: exportedBindings,
})
```

At the start of `LoadLibrary`, check cache first:

```go
ns := callerEnv.Namespace()
if inst, ok := ns.ModuleInstance(resolvedPath); ok {
    // Reuse cached instance — copy exports to caller
    copyExports(inst.Exports, callerEnv)
    return callerEnv, nil
}
```

**Step 3: Run tests, commit**

```bash
make test
git commit -m "feat(machine): cache module instances in Namespace

Library loader checks namespace cache before loading. After first
load, subsequent imports of the same library in the same namespace
reuse the cached instance."
```

### Task 4.3: Derive Gets Empty Instance Table

**Files:**
- Modify: `environment/namespace.go`
- Test: `environment/namespace_test.go`

**Step 1: Write the failing test**

```go
func TestNamespace_Derive_IsolatesModuleInstances(t *testing.T) {
    parent := NewNamespace()
    parent.SetModuleInstance("(scheme base)", &ModuleInstance{})

    child := parent.Derive()

    _, ok := child.ModuleInstance("(scheme base)")
    if ok {
        t.Error("derived namespace should not inherit module instances")
    }
}
```

**Step 2: Verify Derive does NOT copy moduleInstances**

`Derive` already creates a fresh `Namespace` via `NewChildNamespace` which initializes with nil `moduleInstances`. Verify the test passes without code changes. If not, ensure `Derive` does not copy the map.

**Step 3: Commit**

```bash
git commit -m "test(environment): verify Derive isolates module instances"
```

### Task 4.4: Add AttachModule

**Files:**
- Modify: `environment/namespace.go`
- Test: `environment/namespace_test.go`

**Step 1: Write the failing test**

```go
func TestNamespace_AttachModule(t *testing.T) {
    source := NewNamespace()
    inst := &ModuleInstance{}
    source.SetModuleInstance("(scheme write)", inst)

    target := NewNamespace()
    err := source.AttachModule("(scheme write)", target)
    if err != nil {
        t.Fatal(err)
    }

    got, ok := target.ModuleInstance("(scheme write)")
    if !ok || got != inst {
        t.Error("attached module instance should be shared (same pointer)")
    }

    // Attaching a non-existent module should error
    err = source.AttachModule("(scheme nonexistent)", target)
    if err == nil {
        t.Error("expected error for non-existent module")
    }
}
```

**Step 2: Implement AttachModule**

```go
func (p *Namespace) AttachModule(path string, target *Namespace) error {
    inst, ok := p.moduleInstances[path]
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotFound, "attachModule: %s not loaded in source namespace", path)
    }
    target.SetModuleInstance(path, inst)
    return nil
}
```

Note: `environment/` importing `werr/` is fine — `werr` is at the bottom of the dependency graph.

**Step 3: Run tests, commit**

```bash
go test -v -run TestNamespace_AttachModule ./environment/...
git commit -m "feat(environment): add Namespace.AttachModule for sharing module instances"
```

### Task 4.5: Add GlobalEnvironmentFrame.DeleteBinding

**Files:**
- Modify: `environment/global_environment_frame.go`
- Test: `environment/global_environment_frame_test.go`

**Step 1: Write the failing test**

```go
func TestGlobalEnvironmentFrame_DeleteBinding(t *testing.T) {
    ns := NewNamespace()
    env := ns.Runtime()

    sym := values.NewSymbol("x")
    gi, created := env.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
    qt.Assert(t, qt.IsTrue(created))
    env.SetOwnGlobalValue(gi, values.NewInteger(42))

    // Verify binding exists
    b := env.GetBinding(sym)
    qt.Assert(t, qt.IsNotNil(b))

    // Delete it
    deleted := env.Global().DeleteBinding(sym)
    qt.Assert(t, qt.IsTrue(deleted))

    // Verify binding is gone
    b = env.GetBinding(sym)
    qt.Assert(t, qt.IsNil(b))

    // Deleting non-existent binding returns false
    deleted = env.Global().DeleteBinding(values.NewSymbol("nonexistent"))
    qt.Assert(t, qt.IsFalse(deleted))
}
```

**Step 2: Implement DeleteBinding**

In `environment/global_environment_frame.go`:

```go
func (p *GlobalEnvironmentFrame) DeleteBinding(sym *values.Symbol) bool {
    p.mu.Lock()
    defer p.mu.Unlock()

    key := sym.Key()
    _, ok := p.keys[key]
    if !ok {
        return false
    }
    delete(p.keys, key)
    // Note: binding slot in p.bindings is not compacted — index-based
    // references from compiled code would be stale. This is only safe
    // for top-level REPL/eval bindings, not for bindings referenced
    // by compiled bytecode.
    return ok
}
```

**Step 3: Run tests, commit**

```bash
go test -v -run TestGlobalEnvironmentFrame_DeleteBinding ./environment/...
git commit -m "feat(environment): add GlobalEnvironmentFrame.DeleteBinding"
```

### Task 4.6: Verify and Commit PR 4

```bash
make lint && make test
```

---

## PR 5: Scheme API

### Task 5.1: Create Namespace Extension

**Files:**
- Create: `internal/extensions/namespace/register.go`
- Create: `internal/extensions/namespace/prim_namespace.go`

**Step 1: Create extension skeleton**

In `internal/extensions/namespace/register.go`:

```go
package namespace

import "github.com/aalpar/wile/registry"

var Extension = &extension{}

type extension struct{}

func (e *extension) AddToRegistry(reg *registry.Registry) {
    reg.AddPrimitive("namespace?", PrimNamespaceQ, 1, false, "namespace", "")
    reg.AddPrimitive("make-namespace", PrimMakeNamespace, -1, false, "namespace", "")
    reg.AddPrimitive("namespace-derive", PrimNamespaceDerive, -1, false, "namespace", "")
    reg.AddPrimitive("namespace-define!", PrimNamespaceDefine, 3, false, "namespace", "")
    reg.AddPrimitive("namespace-ref", PrimNamespaceRef, -1, false, "namespace", "")
    reg.AddPrimitive("namespace-bound?", PrimNamespaceBound, 2, false, "namespace", "")
    reg.AddPrimitive("namespace-undefine!", PrimNamespaceUndefine, 2, false, "namespace", "")
    reg.AddPrimitive("namespace-bound-names", PrimNamespaceBoundNames, 1, false, "namespace", "")
    reg.AddPrimitive("namespace-require", PrimNamespaceRequire, 2, false, "namespace", "")
    reg.AddPrimitive("namespace-name", PrimNamespaceName, 1, false, "namespace", "")
}
```

**Step 2: Implement primitives**

In `internal/extensions/namespace/prim_namespace.go`, implement each primitive. Example for `namespace-define!`:

```go
func PrimNamespaceDefine(mc *machine.MachineContext) error {
    ns, ok := mc.Arg(0).(*environment.Namespace)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrTypeMismatch, "namespace-define!: expected namespace")
    }
    sym, ok := mc.Arg(1).(*values.Symbol)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrTypeMismatch, "namespace-define!: expected symbol")
    }
    val := mc.Arg(2)

    env := ns.Runtime()
    gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
    env.SetOwnGlobalValue(gi, val)
    mc.SetValue(values.VoidValue)
    return nil
}
```

**Step 3: Run tests, commit**

```bash
go test -v ./internal/extensions/namespace/...
git commit -m "feat: add namespace extension with Scheme primitives"
```

### Task 5.2: Register Extension in Engine

**Files:**
- Modify: `engine.go` (or `internal/extensions/all/`)

Add the namespace extension to the default extension set so it's available without explicit `WithExtension`. The namespace primitives are core functionality, not optional.

**Step 1: Add to core or default extensions**

```go
import nsext "github.com/aalpar/wile/internal/extensions/namespace"

// In buildRegistry or equivalent:
nsext.Extension.AddToRegistry(reg)
```

**Step 2: Run tests, commit**

```bash
make test
git commit -m "feat: register namespace extension in default engine"
```

### Task 5.3: Add current-namespace Parameter

**Files:**
- Modify: `internal/extensions/namespace/prim_namespace.go`
- Modify: bootstrap macro sources or init code

**Step 1: Create the parameter**

`current-namespace` is a Scheme parameter initialized to `(interaction-environment)`. It can be defined in the namespace extension's init function:

```go
func (e *extension) Init(mc *machine.MachineContext) error {
    // Create the current-namespace parameter
    ns := mc.EnvironmentFrame().Namespace()
    param := values.NewParameter(ns, nil)  // no converter
    // Register as a global binding
    env := mc.EnvironmentFrame()
    sym := values.NewSymbol("current-namespace")
    gi, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
    env.SetOwnGlobalValue(gi, param)
    return nil
}
```

**Step 2: Run tests, commit**

```bash
make test
git commit -m "feat: add current-namespace parameter"
```

### Task 5.4: Add 1-arg eval Form

**Files:**
- Modify: `internal/extensions/eval/prim_eval.go`
- Test: `test/scheme/namespace-test.scm`

**Step 1: Modify PrimEval to handle 1-arg case**

In `internal/extensions/eval/prim_eval.go`:

```go
func PrimEval(mc *machine.MachineContext) error {
    expr := mc.Arg(0)

    var ns *environment.Namespace
    if mc.ArgCount() >= 2 {
        // 2-arg form: (eval expr env)
        envSpec := mc.Arg(1)
        var ok bool
        ns, ok = envSpec.(*environment.Namespace)
        if !ok {
            return werr.WrapForeignErrorf(werr.ErrTypeMismatch, "eval: expected namespace")
        }
    } else {
        // 1-arg form: (eval expr) — use current-namespace
        param := mc.EnvironmentFrame().GetBinding(values.NewSymbol("current-namespace"))
        if param == nil {
            return werr.WrapForeignErrorf(werr.ErrUndefined, "eval: current-namespace not defined")
        }
        ns = resolveParameterToNamespace(mc, param)
    }

    env := ns.Runtime()
    // ... rest of eval unchanged ...
}
```

Also update the arity registration from `2, false` to `-1, false` (variadic).

**Step 2: Run tests, commit**

```bash
make test
git commit -m "feat: eval accepts 1-arg form using current-namespace"
```

### Task 5.5: Add Compatibility Aliases

**Files:**
- Modify: `extensions/introspection/prim_introspection.go`

**Step 1: Make environment-* primitives delegate to namespace-* equivalents**

The existing `environment-ref`, `environment-bound?`, `environment-bound-names` already operate on `TopLevelEnvironment` (now `Namespace`). They already work. Keep them as-is — they're already namespace operations under the old name.

Add `environment?` as an alias for `namespace?` if not already present.

**Step 2: Commit**

```bash
git commit -m "docs: document environment-* as aliases for namespace-*"
```

### Task 5.6: Write Scheme Test Suite

**Files:**
- Create: `test/scheme/namespace-test.scm`

```scheme
;; namespace-test.scm — Tests for namespace primitives

(import (chibi test))

(test-group "namespace"
  (test-group "predicates"
    (test-assert (namespace? (current-namespace)))
    (test-assert (not (namespace? 42))))

  (test-group "make-namespace"
    (let ((ns (make-namespace '(scheme base))))
      (test-assert (namespace? ns))
      (test-assert (namespace-bound? ns '+))
      (test-assert (not (namespace-bound? ns 'nonexistent)))))

  (test-group "define and ref"
    (let ((ns (make-namespace '(scheme base))))
      (namespace-define! ns 'x 42)
      (test 42 (namespace-ref ns 'x))
      (test-assert (namespace-bound? ns 'x))
      (test 'default (namespace-ref ns 'nonexistent 'default))))

  (test-group "undefine"
    (let ((ns (make-namespace '(scheme base))))
      (namespace-define! ns 'y 10)
      (test-assert (namespace-bound? ns 'y))
      (namespace-undefine! ns 'y)
      (test-assert (not (namespace-bound? ns 'y)))))

  (test-group "bound-names"
    (let ((ns (make-namespace)))
      (namespace-define! ns 'a 1)
      (namespace-define! ns 'b 2)
      (let ((names (namespace-bound-names ns)))
        (test-assert (memq 'a names))
        (test-assert (memq 'b names)))))

  (test-group "derive"
    (let* ((parent (make-namespace '(scheme base)))
           (child (namespace-derive parent)))
      (test-assert (namespace? child))
      ;; child has scheme base
      (test-assert (namespace-bound? child '+))
      ;; parent define doesn't leak to child
      (namespace-define! parent 'z 99)
      (test-assert (not (namespace-bound? child 'z)))))

  (test-group "eval with current-namespace"
    (let ((ns (make-namespace '(scheme base))))
      (parameterize ([current-namespace ns])
        (eval '(define w 7))
        (test 7 (eval 'w)))))

  (test-group "namespace-require"
    (let ((ns (make-namespace '(scheme base))))
      (namespace-require ns '(scheme write))
      ;; display should now be bound
      (test-assert (namespace-bound? ns 'display))))
)
```

**Step 1: Run tests**

```bash
./dist/darwin/arm64/wile --file test/scheme/namespace-test.scm
```

**Step 2: Commit**

```bash
git commit -m "test: comprehensive Scheme test suite for namespace primitives"
```

### Task 5.7: Update PRIMITIVES.md

**Files:**
- Modify: `PRIMITIVES.md`

Add a new "Namespaces" section after the existing "Evaluation" section:

```markdown
## Namespaces

| Primitive | Description |
|-----------|-------------|
| `make-namespace` | Create namespace, optionally pre-loaded with libraries |
| `namespace-derive` | Create child namespace with shared interning, isolated bindings |
| `namespace?` | Test for namespace |
| `namespace-name` | Get namespace name (for debugging) |
| `namespace-define!` | Create or update binding in namespace |
| `namespace-ref` | Look up binding by symbol |
| `namespace-bound?` | Test if symbol is bound |
| `namespace-undefine!` | Remove binding from namespace |
| `namespace-bound-names` | List all bound symbols |
| `namespace-require` | Dynamically load library into namespace |
| `current-namespace` | Parameter: active namespace for eval |
```

**Step 1: Commit**

```bash
git commit -m "docs: add namespace primitives to PRIMITIVES.md"
```

### Task 5.8: Final Verification

```bash
make lint && make covercheck
make test
make bench-gabriel   # verify no performance regression
```

Compare Gabriel benchmark results against baseline. All benchmarks should be within noise (±2%).

```bash
git commit -m "feat: namespace Scheme API complete

Adds namespace primitives: make-namespace, namespace-derive,
namespace-define!, namespace-ref, namespace-bound?,
namespace-undefine!, namespace-bound-names, namespace-require,
namespace?, namespace-name, current-namespace parameter.

eval gains 1-arg form using current-namespace.
R7RS environment-* primitives kept as aliases."
```

---

## Summary

| PR | Tasks | Estimated files | Key risk |
|----|-------|-----------------|----------|
| 1: Rename | 1.1–1.6 | ~122 | Mechanical but high-fanout; must be done atomically |
| 2: Registry | 2.1–2.5 | ~8 | Backward compat — existing `NewEngine` callers must work |
| 3: Authorizer | 3.1–3.2 | ~8 | Two compile-time gate sites don't have `MachineContext` |
| 4: Module instances | 4.1–4.6 | ~5 | Library loader caching needs careful re-load semantics |
| 5: Scheme API | 5.1–5.8 | ~10 | `eval` 1-arg form + `current-namespace` parameter wiring |

Dependencies: PR 1 → PR 2 → PR 4 → PR 5. PR 3 depends on PR 1 only.
