# Plan: Extension-Level Sandboxing Model

**Status**: Proposed
**Date**: 2026-02-20
**Related**: `AUTHORIZATION_FRAMEWORK.md` (fine-grained layer), `docs/EXTENSIONS.md`

## Motivation

Embedders need to control what capabilities Scheme code has access to. The library system's `export` mechanism controls outward visibility (what importers see), not inward capability (what code inside a library can do). Sandboxing requires inward restriction.

Wile already has the right architecture for coarse-grained sandboxing — extensions are opt-in via `WithExtension()` — but this isn't documented as a security boundary, there's no convenience API for common sandbox profiles, and the relationship between extension-level and fine-grained authorization isn't articulated.

This plan documents the extension-level sandboxing model and the concrete work to make it usable.

## Architecture: Two-Layer Sandboxing

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 1: Extension-level (this plan)                        │
│                                                             │
│ Controls WHICH primitives exist in the engine.              │
│ Mechanism: WithExtension() at Engine construction.          │
│ Granularity: per-extension (io, files, eval, threads, etc.) │
│ Enforcement: primitives not in registry don't exist.        │
│ Propagation: LibraryEnvFactory inherits engine's registry.  │
└─────────────────────────────────────────────────────────────┘
┌─────────────────────────────────────────────────────────────┐
│ Layer 2: Authorization (AUTHORIZATION_FRAMEWORK.md)         │
│                                                             │
│ Controls WHAT operations primitives can perform.            │
│ Mechanism: Authorizer via context.Context.                  │
│ Granularity: per-operation (read file X, delete file Y).    │
│ Enforcement: Check() call inside each gated primitive.      │
└─────────────────────────────────────────────────────────────┘
```

Layer 1 is coarse but zero-cost: if the filesystem extension isn't loaded, filesystem primitives don't exist — no runtime checks needed. Layer 2 adds fine-grained control within loaded extensions (e.g., allow reading `/data/` but deny writing).

Most embedders need only Layer 1. Layer 2 is for cases where an extension must be partially available (e.g., filesystem read but not write).

## Extension Security Classification

Each extension falls into one of three categories:

| Category | Extensions | Threat |
|----------|-----------|--------|
| **Safe** | core, io, exceptions, math, all | Pure computation + in-memory ports. No ambient authority. |
| **Privileged** | files, eval, system, gointerop | Ambient authority: filesystem, process env, code loading, Go bridge. |
| **Context-dependent** | threads | Resource exhaustion, concurrency bugs. Safe for trusted code. |

### Detailed breakdown

**Safe extensions** — no external side effects:

| Extension | Package | What it provides |
|-----------|---------|-----------------|
| core | `registry/core` | Pairs, lists, arithmetic, control, vectors, strings, characters, bytevectors, syntax, parameters, prompts, boxes, hashtables, equality, predicates, special forms, bootstrap macros |
| io | `internal/extensions/io` | `read`, `write`, `display`, `newline`, string/bytevector ports, `current-input-port`, `current-output-port`, `current-error-port`, port predicates, binary I/O. All in-memory or on caller-provided ports — no filesystem access. |
| exceptions | `extensions/exceptions` | `raise`, `with-exception-handler`, `guard`, `error`, `error-object?` |
| math | `extensions/math` | `sqrt`, `sin`, `cos`, trigonometric/transcendental functions |
| all | `internal/extensions/all` | Records, promises, `string-copy!`, `string-fill!` |

**Privileged extensions** — require trust or authorization:

| Extension | Package | Capability granted |
|-----------|---------|-------------------|
| files | `extensions/files` | `open-input-file`, `open-output-file`, `delete-file`, `file-exists?`, `call-with-input-file`, `call-with-output-file`, `with-input-from-file`, `with-output-to-file` |
| eval | `internal/extensions/eval` | `eval`, `load`, `interaction-environment`, `scheme-report-environment`, `null-environment`, `environment`, `syntax-expand`, `compile` |
| system | `extensions/system` | `exit`, `emergency-exit`, `command-line`, `get-environment-variable`, `get-environment-variables` |
| gointerop | `extensions/gointerop` | Go function bridge — arbitrary Go code execution |

**Context-dependent:**

| Extension | Package | Risk |
|-----------|---------|------|
| threads | `extensions/threads` | SRFI-18 threads, mutexes, condition variables. Resource exhaustion via unbounded thread creation. Safe for trusted code; risky for untrusted. |

## How propagation works

The `LibraryEnvFactory` in `engine.go:182-211` closes over the engine's registry. When a library is loaded via `(import ...)`, the factory creates a new environment and applies the *same* registry. This means:

1. If files extension isn't loaded, no library can access the filesystem — even if a `.sld` file on disk contains `(import (scheme file))`.
2. Extension libraries registered as synthetic R7RS libraries (e.g., `(wile math)`) only export primitives that exist in the engine's registry.
3. Standard R7RS libraries loaded from `.sld` files get environments created by the factory, so they inherit the same restrictions.

**The restriction is transitive and hermetic** — there's no way for Scheme code to escalate privileges within a single engine.

## Concrete work items

### Phase 1: Convenience API

Add a `SafeExtensions()` function that returns the safe extension set:

```go
// SafeExtensions returns extensions suitable for sandboxed engines:
// io, exceptions, math, and all. These provide R7RS (scheme base)
// functionality without filesystem, eval, system, or Go interop access.
func SafeExtensions() []EngineOption
```

Usage:

```go
eng, err := wile.NewEngine(ctx,
    append(wile.SafeExtensions(),
        wile.WithLibraryPaths("./lib"),
    )...,
)
```

**Location**: `options.go`

**Open question**: Should this be a single `WithSafeExtensions()` option or a function returning a slice? The slice is more composable (`append(SafeExtensions(), WithExtension(threads.Extension))`), but a single option is simpler for the common case. Leaning toward the single option with the slice available as a package-level variable.

### Phase 2: Document security boundaries

Add a `docs/SANDBOXING.md` covering:

- The two-layer model (extension-level + authorization)
- Extension security classification table
- Example: minimal sandbox, safe sandbox, full-featured sandbox
- How library propagation works
- What sandboxing does NOT cover (resource limits, CPU/memory, stack depth — separate concerns; `WithMaxCallDepth` exists for stack)
- Relationship to `AUTHORIZATION_FRAMEWORK.md`

### Phase 3: Verify isolation invariants

Write integration tests that verify:

1. An engine without files extension rejects `(open-input-file "x")` with an unbound-variable error (not a runtime error — the binding shouldn't exist at all).
2. A library loaded by a restricted engine also lacks the restricted primitives.
3. `(import (scheme file))` fails in a restricted engine (library exists on disk but its environment lacks the primitives, or the synthetic library isn't registered).
4. `eval` in a restricted engine (without eval extension) is unbound.

**Location**: `integration/sandbox_test.go` or `engine_sandbox_test.go`

### Phase 4: Registry filtering

Add methods to `Registry` that let embedders subtract primitives from a fully-populated registry. This enables fine-grained control within extensions without requiring every extension to be rebuilt from scratch.

**The use case:** An embedder wants the full language minus mutation — no `set!`, `set-car!`, `set-cdr!`, `vector-set!`, `string-set!`, `list-set!`, `hashtable-set!`, `set-box!`. Or: include the io extension but remove `read` (output-only sandbox). Extension-level filtering (Phase 1) can't express this; you need per-primitive granularity.

**API — two methods on `Registry`:**

```go
// Without returns a new Registry with the named primitives removed.
// Names that don't match any registered primitive are silently ignored.
// Compile-time bindings, init funcs, macro sources, and global values
// are copied unchanged.
func (p *Registry) Without(names ...string) *Registry

// WithoutCategory returns a new Registry with all primitives in the
// named categories removed. Categories are matched against PrimitiveSpec.Category.
func (p *Registry) WithoutCategory(categories ...string) *Registry
```

**Usage:**

```go
// Immutable sandbox: full core, no mutation primitives
reg := registry.NewRegistry()
core.AddToRegistry(reg)
io.AddToRegistry(reg)
restricted := reg.Without(
    "set!", "set-car!", "set-cdr!",
    "vector-set!", "string-set!", "list-set!",
    "hashtable-set!", "hashtable-delete!", "hashtable-clear!",
    "set-box!",
)
eng, err := wile.NewEngine(ctx, wile.WithRegistry(restricted))
```

```go
// Remove all hashtable primitives by category
restricted := reg.WithoutCategory("hashtables")
```

**Implementation:** Both methods iterate `p.primitives`, skip matches, copy the rest into a new `Registry`. `Clone()` already exists and provides the pattern. Bindings, init funcs, macro sources, and global values are copied as-is — filtering only applies to primitives.

**What about compile-time bindings?** `set!` is both a compile-time binding (in `specialforms.go`) and a runtime operation (compiled by the compiler). Removing the primitive from the registry doesn't remove the compile-time binding — the compiler would still recognize `set!` as a special form but would have no runtime implementation. This would produce a compile-time error, which is the correct behavior: "I know what `set!` means, but you can't use it here."

However, if the goal is to make `set!` completely unbound (as if it never existed), the filter would also need to operate on compile-time bindings. This suggests a third method or an option:

```go
// WithoutBindings returns a new Registry with the named compile-time
// bindings also removed. Use after Without() to fully erase a name.
func (p *Registry) WithoutBindings(names ...string) *Registry
```

**Open question:** Is compile-error-on-`set!` sufficient, or does it need to be fully unbound? Compile error is arguably better — it tells the programmer "mutation is disallowed" rather than the confusing "undefined variable: set!". But this is a UX decision for the embedder.

**What filtering does NOT cover:**
- Bootstrap macros are registered as source strings, not as named primitives. You can't selectively remove `cond` or `let` via `Without()`. This is fine — derived forms are pure syntax transformations with no capability implications.
- Syntax compilers and primitive expanders are registered via `machine.RegisterSyntaxCompilers` / `machine.RegisterPrimitiveExpanders`, outside the registry. These are core language constructs (`if`, `lambda`, `define`), not capabilities.

**Location:** `registry/registry.go` (methods on `*Registry`)

**Existing infrastructure:**
- `Registry.Clone()` — deep copy pattern to follow
- `PrimitiveSpec.Category` — already on every primitive (15 categories in core, plus extension categories like `"io"`, `"ports"`, `"eval"`, `"records"`, `"promises"`)
- `PrimitivesByCategory()` — read-only grouping, exists but only for inspection

### Phase 5: Library name in factory signature

Pass the library name through `LibraryEnvFactory` so the factory knows which library it's creating an environment for. This is a low-cost signature change that enables per-library policies in the future and makes the loading path self-documenting.

**Current signature** (`environment/top_level_environment.go:29`):

```go
type LibraryEnvFactory func(context.Context, *EnvironmentFrame) (*EnvironmentFrame, error)
```

**Proposed signature:**

```go
type LibraryEnvFactory func(context.Context, *EnvironmentFrame, machine.LibraryName) (*EnvironmentFrame, error)
```

**Note:** This introduces a dependency from `environment/` on `machine.LibraryName`. If that's undesirable (layering violation — `environment/` is lower than `machine/`), alternatives:
- Move `LibraryName` to `environment/` or a shared package (e.g., `values/`)
- Use `[]string` directly in the signature and convert at the call site
- Define a minimal interface or type alias in `environment/`

The `[]string` option is simplest and avoids the dependency entirely:

```go
type LibraryEnvFactory func(context.Context, *EnvironmentFrame, []string) (*EnvironmentFrame, error)
```

**Blast radius** — well-contained:

| What | Where | Change |
|------|-------|--------|
| Type definition | `environment/top_level_environment.go:29` | Add `[]string` param |
| Field + getter + setter | `environment/top_level_environment.go:80,236,241` | Follows from type |
| Call site (only one) | `machine/library_loader.go:130` | Pass `expectedName.Parts` — already in scope |
| Engine factory closure | `engine.go:182` | Add param to closure signature (ignore or log) |
| Bootstrap factory | `internal/bootstrap/environment_tiny.go:148` | Add param to `NewLibraryEnvironmentFrame` |
| CLI setup | `cmd/scheme/main.go:191` | Points to bootstrap factory |
| Tests | `machine/library_test.go:309,745`, `machine/library_scheme_test.go:54` | Point to bootstrap factory |
| Docs | `docs/EXTENSION_LIBRARIES.md`, `docs/dev/ENVIRONMENT_SYSTEM.md` | Text updates |

Factories that don't need the name ignore the parameter. The engine factory could log it or pass it to a future policy callback.

### Phase 6: Library load observability

Add an optional observer that records library load events. This gives embedders visibility into which libraries were loaded, what they exported, and what bindings actually flowed into the importer.

**Observer interface:**

```go
// LibraryImportEvent records what happened when a library was imported.
type LibraryImportEvent struct {
    Library     []string          // imported library name parts, e.g., ["scheme", "base"]
    SourceFile  string            // path to .sld file (empty for synthetic libraries)
    Exports     []string          // all names exported by the library
    Imported    []string          // names that actually landed in the importer (after only/except/prefix/rename)
    Importer    []string          // importing library name (nil for top-level import)
}
```

**Integration points:**

There are three sites that process `(import ...)`:

| Site | File | Context |
|------|------|---------|
| `processLibraryImport` | `compile_time_continuation_library.go:300` | Inside `define-library` — has `lib *CompiledLibrary`, so importer name is `lib.Name` |
| `CompileImport` | `compile_time_continuation_library.go:717` | Top-level `(import ...)` — importer is `nil` (script/REPL) |
| `expandImportForm` | `expander_time_continuation.go:747` | Top-level during expansion — importer is `nil` (script/REPL) |

The key insight: `processLibraryImport` already receives the importing library as `lib *CompiledLibrary`. The importer identity is in scope — it's just not passed further. The other two sites are top-level imports where `nil` is the correct importer identity (not a missing value).

All three sites follow the same sequence:
1. `LoadLibrary` → get `*CompiledLibrary` (library name, exports, source file)
2. `ApplyToExports` → get post-modifier bindings (what actually lands)
3. `CopyLibraryBindingsToEnvAtPhase` → install bindings

The callback fires between steps 2 and 3, where all information converges.

**Where the data lives now:**

| Information | Available at | Currently stored? |
|-------------|-------------|-------------------|
| Library name | `LoadLibrary` / `CompiledLibrary.Name` | Yes — in `LibraryRegistry` |
| Source file | `loadLibraryFromFile` / `CompiledLibrary.SourceFile` | Yes — in `CompiledLibrary` |
| Export list | `CompiledLibrary.Exports` | Yes — in `CompiledLibrary` |
| Post-modifier bindings | `ApplyToExports` return value | **No** — computed and consumed, not stored |
| Importing library name | `processLibraryImport` `lib` param | **In scope but not propagated** |

**Approach:** Add an optional `LibraryImportObserver` callback to `LibraryRegistry` or `TopLevelEnvironment`. Fire it at all three import sites, between `ApplyToExports` and `CopyLibraryBindingsToEnvAtPhase`. The observer is read-only — it observes but doesn't influence the import.

```go
type LibraryImportObserver func(LibraryImportEvent)
```

Embedders that don't set it pay nothing (nil check before firing).

**Future extension path:** If the observer's return type changes from nothing to `error`, it becomes an enforcement hook — a callback that can reject an import. The call sites already have error handling around the import step. This is additive (change the callback signature), not a redesign. The plan does NOT include enforcement; it only includes observation. But the architecture doesn't prevent it.

### Phase 7: Bootstrap factory alignment (optional)

The bootstrap factory (`internal/bootstrap/environment_tiny.go:53-65`) hardcodes `allExtensions` and loads everything. This is the test/standalone path (used by `NewTopLevelEnvironmentFrameTiny` and `NewLibraryEnvironmentFrame`).

For the engine path this doesn't matter — the engine uses its own factory. But the bootstrap factory creates an inconsistency: tests that use `NewLibraryEnvironmentFrame` directly get full capabilities regardless of any restriction the test intends.

**Deferred** — only matters if tests need to verify sandboxed library behavior through the bootstrap path, which they shouldn't.

## Scope boundaries

**Covered by this plan:**
- Extension-level capability control
- Convenience API for sandbox profiles
- Documentation of security boundaries
- Integration tests for isolation
- Registry filtering (`Without`, `WithoutCategory`) for per-primitive control
- Library name in factory signature (forward-compatible for per-library policies)
- Library load observability (what was loaded, what bindings landed where)

**Covered by AUTHORIZATION_FRAMEWORK.md:**
- Fine-grained per-operation authorization within loaded extensions
- `Check(AccessRequest)` at runtime primitive call sites
- `FilesystemRoot`, `ReadOnly`, `DenyAll` authorizers

**Not covered (separate concerns):**
- Resource limits: CPU time, memory, allocation rate — requires VM-level instrumentation
- Stack depth limits: `WithMaxCallDepth` already exists
- Network access: no network primitives exist yet; when added, they'd be a new privileged extension
- Per-library policies (enforcement): Phase 4 passes the library name through the factory, which *enables* per-library policies. The actual policy mechanism (allowlist, callback, etc.) is future work — build when there's demand.
- Information flow: a privileged library can pass capabilities (e.g., open file handle) to an unprivileged library via exported values. Preventing this requires an object-capability model, which is a fundamental architecture change.

## Dependencies

- Phase 1 (convenience API): None
- Phase 2 (documentation): None
- Phase 3 (integration tests): Phase 1
- Phase 4 (registry filtering): None — operates on `Registry` type, independent of other phases
- Phase 5 (factory signature): None (mechanical change)
- Phase 6 (observability): Phase 5 (needs library name in factory for importer tracking)
- Phase 7 (bootstrap alignment): Deferred
- `AUTHORIZATION_FRAMEWORK.md` is independent and can proceed in parallel

## Decision log

| Decision | Rationale |
|----------|-----------|
| Extension-level as primary mechanism | Embedder is the security principal. Extensions are already opt-in. Zero-cost when extension not loaded. |
| `export` is not a security mechanism | `export` controls outward visibility (namespace management), not inward capability. A library with full imports can do anything regardless of its export list. |
| Safe = no ambient authority | Extensions classified as safe have no way to affect the outside world. io extension uses caller-provided ports or in-memory ports — no filesystem access. |
| `[]string` over `LibraryName` in factory | Avoids `environment/` → `machine/` dependency. `LibraryName.Parts` is `[]string` internally; convert at call site. |
| Observer as optional callback | Embedders that don't need observability pay nothing. No allocation, no interface dispatch on the hot path. Observer is read-only — observation now, enforcement later if needed (change return type to `error`). |
| Post-modifier bindings not stored | Computed on demand by `ApplyToExports`. Storing them would mean every import set grows. Observer callback is fire-and-forget — cheaper than persistent storage. |
| Importer identity is already in scope | `processLibraryImport` has `lib *CompiledLibrary` — the importing library name. Top-level sites (`CompileImport`, `expandImportForm`) correctly have `nil` importer. No new threading needed; just pass what's already available into the callback. |
| Subtraction over composition for filtering | `Without()` starts from the full registry and removes. Alternative (export individual `add*` functions for embedders to compose) requires embedders to know the internal structure of core. Subtraction is simpler: start with everything, remove what you don't want. |
| Compile-time binding removal separate from primitive removal | `Without()` removes runtime primitives. Compile-time bindings (`set!` as special form) stay — the compiler recognizes the form but the runtime rejects it. This produces a clear compile error ("mutation disallowed") rather than a confusing unbound-variable error. `WithoutBindings()` available for embedders who want full erasure. |
| Per-library enforcement deferred | Phase 5 threads the library name through the factory. Phase 6 observes imports with importer identity. Enforcement (blocking imports) is a future callback return-type change, not a redesign. |
