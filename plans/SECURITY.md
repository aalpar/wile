# Security Plans

---

# Extension-Level Sandboxing Model

**Status**: Phases 1-6 implemented
**Date**: 2026-02-20
**Related**: Authorization Framework (below), `docs/EXTENSIONS.md`

## Motivation

Embedders need to control what capabilities Scheme code has access to. The library system's `export` mechanism controls outward visibility (what importers see), not inward capability (what code inside a library can do). Sandboxing requires inward restriction.

Wile already has the right architecture for coarse-grained sandboxing — extensions are opt-in via `WithExtension()` — but this isn't documented as a security boundary, there's no convenience API for common sandbox profiles, and the relationship between extension-level and fine-grained authorization isn't articulated.

This plan documents the extension-level sandboxing model and the concrete work to make it usable.

## Architecture: Two-Layer Sandboxing

```
+-------------------------------------------------------------+
| Layer 1: Extension-level (this section)                      |
|                                                              |
| Controls WHICH primitives exist in the engine.               |
| Mechanism: WithExtension() at Engine construction.           |
| Granularity: per-extension (io, files, eval, threads, etc.)  |
| Enforcement: primitives not in registry don't exist.         |
| Propagation: LibraryEnvFactory inherits engine's registry.   |
+-------------------------------------------------------------+
+-------------------------------------------------------------+
| Layer 2: Authorization (see below)                           |
|                                                              |
| Controls WHAT operations primitives can perform.             |
| Mechanism: Authorizer via context.Context.                   |
| Granularity: per-operation (read file X, delete file Y).     |
| Enforcement: Check() call inside each gated primitive.       |
+-------------------------------------------------------------+
```

Layer 1 is coarse but zero-cost: if the filesystem extension isn't loaded, filesystem primitives don't exist — no runtime checks needed. Layer 2 adds fine-grained control within loaded extensions (e.g., allow reading `/data/` but deny writing).

Most embedders need only Layer 1. Layer 2 is for cases where an extension must be partially available (e.g., filesystem read but not write).

## Extension Security Classification

Each extension falls into one of three categories:

| Category | Extensions | Threat |
|----------|-----------|--------|
| **Safe** | core, io, exceptions, math, introspection, all | Pure computation + in-memory ports. No ambient authority. |
| **Privileged** | files, eval, system | Ambient authority: filesystem, process env, code loading. |
| **Context-dependent** | gointerop, threads | Resource exhaustion, concurrency bugs. No ambient authority. Safe for trusted code. |

### Detailed breakdown

**Safe extensions** — no external side effects:

| Extension | Package | What it provides |
|-----------|---------|-----------------|
| core | `registry/core` | Pairs, lists, arithmetic, control, vectors, strings, characters, bytevectors, syntax, parameters, prompts, boxes, hashtables, equality, predicates, special forms, bootstrap macros |
| io | `internal/extensions/io` | `read`, `write`, `display`, `newline`, string/bytevector ports, `current-input-port`, `current-output-port`, `current-error-port`, port predicates, binary I/O. All in-memory or on caller-provided ports — no filesystem access. |
| exceptions | `extensions/exceptions` | `raise`, `with-exception-handler`, `guard`, `error`, `error-object?` |
| math | `extensions/math` | `sqrt`, `sin`, `cos`, trigonometric/transcendental functions |
| introspection | `extensions/introspection` | `environment?`, `interaction-environment`, `environment-bound-names`, `environment-ref`, `environment-bound?`. Read-only environment introspection — no side effects, no new capabilities without `eval`. |
| all (safe subset) | `internal/extensions/all` | Records, promises, `string-copy!`, `string-fill!`, additional string/character ops. Exposed as `all.SafeExtension`; note that `all.Extension` includes *all* sub-extensions and is NOT safe. |

**Privileged extensions** — require trust or authorization:

| Extension | Package | Capability granted |
|-----------|---------|-------------------|
| files | `extensions/files` | `open-input-file`, `open-output-file`, `delete-file`, `file-exists?`, `call-with-input-file`, `call-with-output-file`, `with-input-from-file`, `with-output-to-file` |
| eval | `internal/extensions/eval` | `eval`, `load`, `scheme-report-environment`, `null-environment`, `environment`, `expand`, `compile` |
| system | `extensions/system` | `exit`, `emergency-exit`, `command-line`, `get-environment-variable`, `get-environment-variables` |

**Context-dependent:**

| Extension | Package | Risk |
|-----------|---------|------|
| gointerop | `extensions/gointerop` | Go concurrency primitives: channels, wait groups, rw-mutexes, atomics, once. Resource exhaustion via unbounded object creation. No ambient authority. Safe for trusted code. |
| threads | `extensions/threads` | SRFI-18 threads, mutexes, condition variables. Resource exhaustion via unbounded thread creation. Safe for trusted code; risky for untrusted. |

## How propagation works

The `LibraryEnvFactory` in `engine.go:182` (set via `SetLibraryEnvFactory` on `TopLevelEnvironment`) closes over the engine's registry. When a library is loaded via `(import ...)`, the factory creates a new environment and applies the *same* registry. This means:

1. If files extension isn't loaded, no library can access the filesystem — even if a `.sld` file on disk contains `(import (scheme file))`.
2. Extension libraries registered as synthetic R7RS libraries (e.g., `(wile math)`) only export primitives that exist in the engine's registry.
3. Standard R7RS libraries loaded from `.sld` files get environments created by the factory, so they inherit the same restrictions.

**The restriction is transitive and hermetic** — there's no way for Scheme code to escalate privileges within a single engine.

## Concrete work items

### Phase 1: Convenience API — **Implemented**

**Location**: `options.go`, `engine.go`, `internal/extensions/all/register.go`

Both forms were implemented:

- `SafeExtensions() []EngineOption` — composable slice for `append`
- `WithSafeExtensions() EngineOption` — convenience wrapper for the common case
- `WithoutCore() EngineOption` — bare engine with no core primitives

**Deviation from original plan**: `SafeExtensions()` uses `all.SafeExtension` instead of `all.Extension`. The `all.Extension` aggregates *all* sub-extensions (including files, eval, system, gointerop, threads), which would defeat sandboxing. `SafeExtension` was added to `internal/extensions/all/register.go` to expose only the safe local parts: records, promises, strings, and characters.

### Phase 2: Document security boundaries — **Implemented**

**Location**: `docs/SANDBOXING.md`

Covers: extension security classification, API usage (safe sandbox, composable, custom, bare), enforcement mechanism, what sandboxing does NOT cover (CPU, memory, goroutines, information flow, `include` gap), and pointers to related docs.

### Phase 3: Verify isolation invariants — **Implemented**

**Location**: `engine_sandbox_test.go`, `wile_test.go`

Tests implemented:

1. Safe engine rejects privileged primitives (`open-input-file`, `eval`, `exit`, `make-channel`, `load`, `delete-file`) with `CompilationError`.
2. Safe engine allows safe primitives (`+`, `display`, `sqrt`, `guard`, `make-record-type`, `force`).
3. `WithoutCore()` produces a bare engine where `+` and `car` are unbound.
4. `WithoutCore()` + `WithExtension(math.Extension)` gives `sqrt` but not `+`.
5. Library loaded by a safe engine fails when it tries to use `open-input-file`.
6. Option tests: `TestWithoutCore`, `TestWithSafeExtensions`, `TestSafeExtensions`.

### Phase 4: Registry filtering — **Implemented**

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

**Location:** `registry/registry.go` (methods on `*Registry`), `engine.go` (`Engine.Registry()` getter)

**What was implemented:**
- `Without(names ...string) *Registry` — filter by primitive name
- `WithoutCategory(categories ...string) *Registry` — filter by `PrimitiveSpec.Category`
- `WithoutBindings(names ...string) *Registry` — filter compile-time bindings
- `filterPrimitives` — shared helper to avoid duplication between `Without` and `WithoutCategory`
- `Engine.Registry() *Registry` — returns a clone of the engine's registry for filtering
- Unit tests in `registry/registry_test.go`; integration tests in `engine_sandbox_test.go`

### Phase 5: Library name in factory signature — **Implemented**

Pass the library name through `LibraryEnvFactory` so the factory knows which library it's creating an environment for. This is a low-cost signature change that enables per-library policies in the future and makes the loading path self-documenting.

**Current signature** (`environment/top_level_environment.go:30`):

```go
type LibraryEnvFactory func(context.Context, *EnvironmentFrame) (*EnvironmentFrame, error)
```

**Proposed signature:**

```go
type LibraryEnvFactory func(context.Context, *EnvironmentFrame, []string) (*EnvironmentFrame, error)
```

**Blast radius** — well-contained:

| What | Where | Change |
|------|-------|--------|
| Type definition | `environment/top_level_environment.go:30` | Add `[]string` param |
| Field + getter + setter | `environment/top_level_environment.go:81,237,242` | Follows from type |
| Call site 1 | `machine/library_loader.go:130` | Pass `expectedName.Parts` |
| Call site 2 | `machine/compile_time_continuation_library.go:120` | Pass `libName.Parts` |
| Engine factory closure | `engine.go:139` | Add `_ []string` param (unused for now) |
| Bootstrap factory | `internal/bootstrap/environment_tiny.go:149` | Add `_ []string` param |
| CLI setup | `cmd/scheme/main.go:229` | Points to bootstrap factory (no change needed) |
| Tests (factory reference) | `machine/library_test.go:309,745`, `machine/library_scheme_test.go:54` | Point to bootstrap factory (no change needed — signature matches) |
| Tests (direct calls) | `internal/bootstrap/library_environment_test.go`, `multi_environment_test.go` | Add `nil` third argument |
| Docs | `docs/EXTENSION_LIBRARIES.md`, `docs/dev/ENVIRONMENT_SYSTEM.md` | Text updates (deferred) |

### Phase 6: Library load observability — **Implemented**

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
| `CompileImport` | `compile_time_continuation_library.go:718` | Top-level `(import ...)` — importer is `nil` (script/REPL) |
| `expandImportForm` | `expander_time_continuation.go:748` | Top-level during expansion — importer is `nil` (script/REPL) |

All three sites follow the same sequence:
1. `LoadLibrary` -> get `*CompiledLibrary` (library name, exports, source file)
2. `ApplyToExports` -> get post-modifier bindings (what actually lands)
3. `CopyLibraryBindingsToEnvAtPhase` -> install bindings

The callback fires between steps 2 and 3, where all information converges.

**Approach:** Add an optional `LibraryImportObserver` callback to `LibraryRegistry` or `TopLevelEnvironment`. Fire it at all three import sites, between `ApplyToExports` and `CopyLibraryBindingsToEnvAtPhase`. The observer is read-only — it observes but doesn't influence the import.

```go
type LibraryImportObserver func(LibraryImportEvent)
```

Embedders that don't set it pay nothing (nil check before firing).

**What was implemented:**
- `LibraryImportEvent` struct and `LibraryImportObserver` callback type in `machine/library.go`
- `SetImportObserver` / `ImportObserver` on `*LibraryRegistry`
- `fireImportObserver` helper that extracts the registry from `env.LibraryRegistry()`, type-asserts, nil-checks, and fires
- Observer fires at Site 1 (`processLibraryImport`, library-internal) and Site 2 (`CompileImport`, top-level)
- NOT fired at Site 3 (`expandImportForm`) — the expander pre-loads libraries for macros, but the compiler is the definitive import site. Firing both would produce duplicate events for the same `(import ...)` form.
- `WithImportObserver` engine option in `options.go`; `LibraryImportEvent` type alias re-exported from `wile` package
- Integration tests in `engine_sandbox_test.go`: basic observer, `(only ...)` modifier
- Exports and Imported are sorted for deterministic observation

## Scope boundaries

**Covered by this plan:**
- Extension-level capability control
- Convenience API for sandbox profiles
- Documentation of security boundaries
- Integration tests for isolation
- Registry filtering (`Without`, `WithoutCategory`) for per-primitive control
- Library name in factory signature (forward-compatible for per-library policies)
- Library load observability (what was loaded, what bindings landed where)

**Not covered (separate concerns):**
- Resource limits: CPU time, memory, allocation rate — requires VM-level instrumentation
- Stack depth limits: `WithMaxCallDepth` already exists
- Network access: no network primitives exist yet; when added, they'd be a new privileged extension
- Per-library policies (enforcement): Phase 5 passes the library name through the factory, which *enables* per-library policies. The actual policy mechanism (allowlist, callback, etc.) is future work.
- Information flow: a privileged library can pass capabilities (e.g., open file handle) to an unprivileged library via exported values. Preventing this requires an object-capability model, which is a fundamental architecture change.

## Dependencies

- Phase 1 (convenience API): None — **Done**
- Phase 2 (documentation): None — **Done**
- Phase 3 (integration tests): Phase 1 — **Done**
- Phase 4 (registry filtering): None — **Done**
- Phase 5 (factory signature): None — **Done**
- Phase 6 (observability): Phase 5 — **Done**
- Authorization framework is independent and can proceed in parallel

## Decision log

| Decision | Rationale |
|----------|-----------|
| Extension-level as primary mechanism | Embedder is the security principal. Extensions are already opt-in. Zero-cost when extension not loaded. |
| `export` is not a security mechanism | `export` controls outward visibility (namespace management), not inward capability. A library with full imports can do anything regardless of its export list. |
| Safe = no ambient authority | Extensions classified as safe have no way to affect the outside world. io extension uses caller-provided ports or in-memory ports — no filesystem access. |
| `[]string` over `LibraryName` in factory | Avoids `environment/` -> `machine/` dependency. `LibraryName.Parts` is `[]string` internally; convert at call site. |
| Observer as optional callback | Embedders that don't need observability pay nothing. No allocation, no interface dispatch on the hot path. Observer is read-only — observation now, enforcement later if needed (change return type to `error`). |
| Subtraction over composition for filtering | `Without()` starts from the full registry and removes. Alternative (export individual `add*` functions for embedders to compose) requires embedders to know the internal structure of core. Subtraction is simpler: start with everything, remove what you don't want. |
| Compile-time binding removal separate from primitive removal | `Without()` removes runtime primitives. Compile-time bindings (`set!` as special form) stay — the compiler recognizes the form but the runtime rejects it. This produces a clear compile error ("mutation disallowed") rather than a confusing unbound-variable error. `WithoutBindings()` available for embedders who want full erasure. |

---

# Authorization Framework

**Status**: Phases 1-3 implemented

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
Engine.Eval(ctx) -> security.WithAuthorizer(ctx, authorizer) -> flows to all sites
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
| `extensions/files/prim_files.go` | open-input/output-file, file-exists?, delete-file, call-with-*-file | `file` | read/write/stat/delete |
| `extensions/system/prim_system.go` | get-environment-variable(s), exit, emergency-exit | `env`/`process` | read/exit |
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

- **Not covered**: Load path resolution (planned, no design doc yet), network primitives (not yet), resource limits (CPU/memory/stack — separate concern)
- **Composes with load-path stack**: path -> [resolve] -> [authorization check] -> os.Open

---

# Opcode Resource Limits Design

**Status:** Design (not yet implemented)
**Date:** 2026-02-15

## Problem

The VM loop checks `ctx.Done()` every 1024 operations (`contextCheckMask`), but several opcodes can block *inside* a single `Apply()` call for unbounded time before returning to the loop. A malicious or runaway Scheme program can exploit this to consume unbounded CPU within a single opcode execution.

## Threat Model

An untrusted Scheme program (or malicious input to a trusted program) triggers unbounded computation inside a single VM opcode, bypassing the context cancellation check in the main VM loop.

The attacker controls Scheme source code (or input data that drives macro expansion). The embedder controls engine configuration. The goal is to give embedders per-category knobs that cap the work any single opcode can do.

## Existing Limits

| Limit | Location | Default | Error Sentinel |
|-------|----------|---------|----------------|
| Call depth | `machine_context.go:69` | 0 (unlimited) | `ErrCallDepthExceeded` |
| read-string allocation | `prim_read_write.go:34` | 100 MB | `ErrAllocationLimitExceeded` |
| read-bytevector allocation | `prim_read_write.go:38` | 100 MB | `ErrAllocationLimitExceeded` |
| VM loop context check | `machine_context.go:38` | Every 1024 ops | (context error) |
| Match VM context check | `match.go:237` | Every 1024 iterations | (context error) |

## Complete Non-O(1) Operation Catalog

### Engine VM (14 non-O(1) operations)

#### Unbounded — Require Limits

| Operation | File | Complexity | Dominant Cost | Limit Category |
|-----------|------|-----------|---------------|----------------|
| `SyntaxCaseMatch` | `operation_syntax_case.go:65` | O(input) | Runs match VM internally | Match Steps |
| `SyntaxRulesTransform` | `operation_syntax_rules_transform.go:105` | O(clauses x (input + template x reps)) | Match per clause + Expand on first match | Match Steps + Expand Steps |
| `SyntaxTemplateExpand` | `operation_syntax_case.go:206` | O(template x reps) | Recursive `expandSyntaxValue` calls | Expand Steps |
| `Apply` (composable cont.) | `machine_context.go:486` (`applyComposableContinuation`) | O(d) + O(d) + O(w) | `DeepCopy` O(d) + `GraftContinuation` O(d) + winding O(w) | Continuation Copy Depth |
| `ForeignFunctionCall` | `operation_foreign_function_call.go:61` | O(?) unbounded | Arbitrary Go code | Embedder Responsibility (ctx) |

#### Bounded by Compile-Time Constants — No Limits Needed

| Operation | File | Complexity | Why Safe |
|-----------|------|-----------|----------|
| `BuildSyntaxList` | (operation_build_syntax.go) | O(n), n = stack pops | n is argument count at call site, set at compile time |
| `MakeCaseLambdaClosure` | (operation_case_lambda.go) | O(c), c = clauses | c is compile-time constant, typically 2-4 |
| `BindPatternVars` | `operation_syntax_case.go:131` | O(v), v = pattern vars | v is compile-time constant, typically < 20 |
| `StoreSyntaxCaseInput` | `operation_syntax_case.go:248` | O(input) for DatumToSyntax | One-time conversion; bounded by input already parsed |

#### Bounded by Other Limits — No Additional Limits Needed

| Operation | File | Complexity | Why Safe |
|-----------|------|-----------|----------|
| `RestoreContinuation` | (operation_restore.go) | O(stack) for evals copy | Stack depth bounded by `maxCallDepth` (when set) |
| `Push` (multiValues path) | (operation_push.go) | O(v), v = value count | Rare path (`call-with-values` only); v is result count |
| `Apply` (normal path) | (operation_apply.go) | O(args + params) | args = call-site arity; params typically 1-5 |
| `SaveContinuationOffsetImmediate` | (operation_save_continuation.go) | O(1) | Just captures pointers |
| `SyntaxCaseNoMatch` | `operation_syntax_case.go:181` | O(1) | Always errors immediately |

### Match VM (2 non-O(1) instructions)

| Instruction | File | Complexity | Status |
|-------------|------|-----------|--------|
| `ByteCodeDone` | `match.go` | O(1) in practice | Safe — checks immediate cdr + one lookahead |
| `ByteCodeSkipIfTailCount` | `match.go` | O(1) with cache | **Fixed in 766bce1** — tail count cached, decremented per iteration |

### Syntax Expansion Helpers (tree-walking)

| Function | File | Complexity | Called From |
|----------|------|-----------|------------|
| `addScopeToSyntax` | `operation_syntax_rules_transform.go:349` | O(tree) | `SyntaxPair.AddScope` |
| `addScopeToSyntaxSkipFreeIds` | `operation_syntax_rules_transform.go:249` | O(tree) | Template expansion |
| `mapSyntaxTree` | `internal/syntax/syntax_pair.go` | O(tree) | `SyntaxPair.AddScope` |
| `DatumToSyntaxValue` | `internal/schemeutil/` | O(tree) | `StoreSyntaxCaseInput` (conditional) |
| `expandSyntaxValue` | `internal/match/syntax_adapter.go:255` | O(template x reps) | `SyntaxMatcher.Expand` |

These are covered by the **Expand Steps** category — they are called as part of template expansion.

## Limit Categories

### Category 1: Match Steps

**What it caps:** Iterations of the match VM's bytecode dispatch loop.

**Enforcement point:** `Matcher.MatchSyntaxWithLiterals()` in `internal/match/match.go`, line 234+. The loop already has an `iterations` counter (line 234) and a batched `ctx.Done()` check every 1024 iterations (line 237). The limit check piggybacks on this existing counter, inside the same batch check:

```
if maxMatchSteps > 0 && iterations > maxMatchSteps {
    return ErrMatchStepsExceeded
}
```

**Plumbing (Engine -> Matcher):**

```
engineConfig.maxMatchSteps
  -> Engine.maxMatchSteps
    -> MachineContext.maxMatchSteps (new field, inherited by sub-contexts)
      -> passed to SyntaxMatcher / Matcher at match call sites
```

**API:**
```go
wile.WithMaxMatchSteps(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrMatchStepsExceeded = NewStaticError("match step limit exceeded")`

### Category 2: Expand Steps

**What it caps:** Total recursive calls to `expandSyntaxValue()` (and the tree-walking helpers it calls) during a single template expansion.

**Enforcement point:** At the top of `SyntaxMatcher.expandSyntaxValue()` in `internal/match/syntax_adapter.go`, line 255+:

```
p.expandSteps++
if p.maxExpandSteps > 0 && p.expandSteps > p.maxExpandSteps {
    return nil, ErrExpandStepsExceeded
}
```

The counter resets at the start of each `Expand()` call (line 238).

**API:**
```go
wile.WithMaxExpandSteps(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrExpandStepsExceeded = NewStaticError("expand step limit exceeded")`

### Category 3: Continuation Copy Depth

**What it caps:** The number of continuation frames walked during `DeepCopy()` and `GraftContinuation()` when invoking a composable continuation.

**Enforcement point:** Inside `MachineContinuation.DeepCopy()` in `machine/machine_continuation.go`, line 164:

```
depth := 0
for current.parent != nil {
    depth++
    if maxContinuationCopyDepth > 0 && depth > maxContinuationCopyDepth {
        return nil, ErrContinuationCopyDepthExceeded
    }
    parentCopy := current.parent.Copy()
    current.parent = parentCopy
    current = parentCopy
}
```

**This is distinct from `maxCallDepth`:**
- `maxCallDepth` limits how deep the *live* call stack grows during execution.
- `maxContinuationCopyDepth` limits how much work a single *continuation invocation* does when copying a captured continuation segment.

**API:**
```go
wile.WithMaxContinuationCopyDepth(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrContinuationCopyDepthExceeded = NewStaticError("continuation copy depth exceeded")`

### ForeignFunctionCall: Embedder Responsibility

`ForeignFunctionCall` calls arbitrary Go code via the `ForeignFunction` signature. The context is accessible via `mc.Context()`. The VM cannot impose a step limit on opaque Go code. This is documented as the embedder's responsibility:

- Use `context.WithTimeout` or `context.WithDeadline` to bound total execution time.
- Foreign functions that perform unbounded work must check `mc.Context().Done()` internally.
- The engine's built-in primitives already follow this contract.

## New Error Sentinels

Added to `values/foreign_error.go`, grouped with existing resource-exhaustion errors:

```go
ErrMatchStepsExceeded              = NewStaticError("match step limit exceeded")
ErrExpandStepsExceeded             = NewStaticError("expand step limit exceeded")
ErrContinuationCopyDepthExceeded   = NewStaticError("continuation copy depth exceeded")
```

## API Summary

```go
// Existing
wile.WithMaxCallDepth(n uint64) EngineOption

// New
wile.WithMaxMatchSteps(n uint64) EngineOption
wile.WithMaxExpandSteps(n uint64) EngineOption
wile.WithMaxContinuationCopyDepth(n uint64) EngineOption
```

All follow the same convention:
- 0 = unlimited (default)
- \> 0 = hard cap
- Exceeded -> wrapped sentinel error, propagated as Scheme exception

## Plumbing Summary

All three new limits follow the same path as `maxCallDepth`:

```
EngineOption (options.go)
  -> engineConfig field (options.go)
    -> Engine field (engine.go)
      -> MachineContext field (machine_context.go)
        -> inherited by sub-contexts (NewSubContext, NewSubContextForThread)
          -> passed to enforcement point (match/expand/deep-copy)
```

## Future Work

- **Default non-zero limits:** Once the limits are implemented and tested, consider changing defaults from 0 (unlimited) to sensible non-zero values. This is a separate decision that affects backward compatibility.
- **Scope-walking step counting:** If `addScopeToSyntax` tree walks prove to be a security concern independent of expand steps, they can be added to the expand step counter.
- **Macro expansion depth:** The expander (`ExpandExpression` in `expander_time_continuation.go:102`) recursively expands macro results without tracking depth. This is a *separate* concern from template expansion steps — it's about how many times a macro output is re-expanded, not how large a single expansion is. A `maxExpansionDepth` limit (tracking recursive re-expansion) may be needed as a fourth category.
