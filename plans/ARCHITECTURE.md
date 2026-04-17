# Architecture Plans

**Status:** 1/4 sections complete

> **Completed:** Environment Introspection Phase 1 (PrimEnvironmentQ, PrimEnvironmentBoundNames, PrimEnvironmentRef, PrimEnvironmentBoundQ in extensions/introspection/).
> **Incomplete:** Dialect System (all 4 phases — no FormRegistry, no WithDialect), Module Decomposition (all 5 phases), Plugin Shadowing (all 4 phases — no primitiveOverrides).

# Dialect System

**Status**: Proposed
**Date**: 2026-02-20
**Related**: Sandboxing Model (see SECURITY.md), `docs/EXTENSIONS.md`

## Motivation

Wile's engine is language-agnostic below the surface. The bytecode VM, continuation model, scope system, and eval stack don't encode R7RS — they encode "a Lisp with hygienic macros." The R7RS personality lives in four configurable layers on top. Three of these layers are already per-engine configurable. One isn't.

Making all four layers configurable turns Wile from "an R7RS implementation" into "a Lisp platform that ships with R7RS as the default dialect." An embedder could use R6RS, a custom DSL, or a Clojure-flavored Lisp — same VM, different personality.

This aligns with "embedding is the product": embedders choose not just which capabilities their engine has, but which *language* it speaks.

## Architecture: Four Layers of Language Personality

```
+------------------------------------------------------+
| Layer 1: Runtime Primitives           [per-registry]  |
| What: names, arities, implementations                 |
| Examples: car/cdr, +/-, string-ref, eval              |
| Configurable today: YES (WithRegistry, WithExtension) |
+------------------------------------------------------+
+------------------------------------------------------+
| Layer 2: Bootstrap Macros             [per-registry]  |
| What: derived forms as Scheme source text              |
| Examples: and, or, let, cond, guard, define-values    |
| Configurable today: YES (AddMacroSource)              |
+------------------------------------------------------+
+------------------------------------------------------+
| Layer 3: Syntax Compilers + Expanders [per-environment]|
| What: how special forms are expanded and compiled      |
| Examples: define-syntax, import, syntax-case           |
| Configurable today: YES (RegisterSyntaxCompilers is   |
|   a data table bound per-environment)                 |
+------------------------------------------------------+
+------------------------------------------------------+
| Layer 4: Forms Registry                    [GLOBAL]   |
| What: core form validators + compilers                |
| Examples: if, lambda, define, set!, quote, begin      |
| Configurable today: NO -- package-level global in     |
|   internal/forms/form_spec.go:58                      |
| THIS IS THE ONLY STRUCTURAL BLOCKER                   |
+------------------------------------------------------+
```

## The Blocker: Global Forms Registry

`internal/forms/form_spec.go:58`:

```go
var registry = make(map[string]*FormSpec)
```

Populated by `init()` in `machine/register.go`. Maps form names (`"if"`, `"lambda"`, `"define"`, `"set!"`, `"quote"`, `"begin"`, `"quasiquote"`, `"dynamic-wind"`) to validator + compiler function pairs. Every engine in the same process shares this map.

**Why it's global:** The forms registry is used by both the validator (`internal/validate/`) and the compiler (`machine/compile_validated.go:53`). The validator needs to know "is this name a special form?" to produce the right `ValidatedExpr` type. The compiler needs to know "which compiler handles this form?" to emit bytecode. Both currently call `forms.Lookup()` which hits the global.

**Two-tier dispatch** (`machine/syntax_compilers_registry.go:29-34`):

- **Tier 1 (Validated Forms)**: `if`, `lambda`, `define`, `set!`, `quote`, `begin`, `quasiquote`, `dynamic-wind`, `case-lambda` — go through the validation layer to produce typed `ValidatedExpr` nodes, then compile via `compileValidated*` methods. Registered in the global forms registry.
- **Tier 2 (Registry Forms)**: `define-syntax`, `import`, `syntax-case`, `include`, `cond-expand`, etc. — pass through validation as `ValidatedLiteral`, dispatched via the per-environment syntax compiler table.

Tier 2 is already per-environment. Only Tier 1 is global.

## What a Dialect Bundles

```go
// Dialect defines a complete language personality for a Wile engine.
type Dialect struct {
    // Layer 1: runtime primitives and their names
    Registry func(*registry.Registry) error

    // Layer 2: derived forms as Scheme source
    MacroSources []string

    // Layer 3: syntax compilers (Tier 2 forms)
    SyntaxCompilers func(*environment.EnvironmentFrame) error

    // Layer 3: primitive expanders
    PrimitiveExpanders func(*environment.EnvironmentFrame) error

    // Layer 4: core form validators + compilers (Tier 1 forms)
    FormSpecs map[string]*forms.FormSpec

    // Compile-time binding names (special forms + auxiliary syntax)
    CompileTimeBindings []string

    // Features for cond-expand
    Features []string
}
```

Usage:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithDialect(r7rs.Dialect),
    wile.WithExtension(files.Extension),
    wile.WithLibraryPaths("./stdlib/lib"),
)
```

`r7rs.Dialect` is the default — identical to today's behavior. If no dialect is specified, `NewEngine` uses R7RS.

## Dialect Variations: What Changes Where

### R6RS

| What changes | Layer | Detail |
|-------------|-------|--------|
| Library syntax | 3 | R6RS `(library ...)` has fixed-order declarations. R7RS `define-library` allows any-order. |
| Condition system | 1+2 | `&condition`, `&assertion`, etc. — new primitives + macros replacing R7RS `error` objects |
| Error strictness | 1 | R6RS "must raise" vs R7RS "is an error" (undefined). |
| Records | 1+2 | R6RS `define-record-type` with inheritance, sealed, opaque — richer than R7RS SRFI-9 style |
| `syntax-case` emphasis | already supported | Both `syntax-rules` and `syntax-case` work today |
| Hashtables | 1 | R6RS `(rnrs hashtables)` has different API from Wile's current hashtables |
| Tail context strictness | 4 | R6RS requires tail position in more contexts |
| `import` semantics | 3 | R6RS `import` only inside `library`, not at top-level |

### Custom Lisp / DSL

| What changes | Layer | Detail |
|-------------|-------|--------|
| Rename special forms | 4 | `fn` instead of `lambda`, `def` instead of `define` |
| Rename primitives | 1 | `first`/`rest` instead of `car`/`cdr` |
| Different derived forms | 2 | Clojure-style `defn`, `let` with vector bindings |
| Remove mutation | 1+4 | No `set!`, `set-car!`, `set-cdr!` — remove from forms registry + primitives |
| Add new types | values pkg | Persistent vectors, atoms, keywords — deeper change, outside dialect system |

## Concrete Work Items

### Phase 1: De-globalize the forms registry

Replace the global `map[string]*FormSpec` in `internal/forms/form_spec.go` with a `FormRegistry` type that can be instantiated per-engine.

**Proposed:**

```go
type FormRegistry struct {
    specs map[string]*FormSpec
}

func NewFormRegistry() *FormRegistry { ... }
func (r *FormRegistry) Register(spec *FormSpec) { ... }
func (r *FormRegistry) Lookup(name string) *FormSpec { ... }
func (r *FormRegistry) Clone() *FormRegistry { ... }
```

A default `FormRegistry` is built once (equivalent to today's `init()`) and cloned per-engine.

**Blast radius:**

| What | Where | Change |
|------|-------|--------|
| Global registry | `internal/forms/form_spec.go` | Replace with `FormRegistry` type |
| Init registration | `machine/register.go` `init()` | Build a default `FormRegistry` instead of mutating global |
| Compiler dispatch | `machine/compile_validated.go:38` | `forms.Lookup(...)` -> `p.formRegistry.Lookup(...)` |
| CompileTimeContinuation | `machine/compile_time_continuation.go` | Add `formRegistry *forms.FormRegistry` field |
| Validator | `internal/validate/` | Validators call `forms.Lookup()` to check special-form status; need registry passed in |
| Expander | `machine/expander_time_continuation.go` | May reference forms registry for form detection |
| Engine construction | `engine.go` | Build/clone forms registry, pass to compiler |

The validator is the subtlest part. `internal/validate/` uses `forms.Lookup()` to decide whether an identifier names a special form (which determines which `ValidatedExpr` type to produce). The registry needs to flow into the validation path, likely through the validation context.

### Phase 2: Dialect type and WithDialect option

Define the `Dialect` type and `WithDialect()` engine option.

`NewEngine` uses `Dialect.FormSpecs` instead of the global forms registry. If no dialect is specified, it uses `r7rs.DefaultDialect`.

### Phase 3: Extract R7RS as the default dialect

Move all current R7RS-specific configuration into `r7rs.DefaultDialect`:

- `core.AddToRegistry` -> `Dialect.BuildRegistry`
- `core.compileTimeBindings` -> `Dialect.CompileTimeBindings`
- `core.bootstrapMacroSource` -> `Dialect.MacroSources`
- `machine.RegisterSyntaxCompilers` table -> `Dialect.SyntaxCompilers`
- `machine.RegisterPrimitiveExpanders` table -> `Dialect.PrimitiveExpanders`
- `machine/register.go` `init()` registrations -> `Dialect.FormSpecs`
- `machine.Features()` -> `Dialect.Features`

After this phase, Wile behaves identically but R7RS is a configuration, not hardcoded.

### Phase 4: Second dialect (validates the abstraction)

Implement one non-R7RS dialect to prove the abstraction works. Candidates:

**Option A: R7RS-minimal** — R7RS without mutation. Removes `set!`, `set-car!`, `set-cdr!`, `vector-set!`, `string-set!`, `list-set!`, `hashtable-set!`, `set-box!` from primitives and removes `set!` from the forms registry. Connects to sandboxing model Phase 4 (registry filtering).

**Option B: R6RS-core** — R6RS library syntax + condition system. More ambitious, proves the library system is dialect-configurable.

**Option C: Custom DSL** — Minimal Lisp with renamed forms (`fn`, `def`, `let`). Proves form-name independence.

Recommendation: **Option A first** (validates the forms registry de-globalization with minimal risk), then **Option B** (validates the full dialect system with real semantic differences).

## Scope Boundaries

**Covered:** De-globalizing the forms registry, `Dialect` type and `WithDialect()` option, extracting R7RS as the default dialect, validating with a second dialect.

**Not covered:** New value types, parser changes for non-s-expression syntax, REPL personality, R6RS full compliance.

## Dependencies

- Phase 1 (forms registry): None — internal refactor
- Phase 2 (Dialect type): Phase 1
- Phase 3 (extract R7RS): Phase 2
- Phase 4 (second dialect): Phase 3
- Sandboxing model Phase 4 (`Registry.Without`) composes with dialect system for mutation-free dialects

## Decision Log

| Decision | Rationale |
|----------|-----------|
| De-globalize forms registry, not per-environment | Per-environment would require the forms registry in `EnvironmentFrame`, adding weight to every environment. Per-engine (via `CompileTimeContinuation`) is sufficient — all code in one engine speaks one dialect. |
| Dialect as struct, not interface | Dialects are data (tables of specs), not behavior. A struct with function fields is simpler than an interface with methods. |
| R7RS as default, not special | After extraction, R7RS is just `r7rs.DefaultDialect`. No special-casing in the engine. |
| Validator needs registry access | Validators produce different `ValidatedExpr` types based on whether a name is a special form. The forms registry must flow into validation, not just compilation. |
| Phase 4 starts with mutation-free variant | Smallest delta from R7RS. Validates the abstraction without requiring new primitives, types, or library system changes. |
| Parser not included | S-expression syntax is shared across all Lisp dialects. Dialects that need different surface syntax are a parser concern, not a dialect concern. |

---

# Module Decomposition

**Status**: Proposed
**Date**: 2026-02-20
**Related**: Sandboxing Model (see SECURITY.md), Dialect System (above)

## Motivation

The sandboxing plan establishes that extensions are opt-in capabilities. The dialect plan establishes that the language personality is configurable. The logical conclusion: the core repo should contain only what's irreducible, and everything else should be a separate Go module that embedders compose via `go get` and `WithExtension()`.

This gives embedders precise control over binary size, capability surface, and dependency footprint. A minimal embedded Scheme that does pure computation imports only `github.com/aalpar/wile`. One that needs I/O adds `github.com/aalpar/wile-io`. No unused code compiles in.

## The Core Boundary

The boundary is determined by answering: **what can't be removed without making the engine unable to function?**

### What stays in `github.com/aalpar/wile`

| Package | Contents | Why irreducible |
|---------|----------|----------------|
| `values/` | Type system: Value interface, Pair, Symbol, Number, etc. | Every package depends on this |
| `environment/` | Bindings, scopes, phases, Namespace | Compiler and VM require it |
| `machine/` | VM, compiler, expander, continuations, bytecode, operations | The execution engine |
| `registry/` | Registry type, Extension interface, Phase, PrimitiveSpec | The composition mechanism |
| `registry/core/` | ~148 primitives in 15 categories + core bootstrap macros | Language doesn't function without `cons`, `+`, `if`, `lambda` |
| `internal/*` | Tokenizer, parser, syntax objects, validator, forms registry, pattern matcher | Compilation pipeline |
| Root package | Engine, EngineOption, Dialect, public API | Embedder entry point |

### Core primitives (15 categories, ~148 primitives)

These are in `registry/core/` and are truly irreducible: special forms, predicates, equality, pairs, lists, arithmetic, control, vectors, strings, characters, bytevectors, syntax, parameters, boxes, hashtables, continuations.

### Core bootstrap macros

`and`, `or`, `let`, `let*`, `letrec`, `letrec*`, `cond`, `case`, `when`, `unless`, `parameterize`, `let-values`, `let*-values`, `define-values`, `do`, `map`, `for-each`, `with-continuation-barrier`, `with-baffle`

### Bootstrap macros that move to extensions

| Macro | Depends on | Moves to |
|-------|-----------|----------|
| `guard`, `guard-aux` | `with-exception-handler`, `raise` | `wile-exceptions` |
| `delay`, `delay-force` | `%make-lazy-promise` | `wile-promises` |
| `define-record-type`, `define-record-type-impl` | `make-record-type`, etc. | `wile-records` |

## Extension Modules

Each extension is a separate Go module with a single dependency on `github.com/aalpar/wile`.

### Extension inventory

| Module | Current location | Security class |
|--------|-----------------|---------------|
| `wile-io` | `internal/extensions/io/` | safe |
| `wile-records` | split from `internal/extensions/all/` | safe |
| `wile-promises` | split from `internal/extensions/all/` | safe |
| `wile-math` | `extensions/math/` | safe |
| `wile-introspection` | `extensions/introspection/` | safe |
| `wile-files` | `extensions/files/` | privileged |
| `wile-process` | `extensions/process/` | privileged |
| `wile-eval` | `internal/extensions/eval/` | privileged |
| `wile-system` | `extensions/system/` | privileged |
| `wile-threads` | `extensions/threads/` | context-dependent |
| `wile-gointerop` | `extensions/gointerop/` | privileged |

No extension depends on another extension. No circular dependencies.

### Import graph

```
github.com/aalpar/wile                 (core -- zero external deps)

github.com/aalpar/wile-io              -> wile
github.com/aalpar/wile-exceptions      -> wile
github.com/aalpar/wile-records         -> wile
github.com/aalpar/wile-promises        -> wile
github.com/aalpar/wile-math            -> wile
github.com/aalpar/wile-files           -> wile
github.com/aalpar/wile-eval            -> wile
github.com/aalpar/wile-system          -> wile
github.com/aalpar/wile-threads         -> wile
github.com/aalpar/wile-gointerop       -> wile

github.com/aalpar/wile-scheme          -> all of the above (CLI/REPL)
```

## Why ~148 Core Primitives Is the Floor

Three constraints define the floor:

1. **Bootstrap dependency.** Bootstrap macros reference specific primitives: `memv`, `cons`, `car`, `cdr`, `apply`, `call-with-values`, `dynamic-wind`, etc. These must be Go-level because bootstrap macros run *before* any Scheme-defined functions exist.

2. **Scheme-level load ordering.** Go primitives are all available simultaneously after `Registry.Apply()`. Scheme-defined functions are evaluated sequentially. A second dependency graph layered on top of `go.mod` is the complexity you'd be buying.

3. **No embedder benefit.** Nobody wants Scheme-without-subtraction. The sandboxing use case is about *capabilities* (filesystem, eval, system), not *arithmetic*.

## Initialization Ordering

### Two-tier solution

**Tier 1 (Bootstrap): `AddMacroSource` — core only, no cross-extension dependencies.**

Bootstrap macros loaded during step 6 of engine init. Depend ONLY on Go primitives and earlier bootstrap macros. Required for the library system to compile `(define-library ...)` and `(import ...)`.

**Tier 2 (Library): Extension Scheme code loaded via the library system.**

Extension-specific macros and functions become library exports, loaded on demand via `(import ...)`. The library system handles dependency resolution automatically.

## The Embedder Experience

**Minimal (pure computation):**

```go
import "github.com/aalpar/wile"

eng, _ := wile.NewEngine(ctx)
result, _ := eng.Eval(ctx, "(+ 1 2 3)")
```

**Safe sandbox with I/O and exceptions:**

```go
import (
    "github.com/aalpar/wile"
    wileio "github.com/aalpar/wile-io"
    wileexc "github.com/aalpar/wile-exceptions"
)

eng, _ := wile.NewEngine(ctx,
    wile.WithExtension(wileio.Extension),
    wile.WithExtension(wileexc.Extension),
)
```

**Full R7RS:**

```go
import (
    "github.com/aalpar/wile"
    "github.com/aalpar/wile-r7rs"
)

eng, _ := wile.NewEngine(ctx,
    wile.WithDialect(r7rs.Dialect),
    wile.WithLibraryPaths("./stdlib/lib"),
)
```

## Phases

### Phase 1: Decompose `internal/extensions/all/`

Split: Records -> new `internal/extensions/records/`, Promises -> new `internal/extensions/promises/`, String mutation ops -> `registry/core/strings.go`. Move bootstrap macros to respective extensions.

**Prerequisite:** None. Internal refactor.

### Phase 2: Make `internal/extensions/` public

Move internal extensions to public packages. After this, all extensions are importable by external code.

**Prerequisite:** Phase 1.

### Phase 3: Extract extension modules

One at a time, extract to separate repos. Order (least dependencies first):
1. `wile-math` 2. `wile-system` 3. `wile-gointerop` 4. `wile-threads` 5. `wile-files` 6. `wile-io` 7. `wile-exceptions` 8. `wile-records` 9. `wile-promises` 10. `wile-eval`

**Prerequisite:** Phase 2.

### Phase 4: Create convenience bundles

- `wile-r7rs` — full R7RS dialect bundle
- `wile-std` — safe extensions bundle

**Prerequisite:** Phase 3, Dialect System Phase 3.

### Phase 5: Go workspace setup

Create `wile-workspace` with `go.work` linking all modules.

## Verified Dependency Analysis (2026-02-25)

- `engine.go` does NOT import any extensions. Extensions are purely opt-in via `WithExtension()`.
- The only all-extensions import is `internal/bootstrap`. Only `cmd/wile/main.go` imports it.
- All 6 public extensions can be extracted with zero production code changes.
- Internal extensions (io, eval) have internal package dependencies that need resolution before extraction.
- `wile-extension-example/` already validates the separate-module pattern.

### Key blockers for Phase 3

1. Phase 2 prerequisite (make io/eval public)
2. files->io dependency needs resolution strategy
3. Test migration (files tests import internal io directly)

## Decision Log

| Decision | Rationale |
|----------|-----------|
| Core includes all ~148 primitives | Bootstrap macros depend on them. Shrinking core further buys no real embedder benefit. |
| Bootstrap macros split by dependency | 16 macros depend only on core -> stay in Tier 1. 5 macros depend on extension primitives -> Tier 2 (library system). |
| Two-tier Scheme initialization | Avoids parallel dependency graph. |
| Library system is the primary mechanism for extension Scheme code | Extension authors write standard R7RS library definitions. |
| Separate repos over monorepo subdirectories | Independent versioning, CI, and release cycles. |
| No extension depends on another extension | Keeps dependency graph flat. |

---

# Plugin Shadowing Design

**Status:** Proposed — Not implemented

**Dependencies:** Public extension system (see Module Decomposition Phase 2)

## Problem

Extensions need to shadow core primitives (e.g., R6RS compat needs to shadow `error`). Current architecture has globally registered primitives with no override mechanism.

## Three-Tier Lookup Model

```
+---------------------------------+
| User Environment                |  <- Highest priority
| (user bindings, plugin overrides)|
+---------------------------------+
| Extension Layer                 |  <- Middle priority
| (extension-provided bindings)   |
+---------------------------------+
| Core Registry                   |  <- Lowest priority (fallback)
| (built-in primitives)           |
+---------------------------------+
```

## Design

Add optional `primitiveOverrides map[string]values.Value` to `Environment`. Lookup checks: local bindings -> primitive overrides (nil check fast path) -> parent -> registry fallback.

Extensions declare overrides via `PrimitiveOverrides() map[string]PrimitiveSpec` method on Extension interface.

Scheme-level shadowing already works today via `(define error (let ((orig error)) (lambda args ...)))`.

## Use Cases

1. **R6RS compatibility** — shadow `error` to accept both R6RS and R7RS signatures
2. **Debug/trace mode** — shadow primitives to log calls
3. **Security/sandboxing** — intercept dangerous operations
4. **Performance instrumentation** — measure primitive call counts

## Implementation Phases

| Phase | Description |
|-------|-------------|
| 1 | Add `primitiveOverrides` to Environment, modify `Get()` |
| 2 | Add `PrimitiveOverrides()` to Extension interface |
| 3 | Scheme API: `define-primitive-override` (optional) |
| 4 | Performance profiling, error messages, debugging support |

## Design Decisions

- **Environment-scoped** (not global) — isolates extensions
- **Explicit opt-in** — no accidental shadowing
- **Immutable core** — core registry primitives can't be modified, only shadowed
- **Child inheritance** — overrides propagate via parent chain
- **No removal** — create new environment for clean state
- **Fast path**: nil pointer check (<1% overhead when no overrides)

## Alternatives Rejected

- Global override registry — no scoping flexibility
- Middleware chain — complex, high overhead
- Macro-based rewriting — breaks compiled code caching

---

# Read-Only Environment Introspection Primitives

**Status:** Planned

## Context

Wile exposes environments as first-class values (`Namespace` implements `values.Value`),
but provides no Scheme-level API to inspect their contents. The Go side has `Keys()`, `Bindings()`,
`GetBinding()` — none of it reachable from Scheme. This adds 4 read-only introspection primitives
following MIT Scheme naming conventions, informed by prior art from MIT Scheme, Chez Scheme, and Guile.

Part of the broader **reflection features** initiative.

## Prior Art

### MIT Scheme (most complete)
- `environment?`, `environment-has-parent?`, `environment-parent`
- `environment-bound-names`, `environment-macro-names`, `environment-bindings`
- `environment-lookup`, `environment-assigned?`, `environment-lookup-macro`
- `environment-define`, `environment-assign!`, `environment-definable?`

### Chez Scheme (pragmatic)
- `environment?`, `environment-symbols`, `copy-environment`
- `top-level-value`, `top-level-bound?`
- `define-top-level-value`, `set-top-level-value!`

### Guile (module-centric)
- Environments ~ modules; `resolve-module`, `resolve-interface`
- `module-obarray`, `variable-ref`, `variable-set!`

### Design choices
- MIT-style naming: `environment-*` (most established convention)
- Read-only first cut (no mutation)
- All binding types in a single list (no separate macro-names)

## Primitives (Phase 1 -- Read-Only)

| Primitive | Params | Returns |
|-----------|--------|---------|
| `environment?` | 1 (obj) | `#t` if obj is an environment |
| `environment-bound-names` | 1 (env) | List of all bound symbols (variables, syntax, primitives) |
| `environment-ref` | 2 (env, symbol) | Value bound to symbol; error if unbound |
| `environment-bound?` | 2 (env, symbol) | `#t` if symbol is bound |

All registered at `PhaseRuntime` (same as existing eval primitives).

## Files to Modify

### 1. `internal/extensions/eval/prim_eval.go` -- Add 4 implementations

Append after `PrimSyntaxLocalIdentifierAsBinding` (end of file):

- **`PrimEnvironmentQ`**: Use `helpers.MakeTypePredicate` checking `*environment.Namespace`.
- **`PrimEnvironmentBoundNames`**: Type-assert arg -> `*Namespace`, call `topLevelEnv.Runtime().GlobalEnvironment().Keys()`, cons each `*values.Symbol` onto accumulator.
- **`PrimEnvironmentRef`**: Type-assert env arg, require `*values.Symbol` for second arg, call `env.GetBinding(sym)`. Return `binding.Value()`. Error with `ErrNoSuchBinding` if nil.
- **`PrimEnvironmentBoundQ`**: Same as ref but return `BoolToBoolean(binding != nil)`.

### 2. `internal/extensions/eval/register.go` -- Add 4 specs

### 3. `registry/core/prim_env_extra_test.go` -- Add tests

## Design Decisions

- **`environment-bound-names` includes all binding types** in one list.
- **`environment-ref` traverses the parent chain** via `GetBinding()`.
- **Symbol comparison by string key** — `helpers.EqIdentity` compares symbols by `.Key` field; no interning needed.
- **No mutation primitives** in this cut.

## Go Infrastructure Already Available

```
Scheme primitive          ->  Go path
environment?              ->  type assertion on *Namespace
environment-bound-names   ->  topLevel.Runtime().GlobalEnvironment().Keys()
environment-ref           ->  topLevel.Runtime().GetBinding(sym).Value()
environment-bound?        ->  topLevel.Runtime().GetBinding(sym) != nil
```

## Future Extensions

Phase 2 (mutation): `environment-define!`, `environment-set!`
Phase 3 (navigation/metadata): `environment-has-parent?`, `environment-parent`, `environment-macro-names`, `environment-assigned?`, `environment-copy`

## Sources

- [MIT Scheme Environment Operations](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Environment-Operations.html)
- [Chez Scheme System Operations (CSUG 9.5)](https://cisco.github.io/ChezScheme/csug9.5/system.html)
- [Guile Module System Reflection](https://www.gnu.org/software/guile/manual/html_node/Module-System-Reflection.html)
