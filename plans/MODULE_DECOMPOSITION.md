# Plan: Module Decomposition

**Status**: Proposed
**Date**: 2026-02-20
**Related**: `SANDBOXING_MODEL.md` (extension-level capability control), `DIALECT_SYSTEM.md` (dialect configurability)

## Motivation

The sandboxing plan establishes that extensions are opt-in capabilities. The dialect plan establishes that the language personality is configurable. The logical conclusion: the core repo should contain only what's irreducible, and everything else should be a separate Go module that embedders compose via `go get` and `WithExtension()`.

This gives embedders precise control over binary size, capability surface, and dependency footprint. A minimal embedded Scheme that does pure computation imports only `github.com/aalpar/wile`. One that needs I/O adds `github.com/aalpar/wile-io`. No unused code compiles in.

## The Core Boundary

The boundary is determined by answering: **what can't be removed without making the engine unable to function?**

### What stays in `github.com/aalpar/wile`

| Package | Contents | Why irreducible |
|---------|----------|----------------|
| `values/` | Type system: Value interface, Pair, Symbol, Number, String, Vector, ByteVector, Boolean, Char, Port, etc. | Every package depends on this |
| `environment/` | Bindings, scopes, phases, TopLevelEnvironment, GlobalEnvironmentFrame | Compiler and VM require it |
| `machine/` | VM (MachineContext.Run), compiler, expander, continuations, bytecode, operations | The execution engine |
| `registry/` | Registry type, Extension interface, Phase, RegistryBuilder, PrimitiveSpec | The composition mechanism |
| `registry/core/` | ~148 primitives in 15 categories + core bootstrap macros | Language doesn't function without `cons`, `+`, `if`, `lambda` |
| `internal/*` | Tokenizer, parser, syntax objects, validator, forms registry, pattern matcher, bootstrap | Compilation pipeline |
| Root package | Engine, EngineOption, Dialect, public API | Embedder entry point |

### Core primitives (15 categories, ~148 primitives)

These are in `registry/core/` and are truly irreducible:

| Category | File | Key primitives |
|----------|------|---------------|
| special forms | `specialforms.go` | `if`, `lambda`, `define`, `set!`, `begin`, `quote`, `quasiquote`, `dynamic-wind`, `case-lambda`, `else`, `=>`, `...`, `_`, `syntax-rules`, `define-syntax`, `import`, `include`, `cond-expand` |
| predicates | `predicates.go` | `null?`, `pair?`, `boolean?`, `number?`, `symbol?`, `string?`, `char?`, `vector?`, `bytevector?`, `procedure?`, `list?`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`, `exact?`, `inexact?`, `exact-integer?` |
| equality | `equality.go` | `eq?`, `eqv?`, `equal?`, `boolean=?`, `symbol=?`, `not` |
| pairs | `pairs.go` | `cons`, `car`, `cdr`, `set-car!`, `set-cdr!`, 28 CxR accessors |
| lists | `lists.go` | `list`, `make-list`, `append`, `reverse`, `length`, `list-ref`, `list-set!`, `list-tail`, `list-copy`, `memq`, `memv`, `member`, `assq`, `assv`, `assoc` |
| arithmetic | `arithmetic.go` | `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `=`, `abs`, `min`, `max`, `quotient`, `remainder`, `modulo`, `gcd`, `lcm`, `exact`, `inexact`, `number->string`, `string->number`, `floor`, `ceiling`, `truncate`, `round`, `rationalize`, `make-rectangular`, `real-part`, `imag-part`, `magnitude`, `angle` |
| control | `control.go` | `apply`, `call/cc`, `call-with-current-continuation`, `values`, `call-with-values`, `call-with-continuation-barrier` |
| vectors | `vectors.go` | `make-vector`, `vector`, `vector-length`, `vector-ref`, `vector-set!`, `vector->list`, `list->vector`, `vector-copy`, `vector-fill!`, `vector-copy!`, `vector-append`, `vector-map`, `vector-for-each` |
| strings | `strings.go` | `make-string`, `string`, `string-length`, `string-ref`, `string-set!`, `substring`, `string-append`, `string->list`, `list->string`, `string-copy`, `string->symbol`, `symbol->string`, string comparisons |
| characters | `characters.go` | `char->integer`, `integer->char`, char comparisons |
| bytevectors | `byte_vectors.go` | `make-bytevector`, `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!`, `bytevector-copy`, `bytevector-copy!`, `bytevector-append`, `utf8->string`, `string->utf8` |
| syntax | `syntax.go` | `identifier?`, `syntax->datum`, `datum->syntax`, `generate-temporaries`, `bound-identifier=?`, `free-identifier=?` |
| parameters | `parameters.go` | `make-parameter`, `parameter?` |
| boxes | `boxes.go` | `box`, `box?`, `unbox`, `set-box!` |
| hashtables | `hashtables.go` | `make-hashtable`, `hashtable?`, `hashtable-ref`, `hashtable-set!`, `hashtable-delete!`, `hashtable-keys`, `hashtable-values`, `hashtable-size`, `hashtable-copy`, `hashtable-clear!` |
| continuations | `prompts.go` | `make-continuation-prompt-tag`, `default-continuation-prompt-tag`, `continuation-prompt-tag?`, `call-with-continuation-prompt`, `abort-current-continuation`, `call-with-composable-continuation` |

### Core bootstrap macros

These macros depend ONLY on core primitives and stay in `registry/core/bootstrap.scm`:

`and`, `or`, `let`, `let*`, `letrec`, `letrec*`, `cond`, `case`, `when`, `unless`, `parameterize`, `let-values`, `let*-values`, `define-values`, `do`, `map`, `for-each`, `with-continuation-barrier`, `with-baffle`

### Bootstrap macros that move to extensions

These macros depend on extension primitives and move to the extension that provides those primitives (via `AddMacroSource`):

| Macro | Depends on | Moves to |
|-------|-----------|----------|
| `guard`, `guard-aux` | `with-exception-handler`, `raise` | `wile-exceptions` |
| `delay`, `delay-force` | `%make-lazy-promise` | `wile-promises` |
| `define-record-type`, `define-record-type-impl` | `make-record-type`, `record-constructor`, `record-predicate`, `record-accessor`, `record-modifier` | `wile-records` |

## Extension Modules

Each extension is a separate Go module with a single dependency on `github.com/aalpar/wile`.

### Extension inventory

| Module | Current location | Primitives | Macros | Security class |
|--------|-----------------|------------|--------|---------------|
| `wile-io` | `internal/extensions/io/` | read, write, display, newline, ports, string/bytevector ports, current-input/output-port, binary I/O | none | safe |
| `wile-exceptions` | `extensions/exceptions/` | raise, with-exception-handler, raise-continuable, error, error-object?, error-object-message, error-object-irritants, error-object-type | `guard`, `guard-aux` | safe |
| `wile-records` | split from `internal/extensions/all/` | make-record-type, record-constructor, record-predicate, record-accessor, record-modifier, record?, record-type? | `define-record-type`, `define-record-type-impl` | safe |
| `wile-promises` | split from `internal/extensions/all/` | %make-lazy-promise, promise?, force, make-promise | `delay`, `delay-force` | safe |
| `wile-math` | `extensions/math/` | sqrt, sin, cos, tan, asin, acos, atan, exp, log, expt, floor, ceiling, truncate, round | none | safe |
| `wile-files` | `extensions/files/` | open-input-file, open-output-file, delete-file, file-exists?, call-with-input-file, call-with-output-file, with-input-from-file, with-output-to-file | none | privileged |
| `wile-eval` | `internal/extensions/eval/` | eval, load, interaction-environment, scheme-report-environment, null-environment, environment, syntax-expand, compile | none | privileged |
| `wile-system` | `extensions/system/` | exit, emergency-exit, command-line, get-environment-variable, get-environment-variables | none | privileged |
| `wile-threads` | `extensions/threads/` | SRFI-18 threads, mutexes, condition variables, thread time | none | context-dependent |
| `wile-gointerop` | `extensions/gointerop/` | Go function bridge | none | privileged |

### Decomposing `internal/extensions/all/`

The current `all` extension is a grab bag that bundles:

| Contents | Destination |
|----------|-------------|
| Records (make-record-type, etc.) | `wile-records` |
| Promises (%make-lazy-promise, force, etc.) | `wile-promises` |
| String mutation (string-copy!, string-fill!) | `registry/core/` (these are `(scheme base)` and use only core types) |

After decomposition, `internal/extensions/all/` no longer exists.

### Import graph

```
github.com/aalpar/wile                 (core — zero external deps)

github.com/aalpar/wile-io              → wile
github.com/aalpar/wile-exceptions      → wile
github.com/aalpar/wile-records         → wile
github.com/aalpar/wile-promises        → wile
github.com/aalpar/wile-math            → wile
github.com/aalpar/wile-files           → wile
github.com/aalpar/wile-eval            → wile
github.com/aalpar/wile-system          → wile
github.com/aalpar/wile-threads         → wile
github.com/aalpar/wile-gointerop       → wile

github.com/aalpar/wile-scheme          → all of the above (CLI/REPL)
```

No extension depends on another extension. No circular dependencies. Each extension's only dependency is the core module.

### Extension module structure

Each extension module follows the same pattern:

```
wile-io/
├── go.mod              # module github.com/aalpar/wile-io
├── register.go         # var Extension = registry.NewExtension("io", AddToRegistry)
├── prim_read_write.go  # primitive implementations
├── prim_ports.go
├── ...
├── macros.scm          # bootstrap macros (if any), embedded via go:embed
└── *_test.go
```

The embedder imports:

```go
import (
    "github.com/aalpar/wile"
    wileio "github.com/aalpar/wile-io"
)

eng, _ := wile.NewEngine(ctx,
    wile.WithExtension(wileio.Extension),
)
```

## R7RS Standard Library Files (.sld)

The `lib/` directory contains `.sld` files that define standard R7RS libraries (e.g., `lib/scheme/base.sld`, `lib/scheme/file.sld`). These reference extension primitives via `(import ...)`.

**Problem:** `lib/scheme/file.sld` re-exports filesystem primitives. If `wile-files` is a separate repo, where does `scheme/file.sld` live?

**Solution:** Each extension module ships its own `.sld` files. The engine's `WithLibraryPaths()` searches all configured paths. The extension provides a helper:

```go
// In wile-files
import "embed"

//go:embed lib
var LibDir embed.FS

// LibraryPath returns the path to the embedded library files.
func LibraryPath() string { ... }
```

Or the extension registers its library directly via the synthetic library mechanism (already exists in `engine.go:155-178` for extension primitives).

**For standard R7RS libraries** that bundle primitives from multiple extensions (e.g., `(scheme base)` includes both core and io primitives), the `.sld` file lives with whoever bundles the full R7RS personality — the `r7rs` dialect package or a `wile-std` convenience module.

## Convenience Bundles

Individual extension repos provide maximum granularity. Convenience bundles provide common compositions:

### Option A: `r7rs` package in core

A package within `github.com/aalpar/wile` that re-exports extension configurations:

```go
// wile/r7rs/dialect.go
package r7rs

var Dialect = wile.Dialect{
    // ... includes io, exceptions, records, promises, math
}
```

**Problem:** This creates an import dependency from `wile` core to all extension modules, defeating the purpose of the split.

### Option B: Separate `wile-r7rs` module

```go
// github.com/aalpar/wile-r7rs
package r7rs

import (
    wileio "github.com/aalpar/wile-io"
    wileexc "github.com/aalpar/wile-exceptions"
    wilerec "github.com/aalpar/wile-records"
    wileprom "github.com/aalpar/wile-promises"
    // ...
)

var AllExtensions = []wile.EngineOption{
    wile.WithExtension(wileio.Extension),
    wile.WithExtension(wileexc.Extension),
    wile.WithExtension(wilerec.Extension),
    wile.WithExtension(wileprom.Extension),
    // ...
}

var Dialect = wile.Dialect{ ... }
```

**This is the right approach.** The core repo stays dependency-free. The bundle repo depends on all extensions. Embedders who want full R7RS `go get github.com/aalpar/wile-r7rs` and get everything.

### Option C: `wile-std` (standard library bundle)

A bundle of the "safe" extensions — io, exceptions, records, promises, math — without the privileged ones (files, eval, system, gointerop, threads):

```go
// github.com/aalpar/wile-std
var SafeExtensions = []wile.EngineOption{ ... }
```

This connects to `SANDBOXING_MODEL.md` Phase 1 (SafeExtensions convenience API), just as a separate module instead of in-core.

## Go Workspace for Development

```
wile-workspace/
├── go.work
├── wile/
├── wile-io/
├── wile-exceptions/
├── wile-records/
├── wile-promises/
├── wile-math/
├── wile-files/
├── wile-eval/
├── wile-system/
├── wile-threads/
├── wile-gointerop/
├── wile-r7rs/
└── wile-scheme/
```

`go.work`:

```
go 1.24

use (
    ./wile
    ./wile-io
    ./wile-exceptions
    ./wile-records
    ./wile-promises
    ./wile-math
    ./wile-files
    ./wile-eval
    ./wile-system
    ./wile-threads
    ./wile-gointerop
    ./wile-r7rs
    ./wile-scheme
)
```

Local changes to any module are immediately visible to all others via `go work`. Published releases use normal `go get` with version constraints.

## Why ~148 Core Primitives Is the Floor

It's tempting to shrink core further — do we really need `-` when `(+ a (* -1 b))` works? The answer is yes, and the reasons are structural, not just performance.

**Three constraints that define the floor:**

1. **Bootstrap dependency.** The bootstrap macros (`cond`, `case`, `parameterize`, `define-values`, `map`, `for-each`, etc.) reference specific primitives: `memv`, `cons`, `car`, `cdr`, `apply`, `call-with-values`, `dynamic-wind`, `null?`, `not`, `list`, `set!`. These primitives must be Go-level (`AddPrimitive`) because bootstrap macros run *before* any Scheme-defined functions exist. You can't define `memv` in Scheme and then use it in `cond`'s expansion — `cond` is being loaded at the same time.

2. **Scheme-level load ordering.** Go primitives (`AddPrimitive`) are all available simultaneously after `Registry.Apply()` — no ordering issue. Scheme-defined functions (`AddMacroSource`) are evaluated sequentially and can only reference things defined before them. If `-` were a Scheme function provided by an extension, the extension's macro source must be evaluated before any code that uses `-`. This ordering is implicit (determined by `WithExtension()` call order) and invisible to Go's module system. A second dependency graph layered on top of `go.mod` is the complexity you'd be buying.

3. **No embedder benefit.** Nobody wants Scheme-without-subtraction. The sandboxing use case is about *capabilities* (filesystem, eval, system), not *arithmetic*. Removing `append` or `list-ref` from core saves nothing meaningful — the embedder still needs them, they just import them from somewhere else.

**What could theoretically leave core** (not used by bootstrap, not performance-critical):

| Primitives | Count | Why you'd extract |
|-----------|-------|-------------------|
| CxR accessors (`caaar` through `cddddr`) | 28 | Pure convenience |
| Hashtables | 10 | Non-R7RS (Wile extension) |
| Boxes | 4 | Non-R7RS (Wile extension) |
| Bytevectors (some) | ~5 | Scripts that don't do binary |

But the savings are marginal (~47 primitives) and the cost is real: more repos, more version coordination, load-ordering problems for any that get Scheme-level replacements.

**The goal is not to minimize core — it's to make the extension mechanism so clear that it's self-service.** Someone should be able to read the docs and build their own `guard`-equivalent without special knowledge. Whether `guard` itself lives in core or in an extension is a pragmatic choice. What matters is that the mechanism for low-level extension works tractably from the outside.

## Initialization Ordering

### The problem

The engine initializes in a strict sequence:

```
1. Registry built       — AddPrimitive() from all extensions (Go-level)
2. Environment created
3. Registry applied     — Go primitives become env bindings (ForeignClosure)
4. Syntax compilers     — RegisterSyntaxCompilers
5. Expanders            — RegisterPrimitiveExpanders
6. Bootstrap macros     — AddMacroSource strings evaluated sequentially
7. Library system       — LibraryEnvFactory set, search paths configured
8. User code runs
```

Steps 1-3 are order-independent: all Go primitives land in the environment before any Scheme runs. Step 6 is order-dependent: macro sources are evaluated sequentially and can reference results of earlier evaluations. The library system (step 7) isn't active yet during step 6.

This creates a hard split between things that can be defined during bootstrap (step 6) and things that must wait for the library system (step 7+).

### Four techniques for providing Scheme-level functionality

| Technique | Bound as | When available | Ordering constraint |
|-----------|----------|---------------|-------------------|
| `AddPrimitive` (ForeignFunction) | Go closure in env | After step 3 — before any Scheme | None — all available simultaneously |
| `AddMacroSource` (macro/define) | Scheme syntax transformer or closure | After its source is evaluated in step 6 | Must come after any macro/function it references |
| Synthetic library (engine.go:155-178) | Available via `(import ...)` | On demand, after step 7 | Library system handles ordering |
| R7RS library (.sld file) | Available via `(import ...)` | On demand, after step 7 | Library system handles ordering |

The first technique has no ordering problem. The last two have ordering solved by the library system (load-on-demand, cycle detection, caching). The second technique — `AddMacroSource` — is the trouble zone.

### The solution: two tiers

**Tier 1 (Bootstrap): `AddMacroSource` — core only, no cross-extension dependencies.**

Bootstrap macros are loaded during step 6. They depend ONLY on Go primitives (always available) and on earlier bootstrap macros (guaranteed by sequential evaluation within a single macro source string). No extension's bootstrap code depends on another extension's bootstrap code.

These macros are required for the language to function at all — including for the library system to compile `(define-library ...)` and `(import ...)` forms. They are: `and`, `or`, `let`, `let*`, `letrec`, `letrec*`, `cond`, `case`, `when`, `unless`, `parameterize`, `let-values`, `let*-values`, `define-values`, `do`, `map`, `for-each`, `with-continuation-barrier`, `with-baffle`.

**Tier 2 (Library): Extension Scheme code loaded via the library system.**

Extension-specific macros and functions become library exports, loaded on demand via `(import ...)`. The library system handles dependency resolution automatically.

Example — `guard` as a library export:

```scheme
;; Provided by wile-exceptions as part of its library definition
(define-library (wile exceptions)
  (export guard with-exception-handler raise raise-continuable
          error error-object? error-object-message
          error-object-irritants error-object-type)
  (import (scheme base))   ;; for let, if, lambda, call/cc — all bootstrap
  (begin
    (define-syntax guard ...)
    (define-syntax guard-aux ...)))
```

When user code writes `(import (wile exceptions))`, the library system loads and evaluates this definition. `guard` depends on `with-exception-handler` and `raise` (Go primitives from the same extension, already in the environment) and on `call/cc` and `let` (core bootstrap, already loaded). No ordering problem.

**`(scheme base)` re-exports from extension libraries.** R7RS says `guard` is part of `(scheme base)`. So `(scheme base)` would `(import (wile exceptions))` and re-export `guard`. This is how R7RS is designed — `(scheme base)` is a composite library that bundles features from multiple sources. The library system handles the transitive dependency resolution.

### How extensions provide both Go and Scheme code

An extension module provides:

1. **Go primitives** via `AddPrimitive` — available immediately after registry application (step 3)
2. **A synthetic library** registered in the library system — provides Scheme-level macros/functions, loaded on demand after step 7

The engine already creates synthetic libraries for extension primitives (`engine.go:155-178`). The extension just needs to also include macro sources in its library definition. The mechanism for this is:

- Extension ships a `.sld` file (embedded via `go:embed`) that defines the library with both exported Go primitives and Scheme macros
- Or the extension programmatically creates a `CompiledLibrary` that includes macro sources

The specific API for attaching Scheme definitions to synthetic libraries needs design work (it doesn't exist yet — synthetic libraries currently only export Go primitives). This is a concrete deliverable of the module decomposition.

### What this means for extension authors

An extension author who wants to provide low-level Scheme functionality (macros, helper functions, derived forms) follows this pattern:

1. Implement Go primitives normally via `AddPrimitive`
2. Write Scheme macros/functions in a `.sld` file that `(import ...)` whatever bootstrap macros they need
3. Ship the `.sld` file with the extension module (embedded via `go:embed`)
4. Users access the functionality via `(import (extension-name))`

The extension author doesn't need to know about bootstrap ordering, `AddMacroSource` internals, or the engine's initialization sequence. They write a standard R7RS library definition. The library system handles the rest.

### Fallback: declared dependencies for global Scheme definitions

For the rare case where an extension truly needs Scheme code available globally (not via `(import ...)`), the Extension interface supports an optional dependency declaration:

```go
// SchemeDependencies returns extension names whose Scheme-level definitions
// must be loaded before this extension's AddMacroSource code.
// Most extensions should use the library system instead of this mechanism.
type SchemeDependencyDeclarer interface {
    SchemeDependencies() []string
}
```

The engine topologically sorts extensions with `AddMacroSource` code based on these declarations. This is the K8s PostStartHook pattern — explicit dependency declaration resolved at startup.

This should be rarely used. The library system is the primary mechanism.

## The Embedder Experience

**Minimal (pure computation):**

```go
import "github.com/aalpar/wile"

eng, _ := wile.NewEngine(ctx)
result, _ := eng.Eval(ctx, "(+ 1 2 3)")
// result is values.Int(6) — no I/O, no files, no eval
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
    wile.WithLibraryPaths("./lib"),
)
```

**Custom DSL with Go interop:**

```go
import (
    "github.com/aalpar/wile"
    wilegio "github.com/aalpar/wile-gointerop"
)

eng, _ := wile.NewEngine(ctx,
    wile.WithExtension(wilegio.Extension),
)
eng.RegisterFunc("process-data", myGoFunction)
```

## Phases

### Phase 1: Decompose `internal/extensions/all/`

Split the grab bag:
- Records → new `internal/extensions/records/` (temporary, before extraction)
- Promises → new `internal/extensions/promises/` (temporary, before extraction)
- String mutation ops (`string-copy!`, `string-fill!`) → `registry/core/strings.go`

Move bootstrap macros: `guard`/`guard-aux` to exceptions, `delay`/`delay-force` to promises, `define-record-type` to records. Each extension's `AddToRegistry` calls `r.AddMacroSource(...)` for its macros.

**Prerequisite:** None. Internal refactor, no public API change.

### Phase 2: Make `internal/extensions/` public

Move internal extensions to public packages:
- `internal/extensions/io/` → `extensions/io/` (or top-level `io/`)
- `internal/extensions/eval/` → `extensions/eval/`
- New `extensions/records/`, `extensions/promises/` from Phase 1

After this phase, all extensions are importable by external code.

**Prerequisite:** Phase 1.

### Phase 3: Extract extension modules

One extension at a time, extract to separate repos:

1. Start with a leaf extension that has no dependents — `wile-math` or `wile-system`
2. Create the repo, move code, update imports
3. Set up CI and release tagging
4. Update `wile-scheme` (CLI) to import from the new module
5. Repeat for each extension

Order (least dependencies first):
1. `wile-math` — pure math, zero dependents
2. `wile-system` — exit/env vars, zero dependents
3. `wile-gointerop` — Go bridge, zero dependents
4. `wile-threads` — SRFI-18, zero dependents
5. `wile-files` — filesystem, zero dependents
6. `wile-io` — ports/read/write
7. `wile-exceptions` — raise/guard
8. `wile-records` — record types
9. `wile-promises` — lazy evaluation
10. `wile-eval` — eval/load/environments

**Prerequisite:** Phase 2.

### Phase 4: Create convenience bundles

- `wile-r7rs` — full R7RS dialect bundle (all extensions + standard library .sld files)
- `wile-std` — safe extensions bundle (io + exceptions + records + promises + math)

**Prerequisite:** Phase 3 (all extensions extracted).

### Phase 5: Go workspace setup

Create `wile-workspace` repo (or document the workspace pattern) with `go.work` linking all modules for development.

**Prerequisite:** Phase 3.

## Scope Boundaries

**Covered by this plan:**
- Core/extension boundary definition
- `internal/extensions/all/` decomposition
- Extension extraction to separate Go modules
- Convenience bundles (`wile-r7rs`, `wile-std`)
- Go workspace for multi-module development
- Bootstrap macro migration

**Not covered:**
- Dialect system implementation (`DIALECT_SYSTEM.md`) — composable with but independent of module extraction
- Authorization framework (`AUTHORIZATION_FRAMEWORK.md`) — lives in core
- Sandboxing convenience API (`SANDBOXING_MODEL.md`) — moves to `wile-std` bundle
- New value types — `values/` stays in core; new types added there
- Parser extensions — `internal/tokenizer`, `internal/parser` stay in core

## Dependencies on Other Plans

| This plan | Depends on | Why |
|-----------|-----------|-----|
| Phase 1 (decompose all) | Nothing | Internal refactor |
| Phase 2 (make public) | None | Internal refactor |
| Phase 3 (extract modules) | Phase 2 | Can't extract until public |
| Phase 4 (bundles) | `DIALECT_SYSTEM.md` Phase 3 | `wile-r7rs` needs the Dialect type |
| Phase 4 (bundles) | `SANDBOXING_MODEL.md` Phase 1 | `wile-std` is SafeExtensions as a module |

## Decision Log

| Decision | Rationale |
|----------|-----------|
| Core includes all ~148 primitives in `registry/core/` | Bootstrap macros depend on them (can't be Scheme-defined — they run before the library system). Shrinking core further buys no real embedder benefit and creates Scheme-level load-ordering problems. The goal is a clear extension mechanism, not minimal core size. |
| Bootstrap macros split by dependency | 16 macros depend only on core → stay as `AddMacroSource` (Tier 1). 5 macros depend on extension primitives → become library exports loaded via `(import ...)` (Tier 2). |
| Two-tier Scheme initialization | Tier 1 (bootstrap `AddMacroSource`): core-only, no cross-extension deps. Tier 2 (library system): extension Scheme code loaded on demand via `(import ...)`). Library system handles ordering automatically. This avoids a parallel dependency graph. |
| Library system is the primary mechanism for extension Scheme code | Extensions provide Go primitives via `AddPrimitive` and Scheme definitions via `.sld` library files. Users access both via `(import ...)`. Extension authors don't need to understand bootstrap internals — they write standard R7RS library definitions. |
| `SchemeDependencies` as fallback only | K8s-style explicit dependency declaration for the rare case where global Scheme definitions (outside the library system) are needed. Most extensions should use the library system instead. |
| String mutation ops move to core | `string-copy!` and `string-fill!` are `(scheme base)` and use only core types. They don't belong in a separate extension. |
| Separate repos over monorepo subdirectories | Go modules in subdirectories of a monorepo share version tags, complicating independent releases. Separate repos have independent versioning, CI, and release cycles. |
| `wile-r7rs` as a separate bundle module | Can't live in core — would create import dependency from core to all extensions. A separate module that depends on all extensions provides the "batteries included" experience without bloating core. |
| Extension extraction order: leaves first | Extensions with zero dependents (math, system) are safest to extract first. Extensions that other code might reference (io, exceptions) go later to minimize mid-migration breakage. |
| No extension depends on another extension | Each extension depends only on `wile` core. This keeps the dependency graph flat and prevents cascading version conflicts. If an extension needs functionality from another (e.g., eval needs io for `load`), it depends on the shared core interfaces, not the extension. |

## Open Questions

1. **I/O in core vs separate?** `display`, `read`, `write` are `(scheme base)`. An engine without I/O can't print. But for embedded use, the Go program is the I/O layer — results come back as `values.Value`. The engine can function without `display`. Leaning toward separate.

2. **How many repos is too many?** 10 extension repos + 2 bundles + 1 CLI = 14 repos. This is manageable but requires CI/release tooling. Alternative: fewer, larger repos (e.g., merge records + promises + exceptions into `wile-std-extensions`). The tradeoff is granularity vs management overhead.

3. **Versioning discipline?** Each extension pins a minimum `wile` core version. Breaking changes in core (e.g., `MachineContext` API) require coordinated releases. A compatibility matrix or CI that tests extensions against core HEAD would help.

4. **Test migration?** Many tests in `registry/core/*_test.go` use `runSchemeCode` which creates a full environment with all extensions. These tests would need to use only core primitives, or be restructured to test with explicit extension composition.

5. **Synthetic library API for Scheme definitions.** Synthetic libraries currently only export Go primitives (`engine.go:155-178`). To support Tier 2 (extension Scheme code loaded via library system), synthetic libraries need to also carry macro sources that are evaluated when the library is first loaded. The API for attaching Scheme definitions to synthetic libraries needs design work — this is a concrete deliverable.

6. **Pragmatic core size.** The goal is a clear, self-service extension mechanism — not minimal core. Some things that *could* technically be extensions (e.g., `guard`, `define-record-type`) may stay in core for pragmatic reasons. What matters is that the mechanism works identically for things inside and outside core. If someone builds their own `guard`-equivalent as an extension, the same library-system path works — they write a `.sld` file, ship it with their module, users `(import ...)` it.

## Verified Dependency Analysis (2026-02-25)

Concrete import analysis of every extension package, confirming which can be extracted today.

### engine.go does NOT import any extensions

`engine.go:17-31` imports only: `environment`, `internal/parser`, `internal/syntax`, `machine`, `registry`, `registry/core`, `values`. Extensions are purely opt-in via `WithExtension()`. An embedder who writes `wile.NewEngine(ctx)` compiles only core into their binary. **The Go linker already dead-code-eliminates unreferenced extension packages within the module.**

This means separate repos are not needed for binary size reduction alone — the current single-module structure already supports it. Separate repos buy: independent versioning, pkg.go.dev discoverability, and ecosystem signal.

### The only all-extensions import is internal/bootstrap

`internal/bootstrap/environment_tiny.go:37-51` imports all 9 extensions (6 public + 3 internal) into the `allExtensions` slice. Only `cmd/scheme/main.go` imports `internal/bootstrap`. Embedders using the Engine API never touch this path.

### Public extensions: verified clean dependency profiles

Every file in `extensions/` was checked. Production code imports only public packages:

| Extension | Production imports (non-stdlib) |
|-----------|-------------------------------|
| **math** | `registry`, `registry/helpers`, `machine`, `values` |
| **system** | `registry`, `registry/helpers`, `machine`, `values` |
| **exceptions** | `registry`, `registry/helpers`, `machine`, `values` |
| **gointerop** | `registry`, `registry/helpers`, `machine`, `values` |
| **threads** | `registry`, `registry/helpers`, `machine`, `values` |
| **files** | `registry`, `registry/helpers`, `machine`, `values` |

No `internal/` imports in production code. Test-only: `files` tests import `internal/extensions/io` (for port state setup).

**All 6 can be extracted to separate repos with zero production code changes.** Test files would need adjustment (files tests need io extension loaded via Engine, not direct internal import).

### Internal extensions: confirmed internal package dependencies

| Extension | Internal imports (production) | Extraction blocker |
|-----------|------------------------------|-------------------|
| **io** | `internal/parser`, `internal/tokenizer`, `internal/syntax` | `read`/`read-syntax` need parser; port caching needs tokenizer |
| **eval** | `internal/parser`, `internal/schemeutil`, `internal/syntax` | `eval`/`load` need full parse→expand→compile pipeline |
| **all** | imports all 8 other extensions (both public and internal) | Meta-package; own primitives (records, promises, strings, chars) use only public packages |

### `all` extension decomposition confirmed viable

`all/register.go` aggregates all extensions. But its own primitive files have clean imports:
- `prim_all.go` (records, promises): `environment`, `machine`, `registry/helpers`, `values`
- `prim_strings.go`: `machine`, `registry/helpers`, `values`
- `prim_characters.go`: `machine`, `registry/helpers`, `values`

These can be split into `wile-records`, `wile-promises`, and moved to core (strings/chars) as the plan describes.

### files → io runtime dependency confirmed

`files` extension calls `extio.GetCurrentInputPort()`, `extio.SetCurrentInputPort()`, etc. for `with-input-from-file`/`with-output-to-file`. The import is `internal/extensions/io` — currently internal.

**Options for extraction:**
1. Extract io first, then files depends on `wile-io` (violates "no extension depends on another extension" rule)
2. Move port state accessors to a public interface in core (e.g., `registry/portstate`)
3. Accept that files+io are always co-loaded (bundle them or keep io in core)

### Existing extension example validates the pattern

`wile-extension-example/` is already a separate Go module (`go.mod`: `require github.com/aalpar/wile v1.3.1`). Its `kvstore/` package demonstrates the full extension authoring pattern: `registry.Extension` + `registry.Closeable` + `PrimitiveSpec` registration. Pattern is proven and working.

### Scheme libraries inventory

```
lib/scheme/   — R7RS-small core (base, char, complex, cxr, eval, file, inexact, lazy, load,
                process-context, read, repl, time, write, r5rs, case-lambda) — must stay
lib/srfi/     — SRFI-1 only (list operations)
lib/chibi/    — test.sld, optional.sld, diff.sld, term/ (terminal)
```

Too few SRFIs to justify a separate repo yet. As more are added, `wile-srfi` makes sense.

### Key conclusion

The plan's phased approach is correct. The immediate blockers for Phase 3 (extract extension modules) are:
1. **Phase 2 prerequisite (make io/eval public)** — requires deciding whether to expose `internal/parser` etc. or create public interfaces
2. **files→io dependency** — needs a resolution strategy before files can be independently extracted
3. **Test migration** — files tests import internal io directly; would need Engine-based test setup
