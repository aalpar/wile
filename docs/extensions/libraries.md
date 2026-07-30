# Extension Libraries: R7RS Import Integration

When the R7RS library system is enabled, Wile extensions automatically become
importable Scheme libraries. This allows Scheme code to selectively import
extension primitives using standard R7RS `(import ...)` syntax with full
support for `only`, `except`, `prefix`, and `rename` modifiers.

## Enabling the Library System

The library system is **opt-in**. Without `WithLibraryPaths`, `(import ...)`
raises a configuration error.

```go
// Library system disabled (default)
engine, _ := wile.NewEngine(ctx)
// (import ...) → error: "no library registry configured"

// Library system enabled with the default search path (".")
engine, _ := wile.NewEngine(ctx,
    wile.WithLibraryPaths(),
)

// Library system enabled with custom search paths
engine, _ := wile.NewEngine(ctx,
    wile.WithLibraryPaths("/app/libs", "./vendor"),
)
// Search order: /app/libs, ./vendor, .
```

The embedded standard library is not a search path: it is served by the
`FileResolver` chain over `stdlib.FS` (see
[`source-loading.md`](../embedding/source-loading.md)).
`compilation.DefaultLibraryPaths` holds the sole default, `"."`.

## Importing Extension Primitives

Once enabled, each extension with runtime primitives is automatically
registered as an R7RS library named `(wile <extension-name>)`:

```scheme
;; Import all primitives from the math extension
(import (wile math))
(sqrt 16)  ; → 4.0

;; Import only specific bindings
(import (only (wile math) sqrt sin cos))

;; Exclude specific bindings
(import (except (wile math) expt rationalize))

;; Prefix all imported bindings
(import (prefix (wile math) m:))
(m:sqrt 9)  ; → 3.0

;; Rename specific bindings
(import (rename (wile math) (sqrt square-root)))
(square-root 25)  ; → 5.0

;; Combine modifiers
(import (prefix (only (wile math) sqrt sin cos) math:))
(math:sqrt 4)  ; → 2.0
```

### Available Extension Libraries

| Library name | Go package | Primitives |
|-------------|------------|------------|
| `(wile io)` | `pkg/extensions/io` | 41 port/read/write primitives |
| `(wile math)` | `extensions/math` | 39 math primitives |
| `(wile system)` | `extensions/system` | 6 system primitives |
| `(wile files)` | `extensions/files` | 13 file/directory primitives |
| `(wile process)` | `extensions/process` | 8 process execution primitives |
| `(wile threads)` | `extensions/threads` | SRFI-18 threading primitives |
| `(wile gointerop)` | `extensions/gointerop` | Go concurrency primitives |
| `(wile introspection)` | `extensions/introspection` | Environment introspection, features, disassembler |
| `(wile eval)` | `extensions/eval` | 16 eval/load/expand/syntax-local primitives |
| `(wile charsets)` | `extensions/charsets` | 20 SRFI-14 character-set primitives |
| `(wile sat)` | `extensions/sat` | 2 CDCL SAT kernel primitives |
| `(wile algebragraph)` | `extensions/algebragraph` | 2 graph path-counting primitives |

Which of these exist in a given engine depends on the extension set: an
extension becomes a library only if it was loaded via `WithExtension` or a
profile. Internal extensions are registered the same way and yield
`(wile all)` or `(wile all-safe)`, `(wile envvars)`, and `(wile namespace)`, but
their Go packages are not importable by external code.

## How It Works

### Automatic Library Registration

During `NewEngine`, the engine tracks which primitives each extension
contributes. When `WithLibraryPaths` is active, these primitives are wrapped
in synthetic `CompiledLibrary` objects and registered in the
`LibraryRegistry`.

```
NewEngine initialization:
                                          registration index
  ┌─ core.AddToRegistry(reg)             0 → C
  │
  ├─ math.AddToRegistry(reg)             C → M
  ├─ record snapshot: "math" = [C, M)
  │
  ├─ system.AddToRegistry(reg)           M → S
  ├─ record snapshot: "system" = [M, S)
  │
  └─ ... (library system setup)
      ├─ Create LibraryRegistry
      ├─ For "math": create library (wile math) exporting prims[C:M]
      └─ For "system": create library (wile system) exporting prims[M:S]
```

Each snapshot records the extension's half-open index range plus its
`LibraryNamer` and `Describer`, if any. `registerExtensionLibraries`
(`pkg/wile/engine.go`) then calls `Registry.RuntimePrimitiveNamesRange(start,
end)` to determine exactly which runtime primitives that extension contributed.
Extensions that register only compile-time bindings (no `PhaseRuntime`
primitives) yield no names, so no library is created for them.

### Extension Primitives vs. Library Imports

Extension primitives are pre-bound in the top-level environment regardless of
whether the library system is enabled. The library system adds a *second* path
to the same closures:

```
                    ┌──────────────────────────┐
                    │  Top-level environment   │
                    │                          │
    Direct access:  │  sqrt  → ForeignClosure  │ ← always available
                    │  sin   → ForeignClosure  │
                    │  cos   → ForeignClosure  │
                    │  ...                     │
                    └──────────────────────────┘
                               ▲
                               │ (same closures)
                               │
                    ┌──────────┴───────────────┐
    Import access:  │  Library (wile math)     │ ← only with WithLibraryPaths
                    │  exports: sqrt, sin, cos │
                    └──────────────────────────┘
```

`WithStrictNamespace()` is the exception: the profile's extension primitives are
still registered (so libraries can import them) but are withheld from the top
level, leaving `(import ...)` as the only path.

The library system is useful for:
- **Selective import**: Only pull in the bindings you need
- **Namespacing**: Prefix bindings to avoid collisions
- **R7RS compliance**: Standard library import syntax
- **Scheme library files**: Loading `.sld` files from disk

## Custom Library Names

Extensions can supply `registry.WithLibraryName` (or implement
`registry.LibraryNamer` directly) to override the default `(wile <name>)`
naming:

```go
var Extension = registry.NewExtension("utils",
    func(r *registry.Registry) error {
        r.AddPrimitive(registry.PrimitiveSpec{
            Name: "helper", ParamCount: 0, Impl: primHelper,
        }, registry.PhaseSetRuntime)
        return nil
    },
    registry.WithLibraryName("myorg", "utils"))
```

An empty name part is rejected at engine construction with `ErrEngineInit`.

```scheme
;; Imports as (myorg utils) instead of (wile utils)
(import (myorg utils))
(helper)
```

Without `LibraryNamer`, the library name defaults to `(wile <ext.Name()>)`.

## Scheme Library Files (.sld)

The library system also supports loading pure Scheme libraries from `.sld`
files on disk. These use the standard R7RS `define-library` form:

```scheme
;; File: mylib/greet.sld
(define-library (mylib greet)
  (export greeting)
  (begin
    (define greeting "hello from library")))
```

```go
engine, _ := wile.NewEngine(ctx,
    wile.WithLibraryPaths("/path/to/libs"),
)
```

```scheme
(import (mylib greet))
greeting  ; → "hello from library"
```

### File Search

`ResolveLibraryFile` converts the library name to a relative path and hands it
to the engine's `FileResolver`, once per recognized extension. The **extension
is the outer loop**: every resolver and every search directory is tried with
`.sld` before `.scm` is tried anywhere.

```
Library name     →  Path probed
(mylib greet)    →  mylib/greet.sld   (whole resolver chain)
                    mylib/greet.scm   (whole resolver chain, only if no .sld hit)
```

Within `OSFileResolver`, the directories tried for each probe are, in order: the
current load directory from the load-path stack, the library registry's search
paths (user-supplied first, then the default `"."`), `$SCHEME_INCLUDE_PATH`, the
working directory, and the filesystem root as a last resort. Each candidate is
located with `os.Stat` and opened only after the authorizer permits
`code:load` on it.

## Combining Extensions, Libraries, and RegisterFunc

All three mechanisms compose naturally:

```go
engine, _ := wile.NewEngine(ctx,
    wile.WithExtension(math.Extension),
    wile.WithLibraryPaths(tmpDir),
)

// RegisterFunc adds to the top-level environment (not importable as a library)
engine.RegisterFunc("go-add", func(a, b int64) int64 {
    return a + b
})
```

```scheme
;; Import from extension library
(import (wile math))

;; Import from .sld file on disk
(import (math-utils))

;; Use RegisterFunc binding (always in top-level, no import needed)
(double (go-add 3 7))  ; → 20
```

Note: `RegisterFunc` bindings are added directly to the top-level environment
after engine creation. They are **not** wrapped in a library and cannot be
imported via `(import ...)`. They are always available without import.

## Library Environment Isolation

When a `.sld` library file is loaded, it runs in an isolated environment that
mirrors the engine's configuration (same registry, same macros) but does not
share the caller's bindings. This is standard R7RS library semantics.

The `LibraryEnvFactory` creates these environments:

```
Caller env ──► LibraryEnvFactory ──► Namespace.NewChildRuntime()
                                         │
                                         ├─ Apply registry
                                         ├─ Register syntax compilers
                                         ├─ Register primitive expanders
                                         ├─ Load bootstrap macros
                                         ├─ Load bootstrap procedures
                                         └─ Inject documentation
```

The frame is fresh; the `Namespace` is the engine's, shared with the caller.
That is why a `NamespaceInit` must be idempotent: it re-runs for every library
environment the engine builds. See "Registering Other Items" in
[`architecture.md`](architecture.md).

## Complete Example

A self-contained example showing extension creation, library import, and
Scheme-side usage:

```go
package main

import (
    "context"
    "fmt"

    "github.com/aalpar/wile/pkg/wile"
    "github.com/aalpar/wile/extensions/math"
    "github.com/aalpar/wile/extensions/process"
    "github.com/aalpar/wile/extensions/system"
)

func main() {
    ctx := context.Background()

    engine, err := wile.NewEngine(ctx,
        wile.WithExtensions(
            math.Extension,
            process.Extension,
            system.Extension,
        ),
        wile.WithLibraryPaths(),
    )
    if err != nil {
        panic(err)
    }

    result, err := engine.EvalMultiple(ctx, `
        ;; Selectively import only what we need
        (import (only (wile math) sqrt floor))
        (import (prefix (wile system) sys:))

        ;; Use imported bindings
        (let ((x (sqrt 200)))
          (floor x))
    `)
    if err != nil {
        panic(err)
    }

    fmt.Println(result.SchemeString()) // "14.0"
}
```

## Limitations

1. **Library system is opt-in.** Without `WithLibraryPaths()`, `(import ...)`
   fails with `"no library registry configured"`.

2. **Extensions must be added before engine creation.** The registry is
   populated during `NewEngine` and cannot be modified afterward.

3. **`RegisterFunc` bindings are not importable.** Functions registered via
   `RegisterFunc`/`RegisterFuncs`/`RegisterPrimitive` bind directly into the
   top-level environment and are not wrapped in a library.

4. **`LibraryEnvFactory` is per-`Namespace`.** Each engine stores
   its own factory via `SetLibraryEnvFactory`, so multiple engines can coexist
   safely without race conditions.

5. **Extensions with only compile-time bindings produce no library.** If an
   extension registers no `PhaseRuntime` primitives, no library is created
   for it.

6. **Cycle detection.** `LoadLibrary` carries a per-goroutine load chain on the
   context and raises `ErrCircularDependency` on synchronous re-entry. A name
   another goroutine is still loading is a wait, not a cycle.
