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

// Library system enabled with default search paths ("." and "./stdlib/lib")
engine, _ := wile.NewEngine(ctx,
    wile.WithLibraryPaths(),
)

// Library system enabled with custom search paths
engine, _ := wile.NewEngine(ctx,
    wile.WithLibraryPaths("/app/libs", "./vendor"),
)
// Search order: /app/libs, ./vendor, ., ./stdlib/lib
```

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
(import (except (wile math) expt square))

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
| `(wile math)` | `extensions/math` | 35 math primitives |
| `(wile system)` | `extensions/system` | 6 system primitives |
| `(wile files)` | `extensions/files` | 13 file/directory primitives |
| `(wile process)` | `extensions/process` | 8 process execution primitives |
| `(wile threads)` | `extensions/threads` | SRFI-18 threading primitives |
| `(wile gointerop)` | `extensions/gointerop` | Go concurrency primitives |
| `(wile introspection)` | `extensions/introspection` | Environment introspection, features, disassembler |
| `(wile eval)` | `extensions/eval` | `eval`, `load`, expand, syntax-local-* |

## How It Works

### Automatic Library Registration

During `NewEngine`, the engine tracks which primitives each extension
contributes. When `WithLibraryPaths` is active, these primitives are wrapped
in synthetic `CompiledLibrary` objects and registered in the
`LibraryRegistry`.

```
NewEngine initialization:
                                          primitive count
  ┌─ core.AddToRegistry(reg)             0 → 80
  │
  ├─ snapshot = 80
  ├─ math.AddToRegistry(reg)             80 → 110
  ├─ record: "math" contributed prims[80:110]
  │
  ├─ snapshot = 110
  ├─ system.AddToRegistry(reg)           110 → 119
  ├─ record: "system" contributed prims[110:119]
  │
  └─ ... (library system setup)
      ├─ Create LibraryRegistry
      ├─ For "math": create library (wile math) exporting prims[80:110]
      └─ For "system": create library (wile system) exporting prims[110:119]
```

The snapshot mechanism uses `Registry.RuntimePrimitiveNamesSince(startIndex)`
to determine exactly which runtime primitives an extension contributed.
Extensions that register only compile-time bindings (no `PhaseRuntime`
primitives) do not produce a library.

### Extension Primitives vs. Library Imports

Extension primitives are **always** available in the top-level environment
regardless of whether the library system is enabled. The library system adds
a *second* path for accessing the same primitives:

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

The library system is useful for:
- **Selective import**: Only pull in the bindings you need
- **Namespacing**: Prefix bindings to avoid collisions
- **R7RS compliance**: Standard library import syntax
- **Scheme library files**: Loading `.sld` files from disk

## Custom Library Names

Extensions can implement `registry.LibraryNamer` to override the default
`(wile <name>)` naming:

```go
type myExtension struct{}

func (e *myExtension) Name() string              { return "utils" }
func (e *myExtension) LibraryName() []string     { return []string{"myorg", "utils"} }
func (e *myExtension) AddToRegistry(r *registry.Registry) error {
    r.AddPrimitive(registry.PrimitiveSpec{
        Name: "helper", ParamCount: 0, Impl: primHelper,
    }, registry.PhaseRuntime)
    return nil
}
```

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

The `LibraryRegistry` searches for library files by converting the library
name to a path:

```
Library name     →  Search paths tried
(mylib greet)    →  /path/to/libs/mylib/greet.sld
                    /path/to/libs/mylib/greet.scm
                    ./mylib/greet.sld
                    ./mylib/greet.scm
                    ./stdlib/lib/mylib/greet.sld
                    ./stdlib/lib/mylib/greet.scm
```

User-supplied paths are searched first (in order), then the defaults
(`"."` and `"./stdlib/lib"`).

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
share mutable state with the caller. This is standard R7RS library semantics.

The `LibraryEnvFactory` creates these isolated environments:

```
Caller env ──► LibraryEnvFactory ──► Fresh env (same config)
                                         │
                                         ├─ Apply registry
                                         ├─ Register syntax compilers
                                         ├─ Register primitive expanders
                                         └─ Load bootstrap macros
```

## Complete Example

A self-contained example showing extension creation, library import, and
Scheme-side usage:

```go
package main

import (
    "context"
    "fmt"

    "github.com/aalpar/wile"
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
   `RegisterFunc`/`RegisterFuncs` go directly to the top-level environment
   and are not wrapped in a library.

4. **`LibraryEnvFactory` is per-`Namespace`.** Each engine stores
   its own factory via `SetLibraryEnvFactory`, so multiple engines can coexist
   safely without race conditions.

5. **Extensions with only compile-time bindings produce no library.** If an
   extension registers no `PhaseRuntime` primitives, no library is created
   for it.

6. **Cycle detection.** The `LibraryRegistry` detects circular imports and
   reports an error.
