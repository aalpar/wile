# Source Loading Architecture

Scheme has three mechanisms that load source files: `include` (textual
inclusion), `load` (evaluate a file), and `import` (load a library by
name). All three need file resolution — mapping a path or library name to
an open file handle. Embedding adds a second dimension: virtual
filesystems backed by `embed.FS` or any `fs.FS`, where the "files" exist
only in memory. The source loading architecture unifies these concerns
behind a single `FileResolver` interface, composable into chains that
search multiple sources in priority order.

## FileResolver Interface

```go
type FileResolver interface {
    ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error)
}
```

A `FileResolver` takes a relative (or absolute) path and returns an open
`fs.File`, the resolved canonical path (used for load-path tracking and
error messages), and an error.

The chain protocol uses error identity to distinguish "not found here, try
the next resolver" from hard errors:

- `errors.Is(err, werr.ErrFileNotFound)` → fall through to the next resolver
- Any other error (security denial, I/O failure) → propagate immediately

This convention lets `ChainFileResolver` compose resolvers without
swallowing real errors.

## Resolver Implementations

```
┌─────────────────────────────────────────────────────────┐
│                   ChainFileResolver                     │
│  Tries each resolver in order; falls through on         │
│  ErrFileNotFound, propagates all other errors.          │
├──────────────────────┬──────────────────────────────────┤
│   FSFileResolver     │       OSFileResolver             │
│   (virtual fs.FS)    │       (OS filesystem)            │
│                      │                                  │
│  1. LoadPath dir     │  1. LoadPath dir                 │
│  2. Registry paths   │  2. Registry paths               │
│  3. FS root          │  3. SCHEME_INCLUDE_PATH          │
│                      │  4. CWD                          │
├──────────────────────┴──────────────────────────────────┤
│                  EmbedFileResolver                      │
│  Fixed bootstrap FS — not in the chain.                 │
│  Loads core macros (and, or, let, cond, etc.)           │
└─────────────────────────────────────────────────────────┘
```

### OSFileResolver

Resolves files from the OS filesystem. Resolution order:

1. `LoadPathStack.CurrentDir()` — the directory of the currently-loading
   file, enabling relative `include` paths from OS-loaded sources.
2. Library registry search paths (`LibraryRegistry.GetSearchPaths()`)
3. `SCHEME_INCLUDE_PATH` environment variable (colon-separated on Unix,
   semicolon-separated on Windows)
4. Current working directory
5. Filesystem root, as a last resort (mirroring `sourceload.Finder`'s `"."`
   fallback; not reported in the searched-dirs list on failure)

Absolute paths bypass the search list and are opened directly (still
subject to authorization). Before returning, runs a `code`/`load`
authorization check via `security.CheckWithAuthorizer`. This gates file
access in sandboxed engines. When the authorizer reports a
`security.RootConfined` confinement root, the open itself goes through
`os.Root`, closing the TOCTOU gap between the check and the open
(`resolver/confined.go`).

### FSFileResolver

Resolves files from any `fs.FS` — typically an `embed.FS` holding the
standard library. Rejects absolute paths (virtual filesystems have no
concept of root-relative paths). Resolution order:

1. Relative to `LoadPathStack.CurrentDir()` — the directory of the
   currently-loading file, enabling relative `include` paths
2. Library registry search paths
3. Path as-is at FS root

Also runs the security authorization check, consistent with
`OSFileResolver`.

### EmbedFileResolver

A minimal resolver backed by any `fs.FS`, with no path resolution and no
security checks. Used exclusively for bootstrap: loading `bootstrap.scm`
and its includes from `core.BootstrapFS`. This resolver is never exposed
to embedders and is not part of the chain.

### ChainFileResolver

Composes a list of `FileResolver`s into a single resolver. Tries each in
order. On `ErrFileNotFound`, proceeds to the next. On any other error,
returns immediately.

```go
func (p *ChainFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
    var lastErr error
    for _, r := range p.resolvers {
        f, resolved, err := r.ResolveAndOpen(ctx, path)
        if err == nil {
            return f, resolved, nil
        }
        if !errors.Is(err, werr.ErrFileNotFound) {
            return nil, "", err  // hard error — stop searching
        }
        lastErr = err
    }
    return nil, "", lastErr
}
```

## Engine Wiring

The `Engine` constructor builds the resolver chain from options. Each
`WithSourceFS` or `WithSourceOS` call appends a resolver factory to an
ordered list:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithSourceFS(stdlib.FS),  // FSFileResolver — searched first
    wile.WithSourceOS(),           // OSFileResolver — searched second
)
```

At engine init, the factories are instantiated and composed:

- **Zero options:** Default is `OSFileResolver` (backwards compatible).
- **One option:** Single resolver, no wrapping.
- **Multiple options:** Wrapped in `ChainFileResolver`, searched in call order.

The key rule: once any resolver option is specified, the implicit OS
default is suppressed. If you want OS access alongside a virtual FS, you
must explicitly add `WithSourceOS()`.

### Bootstrap Isolation

Bootstrap macros (`and`, `or`, `let`, `cond`, etc.) are always loaded from
`core.BootstrapFS` via a separate `EmbedFileResolver`. This resolver is
wired during `NewEngine` before the embedder-visible chain is configured,
and is never part of that chain. An embedder cannot accidentally shadow
bootstrap definitions by providing a virtual FS with conflicting paths.

## Library Import Resolution

When `(import (scheme base))` is evaluated, the library loader converts
the library name to a filesystem path:

```
(scheme base)  →  "scheme/base.sld"
(srfi 1)       →  "srfi/1.sld"
(chibi test)   →  "chibi/test.sld"
```

The loader tries `.sld` first, then `.scm` as a fallback:

```go
f, path, err := resolver.ResolveAndOpen(ctx, "scheme/base.sld")
if errors.Is(err, werr.ErrFileNotFound) {
    f, path, err = resolver.ResolveAndOpen(ctx, "scheme/base.scm")
}
```

This fallback logic lives in the library loader, not in any resolver.
Resolvers only see opaque file paths.

### Profiles Gate Which Standard Libraries Load

A security profile (`WithProfile`) is a statement about which primitives the
configuration registers. A standard library is loadable only if the active
profile can satisfy the library's *entire* export contract.

At library finalization the loader runs `validateLibraryExports`
(`pkg/machine/compilation/compile_library_forms.go`), which enforces R7RS §5.6:
every identifier in the library's export list must be defined or imported by the
library. If the profile does not register a primitive the library re-exports,
the library is invalid in that configuration and fails to load — **including
under a subset import**. For example, under the `Tiny` profile:

```scheme
(import (only (scheme base) car cons))   ; ERROR under Tiny
```

fails even though `car`/`cons` are available, because `(scheme base)` also
exports ~64 I/O and numeric primitives (`display`, `write`, `read`, `floor`, …)
that `Tiny` does not register. `(scheme base)` cannot satisfy its own export
list under `Tiny`, so it is not a loadable library there. (This is intended, not
a workaround target — see issue #801, resolved by-design.) The validation is
eager and reports all unsatisfied exports at once, naming both possible causes
(a typo in the export list, or a profile that does not register the primitives).

Two consequences for embedders:

- Under `Tiny`, core primitives such as `car`/`cons` are already bound in the
  bare top level — importing them from `(scheme base)` is neither necessary nor
  supported.
- To import a standard library, choose a profile that registers the primitives
  it exports (e.g. `Small` or `KitchenSink`). To layer `(import …)` over a bare
  top level whose extensions *are* registered, see `WithStrictNamespace()`.

**Security profile ≠ language standard.** The capability axis above (which
primitives a profile *exposes*) is orthogonal to the *language standard* the
engine implements. Wile is R7RS by default; selecting a different standard
(R5RS, or R6RS in future) is a separate startup concern, not something achieved
by subset-importing R7RS's `(scheme base)` under a restricted profile. Note that
`(scheme r5rs)` today is a re-export bundle layered *on top of* the R7RS core
(it imports `(scheme base)` et al.), so it requires a profile that provides those
primitives — it is not a non-R7RS baseline. The first-class standard selector is
`WithDialect(d)` (`pkg/wile/dialect.go`): it forks the R7RS-default forms
registry per engine and lets the dialect install, replace, or remove special
forms; a dialect that also implements `PrimitiveRemover` or
`BootstrapProcedureRewriter` reshapes the primitive and bootstrap layers too.
`DefaultDialect` is the R7RS baseline, applied when no dialect is given.

### Library Search Paths

The `LibraryRegistry` holds an ordered list of search paths. Default:

```go
var DefaultLibraryPaths = []string{"."}
```

Only the current directory is searched by default. The embedded standard
library is reached through the FileResolver chain (`WithSourceFS(stdlib.FS)`),
not through a search path. An earlier `"./pkg/stdlib/lib"` default was a
development-tree convenience that resolves nothing in a deployed binary.

`WithLibraryPaths(paths...)` prepends user-supplied paths before the
default. Calling it at all is what enables the library system: without it,
`(import ...)` raises a configuration error. Within each resolver, search
paths are tried in order before falling back to the FS root or CWD.

## Embedded Standard Library

The `pkg/stdlib/` package embeds the full R7RS standard library tree, in a
single `//go:embed lib`. The raw `embed.FS` is sub'd via
`fs.Sub(rawFS, "lib")` so that paths in the exported `FS` match the library
name convention directly (e.g., `"scheme/base.sld"`, not
`"lib/scheme/base.sld"`). This means `"."` in `DefaultLibraryPaths` resolves
libraries from the embedded FS without any path prefix gymnastics.

```go
package stdlib

import (
    "embed"
    "io/fs"
)

//go:embed lib
var rawFS embed.FS

var FS fs.FS         // = fs.Sub(rawFS, "lib") in init()
var LibFS fs.FS = rawFS  // "lib/" prefix retained
```

Both shapes are exported off the one embed. `wile.StdLibFS` re-exports
`stdlib.LibFS` (so `pkg/wile` needs no second embed of the same tree), which
means it keeps the `lib/` prefix and must be paired with
`WithLibraryPaths("lib")`. `WithSourceFS(stdlib.FS)` needs no extra search
path; `WithSourceFS(wile.StdLibFS)` alone resolves nothing.

The directory structure under `pkg/stdlib/lib/` mirrors the library name
hierarchy:

```
pkg/stdlib/lib/
├── scheme/
│   ├── base.sld
│   ├── write.sld
│   ├── char.sld
│   └── ...
├── chibi/
│   └── test.sld
├── srfi/
│   └── 1.scm
└── wile/
    ├── algebra.sld
    └── kanren.sld
```

Embedders get zero-configuration library support:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithProfile(wile.KitchenSink),
    wile.WithSourceFS(stdlib.FS),   // embedded libs
    wile.WithSourceOS(),            // user files on disk
)
```

With this configuration, `(import (scheme base))` resolves from the
embedded FS. User code on the OS filesystem can `include` and `import`
normally. If both the embedded FS and the OS have the same library, the
embedded FS wins (it was added first).

## Resolution Priority

For a given path, the full resolution order is:

```
ChainFileResolver (in WithSource* call order)
│
├─ FSFileResolver (WithSourceFS)
│   1. LoadPathStack.CurrentDir() + path
│   2. LibraryRegistry search paths, each + path
│   3. FS root + path
│
└─ OSFileResolver (WithSourceOS)
    1. LoadPathStack.CurrentDir() + path
    2. LibraryRegistry search paths, each + path
    3. SCHEME_INCLUDE_PATH dirs, each + path
    4. CWD + path

Bootstrap: always from core.BootstrapFS via EmbedFileResolver (separate)
```

At each step, the first successful open wins. `ErrFileNotFound` moves to
the next step. Any other error terminates the search.

### CLI Configuration

The CLI (`cmd/wile/main.go`) configures the full chain:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithProfile(wile.KitchenSink),
    wile.WithSourceFS(stdlib.FS),          // embedded standard library
    wile.WithSourceOS(),                   // user files on disk
    wile.WithLibraryPaths(libPaths...),    // -L flag + SCHEME_LIBRARY_PATH
)
```

`buildLibraryPaths()` merges `-L` command-line flags (highest priority)
with `SCHEME_LIBRARY_PATH` environment variable paths. These prepend to
the default search paths.

## Architecture Layers

```
┌─────────────────────────────────────────────────────┐
│  Scheme evaluation (machine/compilation/)           │
│  include, load, import → parse, expand, compile     │
└──────────────────────┬──────────────────────────────┘
                       │ calls ResolveAndOpen
┌──────────────────────▼──────────────────────────────┐
│  Resolver adapters (machine/compilation/resolver/)  │
│  Builds search-dir lists from Scheme-specific       │
│  sources (registry, env vars, CWD).                 │
│  Wraps with security authorization and werr errors. │
└──────────────────────┬──────────────────────────────┘
                       │ delegates to
┌──────────────────────▼──────────────────────────────┐
│  sourceload (machine/compilation/sourceload/)       │
│  Pure file finding: fs.FS + search dirs → open.     │
│  LoadStack for relative path tracking.              │
│  Walk for file enumeration.                         │
│  Dependencies: io/fs, path, sync, errors. Nothing   │
│  else — zero Scheme knowledge.                      │
└─────────────────────────────────────────────────────┘
```

## Code Locations

| Component | File |
|-----------|------|
| `Finder` (file search) | `pkg/machine/compilation/sourceload/finder.go` |
| `LoadStack` (load tracking) | `pkg/machine/compilation/sourceload/load_stack.go` |
| `Walk` (file enumeration) | `pkg/machine/compilation/sourceload/walk.go` |
| `ErrNotFound` sentinel | `pkg/machine/compilation/sourceload/doc.go` |
| `FileResolver` interface | `pkg/environment/file_resolver.go` |
| `PathTracker` interface | `pkg/environment/file_resolver.go` |
| `OSFileResolver` | `pkg/machine/compilation/resolver/os_file_resolver.go` |
| `FSFileResolver` | `pkg/machine/compilation/resolver/fs_file_resolver.go` |
| `EmbedFileResolver` | `pkg/machine/compilation/resolver/embed_file_resolver.go` |
| `ChainFileResolver` | `pkg/machine/compilation/resolver/chain_file_resolver.go` |
| Resolver aliases for older call sites | `pkg/machine/compilation/resolver_compat.go` |
| Sandbox path containment | `pkg/machine/compilation/resolver/confined.go` |
| Engine resolver wiring | `pkg/wile/engine.go` (`newFileResolver`) |
| Engine options | `pkg/wile/options.go` (`WithSourceFS`, `WithSourceOS`) |
| Library loader | `pkg/machine/compilation/library_loader.go` |
| Library registry / search paths | `pkg/machine/compilation/library_registry.go` |
| Embedded stdlib | `pkg/stdlib/stdlib.go` |
| CLI configuration | `cmd/wile/main.go` |
