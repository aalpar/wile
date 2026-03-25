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
│  1. LoadPath dir     │  1. LibraryRegistry paths        │
│  2. Registry paths   │  2. SCHEME_INCLUDE_PATH          │
│  3. FS root          │  3. CWD                          │
├──────────────────────┴──────────────────────────────────┤
│                  EmbedFileResolver                       │
│  Fixed bootstrap FS — not in the chain.                 │
│  Loads core macros (and, or, let, cond, etc.)           │
└─────────────────────────────────────────────────────────┘
```

### OSFileResolver

Resolves files from the OS filesystem. Resolution order:

1. Library registry search paths (`LibraryRegistry.GetSearchPaths()`)
2. `SCHEME_INCLUDE_PATH` environment variable (colon-separated on Unix,
   semicolon-separated on Windows)
3. Current working directory

Before opening, runs a security authorization check via
`security.CheckWithAuthorizer`. This gates file access in sandboxed
engines.

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

### Library Search Paths

The `LibraryRegistry` holds an ordered list of search paths. Default:

```go
var DefaultLibraryPaths = []string{".", "./stdlib/lib"}
```

`WithLibraryPaths(paths...)` prepends user-supplied paths before the
defaults. Within each resolver, search paths are tried in order before
falling back to the FS root or CWD.

## Embedded Standard Library

The `stdlib/` package embeds the full R7RS standard library tree:

```go
package stdlib

import "embed"

//go:embed lib
var FS embed.FS
```

The directory structure under `stdlib/lib/` mirrors the library name
hierarchy:

```
stdlib/lib/
├── scheme/
│   ├── base.sld
│   ├── write.sld
│   ├── char.sld
│   └── ...
├── chibi/
│   └── test.sld
├── srfi/
│   └── 1.sld
└── wile/
    ├── algebra.sld
    └── kanren.sld
```

Embedders get zero-configuration library support:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithAllExtensions(),
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
    1. LibraryRegistry search paths, each + path
    2. SCHEME_INCLUDE_PATH dirs, each + path
    3. CWD + path

Bootstrap: always from core.BootstrapFS via EmbedFileResolver (separate)
```

At each step, the first successful open wins. `ErrFileNotFound` moves to
the next step. Any other error terminates the search.

### CLI Configuration

The CLI (`cmd/wile/main.go`) configures the full chain:

```go
eng, err := wile.NewEngine(ctx,
    wile.WithAllExtensions(),
    wile.WithSourceFS(stdlib.FS),          // embedded standard library
    wile.WithSourceOS(),                   // user files on disk
    wile.WithLibraryPaths(libPaths...),    // -L flag + SCHEME_LIBRARY_PATH
)
```

`buildLibraryPaths()` merges `-L` command-line flags (highest priority)
with `SCHEME_LIBRARY_PATH` environment variable paths. These prepend to
the default search paths.

### Known Limitation

`cond-expand (library ...)` uses `os.Stat` directly and cannot detect
libraries in a virtual `fs.FS`. This requires threading `FileResolver`
into the `FeatureRequirement` interface.

## Code Locations

| Component | File |
|-----------|------|
| `FileResolver` interface | `machine/file_resolver.go` |
| `OSFileResolver` | `machine/file_resolver.go` |
| `FSFileResolver` | `machine/file_resolver.go` |
| `EmbedFileResolver` | `machine/file_resolver.go` |
| `ChainFileResolver` | `machine/file_resolver.go` |
| Engine resolver wiring | `engine.go` (`newFileResolver`) |
| Engine options | `options.go` (`WithSourceFS`, `WithSourceOS`) |
| Library loader | `machine/library_loader.go` |
| Library registry / search paths | `machine/library_registry.go` |
| Embedded stdlib | `stdlib/stdlib.go` |
| CLI configuration | `cmd/wile/main.go` |
