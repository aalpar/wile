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

- `werr.ErrFileNotFound` or `fs.ErrNotExist` → fall through to the next resolver
- `security.ErrAccessDenied` → fall through only if a later resolver authorizes
  under a different source (virtual FS versus host OS, reported by
  `SourceGate.AuthorizedSource`); a resolver that authorizes nothing
  (`EmbedFileResolver`) ends the scan
- Any other error (I/O failure) → propagate immediately

This convention lets `ChainFileResolver` compose resolvers without
swallowing real errors.

## Resolver Implementations

```
┌─────────────────────────────────────────────────────────┐
│                   ChainFileResolver                     │
│  Tries each resolver in order; falls through on         │
│  not-found and on a denial a later source can revisit;  │
│  propagates all other errors.                           │
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
│  Serves includes in bootstrap sources (and, or, cond)   │
└─────────────────────────────────────────────────────────┘
```

### OSFileResolver

Resolves files from the OS filesystem. Resolution order:

1. `LoadStack.CurrentDir()` — the directory of the currently-loading
   file, enabling relative `include` paths from OS-loaded sources.
2. Library registry search paths (`LibraryRegistry.GetSearchPaths()`)
3. `SCHEME_INCLUDE_PATH` environment variable (colon-separated on Unix,
   semicolon-separated on Windows)
4. Current working directory

Each directory is made absolute and duplicates are dropped. There is no
filesystem-root fallback.

Absolute paths bypass the search list and are opened directly (still
subject to authorization). Every candidate path is authorized
(`code`/`load`, via `security.CheckWithAuthorizer`) before it is stat'd or
opened, so a sandboxed engine cannot probe a denied path for existence; a
denied candidate is skipped, and the denial is reported only if no later
candidate opens. When the authorizer reports a
`security.RootConfined` confinement root, the open itself goes through
`os.Root`, closing the TOCTOU gap between the check and the open
(`resolver/confined.go`).

### FSFileResolver

Resolves files from any `fs.FS` — typically an `embed.FS` holding the
standard library. Rejects absolute paths (virtual filesystems have no
concept of root-relative paths). Resolution order:

1. Relative to `LoadStack.CurrentDir()` — the directory of the
   currently-loading file, enabling relative `include` paths
2. Library registry search paths
3. Path as-is at FS root

Also authorizes every candidate before touching it, consistent with
`OSFileResolver`, but under `TargetSource` `security.SourceVirtualFS`, which
is what lets a chain look past a virtual-FS denial to the OS resolver.

### EmbedFileResolver

A minimal resolver backed by any `fs.FS`, with no path resolution and no
security checks. Used exclusively for bootstrap: the bootstrap macro and
procedure sources are registered on the registry as strings, and this
resolver over `core.BootstrapFS` serves any `(include ...)` inside them.
This resolver is never exposed to embedders and is not part of the chain.

### ChainFileResolver

Composes a list of `FileResolver`s into a single resolver. Tries each in
order. On not-found, proceeds to the next. On a denial, proceeds only under
the source rule above. On any other error, returns immediately.

```go
func (p *ChainFileResolver) ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error) {
    var lastErr error
    for i, r := range p.resolvers {
        f, resolved, err := r.ResolveAndOpen(ctx, path)
        if err == nil {
            return f, resolved, nil
        }
        if !p.continuesPast(err, i) {
            return nil, "", err // hard error, or a denial nothing later can revisit
        }
        lastErr = err
    }
    return nil, "", lastErr
}

func (p *ChainFileResolver) continuesPast(err error, i int) bool {
    if IsNotFound(err) {
        return true
    }
    if !errors.Is(err, security.ErrAccessDenied) {
        return false
    }
    refused, ok := p.resolvers[i].(SourceGate)
    if !ok {
        return false
    }
    for _, r := range p.resolvers[i+1:] {
        later, ok := r.(SourceGate)
        if !ok {
            return false // an ungated resolver would hand out the refused file
        }
        if later.AuthorizedSource() != refused.AuthorizedSource() {
            return true
        }
    }
    return false
}
```

The denial rule is what keeps `WithSourceFS(...)` + `WithSourceOS()` working
under a path-confining authorizer: a name absent from the `fs.FS` is refused
rather than reported missing, and that refusal says nothing about the host
file the OS resolver would serve.

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

Bootstrap macros (`and`, `or`, `cond`, etc.) always come from sources
embedded in `pkg/registry/core`, loaded with a separate `EmbedFileResolver`
over `core.BootstrapFS`. This resolver is
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
f, path, err := res.ResolveAndOpen(ctx, "scheme/base.sld")
if resolver.IsNotFound(err) {
    f, path, err = res.ResolveAndOpen(ctx, "scheme/base.scm")
}
```

This fallback logic lives in the library loader (`ResolveLibraryFile` in
`library_registry.go`), not in any resolver. Only absence of the `.sld`
licenses the `.scm`; a denial or I/O error propagates. The whole chain is
tried for `.sld` before any resolver is asked for `.scm`. Resolvers only see
opaque file paths.

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
exports 65 I/O and numeric primitives (`display`, `write`, `read`, `floor`, …)
that `Tiny` does not register. `(scheme base)` cannot satisfy its own export
list under `Tiny`, so it is not a loadable library there. (This is intended, not
a workaround target — see issue #801, resolved by-design.) The validation is
eager and reports all unsatisfied exports at once, naming the possible causes
(a typo in the export list, a profile that does not register the primitives, or
a binder introduced by a macro template, which is hygienically distinct from the
exported name).

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
│   └── 1.sld
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
    wile.WithLibraryPaths(),        // enables (import ...); required
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
│   1. LoadStack.CurrentDir() + path
│   2. LibraryRegistry search paths, each + path
│   3. FS root + path
│
└─ OSFileResolver (WithSourceOS)
    1. LoadStack.CurrentDir() + path
    2. LibraryRegistry search paths, each + path
    3. SCHEME_INCLUDE_PATH dirs, each + path
    4. CWD + path

Bootstrap: always from core.BootstrapFS via EmbedFileResolver (separate)
```

At each step, the first successful open wins. Not-found moves to the next
step. A denied candidate is skipped; a resolver that found nothing else
reports the denial, which the chain looks past only under the source rule
in [ChainFileResolver](#chainfileresolver). Any other error terminates the
search.

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
│  Imports: context, errors, io/fs, path, slices,     │
│  sync, werr. Zero Scheme knowledge.                 │
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
| `LoadStack` on ctx (`WithLoadStack`, `LoadStackFromContext`) | `pkg/machine/compilation/sourceload/context.go` |
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
