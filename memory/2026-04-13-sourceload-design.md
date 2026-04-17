# sourceload — Isolated File Finding and Load Tracking

**Status**: Approved
**Date**: 2026-04-13
**Location**: `machine/compilation/sourceload/`

## Motivation

File-finding logic is currently mixed into the Scheme implementation across
`environment/` (LoadPathStack, ResolveFile), `resolver/` (four resolver types
with security/werr/environment dependencies), and `machine/compilation/`
(library name conversion, search-dir construction). The file-finding algorithm
itself — search directories in an `fs.FS` for a named path — has no inherent
dependency on Scheme.

Extracting this core into an isolated package achieves three goals:

1. **Clarity**: the file-finding algorithm is readable and testable without
   understanding the Scheme implementation.
2. **Reuse**: the package can be used by other tools in the workspace or
   eventually reimplemented in Scheme itself.
3. **Isolation**: Scheme-specific concerns (security authorization, werr error
   types, library name conversion, environment variables) stay in the adapter
   layer (`resolver/`), not in the finding logic.

## Package Overview

```
machine/compilation/sourceload/
├── doc.go          — package doc, ErrNotFound sentinel
├── finder.go       — Finder type: search + open
├── load_stack.go   — LoadStack type: file load tracking
├── walk.go         — Walk: file enumeration over fs.FS
└── *_test.go       — tests for all of the above
```

**Dependencies:** `io/fs`, `path`, `sync`, `errors`. Nothing else.

**What it does:**
- Searches an `fs.FS` across ordered directories for a named file
- Tracks which files are currently being loaded (stack)
- Walks an `fs.FS` to enumerate files matching a caller-provided filter
- Accepts an optional path canonicalizer for OS-style absolute paths

**What it does NOT do:**
- Security authorization (caller wraps)
- Error wrapping with project error types (returns stdlib errors)
- Library name conversion (caller's concern)
- Read environment variables (caller builds the search dir list)
- Know what ".sld" or ".scm" means (caller provides the filter)

## Types and API

### Finder

```go
type Finder struct {
    fsys         fs.FS
    searchDirs   []string
    stack        *LoadStack
    canonicalize func(string) string
}

type FinderOption func(*Finder)

func WithStack(s *LoadStack) FinderOption
func WithCanonicalize(fn func(string) string) FinderOption

func NewFinder(fsys fs.FS, searchDirs []string, opts ...FinderOption) *Finder

// Open searches for path and returns the first match.
// Search order: stack.CurrentDir() (if stack non-nil and non-empty),
// then each searchDir, then fs root (".").
// Returns (file, resolvedPath, error).
// The resolvedPath passes through canonicalize if set.
func (f *Finder) Open(path string) (fs.File, string, error)
```

### LoadStack

```go
type LoadStack struct { ... }  // thread-safe via sync.RWMutex

func NewLoadStack() *LoadStack
func (s *LoadStack) Push(path string)    // panics on empty string
func (s *LoadStack) Pop()                // no-op if empty
func (s *LoadStack) CurrentDir() string  // path.Dir of top, or ""
func (s *LoadStack) Current() string     // top path, or ""
func (s *LoadStack) Depth() int
```

`LoadStack` implements `environment.PathTracker` (see Integration below).

### Walk

```go
// Walk traverses fsys under each search directory, calling fn for every
// file where accept returns true. Hidden directories (leading ".") are
// skipped. Non-existent directories are silently skipped. Errors are
// accumulated via errors.Join and returned alongside partial results.
//
// No "." fallback — only walks directories explicitly provided.
// No deduplication — caller handles domain-specific identity.
func Walk(fsys fs.FS, searchDirs []string, accept func(name string) bool, fn func(relPath string)) error
```

`accept` receives the filename (not the full path). `fn` receives the
slash-separated path relative to the search directory.

### ErrNotFound

```go
var ErrNotFound = errors.New("sourceload: file not found")
```

Package-level sentinel. The `resolver/` adapter translates to
`werr.ErrFileNotFound` at the boundary.

## Resolution Algorithm

```
Open("lib/helper.scm")

1. Reject empty path → error

2. Build candidate dirs (ordered):
   a. stack.CurrentDir()     — if stack non-nil and CurrentDir() non-empty
   b. searchDirs[0], [1], …  — as provided to NewFinder
   c. "."                     — fs root, always last

3. For each dir:
   candidate := path.Join(dir, path)
   _, err := fs.Stat(fsys, candidate)
   if err == nil:
       f, err := fsys.Open(candidate)
       resolved := candidate
       if canonicalize != nil:
           resolved = canonicalize(candidate)
       return f, resolved, nil

4. Not found → return ErrNotFound
```

No absolute path handling inside the algorithm. The `fs.FS` spec requires
unrooted paths. Absolute paths are the OS adapter's concern.

## Integration with environment/

`environment/` gains a small interface so it never imports `sourceload/`:

```go
// PathTracker tracks the stack of files currently being loaded.
type PathTracker interface {
    Push(path string)
    Pop()
    CurrentDir() string
}
```

`Namespace` stores a `PathTracker`. `sourceload.LoadStack` implements it.
The full `LoadStack` API (`Current()`, `Depth()`) is available to callers
that hold the concrete type. The `resolver/` layer has the concrete type;
`environment/` only sees the interface.

## Integration with resolver/

The existing resolvers become thin adapters over `sourceload.Finder`:

**OSFileResolver:** Constructs a `Finder` with `os.DirFS("/")`, search dirs
built from `LibraryRegistry.GetSearchPaths()` + `SCHEME_INCLUDE_PATH` +
CWD, shared `LoadStack`, and `filepath.Abs` as canonicalizer. Handles
absolute paths before calling `finder.Open`. Wraps result with security
authorization and `werr` errors.

**FSFileResolver:** Constructs a `Finder` with the user's `fs.FS`, search
dirs from `LibraryRegistry.GetSearchPaths()`, shared `LoadStack`, no
canonicalizer. Wraps result with security authorization and `werr` errors.

**EmbedFileResolver:** Unchanged. Direct `fsys.Open(path)` — too simple
for a Finder.

**ChainFileResolver:** Unchanged structurally. Fall-through check adapts
from `werr.ErrFileNotFound` to `sourceload.ErrNotFound` (or the adapter
translates at the boundary).

**Walk wrappers** (`WalkOSSchemeFiles`, `WalkFSSchemeFiles`): Become thin
wrappers around `sourceload.Walk` that add security filtering to the
`accept` function.

## File Changes

### Created
| File | Contents |
|------|----------|
| `sourceload/doc.go` | Package doc, `ErrNotFound` sentinel |
| `sourceload/finder.go` | `Finder`, `FinderOption`, `NewFinder`, `Open` |
| `sourceload/load_stack.go` | `LoadStack` |
| `sourceload/walk.go` | `Walk` function |
| `sourceload/*_test.go` | Tests |

### Deleted
| File | Reason |
|------|--------|
| `environment/resolve.go` | Logic subsumed by `Finder.Open` |
| `environment/resolve_test.go` | Tests move to `sourceload/` |
| `environment/load_path_stack.go` | Type moves to `sourceload.LoadStack` |
| `environment/load_path_stack_test.go` | Tests move to `sourceload/` |

### Modified
| File | Change |
|------|--------|
| `environment/namespace.go` | Store `PathTracker` interface instead of `*LoadPathStack` |
| `environment/file_resolver.go` | Add `PathTracker` interface, update doc comments |
| `environment/environment_frame.go` | `LoadPathStack()` returns `PathTracker` |
| `resolver/os_file_resolver.go` | Use `sourceload.Finder` internally |
| `resolver/fs_file_resolver.go` | Use `sourceload.Finder` internally |
| `resolver/helpers.go` | Remove resolution logic; keep search-dir-list builder + Walk wrappers |
| `resolver/chain_file_resolver.go` | Error check adapts to `sourceload.ErrNotFound` |
| `engine.go` | Create `sourceload.NewLoadStack()` |
| `docs/design/SOURCE_LOADING.md` | Update architecture |

### Unchanged
| File | Reason |
|------|--------|
| `resolver/embed_file_resolver.go` | Too simple for Finder |
