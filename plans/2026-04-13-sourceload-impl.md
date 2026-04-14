# sourceload — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract file-finding and load-stack tracking into an isolated `machine/compilation/sourceload/` package with zero project dependencies, then wire the existing resolvers as thin adapters.

**Architecture:** `sourceload/` provides `Finder` (search `fs.FS` + dirs), `LoadStack` (file load tracking), and `Walk` (enumeration). `environment/` gains a `PathTracker` interface (breaks import cycle). `resolver/` becomes a thin adapter adding security, `werr`, and search-dir-list construction.

**Tech Stack:** Go 1.24, `make lint`, `make covercheck`

**Design context:** `plans/2026-04-13-sourceload-design.md`

---

## Task 1: Create `sourceload/` package — LoadStack

Pure addition. No existing code changes.

**Files:**
- Create: `machine/compilation/sourceload/doc.go`
- Create: `machine/compilation/sourceload/load_stack.go`
- Create: `machine/compilation/sourceload/load_stack_test.go`

**Step 1: Create `doc.go`**

```go
// Package sourceload provides isolated file-finding and load-stack tracking.
//
// It searches an fs.FS across ordered directories for named files, tracks
// which files are currently being loaded (for relative path resolution),
// and walks an fs.FS to enumerate files matching a caller-provided filter.
//
// This package has zero project dependencies — it uses only io/fs, path,
// sync, and errors from the standard library.
package sourceload

import "errors"

// ErrNotFound is returned when a file cannot be found in any search directory.
var ErrNotFound = errors.New("sourceload: file not found")
```

**Step 2: Create `load_stack.go`**

```go
package sourceload

import (
	"path"
	"sync"
)

// LoadStack tracks files currently being loaded, providing relative path
// resolution via the current file's directory. Thread-safe via sync.RWMutex.
//
// The zero value is not usable; create with NewLoadStack.
type LoadStack struct {
	mu    sync.RWMutex
	paths []string
}

// NewLoadStack creates an empty load path stack.
func NewLoadStack() *LoadStack {
	return &LoadStack{
		paths: make([]string, 0, 8),
	}
}

// Push adds a path to the top of the stack.
// Panics if path is empty (programming error — resolved paths are never empty).
func (p *LoadStack) Push(filePath string) {
	if filePath == "" {
		panic("sourceload: Push called with empty path")
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.paths = append(p.paths, filePath)
}

// Pop removes the top path from the stack.
// Does nothing if the stack is empty. This silent behavior supports
// defer patterns where the depth cannot be checked before popping.
func (p *LoadStack) Pop() {
	p.mu.Lock()
	defer p.mu.Unlock()
	if len(p.paths) > 0 {
		p.paths = p.paths[:len(p.paths)-1]
	}
}

// Current returns the path at the top of the stack without removing it.
// Returns empty string if the stack is empty.
func (p *LoadStack) Current() string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	if len(p.paths) == 0 {
		return ""
	}
	return p.paths[len(p.paths)-1]
}

// CurrentDir returns the directory of the path at the top of the stack.
// Returns empty string if the stack is empty.
// Uses slash-separated path semantics (path.Dir, not filepath.Dir).
func (p *LoadStack) CurrentDir() string {
	current := p.Current()
	if current == "" {
		return ""
	}
	return path.Dir(current)
}

// Depth returns the number of paths on the stack.
func (p *LoadStack) Depth() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.paths)
}
```

**Step 3: Write tests in `load_stack_test.go`**

Test cases:
- Push + Current: single path, verify Current returns it
- Push + Pop + Current: push two, pop one, verify top
- CurrentDir: push `"lib/scheme/base.sld"`, verify `"lib/scheme"`
- CurrentDir empty: empty stack returns `""`
- Depth: push 3, verify 3; pop 1, verify 2
- Pop on empty: no panic
- Push empty panics: `assert.Panics` on `Push("")`
- Thread safety: concurrent Push/Pop doesn't race (run with `-race`)

**Step 4: Run tests**

Run: `go test -v -race ./machine/compilation/sourceload/...`
Expected: PASS

**Step 5: Commit**

```
feat: add machine/compilation/sourceload package with LoadStack

Zero-dependency file load stack tracking. Uses only io/fs, path, sync,
and errors. Panics on empty Push (programming error, not runtime).
```

---

## Task 2: Create `sourceload/` — Finder

**Files:**
- Create: `machine/compilation/sourceload/finder.go`
- Create: `machine/compilation/sourceload/finder_test.go`

**Step 1: Create `finder.go`**

```go
package sourceload

import (
	"io/fs"
	"path"
)

// Finder searches an fs.FS for files across ordered directories.
type Finder struct {
	fsys         fs.FS
	searchDirs   []string
	stack        *LoadStack
	canonicalize func(string) string
}

// FinderOption configures a Finder.
type FinderOption func(*Finder)

// WithStack attaches a LoadStack. When non-nil, the stack's CurrentDir
// is prepended to the search order in Open.
func WithStack(s *LoadStack) FinderOption {
	return func(f *Finder) {
		f.stack = s
	}
}

// WithCanonicalize sets a function to transform resolved paths before
// returning them. For OS filesystems, this is typically filepath.Abs.
func WithCanonicalize(fn func(string) string) FinderOption {
	return func(f *Finder) {
		f.canonicalize = fn
	}
}

// NewFinder creates a Finder that searches fsys across the given
// directories. Panics if fsys is nil.
func NewFinder(fsys fs.FS, searchDirs []string, opts ...FinderOption) *Finder {
	if fsys == nil {
		panic("sourceload: NewFinder called with nil fs.FS")
	}
	q := &Finder{
		fsys:       fsys,
		searchDirs: searchDirs,
	}
	for _, opt := range opts {
		opt(q)
	}
	return q
}

// Open searches for path and returns the first match.
//
// Search order:
//  1. stack.CurrentDir() — if stack is non-nil and CurrentDir is non-empty
//  2. Each searchDir in order
//  3. fs root "."
//
// Returns the open file, the resolved path (passed through canonicalize
// if set), and any error. Returns ErrNotFound if no match.
func (f *Finder) Open(filePath string) (fs.File, string, error) {
	if filePath == "" {
		return nil, "", ErrNotFound
	}

	dirs := f.buildSearchDirs()
	for _, dir := range dirs {
		candidate := path.Join(dir, filePath)
		if !fs.ValidPath(candidate) {
			continue
		}
		_, err := fs.Stat(f.fsys, candidate)
		if err != nil {
			continue
		}
		file, err := f.fsys.Open(candidate)
		if err != nil {
			continue
		}
		resolved := candidate
		if f.canonicalize != nil {
			resolved = f.canonicalize(candidate)
		}
		return file, resolved, nil
	}

	return nil, "", ErrNotFound
}

// buildSearchDirs returns the ordered list of directories to search.
func (f *Finder) buildSearchDirs() []string {
	var dirs []string
	if f.stack != nil {
		d := f.stack.CurrentDir()
		if d != "" && d != "." {
			dirs = append(dirs, d)
		}
	}
	dirs = append(dirs, f.searchDirs...)
	dirs = append(dirs, ".")
	return dirs
}
```

**Step 2: Write tests in `finder_test.go`**

Use `testing/fstest.MapFS` for all tests — no OS filesystem needed.

Test cases:
- **Basic find**: file at `"lib/foo.scm"`, searchDirs `["lib"]`, `Open("foo.scm")` succeeds
- **Search order**: same file in two dirs, first dir wins
- **Stack CurrentDir**: push `"lib/bar.scm"` onto stack, `Open("foo.scm")` finds `"lib/foo.scm"`
- **FS root fallback**: no search dirs, file at root `"foo.scm"`, `Open("foo.scm")` succeeds
- **Not found**: returns `ErrNotFound`
- **Empty path**: returns `ErrNotFound`
- **Canonicalize**: set canonicalize to `strings.ToUpper`, verify returned path is uppercased
- **Nil stack**: no panic, skips stack dir
- **Invalid path**: candidate with `..` or absolute path skipped via `fs.ValidPath`

**Step 3: Run tests**

Run: `go test -v ./machine/compilation/sourceload/...`
Expected: PASS

**Step 4: Commit**

```
feat: add sourceload.Finder for fs.FS-based file search

Searches ordered directories within an fs.FS. Supports optional
LoadStack for relative path resolution and optional path canonicalizer
for OS-style absolute paths.
```

---

## Task 3: Create `sourceload/` — Walk

**Files:**
- Create: `machine/compilation/sourceload/walk.go`
- Create: `machine/compilation/sourceload/walk_test.go`

**Step 1: Create `walk.go`**

```go
package sourceload

import (
	"errors"
	"io/fs"
	"strings"
)

// Walk traverses fsys under each search directory, calling fn for every
// file where accept returns true. Hidden directories (name starting with
// ".") are skipped. Non-existent directories are silently skipped.
//
// accept receives the filename (not the full path).
// fn receives the slash-separated path relative to the search directory.
//
// No "." fallback — only walks directories explicitly provided.
// No deduplication — caller handles domain-specific identity.
// Errors are accumulated via errors.Join and returned alongside partial results.
func Walk(fsys fs.FS, searchDirs []string, accept func(name string) bool, fn func(relPath string)) error {
	var walkErrs []error
	for _, dir := range searchDirs {
		err := walkDir(fsys, dir, accept, fn)
		if err != nil {
			walkErrs = append(walkErrs, err)
		}
	}
	return errors.Join(walkErrs...)
}

// walkDir walks a single directory in fsys.
func walkDir(fsys fs.FS, baseDir string, accept func(string) bool, fn func(string)) error {
	prefix := baseDir
	if prefix == "." {
		prefix = ""
	}
	return fs.WalkDir(fsys, baseDir, func(filePath string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return fs.SkipDir
			}
			if filePath != baseDir && isHidden(d.Name()) {
				return fs.SkipDir
			}
			return nil
		}
		if walkErr != nil || !accept(d.Name()) {
			return nil //nolint:nilerr
		}
		relPath := filePath
		if prefix != "" {
			relPath = strings.TrimPrefix(filePath, prefix+"/")
		}
		fn(relPath)
		return nil
	})
}

// isHidden reports whether the name starts with ".".
func isHidden(name string) bool {
	return len(name) > 0 && name[0] == '.'
}
```

**Step 2: Write tests in `walk_test.go`**

Use `testing/fstest.MapFS`.

Test cases:
- **Basic walk**: 3 `.sld` files, 1 `.txt` file; accept filters `.sld`; fn called 3 times
- **Hidden dirs skipped**: `.hidden/lib.sld` not visited
- **Multiple search dirs**: `["a", "b"]` each with files; fn called for both
- **Non-existent dir**: silently skipped, no error
- **No "." fallback**: files at root not found unless `"."` is in searchDirs
- **relPath is relative to search dir**: file at `"lib/scheme/base.sld"` with searchDir `"lib"` yields `"scheme/base.sld"`

**Step 3: Run tests**

Run: `go test -v ./machine/compilation/sourceload/...`
Expected: PASS

**Step 4: Commit**

```
feat: add sourceload.Walk for fs.FS file enumeration

Walks search directories calling accept filter on filenames. Hidden
dirs skipped. No dedup. Returns paths relative to each search dir.
```

---

## Task 4: Add `PathTracker` interface to `environment/`

**Files:**
- Modify: `environment/file_resolver.go`

**Step 1: Add the interface**

Add after the `LibrarySearcher` interface:

```go
// PathTracker tracks the stack of files currently being loaded.
// Implementations provide relative path resolution for include/load
// and load provenance introspection.
//
// The concrete implementation is sourceload.LoadStack. This interface
// is defined here so environment/ can store it without importing
// machine/compilation/sourceload/.
type PathTracker interface {
	Push(path string)
	Pop()
	Current() string
	CurrentDir() string
	Depth() int
}
```

**Step 2: Run tests**

Run: `go test ./environment/...`
Expected: PASS (additive change only)

**Step 3: Commit**

```
feat: add PathTracker interface to environment package

Breaks the import cycle so environment/ can store a load-path tracker
without depending on machine/compilation/sourceload/.
```

---

## Task 5: Migrate `Namespace` to `PathTracker` + `sourceload.LoadStack`

This is the central wiring change. All callers that access the load path
stack go through `PathTracker` now.

**Files:**
- Modify: `environment/namespace.go`
- Modify: `environment/environment_frame.go`
- Modify: `environment/namespace_test.go`
- Modify: `engine.go`

**Step 1: Update `Namespace` struct and accessors**

In `environment/namespace.go`:

Change the field:
```go
// Before:
loadPathStack *LoadPathStack

// After:
loadPathStack PathTracker
```

Change `NewNamespace()`:
```go
// Before:
loadPathStack: NewLoadPathStack(),

// After:
// loadPathStack left nil — must be set by the engine via SetLoadPathStack.
// This breaks the dependency: environment/ no longer creates the concrete type.
```

Add setter:
```go
// SetLoadPathStack sets the load path tracker. Called by engine initialization.
func (p *Namespace) SetLoadPathStack(s PathTracker) {
	p.loadPathStack = s
}
```

Change accessor return type:
```go
// Before:
func (p *Namespace) LoadPathStack() *LoadPathStack {

// After:
func (p *Namespace) LoadPathStack() PathTracker {
```

In `environment/environment_frame.go`:

Change:
```go
// Before:
func (p *EnvironmentFrame) LoadPathStack() *LoadPathStack {

// After:
func (p *EnvironmentFrame) LoadPathStack() PathTracker {
```

**Step 2: Update `engine.go`**

In `NewEngine`, after `NewNamespace()`, add:
```go
ns.SetLoadPathStack(sourceload.NewLoadStack())
```

Add import `"github.com/aalpar/wile/machine/compilation/sourceload"`.

Update `Engine.PushLoadPath` — `Push` no longer returns an error (it panics on empty). Guard with an explicit check for the public API:

```go
func (p *Engine) PushLoadPath(filePath string) error {
	stack := p.namespace.LoadPathStack()
	if stack == nil {
		return nil
	}
	if filePath == "" {
		return werr.WrapForeignErrorf(werr.ErrInvalidLoadPath, "path must not be empty")
	}
	stack.Push(filePath)
	return nil
}
```

**Step 3: Update callers that handled Push errors**

Three call sites called `stack.Push(resolvedPath)` and checked the error.
After a successful `ResolveAndOpen`, `resolvedPath` is never empty, so the
error check was purely defensive. With panic-on-empty, remove the checks:

In `compile_time_continuation_include.go:69-76`:
```go
// Before:
stack := p.env.LoadPathStack()
if stack != nil {
	pushErr := stack.Push(filePath)
	if pushErr != nil {
		return werr.WrapForeignErrorf(pushErr, "include: push load path for %q", fn.Value)
	}
	defer stack.Pop()
}

// After:
stack := p.env.LoadPathStack()
if stack != nil {
	stack.Push(filePath)
	defer stack.Pop()
}
```

Same pattern in `library_loader.go:105-112` and `prim_eval.go:131-136`.

**Step 4: Update namespace tests**

In `environment/namespace_test.go`, the tests call `stack.Push(path)` and
check `err`. Update to just call `stack.Push(path)` (no return value).
Tests that checked `c.Assert(err, qt.IsNil)` after Push: remove the assert.

**Step 5: Run full test suite**

Run: `make lint && go test ./...`
Expected: PASS

**Step 6: Commit**

```
refactor: migrate Namespace to PathTracker interface

Namespace stores PathTracker (interface) instead of *LoadPathStack
(concrete). Engine creates sourceload.NewLoadStack() at init.
Push no longer returns error — panics on empty (programming error).
```

---

## Task 6: Wire `OSFileResolver` to use `sourceload.Finder`

**Files:**
- Modify: `machine/compilation/resolver/os_file_resolver.go`

**Step 1: Rewrite OSFileResolver**

```go
type OSFileResolver struct {
	env    *environment.EnvironmentFrame
	finder *sourceload.Finder
}

func NewOSFileResolver(env *environment.EnvironmentFrame) *OSFileResolver {
	return &OSFileResolver{
		env: env,
	}
}
```

The `finder` is lazily created on first `ResolveAndOpen` call (search dirs
depend on runtime state: CWD, SCHEME_INCLUDE_PATH, registry paths).

```go
func (p *OSFileResolver) ResolveAndOpen(_ context.Context, filePath string) (fs.File, string, error) {
	if filePath == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}

	// Absolute path: handle directly on OS, bypass Finder.
	if filepath.IsAbs(filePath) {
		return openAbsoluteAuthorized(p.env.Namespace().Authorizer(), filePath)
	}

	finder := p.makeFinder()
	f, resolved, err := finder.Open(filePath)
	if err != nil {
		if errors.Is(err, sourceload.ErrNotFound) {
			return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "file %q not found", filePath)
		}
		return nil, "", err
	}

	// Security check on the resolved path.
	auth := p.env.Namespace().Authorizer()
	if secErr := checkAuth(auth, resolved); secErr != nil {
		f.Close()
		return nil, "", secErr
	}

	return f, resolved, nil
}
```

`makeFinder` builds a `sourceload.Finder` with `os.DirFS("/")`, the OS search
dirs (from `osSearchDirs`), the shared `LoadStack` (type-asserted from
`PathTracker`), and `filepath.Abs` as canonicalizer.

`openAbsoluteAuthorized` handles the absolute-path special case: `os.Stat`,
security check, `os.Open`. This is the one path that doesn't go through
`fs.FS` (absolute paths are invalid in `fs.FS`).

**Step 2: Run resolver tests**

Run: `go test -v ./machine/compilation/resolver/...`
Expected: PASS

**Step 3: Commit**

```
refactor: wire OSFileResolver to use sourceload.Finder

Absolute paths handled directly. Relative paths go through Finder
with os.DirFS("/") and filepath.Abs canonicalizer. Security check
applied after resolution.
```

---

## Task 7: Wire `FSFileResolver` to use `sourceload.Finder`

**Files:**
- Modify: `machine/compilation/resolver/fs_file_resolver.go`

**Step 1: Rewrite FSFileResolver**

```go
type FSFileResolver struct {
	fsys fs.FS
	env  *environment.EnvironmentFrame
}
```

`ResolveAndOpen`:
```go
func (p *FSFileResolver) ResolveAndOpen(_ context.Context, filePath string) (fs.File, string, error) {
	if filePath == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	if filepath.IsAbs(filePath) {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
			"resolve: absolute paths not supported in virtual filesystem: %s", filePath)
	}

	finder := p.makeFinder()
	f, resolved, err := finder.Open(filePath)
	if err != nil {
		if errors.Is(err, sourceload.ErrNotFound) {
			return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
				"file %q not found in virtual filesystem", filePath)
		}
		return nil, "", err
	}

	auth := p.env.Namespace().Authorizer()
	if secErr := checkAuth(auth, resolved); secErr != nil {
		f.Close()
		return nil, "", secErr
	}

	return f, resolved, nil
}
```

`makeFinder` builds a `sourceload.Finder` with `p.fsys`, search dirs from
`LibraryRegistry.GetSearchPaths()`, the shared `LoadStack`, no canonicalizer.

**Step 2: Update `EnumerateFiles` to use `sourceload.Walk`**

```go
func (p *FSFileResolver) EnumerateFiles() ([]string, error) {
	auth := p.env.Namespace().Authorizer()
	var result []string

	searchPaths := p.fsDirs()
	err := sourceload.Walk(p.fsys, searchPaths, isSchemeFile, func(relPath string) {
		if isAuthorized(auth, relPath) {
			result = append(result, relPath)
		}
	})

	return result, err
}
```

Note: `EnumerateFiles` adds the `"."` root walk and dedup-by-searchpath logic
from the current implementation. Adapt as needed to preserve existing behavior.

**Step 3: Run tests**

Run: `go test -v ./machine/compilation/resolver/...`
Expected: PASS

**Step 4: Commit**

```
refactor: wire FSFileResolver to use sourceload.Finder and Walk

Resolution delegates to sourceload.Finder. Enumeration delegates to
sourceload.Walk with security filtering in the adapter.
```

---

## Task 8: Update `ChainFileResolver` error check

**Files:**
- Modify: `machine/compilation/resolver/chain_file_resolver.go`

**Step 1: Update fall-through check**

The chain resolver currently checks `werr.ErrFileNotFound` for fall-through.
Since `OSFileResolver` and `FSFileResolver` now translate `sourceload.ErrNotFound`
to `werr.ErrFileNotFound` at the adapter boundary, the chain resolver check
is **unchanged**. Verify this is correct by reading the updated resolver code.

If the adapters pass through `sourceload.ErrNotFound` instead of translating,
update the chain check:

```go
// If adapters translate:  (preferred — chain stays werr-aware)
if !errors.Is(err, werr.ErrFileNotFound) {

// If adapters pass through:  (chain must know about sourceload)
if !errors.Is(err, werr.ErrFileNotFound) && !errors.Is(err, sourceload.ErrNotFound) {
```

**Step 2: Run tests**

Run: `go test -v ./machine/compilation/resolver/...`
Expected: PASS

**Step 3: Commit (if changes needed)**

```
fix: update ChainFileResolver error check for sourceload
```

---

## Task 9: Delete old code

**Files:**
- Delete: `environment/resolve.go`
- Delete: `environment/resolve_test.go`
- Delete: `environment/load_path_stack.go`
- Delete: `environment/load_path_stack_test.go`
- Modify: `environment/file_resolver.go` (update doc comments)
- Modify: `resolver/helpers.go` (remove duplicated resolution logic if any remains)

**Step 1: Delete files**

Remove the four files. All their functionality is now in `sourceload/` or
handled by `Finder`.

**Step 2: Fix any compile errors**

`environment/namespace.go` no longer references `NewLoadPathStack` (removed
in Task 5). Verify no other references to deleted types remain:

Run: `go build ./...`

**Step 3: Update doc comment in `environment/file_resolver.go`**

Replace references to `LoadPathStack` with `PathTracker`.

**Step 4: Run full suite**

Run: `make lint && make covercheck`
Expected: PASS

**Step 5: Commit**

```
refactor: delete environment/resolve.go and load_path_stack.go

Logic moved to sourceload.Finder (resolution) and sourceload.LoadStack
(stack tracking). environment/ now uses PathTracker interface.
```

---

## Task 10: Update docs and plan status

**Files:**
- Modify: `docs/design/SOURCE_LOADING.md`
- Modify: `plans/CLAUDE.md`
- Modify: `environment/CLAUDE.local.md` (remove LoadPathStack references from type docs)

**Step 1: Update `SOURCE_LOADING.md`**

Add a section describing the `sourceload/` layer beneath the resolver layer.
Update the architecture diagram to show three layers: sourceload → resolver → engine.
Update the "Code Locations" table. Remove the "Known Limitation" about
`cond-expand` using `os.Stat` directly (already fixed in PR #645).

**Step 2: Update `plans/CLAUDE.md`**

Mark `2026-04-13-sourceload-design.md` status as **Complete**.
Add `2026-04-13-sourceload-impl.md` to completed plans table.

**Step 3: Update `environment/CLAUDE.local.md`**

Update type documentation: `LoadPathStack` → `PathTracker` interface.
Note that the concrete implementation is `sourceload.LoadStack`.

**Step 4: Commit**

```
docs: update SOURCE_LOADING.md and plan status for sourceload extraction
```

---

## Execution Summary

| Task | Description | Effort | Dependencies |
|------|-------------|--------|--------------|
| 1 | sourceload/LoadStack | S | — |
| 2 | sourceload/Finder | M | 1 |
| 3 | sourceload/Walk | S | — |
| 4 | PathTracker interface in environment/ | S | — |
| 5 | Migrate Namespace to PathTracker + sourceload.LoadStack | M | 1, 4 |
| 6 | Wire OSFileResolver to Finder | M | 2, 5 |
| 7 | Wire FSFileResolver to Finder + Walk | M | 2, 3, 5 |
| 8 | Update ChainFileResolver error check | S | 6, 7 |
| 9 | Delete old code | S | 8 |
| 10 | Update docs and plan status | S | 9 |

**Parallelizable:** Tasks 1+3+4 have no dependencies on each other. Tasks 6+7 can parallelize after 5 completes.

**Total: 10 commits.** Each task is one commit with tests passing.

## Open Questions

- **`LoadStack.CurrentDir` uses `path.Dir` (slash-separated).** The old `LoadPathStack.CurrentDir` used `filepath.Dir` for absolute paths and `path.Dir` for relative. Since the `sourceload` package works exclusively with `fs.FS` paths (unrooted, slash-separated), `path.Dir` is correct. The OS resolver's canonicalizer (`filepath.Abs`) produces paths that `path.Dir` handles correctly on Unix. On Windows, this may need attention — but Wile doesn't currently target Windows.

- **`os.DirFS("/")` path translation.** `os.DirFS("/")` expects paths like `"usr/local/lib/foo.scm"` (no leading slash). The `osSearchDirs` function currently returns absolute paths like `"/usr/local/lib"`. These must be stripped of the leading `/` before being passed as search dirs to the Finder. The canonicalizer restores the absolute form on the way out.
