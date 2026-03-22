# fs.FS Source Loading — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Route all source file loading (include, load, library import) through `fs.FS` so embedders can provide virtual filesystems.

**Architecture:** Add `FSFileResolver` alongside existing `OSFileResolver`. Route library loading through `FileResolver` (currently bypassed). Relax `LoadPathStack` to accept relative paths for FS-resolved files. Wire via `WithSourceFS(fs.FS)` engine option.

**Tech Stack:** Go `io/fs`, `path` (not `filepath` for FS paths), `testing/fstest.MapFS`

**Design doc:** `plans/FS-SOURCE-LOADING.md`

---

## Task 1: Relax `LoadPathStack` to Accept Relative Paths

The load path stack currently rejects relative paths. `FSFileResolver` returns
relative paths (all `fs.FS` paths are relative to FS root). Without this change,
include/load within a virtual FS can't track the current file for relative resolution.

**Files:**
- Modify: `environment/load_path_stack.go`
- Modify: `environment/load_path_stack_test.go`

**Step 1: Write tests for relative path support**

Add to `environment/load_path_stack_test.go`:

```go
func TestLoadPathStack_RelativePaths(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	// Relative paths should be accepted (for fs.FS-resolved paths)
	c.Assert(stack.Push("lib/math.sld"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "lib/math.sld")
	c.Assert(stack.CurrentDir(), qt.Equals, "lib")
	c.Assert(stack.Depth(), qt.Equals, 1)

	// Nested relative include
	c.Assert(stack.Push("lib/impl.scm"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "lib/impl.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "lib")
	c.Assert(stack.Depth(), qt.Equals, 2)

	stack.Pop()
	c.Assert(stack.Current(), qt.Equals, "lib/math.sld")

	stack.Pop()
	c.Assert(stack.Depth(), qt.Equals, 0)
}

func TestLoadPathStack_MixedAbsoluteAndRelative(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	c.Assert(stack.Push("/app/main.scm"), qt.IsNil)
	c.Assert(stack.Push("lib/helper.scm"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "lib/helper.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "lib")

	stack.Pop()
	c.Assert(stack.Current(), qt.Equals, "/app/main.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "/app")
}

func TestLoadPathStack_CurrentDir_RelativeRootFile(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	// File at FS root — CurrentDir should return "."
	c.Assert(stack.Push("main.scm"), qt.IsNil)
	c.Assert(stack.CurrentDir(), qt.Equals, ".")
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestLoadPathStack_Relative|TestLoadPathStack_Mixed|TestLoadPathStack_CurrentDir_Relative' ./environment/...`

Expected: FAIL — `Push` returns `ErrInvalidLoadPath` for relative paths.

**Step 3: Update `LoadPathStack`**

In `environment/load_path_stack.go`:

1. In `Push`: remove the `filepath.IsAbs` guard. Accept all non-empty paths.

```go
func (s *LoadPathStack) Push(p string) error {
	if p == "" {
		return werr.WrapForeignErrorf(werr.ErrInvalidLoadPath, "path must not be empty")
	}

	s.mu.Lock()
	defer s.mu.Unlock()

	s.paths = append(s.paths, p)
	return nil
}
```

2. In `CurrentDir`: use `path.Dir` for relative paths, `filepath.Dir` for absolute.

```go
import "path"

func (s *LoadPathStack) CurrentDir() string {
	current := s.Current()
	if current == "" {
		return ""
	}
	if filepath.IsAbs(current) {
		return filepath.Dir(current)
	}
	return path.Dir(current)
}
```

3. Update the `paths` field comment from `// absolute paths only` to
`// absolute or relative paths; top = paths[len-1]`.

4. Update the `Push` doc comment to reflect that both absolute and relative
paths are accepted. Remove the "Returns a wrapped ErrInvalidLoadPath" sentence
about non-absolute paths.

**Step 4: Update the existing relative-path-rejection test**

`TestLoadPathStack_PushRelativePathReturnsError` now tests the wrong thing —
relative paths should succeed. Rename it to `TestLoadPathStack_PushEmptyPathReturnsError`
and only test the empty-string case:

```go
func TestLoadPathStack_PushEmptyPathReturnsError(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	err := stack.Push("")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrInvalidLoadPath), qt.IsTrue)
}
```

**Step 5: Run all `LoadPathStack` tests**

Run: `go test -v -run TestLoadPathStack ./environment/...`

Expected: all PASS.

**Step 6: Run `make lint`**

Run: `make lint`

Expected: PASS (no formatting or import issues).

**Step 7: Commit**

```
feat: allow relative paths in LoadPathStack

Supports fs.FS-resolved paths which are relative to the FS root.
Previously only absolute paths were accepted, which prevented
virtual filesystem includes from tracking the current file.
```

---

## Task 2: Add `FSFileResolver`

**Files:**
- Modify: `machine/file_resolver.go`
- Modify: `machine/file_resolver_test.go`

**Step 1: Write tests for `FSFileResolver`**

Add to `machine/file_resolver_test.go`:

```go
// --- FSFileResolver ---

func TestFSFileResolver_EmptyPath(t *testing.T) {
	fsys := fstest.MapFS{"a.scm": {Data: []byte("1")}}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

func TestFSFileResolver_RejectsAbsolutePath(t *testing.T) {
	fsys := fstest.MapFS{"a.scm": {Data: []byte("1")}}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "/abs/path.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
	qt.Assert(t, err.Error(), qt.Contains, "absolute")
}

func TestFSFileResolver_DirectPath(t *testing.T) {
	fsys := fstest.MapFS{"hello.scm": {Data: []byte("42")}}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "hello.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "hello.scm")
}

func TestFSFileResolver_RelativeToLoadPathStack(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/main.sld":  {Data: []byte("(define-library (main))")},
		"lib/helper.scm": {Data: []byte("42")},
	}
	env := environment.NewNamespace().Runtime()
	stack := env.LoadPathStack()
	qt.Assert(t, stack.Push("lib/main.sld"), qt.IsNil)
	defer stack.Pop()

	r := NewFSFileResolver(fsys, env)

	// "helper.scm" should resolve relative to "lib/" (current load dir)
	f, resolved, err := r.ResolveAndOpen(context.Background(), "helper.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "lib/helper.scm")
}

func TestFSFileResolver_ViaSearchPaths(t *testing.T) {
	fsys := fstest.MapFS{
		"vendor/util.scm": {Data: []byte("99")},
	}
	env := environment.NewNamespace().Runtime()
	libReg := NewLibraryRegistry()
	libReg.PrependSearchPath("vendor")
	env.SetLibraryRegistry(libReg)

	r := NewFSFileResolver(fsys, env)

	f, resolved, err := r.ResolveAndOpen(context.Background(), "util.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, resolved, qt.Equals, "vendor/util.scm")
}

func TestFSFileResolver_NotFound(t *testing.T) {
	fsys := fstest.MapFS{}
	env := environment.NewNamespace().Runtime()
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "missing.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrFileNotFound), qt.IsTrue)
}

func TestFSFileResolver_SecurityDenied(t *testing.T) {
	fsys := fstest.MapFS{"secret.scm": {Data: []byte("(launch-missiles)")}}
	env := environment.NewNamespace().Runtime()
	env.Namespace().SetAuthorizer(security.DenyAll())
	r := NewFSFileResolver(fsys, env)

	_, _, err := r.ResolveAndOpen(context.Background(), "secret.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, security.ErrAccessDenied), qt.IsTrue)
}

func TestFSFileResolver_InterfaceCompliance(t *testing.T) {
	var _ FileResolver = (*FSFileResolver)(nil)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestFSFileResolver ./machine/...`

Expected: FAIL — `NewFSFileResolver` undefined.

**Step 3: Implement `FSFileResolver`**

Add to `machine/file_resolver.go`:

```go
import (
	"path"
	// ... existing imports
)

// FSFileResolver resolves files from a virtual filesystem (fs.FS).
// Used when an embedder provides WithSourceFS. All paths are relative
// to the FS root. Absolute paths are rejected.
//
// Resolution priority:
//  1. Relative to current load directory (from LoadPathStack)
//  2. Library registry search paths
//  3. Relative to FS root (path as-is)
type FSFileResolver struct {
	fsys fs.FS
	env  *environment.EnvironmentFrame
}

// NewFSFileResolver creates a resolver backed by the given filesystem.
func NewFSFileResolver(fsys fs.FS, env *environment.EnvironmentFrame) *FSFileResolver {
	return &FSFileResolver{fsys: fsys, env: env}
}

func (p *FSFileResolver) ResolveAndOpen(ctx context.Context, filePath string) (fs.File, string, error) {
	if filePath == "" {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound, "resolve: empty filename")
	}
	if filepath.IsAbs(filePath) {
		return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
			"resolve: absolute paths not supported with virtual filesystem: %s", filePath)
	}

	var searched []string

	// Strategy 1: relative to current load directory
	stack := p.env.LoadPathStack()
	if stack != nil {
		currentDir := stack.CurrentDir()
		if currentDir != "" && currentDir != "." {
			candidate := path.Join(currentDir, filePath)
			_, err := fs.Stat(p.fsys, candidate)
			if err == nil {
				return p.openChecked(ctx, candidate)
			}
			searched = append(searched, currentDir+"/")
		}
	}

	// Strategy 2: library registry search paths
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			for _, dir := range reg.GetSearchPaths() {
				if dir == "" {
					continue
				}
				candidate := path.Join(dir, filePath)
				_, err := fs.Stat(p.fsys, candidate)
				if err == nil {
					return p.openChecked(ctx, candidate)
				}
				searched = append(searched, dir+"/")
			}
		}
	}

	// Strategy 3: relative to FS root (path as-is)
	_, err := fs.Stat(p.fsys, filePath)
	if err == nil {
		return p.openChecked(ctx, filePath)
	}
	searched = append(searched, "./")

	searchedList := strings.Join(searched, ", ")
	return nil, "", werr.WrapForeignErrorf(werr.ErrFileNotFound,
		"file %q not found in virtual filesystem; searched: %s", filePath, searchedList)
}

func (p *FSFileResolver) openChecked(ctx context.Context, resolvedPath string) (fs.File, string, error) {
	auth, _ := p.env.Namespace().Authorizer().(security.Authorizer)
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   resolvedPath,
	})
	if err != nil {
		return nil, "", err
	}

	f, err := p.fsys.Open(resolvedPath)
	if err != nil {
		return nil, "", werr.WrapForeignErrorWithCause(werr.ErrFileNotFound, err, "open %s", resolvedPath)
	}
	return f, resolvedPath, nil
}
```

Note: add `"path"` and `"strings"` to the import block if not already present.
The `"path/filepath"` import is already there for `filepath.IsAbs`.

**Step 4: Run tests**

Run: `go test -v -run TestFSFileResolver ./machine/...`

Expected: all PASS.

**Step 5: Run `make lint`**

Run: `make lint`

Expected: PASS.

**Step 6: Commit**

```
feat: add FSFileResolver for virtual filesystem source loading

FSFileResolver resolves include/load/import files from an fs.FS.
Rejects absolute paths. Resolution priority: load path stack,
library search paths, FS root.
```

---

## Task 3: Route Library Loading Through `FileResolver`

Currently `LoadLibrary` / `loadLibraryFromFile` bypass `FileResolver` and call
`os.Open` directly. This must go through `FileResolver` so `FSFileResolver`
works for library imports.

**Files:**
- Modify: `machine/library_loader.go`
- Modify: `machine/library_registry.go` (add `ToFSPath` method)

**Step 1: Add `ToFSPath` to `LibraryName`**

`ToFilePath()` uses `os.PathSeparator` which produces `\` on Windows.
`fs.FS` always uses `/`. Add a method that always produces forward-slash paths:

In `machine/library_registry.go`:

```go
// ToFSPath returns the library name as a forward-slash-separated path
// with .sld extension. Unlike ToFilePath, this always uses "/" as
// separator, suitable for fs.FS operations.
func (p LibraryName) ToFSPath() string {
	return strings.Join(p.Parts, "/") + ".sld"
}
```

**Step 2: Refactor `LoadLibrary` to use `FileResolver`**

Replace the resolution + `os.Open` logic with `FileResolver.ResolveAndOpen`.
The resolver already handles search paths, security, and opening.

In `machine/library_loader.go`, replace `LoadLibrary`:

```go
func LoadLibrary(ctx context.Context, name LibraryName, env *environment.EnvironmentFrame) (*CompiledLibrary, error) {
	registryAny := env.LibraryRegistry()
	if registryAny == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "load-library: no library registry configured")
	}
	registry, ok := registryAny.(*LibraryRegistry)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "load-library: invalid library registry type")
	}

	lib := registry.Lookup(name)
	if lib != nil {
		return lib, nil
	}

	if registry.IsLoading(name) {
		return nil, werr.WrapForeignErrorf(werr.ErrCircularDependency,
			"circular dependency detected while loading %s", name.SchemeString())
	}
	registry.StartLoading(name)
	defer registry.FinishLoading(name)

	// Resolve and open via FileResolver (supports both OS and virtual FS).
	resolver, resolverOK := env.FileResolver().(FileResolver)
	if !resolverOK {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration,
			"load-library: no file resolver configured")
	}

	// Try .sld first, then .scm
	sldPath := name.ToFSPath()
	f, filePath, err := resolver.ResolveAndOpen(ctx, sldPath)
	if err != nil {
		scmPath := strings.TrimSuffix(sldPath, ".sld") + ".scm"
		f, filePath, err = resolver.ResolveAndOpen(ctx, scmPath)
		if err != nil {
			return nil, werr.WrapForeignErrorf(err,
				"could not find library %s", name.SchemeString())
		}
	}
	defer f.Close() //nolint:errcheck

	lib, err = loadLibraryFromReader(ctx, f, filePath, name, env)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"error loading library %s from %s", name.SchemeString(), filePath)
	}

	err = registry.Register(lib)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err,
			"error registering library %s", name.SchemeString())
	}

	return lib, nil
}
```

**Step 3: Rename `loadLibraryFromFile` to `loadLibraryFromReader`**

The function no longer opens the file — it receives an already-open `fs.File`.
Change its signature to accept `io.Reader` (or `fs.File`) instead of a path:

```go
// loadLibraryFromReader parses, compiles, and executes a library from an open file.
func loadLibraryFromReader(ctx context.Context, r io.Reader, filePath string, expectedName LibraryName, callerEnv *environment.EnvironmentFrame) (*CompiledLibrary, error) {
	// Push to stack after successful open, pop on exit
	stack := callerEnv.LoadPathStack()
	if stack != nil {
		pushErr := stack.Push(filePath)
		if pushErr != nil {
			return nil, pushErr
		}
		defer stack.Pop()
	}

	// Create a fresh environment for the library
	factory := callerEnv.Namespace().LibraryEnvFactory()
	if factory == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryConfiguration, "LibraryEnvFactory not configured")
	}
	libEnv, err := factory(ctx, callerEnv, expectedName.Parts)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "could not create library environment")
	}

	libEnv.SetLibraryRegistry(callerEnv.LibraryRegistry())

	reader := bufio.NewReader(r)
	p := parser.NewParserWithFile(libEnv, true, reader, filePath)

	stx, err := p.ReadSyntax(ctx)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "library file is empty")
		}
		return nil, werr.WrapForeignErrorf(err, "could not parse library file")
	}

	pair, ok := stx.(*syntax.SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "expected define-library form, got %T", stx)
	}

	carStx := pair.SyntaxCar()
	carSym, ok := carStx.(*syntax.SyntaxSymbol)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "expected define-library, got %T", carStx)
	}

	symName := carSym.Sym.Key
	if symName != "define-library" && symName != "library" {
		return nil, werr.WrapForeignErrorf(werr.ErrLibraryFormMalformed, "expected define-library, got %s", symName)
	}

	lib, err := compileAndExecuteLibrary(ctx, stx, expectedName, libEnv, filePath)
	if err != nil {
		return nil, err
	}

	return lib, nil
}
```

Note: `os` import can be removed from `library_loader.go` if no other code in
the file uses it. The `"io"` import is already present.

**Step 4: Remove `FindLibraryFile` from `LibraryRegistry`**

`FindLibraryFile` used `os.Stat` directly and is no longer called. Check for
any other callers:

Run: `grep -r 'FindLibraryFile' --include='*.go' .`

If only `library_loader.go` (now removed) and `library_registry.go` (definition)
reference it, delete the method. If other callers exist, keep it but add a
deprecation comment.

**Step 5: Run existing library tests**

Run: `go test -v -run 'TestLoadLibrary|TestLibraryLoader' ./machine/...`

Expected: all PASS — existing tests use OS filesystem, `OSFileResolver` handles
them the same way.

Also run integration tests:

Run: `go test -v ./integration/...`

Expected: PASS.

**Step 6: Run `make lint`**

Run: `make lint`

Expected: PASS.

**Step 7: Commit**

```
refactor: route library loading through FileResolver

LoadLibrary now uses FileResolver.ResolveAndOpen instead of calling
os.Open directly. This enables library imports from virtual filesystems.
Adds LibraryName.ToFSPath() for forward-slash paths.
Removes FindLibraryFile (dead code after this change).
```

---

## Task 4: Remove `filepath.IsAbs` Guards from Include/Load Push Sites

Currently `include` and `load` skip pushing to `LoadPathStack` when the resolved
path isn't absolute. With `FSFileResolver` returning relative paths, these guards
must be removed so relative includes chain correctly within a virtual FS.

**Files:**
- Modify: `machine/compile_time_continuation_include.go`
- Modify: `internal/extensions/eval/prim_eval.go`

**Step 1: Update `include` in `compile_time_continuation_include.go`**

Lines ~77-86. Remove the `filepath.IsAbs` guard. Always push to stack if
stack is non-nil:

```go
// Push to stack after successful open, pop on exit.
stack := p.env.LoadPathStack()
if stack != nil {
	pushErr := stack.Push(filePath)
	if pushErr != nil {
		return pushErr
	}
	defer stack.Pop()
}
```

Remove the comments about "embedded/virtual filesystems return relative paths
that don't participate in load-path resolution" — they now do participate.

**Step 2: Update `load` in `prim_eval.go`**

Lines ~137-144. Same change — remove the `filepath.IsAbs` guard:

```go
stack := env.LoadPathStack()
if stack != nil {
	err = stack.Push(absPath)
	if err != nil {
		return err
	}
	defer stack.Pop()
}
```

Note: the variable is still called `absPath` from the resolver. Consider
renaming to `resolvedPath` since it may now be relative. The resolver returns
the resolved path as its second return value.

**Step 3: Run existing include/load tests**

Run: `go test -v -run 'TestCompileInclude|TestLoad' ./machine/... ./internal/extensions/eval/...`

Expected: all PASS. Existing OS-based tests still return absolute paths
from `OSFileResolver`, so behavior is unchanged.

**Step 4: Run `make lint`**

Run: `make lint`

Expected: PASS.

**Step 5: Commit**

```
refactor: always push resolved path to LoadPathStack

Removes filepath.IsAbs guards from include and load push sites.
Relative paths from FSFileResolver now participate in load-path
resolution, enabling nested includes within virtual filesystems.
```

---

## Task 5: Add `WithSourceFS` Engine Option and Wire It

**Files:**
- Modify: `options.go` (add option + config field)
- Modify: `engine.go` (wire FSFileResolver)

**Step 1: Add config field and option**

In `options.go`, add to `engineConfig`:

```go
import "io/fs"

type engineConfig struct {
	// ... existing fields ...
	sourceFS fs.FS // virtual filesystem for source loading (via WithSourceFS)
}
```

Add the option function:

```go
// WithSourceFS sets a virtual filesystem for all source file loading
// (include, load, library import). When set, the OS filesystem is not
// consulted for source files. Library search paths from WithLibraryPaths
// become relative paths within the FS.
//
// Bootstrap macros are unaffected — they always load from the embedded
// bootstrap filesystem.
//
// Example:
//
//	//go:embed scheme
//	var schemeFS embed.FS
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithSourceFS(schemeFS),
//	    wile.WithLibraryPaths("lib"),
//	)
func WithSourceFS(fsys fs.FS) EngineOption {
	return func(cfg *engineConfig) {
		cfg.sourceFS = fsys
	}
}
```

**Step 2: Wire `FSFileResolver` in `engine.go`**

In `NewEngine`, where `OSFileResolver` is created (~line 177), check for
`sourceFS`:

```go
// Set the default file resolver for runtime include/load operations.
// This must happen after bootstrap (which uses EmbedFileResolver).
if cfg.sourceFS != nil {
	env.SetFileResolver(machine.NewFSFileResolver(cfg.sourceFS, env))
} else {
	env.SetFileResolver(machine.NewOSFileResolver(env))
}
```

Same change in the library-enabled branch (~line 184-185):

```go
if env.FileResolver() == nil {
	if cfg.sourceFS != nil {
		env.SetFileResolver(machine.NewFSFileResolver(cfg.sourceFS, env))
	} else {
		env.SetFileResolver(machine.NewOSFileResolver(env))
	}
}
```

**Step 3: Run `make lint`**

Run: `make lint`

Expected: PASS.

**Step 4: Commit**

```
feat: add WithSourceFS engine option

When set, all source loading (include, load, library import) resolves
within the provided fs.FS. The OS filesystem is not consulted.
```

---

## Task 6: Integration Tests

End-to-end tests that exercise `WithSourceFS` through the engine API.

**Files:**
- Modify: `fs_source_test.go` (new file in root package)

**Step 1: Write integration tests**

Create `fs_source_test.go` in the root package:

```go
package wile_test

import (
	"context"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/values"
)

func TestWithSourceFS_Load(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"helper.scm": {Data: []byte("(define helper-val 42)")},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	_, err = eng.Eval(ctx, `(load "helper.scm")`)
	c.Assert(err, qt.IsNil)

	result, err := eng.Eval(ctx, "helper-val")
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.CmpEquals(), values.NewInteger(42))
}

func TestWithSourceFS_NestedLoad(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"main.scm":       {Data: []byte(`(load "sub/helper.scm")`)},
		"sub/helper.scm": {Data: []byte("(define nested-val 99)")},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	_, err = eng.Eval(ctx, `(load "main.scm")`)
	c.Assert(err, qt.IsNil)

	result, err := eng.Eval(ctx, "nested-val")
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.CmpEquals(), values.NewInteger(99))
}

func TestWithSourceFS_LibraryImport(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/mylib.sld": {Data: []byte(`
			(define-library (mylib)
				(export greet)
				(begin
					(define greet "hello from fs")))
		`)},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.Eval(ctx, `(import (mylib)) greet`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.CmpEquals(), values.NewString("hello from fs"))
}

func TestWithSourceFS_IncludeInLibrary(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"lib/mylib.sld": {Data: []byte(`
			(define-library (mylib)
				(export compute)
				(include "impl.scm"))
		`)},
		"lib/impl.scm": {Data: []byte(`(begin (define compute 777))`)},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("lib"),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.Eval(ctx, `(import (mylib)) compute`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.CmpEquals(), values.NewInteger(777))
}

func TestWithSourceFS_LoadRejectsAbsolutePath(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	fsys := fstest.MapFS{
		"a.scm": {Data: []byte("1")},
	}

	eng, err := wile.NewEngine(ctx,
		wile.WithSafeExtensions(),
		wile.WithSourceFS(fsys),
	)
	c.Assert(err, qt.IsNil)

	_, err = eng.Eval(ctx, `(load "/absolute/path.scm")`)
	c.Assert(err, qt.IsNotNil)
}

func TestWithSourceFS_NotSet_UsesOSFilesystem(t *testing.T) {
	// Verify that without WithSourceFS, behavior is unchanged.
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithSafeExtensions())
	c.Assert(err, qt.IsNil)

	// Loading a nonexistent file should fail with file-not-found, not
	// a "no resolver" error — proving OSFileResolver is still in use.
	_, err = eng.Eval(ctx, `(load "definitely-nonexistent-file.scm")`)
	c.Assert(err, qt.IsNotNil)
}
```

**Step 2: Run integration tests**

Run: `go test -v -run TestWithSourceFS .`

Expected: all PASS.

**Step 3: Run full test suite**

Run: `make test`

Expected: all PASS. No regressions.

**Step 4: Run `make lint && make covercheck`**

Run: `make lint && make covercheck`

Expected: PASS.

**Step 5: Commit**

```
test: add integration tests for WithSourceFS

Covers load, nested load, library import, include within library,
absolute path rejection, and OS fallback when WithSourceFS is not set.
```

---

## Task 7: Final Verification

**Step 1: Run full test suite + lint + covercheck**

Run: `make lint && make test && make covercheck`

Expected: all PASS.

**Step 2: Verify no remaining direct `os.Open`/`os.Stat` in source loading paths**

Run: `grep -n 'os\.Open\|os\.Stat' machine/library_loader.go machine/compile_time_continuation_include.go internal/extensions/eval/prim_eval.go`

Expected: no matches (all source loading now goes through `FileResolver`).

**Step 3: Update design doc status**

In `plans/FS-SOURCE-LOADING.md`, change status to:

```
**Status:** Complete
```
