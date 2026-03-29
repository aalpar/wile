# `available-libraries` Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add library discovery via `(available-libraries)` Scheme primitive and `Engine.AvailableLibraries()` Go API.

**Architecture:** Extend the FileResolver chain with an optional `LibraryEnumerator` interface. Each resolver that can enumerate its filesystem implements it. Discovery unions filesystem-discovered libraries with registry-known synthetic libraries.

**Tech Stack:** Go stdlib (`io/fs`, `path/filepath`, `strconv`, `testing/fstest`), existing `machine/`, `values/`, `extensions/introspection/` packages.

**Design doc:** `plans/AVAILABLE-LIBRARIES.md`

---

### Task 1: `filePathToLibraryName` Helper

Converts a filesystem path like `"scheme/base.sld"` to a `LibraryName`. This is the inverse of `ToFilePath()`/`ToFSPath()` and is the core building block for all enumeration.

**Files:**
- Modify: `machine/library_registry.go`
- Modify: `machine/library_registry_test.go`

**Step 1: Write the failing tests**

In `machine/library_registry_test.go` (this is `package machine_test`, so use exported names):

```go
func TestFilePathToLibraryName(t *testing.T) {
	tests := []struct {
		name     string
		path     string
		wantKey  string
		wantErr  bool
	}{
		{"sld extension", "scheme/base.sld", "scheme/base", false},
		{"scm extension", "chibi/test.scm", "chibi/test", false},
		{"nested path", "wile/algebra/rewrite.sld", "wile/algebra/rewrite", false},
		{"single component", "base.sld", "base", false},
		{"no extension", "scheme/base", "", true},
		{"wrong extension", "scheme/base.txt", "", true},
		{"empty path", "", "", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result, err := machine.FilePathToLibraryName(tt.path)
			if tt.wantErr {
				c.Assert(err, qt.IsNotNil)
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.Key(), qt.Equals, tt.wantKey)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestFilePathToLibraryName ./machine/...`
Expected: FAIL — `FilePathToLibraryName` undefined.

**Step 3: Implement `FilePathToLibraryName`**

In `machine/library_registry.go`, add after the existing `LibraryName` methods:

```go
// FilePathToLibraryName converts a forward-slash-separated file path with
// .sld or .scm extension to a LibraryName. This is the inverse of ToFSPath().
// Returns an error if the path has no recognized extension or is empty.
func FilePathToLibraryName(path string) (LibraryName, error) {
	if path == "" {
		return LibraryName{}, werr.WrapForeignErrorf(
			werr.ErrInvalidArgument, "filePathToLibraryName: empty path",
		)
	}
	var trimmed string
	switch {
	case strings.HasSuffix(path, ".sld"):
		trimmed = strings.TrimSuffix(path, ".sld")
	case strings.HasSuffix(path, ".scm"):
		trimmed = strings.TrimSuffix(path, ".scm")
	default:
		return LibraryName{}, werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"filePathToLibraryName: unrecognized extension in %q", path,
		)
	}
	parts := strings.Split(trimmed, "/")
	return NewLibraryName(parts...), nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestFilePathToLibraryName ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add FilePathToLibraryName helper

Inverse of ToFSPath() — converts filesystem paths to LibraryName
for library enumeration support.
```

---

### Task 2: `LibraryName.ToSchemeValue` Method

Converts a `LibraryName` to a Scheme list suitable for returning from `(available-libraries)`. Parts that parse as nonnegative integers become `*values.Integer`, others become `*values.Symbol`.

**Files:**
- Modify: `machine/library_registry.go`
- Modify: `machine/library_registry_test.go`

**Step 1: Write the failing tests**

In `machine/library_registry_test.go`:

```go
func TestLibraryNameToSchemeValue(t *testing.T) {
	tests := []struct {
		name   string
		parts  []string
		expect string // Scheme display form
	}{
		{"symbol parts", []string{"scheme", "base"}, "(scheme base)"},
		{"integer part", []string{"srfi", "1"}, "(srfi 1)"},
		{"single part", []string{"base"}, "(base)"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			ln := machine.NewLibraryName(tt.parts...)
			result := ln.ToSchemeValue()
			c.Assert(fmt.Sprint(result), qt.Equals, tt.expect)
		})
	}
}
```

Note: `fmt.Sprint(result)` uses the `String()` method on the Scheme value, which should produce the expected display. Verify that `values.List(...)` and the individual symbol/integer values display correctly by reading `values.Pair.String()` before committing to this assertion format.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestLibraryNameToSchemeValue ./machine/...`
Expected: FAIL — `ToSchemeValue` undefined.

**Step 3: Implement `ToSchemeValue`**

In `machine/library_registry.go`, add to `LibraryName` methods:

```go
// ToSchemeValue converts a LibraryName to a Scheme list.
// Parts that parse as nonnegative integers become exact integers;
// all others become symbols. Matches R7RS library name syntax.
func (p LibraryName) ToSchemeValue() values.Value {
	elems := make([]values.Value, len(p.Parts))
	for i, part := range p.Parts {
		n, err := strconv.ParseInt(part, 10, 64)
		if err == nil && n >= 0 {
			elems[i] = values.NewInteger(n)
		} else {
			elems[i] = values.NewSymbol(part)
		}
	}
	return values.List(elems...)
}
```

Requires adding `"strconv"` and `"github.com/aalpar/wile/values"` to imports.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestLibraryNameToSchemeValue ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add LibraryName.ToSchemeValue method

Converts LibraryName to Scheme list with proper R7RS types
(symbols for identifiers, integers for numeric components).
```

---

### Task 3: `LibraryRegistry.AllNames` Method

Extracts just the names from `All()`, avoiding the need for callers to unwrap `*CompiledLibrary`.

**Files:**
- Modify: `machine/library_registry.go`
- Modify: `machine/library_registry_test.go`

**Step 1: Write the failing test**

In `machine/library_registry_test.go`:

```go
func TestLibraryRegistryAllNames(t *testing.T) {
	c := qt.New(t)
	reg := machine.NewLibraryRegistry()
	env := environment.NewNamespace().Runtime()

	// Register two libraries.
	lib1 := machine.NewCompiledLibrary(machine.NewLibraryName("scheme", "base"), env)
	c.Assert(reg.Register(lib1), qt.IsNil)
	lib2 := machine.NewCompiledLibrary(machine.NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(lib2), qt.IsNil)

	names := reg.AllNames()
	c.Assert(len(names), qt.Equals, 2)
	// Sorted by Key().
	c.Assert(names[0].Key(), qt.Equals, "scheme/base")
	c.Assert(names[1].Key(), qt.Equals, "wile/io")
}

func TestLibraryRegistryAllNamesEmpty(t *testing.T) {
	c := qt.New(t)
	reg := machine.NewLibraryRegistry()
	names := reg.AllNames()
	c.Assert(len(names), qt.Equals, 0)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestLibraryRegistryAllNames ./machine/...`
Expected: FAIL — `AllNames` undefined.

**Step 3: Implement `AllNames`**

In `machine/library_registry.go`, add after `All()`:

```go
// AllNames returns the names of all registered libraries, sorted by key.
func (p *LibraryRegistry) AllNames() []LibraryName {
	libs := p.All()
	names := make([]LibraryName, len(libs))
	for i, lib := range libs {
		names[i] = lib.Name
	}
	return names
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestLibraryRegistryAllNames ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add LibraryRegistry.AllNames method

Returns sorted library names without exposing CompiledLibrary internals.
```

---

### Task 4: `LibraryEnumerator` Interface and `FSFileResolver` Implementation

The core enumeration interface plus the first concrete implementation. `FSFileResolver` walks its `fs.FS` using the same search paths it uses for resolution.

**Files:**
- Modify: `machine/file_resolver.go`
- Modify: `machine/file_resolver_test.go`

**Step 1: Write the failing tests**

In `machine/file_resolver_test.go` (this is `package machine`, so use unexported names):

```go
func TestFSFileResolverEnumerateLibraries(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("(define-library (scheme base))")},
		"scheme/write.sld": &fstest.MapFile{Data: []byte("(define-library (scheme write))")},
		"chibi/test.scm":   &fstest.MapFile{Data: []byte("(define-library (chibi test))")},
		".hidden/lib.sld":  &fstest.MapFile{Data: []byte("skip")},
		"readme.txt":       &fstest.MapFile{Data: []byte("not a library")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	resolver := NewFSFileResolver(fsys, env)

	enumerator, ok := resolver.(LibraryEnumerator)
	c.Assert(ok, qt.IsTrue)

	libs, err := enumerator.EnumerateLibraries()
	c.Assert(err, qt.IsNil)

	keys := make([]string, len(libs))
	for i, lib := range libs {
		keys[i] = lib.Key()
	}
	// Sorted, no hidden dirs, no .txt files.
	c.Assert(keys, qt.DeepEquals, []string{"chibi/test", "scheme/base", "scheme/write"})
}

func TestFSFileResolverEnumerateWithSearchPaths(t *testing.T) {
	c := qt.New(t)

	// Libraries live under "lib/" subdirectory.
	fsys := fstest.MapFS{
		"lib/scheme/base.sld":  &fstest.MapFile{Data: []byte("")},
		"lib/scheme/write.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"lib"})
	ns.SetLibraryRegistry(reg)

	resolver := NewFSFileResolver(fsys, env)

	enumerator, ok := resolver.(LibraryEnumerator)
	c.Assert(ok, qt.IsTrue)

	libs, err := enumerator.EnumerateLibraries()
	c.Assert(err, qt.IsNil)

	keys := make([]string, len(libs))
	for i, lib := range libs {
		keys[i] = lib.Key()
	}
	// Search path "lib" is stripped, so we get (scheme base), not (lib scheme base).
	c.Assert(keys, qt.DeepEquals, []string{"scheme/base", "scheme/write"})
}

func TestFSFileResolverEnumerateSldBeatsScm(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
		"scheme/base.scm": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	resolver := NewFSFileResolver(fsys, env)
	enumerator := resolver.(LibraryEnumerator)

	libs, err := enumerator.EnumerateLibraries()
	c.Assert(err, qt.IsNil)

	// Only one entry, not two.
	c.Assert(len(libs), qt.Equals, 1)
	c.Assert(libs[0].Key(), qt.Equals, "scheme/base")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestFSFileResolverEnumerate ./machine/...`
Expected: FAIL — `LibraryEnumerator` undefined.

**Step 3: Implement**

In `machine/file_resolver.go`:

Add the interface near the top (after `FileResolver` interface):

```go
// LibraryEnumerator is an optional interface that FileResolvers can implement
// to support library discovery. Enumeration is the inverse of resolution:
// same directories, same priority, but walking files instead of looking up
// a specific name.
type LibraryEnumerator interface {
	EnumerateLibraries() ([]LibraryName, error)
}
```

Add a package-level helper for walking (used by both FS and OS implementations):

```go
// isLibraryFile returns true if the filename has a .sld or .scm extension.
func isLibraryFile(name string) bool {
	return strings.HasSuffix(name, ".sld") || strings.HasSuffix(name, ".scm")
}

// isHidden returns true if the name starts with ".".
func isHidden(name string) bool {
	return len(name) > 0 && name[0] == '.'
}
```

Add the `EnumerateLibraries` method to `FSFileResolver`:

```go
// EnumerateLibraries walks the virtual filesystem to discover importable
// libraries. Walks each search path (stripping the prefix) and the FS root.
// .sld files take precedence over .scm for the same library name.
func (p *FSFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	addFromDir := func(baseDir string) error {
		prefix := baseDir
		if prefix == "." {
			prefix = ""
		}
		return fs.WalkDir(p.fsys, baseDir, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return nil // skip unreadable entries
			}
			if d.IsDir() {
				if isHidden(d.Name()) {
					return fs.SkipDir
				}
				return nil
			}
			if !isLibraryFile(d.Name()) {
				return nil
			}

			relPath := path
			if prefix != "" {
				relPath = strings.TrimPrefix(path, prefix+"/")
			}

			name, nameErr := FilePathToLibraryName(relPath)
			if nameErr != nil {
				return nil // skip unrecognizable files
			}

			key := name.Key()
			if seen[key] {
				return nil
			}
			seen[key] = true
			result = append(result, name)
			return nil
		})
	}

	// Walk search paths first (same priority as resolution).
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			for _, dir := range reg.GetSearchPaths() {
				if dir == "" {
					continue
				}
				_ = addFromDir(dir)
			}
		}
	}

	// Walk FS root as fallback (matches resolution strategy 3).
	_ = addFromDir(".")

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
```

Requires adding `"io/fs"` to imports if not already present.

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestFSFileResolverEnumerate ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add LibraryEnumerator interface and FSFileResolver implementation

FSFileResolver walks its fs.FS using the same search paths used for
resolution, converting file paths to library names.
```

---

### Task 5: `OSFileResolver.EnumerateLibraries`

Walks the OS filesystem using the same search paths, `SCHEME_INCLUDE_PATH`, and CWD fallback that resolution uses.

**Files:**
- Modify: `machine/file_resolver.go`
- Modify: `machine/file_resolver_test.go`

**Step 1: Write the failing tests**

In `machine/file_resolver_test.go`:

```go
func TestOSFileResolverEnumerateLibraries(t *testing.T) {
	c := qt.New(t)

	dir := realDir(t, t.TempDir())

	// Create library files in a search path.
	libDir := filepath.Join(dir, "libs")
	c.Assert(os.MkdirAll(filepath.Join(libDir, "scheme"), 0o755), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, "scheme", "base.sld"), []byte(""), 0o644), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, "scheme", "write.sld"), []byte(""), 0o644), qt.IsNil)

	// Create a hidden directory that should be skipped.
	c.Assert(os.MkdirAll(filepath.Join(libDir, ".hidden"), 0o755), qt.IsNil)
	c.Assert(os.WriteFile(filepath.Join(libDir, ".hidden", "skip.sld"), []byte(""), 0o644), qt.IsNil)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{libDir})
	ns.SetLibraryRegistry(reg)

	// Clear SCHEME_INCLUDE_PATH to avoid interference.
	t.Setenv(SchemeIncludePathEnv, "")

	resolver := NewOSFileResolver(env)
	enumerator, ok := resolver.(LibraryEnumerator)
	c.Assert(ok, qt.IsTrue)

	libs, err := enumerator.EnumerateLibraries()
	c.Assert(err, qt.IsNil)

	keys := make([]string, len(libs))
	for i, lib := range libs {
		keys[i] = lib.Key()
	}
	c.Assert(keys, qt.DeepEquals, []string{"scheme/base", "scheme/write"})
}

func TestOSFileResolverEnumerateEmptySearchPaths(t *testing.T) {
	c := qt.New(t)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"/nonexistent/path"})
	ns.SetLibraryRegistry(reg)

	t.Setenv(SchemeIncludePathEnv, "")

	resolver := NewOSFileResolver(env)
	enumerator := resolver.(LibraryEnumerator)

	libs, err := enumerator.EnumerateLibraries()
	c.Assert(err, qt.IsNil)
	c.Assert(len(libs), qt.Equals, 0)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestOSFileResolverEnumerate ./machine/...`
Expected: FAIL — `OSFileResolver` does not implement `LibraryEnumerator`.

**Step 3: Implement**

In `machine/file_resolver.go`, add to `OSFileResolver`:

```go
// EnumerateLibraries walks the OS filesystem to discover importable libraries.
// Walks library registry search paths, SCHEME_INCLUDE_PATH directories, and CWD,
// matching the same search order used by ResolveAndOpen.
func (p *OSFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	walkDir := func(baseDir string) {
		filepath.WalkDir(baseDir, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return nil // skip unreadable entries
			}
			if d.IsDir() {
				if isHidden(d.Name()) {
					return filepath.SkipDir
				}
				return nil
			}
			if !isLibraryFile(d.Name()) {
				return nil
			}

			relPath, relErr := filepath.Rel(baseDir, path)
			if relErr != nil {
				return nil
			}
			// Normalize to forward slashes for FilePathToLibraryName.
			relPath = filepath.ToSlash(relPath)

			name, nameErr := FilePathToLibraryName(relPath)
			if nameErr != nil {
				return nil
			}

			key := name.Key()
			if seen[key] {
				return nil
			}
			seen[key] = true
			result = append(result, name)
			return nil
		})
	}

	// Library registry search paths (same priority as ResolveAndOpen).
	regAny := p.env.LibraryRegistry()
	if regAny != nil {
		reg, ok := regAny.(*LibraryRegistry)
		if ok {
			for _, dir := range reg.GetSearchPaths() {
				walkDir(dir)
			}
		}
	}

	// SCHEME_INCLUDE_PATH (same as ResolveAndOpen).
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath != "" {
		for _, dir := range filepath.SplitList(includePath) {
			walkDir(dir)
		}
	}

	// CWD fallback (same as ResolveAndOpen).
	cwd, err := os.Getwd()
	if err == nil {
		walkDir(cwd)
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestOSFileResolverEnumerate ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add OSFileResolver.EnumerateLibraries

Walks OS filesystem search paths, SCHEME_INCLUDE_PATH, and CWD
using the same priority order as ResolveAndOpen.
```

---

### Task 6: `ChainFileResolver.EnumerateLibraries`

Delegates to child resolvers that implement `LibraryEnumerator`, unioning results with first-resolver-wins deduplication.

**Files:**
- Modify: `machine/file_resolver.go`
- Modify: `machine/file_resolver_test.go`

**Step 1: Write the failing test**

In `machine/file_resolver_test.go`:

```go
func TestChainFileResolverEnumerateLibraries(t *testing.T) {
	c := qt.New(t)

	fs1 := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
		"my/lib.sld":      &fstest.MapFile{Data: []byte("")},
	}
	fs2 := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("")}, // duplicate
		"scheme/write.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	chain := NewChainFileResolver([]FileResolver{
		NewFSFileResolver(fs1, env),
		NewFSFileResolver(fs2, env),
	})

	enumerator, ok := chain.(LibraryEnumerator)
	c.Assert(ok, qt.IsTrue)

	libs, err := enumerator.EnumerateLibraries()
	c.Assert(err, qt.IsNil)

	keys := make([]string, len(libs))
	for i, lib := range libs {
		keys[i] = lib.Key()
	}
	// Union of both resolvers, deduplicated, sorted.
	c.Assert(keys, qt.DeepEquals, []string{"my/lib", "scheme/base", "scheme/write"})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestChainFileResolverEnumerate ./machine/...`
Expected: FAIL — `ChainFileResolver` does not implement `LibraryEnumerator`.

**Step 3: Implement**

In `machine/file_resolver.go`, add to `ChainFileResolver`:

```go
// EnumerateLibraries unions library enumerations from all child resolvers
// that implement LibraryEnumerator. First resolver wins on duplicate keys,
// matching the resolution priority order.
func (p *ChainFileResolver) EnumerateLibraries() ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	for _, r := range p.resolvers {
		enumerator, ok := r.(LibraryEnumerator)
		if !ok {
			continue
		}
		libs, err := enumerator.EnumerateLibraries()
		if err != nil {
			continue // best effort: skip failing resolvers
		}
		for _, lib := range libs {
			key := lib.Key()
			if seen[key] {
				continue
			}
			seen[key] = true
			result = append(result, lib)
		}
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestChainFileResolverEnumerate ./machine/...`
Expected: PASS

**Step 5: Run all file_resolver tests**

Run: `go test -v -run TestFSFileResolver\|TestOSFileResolver\|TestChainFileResolver ./machine/...`
Expected: ALL PASS

**Step 6: Commit**

```
feat(machine): add ChainFileResolver.EnumerateLibraries

Unions child resolver enumerations with first-resolver-wins
deduplication, matching the resolution chain's priority order.
```

---

### Task 7: `DiscoverAvailableLibraries` Function

Top-level discovery function that combines resolver enumeration with registry-known libraries. This is the shared logic used by both the Go API and the Scheme primitive.

**Files:**
- Create: `machine/library_discovery.go`
- Create: `machine/library_discovery_test.go`

**Step 1: Write the failing test**

Create `machine/library_discovery_test.go`:

```go
package machine

import (
	"sort"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
)

func TestDiscoverAvailableLibraries(t *testing.T) {
	c := qt.New(t)

	// Set up a virtual FS with two libraries.
	fsys := fstest.MapFS{
		"scheme/base.sld": &fstest.MapFile{Data: []byte("")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	reg.SetSearchPaths([]string{"."})
	ns.SetLibraryRegistry(reg)

	// Register a synthetic extension library (not on disk).
	syntheticLib := NewCompiledLibrary(NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(syntheticLib), qt.IsNil)

	resolver := NewFSFileResolver(fsys, env)
	env.SetFileResolver(resolver)

	libs, err := DiscoverAvailableLibraries(resolver, reg)
	c.Assert(err, qt.IsNil)

	keys := make([]string, len(libs))
	for i, lib := range libs {
		keys[i] = lib.Key()
	}
	sort.Strings(keys)
	// Union: filesystem (scheme/base) + registry (wile/io).
	c.Assert(keys, qt.DeepEquals, []string{"scheme/base", "wile/io"})
}

func TestDiscoverAvailableLibrariesNoEnumerator(t *testing.T) {
	c := qt.New(t)

	ns := environment.NewNamespace()
	env := ns.Runtime()
	reg := NewLibraryRegistry()
	ns.SetLibraryRegistry(reg)

	syntheticLib := NewCompiledLibrary(NewLibraryName("wile", "io"), env)
	c.Assert(reg.Register(syntheticLib), qt.IsNil)

	// EmbedFileResolver does not implement LibraryEnumerator.
	resolver := NewEmbedFileResolver(fstest.MapFS{})

	libs, err := DiscoverAvailableLibraries(resolver, reg)
	c.Assert(err, qt.IsNil)

	// Only registry libraries.
	c.Assert(len(libs), qt.Equals, 1)
	c.Assert(libs[0].Key(), qt.Equals, "wile/io")
}

func TestDiscoverAvailableLibrariesNilRegistry(t *testing.T) {
	c := qt.New(t)

	resolver := NewEmbedFileResolver(fstest.MapFS{})

	libs, err := DiscoverAvailableLibraries(resolver, nil)
	c.Assert(err, qt.IsNil)
	c.Assert(len(libs), qt.Equals, 0)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestDiscoverAvailable ./machine/...`
Expected: FAIL — `DiscoverAvailableLibraries` undefined.

**Step 3: Implement**

Create `machine/library_discovery.go`:

```go
package machine

import "sort"

// DiscoverAvailableLibraries returns all importable library names by
// combining filesystem discovery (via the resolver's LibraryEnumerator)
// with registry-known libraries (synthetic extension libraries).
// Returns a sorted, deduplicated list.
//
// If the resolver does not implement LibraryEnumerator, only registry
// libraries are returned. If reg is nil, only filesystem libraries are
// returned.
func DiscoverAvailableLibraries(resolver FileResolver, reg *LibraryRegistry) ([]LibraryName, error) {
	seen := make(map[string]bool)
	var result []LibraryName

	// Filesystem discovery via resolver chain.
	if enumerator, ok := resolver.(LibraryEnumerator); ok {
		libs, err := enumerator.EnumerateLibraries()
		if err != nil {
			return nil, err
		}
		for _, lib := range libs {
			key := lib.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, lib)
			}
		}
	}

	// Registry-known libraries (synthetic extensions).
	if reg != nil {
		for _, name := range reg.AllNames() {
			key := name.Key()
			if !seen[key] {
				seen[key] = true
				result = append(result, name)
			}
		}
	}

	sort.Slice(result, func(i, j int) bool {
		return result[i].Key() < result[j].Key()
	})
	return result, nil
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestDiscoverAvailable ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add DiscoverAvailableLibraries function

Combines FileResolver enumeration with LibraryRegistry to produce
the complete set of importable library names.
```

---

### Task 8: `Engine.AvailableLibraries` Go API

Thin wrapper on the Engine that accesses the file resolver and registry.

**Files:**
- Modify: `engine.go`

**Step 1: Write the failing test**

This will be tested via the Scheme primitive in Task 9 (integration test). For the Go API, add a focused unit test. Since there's no `engine_test.go`, add to an appropriate location. The introspection test file already creates engines — we'll add the Go API test there in Task 9.

For now, add the method and verify it compiles.

**Step 2: Implement**

In `engine.go`, add after the `Registry()` method:

```go
// AvailableLibraries returns all importable library names by combining
// filesystem discovery with registry-known libraries (synthetic extensions).
// Returns a sorted, deduplicated list. If the library system is not enabled
// (no WithLibraryPaths call), returns an empty list.
func (p *Engine) AvailableLibraries(_ context.Context) ([]machine.LibraryName, error) {
	regAny := p.env.LibraryRegistry()
	if regAny == nil {
		return nil, nil
	}
	reg, ok := regAny.(*machine.LibraryRegistry)
	if !ok {
		return nil, nil
	}

	resolverAny := p.env.FileResolver()
	resolver, _ := resolverAny.(machine.FileResolver)

	return machine.DiscoverAvailableLibraries(resolver, reg)
}
```

**Step 3: Verify it compiles**

Run: `go build ./...`
Expected: SUCCESS

**Step 4: Commit**

```
feat: add Engine.AvailableLibraries Go API

Thin wrapper that accesses the file resolver and registry from the
engine's environment, delegating to DiscoverAvailableLibraries.
```

---

### Task 9: `(available-libraries)` Scheme Primitive

The Scheme-accessible entry point in the introspection extension.

**Files:**
- Modify: `extensions/introspection/prim_introspection.go`
- Modify: `extensions/introspection/register.go`
- Modify: `extensions/introspection/prim_introspection_test.go`

**Step 1: Write the failing tests**

In `extensions/introspection/prim_introspection_test.go`, add:

```go
func TestAvailableLibraries(t *testing.T) {
	c := qt.New(t)

	t.Run("returns a list", func(t *testing.T) {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithExtension(extintrospection.Extension),
			wile.WithLibraryPaths("."),
			wile.WithSourceFS(wile.StdLibFS),
		)
		c.Assert(err, qt.IsNil)

		result := schemeEval(t, engine, `(list? (available-libraries))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("contains scheme base", func(t *testing.T) {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithExtension(extintrospection.Extension),
			wile.WithLibraryPaths("."),
			wile.WithSourceFS(wile.StdLibFS),
		)
		c.Assert(err, qt.IsNil)

		result := schemeEval(t, engine, `
			(let loop ((libs (available-libraries)))
			  (cond
			    ((null? libs) #f)
			    ((equal? (car libs) '(scheme base)) #t)
			    (else (loop (cdr libs)))))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("each element is a list", func(t *testing.T) {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithExtension(extintrospection.Extension),
			wile.WithLibraryPaths("."),
			wile.WithSourceFS(wile.StdLibFS),
		)
		c.Assert(err, qt.IsNil)

		result := schemeEval(t, engine, `
			(let loop ((libs (available-libraries)) (ok #t))
			  (if (null? libs)
			      ok
			      (loop (cdr libs) (and ok (list? (car libs))))))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("empty when library system disabled", func(t *testing.T) {
		// No WithLibraryPaths — library system not enabled.
		engine := newEngine(t)
		result := schemeEval(t, engine, `(null? (available-libraries))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("includes synthetic extension libraries", func(t *testing.T) {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithExtension(extintrospection.Extension),
			wile.WithLibraryPaths("."),
			wile.WithSourceFS(wile.StdLibFS),
		)
		c.Assert(err, qt.IsNil)

		// (wile introspection) is a synthetic extension library.
		result := schemeEval(t, engine, `
			(let loop ((libs (available-libraries)))
			  (cond
			    ((null? libs) #f)
			    ((equal? (car libs) '(wile introspection)) #t)
			    (else (loop (cdr libs)))))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		engine := newEngine(t)
		evalExpectError(t, engine, `(available-libraries 42)`)
	})
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestAvailableLibraries ./extensions/introspection/...`
Expected: FAIL — `available-libraries` undefined.

**Step 3: Implement the primitive**

In `extensions/introspection/prim_introspection.go`, add:

```go
// PrimAvailableLibraries implements the (available-libraries) primitive.
// Returns a sorted list of all importable library names.
// Each library name is a list of symbols/integers matching R7RS syntax.
func PrimAvailableLibraries(mc *machine.MachineContext) error {
	env := mc.EnvironmentFrame()

	regAny := env.LibraryRegistry()
	if regAny == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
	reg, ok := regAny.(*machine.LibraryRegistry)
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}

	resolverAny := env.FileResolver()
	resolver, _ := resolverAny.(machine.FileResolver)

	libs, err := machine.DiscoverAvailableLibraries(resolver, reg)
	if err != nil {
		return werr.WrapForeignErrorf(
			werr.ErrInternalError,
			"available-libraries: %s", err,
		)
	}

	elems := make([]values.Value, len(libs))
	for i, lib := range libs {
		elems[i] = lib.ToSchemeValue()
	}
	mc.SetValue(values.List(elems...))
	return nil
}
```

Note: Check that `werr.ErrInternalError` exists. If not, use `werr.ErrRuntimeError` or whichever sentinel is appropriate for internal failures. Grep for the actual sentinel.

**Step 4: Register the primitive**

In `extensions/introspection/register.go`, add to `addPrimitives`:

```go
{Name: "available-libraries", Impl: PrimAvailableLibraries,
    Doc: "Returns a sorted list of all importable library names. Each name is a list of symbols/integers in R7RS library name syntax.", Category: "introspection"},
```

**Step 5: Run tests to verify they pass**

Run: `go test -v -run TestAvailableLibraries ./extensions/introspection/...`
Expected: PASS

**Step 6: Run all introspection tests**

Run: `go test -v ./extensions/introspection/...`
Expected: ALL PASS

**Step 7: Commit**

```
feat(introspection): add (available-libraries) primitive

Returns sorted list of all importable library names, combining
filesystem discovery with synthetic extension libraries.
```

---

### Task 10: Lint and Coverage Check

**Step 1: Run linter**

Run: `make lint`
Expected: PASS. Fix any issues.

**Step 2: Run coverage check**

Run: `make covercheck`
Expected: PASS. Fix any issues.

**Step 3: Run full test suite**

Run: `make test`
Expected: ALL PASS.

**Step 4: Commit any fixes**

```
fix: lint and coverage fixes for available-libraries
```
