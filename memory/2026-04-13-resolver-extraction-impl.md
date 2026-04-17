# Resolver Extraction — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extract file resolution infrastructure from `machine/compilation/` into `machine/compilation/resolver/`, with a clean boundary: resolvers enumerate files (paths), not libraries.

**Architecture:** The new `resolver` package owns the 4 `FileResolver` implementations, shared helpers, authorization checks, and the `SchemeIncludePathEnv` constant. It defines a `FileEnumerator` interface returning `[]string` (file paths). The library-level interpretation (converting paths to `LibraryName`, deduplication by identity) stays in `compilation/`. Type aliases in `compilation/` preserve backward compatibility for external callers.

**Tech Stack:** Go 1.24, `make lint`, `make covercheck`

**Design context:** Discussion in conversation 2026-04-13. Key principle: the file resolver is I/O infrastructure; it finds `.sld`/`.scm` files. The library system (identity, registry, imports) is a separate concern that uses the resolver. Resolvers should not depend on `LibraryName`.

---

## Task 1: Create `resolver` package with types, helpers, and `FileEnumerator`

This is the big additive task. Nothing in compilation changes yet — pure addition.

**Files:**
- Create: `machine/compilation/resolver/doc.go`
- Create: `machine/compilation/resolver/helpers.go`
- Create: `machine/compilation/resolver/os_file_resolver.go`
- Create: `machine/compilation/resolver/embed_file_resolver.go`
- Create: `machine/compilation/resolver/fs_file_resolver.go`
- Create: `machine/compilation/resolver/chain_file_resolver.go`

**Step 1: Create `doc.go` with package doc and `FileEnumerator` interface**

```go
// Package resolver provides file resolution infrastructure for include/load
// operations. Implementations resolve file paths against OS filesystems,
// virtual fs.FS instances, or embedded filesystems.
//
// The FileResolver interface is defined in environment/ (to break import cycles).
// This package provides the concrete implementations.
//
// Resolvers are I/O infrastructure — they find files by path and enumerate
// .sld/.scm files in search directories. Library-level interpretation
// (converting paths to library names, deduplication by library identity)
// belongs to the library system in compilation/.
package resolver

// FileEnumerator is an optional interface that FileResolvers can implement
// to support file discovery. Returns slash-separated paths relative to
// each search directory, filtered to .sld/.scm files.
//
// Paths are returned in walk order within each search directory.
// No deduplication is performed — callers handle library-level identity.
type FileEnumerator interface {
	EnumerateFiles() ([]string, error)
}
```

**Step 2: Create `helpers.go` with shared infrastructure**

Move from `machine/compilation/file_resolver.go`:
- `isLibraryFile` → rename to `IsSchemeFile` (exported for tests, but also used by callers)
- `isHidden`
- `isAuthorized`
- `osSearchDirs`
- `openAuthorized`
- `walkOSLibraries` → rename to `WalkOSSchemeFiles`
- `walkFSDir` → rename to `WalkFSSchemeFiles`
- `SchemeIncludePathEnv` constant (move from `compile_time_continuation_include.go`)

Imports: `environment`, `security`, `werr`, stdlib only. No `LibraryName`, no compilation types.

The walk helpers' callback signature stays `func(relPath string)` — unchanged. Callers collect paths into a slice.

```go
package resolver

import (
	"context"
	"errors"
	"io/fs"
	"os"
	pathpkg "path"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

const (
	// SchemeIncludePathEnv is the environment variable name for the Scheme include path.
	SchemeIncludePathEnv = "SCHEME_INCLUDE_PATH"
)

// IsSchemeFile reports whether the filename has a .sld or .scm extension.
func IsSchemeFile(name string) bool {
	return strings.HasSuffix(name, ".sld") || strings.HasSuffix(name, ".scm")
}

// isHidden reports whether the name starts with ".".
func isHidden(name string) bool {
	return len(name) > 0 && name[0] == '.'
}

// isAuthorized reports whether the security authorizer permits loading the
// given path. Returns true when no authorizer is configured (open sandbox).
func isAuthorized(auth security.Authorizer, target string) bool {
	return security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   target,
	}) == nil
}

// OSSearchDirs returns the fallback directory list for OS-based file search.
// Search order: library registry paths → SCHEME_INCLUDE_PATH → CWD.
func OSSearchDirs(env *environment.EnvironmentFrame) []string {
	var dirs []string
	reg := env.LibraryRegistry()
	if reg != nil {
		dirs = append(dirs, reg.GetSearchPaths()...)
	}
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath != "" {
		dirs = append(dirs, filepath.SplitList(includePath)...)
	}
	cwd, err := os.Getwd()
	if err == nil {
		dirs = append(dirs, cwd)
	}
	return dirs
}

// OpenAuthorized performs security authorization then opens absPath on the OS filesystem.
func OpenAuthorized(auth security.Authorizer, absPath string) (fs.File, string, error) {
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return nil, "", err
	}
	f, err := os.Open(absPath)
	if err != nil {
		sentinel := werr.ErrFileOpen
		if errors.Is(err, os.ErrNotExist) {
			sentinel = werr.ErrFileNotFound
		}
		return nil, "", werr.WrapForeignErrorWithCause(sentinel, err, "open %s", absPath)
	}
	return f, absPath, nil
}

// WalkOSSchemeFiles walks baseDir on the OS filesystem, calling fn with the
// slash-separated path of each .sld/.scm file relative to baseDir.
// Hidden directories and unauthorized files are silently skipped.
func WalkOSSchemeFiles(baseDir string, auth security.Authorizer, fn func(relPath string)) error {
	return filepath.WalkDir(baseDir, func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return filepath.SkipDir
			}
			if path != baseDir && isHidden(d.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		absPath, absErr := filepath.Abs(path)
		if walkErr != nil || absErr != nil || !IsSchemeFile(d.Name()) || !isAuthorized(auth, absPath) {
			return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
		}
		rel, relErr := filepath.Rel(baseDir, path)
		if relErr == nil {
			fn(filepath.ToSlash(rel))
		}
		return nil
	})
}

// WalkFSSchemeFiles walks baseDir in fsys, calling fn with the path of each
// .sld/.scm file relative to baseDir. Hidden directories and unauthorized
// files are silently skipped. Non-existent directories are skipped (fs.SkipAll).
// If skipSubdir is non-nil, subdirectory paths returning true are also skipped.
func WalkFSSchemeFiles(fsys fs.FS, baseDir string, auth security.Authorizer, skipSubdir func(string) bool, fn func(relPath string)) error {
	prefix := baseDir
	if prefix == "." {
		prefix = ""
	}
	return fs.WalkDir(fsys, baseDir, func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return fs.SkipDir
			}
			if path != baseDir && (isHidden(d.Name()) || (skipSubdir != nil && skipSubdir(path))) {
				return fs.SkipDir
			}
			return nil
		}
		if walkErr != nil || !IsSchemeFile(d.Name()) {
			return nil //nolint:nilerr // skip unreadable/irrelevant files, continue walking
		}
		relPath := strings.TrimPrefix(path, prefix+"/")
		if prefix == "" {
			relPath = path
		}
		if !isAuthorized(auth, relPath) {
			return nil
		}
		fn(relPath)
		return nil
	})
}
```

**Step 3: Create the four resolver files**

Each file contains one resolver type, its constructor, `ResolveAndOpen`, and `EnumerateFiles`. These are direct ports from `machine/compilation/file_resolver.go` with `LibraryName`/`FilePathToLibraryName` references removed.

`os_file_resolver.go`:
- `OSFileResolver` struct + `NewOSFileResolver` + `ResolveAndOpen` — unchanged logic
- `EnumerateFiles` — same walk logic as current `EnumerateLibraries`, but collects `relPath` strings instead of converting to `LibraryName`. No dedup (raw paths).

`embed_file_resolver.go`:
- `EmbedFileResolver` struct + `NewEmbedFileResolver` + `ResolveAndOpen` — unchanged
- No `EnumerateFiles` (EmbedFileResolver doesn't implement `FileEnumerator`, matching current behavior where it doesn't implement `LibraryEnumerator`)

`fs_file_resolver.go`:
- `FSFileResolver` struct + `NewFSFileResolver` + `ResolveAndOpen` + `fsDirs` + `openChecked` — unchanged
- `EnumerateFiles` — same walk logic, collects paths, no dedup

`chain_file_resolver.go`:
- `ChainFileResolver` struct + `NewChainFileResolver` + `ResolveAndOpen` — unchanged
- `EnumerateFiles` — iterates child resolvers, collects from those implementing `FileEnumerator`. Concatenates results (no dedup — preserves priority ordering, caller handles library-level identity).

**Step 4: Verify the new package compiles**

Run: `go build ./machine/compilation/resolver/...`
Expected: PASS

**Step 5: Commit**

```
feat: add machine/compilation/resolver package

Extracts file resolution infrastructure from compilation. Four resolver
types with FileEnumerator interface returning file paths instead of
library names — library-level interpretation stays in compilation.
```

---

## Task 2: Add resolver tests

**Files:**
- Create: `machine/compilation/resolver/resolver_test.go`

Port tests from `machine/compilation/file_resolver_test.go` to the new package. Key changes:

1. Package is `resolver` (not `compilation`) — tests use unexported helpers directly
2. Enumeration tests assert `[]string` paths instead of `LibraryName` keys
3. `LibraryEnumerator` interface compliance checks become `FileEnumerator`
4. `realDir` helper moves to the new test file
5. `SchemeIncludePathEnv` references use package-local constant

**Step 1: Port resolution tests (OSFileResolver, EmbedFileResolver, FSFileResolver, ChainFileResolver)**

All `ResolveAndOpen` tests port directly — the resolution interface is unchanged.

**Step 2: Port enumeration tests with updated assertions**

Example: `TestFSFileResolverEnumerateLibraries` becomes `TestFSFileResolverEnumerateFiles`:

```go
func TestFSFileResolverEnumerateFiles(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"scheme/base.sld":  &fstest.MapFile{Data: []byte("")},
		"scheme/write.sld": &fstest.MapFile{Data: []byte("")},
		"chibi/test.scm":   &fstest.MapFile{Data: []byte("")},
		".hidden/lib.sld":  &fstest.MapFile{Data: []byte("skip")},
		"readme.txt":       &fstest.MapFile{Data: []byte("not a library")},
	}

	ns := environment.NewNamespace()
	env := ns.Runtime()

	fr := NewFSFileResolver(fsys, env)
	files, err := fr.EnumerateFiles()
	c.Assert(err, qt.IsNil)

	// Returns paths, not library names. No dedup on extension.
	sort.Strings(files)
	c.Assert(files, qt.DeepEquals, []string{
		"chibi/test.scm",
		"scheme/base.sld",
		"scheme/write.sld",
	})
}
```

The `.sld beats .scm` test changes: `EnumerateFiles` returns BOTH paths; dedup is the caller's job. Test asserts both paths are returned.

**Step 3: Port interface compliance tests**

```go
func TestFileResolverInterfaceCompliance(t *testing.T) {
	var _ environment.FileResolver = (*OSFileResolver)(nil)
	var _ environment.FileResolver = (*EmbedFileResolver)(nil)
	var _ environment.FileResolver = (*FSFileResolver)(nil)
	var _ environment.FileResolver = (*ChainFileResolver)(nil)

	var _ FileEnumerator = (*FSFileResolver)(nil)
	var _ FileEnumerator = (*OSFileResolver)(nil)
	var _ FileEnumerator = (*ChainFileResolver)(nil)
}
```

**Step 4: Run tests**

Run: `go test -v ./machine/compilation/resolver/...`
Expected: PASS

**Step 5: Commit**

```
test: add resolver package tests

Ports file_resolver_test.go to the new package with updated
enumeration assertions (file paths instead of LibraryName keys).
```

---

## Task 3: Wire compilation to use resolver package

**Files:**
- Create: `machine/compilation/resolver_compat.go` (backward-compat aliases)
- Modify: `machine/compilation/compile_time_continuation_include.go` (constant reference)
- Modify: `machine/compilation/library_discovery.go` (use `FileEnumerator` + convert)
- Modify: `machine/compilation/library_export_index.go` (use `FileEnumerator` + convert)

**Step 1: Create `resolver_compat.go` with type aliases and constructor re-exports**

```go
package compilation

import "github.com/aalpar/wile/machine/compilation/resolver"

// Backward-compatible type aliases for resolver types.
// External callers (engine.go, options.go, bootstrap) continue using
// compilation.NewOSFileResolver etc. without import changes.

type OSFileResolver = resolver.OSFileResolver
type EmbedFileResolver = resolver.EmbedFileResolver
type FSFileResolver = resolver.FSFileResolver
type ChainFileResolver = resolver.ChainFileResolver

var (
	NewOSFileResolver    = resolver.NewOSFileResolver
	NewEmbedFileResolver = resolver.NewEmbedFileResolver
	NewFSFileResolver    = resolver.NewFSFileResolver
	NewChainFileResolver = resolver.NewChainFileResolver
)
```

Note: `FileResolver` stays as `type FileResolver = environment.FileResolver` — unchanged.

**Step 2: Update `SchemeIncludePathEnv` reference**

In `compile_time_continuation_include.go`, replace the constant definition with an import:

```go
const SchemeIncludePathEnv = resolver.SchemeIncludePathEnv
```

Or just re-export. The constant was defined there but only used by `file_resolver.go` (which is moving) and test files. Keep a re-export for external callers.

**Step 3: Replace `LibraryEnumerator` usage in `library_discovery.go`**

Before:
```go
enumerator, ok := resolver.(LibraryEnumerator)
if ok {
    libs, err := enumerator.EnumerateLibraries()
    ...
    for _, lib := range libs {
        key := lib.Key()
        ...
    }
}
```

After:
```go
fileEnum, ok := res.(resolver.FileEnumerator)
if ok {
    files, err := fileEnum.EnumerateFiles()
    ...
    for _, path := range files {
        name, nameErr := FilePathToLibraryName(path)
        if nameErr != nil {
            continue
        }
        key := name.Key()
        if !seen[key] {
            seen[key] = true
            result = append(result, name)
        }
    }
}
```

**Step 4: Replace `LibraryEnumerator` usage in `library_export_index.go`**

Same pattern: `FileEnumerator` → `EnumerateFiles` → `FilePathToLibraryName` → loop.

**Step 5: Run tests**

Run: `make lint && go test ./machine/compilation/...`
Expected: PASS (old tests still work via aliases, callers updated)

**Step 6: Commit**

```
refactor: wire compilation to use resolver package

Adds backward-compat type aliases. Replaces LibraryEnumerator with
resolver.FileEnumerator in library_discovery.go and library_export_index.go.
```

---

## Task 4: Delete old resolver code from compilation

**Files:**
- Delete: `machine/compilation/file_resolver.go`
- Delete: `machine/compilation/file_resolver_test.go`
- Modify: `environment/file_resolver.go` (update doc comment about where implementations live)

**Step 1: Delete `file_resolver.go`**

All types are now in `resolver/`. The `LibraryEnumerator` interface is gone (replaced by `resolver.FileEnumerator`). The shared helpers are in `resolver/helpers.go`.

**Step 2: Delete `file_resolver_test.go`**

Tests are now in `resolver/resolver_test.go`.

**Step 3: Update `environment/file_resolver.go` doc comment**

Change:
```go
// The concrete implementations (OSFileResolver, FSFileResolver,
// EmbedFileResolver, ChainFileResolver) live in machine/compilation/.
```

To:
```go
// The concrete implementations (OSFileResolver, FSFileResolver,
// EmbedFileResolver, ChainFileResolver) live in machine/compilation/resolver/.
```

**Step 4: Run full suite**

Run: `make lint && make covercheck`
Expected: PASS

**Step 5: Commit**

```
refactor: delete old file_resolver.go from compilation

Implementations moved to machine/compilation/resolver/.
LibraryEnumerator replaced by resolver.FileEnumerator.
```

---

## Task 5: Update TODO.md and plan status

**Files:**
- Modify: `TODO.md` (mark Task 8.1 done)
- Modify: `plans/TECH-DEBT-2026-04.md` (mark Task 8.1 done)
- Modify: `plans/TECH-DEBT-2026-04-IMPL.md` (mark Task 8.1 done)
- Modify: `plans/CLAUDE.md` (add this plan to completed section)

**Step 1: Update TODO.md**

Change Task 8.1 from `- [ ]` to `- [x]` with note:

```
- [x] **Task 8.1: Extract `machine/compilation/resolver/`** [Done]: FileResolver implementations extracted. `EnumerateLibraries` replaced with `FileEnumerator.EnumerateFiles` (returns paths, not LibraryName). Type aliases in compilation for backward compat. `plans/2026-04-13-resolver-extraction-impl.md`
```

**Step 2: Update TECH-DEBT plans**

Mark 8.1 as done in both assessment and impl plan.

**Step 3: Update plans/CLAUDE.md**

Move this plan entry to completed section.

**Step 4: Commit**

```
docs: mark resolver extraction complete
```

---

## Execution Summary

| Task | Description | Effort | Dependencies |
|------|-------------|--------|--------------|
| 1 | Create resolver package (types + helpers) | M | — |
| 2 | Port tests to resolver package | S | 1 |
| 3 | Wire compilation via aliases + migrate callers | M | 1 |
| 4 | Delete old code, update docs | S | 2, 3 |
| 5 | Update TODO/plan status | S | 4 |

**Total: 5 commits.** Tasks 1-2 can parallelize with task 3 prep work, but commits must be sequential.

## Open Questions

- **`isLibraryFile` → `IsSchemeFile` naming**: The function checks `.sld`/`.scm` extensions. "Scheme file" is accurate for a resolver that doesn't know about libraries. If a better name exists, apply it.
- **Export helpers or keep unexported**: `OSSearchDirs`, `OpenAuthorized`, `WalkOSSchemeFiles`, `WalkFSSchemeFiles` are currently unexported in compilation. They need to be exported from `resolver/` since the resolver types use them across files. Whether to export `IsSchemeFile` depends on whether external code needs it — currently no, but the walk functions use it internally.
