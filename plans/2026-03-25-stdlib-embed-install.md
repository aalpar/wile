# stdlib Embed + Engine Migration + make install

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Embed Scheme standard libraries into the binary via a shared `stdlib` package, migrate the CLI to the Engine API, and add a `make install` target that installs both binary and libraries.

**Architecture:** New `stdlib/` package holds `lib/` source of truth with `//go:embed`. CLI switches from manual `bootstrap.NewTopLevelWithRegistry` + `runtime.Compile/Run` to `wile.NewEngine` with `WithSourceFS(stdlib.FS)`. A new `AllExtensions()` function in the `wile` root package lists all extensions (parallel to existing `SafeExtensions()`). Makefile `install` target copies binary to `$GOBIN` and libraries to `$(PREFIX)/share/wile/lib`.

**Tech Stack:** Go `embed` package, existing `FileResolver` chain (`FSFileResolver` → `ChainFileResolver`), existing Engine options (`WithSourceFS`, `WithSourceOS`, `WithLibraryPaths`).

---

## Task 1: Move `lib/` → `stdlib/lib/` and Create Embed Package

**Files:**
- Create: `stdlib/stdlib.go`
- Move: `lib/` → `stdlib/lib/` (all `.sld`, `.scm`, and `CLAUDE.local.md` files)

**Step 1: Create the `stdlib/` directory and move libraries**

```bash
git mv lib/ stdlib/lib/
```

**Step 2: Create `stdlib/stdlib.go`**

```go
// Package stdlib provides the embedded R7RS standard library files.
//
// These libraries are compiled into the binary and available via
// [FS] for use with [wile.WithSourceFS]. Embedders who want
// zero-configuration library support can use this directly:
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithSourceFS(stdlib.FS),
//	    wile.WithSourceOS(),
//	    wile.WithLibraryPaths(),
//	)
package stdlib

import "embed"

//go:embed lib
var FS embed.FS
```

**Step 3: Add `stdlib/stdlib_test.go`**

Verify the embed contains expected files:

```go
package stdlib_test

import (
	"io/fs"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/stdlib"
)

func TestFS_ContainsSchemeBase(t *testing.T) {
	c := qt.New(t)
	f, err := stdlib.FS.Open("lib/scheme/base.sld")
	c.Assert(err, qt.IsNil)
	defer f.Close()
}

func TestFS_ContainsExpectedLibraries(t *testing.T) {
	c := qt.New(t)

	expected := []string{
		"lib/scheme/base.sld",
		"lib/scheme/write.sld",
		"lib/chibi/test.sld",
		"lib/srfi/1.sld",
		"lib/wile/kanren.sld",
	}
	for _, path := range expected {
		_, err := fs.Stat(stdlib.FS, path)
		c.Assert(err, qt.IsNil, qt.Commentf("missing: %s", path))
	}
}
```

**Step 4: Verify test passes**

Run: `go test ./stdlib/...`
Expected: PASS

**Step 5: Commit**

```
feat(stdlib): create stdlib package with embedded Scheme libraries

Move lib/ → stdlib/lib/ so library files are embedded into the binary
via //go:embed. Embedders and the CLI can use stdlib.FS with
WithSourceFS for zero-configuration library support.
```

---

## Task 2: Update All `lib/` References to `stdlib/lib/`

**Files:**
- Modify: `Makefile` (bench-kanren target, lines ~207-211)
- Modify: `examples/benchmarks/run-extended.sh:36` (`LIBDIR`)
- Modify: `examples/benchmarks/kanren-benchmark.scm:9` (comment)
- Modify: `integration/r7rs_test.go:43` (`getLibPath`)
- Modify: `fs_source_test.go` (6 occurrences of `WithLibraryPaths("lib")`)
- Modify: `internal/extensions/eval/load_path_integration_test.go:607`
- Modify: `docs/design/EMBEDDING.md:189`
- Modify: `machine/library_registry.go:135` (default `"./lib"` → `"./stdlib/lib"`)

**Step 1: Update `Makefile` bench-kanren references**

Change `SCHEME_LIBRARY_PATH=lib` → `SCHEME_LIBRARY_PATH=stdlib/lib` in the three bench-kanren lines.

**Step 2: Update `run-extended.sh`**

Line 36: `LIBDIR="$PROJECT_ROOT/lib"` → `LIBDIR="$PROJECT_ROOT/stdlib/lib"`

**Step 3: Update `integration/r7rs_test.go`**

```go
func getLibPath() string {
	return filepath.Join(getProjectRoot(), "stdlib", "lib")
}
```

**Step 4: Update `fs_source_test.go`**

All 6 occurrences: `WithLibraryPaths("lib")` → `WithLibraryPaths("stdlib/lib")`

**Step 5: Update `load_path_integration_test.go`**

Line 607: `wile.WithLibraryPaths("lib", libDir)` → `wile.WithLibraryPaths("stdlib/lib", libDir)`

**Step 6: Update `machine/library_registry.go` default path**

```go
var DefaultLibraryPaths = []string{
	".",
	"./stdlib/lib",
}
```

**Step 7: Update `docs/design/EMBEDDING.md` and `kanren-benchmark.scm` comment**

Update paths in documentation and comments.

**Step 8: Verify build and tests pass**

Run: `make build && go test ./... && make test-scheme`
Expected: All pass

**Step 9: Commit**

```
refactor: update all lib/ references to stdlib/lib/

Makefile, benchmarks, integration tests, and default library paths
now point to stdlib/lib/ after the library move.
```

---

## Task 3: Add `AllExtensions()` to Engine Options

**Files:**
- Modify: `options.go` (add imports + `AllExtensions` + `WithAllExtensions`)
- Create: `engine_all_extensions_test.go`

**Step 1: Write the test**

```go
package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
)

func TestAllExtensions_EngineCreation(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Verify a primitive from each extension category exists
	for _, name := range []string{
		"display",       // io
		"open-input-file", // files
		"sin",           // math
		"procedure-arity", // introspection
		"eval",          // eval
		"make-thread",   // threads
		"go-channel",    // gointerop
		"make-record-type", // all
		"command-line",  // system
	} {
		_, found := eng.Get(name)
		c.Assert(found, qt.IsTrue, qt.Commentf("missing: %s", name))
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -run TestAllExtensions_EngineCreation .`
Expected: FAIL — `AllExtensions` and `WithAllExtensions` undefined

**Step 3: Implement `AllExtensions()` and `WithAllExtensions()` in `options.go`**

Add imports for all extension packages (files, eval, namespace, threads, gointerop, system) and the two functions following the `SafeExtensions()` pattern:

```go
func AllExtensions() []EngineOption {
	return []EngineOption{
		WithExtension(io.Extension),
		WithExtension(files.Extension),
		WithExtension(math.Extension),
		WithExtension(introspection.Extension),
		WithExtension(eval.Extension),
		WithExtension(nsext.Extension),
		WithExtension(threads.Extension),
		WithExtension(gointerop.Extension),
		WithExtension(all.Extension),
		WithExtension(system.Extension),
	}
}

func WithAllExtensions() EngineOption {
	return func(cfg *engineConfig) {
		for _, opt := range AllExtensions() {
			opt(cfg)
		}
	}
}
```

The extension order matches `bootstrap.allExtensions` exactly.

**Step 4: Run test to verify it passes**

Run: `go test -run TestAllExtensions_EngineCreation .`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
feat(api): add AllExtensions() and WithAllExtensions() engine options

Parallel to SafeExtensions(), AllExtensions() provides the full set
of extensions (io, files, math, introspection, eval, namespace,
threads, gointerop, all, system) for creating a complete Scheme
runtime via the Engine API. The CLI migration will use this.
```

---

## Task 4: Engine Integration Test with Embedded stdlib

**Files:**
- Create: `engine_stdlib_test.go`

**Step 1: Write integration test**

Test that `NewEngine` + `WithSourceFS(stdlib.FS)` + `WithLibraryPaths()` can import and use standard libraries:

```go
package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/stdlib"
)

func TestEngine_EmbeddedStdlib_Import(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Import (scheme base) — should resolve from embedded FS
	result, err := eng.EvalMultiple(ctx, `
		(import (scheme base))
		(+ 1 2)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")
}

func TestEngine_EmbeddedStdlib_ChibiTest(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// Import (chibi test) — verifies nested embedded lib resolution
	result, err := eng.EvalMultiple(ctx, `
		(import (chibi test))
		(test-begin "embedded")
		(test-assert (= 1 1))
		(test-end)
	`)
	c.Assert(err, qt.IsNil)
	_ = result
}

func TestEngine_EmbeddedStdlib_SRFI1(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx,
		wile.WithAllExtensions(),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	result, err := eng.EvalMultiple(ctx, `
		(import (srfi 1))
		(fold + 0 '(1 2 3 4 5))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "15")
}
```

**Step 2: Run tests**

Run: `go test -run TestEngine_EmbeddedStdlib .`
Expected: PASS (if infrastructure is correct), or reveals issues to fix

**Step 3: Commit**

```
test: verify Engine with embedded stdlib resolves standard libraries

Integration tests confirm that WithSourceFS(stdlib.FS) enables
(import (scheme base)), (import (chibi test)), and (import (srfi 1))
from the embedded filesystem without SCHEME_LIBRARY_PATH.
```

---

## Task 5: Migrate CLI to Engine API

**Files:**
- Modify: `cmd/wile/main.go` (major rewrite of initialization and execution)

This is the largest task. The CLI currently:
1. Calls `bootstrap.NewTopLevelWithRegistry(ctx)` → returns `(env, registry, error)`
2. Manually creates `LibraryRegistry` via `initLibraryRegistry(ctx)`
3. Attaches it to env, sets `LibraryEnvFactory`
4. Uses `runtime.Load/Compile/Run` for file execution
5. Passes raw `env` + `registry` to REPL

After migration:
1. Calls `wile.NewEngine(ctx, opts...)` → returns `(*Engine, error)`
2. Library paths, embedded FS, extensions all configured via options
3. Uses `eng.EvalMultipleWithSource` for file/eval execution
4. Passes `eng.Environment()` and `eng.Registry()` to REPL

**Step 1: Rewrite `main()` initialization**

Replace lines ~225-238 with:

```go
ctx, cancel := context.WithCancel(context.Background())
defer cancel()

// Build library search paths: -L flag > SCHEME_LIBRARY_PATH > defaults
libPaths := buildLibraryPaths()

eng, err0 := wile.NewEngine(ctx,
    wile.WithAllExtensions(),
    wile.WithSourceFS(stdlib.FS),
    wile.WithSourceOS(),
    wile.WithLibraryPaths(libPaths...),
)
if err0 != nil {
    Failf(err0, "Cannot create engine")
}
defer eng.Close()
```

**Step 2: Extract `buildLibraryPaths()` from existing `initLibraryRegistry`**

```go
// buildLibraryPaths merges -L flag and SCHEME_LIBRARY_PATH into a path list.
// The Engine prepends these to DefaultLibraryPaths internally.
func buildLibraryPaths() []string {
    var paths []string

    // Environment variable paths
    envPath := os.Getenv(SchemeLibraryPathEnv)
    if envPath != "" {
        parts := strings.Split(envPath, string(os.PathListSeparator))
        for _, p := range parts {
            if p != "" {
                paths = append(paths, p)
            }
        }
    }

    // Command line paths (highest priority — prepend)
    if opts.LibraryPath != "" {
        var cmdPaths []string
        parts := strings.Split(opts.LibraryPath, string(os.PathListSeparator))
        for _, p := range parts {
            if p != "" {
                cmdPaths = append(cmdPaths, p)
            }
        }
        paths = append(cmdPaths, paths...)
    }

    return paths
}
```

Delete `initLibraryRegistry()` — no longer needed.

**Step 3: Rewrite `runFile()` to use Engine**

```go
func runFile(ctx context.Context, eng *wile.Engine, fin *bufio.Reader, filename string, shebang bool) {
    if shebang {
        peek, err := fin.Peek(2)
        if err == nil && peek[0] == '#' && peek[1] == '!' {
            _, _ = fin.ReadString('\n')
        }
    }

    absPath, absErr := filepath.Abs(filename)
    if absErr != nil {
        Failf(absErr, "cannot resolve path")
    }

    content, err := io.ReadAll(fin)
    if err != nil {
        Failf(err, "cannot read file")
    }

    var result wile.Value
    loadErr := eng.WithLoadPath(absPath, func() error {
        var evalErr error
        result, evalErr = eng.EvalMultipleWithSource(ctx, string(content), filename)
        return evalErr
    })
    if loadErr != nil {
        Failf(loadErr)
    }
    if result != nil && result.String() != "#<void>" {
        Printf("%s\n", result.String())
    }
}
```

**Step 4: Rewrite `runEval()` to use Engine**

```go
func runEval(ctx context.Context, eng *wile.Engine, exprs []string) {
    combined := strings.Join(exprs, "\n")
    result, err := eng.EvalMultipleWithSource(ctx, combined, "<eval>")
    if err != nil {
        Failf(err)
    }
    if result != nil && result.String() != "#<void>" {
        Printf("%s\n", result.String())
    }
}
```

**Step 5: Rewrite file loading loop**

Replace the `-f` file loading loop to use Engine. For intermediate files (loaded silently):

```go
loadErr := eng.WithLoadPath(absPath, func() error {
    _, err := eng.EvalMultipleWithSource(ctx, string(content), fn)
    return err
})
```

**Step 6: Update `runREPL()` to use Engine**

```go
func runREPL(ctx context.Context, eng *wile.Engine) {
    docProv := repl.NewRegistryDocProvider(eng.Registry())
    r := repl.New(eng.Environment(), repl.WithDocProvider(docProv))
    err := r.Run(ctx)
    if err != nil {
        Failf(err, "REPL error")
    }
}
```

**Step 7: Update imports**

Remove:
- `"github.com/aalpar/wile/environment"`
- `"github.com/aalpar/wile/internal/bootstrap"`
- `"github.com/aalpar/wile/internal/parser"`
- `"github.com/aalpar/wile/internal/syntax"`
- `"github.com/aalpar/wile/machine"`
- `"github.com/aalpar/wile/registry"`
- `"github.com/aalpar/wile/runtime"`

Add:
- `"github.com/aalpar/wile"`
- `"github.com/aalpar/wile/stdlib"`

Keep:
- `"github.com/aalpar/wile/extensions/system"` (for `system.SetCommandLine`)
- `"github.com/aalpar/wile/internal/repl"`

**Step 8: Update function signatures throughout**

All functions that took `env *environment.EnvironmentFrame` now take `eng *wile.Engine`.

**Step 9: Verify build and tests**

Run: `make build && make test && make smoke-test`
Expected: All pass

**Step 10: Verify benchmarks still work**

Run: `make bench-kanren` (uses `SCHEME_LIBRARY_PATH=stdlib/lib`)
Expected: PASS

**Step 11: Commit**

```
feat(cli): migrate to Engine API with embedded stdlib

Replace manual bootstrap.NewTopLevelWithRegistry + runtime.Compile/Run
with wile.NewEngine using WithSourceFS(stdlib.FS), WithSourceOS(),
WithAllExtensions(), and WithLibraryPaths(). Libraries now resolve
from the embedded filesystem by default — no SCHEME_LIBRARY_PATH
needed for standard libraries.

Removes initLibraryRegistry() and direct dependencies on internal
packages (bootstrap, parser, syntax, machine, runtime).
```

---

## Task 6: Update Makefile Install Target

**Files:**
- Modify: `Makefile` (rewrite `install` target)

**Step 1: Update install target**

Replace existing install target (lines 93-99) with:

```makefile
# Install prefix for non-Go files (libraries, data).
# Binary is always installed to GOBIN; libraries to PREFIX/share/wile/lib.
PREFIX ?= /usr/local
DATADIR = $(PREFIX)/share/wile

# Install the wile binary to GOBIN and standard libraries to PREFIX/share/wile/lib.
#   make install
#   make install PREFIX=/opt/wile
.PHONY: install
install: build
	@mkdir -p $(GOBIN)
	cp $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) $(GOBIN)/$(MY_BIN)
	@echo "Installed $(MY_BIN) to $(GOBIN)/$(MY_BIN)"
	@mkdir -p $(DATADIR)
	cp -R stdlib/lib $(DATADIR)/
	@echo "Installed libraries to $(DATADIR)/lib/"
```

**Step 2: Test install**

Run: `make install PREFIX=/tmp/wile-test`
Expected: Binary copied to `$GOBIN/wile`, libraries to `/tmp/wile-test/share/wile/lib/`

Verify: `ls /tmp/wile-test/share/wile/lib/scheme/base.sld` exists

**Step 3: Test that installed binary finds embedded libs without env vars**

```bash
$GOBIN/wile -e '(import (scheme base)) (display (+ 1 2)) (newline)'
```

Expected output: `3` (resolved from embedded FS, no SCHEME_LIBRARY_PATH needed)

**Step 4: Test that installed filesystem libs override embedded**

```bash
SCHEME_LIBRARY_PATH=/tmp/wile-test/share/wile/lib $GOBIN/wile -e '(import (srfi 1)) (display (iota 5)) (newline)'
```

Expected output: `(0 1 2 3 4)`

**Step 5: Commit**

```
feat(build): add library installation to make install

make install now copies stdlib/lib/ to $(PREFIX)/share/wile/lib/
in addition to the binary. PREFIX defaults to /usr/local.
Libraries are also embedded in the binary, so the filesystem
install is for overrides and discoverability.
```

---

## Task 7: Update `cmd/wile/main_test.go`

**Files:**
- Modify: `cmd/wile/main_test.go`

The existing tests for `initLibraryRegistry` and library path priority need updating
since `initLibraryRegistry` is removed. Replace with tests for `buildLibraryPaths()`.

**Step 1: Review existing tests**

Read `cmd/wile/main_test.go` to identify tests referencing `initLibraryRegistry`.

**Step 2: Update tests**

Replace `TestLibraryPathPriority` and `TestSchemeLibraryPathEnvConstant` to test
`buildLibraryPaths()` instead. The `SchemeLibraryPathEnv` constant test stays as-is.

**Step 3: Verify tests pass**

Run: `go test ./cmd/wile/...`
Expected: PASS

**Step 4: Run full CI check**

Run: `make lint && make covercheck`
Expected: Both pass

**Step 5: Commit**

```
test(cli): update main_test.go for Engine-based initialization

Replace initLibraryRegistry tests with buildLibraryPaths tests.
```

---

## Summary of Changes

| Component | Before | After |
|-----------|--------|-------|
| Library location | `lib/` | `stdlib/lib/` |
| Library access | `SCHEME_LIBRARY_PATH` required | Embedded in binary, zero-config |
| CLI initialization | `bootstrap.NewTopLevelWithRegistry` | `wile.NewEngine` with options |
| CLI execution | `runtime.Compile/Run` | `eng.EvalMultipleWithSource` |
| `make install` | Binary only | Binary + libraries |
| Engine API | `SafeExtensions()` only | `AllExtensions()` added |
| Embedder access | Must copy `lib/` or set env var | `stdlib.FS` with `WithSourceFS` |

## Risk Notes

- **`//go:embed lib`** embeds the directory tree. The `CLAUDE.local.md` files under `stdlib/lib/` will also be embedded. These are small and harmless, but could be excluded with a `.go` build tag or by removing them from lib/. Low priority.
- **`DefaultLibraryPaths`** change from `"./lib"` to `"./stdlib/lib"` affects any user running `wile` from the repo root without setting `SCHEME_LIBRARY_PATH`. This is the correct behavior post-migration.
- **REPL unchanged**: The REPL takes `env` + `DocProvider`. Engine provides both. No REPL changes needed.
- **`runtime` package**: Still exists and works. Not removed — just no longer used by the CLI. Other consumers (if any) are unaffected.
