# Public REPL API Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move the REPL from `internal/repl/` to a new public package `repl/` with an Engine-centric API, enabling external embedders to use REPL components independently.

**Architecture:** Six independently constructible components (REPL loop, meta-commands, completer, debug context, doc provider, pager) move to `repl/`. The compile/run path rewires from internal parser/compilation APIs to the public Engine API via three new Engine methods: `ReadExpression`, `IsIncompleteInput`, `SetDebugger`. The `internal/docparse` package promotes to `docparse/` (public) since both `engine.go` and `repl/meta.go` need it.

**Tech Stack:** Go, existing `wile` Engine API, `machine` package, `registry` package, `ergochat/readline`

**Design doc:** `plans/2026-04-07-public-repl-api-design.md`

---

## Phase 1: Engine API Additions

Add `ReadExpression`, `IsIncompleteInput`, and `SetDebugger` to the Engine. These are prerequisites for the REPL migration — the new public REPL package will call them instead of reaching into internal packages.

### Task 1.1: Add `IsIncompleteInput` to `wile` package

**Files:**
- Modify: `error.go`
- Test: `error_test.go`

**Step 1: Write the failing test**

Add to `error_test.go`:

```go
func TestIsIncompleteInput(t *testing.T) {
	tcs := []struct {
		name     string
		err      error
		expected bool
	}{
		{"nil error", nil, false},
		{"EOF", io.EOF, false},
		{"unexpected EOF", errors.New("unexpected EOF in list"), true},
		{"unterminated string", errors.New("unterminated string literal"), true},
		{"unclosed paren", errors.New("unclosed parenthesis"), true},
		{"plain error", errors.New("undefined variable"), false},
		{"wrapped incomplete", fmt.Errorf("parse: %w", errors.New("unexpected EOF")), true},
		{"compilation error wrapping incomplete", &CompilationError{
			Message: "parse error",
			Cause:   errors.New("unexpected EOF in list"),
		}, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, IsIncompleteInput(tc.err), qt.Equals, tc.expected)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestIsIncompleteInput ./...`
Expected: FAIL — `IsIncompleteInput` undefined

**Step 3: Write the implementation**

Add to `error.go`:

```go
// IsIncompleteInput reports whether a parse error indicates the input
// is a valid prefix of an expression that needs more input to complete.
// This is useful for REPL implementations that accumulate multi-line input.
//
// Returns true for errors containing "unexpected EOF", "unterminated",
// or "unclosed". Returns false for nil, plain io.EOF, and all other errors.
func IsIncompleteInput(err error) bool {
	if err == nil {
		return false
	}
	errStr := err.Error()
	return strings.Contains(errStr, "unexpected EOF") ||
		strings.Contains(errStr, "unterminated") ||
		strings.Contains(errStr, "unclosed")
}
```

Note: plain `io.EOF` means "no more input" (normal end), not "expression was cut off". The internal REPL's `isIncompleteInput` treats bare EOF as incomplete — but `ReadExpression` (Task 1.2) will wrap bare EOF in a `CompilationError` before the caller sees it, so the public API only needs to check error message strings.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestIsIncompleteInput ./...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

---

### Task 1.2: Add `ReadExpression` to Engine

**Files:**
- Modify: `expression.go`
- Test: `expression_test.go`

**Step 1: Write the failing test**

Add to `expression_test.go` (create if needed — check first):

```go
func TestReadExpression(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name       string
		input      string
		wantValue  string
		wantErr    bool
		incomplete bool
	}{
		{"simple atom", "42", "42", false, false},
		{"list", "(+ 1 2)", "(+ 1 2)", false, false},
		{"string", `"hello"`, `"hello"`, false, false},
		{"incomplete paren", "(+ 1", "", true, true},
		{"incomplete string", `"hello`, "", true, true},
		{"empty input", "", "", true, true},
		{"trailing ignored", "(+ 1 2) (+ 3 4)", "(+ 1 2)", false, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			r := strings.NewReader(tc.input)
			expr, err := eng.ReadExpression(ctx, r)
			if tc.wantErr {
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, wile.IsIncompleteInput(err), qt.Equals, tc.incomplete)
				return
			}
			qt.Assert(t, err, qt.IsNil)
			// Verify round-trip by compiling and running
			cc, compileErr := eng.Compile(ctx, expr)
			qt.Assert(t, compileErr, qt.IsNil)
			val, runErr := eng.Run(ctx, cc)
			qt.Assert(t, runErr, qt.IsNil)
			qt.Assert(t, val.SchemeString(), qt.Equals, tc.wantValue)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestReadExpression ./...`
Expected: FAIL — `ReadExpression` undefined

**Step 3: Write the implementation**

Add to `expression.go`:

```go
// ReadExpression reads a single complete expression from r.
//
// Unlike [Engine.Parse], ReadExpression does not require the reader to
// contain exactly one expression — it reads the first complete expression
// and stops. Trailing input in the reader is ignored (the reader position
// advances past the consumed expression).
//
// Use [IsIncompleteInput] to check whether a returned error indicates the
// input is a valid prefix of an expression that needs more input to complete.
// This is the intended pattern for REPL implementations:
//
//	expr, err := eng.ReadExpression(ctx, r)
//	if err != nil {
//	    if wile.IsIncompleteInput(err) {
//	        // prompt for more input
//	    }
//	    // real parse error
//	}
func (p *Engine) ReadExpression(ctx context.Context, r io.Reader) (*Expression, error) {
	pr := parser.NewParser(p.env, true, r)
	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, &CompilationError{Message: "parse error", Cause: err}
	}
	return &Expression{stx: stx}, nil
}
```

Add `"io"` to imports in `expression.go`.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestReadExpression ./...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

---

### Task 1.3: Add `SetDebugger` to Engine

**Files:**
- Modify: `engine.go`
- Test: `engine_test.go` (or `engine_debugger_test.go`)

**Step 1: Write the failing test**

Add test that verifies `SetDebugger` stores and `runCompiled` attaches it:

```go
func TestSetDebugger(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	dbg := machine.NewDebugger()
	eng.SetDebugger(dbg)

	// Setting nil clears it
	eng.SetDebugger(nil)
}
```

This is a minimal smoke test. The real integration test comes in Phase 3 when the REPL exercises it.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestSetDebugger ./...`
Expected: FAIL — `SetDebugger` undefined

**Step 3: Write the implementation**

Add field to `Engine` struct in `engine.go`:

```go
type Engine struct {
	namespace       *environment.Namespace
	env             *environment.EnvironmentFrame
	registry        *registry.Registry
	debugger        *machine.Debugger  // add this field
	lastCounters    machine.VMCounters
	closers         []registry.Closeable
	closed          bool
	maxCallDepth    uint64
	inlineThreshold int
}
```

Add method:

```go
// SetDebugger attaches a debugger to the engine. Subsequent [Engine.Run]
// calls will execute with the debugger active, enabling breakpoints and
// stepping. Pass nil to detach the debugger.
func (p *Engine) SetDebugger(d *machine.Debugger) {
	p.debugger = d
}
```

Modify `runCompiled` to attach the debugger when present:

```go
func (p *Engine) runCompiled(ctx context.Context, cc *CompiledCode) (Value, error) {
	mc := machine.AcquireTopLevelContext(ctx, cc.template, cc.env)
	defer machine.ReleaseTopLevelContext(mc)
	mc.SetMaxCallDepth(p.maxCallDepth)
	if p.debugger != nil {
		mc.SetDebugger(p.debugger)
	}

	err := mc.RunWithEscapeHandling()
	p.lastCounters = mc.Counters()
	val := mc.GetValue()
	if err != nil {
		return nil, p.wrapRuntimeError(err)
	}
	return wrapValue(val), nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestSetDebugger ./...`
Expected: PASS

**Step 5: Run full test suite and lint**

Run: `make lint && go test -v ./...`
Expected: PASS — existing tests must not break (debugger is nil by default)

---

## Phase 2: Promote `docparse` to Public Package

The `repl/meta.go` file imports `internal/docparse` for `ParseDocstring`. Since `repl/` is now public, it cannot import internal packages. Promote `docparse/` to a public package.

### Task 2.1: Move `internal/docparse/` to `docparse/`

**Files:**
- Move: `internal/docparse/docparse.go` → `docparse/docparse.go`
- Move: `internal/docparse/docparse_test.go` → `docparse/docparse_test.go`
- Modify: `engine.go` (update import path)

**Step 1: Move files**

```bash
mkdir -p docparse
cp internal/docparse/docparse.go docparse/docparse.go
cp internal/docparse/docparse_test.go docparse/docparse_test.go
```

No code changes to the moved files — the package name `docparse` stays the same.

**Step 2: Update import in `engine.go`**

Change:
```go
"github.com/aalpar/wile/internal/docparse"
```
To:
```go
"github.com/aalpar/wile/docparse"
```

**Step 3: Remove old `internal/docparse/`**

```bash
rm -rf internal/docparse/
```

**Step 4: Run tests and lint**

Run: `make lint && go test -v ./docparse/... ./...`
Expected: PASS

---

## Phase 3: Create Public `repl/` Package

Move all files from `internal/repl/` to `repl/`, rewriting to use Engine-centric API. This is the main phase.

### Task 3.1: Create `repl/doc.go` and `repl/doc_provider.go`

These files have no internal dependencies — they move unchanged.

**Files:**
- Create: `repl/doc.go` (from `internal/repl/doc.go`)
- Create: `repl/doc_provider.go` (from `internal/repl/doc_provider.go`)

**Step 1: Copy files**

```bash
mkdir -p repl
cp internal/repl/doc.go repl/doc.go
cp internal/repl/doc_provider.go repl/doc_provider.go
```

Update the package doc comment in `repl/doc.go` to reflect the public API:

```go
// Package repl provides composable components for building interactive
// Scheme REPLs on top of the Wile engine.
//
// Components can be used independently or composed into a full REPL:
//
//   - [REPL]: Full read-eval-print loop with readline support
//   - [MetaCommandHandler]: Comma-prefixed commands (,doc, ,apropos, etc.)
//   - [Completer]: Tab completion for bindings and commands
//   - [DebugContext]: Breakpoints, stepping, backtrace
//   - [DocProvider]: Documentation lookup interface
//   - [RegistryDocProvider]: Registry-backed documentation
//
// All components that need engine access take [*wile.Engine] at construction
// time. The REPL composes the other components with sensible defaults; embedders
// can construct and configure individual components for custom use.
package repl
```

**Step 2: Verify compilation**

Run: `go build ./repl/...`
Expected: PASS (these files only import `values/`)

---

### Task 3.2: Create `repl/pager.go`

No internal dependencies — moves unchanged.

**Files:**
- Create: `repl/pager.go` (from `internal/repl/pager.go`)
- Create: `repl/pager_test.go` (from `internal/repl/pager_test.go`)

**Step 1: Copy files**

```bash
cp internal/repl/pager.go repl/pager.go
cp internal/repl/pager_test.go repl/pager_test.go
```

**Step 2: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

---

### Task 3.3: Create `repl/registry_doc_provider.go`

No internal dependencies — moves unchanged.

**Files:**
- Create: `repl/registry_doc_provider.go` (from `internal/repl/registry_doc_provider.go`)
- Create: `repl/registry_doc_provider_test.go` (from `internal/repl/registry_doc_provider_test.go`)

**Step 1: Copy files**

```bash
cp internal/repl/registry_doc_provider.go repl/registry_doc_provider.go
cp internal/repl/registry_doc_provider_test.go repl/registry_doc_provider_test.go
```

**Step 2: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

---

### Task 3.4: Create `repl/debug.go`

No internal dependencies — moves unchanged. Only imports `machine/`.

**Files:**
- Create: `repl/debug.go` (from `internal/repl/debug.go`)
- Create: `repl/debug_test.go` (from `internal/repl/debug_test.go`)

**Step 1: Copy files**

```bash
cp internal/repl/debug.go repl/debug.go
cp internal/repl/debug_test.go repl/debug_test.go
```

**Step 2: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

---

### Task 3.5: Create `repl/completer.go` — Engine-centric rewrite

The completer currently takes `*environment.EnvironmentFrame`. Rewrite to take `*wile.Engine`.

**Files:**
- Create: `repl/completer.go`
- Create: `repl/completer_test.go`

**Step 1: Write the failing test**

```go
package repl_test

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/repl"
)

func TestCompleter_MetaCommand(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"help", "doc", "edit"})

	line := []rune(",he")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2)
	c.Assert(len(newLines) > 0, qt.IsTrue)
	c.Assert(string(newLines[0]), qt.Equals, "lp")
}

func TestCompleter_SchemeBinding(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	sc := repl.NewCompleter(eng, nil)
	line := []rune("(ca")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2)
	c.Assert(len(newLines) > 0, qt.IsTrue)
}

func TestCompleter_EmptyInput(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"help"})
	line := []rune("")
	newLines, _ := sc.Do(line, 0)
	c.Assert(len(newLines), qt.Equals, 0)
}

func TestCompleter_CommaOnly(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"help", "doc", "edit"})
	line := []rune(",")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 0)
	c.Assert(len(newLines), qt.Equals, 3)
}

func TestCompleter_FileCompletion(t *testing.T) {
	c := qt.New(t)
	sc := repl.NewCompleter(nil, []string{"edit"})

	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "foo.scm"), []byte(""), 0644)
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(dir, "foobar.scm"), []byte(""), 0644)
	c.Assert(err, qt.IsNil)

	prefix := filepath.Join(dir, "foo")
	line := []rune(",edit " + prefix)
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, len(prefix))
	c.Assert(len(newLines), qt.Equals, 2)
}
```

**Step 2: Write the implementation**

Create `repl/completer.go`. Key change: replace `env *environment.EnvironmentFrame` with `eng *wile.Engine`, and use `eng.Environment()` internally. `NewCompleter` accepts nil engine (for cases where only meta-command completion is needed).

```go
package repl

import (
	"os"
	"path/filepath"
	"sort"
	"strings"

	wile "github.com/aalpar/wile"
	"github.com/aalpar/wile/environment"
)

// Completer implements readline.AutoCompleter for a Wile REPL.
// It completes Scheme bindings, meta-command names, and filenames.
type Completer struct {
	eng          *wile.Engine
	metaCommands []string
}

// NewCompleter creates a completer. eng may be nil if only meta-command
// completion is needed.
func NewCompleter(eng *wile.Engine, metaCommands []string) *Completer {
	return &Completer{
		eng:          eng,
		metaCommands: metaCommands,
	}
}

// Do implements readline.AutoCompleter.
func (p *Completer) Do(line []rune, pos int) ([][]rune, int) {
	lineStr := string(line[:pos])

	if strings.HasPrefix(lineStr, ",edit ") {
		prefix := lineStr[len(",edit "):]
		return p.completeFilenames(prefix)
	}

	if strings.HasPrefix(lineStr, ",") {
		prefix := lineStr[1:]
		return p.completeFromList(prefix, p.metaCommands)
	}

	prefix := p.extractSymbolPrefix(lineStr)
	if prefix == "" {
		return nil, 0
	}

	names := p.collectBindingNames()
	return p.completeFromList(prefix, names)
}

func (p *Completer) extractSymbolPrefix(line string) string {
	delimiters := " \t\n\r()[]{}\"';,`"
	i := len(line) - 1
	for i >= 0 && !strings.ContainsRune(delimiters, rune(line[i])) {
		i--
	}
	return line[i+1:]
}

func (p *Completer) collectBindingNames() []string {
	if p.eng == nil {
		return nil
	}

	env := p.eng.Environment()
	if env == nil {
		return nil
	}

	topLevel := env.Namespace()
	if topLevel == nil {
		return nil
	}

	seen := make(map[string]bool)
	var names []string

	phases := topLevel.Phases()
	phaseIndices := phases.Phases()
	sort.Ints(phaseIndices)

	for _, phase := range phaseIndices {
		phaseEnv := phases.Get(phase)
		if phaseEnv == nil {
			continue
		}
		global := phaseEnv.GlobalEnvironment()
		if global == nil {
			continue
		}
		for sym := range global.Keys() {
			name := sym.Key
			if !seen[name] {
				seen[name] = true
				names = append(names, name)
			}
		}
	}

	sort.Strings(names)
	return names
}

func (p *Completer) completeFromList(prefix string, candidates []string) ([][]rune, int) {
	var matches [][]rune
	for _, name := range candidates {
		if strings.HasPrefix(name, prefix) {
			suffix := name[len(prefix):]
			matches = append(matches, []rune(suffix))
		}
	}
	return matches, len(prefix)
}

func (p *Completer) completeFilenames(prefix string) ([][]rune, int) {
	matches, _ := filepath.Glob(prefix + "*")
	var results [][]rune
	for _, m := range matches {
		suffix := m[len(prefix):]
		info, err := os.Stat(m)
		if err == nil && info.IsDir() {
			suffix += "/"
		}
		results = append(results, []rune(suffix))
	}
	return results, len(prefix)
}

// BindingNames returns all binding names visible in the environment.
func (p *Completer) BindingNames() []string {
	return p.collectBindingNames()
}
```

Note: the `environment` import is needed for the phase traversal. `environment` is a public package, so this is fine.

**Step 3: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

---

### Task 3.6: Create `repl/meta.go` — Engine-centric rewrite

The meta-command handler currently takes `*environment.EnvironmentFrame` and imports `internal/docparse`. Rewrite to take `*wile.Engine` and import public `docparse/`.

**Files:**
- Create: `repl/meta.go`
- Create: `repl/meta_test.go`

**Step 1: Copy the source file and rewrite**

Copy `internal/repl/meta.go` to `repl/meta.go`. Then apply these changes:

1. **Import path changes:**
   - `"github.com/aalpar/wile/internal/docparse"` → `"github.com/aalpar/wile/docparse"`
   - Remove `"github.com/aalpar/wile/environment"` — use `eng.Environment()` through the engine
   - Add `wile "github.com/aalpar/wile"` import

2. **Struct field change:**
   ```go
   type MetaCommandHandler struct {
       eng      *wile.Engine        // was: env *environment.EnvironmentFrame
       debugCtx *DebugContext
       docProv  DocProvider
       pager    string
   }
   ```

3. **Constructor change:**
   ```go
   type MetaOption func(*MetaCommandHandler)

   func WithMetaDocProvider(dp DocProvider) MetaOption {
       return func(h *MetaCommandHandler) {
           h.docProv = dp
       }
   }

   func NewMetaCommandHandler(eng *wile.Engine, opts ...MetaOption) *MetaCommandHandler {
       q := &MetaCommandHandler{
           eng:   eng,
           pager: os.Getenv("PAGER"),
       }
       for _, opt := range opts {
           opt(q)
       }
       return q
   }
   ```

4. **Environment access:** Everywhere the code uses `p.env`, replace with `p.eng.Environment()`. Add nil guard for `p.eng`:
   ```go
   if p.eng == nil {
       // handle gracefully
   }
   env := p.eng.Environment()
   ```

5. **`docparse` import:** Change from `internal/docparse` to `docparse`.

6. **`compilation` imports stay unchanged** — `machine/compilation` is already public.

**Step 2: Write tests**

Copy `internal/repl/meta_test.go` to `repl/meta_test.go`. Change package to `repl_test`. Rewrite all test helpers:

- Replace `bootstrap.NewTopLevelWithRegistry(ctx)` with `wile.NewEngine(ctx)`
- Replace `parser.NewParser` + `compile` + `run` with `eng.EvalMultiple(ctx, code)`
- Replace `NewMetaCommandHandler(env, debugCtx, docProv)` with `repl.NewMetaCommandHandler(eng, repl.WithMetaDocProvider(docProv))`
- Replace `NewRegistryDocProvider(reg)` with live-registry pattern: `reg, _ := eng.Environment().Namespace().Registry().(*registry.Registry)` then `repl.NewRegistryDocProvider(reg)`

Key test rewrites (representative):

```go
func newTestEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

func TestCmdDoc(t *testing.T) {
	eng := newTestEngine(t)
	reg, _ := eng.Environment().Namespace().Registry().(*registry.Registry)
	docProv := repl.NewRegistryDocProvider(reg)

	tcs := []struct {
		name    string
		args    string
		contain string
	}{
		{"no args", ",doc", "Usage"},
		{"primitive with doc", ",doc +", "+"},
		{"unbound identifier", ",doc nonexistent-xyz", "nbound"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := repl.NewMetaCommandHandler(eng, repl.WithMetaDocProvider(docProv))
			h.Handle(tc.input, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}
```

For the `TestCmdDoc_ClosureDocstring` test, replace the internal parser/compile/run with:

```go
func TestCmdDoc_ClosureDocstring(t *testing.T) {
	eng := newTestEngine(t)
	_, err := eng.EvalMultiple(context.Background(),
		`(define (f x) "Adds one to x." (+ x 1))`)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := repl.NewMetaCommandHandler(eng)
	h.Handle(",doc f", &buf)
	qt.Assert(t, strings.Contains(buf.String(), "Adds one to x."), qt.IsTrue,
		qt.Commentf(",doc should show closure docstring: %q", buf.String()))
}
```

**Step 3: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

---

### Task 3.7: Create `repl/repl.go` — Engine-centric rewrite

The main REPL loop. This is the biggest rewrite — replaces internal parser/compilation with Engine API.

**Files:**
- Create: `repl/repl.go`

**Step 1: Write the implementation**

Key structural changes from `internal/repl/repl.go`:

1. **Struct changes:**
   ```go
   type REPL struct {
       eng         *wile.Engine
       debugCtx    *DebugContext
       metaHandler *MetaCommandHandler
       completer   *Completer
       docProvider DocProvider
       historyFile string
       prompt      string
       contPrompt  string
       out         io.Writer
       errOut      io.Writer
   }
   ```

2. **Options:** Add `WithDebugContext` and `WithCompleter`:
   ```go
   func WithDebugContext(dc *DebugContext) Option {
       return func(r *REPL) {
           r.debugCtx = dc
       }
   }

   func WithCompleter(c *Completer) Option {
       return func(r *REPL) {
           r.completer = c
       }
   }
   ```

3. **Constructor:** Creates defaults for unprovided components:
   ```go
   func New(eng *wile.Engine, opts ...Option) *REPL {
       r := &REPL{
           eng:         eng,
           historyFile: defaultHistoryFile(),
           prompt:      "> ",
           contPrompt:  "  ",
           out:         os.Stdout,
           errOut:      os.Stderr,
       }
       for _, opt := range opts {
           opt(r)
       }
       // Defaults for unprovided components
       if r.debugCtx == nil {
           r.debugCtx = NewDebugContext()
       }
       if r.metaHandler == nil {
           var metaOpts []MetaOption
           if r.docProvider != nil {
               metaOpts = append(metaOpts, WithMetaDocProvider(r.docProvider))
           }
           r.metaHandler = NewMetaCommandHandler(eng, metaOpts...)
       }
       if r.completer == nil {
           r.completer = NewCompleter(eng, r.metaHandler.Commands())
       }
       return r
   }
   ```

4. **Read-eval-print loop** in `Run()`: Replace internal parse/compile/run:

   **Old:**
   ```go
   parser := parser.NewParser(p.env, true, rdr)
   stx, parseErr := parser.ReadSyntax(ctx)
   // ...
   tpl, compileErr := compile(ctx, p.env, stx)
   mv, runErr := p.runWithDebugger(ctx, tpl)
   ```

   **New:**
   ```go
   rdr := strings.NewReader(input)
   expr, parseErr := p.eng.ReadExpression(ctx, rdr)
   // ...
   cc, compileErr := p.eng.Compile(ctx, expr)
   mv, runErr := p.eng.Run(ctx, cc)
   ```

5. **Debugger integration** changes from per-run to session-scoped:

   **Old:** `runWithDebugger` creates a MachineContext and calls `mc.SetDebugger`.

   **New:** In `Run()`, at the start (before the loop), call:
   ```go
   p.eng.SetDebugger(p.debugCtx.Debugger())
   ```
   The Engine's `runCompiled` now attaches the debugger automatically.

6. **`RunSimple`:** Same rewrite — replace `run(ctx, tpl, p.env)` with `eng.Compile` + `eng.Run`.

7. **Delete `compile()` and `run()` helper functions** — replaced by Engine API.

8. **Delete `lineReader` type** — keep it (used by `RunSimple`).

9. **`isIncompleteInput`** calls become `wile.IsIncompleteInput`.

10. **Remove imports:** `internal/parser`, `internal/syntax`, `machine/compilation`, `environment`. Add `wile "github.com/aalpar/wile"`.

11. **`Run` result type:** `Engine.Run` returns `Value` (public wrapper), not `machine.MultipleValues`. Check for void: use `val == wile.Void` or `val.SchemeString()`. The Engine's `Run` returns `wrapValue(mc.GetValue())` which returns `Void` for void results. Check the exact comparison:

    ```go
    val, runErr := p.eng.Run(ctx, cc)
    if runErr != nil {
        fmt.Fprintf(p.errOut, "Exception: %v\n", runErr)
        continue
    }
    if val != wile.Void {
        fmt.Fprintln(p.out, val.SchemeString())
    }
    ```

    Verify: check how `Void` is exported. Search `engine.go` or `value.go` for `Void`.

**Step 2: Run compilation**

Run: `go build ./repl/...`
Expected: PASS

**Step 3: Run existing tests**

Run: `go test -v ./repl/...`
Expected: PASS (meta, completer, debug, pager, doc_provider tests all pass)

---

### Task 3.8: Verify `repl/` has no internal imports

**Step 1: Check imports**

Run: `grep -r 'github.com/aalpar/wile/internal' repl/`
Expected: No matches

If any remain, fix them before proceeding.

---

## Phase 4: Migrate Consumers

### Task 4.1: Update `cmd/wile/main.go`

**Files:**
- Modify: `cmd/wile/main.go`

**Step 1: Update import**

Change:
```go
"github.com/aalpar/wile/internal/repl"
```
To:
```go
"github.com/aalpar/wile/repl"
```

**Step 2: Update `runREPL`**

Change:
```go
func runREPL(ctx context.Context, eng *wile.Engine) {
	reg, _ := eng.Environment().Namespace().Registry().(*registry.Registry)
	docProv := repl.NewRegistryDocProvider(reg)
	r := repl.New(eng.Environment(), repl.WithDocProvider(docProv))
```
To:
```go
func runREPL(ctx context.Context, eng *wile.Engine) {
	reg, _ := eng.Environment().Namespace().Registry().(*registry.Registry)
	docProv := repl.NewRegistryDocProvider(reg)
	r := repl.New(eng, repl.WithDocProvider(docProv))
```

**Step 3: Run tests**

Run: `go test -v ./cmd/...`
Expected: PASS

---

### Task 4.2: Update `cmd/wile/mcp.go`

**Files:**
- Modify: `cmd/wile/mcp.go`

**Step 1: Update import**

Same import change as Task 4.1.

**Step 2: Update MetaCommandHandler construction**

Change (around line 230):
```go
reg, _ := eng.Environment().Namespace().Registry().(*registry.Registry)
	docProv := repl.NewRegistryDocProvider(reg)
p.meta = repl.NewMetaCommandHandler(eng.Environment(), nil, docProv)
```
To:
```go
reg, _ := eng.Environment().Namespace().Registry().(*registry.Registry)
	docProv := repl.NewRegistryDocProvider(reg)
p.meta = repl.NewMetaCommandHandler(eng, repl.WithMetaDocProvider(docProv))
```

**Step 3: Run MCP tests**

Run: `go test -v ./cmd/wile/...`
Expected: PASS

---

### Task 4.3: Delete `internal/repl/`

**Step 1: Verify no remaining imports**

Run: `grep -r 'internal/repl' --include='*.go' .`
Expected: No matches in `.go` files (plan files may still reference it — that's fine).

**Step 2: Delete the directory**

```bash
rm -rf internal/repl/
```

**Step 3: Full test suite**

Run: `make lint && go test -v ./...`
Expected: PASS

---

## Phase 5: Final Verification

### Task 5.1: Run full test suite and lint

**Step 1: Lint**

Run: `make lint`
Expected: PASS

**Step 2: Full tests**

Run: `go test -v ./...`
Expected: PASS

**Step 3: Coverage check**

Run: `make covercheck`
Expected: PASS

### Task 5.2: Verify public API from external perspective

**Step 1: Check that `repl/` is importable**

In a scratch file or mentally verify: all types in `repl/` constructors accept only public types (`*wile.Engine`, `*registry.Registry`, `*machine.Debugger`, `*machine.MachineContext`, `io.Writer`). No `internal/` types leak through the API surface.

**Step 2: Verify `docparse/` is importable**

The `docparse` package should have no internal imports:
Run: `grep -r 'internal' docparse/`
Expected: No matches

### Task 5.3: Update `internal/repl/CLAUDE.local.md` references

The `CLAUDE.local.md` file at `internal/repl/` was deleted with the directory. Check if any other CLAUDE files reference `internal/repl/` and update them:

Run: `grep -r 'internal/repl' CLAUDE*.md */CLAUDE*.md`

Update any stale references to point to `repl/` instead.
