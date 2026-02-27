# REPL Enhancements Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add meta-commands (`,help`, `,doc`, `,edit`), tab completion, and pager support to the Wile REPL.

**Architecture:** Two-layer meta-command dispatch separates session commands from debug commands. A `DocProvider` interface decouples doc lookup from the registry package. A `SchemeCompleter` implements readline's `AutoCompleter` by walking all phase environments. Pager/editor integration shells out to `$PAGER`/`$EDITOR`.

**Tech Stack:** Go, `ergochat/readline` (existing dep), `os/exec` for subprocess spawning.

**Design doc:** `plans/REPL_ENHANCEMENTS.md`

---

## Task 1: Pager Helper

The simplest component with no dependencies on other new code. Used by `,help` and `,doc`.

**Files:**
- Create: `internal/repl/pager.go`
- Create: `internal/repl/pager_test.go`

**Step 1: Write the test**

```go
// internal/repl/pager_test.go
package repl

import (
	"bytes"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteWithPager_NoPager(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	writeWithPager(&buf, "hello world", "")
	c.Assert(buf.String(), qt.Equals, "hello world")
}

func TestWriteWithPager_EmptyContent(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	writeWithPager(&buf, "", "")
	c.Assert(buf.String(), qt.Equals, "")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestWriteWithPager ./internal/repl/...`
Expected: FAIL — `writeWithPager` not defined.

**Step 3: Implement**

```go
// internal/repl/pager.go
package repl

import (
	"fmt"
	"io"
	"os/exec"
	"strings"
)

// writeWithPager writes content to out, piping through the given pager command
// if non-empty. If pager is empty, writes directly to out.
// The pager command string is split on spaces to support arguments (e.g. "less -R").
func writeWithPager(out io.Writer, content string, pager string) {
	if pager == "" || content == "" {
		fmt.Fprint(out, content)
		return
	}

	parts := strings.Fields(pager)
	cmd := exec.Command(parts[0], parts[1:]...)
	cmd.Stdin = strings.NewReader(content)
	cmd.Stdout = out
	cmd.Stderr = out

	err := cmd.Run()
	if err != nil {
		// Pager failed — fall back to direct write
		fmt.Fprint(out, content)
	}
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestWriteWithPager ./internal/repl/...`
Expected: PASS

**Step 5: Run linter**

Run: `make lint` (or `goimports -w internal/repl/pager.go internal/repl/pager_test.go`)

**Step 6: Commit**

```
feat(repl): add pager helper for meta-command output
```

---

## Task 2: DocProvider Interface and Registry Adapter

The `internal/repl` package needs doc lookup without importing `registry`. Define the interface in `internal/repl` and the adapter in a new bridge file.

**Files:**
- Create: `internal/repl/doc_provider.go`
- Create: `internal/repl/registry_doc_provider.go`
- Create: `internal/repl/registry_doc_provider_test.go`

**Step 1: Define the interface**

```go
// internal/repl/doc_provider.go
package repl

// DocInfo holds documentation for a primitive binding.
type DocInfo struct {
	Doc        string
	ParamNames []string
	Category   string
	ParamCount int
	IsVariadic bool
}

// DocProvider looks up documentation for named bindings.
type DocProvider interface {
	// LookupDoc returns documentation for the named primitive.
	// Returns found=false if no documentation exists.
	LookupDoc(name string) (info DocInfo, found bool)
}
```

**Step 2: Write the adapter test**

```go
// internal/repl/registry_doc_provider_test.go
package repl

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/registry"
)

func TestRegistryDocProvider_Found(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	reg.AddPrimitive(registry.PrimitiveSpec{
		Name:       "test-prim",
		ParamCount: 2,
		Doc:        "A test primitive.",
		ParamNames: []string{"a", "b"},
		Category:   "test",
	}, registry.PhaseRuntime)

	provider := NewRegistryDocProvider(reg)
	info, found := provider.LookupDoc("test-prim")
	c.Assert(found, qt.IsTrue)
	c.Assert(info.Doc, qt.Equals, "A test primitive.")
	c.Assert(info.ParamNames, qt.DeepEquals, []string{"a", "b"})
	c.Assert(info.Category, qt.Equals, "test")
}

func TestRegistryDocProvider_NotFound(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	provider := NewRegistryDocProvider(reg)
	_, found := provider.LookupDoc("nonexistent")
	c.Assert(found, qt.IsFalse)
}
```

**Step 3: Run test to verify it fails**

Run: `go test -v -run TestRegistryDocProvider ./internal/repl/...`
Expected: FAIL — `NewRegistryDocProvider` not defined.

**Step 4: Implement the adapter**

```go
// internal/repl/registry_doc_provider.go
package repl

import (
	"github.com/aalpar/wile/registry"
)

// RegistryDocProvider adapts a registry.Registry to the DocProvider interface.
type RegistryDocProvider struct {
	reg *registry.Registry
}

// NewRegistryDocProvider creates a DocProvider backed by the given registry.
func NewRegistryDocProvider(reg *registry.Registry) *RegistryDocProvider {
	return &RegistryDocProvider{reg: reg}
}

// LookupDoc returns documentation for the named primitive from the registry.
func (p *RegistryDocProvider) LookupDoc(name string) (DocInfo, bool) {
	pr, found := p.reg.FindPrimitive(name, 0) // phase 0 = any
	if !found {
		return DocInfo{}, false
	}
	return DocInfo{
		Doc:        pr.Spec.Doc,
		ParamNames: pr.Spec.ParamNames,
		Category:   pr.Spec.Category,
		ParamCount: pr.Spec.ParamCount,
		IsVariadic: pr.Spec.IsVariadic,
	}, true
}
```

**Step 5: Run test to verify it passes**

Run: `go test -v -run TestRegistryDocProvider ./internal/repl/...`
Expected: PASS

**Step 6: Lint and commit**

```
feat(repl): add DocProvider interface and registry adapter
```

---

## Task 3: Expose Registry from Bootstrap

Currently `initializeEnvironment` creates the registry, applies it, and drops it. We need to keep it alive for `DocProvider`.

**Files:**
- Modify: `internal/bootstrap/environment_tiny.go` — return registry from `initializeEnvironment`, store on return path
- Create: `internal/bootstrap/registry_access.go` — expose registry via a package-level accessor or return value
- Modify: `cmd/scheme/main.go` — capture registry, wire DocProvider

**Step 1: Write the test**

```go
// In existing internal/bootstrap/environment_tiny_test.go or a new test
func TestNewTopLevelEnvironmentFrameTiny_ReturnsRegistry(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, reg, err := NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)
	c.Assert(env, qt.IsNotNil)
	c.Assert(reg, qt.IsNotNil)
	// Verify the registry has primitives
	_, found := reg.FindPrimitive("+", 0)
	c.Assert(found, qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNewTopLevelEnvironmentFrameTiny_ReturnsRegistry ./internal/bootstrap/...`
Expected: FAIL — `NewTopLevelWithRegistry` not defined.

**Step 3: Implement**

Add `NewTopLevelWithRegistry` to `internal/bootstrap/environment_tiny.go` that wraps the existing flow but returns the registry:

```go
// NewTopLevelWithRegistry creates a top-level environment and returns both
// the environment frame and the primitive registry for doc introspection.
func NewTopLevelWithRegistry(ctx context.Context) (*environment.EnvironmentFrame, *registry.Registry, error) {
	env := environment.NewTopLevelEnvironmentFrame()
	reg, err := initializeEnvironmentWithRegistry(ctx, env)
	if err != nil {
		return nil, nil, err
	}
	return env, reg, nil
}
```

Refactor `initializeEnvironment` to return the registry:

```go
func initializeEnvironmentWithRegistry(ctx context.Context, env *environment.EnvironmentFrame) (*registry.Registry, error) {
	reg := registry.NewRegistry()
	// ... existing body, same as initializeEnvironment ...
	return reg, nil
}

func initializeEnvironment(ctx context.Context, env *environment.EnvironmentFrame) error {
	_, err := initializeEnvironmentWithRegistry(ctx, env)
	return err
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestNewTopLevelEnvironmentFrameTiny_ReturnsRegistry ./internal/bootstrap/...`
Expected: PASS

**Step 5: Run full test suite**

Run: `go test ./internal/bootstrap/...`
Expected: All existing tests still pass (initializeEnvironment unchanged in behavior).

**Step 6: Lint and commit**

```
feat(bootstrap): expose primitive registry for doc introspection
```

---

## Task 4: MetaCommandHandler — Core Dispatch

The central dispatcher that replaces `DebugContext.HandleDebugCommand` in the REPL loop.

**Files:**
- Create: `internal/repl/meta.go`
- Create: `internal/repl/meta_test.go`

**Step 1: Write the test**

```go
// internal/repl/meta_test.go
package repl

import (
	"bytes"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestMetaCommandHandler_UnknownCommand(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	handled := h.Handle(",bogus", &buf)
	c.Assert(handled, qt.IsTrue)
	c.Assert(buf.String(), qt.Matches, `.*Unknown command.*`)
}

func TestMetaCommandHandler_EmptyComma(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	handled := h.Handle(",", &buf)
	c.Assert(handled, qt.IsTrue)
}

func TestMetaCommandHandler_NotACommand(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	handled := h.Handle("(+ 1 2)", &buf)
	c.Assert(handled, qt.IsFalse)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestMetaCommandHandler ./internal/repl/...`
Expected: FAIL

**Step 3: Implement core dispatch**

```go
// internal/repl/meta.go
package repl

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/aalpar/wile/environment"
)

// MetaCommandHandler dispatches comma-prefixed meta-commands.
type MetaCommandHandler struct {
	env      *environment.EnvironmentFrame
	debugCtx *DebugContext
	docProv  DocProvider
}

// NewMetaCommandHandler creates a new meta-command handler.
func NewMetaCommandHandler(
	env *environment.EnvironmentFrame,
	debugCtx *DebugContext,
	docProv DocProvider,
) *MetaCommandHandler {
	return &MetaCommandHandler{
		env:      env,
		debugCtx: debugCtx,
		docProv:  docProv,
	}
}

// Handle processes a line starting with ",". Returns true if the line was
// a meta-command (even if unrecognized), false if it's not a meta-command.
func (p *MetaCommandHandler) Handle(line string, out io.Writer) bool {
	line = strings.TrimSpace(line)
	if !strings.HasPrefix(line, ",") {
		return false
	}

	parts := strings.Fields(strings.TrimPrefix(line, ","))
	if len(parts) == 0 {
		return true
	}

	cmd := parts[0]
	args := parts[1:]

	switch cmd {
	// Session commands
	case "help", "h", "?":
		p.cmdHelp(args, out)
	case "doc":
		p.cmdDoc(args, out)
	case "edit":
		p.cmdEdit(args, out)
	default:
		// Delegate to debug context
		if p.debugCtx != nil && p.debugCtx.HandleDebugCommand(line, out) {
			return true
		}
		fmt.Fprintf(out, "Unknown command: %s (type ,help for commands)\n", cmd)
	}

	return true
}

// Commands returns the list of session meta-command names (for autocomplete).
func (p *MetaCommandHandler) Commands() []string {
	return []string{
		"help", "doc", "edit",
		// Debug commands
		"break", "delete", "list", "enable", "disable",
		"step", "next", "finish", "continue",
		"backtrace", "where",
	}
}
```

Note: `cmdHelp`, `cmdDoc`, `cmdEdit` are stubs at this point — they'll be implemented in the next tasks. Add minimal stubs:

```go
func (p *MetaCommandHandler) cmdHelp(args []string, out io.Writer) {
	fmt.Fprintln(out, "TODO: help")
}

func (p *MetaCommandHandler) cmdDoc(args []string, out io.Writer) {
	fmt.Fprintln(out, "TODO: doc")
}

func (p *MetaCommandHandler) cmdEdit(args []string, out io.Writer) {
	fmt.Fprintln(out, "TODO: edit")
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestMetaCommandHandler ./internal/repl/...`
Expected: PASS

**Step 5: Lint and commit**

```
feat(repl): add MetaCommandHandler with two-layer dispatch
```

---

## Task 5: Wire MetaCommandHandler into REPL Loop

Replace the direct `DebugContext.HandleDebugCommand` call with `MetaCommandHandler.Handle`.

**Files:**
- Modify: `internal/repl/repl.go` — add `metaHandler` field to REPL, option for DocProvider, wire in Run()

**Step 1: Write the test**

The existing REPL tests should still pass. Add a test that verifies meta-commands are dispatched:

```go
// Test that the REPL properly delegates comma commands
// (this is integration-level; verify by running existing repl tests)
```

Since the REPL's `Run()` method reads from readline (interactive), testing is best done by verifying existing tests pass and the wiring compiles.

**Step 2: Modify REPL struct**

Add to `REPL` struct:
```go
metaHandler *MetaCommandHandler
```

Add option:
```go
func WithDocProvider(dp DocProvider) Option {
	return func(r *REPL) {
		r.docProvider = dp
	}
}
```

In `New()`, construct `MetaCommandHandler`:
```go
r.metaHandler = NewMetaCommandHandler(r.env, r.debugCtx, r.docProvider)
```

In `Run()`, replace line 168:
```go
// Before:
p.debugCtx.HandleDebugCommand(trimmed, p.out)

// After:
p.metaHandler.Handle(trimmed, p.out)
```

**Step 3: Run full test suite**

Run: `go test ./internal/repl/...`
Expected: All tests pass.

**Step 4: Lint and commit**

```
refactor(repl): wire MetaCommandHandler into REPL loop
```

---

## Task 6: Implement `,edit`

**Files:**
- Modify: `internal/repl/meta.go` — implement `cmdEdit`
- Add test to: `internal/repl/meta_test.go`

**Step 1: Write the test**

```go
func TestCmdEdit_NoArgs(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdEdit(nil, &buf)
	c.Assert(buf.String(), qt.Matches, `.*Usage.*`)
}

func TestCmdEdit_NoEditor(t *testing.T) {
	c := qt.New(t)
	// Temporarily unset EDITOR
	prev := os.Getenv("EDITOR")
	os.Unsetenv("EDITOR")
	defer os.Setenv("EDITOR", prev)

	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdEdit([]string{"foo.scm"}, &buf)
	c.Assert(buf.String(), qt.Matches, `.*\$EDITOR.*not set.*`)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestCmdEdit ./internal/repl/...`
Expected: FAIL (stub returns "TODO")

**Step 3: Implement**

```go
func (p *MetaCommandHandler) cmdEdit(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,edit <file>")
		return
	}

	editor := os.Getenv("EDITOR")
	if editor == "" {
		fmt.Fprintln(out, "Error: $EDITOR is not set")
		return
	}

	parts := strings.Fields(editor)
	cmdArgs := append(parts[1:], args[0])
	cmd := exec.Command(parts[0], cmdArgs...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		fmt.Fprintf(out, "Editor exited with error: %v\n", err)
	}
}
```

Requires adding `"os/exec"` to imports.

**Step 4: Run tests**

Run: `go test -v -run TestCmdEdit ./internal/repl/...`
Expected: PASS

**Step 5: Lint and commit**

```
feat(repl): implement ,edit meta-command with $EDITOR
```

---

## Task 7: Implement `,doc` — Phase Traversal and Display

The core doc lookup: walk phases, find binding, display doc or type+value.

**Files:**
- Modify: `internal/repl/meta.go` — implement `cmdDoc`
- Add tests to: `internal/repl/meta_test.go`

**Step 1: Write the tests**

```go
func TestCmdDoc_NoArgs(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdDoc(nil, &buf)
	c.Assert(buf.String(), qt.Matches, `.*Usage.*`)
}

func TestCmdDoc_WithDocProvider(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)

	docProv := NewRegistryDocProvider(reg)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(env, nil, docProv)
	h.cmdDoc([]string{"+"}, &buf)
	// Should contain the doc string for +
	output := buf.String()
	c.Assert(output, qt.Not(qt.Equals), "")
	c.Assert(strings.Contains(output, "+"), qt.IsTrue)
}

func TestCmdDoc_UnboundIdentifier(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)

	var buf bytes.Buffer
	h := NewMetaCommandHandler(env, nil, nil)
	h.cmdDoc([]string{"nonexistent-xyz"}, &buf)
	c.Assert(buf.String(), qt.Matches, `.*[Uu]nbound.*`)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestCmdDoc ./internal/repl/...`
Expected: FAIL

**Step 3: Implement**

The implementation needs:
1. A helper to walk phases and find the binding
2. Format output based on binding type
3. Pipe through pager

```go
func (p *MetaCommandHandler) cmdDoc(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,doc <name>")
		return
	}

	name := args[0]
	var content strings.Builder

	// Try DocProvider first (primitive registry docs)
	if p.docProv != nil {
		info, found := p.docProv.LookupDoc(name)
		if found {
			p.formatPrimitiveDoc(&content, name, info)
			writeWithPager(out, content.String(), os.Getenv("PAGER"))
			return
		}
	}

	// Walk phase environments for binding info
	if p.env != nil {
		phases := p.env.TopLevelEnv().Phases()
		phaseIndices := phases.Phases()
		sort.Ints(phaseIndices)

		sym := p.env.InternSymbol(values.NewSymbol(name))
		for _, phase := range phaseIndices {
			phaseEnv := phases.Get(phase)
			if phaseEnv == nil {
				continue
			}
			bnd := phaseEnv.GlobalEnvironment().GetOwnGlobalBinding(sym)
			if bnd != nil && !values.IsVoid(bnd) {
				p.formatBindingDoc(&content, name, bnd, phase)
				writeWithPager(out, content.String(), os.Getenv("PAGER"))
				return
			}
		}
	}

	fmt.Fprintf(out, "Unbound identifier: %s\n", name)
}

func (p *MetaCommandHandler) formatPrimitiveDoc(w *strings.Builder, name string, info DocInfo) {
	// Signature line: (name param1 param2 ...)
	fmt.Fprintf(w, "(%s", name)
	for _, pn := range info.ParamNames {
		fmt.Fprintf(w, " %s", pn)
	}
	if info.IsVariadic {
		fmt.Fprint(w, " ...")
	}
	fmt.Fprintln(w, ")")

	if info.Doc != "" {
		fmt.Fprintf(w, "  %s\n", info.Doc)
	}
	if info.Category != "" {
		fmt.Fprintf(w, "  Category: %s\n", info.Category)
	}
}

func (p *MetaCommandHandler) formatBindingDoc(w *strings.Builder, name string, bnd *environment.Binding, phase int) {
	phaseName := phaseLabel(phase)
	val := bnd.Value()

	switch bnd.BindingType() {
	case environment.BindingTypePrimitive:
		fmt.Fprintf(w, "%s: primitive (%s)\n", name, phaseName)
	case environment.BindingTypeSyntax:
		fmt.Fprintf(w, "%s: syntax transformer (%s)\n", name, phaseName)
	case environment.BindingTypeVariable:
		fmt.Fprintf(w, "%s: %s = %s\n", name, schemeTypeName(val), val.SchemeString())
	default:
		fmt.Fprintf(w, "%s: bound in %s\n", name, phaseName)
	}
}

func phaseLabel(phase int) string {
	switch phase {
	case 0:
		return "runtime"
	case 1:
		return "expand"
	case 2:
		return "compile"
	default:
		return fmt.Sprintf("phase %d", phase)
	}
}

func schemeTypeName(v values.Value) string {
	// Use Go type switch for Scheme type names
	switch v.(type) {
	case *values.Integer:
		return "integer"
	case *values.Rational:
		return "rational"
	case *values.BigFloat:
		return "inexact"
	// ... etc for main value types
	default:
		return fmt.Sprintf("%T", v)
	}
}
```

Note: The exact type switch cases in `schemeTypeName` should be written by checking what concrete types exist in the `values/` package. This is a good opportunity for the implementer to fill in the cases by reading `values/values.go` types.

**Step 4: Run tests**

Run: `go test -v -run TestCmdDoc ./internal/repl/...`
Expected: PASS

**Step 5: Lint and commit**

```
feat(repl): implement ,doc with phase traversal and pager
```

---

## Task 8: Implement `,help`

**Files:**
- Modify: `internal/repl/meta.go` — implement `cmdHelp`
- Add tests to: `internal/repl/meta_test.go`

**Step 1: Write the test**

```go
func TestCmdHelp_ListsCommands(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdHelp(nil, &buf)
	output := buf.String()
	c.Assert(strings.Contains(output, ",doc"), qt.IsTrue)
	c.Assert(strings.Contains(output, ",edit"), qt.IsTrue)
	c.Assert(strings.Contains(output, ",help"), qt.IsTrue)
	c.Assert(strings.Contains(output, ",break"), qt.IsTrue)
}

func TestCmdHelp_SpecificCommand(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdHelp([]string{"doc"}, &buf)
	output := buf.String()
	c.Assert(strings.Contains(output, ",doc"), qt.IsTrue)
	// Should have more detail than the list view
	c.Assert(strings.Contains(output, "binding"), qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestCmdHelp ./internal/repl/...`
Expected: FAIL

**Step 3: Implement**

Use a data-driven approach with a command metadata table:

```go
type commandInfo struct {
	name     string
	aliases  []string
	summary  string
	detail   string
	category string // "session" or "debug"
}

var metaCommands = []commandInfo{
	{"help", []string{"h", "?"}, "Show this help or help for a specific command",
		"Usage: ,help [command]\n\nWith no arguments, lists all commands.\nWith a command name, shows detailed help for that command.",
		"session"},
	{"doc", nil, "Show documentation for a Scheme binding",
		"Usage: ,doc <name>\n\nLooks up the named binding across all phase environments\n(runtime, expand, compile) and displays documentation.\nFor primitives, shows signature, description, and category.\nFor user bindings, shows type and current value.",
		"session"},
	{"edit", nil, "Open file in $EDITOR",
		"Usage: ,edit <file>\n\nOpens the given file in the editor specified by the $EDITOR\nenvironment variable. The REPL blocks until the editor exits.",
		"session"},
	{"break", []string{"b"}, "Set breakpoint at FILE:LINE[:COLUMN]", "Usage: ,break FILE:LINE[:COLUMN]", "debug"},
	{"delete", []string{"d"}, "Delete a breakpoint", "Usage: ,delete ID", "debug"},
	{"list", []string{"l"}, "List breakpoints", "Usage: ,list", "debug"},
	{"enable", nil, "Enable a breakpoint", "Usage: ,enable ID", "debug"},
	{"disable", nil, "Disable a breakpoint", "Usage: ,disable ID", "debug"},
	{"step", []string{"s"}, "Step into", "Usage: ,step", "debug"},
	{"next", []string{"n"}, "Step over", "Usage: ,next", "debug"},
	{"finish", []string{"f"}, "Step out", "Usage: ,finish", "debug"},
	{"continue", []string{"c"}, "Continue execution", "Usage: ,continue", "debug"},
	{"backtrace", []string{"bt"}, "Show stack trace", "Usage: ,backtrace", "debug"},
	{"where", nil, "Show current location", "Usage: ,where", "debug"},
}
```

`cmdHelp` formats the table grouped by category, or shows detail for a specific command. Pipe through `$PAGER`.

**Step 4: Run tests**

Run: `go test -v -run TestCmdHelp ./internal/repl/...`
Expected: PASS

**Step 5: Lint and commit**

```
feat(repl): implement ,help with grouped command listing
```

---

## Task 9: SchemeCompleter — Autocomplete

**Files:**
- Create: `internal/repl/completer.go`
- Create: `internal/repl/completer_test.go`

**Step 1: Write the tests**

```go
// internal/repl/completer_test.go
package repl

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/internal/bootstrap"
)

func TestSchemeCompleter_MetaCommand(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"help", "doc", "edit"})

	line := []rune(",he")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2) // "he" prefix
	// Should suggest "lp" (completing "help")
	c.Assert(len(newLines) > 0, qt.IsTrue)
}

func TestSchemeCompleter_SchemeBinding(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)

	sc := NewSchemeCompleter(env, nil)
	line := []rune("(ca")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2) // "ca" prefix
	// Should include completions like "r", "dr", "ar" (for car, cdr, caar, etc.)
	c.Assert(len(newLines) > 0, qt.IsTrue)
}

func TestSchemeCompleter_EmptyInput(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"help"})
	line := []rune("")
	newLines, _ := sc.Do(line, 0)
	// Empty input: no completions (don't dump everything)
	c.Assert(len(newLines), qt.Equals, 0)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestSchemeCompleter ./internal/repl/...`
Expected: FAIL

**Step 3: Implement**

```go
// internal/repl/completer.go
package repl

import (
	"sort"
	"strings"

	"github.com/aalpar/wile/environment"
)

// SchemeCompleter implements readline.AutoCompleter for the Wile REPL.
type SchemeCompleter struct {
	env          *environment.EnvironmentFrame
	metaCommands []string
}

// NewSchemeCompleter creates a completer that completes Scheme bindings
// from all phase environments and meta-command names.
func NewSchemeCompleter(
	env *environment.EnvironmentFrame,
	metaCommands []string,
) *SchemeCompleter {
	return &SchemeCompleter{
		env:          env,
		metaCommands: metaCommands,
	}
}

// Do implements readline.AutoCompleter.
func (p *SchemeCompleter) Do(line []rune, pos int) ([][]rune, int) {
	lineStr := string(line[:pos])

	// Context 1: after "," — complete meta-command names
	if strings.HasPrefix(lineStr, ",") {
		prefix := lineStr[1:] // strip the ","
		return p.completeFromList(prefix, p.metaCommands)
	}

	// Context 2: after ",edit " or ",load " — complete filenames
	// (defer to a future task or use readline's built-in file completer)

	// Context 3: complete Scheme bindings
	prefix := p.extractSymbolPrefix(lineStr)
	if prefix == "" {
		return nil, 0
	}

	names := p.collectBindingNames()
	return p.completeFromList(prefix, names)
}

// extractSymbolPrefix finds the Scheme symbol being typed at the cursor.
// Walks backward from the end of the line until hitting a delimiter.
func (p *SchemeCompleter) extractSymbolPrefix(line string) string {
	delimiters := " \t\n\r()[]{}\"';,`"
	i := len(line) - 1
	for i >= 0 && !strings.ContainsRune(delimiters, rune(line[i])) {
		i--
	}
	return line[i+1:]
}

// collectBindingNames walks all phase environments and returns unique binding names.
func (p *SchemeCompleter) collectBindingNames() []string {
	if p.env == nil {
		return nil
	}

	seen := make(map[string]bool)
	var names []string

	topLevel := p.env.TopLevelEnv()
	if topLevel == nil {
		return nil
	}

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

// completeFromList returns completions matching the given prefix.
func (p *SchemeCompleter) completeFromList(prefix string, candidates []string) ([][]rune, int) {
	var matches [][]rune
	for _, name := range candidates {
		if strings.HasPrefix(name, prefix) {
			suffix := name[len(prefix):]
			matches = append(matches, []rune(suffix))
		}
	}
	return matches, len(prefix)
}
```

Note: `sym.Key` — verify this is the correct field name for the symbol's string key by reading `values.Symbol`.

**Step 4: Run tests**

Run: `go test -v -run TestSchemeCompleter ./internal/repl/...`
Expected: PASS

**Step 5: Lint and commit**

```
feat(repl): add SchemeCompleter for tab completion
```

---

## Task 10: Wire Autocomplete into REPL

**Files:**
- Modify: `internal/repl/repl.go` — set `AutoComplete` on readline config

**Step 1: Implement**

In the `Run()` method where readline is configured (around line 105-111), add:

```go
// In readline.Config:
AutoComplete: p.completer,
```

Where `p.completer` is a `*SchemeCompleter` field on `REPL`, constructed in `New()`:

```go
r.completer = NewSchemeCompleter(r.env, r.metaHandler.Commands())
```

**Step 2: Run full test suite**

Run: `go test ./internal/repl/... && go test ./cmd/scheme/...`
Expected: All tests pass.

**Step 3: Lint and commit**

```
feat(repl): wire autocomplete into readline
```

---

## Task 11: Wire DocProvider in cmd/scheme

**Files:**
- Modify: `cmd/scheme/main.go` — use `NewTopLevelWithRegistry`, pass `DocProvider` to REPL

**Step 1: Implement**

In `main()`, replace:
```go
env, err0 := bootstrap.NewTopLevelEnvironmentFrameTiny(ctx)
```

With:
```go
env, primRegistry, err0 := bootstrap.NewTopLevelWithRegistry(ctx)
```

In `runREPL()`, pass DocProvider:
```go
func runREPL(ctx context.Context, env *environment.EnvironmentFrame, primRegistry *registry.Registry) {
	docProv := repl.NewRegistryDocProvider(primRegistry)
	r := repl.New(env, repl.WithDocProvider(docProv))
	err := r.Run(ctx)
	if err != nil {
		Failf(err, "REPL error")
	}
}
```

**Step 2: Build and smoke test**

Run: `make build && ./dist/scheme -i` then try `,help`, `,doc +`, `,doc car`
Expected: Correct output.

**Step 3: Lint and commit**

```
feat(cmd/scheme): wire DocProvider for ,doc support
```

---

## Task 12: Filename Completion for ,edit

**Files:**
- Modify: `internal/repl/completer.go` — add file completion context
- Add tests to: `internal/repl/completer_test.go`

**Step 1: Write the test**

```go
func TestSchemeCompleter_FileCompletion(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"edit"})
	// After ",edit " should attempt file completion
	line := []rune(",edit REA")
	newLines, length := sc.Do(line, len(line))
	// Should find README.md or similar
	c.Assert(length, qt.Equals, 3) // "REA" prefix
	_ = newLines // may or may not find matches depending on CWD
}
```

**Step 2: Implement**

In the `Do` method, after detecting `,edit ` or `,load ` prefix, use `filepath.Glob` or directory listing to complete filenames:

```go
// In Do(), before the generic binding completion:
for _, fileCmd := range []string{",edit ", ",load "} {
	if strings.HasPrefix(lineStr, fileCmd) {
		prefix := lineStr[len(fileCmd):]
		return p.completeFilenames(prefix)
	}
}
```

```go
func (p *SchemeCompleter) completeFilenames(prefix string) ([][]rune, int) {
	matches, _ := filepath.Glob(prefix + "*")
	var results [][]rune
	for _, m := range matches {
		suffix := m[len(prefix):]
		// Add trailing "/" for directories
		info, err := os.Stat(m)
		if err == nil && info.IsDir() {
			suffix += "/"
		}
		results = append(results, []rune(suffix))
	}
	return results, len(prefix)
}
```

**Step 3: Run tests**

Run: `go test -v -run TestSchemeCompleter ./internal/repl/...`
Expected: PASS

**Step 4: Lint and commit**

```
feat(repl): add filename completion for ,edit and ,load
```

---

## Task 13: Integration Smoke Test and Cleanup

**Files:**
- Run full test suite
- Manual smoke test of all features

**Step 1: Run all tests**

Run: `make test`
Expected: All tests pass.

**Step 2: Run linter**

Run: `make lint`
Expected: Clean.

**Step 3: Manual smoke test**

```
$ ./dist/scheme
> ,help
> ,help doc
> ,doc +
> ,doc car
> ,doc if
> ,doc nonexistent
> (define x 42)
> ,doc x
> ,edit test.scm     # with $EDITOR set
> <TAB>              # after typing "(ca" to verify completion
> ,<TAB>             # to verify meta-command completion
```

**Step 4: Final commit if any cleanup needed**

```
chore(repl): cleanup and polish REPL enhancements
```
