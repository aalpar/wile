# REPL Decoupling Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all `machine/` and `machine/compilation/` imports from `repl/` by adding Engine-level abstractions.

**Architecture:** Add public types to `values/` (DebugState) and `wile/` (LibraryInfo, Debugger, BreakpointInfo). Add Engine methods (LookupLibrary, LoadedLibraries, UnloadedLibraries, FormLabel, DisassembleValue). Wrap `machine.Debugger` in `wile.Debugger`. Migrate `repl/` to use these abstractions exclusively.

**Tech Stack:** Go standard library only. No new dependencies.

**Design:** `plans/2026-04-11-repl-decoupling-design.md`

---

## Phase 1: Foundation Types in `values/`

### Task 1: Add SourceLocation and DebugState to values/

**Files:**
- Create: `values/debug.go`
- Create: `values/debug_test.go`

**Step 1: Write the test**

```go
// values/debug_test.go
package values_test

import (
    "testing"

    qt "github.com/frankban/quicktest"

    "github.com/aalpar/wile/values"
)

func TestSourceLocation_Fields(t *testing.T) {
    loc := &values.SourceLocation{File: "test.scm", Line: 10, Column: 5}
    qt.Assert(t, loc.File, qt.Equals, "test.scm")
    qt.Assert(t, loc.Line, qt.Equals, 10)
    qt.Assert(t, loc.Column, qt.Equals, 5)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestSourceLocation ./values/...`
Expected: FAIL — `values.SourceLocation` undefined

**Step 3: Write implementation**

```go
// values/debug.go
package values

// SourceLocation holds file/line/column for debug and error display.
type SourceLocation struct {
    File   string
    Line   int
    Column int
}

// DebugState provides read-only access to VM execution state.
// Implemented by the VM's MachineContext; consumed by presentation
// layers (REPL, debugger UI) without importing machine/.
type DebugState interface {
    // CurrentLocation returns the source location at the current
    // execution point, or nil if no source info is available.
    CurrentLocation() *SourceLocation

    // FormatStackTrace returns a human-readable stack trace string,
    // walking at most maxDepth frames.
    FormatStackTrace(maxDepth int) string
}
```

**Step 4: Run tests**

Run: `go test -v -run TestSourceLocation ./values/...`
Expected: PASS

**Step 5: Commit**

---

## Phase 2: MachineContext Implements DebugState

### Task 2: Add DebugState methods to MachineContext

**Files:**
- Modify: `machine/machine_context.go` (add two methods)
- Modify: `machine/machine_context_test.go` (or new test file)

**Step 1: Write the test**

Add to a test file in `machine/`:

```go
func TestMachineContext_DebugState(t *testing.T) {
    // MachineContext should satisfy values.DebugState
    var _ values.DebugState = (*MachineContext)(nil)
}

func TestMachineContext_CurrentLocation_Nil(t *testing.T) {
    // An empty MachineContext has no source location
    mc := &MachineContext{}
    qt.Assert(t, mc.CurrentLocation(), qt.IsNil)
}

func TestMachineContext_FormatStackTrace_Empty(t *testing.T) {
    mc := &MachineContext{}
    qt.Assert(t, mc.FormatStackTrace(10), qt.Equals, "")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestMachineContext_DebugState ./machine/...`
Expected: FAIL — `CurrentLocation` method missing

**Step 3: Write implementation**

Add to `machine/machine_context.go` near `CurrentSource()`:

```go
// CurrentLocation returns the current source location as a
// values.SourceLocation, or nil if no source info is available.
// Implements values.DebugState.
func (p *MachineContext) CurrentLocation() *values.SourceLocation {
    src := p.CurrentSource()
    if src == nil {
        return nil
    }
    return &values.SourceLocation{
        File:   src.File,
        Line:   src.Start.Line(),
        Column: src.Start.Column(),
    }
}

// FormatStackTrace returns a human-readable stack trace string.
// Implements values.DebugState.
func (p *MachineContext) FormatStackTrace(maxDepth int) string {
    trace := p.CaptureStackTrace(maxDepth)
    return trace.String()
}
```

**Step 4: Run tests**

Run: `go test -v -run TestMachineContext_DebugState ./machine/... && go test -v -run TestMachineContext_CurrentLocation ./machine/... && go test -v -run TestMachineContext_FormatStackTrace_Empty ./machine/...`
Expected: PASS

**Step 5: Commit**

---

## Phase 3: Engine Library Methods

### Task 3: Add LibraryInfo and Engine library methods

**Files:**
- Create: `library_info.go` (in `wile/` root)
- Create: `engine_library_test.go` (in `wile/` root)

The Engine already imports `machine/compilation/` and does the `LibraryRegistry`
type assertion in `AvailableLibraries()`. The new methods follow the same pattern.

**Step 1: Write the test**

```go
// engine_library_test.go
package wile_test

import (
    "context"
    "testing"

    qt "github.com/frankban/quicktest"

    "github.com/aalpar/wile"
    "github.com/aalpar/wile/stdlib"
)

func TestEngine_LoadedLibraries_Empty(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx)
    qt.Assert(t, err, qt.IsNil)
    libs := eng.LoadedLibraries()
    qt.Assert(t, len(libs), qt.Equals, 0)
}

func TestEngine_LoadedLibraries_AfterImport(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx,
        wile.WithAllExtensions(),
        wile.WithSourceFS(stdlib.FS),
        wile.WithLibraryPaths("."),
    )
    qt.Assert(t, err, qt.IsNil)

    _, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
    qt.Assert(t, err, qt.IsNil)

    libs := eng.LoadedLibraries()
    qt.Assert(t, len(libs) > 0, qt.IsTrue)

    // Find (scheme base)
    var found bool
    for _, lib := range libs {
        if lib.Name == "(scheme base)" {
            found = true
            qt.Assert(t, len(lib.Exports) > 0, qt.IsTrue)
        }
    }
    qt.Assert(t, found, qt.IsTrue,
        qt.Commentf("should find (scheme base) in loaded libraries"))
}

func TestEngine_LookupLibrary_NotFound(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx)
    qt.Assert(t, err, qt.IsNil)
    info := eng.LookupLibrary("nonexistent", "lib")
    qt.Assert(t, info, qt.IsNil)
}

func TestEngine_LookupLibrary_Found(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx,
        wile.WithAllExtensions(),
        wile.WithSourceFS(stdlib.FS),
        wile.WithLibraryPaths("."),
    )
    qt.Assert(t, err, qt.IsNil)

    _, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
    qt.Assert(t, err, qt.IsNil)

    info := eng.LookupLibrary("scheme", "base")
    qt.Assert(t, info, qt.IsNotNil)
    qt.Assert(t, info.Name, qt.Equals, "(scheme base)")
}

func TestEngine_UnloadedLibraries(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx,
        wile.WithAllExtensions(),
        wile.WithSourceFS(stdlib.FS),
        wile.WithLibraryPaths("."),
    )
    qt.Assert(t, err, qt.IsNil)

    // Don't import algebra — it should appear as unloaded
    libs := eng.UnloadedLibraries(ctx)
    var found bool
    for _, lib := range libs {
        if lib.Name == "(wile algebra)" {
            found = true
        }
    }
    qt.Assert(t, found, qt.IsTrue,
        qt.Commentf("should find (wile algebra) as unloaded"))
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestEngine_LoadedLibraries ./...`
Expected: FAIL — methods undefined

**Step 3: Write implementation**

```go
// library_info.go
package wile

import (
    "context"
    "sort"
    "sync"

    "github.com/aalpar/wile/machine/compilation"
)

// LibraryInfo holds read-only metadata about a Scheme library.
type LibraryInfo struct {
    Name        string   // Scheme representation, e.g. "(scheme base)"
    Description string
    SourceFile  string
    Exports     []string // sorted export names
}

// LookupLibrary returns info for a loaded library identified by its
// name parts (e.g., "scheme", "base"). Returns nil if not loaded.
func (p *Engine) LookupLibrary(parts ...string) *LibraryInfo {
    reg := p.libraryRegistry()
    if reg == nil {
        return nil
    }
    name := compilation.NewLibraryName(parts...)
    lib := reg.Lookup(name)
    if lib == nil {
        return nil
    }
    return compiledLibraryToInfo(lib)
}

// LoadedLibraries returns metadata for all currently loaded libraries,
// sorted by name.
func (p *Engine) LoadedLibraries() []*LibraryInfo {
    reg := p.libraryRegistry()
    if reg == nil {
        return nil
    }
    libs := reg.All()
    q := make([]*LibraryInfo, len(libs))
    for i, lib := range libs {
        q[i] = compiledLibraryToInfo(lib)
    }
    sort.Slice(q, func(i, j int) bool {
        return q[i].Name < q[j].Name
    })
    return q
}

// UnloadedLibraries returns metadata for libraries discoverable via the
// file resolver but not yet imported. Returns nil if no resolver is
// available. Thread-safe via lazy initialization with retry on failure.
func (p *Engine) UnloadedLibraries(ctx context.Context) []*LibraryInfo {
    idx := p.ensureExportIndex(ctx)
    if idx == nil {
        return nil
    }
    libReg := p.libraryRegistry()
    var q []*LibraryInfo
    for _, summary := range idx.Entries() {
        if libReg != nil && libReg.Lookup(summary.Name) != nil {
            continue
        }
        q = append(q, &LibraryInfo{
            Name:        summary.Name.SchemeString(),
            Description: summary.Description,
            SourceFile:  summary.SourceFile,
            Exports:     summary.Exports,
        })
    }
    return q
}

// libraryRegistry extracts the concrete LibraryRegistry from the
// environment, returning nil if unavailable.
func (p *Engine) libraryRegistry() *compilation.LibraryRegistry {
    regSearcher := p.env.LibraryRegistry()
    if regSearcher == nil {
        return nil
    }
    reg, ok := regSearcher.(*compilation.LibraryRegistry)
    if !ok {
        return nil
    }
    return reg
}

// ensureExportIndex lazily builds the library export index on first
// successful call. Retries on transient failures (context cancellation,
// slow filesystem). Permanent conditions (nil env, nil resolver) are
// marked as built to avoid repeated nil checks.
func (p *Engine) ensureExportIndex(ctx context.Context) *compilation.LibraryExportIndex {
    p.exportIndexMu.Lock()
    defer p.exportIndexMu.Unlock()
    if p.exportIndexBuilt {
        return p.exportIndex
    }
    resolver := p.env.FileResolver()
    if resolver == nil {
        p.exportIndexBuilt = true
        return nil
    }
    idx, err := compilation.BuildExportIndex(ctx, resolver, p.libraryRegistry())
    if err != nil {
        return nil // transient failure — retry next call
    }
    p.exportIndex = idx
    p.exportIndexBuilt = true
    return idx
}

func compiledLibraryToInfo(lib *compilation.CompiledLibrary) *LibraryInfo {
    exports := make([]string, 0, len(lib.Exports))
    for name := range lib.Exports {
        exports = append(exports, name)
    }
    sort.Strings(exports)
    return &LibraryInfo{
        Name:        lib.Name.SchemeString(),
        Description: lib.Description,
        SourceFile:  lib.SourceFile,
        Exports:     exports,
    }
}
```

Also add fields to the Engine struct in `engine.go`:

```go
// In Engine struct, add:
exportIndexMu    sync.Mutex
exportIndexBuilt bool
exportIndex      *compilation.LibraryExportIndex
```

**Step 4: Run tests**

Run: `go test -v -run TestEngine_LoadedLibraries ./... && go test -v -run TestEngine_LookupLibrary ./... && go test -v -run TestEngine_UnloadedLibraries ./...`
Expected: PASS

**Step 5: Commit**

---

## Phase 4: Engine Closure Classification + Disassembly

### Task 4: Add FormLabel and DisassembleValue to Engine

**Files:**
- Create: `disassemble.go` (in `wile/` root)
- Create: `engine_disassemble_test.go` (in `wile/` root)

**Step 1: Write the test**

```go
// engine_disassemble_test.go
package wile_test

import (
    "context"
    "strings"
    "testing"

    qt "github.com/frankban/quicktest"

    "github.com/aalpar/wile"
)

func TestEngine_FormLabel(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx)
    qt.Assert(t, err, qt.IsNil)

    // Built-in primitive
    carVal, ok := eng.Get("car")
    qt.Assert(t, ok, qt.IsTrue)
    qt.Assert(t, eng.FormLabel(carVal), qt.Equals, "primitive")

    // User-defined procedure
    _, err = eng.EvalMultiple(ctx, `(define (f x) (+ x 1))`)
    qt.Assert(t, err, qt.IsNil)
    fVal, ok := eng.Get("f")
    qt.Assert(t, ok, qt.IsTrue)
    qt.Assert(t, eng.FormLabel(fVal), qt.Equals, "procedure")

    // Non-callable
    qt.Assert(t, eng.FormLabel(wile.NewInteger(42)), qt.Equals, "")
}

func TestEngine_DisassembleValue_Native(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx)
    qt.Assert(t, err, qt.IsNil)

    _, err = eng.EvalMultiple(ctx, `(define (add1 x) (+ x 1))`)
    qt.Assert(t, err, qt.IsNil)
    val, ok := eng.Get("add1")
    qt.Assert(t, ok, qt.IsTrue)

    dis, disErr := eng.DisassembleValue(val)
    qt.Assert(t, disErr, qt.IsNil)
    qt.Assert(t, strings.Contains(dis, "OP"), qt.IsTrue,
        qt.Commentf("disassembly should contain opcodes: %q", dis))
}

func TestEngine_DisassembleValue_Foreign(t *testing.T) {
    ctx := context.Background()
    eng, err := wile.NewEngine(ctx)
    qt.Assert(t, err, qt.IsNil)

    val, ok := eng.Get("car")
    qt.Assert(t, ok, qt.IsTrue)

    dis, disErr := eng.DisassembleValue(val)
    qt.Assert(t, disErr, qt.IsNil)
    qt.Assert(t, strings.Contains(dis, "foreign"), qt.IsTrue)
    qt.Assert(t, strings.Contains(dis, "car"), qt.IsTrue)
}

func TestEngine_DisassembleValue_NonProcedure(t *testing.T) {
    eng, err := wile.NewEngine(context.Background())
    qt.Assert(t, err, qt.IsNil)

    _, disErr := eng.DisassembleValue(wile.NewInteger(42))
    qt.Assert(t, disErr, qt.IsNotNil)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestEngine_FormLabel ./...`
Expected: FAIL — method undefined

**Step 3: Write implementation**

```go
// disassemble.go
package wile

import (
    "fmt"
    "strings"

    "github.com/aalpar/wile/machine"
    "github.com/aalpar/wile/values"
    "github.com/aalpar/wile/werr"
)

// FormLabel returns a human-readable type label for a value:
// "primitive" for foreign (Go-implemented) closures,
// "procedure" for compiled Scheme closures,
// "" for non-callable values.
func (p *Engine) FormLabel(v values.Value) string {
    switch v.(type) {
    case *machine.ForeignClosure:
        return "primitive"
    case *machine.MachineClosure, *machine.CaseLambdaClosure:
        return "procedure"
    default:
        return ""
    }
}

// DisassembleValue returns the formatted disassembly of a callable value.
// For compiled closures, shows bytecode instructions. For case-lambda,
// shows each clause separately. For foreign closures, shows name, arity,
// and documentation. Returns an error for non-procedure values.
func (p *Engine) DisassembleValue(v values.Value) (string, error) {
    switch c := v.(type) {
    case *machine.MachineClosure:
        if c == nil {
            return "", werr.WrapForeignErrorf(
                werr.ErrInvalidArgument,
                "DisassembleValue: nil closure")
        }
        return machine.DisassembleString(c.Template()), nil

    case *machine.CaseLambdaClosure:
        if c == nil {
            return "", werr.WrapForeignErrorf(
                werr.ErrInvalidArgument,
                "DisassembleValue: nil case-lambda closure")
        }
        var sb strings.Builder
        for i, clause := range c.Clauses() {
            if i > 0 {
                sb.WriteString("\n")
            }
            fmt.Fprintf(&sb, "--- clause %d ---\n", i)
            sb.WriteString(machine.DisassembleString(clause.Template()))
        }
        return sb.String(), nil

    case *machine.ForeignClosure:
        if c == nil {
            return "", werr.WrapForeignErrorf(
                werr.ErrInvalidArgument,
                "DisassembleValue: nil foreign closure")
        }
        var sb strings.Builder
        fmt.Fprintf(&sb, "%s  (foreign, params: %d, variadic: %v)\n",
            c.Name(), c.ParameterCount(), c.IsVariadic())
        if c.Doc() != "" {
            fmt.Fprintf(&sb, "doc: %s\n", c.Doc())
        }
        return sb.String(), nil

    default:
        return "", werr.WrapForeignErrorf(
            werr.ErrInvalidArgument,
            "DisassembleValue: not a procedure (type: %T)", v)
    }
}
```

**Step 4: Run tests**

Run: `go test -v -run TestEngine_FormLabel ./... && go test -v -run TestEngine_DisassembleValue ./...`
Expected: PASS

**Step 5: Commit**

---

## Phase 5: Debugger Wrapper

### Task 5: Add wile.Debugger and wile.BreakpointInfo

**Files:**
- Create: `debugger.go` (in `wile/` root)
- Modify: `engine.go` (update `SetDebugger` signature and struct field)
- Modify: `engine_debugger_test.go` (use `wile.NewDebugger`)

**Step 1: Write the test**

```go
// debugger_test.go
package wile_test

import (
    "context"
    "testing"

    qt "github.com/frankban/quicktest"

    "github.com/aalpar/wile"
    "github.com/aalpar/wile/values"
)

func TestDebugger_BreakpointCRUD(t *testing.T) {
    dbg := wile.NewDebugger()

    id := dbg.SetBreakpoint("test.scm", 10, 0)
    qt.Assert(t, id, qt.Equals, 0)

    bps := dbg.Breakpoints()
    qt.Assert(t, len(bps), qt.Equals, 1)
    qt.Assert(t, bps[0].File, qt.Equals, "test.scm")
    qt.Assert(t, bps[0].Line, qt.Equals, 10)
    qt.Assert(t, bps[0].Enabled, qt.IsTrue)

    qt.Assert(t, dbg.DisableBreakpoint(id), qt.IsTrue)
    bps = dbg.Breakpoints()
    qt.Assert(t, bps[0].Enabled, qt.IsFalse)

    qt.Assert(t, dbg.EnableBreakpoint(id), qt.IsTrue)
    bps = dbg.Breakpoints()
    qt.Assert(t, bps[0].Enabled, qt.IsTrue)

    qt.Assert(t, dbg.RemoveBreakpoint(id), qt.IsTrue)
    qt.Assert(t, len(dbg.Breakpoints()), qt.Equals, 0)

    // Non-existent ID
    qt.Assert(t, dbg.RemoveBreakpoint(999), qt.IsFalse)
}

func TestDebugger_OnBreak(t *testing.T) {
    dbg := wile.NewDebugger()
    var received bool
    dbg.OnBreak(func(state values.DebugState, bp *wile.BreakpointInfo) {
        received = true
    })
    // OnBreak is tested via integration — just verify it doesn't panic
    qt.Assert(t, received, qt.IsFalse) // no break triggered yet
}

func TestDebugger_StepCommands(t *testing.T) {
    dbg := wile.NewDebugger()
    // These should not panic even with no active context
    dbg.StepInto()
    dbg.Continue()
    dbg.StepOver()
    dbg.StepOut()
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestDebugger ./...`
Expected: FAIL — `wile.Debugger` undefined

**Step 3: Write implementation**

```go
// debugger.go
package wile

import (
    "github.com/aalpar/wile/machine"
    "github.com/aalpar/wile/values"
)

// BreakpointInfo holds read-only breakpoint state for display.
type BreakpointInfo struct {
    ID       int
    File     string
    Line     int
    Column   int
    Enabled  bool
    HitCount int
}

// Debugger controls breakpoints and stepping for an Engine.
// It wraps the internal machine.Debugger to avoid exposing VM types.
type Debugger struct {
    inner     *machine.Debugger
    currentMC *machine.MachineContext
    onBreak   func(state values.DebugState, bp *BreakpointInfo)
}

// NewDebugger creates a new Debugger.
func NewDebugger() *Debugger {
    q := &Debugger{
        inner: machine.NewDebugger(),
    }
    q.inner.OnBreak(func(mc *machine.MachineContext, bp *machine.Breakpoint) {
        q.currentMC = mc
        if q.onBreak != nil {
            q.onBreak(mc, machineBreakpointToInfo(bp))
        }
    })
    return q
}

// SetBreakpoint adds a breakpoint at the given source location.
// Returns the breakpoint ID.
func (p *Debugger) SetBreakpoint(file string, line, col int) int {
    return int(p.inner.SetBreakpoint(file, line, col))
}

// RemoveBreakpoint removes a breakpoint by ID.
func (p *Debugger) RemoveBreakpoint(id int) bool {
    return p.inner.RemoveBreakpoint(machine.BreakpointID(id))
}

// EnableBreakpoint enables a breakpoint by ID.
func (p *Debugger) EnableBreakpoint(id int) bool {
    return p.inner.EnableBreakpoint(machine.BreakpointID(id))
}

// DisableBreakpoint disables a breakpoint by ID.
func (p *Debugger) DisableBreakpoint(id int) bool {
    return p.inner.DisableBreakpoint(machine.BreakpointID(id))
}

// Breakpoints returns all breakpoints.
func (p *Debugger) Breakpoints() []BreakpointInfo {
    bps := p.inner.Breakpoints()
    q := make([]BreakpointInfo, len(bps))
    for i, bp := range bps {
        q[i] = BreakpointInfo{
            ID:       int(bp.ID),
            File:     bp.File,
            Line:     bp.Line,
            Column:   bp.Column,
            Enabled:  bp.Enabled,
            HitCount: bp.HitCount,
        }
    }
    return q
}

// StepInto enables step-into mode.
func (p *Debugger) StepInto() {
    p.inner.StepInto()
}

// StepOver enables step-over mode using the stored break context.
func (p *Debugger) StepOver() {
    if p.currentMC != nil {
        p.inner.StepOver(p.currentMC)
    }
}

// StepOut enables step-out mode using the stored break context.
func (p *Debugger) StepOut() {
    if p.currentMC != nil {
        p.inner.StepOut(p.currentMC)
    }
}

// Continue resumes execution.
func (p *Debugger) Continue() {
    p.inner.Continue()
}

// OnBreak sets the callback invoked when a breakpoint is hit or a
// step completes. The DebugState provides source location and stack
// trace access without exposing VM internals.
func (p *Debugger) OnBreak(fn func(state values.DebugState, bp *BreakpointInfo)) {
    p.onBreak = fn
}

// CurrentState returns the DebugState from the most recent break, or
// nil if no break has occurred.
func (p *Debugger) CurrentState() values.DebugState {
    if p.currentMC == nil {
        return nil
    }
    return p.currentMC
}

// inner returns the wrapped machine.Debugger for Engine use.
func (p *Debugger) machineDebugger() *machine.Debugger {
    return p.inner
}

func machineBreakpointToInfo(bp *machine.Breakpoint) *BreakpointInfo {
    if bp == nil {
        return nil
    }
    return &BreakpointInfo{
        ID:       int(bp.ID),
        File:     bp.File,
        Line:     bp.Line,
        Column:   bp.Column,
        Enabled:  bp.Enabled,
        HitCount: bp.HitCount,
    }
}
```

Then update `engine.go`:

- Change field `debugger *machine.Debugger` to `debugger *Debugger`
- Change `SetDebugger(d *machine.Debugger)` to `SetDebugger(d *Debugger)`
- In `runCompiled`, change `mc.SetDebugger(p.debugger)` to
  `mc.SetDebugger(p.debugger.machineDebugger())`

And update `engine_debugger_test.go` to use `wile.NewDebugger()` instead
of `machine.NewDebugger()`.

**Step 4: Run tests**

Run: `go test -v -run TestDebugger ./... && go test -v -run TestSetDebugger ./...`
Expected: PASS

**Step 5: Commit**

---

## Phase 6: Migrate repl/registry_doc_provider.go

### Task 6: Remove machine/compilation from RegistryDocProvider

**Files:**
- Modify: `repl/registry_doc_provider.go`
- Modify: `repl/registry_doc_provider_test.go`

**Key changes:**

1. Constructor takes `*wile.Engine` instead of `*environment.EnvironmentFrame`
2. Remove `exportIndex` and `libraryRegistry()` fields/methods
3. Library search uses `eng.LoadedLibraries()` and `eng.UnloadedLibraries()`
4. `Search()` calls `registry.SearchDoc(reg, env, nil, nil, pattern)` then
   appends library results from Engine
5. `UnloadedLibraries()` delegates to `eng.UnloadedLibraries(ctx)`
6. Remove `import "github.com/aalpar/wile/machine/compilation"`

Constructor signature change:
```go
func NewRegistryDocProvider(reg *registry.Registry, eng *wile.Engine) *RegistryDocProvider
```

The `env` for `SearchDoc` comes from `eng.Environment()` (nil-safe if eng is nil).

**Test changes:**
- Tests that created `NewRegistryDocProvider(reg, nil)` → pass `nil` for both
- Tests that used `compilation.NewLibraryRegistry()` → use `wile.Engine` with
  real library setup
- Remove `import "github.com/aalpar/wile/machine"` (the `Impl` field in one test
  can use `nil` instead of `func(_ machine.CallContext) error { return nil }`)

**Step 1: Update implementation**

See above changes.

**Step 2: Update tests**

Remove `machine` and `machine/compilation` imports from test file.

**Step 3: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

**Step 4: Verify no compilation import**

Run: `grep 'machine/compilation' repl/registry_doc_provider.go`
Expected: No output

**Step 5: Commit**

---

## Phase 7: Migrate repl/meta.go

### Task 7: Remove machine/ and machine/compilation/ from meta.go

**Files:**
- Modify: `repl/meta.go`
- Modify: `repl/meta_test.go`

**Key changes:**

1. `cmdDocLibrary` — use `eng.LookupLibrary(parts...)` instead of
   `compilation.NewLibraryName` + `reg.Lookup`
2. `formatLibraryDoc` — takes `*wile.LibraryInfo` instead of
   `*compilation.CompiledLibrary`
3. `cmdLibraries` — use `eng.LoadedLibraries()` and `eng.UnloadedLibraries(ctx)`
   instead of `compilation.LibraryRegistry` and `compilation.LibrarySummary`
4. `formTypeLabel` — call `eng.FormLabel(v)` (requires storing `eng` reference)
5. `cmdDoc` — replace `bnd.Value().(*machine.ForeignClosure)` type assertion
   with `eng.FormLabel(bnd.Value()) == "primitive"`
6. `DisassembleBinding` — replace closure type switch with `eng.DisassembleValue(val)`
7. Remove both `machine/` and `machine/compilation/` imports

**Test changes in `meta_test.go`:**
- Tests that used `compilation.NewLibraryRegistry()` and
  `compilation.NewCompiledLibrary()` → set up libraries via
  `eng.EvalMultiple(ctx, "(import ...)")` or Engine methods
- Remove `import "github.com/aalpar/wile/machine/compilation"`

**Step 1: Update implementation**
**Step 2: Update tests**
**Step 3: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

**Step 4: Verify no machine imports**

Run: `grep '"github.com/aalpar/wile/machine' repl/meta.go`
Expected: No output

**Step 5: Commit**

---

## Phase 8: Migrate repl/debug.go + repl/repl.go

### Task 8: Remove machine/ from debug.go and repl.go

**Files:**
- Modify: `repl/debug.go`
- Modify: `repl/repl.go`

**Key changes in `debug.go`:**

1. `DebugContext.debugger` — change from `*machine.Debugger` to `*wile.Debugger`
2. `DebugContext.currentMC` — change from `*machine.MachineContext` to
   `values.DebugState`; rename to `currentState`
3. `NewDebugContext` — use `wile.NewDebugger()`
4. `Debugger()` — returns `*wile.Debugger`
5. `SetCurrentMC(mc)` — rename to `SetCurrentState(state values.DebugState)`
6. `cmdNext` — call `p.debugger.StepOver()` (no parameter)
7. `cmdFinish` — call `p.debugger.StepOut()` (no parameter)
8. `cmdBacktrace` — `p.currentState.FormatStackTrace(20)`
9. `cmdWhere` — `p.currentState.CurrentLocation()`
10. `breakpointAction` — use `int` instead of `machine.BreakpointID`
11. `cmdList` — use `wile.BreakpointInfo` instead of `*machine.Breakpoint`
12. Remove `import "github.com/aalpar/wile/machine"`

**Key changes in `repl.go`:**

1. `Debugger()` — returns `*wile.Debugger`
2. `OnBreak` callback — `func(state values.DebugState, bp *wile.BreakpointInfo)`
3. In callback body, use `state.CurrentLocation()` instead of
   `mc.CurrentSource()`, and `bp.File/Line/Column` from `*wile.BreakpointInfo`
4. Remove `import "github.com/aalpar/wile/machine"`

**Step 1: Update implementation**
**Step 2: Run tests**

Run: `go test -v ./repl/...`
Expected: PASS

**Step 3: Verify no machine imports**

Run: `grep '"github.com/aalpar/wile/machine' repl/debug.go repl/repl.go`
Expected: No output

**Step 4: Commit**

---

## Phase 9: Final Verification

### Task 9: Full build + lint + coverage check

**Step 1: Verify zero machine imports in repl/**

Run: `grep -r '"github.com/aalpar/wile/machine' repl/`
Expected: No output

**Step 2: Full test suite**

Run: `make test`
Expected: PASS

**Step 3: Lint**

Run: `make lint`
Expected: PASS

**Step 4: Coverage check**

Run: `make covercheck`
Expected: PASS

**Step 5: Update TODO.md**

Mark Task 8.3 as done. Update the description to reflect extended scope
(both `machine/` and `machine/compilation/` removed from `repl/`).

**Step 6: Commit**
