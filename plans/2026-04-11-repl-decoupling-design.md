# REPL Decoupling from machine/ and machine/compilation/

**Status**: Approved  
**Task**: TODO.md Task 8.3 — Fix REPL importing `machine/compilation`  
**Scope**: Extended to also remove `machine/` imports from `repl/`

## Problem

The `repl/` package is a presentation layer that imports two internal packages:

- `machine/compilation/` — for library metadata types (`LibraryName`, `LibraryRegistry`,
  `CompiledLibrary`, `LibrarySummary`, `LibraryExportIndex`)
- `machine/` — for closure types (`ForeignClosure`, `MachineClosure`, `CaseLambdaClosure`,
  `DisassembleString`), debugger (`Debugger`, `MachineContext`, `Breakpoint`, `BreakpointID`)

This violates the package layering: `repl/` should consume the public `wile.Engine` API,
not reach behind it into VM internals.

## Approach

**Engine as abstraction boundary.** Add methods to `wile.Engine` and types to `wile/` and
`values/` that expose the information the REPL needs without leaking `machine/` types.

## Design

### 1. Library Metadata API

New type in `wile/`:

```go
// LibraryInfo holds read-only metadata about a loaded or discovered library.
type LibraryInfo struct {
    Name        string            // Scheme representation, e.g. "(scheme base)"
    Description string
    SourceFile  string
    Exports     map[string]string // external → internal name
}
```

New Engine methods:

```go
func (p *Engine) LookupLibrary(parts ...string) *LibraryInfo
func (p *Engine) LoadedLibraries() []*LibraryInfo
func (p *Engine) UnloadedLibraries(ctx context.Context) []*LibraryInfo
```

Internally, these do the `(*compilation.LibraryRegistry)` type assertion that `repl/`
currently does — moving the coupling behind the Engine.

### 2. Closure Classification + Disassembly

New Engine methods:

```go
// FormLabel returns "primitive", "procedure", or "" for non-callable values.
func (p *Engine) FormLabel(v values.Value) string

// DisassembleValue returns formatted disassembly of a callable value.
func (p *Engine) DisassembleValue(v values.Value) (string, error)
```

`meta.go`'s `DisassembleBinding` stays on `MetaCommandHandler` but delegates to
`Engine.DisassembleValue`. The `formTypeLabel()` function and `isForeign` type assertion
in `cmdDoc` both become `Engine.FormLabel` calls.

### 3. Debugger Abstraction

New types in `values/` (to avoid `machine/` → `wile/` circular dependency):

```go
// SourceLocation holds file/line/column for debug display.
type SourceLocation struct {
    File   string
    Line   int
    Column int
}

// DebugState provides read-only access to VM state during debug breaks.
type DebugState interface {
    CurrentLocation() *SourceLocation
    FormatStackTrace(maxDepth int) string
}
```

New types in `wile/`:

```go
type BreakpointInfo struct {
    ID       int
    File     string
    Line     int
    Column   int
    Enabled  bool
    HitCount int
}

// Debugger wraps machine.Debugger, translating between machine/* and wile/* types.
type Debugger struct {
    inner *machine.Debugger  // unexported
}

func NewDebugger() *Debugger
func (p *Debugger) SetBreakpoint(file string, line, col int) int
func (p *Debugger) RemoveBreakpoint(id int) bool
func (p *Debugger) EnableBreakpoint(id int) bool
func (p *Debugger) DisableBreakpoint(id int) bool
func (p *Debugger) Breakpoints() []BreakpointInfo
func (p *Debugger) StepInto()
func (p *Debugger) StepOver(state values.DebugState)
func (p *Debugger) StepOut(state values.DebugState)
func (p *Debugger) Continue()
func (p *Debugger) OnBreak(fn func(state values.DebugState, bp *BreakpointInfo))
```

`*MachineContext` satisfies `values.DebugState` via two new methods:
- `CurrentLocation() *values.SourceLocation`
- `FormatStackTrace(n int) string`

### 4. Scope

**In scope:**
- `repl/meta.go` — remove `machine/` and `machine/compilation/`
- `repl/registry_doc_provider.go` — remove `machine/compilation/`
- `repl/debug.go` — remove `machine/`
- `repl/repl.go` — remove `machine/`
- Test files — update to public API types

**Out of scope:**
- `registry/search.go` → `machine/compilation/` (registry is peer to machine)
- `registry/core/prim_reflection.go` → `machine/compilation/` (same)
- Relocating `LibraryName`, `CompiledLibrary`, etc.

**End state:** `repl/` imports only `wile/`, `values/`, `werr/`, `environment/`,
`registry/`, and `docparse/`. Zero imports of `machine/` or `machine/compilation/`.
