# Breakpoint Snap-to-Next Resolution

**Status:** PROPOSED
**Date:** 2026-02-17
**Related:** `machine/debugger.go`, `machine/native_template.go`, DAP integration (TODO P4)

## Problem

When peephole optimization or constant folding eliminates instructions, source lines that originally had executable code may no longer correspond to any operation in the bytecode. A breakpoint set on such a line silently never fires.

Current behavior (`debugger.go:150`): exact match only.

```go
if bp.File == source.File && bp.Line == source.Start.Line() {
```

If no operation carries that source line, the breakpoint is dead.

### How Source Mapping Works

`NativeTemplate` maintains a parallel array:

```
operations[i]  <->  sourceRefs[i]  ->  sourceTable[ref]  ->  SourceContext{file, line, col}
```

`SourceAt(pc)` does a direct O(1) lookup. Multiple operations often share the same source line. Optimization can remove operations, leaving gaps in which source lines are represented.

### When Lines Disappear

| Optimization | What disappears |
|-------------|----------------|
| BranchOnFalseValue fusion | The `Push` before branch — but the branch itself still carries the line |
| Constant folding `(if #f X Y)` | The entire `if` form's test + consequent — only Y's lines survive |
| Dead LoadVoid elimination (proposed) | Intermediate `LoadVoid` ops between defines |
| Push/Pop elimination (proposed) | Redundant stack round-trips |

Currently only the first two are implemented. The risk grows as more optimizations are added.

## Options

### Option A: Snap at Check Time

Change `CheckBreakpoint` to use `>=` on line numbers, with a one-shot resolution to avoid firing on every subsequent line.

```go
func (p *Debugger) CheckBreakpoint(mc *MachineContext) *Breakpoint {
    source := mc.CurrentSource()
    if source == nil {
        return nil
    }

    p.mu.Lock()
    defer p.mu.Unlock()

    for _, bp := range p.breakpoints {
        if !bp.Enabled || bp.File != source.File {
            continue
        }
        if source.Start.Line() >= bp.RequestedLine {
            bp.Line = source.Start.Line()
            bp.RequestedLine = bp.Line // lock in
            bp.HitCount++
            return bp
        }
    }
    return nil
}
```

**Pros:** Minimal change, no new infrastructure.

**Cons:** Resolves to whatever instruction *executes first*, not the first instruction in source order. If the program enters a function mid-way (via a branch or non-linear control flow), the breakpoint snaps to the wrong line. Also requires upgrading `RLock` to `Lock` since resolution mutates the breakpoint.

### Option B: Snap at Set Time (Template Registry)

Resolve breakpoints against compiled templates at the time they are set. Requires a template registry so the debugger can scan all known templates.

New method on `NativeTemplate`:

```go
// NearestSourceLine returns the smallest source line >= targetLine
// in the given file, or -1 if no executable code exists at or after
// that line in this template.
func (p *NativeTemplate) NearestSourceLine(file string, targetLine int) int {
    nearest := -1
    for i := range p.operations {
        src := p.sourceTable[p.sourceRefs[i]]
        if src == nil || src.File != file {
            continue
        }
        line := src.Start.Line()
        if line >= targetLine && (nearest == -1 || line < nearest) {
            nearest = line
        }
    }
    return nearest
}
```

`SetBreakpoint` resolves immediately:

```go
func (p *Debugger) SetBreakpoint(file string, line, column int) BreakpointID {
    resolvedLine := line
    for _, tpl := range p.templates {
        if snapped := tpl.NearestSourceLine(file, line); snapped >= 0 {
            if resolvedLine == line || snapped < resolvedLine {
                resolvedLine = snapped
            }
        }
    }
    // store with resolvedLine, keep original as RequestedLine
}
```

The `Breakpoint` struct gains fields:

```go
type Breakpoint struct {
    ID            BreakpointID
    File          string
    Line          int    // resolved (actual) line
    RequestedLine int    // what the user asked for
    Column        int
    Enabled       bool
    Verified      bool   // false if no executable code found
    HitCount      int
}
```

**Pros:** Correct resolution independent of execution order. Can report `Verified: false` for lines with no executable code (maps to DAP `Breakpoint.verified`). `CheckBreakpoint` stays as exact match with `RLock`.

**Cons:** Requires a template registry — compilation must register each template with the debugger. Must re-resolve when new code is compiled (`eval`, `load`, `define-library`). Significant new infrastructure.

### Option C: Lazy Resolution at Check Time (Correct)

Resolve on first encounter with the relevant template during execution. Uses `NearestSourceLine` from Option B but defers resolution to check time, avoiding the template registry.

New method on `NativeTemplate` (same as Option B):

```go
func (p *NativeTemplate) NearestSourceLine(file string, targetLine int) int {
    nearest := -1
    for i := range p.operations {
        src := p.sourceTable[p.sourceRefs[i]]
        if src == nil || src.File != file {
            continue
        }
        line := src.Start.Line()
        if line >= targetLine && (nearest == -1 || line < nearest) {
            nearest = line
        }
    }
    return nearest
}
```

`CheckBreakpoint` resolves lazily:

```go
func (p *Debugger) CheckBreakpoint(mc *MachineContext) *Breakpoint {
    source := mc.CurrentSource()
    if source == nil {
        return nil
    }

    p.mu.Lock()
    defer p.mu.Unlock()

    for _, bp := range p.breakpoints {
        if !bp.Enabled || bp.File != source.File {
            continue
        }
        // Resolve on first encounter with a template containing this file
        if !bp.Verified {
            snapped := mc.template.NearestSourceLine(bp.File, bp.RequestedLine)
            if snapped >= 0 {
                bp.Line = snapped
                bp.Verified = true
            } else {
                continue
            }
        }
        if bp.Line == source.Start.Line() {
            if bp.Column == 0 || bp.Column == source.Start.Column() {
                bp.HitCount++
                return bp
            }
        }
    }
    return nil
}
```

The `Breakpoint` struct:

```go
type Breakpoint struct {
    ID            BreakpointID
    File          string
    Line          int    // resolved line (after snap-to-next)
    RequestedLine int    // original line the user asked for
    Column        int
    Enabled       bool
    Verified      bool   // true once resolved against a template
    HitCount      int
}
```

`SetBreakpoint` stores the requested line as-is:

```go
func (p *Debugger) SetBreakpoint(file string, line, column int) BreakpointID {
    p.mu.Lock()
    defer p.mu.Unlock()

    id := p.nextID
    p.nextID++

    p.breakpoints[id] = &Breakpoint{
        ID:            id,
        File:          file,
        Line:          line,
        RequestedLine: line,
        Column:        column,
        Enabled:       true,
    }
    return id
}
```

**Pros:**
- Correct resolution — scans the template's source map, not execution order
- No template registry infrastructure
- No compile-time registration step
- Incremental: `CheckBreakpoint` is the only function that changes significantly

**Cons:**
- Requires `Lock` instead of `RLock` (lazy mutation)
- A breakpoint in a function that never executes stays unverified (acceptable — if it never runs, there's nothing to break on)
- Resolution happens against the first template encountered for that file; if the same file has multiple templates, the breakpoint resolves against whichever template executes first. In practice this is correct because `NearestSourceLine` searches by source line, and templates from the same file share the same line numbering.

## Recommendation

**Option C (lazy resolution)** is the correct choice. It gets the resolution right (source map scan, not execution order) without requiring new infrastructure (template registry, compile-time hooks). The trade-offs are acceptable:

- The `RLock` → `Lock` upgrade only affects debugger-attached runs (production runs skip the entire block via `if mc.debugger != nil`).
- Unverified breakpoints in never-executed code are a non-issue — you can't break on code that doesn't run.
- When DAP integration is built (TODO P4), the `Verified` field maps directly to `DAP.Breakpoint.verified`, and the `RequestedLine` / `Line` distinction maps to `DAP.Breakpoint.line` vs the requested line in `SetBreakpointsArguments`.

Option B (template registry) is the right long-term architecture if DAP integration needs to report verification status *before* execution starts. At that point, Option C's lazy mechanism can be kept as a fallback for dynamically compiled code (`eval`, `load`), with Option B's eager resolution handling statically known templates.

## Files

| File | Action | Purpose |
|------|--------|---------|
| `machine/debugger.go` | Modify | Add `RequestedLine`, `Verified` to `Breakpoint`; lazy resolution in `CheckBreakpoint` |
| `machine/native_template.go` | Modify | Add `NearestSourceLine` method |
| `machine/debugger_test.go` | Modify | Tests for snap-to-next behavior |
| `machine/native_template_test.go` | Modify | Tests for `NearestSourceLine` |
