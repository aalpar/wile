# Inline Breakpoint Traps

**Status:** PROPOSED
**Date:** 2026-02-17
**Related:** `plans/BREAKPOINT_SNAP_TO_NEXT.md`, `machine/debugger.go`, `machine/operation_brk.go`

## Problem

The VM loop checks for breakpoints on **every instruction** when a debugger is attached:

```go
// machine_context.go:516-524
if mc.debugger != nil {
    bp := mc.debugger.CheckBreakpoint(mc)    // every instruction
    if bp != nil {
        mc.debugger.TriggerBreak(mc, bp)
    } else if mc.debugger.ShouldStep(mc) {   // every instruction
        mc.debugger.TriggerBreak(mc, nil)
    }
}
```

`CheckBreakpoint` does per-instruction work:
1. `SourceAt(pc)` — O(1) array lookup into source table
2. `RLock` — mutex acquisition
3. Iterate all breakpoints — O(n) string comparisons on file paths + line numbers
4. `RUnlock`

The `mc.debugger != nil` guard makes the non-debugger case free (nil check, always predicted not-taken). But when a debugger IS attached, every instruction pays the full cost even if the nearest breakpoint is thousands of instructions away.

## Design: Operation Patching

Instead of checking breakpoints in the VM loop, **patch the bytecode** at breakpoint locations. Insert a wrapper operation that fires the breakpoint then delegates to the original operation.

This is the same technique hardware debuggers use (INT 3 on x86, BRK on ARM) — transplanted to the bytecode level.

### OperationBreakpointTrap

New operation type that wraps an original operation:

```go
// machine/operation_breakpoint_trap.go
type OperationBreakpointTrap struct {
    OperationBase
    Original Operation    // saved original op at this PC
    BP       *Breakpoint  // breakpoint to check
    Debugger *Debugger    // debugger to notify
}

func (p *OperationBreakpointTrap) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
    if p.BP.Enabled {
        p.BP.HitCount++
        p.Debugger.TriggerBreak(mc, p.BP)
    }
    return p.Original.Apply(ctx, mc)
}
```

The wrapper checks `Enabled` (a single bool read — no mutex, no source map lookup, no iteration) then delegates. Disabled breakpoints cost one branch.

### Template Registry

The debugger needs to find which templates contain operations at a given source location. A lightweight registry:

```go
// machine/debugger.go additions
type Debugger struct {
    // ...existing fields...
    templates []*NativeTemplate                  // registered templates
    patches   map[BreakpointID][]templatePatch   // for removal
}

type templatePatch struct {
    template *NativeTemplate
    pc       int
    original Operation
}

func (p *Debugger) RegisterTemplate(tpl *NativeTemplate) {
    p.mu.Lock()
    defer p.mu.Unlock()
    p.templates = append(p.templates, tpl)
    // Patch any existing breakpoints into this template
    for _, bp := range p.breakpoints {
        p.patchTemplate(tpl, bp)
    }
}
```

Registration points:
- **REPL**: `runWithDebugger` already creates the template — register it before `Run()`
- **Engine**: `runCompiled` — register if a debugger is attached
- **Library loader**: after `compileAndExecuteLibrary` completes
- **MakeClosure at runtime**: closures carry a template — register on first encounter, or eagerly at compile time since the template exists then

Sub-templates (lambdas compiled within a top-level expression) are captured as literals in the parent template. The registry can walk `template.literals` to find nested `*NativeTemplate` values and register them transitively.

### SetBreakpoint: Patch All Templates

When a breakpoint is set at `file:line`:

```go
func (p *Debugger) SetBreakpoint(file string, line, column int) BreakpointID {
    p.mu.Lock()
    defer p.mu.Unlock()

    id := p.nextID
    p.nextID++
    bp := &Breakpoint{ID: id, File: file, Line: line, Column: column, Enabled: true}
    p.breakpoints[id] = bp

    // Patch all registered templates
    for _, tpl := range p.templates {
        p.patchTemplate(tpl, bp)
    }
    return id
}

func (p *Debugger) patchTemplate(tpl *NativeTemplate, bp *Breakpoint) {
    for pc := range tpl.operations {
        src := tpl.SourceAt(pc)
        if src == nil || src.File != bp.File || src.Start.Line() != bp.Line {
            continue
        }
        if bp.Column != 0 && bp.Column != src.Start.Column() {
            continue
        }
        // Already patched at this PC?
        if _, ok := tpl.operations[pc].(*OperationBreakpointTrap); ok {
            continue
        }
        original := tpl.operations[pc]
        tpl.operations[pc] = &OperationBreakpointTrap{
            OperationBase: NewOperationBase("breakpoint-trap"),
            Original:      original,
            BP:            bp,
            Debugger:      p,
        }
        p.patches[bp.ID] = append(p.patches[bp.ID], templatePatch{
            template: tpl,
            pc:       pc,
            original: original,
        })
    }
}
```

### RemoveBreakpoint: Unpatch

```go
func (p *Debugger) RemoveBreakpoint(id BreakpointID) bool {
    p.mu.Lock()
    defer p.mu.Unlock()

    _, ok := p.breakpoints[id]
    if !ok {
        return false
    }
    // Restore original operations
    for _, patch := range p.patches[id] {
        patch.template.operations[patch.pc] = patch.original
    }
    delete(p.patches, id)
    delete(p.breakpoints, id)
    return true
}
```

### Enable/Disable

No patching/unpatching needed — the `Enabled` bool on `Breakpoint` is checked by the trap at runtime. Enable/Disable stays O(1).

### Run() Loop Change

Remove the breakpoint check entirely. Keep stepping check but make it cheaper:

```go
// Before (current):
if mc.debugger != nil {
    bp := mc.debugger.CheckBreakpoint(mc)
    if bp != nil {
        mc.debugger.TriggerBreak(mc, bp)
    } else if mc.debugger.ShouldStep(mc) {
        mc.debugger.TriggerBreak(mc, nil)
    }
}

// After:
if mc.debugger != nil && mc.debugger.stepping {
    if mc.debugger.ShouldStep(mc) {
        mc.debugger.TriggerBreak(mc, nil)
    }
}
```

The `stepping` field is already a plain bool on the struct (no mutex needed). When not stepping, the cost is nil-check + bool-check — essentially free. When stepping, the user expects slow single-step execution anyway.

## Cost Analysis

| Scenario | Before | After |
|----------|--------|-------|
| No debugger attached | 1 nil check / instruction | 1 nil check / instruction |
| Debugger, no breakpoints | SourceAt + RLock + iterate(0) + RUnlock / instruction | 1 nil check + 1 bool check / instruction |
| Debugger, N breakpoints | SourceAt + RLock + iterate(N) + RUnlock / instruction | 1 nil check + 1 bool check / instruction; **O(1) at breakpoint sites only** |
| Debugger, stepping | Same as above + ShouldStep | 1 nil check + 1 bool check + ShouldStep / instruction |

The win is proportional to instruction count between breakpoints — the common case when debugging.

## Concurrency

Template `operations` is a slice. Patching replaces individual elements (pointer-sized writes). In Go, pointer writes to slice elements are atomic on aligned architectures. The debugger mutex serializes Set/Remove. If SRFI-18 threads share templates, a thread might see the old or new operation at a given PC — both are valid (old = miss the breakpoint on this pass, new = hit it). No data race, no corruption.

If stronger guarantees are needed later, the operation slot can use `atomic.Pointer[Operation]`. Not needed for the REPL (single-goroutine execution).

## Interaction with Snap-to-Next

`BREAKPOINT_SNAP_TO_NEXT.md` proposes resolving breakpoints on optimized-away lines to the nearest executable line. That plan's Option B (template registry) is exactly the registry introduced here. The two plans share infrastructure:

- **Template registry** — introduced here, reused by snap-to-next
- **`NearestSourceLine`** — snap-to-next adds this to `NativeTemplate`; `patchTemplate` here would call it instead of exact-match when snap-to-next is implemented
- **`Verified` / `RequestedLine` fields** — snap-to-next adds these to `Breakpoint`; compatible with inline traps

Implementation order: inline traps first (removes the hot-loop overhead), snap-to-next second (refines which PCs get patched).

## Phases

### Phase 1: OperationBreakpointTrap + template registry

| File | Action |
|------|--------|
| `machine/operation_breakpoint_trap.go` | New — `OperationBreakpointTrap` type |
| `machine/debugger.go` | Add `templates`, `patches` fields; `RegisterTemplate`, `patchTemplate`; modify `SetBreakpoint`/`RemoveBreakpoint` |
| `machine/native_template.go` | Add `RegisteredTemplates` method to walk literals for sub-templates (optional — can defer) |

### Phase 2: Remove hot-loop check

| File | Action |
|------|--------|
| `machine/machine_context.go` | Replace `CheckBreakpoint` block with stepping-only check |

### Phase 3: Registration call sites

| File | Action |
|------|--------|
| `internal/repl/repl.go` | Register template in `runWithDebugger` before `Run()` |
| `engine.go` | Register in `runCompiled` if debugger is attached (future — Engine doesn't expose debugger yet) |

### Phase 4: Tests

| File | Action |
|------|--------|
| `machine/operation_breakpoint_trap_test.go` | New — trap fires, disabled trap skips, unpatch restores |
| `machine/debugger_test.go` | Modify — template registration, patch/unpatch lifecycle |
| `machine/source_tracking_coverage_test.go` | Modify — update integration tests that use `SetDebugger` + `CheckBreakpoint` |

## Open Questions

1. **Nested templates**: Should `RegisterTemplate` transitively walk `template.literals` for sub-templates (`*NativeTemplate` stored as closure templates)? This would catch lambdas defined inside the registered expression. The alternative is to register only top-level templates and rely on the fact that breakpoints on inner functions won't fire until that template is separately registered. Transitive walk is more correct; lazy registration is simpler.

2. **Template deregistration**: Templates are garbage-collected when no closures reference them. The registry holds a reference, preventing GC. Options: weak references (not native in Go), explicit `UnregisterTemplate`, or accept the leak (templates are small, debugger sessions are short). For REPL use, the leak is negligible.

3. **Multiple traps at same PC**: If two breakpoints match the same (file, line) — e.g., one with column=0 (any column) and one with column=5 — only one trap can occupy the slot. Options: chain traps, or disallow overlapping breakpoints. The current `CheckBreakpoint` has the same issue (returns first match). Chaining is the correct long-term solution but adds complexity.
