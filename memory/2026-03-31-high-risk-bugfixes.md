# High-Risk Bugfix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix two high-correctness bugs: (1) sub-context winding stack inheritance hazard, (2) cond-expand library check bypasses FileResolver.

**Architecture:** Bug 1 changes `NewSubContext()` to require a `WindingStack` parameter, making forgetting impossible at compile time. Bug 2 threads `FileResolver` through `FeatureRequirement.IsSatisfied` so `libraryRequirement` uses the resolver chain instead of `os.Stat`.

**Tech Stack:** Go, wile VM internals, `machine/` and `machine/compilation/` packages.

---

## Phase 1: Bug 1 — NewSubContext winding stack parameter

### Task 1: Change NewSubContext signature and kill SetWindingStack

**Files:**
- Modify: `machine/machine_context_subcontext.go:38` — change `NewSubContext()` → `NewSubContext(windingStack WindingStack)`
- Modify: `machine/machine_context_subcontext.go:72` — add `WindingStack` to `SubContextParams`
- Modify: `machine/machine_context_subcontext.go:90` — use `WindingStack` in `NewThreadSubContext`
- Delete: `machine/machine_context_winding.go:22-25` — remove `SetWindingStack` method

**Step 1: Change `NewSubContext` signature**

In `machine/machine_context_subcontext.go`, change:
```go
func (p *MachineContext) NewSubContext() *MachineContext {
```
to:
```go
func (p *MachineContext) NewSubContext(windingStack WindingStack) *MachineContext {
```

Add inside the function body (after `mc.barrierValid = p.barrierValid`):
```go
mc.windingStack = windingStack
```

**Step 2: Add WindingStack to SubContextParams and NewThreadSubContext**

In `SubContextParams`, add:
```go
WindingStack     WindingStack
```

In `CaptureSubContextParams`, add:
```go
WindingStack:     p.windingStack,
```

In `NewThreadSubContext`, add to the struct literal:
```go
windingStack: params.WindingStack,
```
(inside the `vmState` block, alongside `env` and `evals`).

**Step 3: Delete SetWindingStack**

Remove lines 22-25 from `machine/machine_context_winding.go`:
```go
// SetWindingStack sets the winding stack (used by sub-contexts).
func (p *MachineContext) SetWindingStack(stack WindingStack) {
	p.windingStack = stack
}
```

**Step 4: Do NOT compile yet — Task 2 fixes all call sites first.**

---

### Task 2: Update all production call sites

Every `NewSubContext()` call must now pass a `WindingStack`. Every subsequent `SetWindingStack` call must be deleted.

**Category A — Sites that already called SetWindingStack (delete the SetWindingStack line, pass the stack to NewSubContext):**

| File:Line | Old pattern | New pattern |
|-----------|-------------|-------------|
| `machine/machine_context_apply.go:303,305` | `sub := p.NewSubContext()` + `sub.SetWindingStack(p.WindingStack())` | `sub := p.NewSubContext(p.WindingStack())` |
| `machine/captured_continuation.go:101,103` | `sub := p.NewSubContext()` + `sub.SetWindingStack(p.WindingStack())` | `sub := p.NewSubContext(p.WindingStack())` |
| `registry/core/prim_prompt.go:99,101` | `sub := mc.NewSubContext()` + `sub.SetWindingStack(mc.WindingStack())` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `registry/core/prim_prompt.go:125,127` | `handlerSub := mc.NewSubContext()` + `handlerSub.SetWindingStack(mc.WindingStack())` | `handlerSub := mc.NewSubContext(mc.WindingStack())` |
| `registry/core/prim_prompt.go:248,250` | `sub := mc.NewSubContext()` + `sub.SetWindingStack(mc.WindingStack())` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `registry/core/prim_exit.go:76,78` | same pattern | same fix |
| `registry/core/prim_parameters.go:49,51` | same pattern | same fix |
| `registry/core/prim_parameters.go:99,101` | same pattern | same fix |
| `registry/core/prim_control.go:76,78` | same pattern | same fix |
| `registry/core/prim_control.go:176,178` | same pattern | same fix |
| `registry/core/prim_control.go:234,236` | same pattern | same fix |
| `registry/core/prim_control.go:256,258` | same pattern | same fix |
| `registry/core/prim_control.go:272,274` | same pattern | same fix |
| `registry/core/prim_control.go:339,341` | same pattern | same fix |
| `registry/core/prim_control.go:355,357` | same pattern | same fix |
| `registry/core/prim_barrier.go:57,59` | same pattern | same fix |
| `registry/core/prim_cont_marks.go:161,163` | same pattern | same fix |
| `registry/core/prim_exceptions.go:143,144` | `sub := mc.NewSubContext()` + `sub.SetWindingStack(excErr.WindingStack[:i])` | `sub := mc.NewSubContext(excErr.WindingStack[:i])` |

**Category B — Internal winding sites (pass computed stack, no SetWindingStack to delete):**

| File:Line | Old | New |
|-----------|-----|-----|
| `machine/machine_context_winding.go:50-51` | `sub := p.NewSubContext()` + `sub.windingStack = stack[:i:i]` | `sub := p.NewSubContext(stack[:i:i])` — delete the direct assignment line |
| `machine/machine_context_winding.go:82-83` | `sub := p.NewSubContext()` + `sub.windingStack = p.windingStack` | `sub := p.NewSubContext(p.windingStack)` — delete the direct assignment line |

**Category C — Bug sites (add winding stack, no SetWindingStack existed):**

| File:Line | Old | New |
|-----------|-----|-----|
| `ffi.go:608` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `ffi.go:711` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `extensions/files/prim_files.go:168` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `extensions/gointerop/prim_gointerop.go:420` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `engine.go:459` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `internal/extensions/all/prim_all.go:272` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `registry/core/prim_exceptions.go:47` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `registry/core/prim_exceptions.go:82` | `sub := mc.NewSubContext()` | `sub := mc.NewSubContext(mc.WindingStack())` |
| `registry/core/prim_exceptions.go:110` | `resumeSub := mc.NewSubContext()` | `resumeSub := mc.NewSubContext(mc.WindingStack())` |

---

### Task 3: Update test call sites

Test files that call `NewSubContext()` or `SetWindingStack` need updating.

| File:Line | Change |
|-----------|--------|
| `machine/machine_context_test.go:221` | `mc.NewSubContext()` → `mc.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:801` | `mc.NewSubContext()` → `mc.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:927` | `mc.NewSubContext()` → `mc.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:949` | `mc.NewSubContext()` → `mc.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:1053` | `parent.NewSubContext()` → `parent.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:1073` | `parent.NewSubContext()` → `parent.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:1089` | `parent.NewSubContext()` → `parent.NewSubContext(machine.WindingStack{})` |
| `machine/machine_context_test.go:1233` | `mc.NewSubContext()` → `mc.NewSubContext(mc.WindingStack())` |
| `machine/machine_context_pipeline_test.go:63` | `mc.NewSubContext()` → `mc.NewSubContext(machine.WindingStack{})` |
| `machine/machine_continuation_test.go:461` | `mc.NewSubContext()` → `mc.NewSubContext(machine.WindingStack{})` |
| `machine/continuation_winding_coverage_test.go:278` | Remove `testMC.SetWindingStack(...)` — set winding stack via a different mechanism (use the `windingStack` field if internal test, or restructure the test) |
| `machine/value_methods_test.go:358` | Remove `mc.SetWindingStack(ws)` — same approach |

For internal tests (`package machine`) that used `SetWindingStack` to set up test state: assign `windingStack` directly (e.g., `mc.windingStack = ws`) since they have package-level access.

**Step: Compile and run tests**

```bash
make build && go test ./machine/... ./registry/... ./extensions/... ./internal/... -count=1
```

---

### Task 4: Update documentation

**Files:**
- Modify: `machine/machine_context_subcontext.go` — update doc comment on `NewSubContext`
- Modify: `machine/CLAUDE.md` — update Gotchas section about sub-context winding
- Modify: `machine/CLAUDE.local.md` — update Gotchas about SubContext winding stack
- Modify: `machine/dynamic_wind.go:43` — update comment referencing `SetWindingStack`

Remove all mentions of `SetWindingStack` from docs. Replace with explanation that `NewSubContext` requires the winding stack parameter.

---

### Task 5: Regression tests for previously-missing sites

**File:** `registry/core/prim_apply_test.go` — extend `TestApplyWindingStackInheritance`

Add test cases for each previously-missing site. Pattern is identical to the existing `apply` test: `dynamic-wind` → sub-context primitive → `call/cc` → re-invoke → assert `before-count == 2`.

New cases to add:

```go
{
    Name: "call/cc inside with-exception-handler in dynamic-wind",
    Code: `
    (let ((k #f)
          (before-count 0))
      (call-with-continuation-prompt
        (lambda ()
          (dynamic-wind
            (lambda () (set! before-count (+ before-count 1)))
            (lambda ()
              (with-exception-handler
                (lambda (e) e)
                (lambda ()
                  (call/cc (lambda (cont) (set! k cont) 'first)))))
            (lambda () #f)))
        (default-continuation-prompt-tag)
        #f)
      (call-with-continuation-prompt
        (lambda () (k 'second))
        (default-continuation-prompt-tag)
        (lambda (v) v))
      before-count)`,
    Expected: values.NewInteger(2),
},
{
    Name: "call/cc inside force in dynamic-wind",
    Code: `
    (let ((k #f)
          (before-count 0))
      (call-with-continuation-prompt
        (lambda ()
          (dynamic-wind
            (lambda () (set! before-count (+ before-count 1)))
            (lambda ()
              (force (delay (call/cc (lambda (cont) (set! k cont) 'first)))))
            (lambda () #f)))
        (default-continuation-prompt-tag)
        #f)
      (call-with-continuation-prompt
        (lambda () (k 'second))
        (default-continuation-prompt-tag)
        (lambda (v) v))
      before-count)`,
    Expected: values.NewInteger(2),
},
```

Note: `call-with-input-file`, `once-do!`, and FFI callbacks are harder to test purely in Scheme. The `with-exception-handler` and `force` tests cover the pattern. The mechanical fix (required parameter) prevents regression for all sites.

**Step: Run the new tests**

```bash
go test -v -run TestApplyWindingStackInheritance ./registry/core/...
```

Expected: all cases PASS (including the 2 new ones).

---

## Phase 2: Bug 2 — cond-expand library check via FileResolver

### Task 6: Change IsSatisfied interface signature

**Files:**
- Modify: `machine/compilation/features.go` — change `IsSatisfied` interface and all 6 implementations

**Step 1: Change the interface**

```go
type FeatureRequirement interface {
    IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool
}
```

**Step 2: Update all 6 implementations**

`featureIdentifier` — add `resolver` param, ignore it:
```go
func (p *featureIdentifier) IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool {
    return IsFeatureSupported(p.name)
}
```

`libraryRequirement` — use resolver instead of `FindLibraryFile`:
```go
func (p *libraryRequirement) IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool {
    if registry != nil && registry.Lookup(p.name) != nil {
        return true
    }
    if resolver == nil {
        return false
    }
    sldPath := p.name.ToFSPath()
    f, _, err := resolver.ResolveAndOpen(context.Background(), sldPath)
    if err == nil {
        f.Close()
        return true
    }
    scmPath := strings.TrimSuffix(sldPath, ".sld") + ".scm"
    f, _, err = resolver.ResolveAndOpen(context.Background(), scmPath)
    if err == nil {
        f.Close()
        return true
    }
    return false
}
```

Add `"context"` and `"strings"` to imports.

`andRequirement` — thread resolver:
```go
func (p *andRequirement) IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool {
    for _, req := range p.requirements {
        if !req.IsSatisfied(registry, resolver) {
            return false
        }
    }
    return true
}
```

`orRequirement` — thread resolver:
```go
func (p *orRequirement) IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool {
    for _, req := range p.requirements {
        if req.IsSatisfied(registry, resolver) {
            return true
        }
    }
    return false
}
```

`notRequirement` — thread resolver:
```go
func (p *notRequirement) IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool {
    return !p.requirement.IsSatisfied(registry, resolver)
}
```

`elseRequirement` — add param, ignore it:
```go
func (p *elseRequirement) IsSatisfied(registry *LibraryRegistry, resolver FileResolver) bool {
    return true
}
```

---

### Task 7: Update IsSatisfied call site

**File:** `machine/compilation/compile_cond_expand.go:30-44`

In `resolveCondExpandClause`, extract the resolver and pass it:

```go
var resolver FileResolver
if fr := p.env.FileResolver(); fr != nil {
    resolver, _ = fr.(FileResolver)
}
```

Wait — `p.env.FileResolver()` returns `environment.FileResolver` which IS `FileResolver` (type alias). So:

```go
resolver := p.env.FileResolver()
```

Then change line 63 from:
```go
if req.IsSatisfied(registry) {
```
to:
```go
if req.IsSatisfied(registry, resolver) {
```

---

### Task 8: Update all IsSatisfied test call sites

Every test that calls `IsSatisfied` needs the new `resolver` parameter. Since most tests don't need a resolver, pass `nil`.

**Files to update:**

| File | Lines | Change |
|------|-------|--------|
| `machine/compilation/features_test.go` | All `.IsSatisfied(...)` calls | Add `nil` as second arg: `.IsSatisfied(nil, nil)` or `.IsSatisfied(registry, nil)` |
| `machine/compilation/library_internal_test.go:78,82,90` | `.IsSatisfied(...)` | Add `nil` as second arg |
| `machine/coverage_fullruntime_test.go:749-802` | All `.IsSatisfied(...)` calls | Add `nil` as second arg |

**Step: Compile and run tests**

```bash
go test ./machine/compilation/... ./machine/... -count=1
```

---

### Task 9: Add regression test for resolver-based library detection

**File:** `machine/compilation/features_test.go`

Add a test that creates an `fs.FS` with a library file, creates an `FSFileResolver`, and verifies that `libraryRequirement.IsSatisfied` returns `true` when using the resolver but would return `false` with `nil` resolver (since the library isn't on the OS filesystem).

```go
func TestLibraryRequirementWithFSResolver(t *testing.T) {
    // Create an in-memory FS with a library file
    fsys := fstest.MapFS{
        "mylib/cool.sld": &fstest.MapFile{Data: []byte("(define-library (mylib cool))")},
    }
    env := environment.NewNamespace().Runtime()
    resolver := NewFSFileResolver(fsys, env)
    registry := NewLibraryRegistry()

    req := NewLibraryRequirement(NewLibraryName("mylib", "cool"))

    // Without resolver: not found
    qt.Assert(t, req.IsSatisfied(registry, nil), qt.IsFalse)

    // With FS resolver: found
    qt.Assert(t, req.IsSatisfied(registry, resolver), qt.IsTrue)

    // .scm fallback
    fsys2 := fstest.MapFS{
        "other/lib.scm": &fstest.MapFile{Data: []byte("(define-library (other lib))")},
    }
    resolver2 := NewFSFileResolver(fsys2, env)
    req2 := NewLibraryRequirement(NewLibraryName("other", "lib"))
    qt.Assert(t, req2.IsSatisfied(registry, resolver2), qt.IsTrue)
}
```

Add `"testing/fstest"` to imports.

**Step: Run the new test**

```bash
go test -v -run TestLibraryRequirementWithFSResolver ./machine/compilation/...
```

---

## Phase 3: Final verification

### Task 10: Full build and test suite

```bash
make lint && make covercheck
```

Fix any issues. Both must pass clean.

### Task 11: Update TODO.md

Mark both bugs as done:

```
- [x] **Sub-context winding stack inheritance hazard** [High, Correctness, Fixed]: ...
- [x] **`cond-expand (library ...)` bypasses FileResolver** [High, S, Fixed]: ...
```
