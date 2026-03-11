# Environment Package Cleanup

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate technical debt in `environment/` identified by staff-engineer assessment — constructor duplication, dead delegation, semantic inconsistency, and minor correctness issues.

**Architecture:** All changes are internal to `environment/`. No public API signatures change (except removing methods that have zero external callers). The package has 143 downstream files — every change must preserve backward compatibility or prove zero external usage.

**Tech Stack:** Go 1.23, `environment/` package, `werr/` error conventions.

---

## Copier Interface Analysis

**Question:** Should a `Copier` interface be introduced for types implementing `Copy()`?

**Finding: No.** A `Copier` interface would have zero consumers. Evidence:

| Type | `Copy()` returns | Callers |
|------|-----------------|---------|
| `Binding` | `values.Value` | Only `binding_test.go` |
| `GlobalEnvironmentFrame` | `values.Value` | 2 sites, both type-assert back: `p.global.Copy().(*GlobalEnvironmentFrame)` |
| `LocalEnvironmentFrame` | `values.Value` | Only tests |
| `EnvironmentFrame` | `*EnvironmentFrame` (concrete) | `machine/machine_closure.go` |
| `Symbol` | `values.Value` | Various |
| `Hashtable` | `*Hashtable` (concrete) | Various |
| `MachineContinuation` | `*MachineContinuation` (concrete) | Various |

No code dispatches on `Copy()` polymorphically. Every call site knows the concrete type and either uses the concrete return directly or type-asserts immediately. A `Copier` interface would add ceremony without enabling any new pattern. YAGNI.

**Action for `EnvironmentFrame.Copy()`:** The return type mismatch (`*EnvironmentFrame` vs `values.Value`) is fine — `EnvironmentFrame.Copy()` correctly returns the concrete type its callers need. No change required.

**Action for `GlobalEnvironmentFrame.Copy()`:** The `values.Value` return forces 2 call sites to type-assert. Both are internal. Change return type to `*GlobalEnvironmentFrame` to eliminate the assertions. This is a minor improvement bundled into Task 7.

---

## Task 1: Extract shared constructor helper in TopLevelEnvironment

**Files:**
- Modify: `environment/top_level_environment.go`
- Test: `environment/top_level_environment_test.go`

**Problem:** Four constructors (`NewTopLevelEnvironment`, `NewChildTopLevelEnvironment`, `NewSchemeReportEnvironment`, `NewChildRuntime`) repeat the same ~10-line boilerplate: create global → create runtime frame → create phase registry → wire phases. `NewChildRuntime` bypasses `newPhaseRegistryWithTopLevel` and directly constructs a `PhaseRegistry` struct literal with field name `owner`, creating a latent divergence if `PhaseRegistry` gains fields.

**Step 1: Write test verifying current constructor equivalence**

Add to `environment/top_level_environment_test.go`:

```go
func TestConstructorEquivalence(t *testing.T) {
	c := qt.New(t)

	// NewChildRuntime must produce a frame with the same structural
	// properties as the other constructors (phases, topLevel, global).
	parent := NewTopLevelEnvironment()
	childRuntime := parent.NewChildRuntime()

	c.Assert(childRuntime.TopLevelEnv(), qt.Equals, parent)
	c.Assert(childRuntime.PhaseLevel(), qt.Equals, PhaseRuntime)
	c.Assert(childRuntime.GlobalEnvironment(), qt.IsNotNil)
	c.Assert(childRuntime.IsTopLevel(), qt.IsTrue)

	// Phase registry must be functional
	expand := childRuntime.Expand()
	c.Assert(expand, qt.IsNotNil)
	c.Assert(expand.PhaseLevel(), qt.Equals, PhaseExpand)

	// NewChildTopLevelEnvironment must produce independent phase registries
	child := parent.NewChildTopLevelEnvironment()
	c.Assert(child.Runtime().TopLevelEnv(), qt.Equals, child)
	c.Assert(child.Expand().PhaseLevel(), qt.Equals, PhaseExpand)

	// NewSchemeReportEnvironment must snapshot globals
	sym := values.NewSymbol("test-snap")
	parent.Runtime().MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
	report := parent.NewSchemeReportEnvironment()
	c.Assert(report.Runtime().GetGlobalIndex(sym), qt.IsNotNil)

	// New binding in parent must NOT appear in report
	sym2 := values.NewSymbol("after-snap")
	parent.Runtime().MaybeCreateOwnGlobalBinding(sym2, BindingTypeVariable)
	c.Assert(report.Runtime().GetGlobalIndex(sym2), qt.IsNil)
}
```

**Step 2: Run test to verify it passes (characterization test)**

Run: `go test ./environment/ -run TestConstructorEquivalence -v`
Expected: PASS

**Step 3: Extract `initRuntimeFrame` helper**

In `top_level_environment.go`, add a private helper and refactor all four constructors:

```go
// initRuntimeFrame creates a runtime EnvironmentFrame with a GlobalEnvironmentFrame
// and PhaseRegistry wired to the given TopLevelEnvironment. Used by all TLE
// constructors to eliminate boilerplate divergence.
func initRuntimeFrame(topLevel *TopLevelEnvironment, global *GlobalEnvironmentFrame) {
	topLevel.runtime = &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   topLevel,
	}
	topLevel.phases = newPhaseRegistryWithTopLevel(topLevel)
	topLevel.runtime.phases = topLevel.phases
}
```

Refactor `NewTopLevelEnvironment`:

```go
func NewTopLevelEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		syntaxInterns: make(map[values.Value]syntax.SyntaxValue),
		loadPathStack: NewLoadPathStack(),
		scopeRegistry: make(map[*syntax.Scope]*EnvironmentFrame),
	}
	initRuntimeFrame(q, newGlobalEnvironmentFrameWithTopLevel(q))
	return q
}
```

Refactor `NewChildTopLevelEnvironment`:

```go
func (p *TopLevelEnvironment) NewChildTopLevelEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		parent:            p,
	}
	initRuntimeFrame(q, newGlobalEnvironmentFrameWithTopLevel(q))
	return q
}
```

Refactor `NewSchemeReportEnvironment`:

```go
func (p *TopLevelEnvironment) NewSchemeReportEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		parent:            p,
	}
	copiedGlobal := p.runtime.global.Copy().(*GlobalEnvironmentFrame)
	copiedGlobal.topLevel = q
	initRuntimeFrame(q, copiedGlobal)
	return q
}
```

Refactor `NewChildRuntime` to use `newPhaseRegistryWithTopLevel`:

```go
func (p *TopLevelEnvironment) NewChildRuntime() *EnvironmentFrame {
	global := newGlobalEnvironmentFrameWithTopLevel(p)

	// NewChildRuntime creates a frame that shares the parent's
	// TopLevelEnvironment directly (not a child TLE). We need a
	// temporary holder to use initRuntimeFrame's wiring, but the
	// frame's topLevel must point to p (the shared TLE).
	//
	// Because initRuntimeFrame sets topLevel.runtime and topLevel.phases,
	// we can't use it here — it would overwrite p's runtime/phases.
	// Instead, use newPhaseRegistryWithTopLevel pattern directly but
	// through the helper to stay in sync on PhaseRegistry fields.
	runtime := &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   p,
	}
	childPhases := newPhaseRegistryForChild(p, runtime)
	runtime.phases = childPhases
	return runtime
}
```

Add the child-specific helper (avoids the `owner` vs field-name divergence):

```go
// newPhaseRegistryForChild creates a PhaseRegistry for a child environment
// that shares a TopLevelEnvironment. Unlike newPhaseRegistryWithTopLevel,
// it does NOT read topLevel.runtime (which belongs to the parent).
func newPhaseRegistryForChild(topLevel *TopLevelEnvironment, runtime *EnvironmentFrame) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:  make(map[int]*EnvironmentFrame),
		owner: topLevel,
	}
	q.envs[PhaseRuntime] = runtime
	return q
}
```

**Step 4: Run all environment tests**

Run: `go test ./environment/ -v -count=1`
Expected: All pass

**Step 5: Run downstream tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 6: Commit**

```
refactor(environment): extract initRuntimeFrame to deduplicate TLE constructors

Four constructors repeated the same runtime-frame + phase-registry wiring.
NewChildRuntime bypassed newPhaseRegistryWithTopLevel, creating a latent
divergence if PhaseRegistry gains fields.

Extract initRuntimeFrame for the three TLE-owning constructors and
newPhaseRegistryForChild for NewChildRuntime.
```

---

## Task 2: Remove dead InternSyntax delegation methods

**Files:**
- Modify: `environment/environment_frame.go`
- Modify: `environment/global_environment_frame.go`
- Modify: `environment/global_environment_frame_test.go` (if tests exist for removed methods)
- Modify: `environment/environment_frame_test.go` (if tests exist for removed methods)

**Problem:** `InternSyntax` is exposed on three types (`GlobalEnvironmentFrame`, `EnvironmentFrame`, `TopLevelEnvironment`). Only `TopLevelEnvironment.InternSyntax` does real work. The other two are pure delegation with identical nil-panic guards. Zero files outside `environment/` call any variant.

**Step 1: Verify zero external callers**

Run: `grep -r "InternSyntax" --include="*.go" | grep -v environment/ | grep -v "_test.go"`
Expected: No matches (already verified in assessment)

**Step 2: Remove `GlobalEnvironmentFrame.InternSyntax`**

Delete the method (lines ~298-311 in `global_environment_frame.go`).

**Step 3: Remove `EnvironmentFrame.InternSyntax`**

Delete the method (lines ~882-893 in `environment_frame.go`).

Also remove the reference in `newEnvironmentFrame`'s doc comment (line ~125) that mentions `InternSyntax()`.

**Step 4: Remove tests for deleted methods**

Search for tests calling `InternSyntax` on `EnvironmentFrame` or `GlobalEnvironmentFrame` in:
- `environment_frame_test.go`
- `global_environment_frame_test.go`

Remove those test cases.

**Step 5: Run all tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 6: Commit**

```
refactor(environment): remove dead InternSyntax delegation methods

GlobalEnvironmentFrame.InternSyntax and EnvironmentFrame.InternSyntax
had zero callers outside environment/. Both were pure delegation to
TopLevelEnvironment.InternSyntax. Callers that need syntax interning
can access it via TopLevelEnv().InternSyntax().
```

---

## Task 3: Fix LibraryRegistry delegation chain

**Files:**
- Modify: `environment/environment_frame.go`
- Modify: `environment/global_environment_frame.go`

**Problem:** `EnvironmentFrame.LibraryRegistry()` calls `p.TopLevel().global.LibraryRegistry()` — walking the parent chain to find the root frame, then accessing `.global`, then delegating to `.topLevel`. But `p.topLevel` is already the correct TopLevelEnvironment. `GlobalEnvironmentFrame.LibraryRegistry/SetLibraryRegistry` are pure delegation with nil-panic guards.

**Step 1: Check external callers of GlobalEnvironmentFrame.LibraryRegistry**

Run: `grep -rn "\.global\.LibraryRegistry\|\.global\.SetLibraryRegistry" --include="*.go" | grep -v environment/`
Expected: No matches outside environment/

**Step 2: Fix EnvironmentFrame.LibraryRegistry to use topLevel directly**

```go
func (p *EnvironmentFrame) LibraryRegistry() any {
	if p.topLevel == nil {
		return nil
	}
	return p.topLevel.LibraryRegistry()
}

func (p *EnvironmentFrame) SetLibraryRegistry(registry any) {
	if p.topLevel == nil {
		return
	}
	p.topLevel.SetLibraryRegistry(registry)
}
```

Note: The original code called `p.TopLevel().global.LibraryRegistry()` which walks the parent chain. The new code uses `p.topLevel` directly. This is correct because `topLevel` is set during construction and always points to the correct `TopLevelEnvironment` — unlike `TopLevel()` which walks the parent chain to find the root `EnvironmentFrame`.

**Step 3: Remove GlobalEnvironmentFrame.LibraryRegistry and SetLibraryRegistry**

Delete both methods from `global_environment_frame.go`.

**Step 4: Run all tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 5: Commit**

```
refactor(environment): simplify LibraryRegistry delegation

EnvironmentFrame.LibraryRegistry walked the parent chain via TopLevel()
then accessed .global.LibraryRegistry() — two levels of indirection when
p.topLevel is already the correct target. Delegate directly.

Remove GlobalEnvironmentFrame.LibraryRegistry/SetLibraryRegistry (zero
external callers, pure delegation).
```

---

## Task 4: Fix HasLocalVariableBinding semantics

**Files:**
- Modify: `environment/environment_frame.go`
- Modify: `environment/environment_frame_test.go`

**Problem:** `HasLocalVariableBinding` finds the **innermost** binding by name (via `GetLocalIndex`, which doesn't check scopes), then checks if that specific binding's scopes match. If the innermost binding has incompatible scopes but an outer binding has compatible scopes, it returns `false`.

This is semantically inconsistent with `GetLocalIndexWithScopes`, which collects ALL candidates and picks the maximal one. The expander and compiler could disagree about the same identifier:
- Expander (`HasLocalVariableBinding`): "no local variable shadows this macro" → expands
- Compiler (`GetLocalIndexWithScopes`): "there IS a matching local variable" → compiles as variable reference

**Decision point:** The semantics depend on what R7RS §4.2.2 requires for shadowing. Two interpretations:

1. **"Innermost binding by name shadows, period"** — even if scopes don't match, the name itself blocks outer scope-compatible bindings. In this case `HasLocalVariableBinding` should return `true` whenever ANY local binding for that name exists, regardless of scopes.

2. **"Only scope-compatible bindings shadow"** — an inner binding with incompatible scopes is invisible, so outer scope-compatible bindings can shadow. In this case `HasLocalVariableBinding` should search ALL local bindings (like `GetLocalIndexWithScopes`).

The current code implements neither interpretation cleanly — it checks the innermost binding's scopes but ignores outer bindings. **Interpretation 2 is correct per Flatt's model** and consistent with `GetLocalIndexWithScopes`.

**Step 1: Write failing test exposing the inconsistency**

Add to `environment/environment_frame_test.go`:

```go
func TestHasLocalVariableBinding_OuterScopeCompatible(t *testing.T) {
	c := qt.New(t)

	// Scenario: inner binding has incompatible scopes, outer has compatible.
	// HasLocalVariableBinding should find the outer binding.
	topLevel := NewTopLevelEnvironment()
	env := topLevel.Runtime()

	scopeA := syntax.NewScope()
	scopeB := syntax.NewScope()

	sym := values.NewSymbol("x")

	// Outer: binding with [scopeA] — compatible with reference [scopeA, scopeB]
	outerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), env)
	outerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeA}, nil)

	// Inner: binding with [scopeB] — incompatible with reference [scopeA]
	// (scopeB is NOT a subset of [scopeA])
	innerEnv := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), outerEnv)
	innerEnv.MaybeCreateLocalBindingWithScopes(sym, BindingTypeVariable, []*syntax.Scope{scopeB}, nil)

	// Reference has [scopeA] — inner binding [scopeB] doesn't match,
	// but outer binding [scopeA] does. Should return true.
	c.Assert(innerEnv.HasLocalVariableBinding(sym, []*syntax.Scope{scopeA}), qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test ./environment/ -run TestHasLocalVariableBinding_OuterScopeCompatible -v`
Expected: FAIL — returns `false` because innermost binding `[scopeB]` doesn't match `[scopeA]`

**Step 3: Rewrite using resolveLocal**

```go
func (p *EnvironmentFrame) HasLocalVariableBinding(sym *values.Symbol, scopes []*syntax.Scope) bool {
	if p == nil {
		return false
	}
	result := p.resolveLocal(sym, scopes, true, func(binding *Binding, _ int, _ int) any {
		if binding.BindingType() == BindingTypeVariable {
			return true
		}
		return nil
	})
	return result != nil
}
```

This uses `resolveLocal` with `checkScopes=true`, which iterates through ALL local bindings in the parent chain and only visits scope-compatible ones. The visitor returns non-nil on the first `BindingTypeVariable` match, stopping the walk.

**Step 4: Run all tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass (including the new test)

Watch for: Integration tests in `machine/scope_resolution_test.go`, `machine/hygiene_test.go`, `machine/let_shadow_macro_test.go`. If any fail, investigate whether they relied on the old innermost-only behavior intentionally.

**Step 5: Commit**

```
fix(environment): HasLocalVariableBinding searches all scope-compatible bindings

Previously found the innermost binding by name and checked its scopes.
If the innermost binding had incompatible scopes, it returned false even
when an outer binding had compatible scopes — disagreeing with
GetLocalIndexWithScopes, which collects all candidates.

Rewrite using resolveLocal with checkScopes=true so that any
scope-compatible BindingTypeVariable in the parent chain is found.
This aligns the expander's shadow check with the compiler's binding
resolution per Flatt's hygiene model.
```

---

## Task 5: Consolidate MaybeCreateLocalBinding using resolveLocal

**Files:**
- Modify: `environment/environment_frame.go`

**Problem:** `MaybeCreateLocalBinding` hand-walks the parent chain with its own loop, duplicating `resolveLocal`'s walk logic. The only difference: if not found, it creates the binding in the innermost frame.

**Step 1: Verify existing tests cover the method**

Run: `go test ./environment/ -run TestMaybeCreateLocalBinding -v`
Expected: PASS (characterization)

**Step 2: Rewrite using resolveLocal**

```go
func (p *EnvironmentFrame) MaybeCreateLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	if !p.hasLocal() {
		return nil, false
	}
	// Search existing bindings in current and parent frames
	result := p.resolveLocal(key, nil, false, func(_ *Binding, slot int, depth int) any {
		return NewLocalIndex(slot, depth)
	})
	if result != nil {
		return result.(*LocalIndex), false
	}
	// Not found — create in the current (innermost) frame
	return p.local.EnsureLocalBinding(key, bt)
}
```

**Step 3: Run all tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 4: Commit**

```
refactor(environment): rewrite MaybeCreateLocalBinding using resolveLocal

Eliminates a hand-rolled parent-chain walk that duplicated resolveLocal's
loop logic. Search delegates to resolveLocal; creation remains on
EnsureLocalBinding.
```

---

## Task 6: ~~Document GetLocalIndexWithScopes walk coupling~~ Superseded

Superseded — `GetLocalIndexWithScopes` now delegates to `resolveLocal` directly. The coupling that needed documenting no longer exists. See `plans/MEDIUM-REFACTORING-BATCH.md` Work Item B.

---

## Task 7: Fix GlobalIndex.EqualTo nil check and Copy return types

**Files:**
- Modify: `environment/global_environment_frame.go`
- Modify: `environment/environment_frame.go`
- Modify: `environment/top_level_environment.go`
- Test: `environment/global_environment_frame_test.go`

**Problem A:** `GlobalIndex.EqualTo` has an asymmetric nil check pattern. Line 60-61:
```go
if value == nil || p == nil {
    return value == nil && p == nil
}
```
The `p == nil` branch is unreachable (method call on nil pointer panics before reaching the check). Use the standard pattern.

**Problem B:** `GlobalEnvironmentFrame.Copy()` returns `values.Value`, forcing 2 internal call sites to type-assert. No code uses it polymorphically. Change return type to `*GlobalEnvironmentFrame`.

**Step 1: Fix GlobalIndex.EqualTo**

```go
func (p *GlobalIndex) EqualTo(value values.Value) bool {
	if p == nil || value == nil {
		return p == nil && value == nil
	}
	v, ok := value.(*GlobalIndex)
	if !ok {
		return false
	}
	return v.Index.EqualTo(p.Index)
}
```

Also simplify the return: `if v.Index.EqualTo(p.Index) { return true } return false` → `return v.Index.EqualTo(p.Index)`.

**Step 2: Change GlobalEnvironmentFrame.Copy return type**

In `global_environment_frame.go`, change:
```go
func (p *GlobalEnvironmentFrame) Copy() *GlobalEnvironmentFrame {
```

Remove the `(*GlobalEnvironmentFrame)(nil)` return for the nil case — return `nil` directly:
```go
if p == nil {
    return nil
}
```

**Step 3: Remove type assertions at call sites**

In `environment_frame.go` line 832:
```go
// Before:
global: p.global.Copy().(*GlobalEnvironmentFrame),
// After:
global: p.global.Copy(),
```

In `top_level_environment.go` line 410:
```go
// Before:
copiedGlobal := p.runtime.global.Copy().(*GlobalEnvironmentFrame)
// After:
copiedGlobal := p.runtime.global.Copy()
```

**Step 4: Run all tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 5: Commit**

```
refactor(environment): fix GlobalIndex.EqualTo nil check, concrete Copy return

GlobalIndex.EqualTo had unreachable p==nil branch. Use standard pattern.
GlobalEnvironmentFrame.Copy returned values.Value but all 2 callers
immediately type-asserted back. Return *GlobalEnvironmentFrame directly.
```

---

## Task 8: Switch LoadPathStack to RWMutex

**Files:**
- Modify: `environment/load_path_stack.go`
- Test: `environment/load_path_stack_test.go`

**Problem:** `Current()`, `CurrentDir()`, and `Depth()` are read-only operations that take a full `sync.Mutex` lock, serializing reads unnecessarily.

**Step 1: Change Mutex to RWMutex**

In `load_path_stack.go`:

```go
type LoadPathStack struct {
	mu    sync.RWMutex
	paths []string
}
```

**Step 2: Change read-only methods to RLock**

`Current()`:
```go
func (s *LoadPathStack) Current() string {
	s.mu.RLock()
	defer s.mu.RUnlock()
	if len(s.paths) == 0 {
		return ""
	}
	return s.paths[len(s.paths)-1]
}
```

`Depth()`:
```go
func (s *LoadPathStack) Depth() int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return len(s.paths)
}
```

Leave `Push()` and `Pop()` using `s.mu.Lock()` / `s.mu.Unlock()` (writers).

Note: `CurrentDir()` calls `Current()` which handles its own locking — no change needed.

**Step 3: Run all tests**

Run: `go test ./... -count=1 -timeout 120s`
Expected: All pass

**Step 4: Run race detector**

Run: `go test ./environment/ -race -count=1`
Expected: No races

**Step 5: Commit**

```
perf(environment): use RWMutex for LoadPathStack read-only operations

Current(), CurrentDir(), and Depth() are read-only but took full Mutex,
serializing concurrent readers. Switch to RWMutex so reads don't block
each other.
```

---

## Execution Order and Dependencies

```
Task 1 (constructor dedup) ─── independent
Task 2 (InternSyntax)      ─── independent
Task 3 (LibraryRegistry)   ─── independent
Task 4 (HasLocalVariable)  ─── independent
Task 5 (MaybeCreateLocal)  ─── independent
Task 6 (walk coupling doc) ─── independent
Task 7 (nil check + Copy)  ─── independent
Task 8 (RWMutex)           ─── independent
```

All tasks are independent — they touch different methods/types. Tasks 1-3 can be parallelized. Tasks 4-5 both touch `environment_frame.go` but different methods. Recommend sequential execution to keep diffs clean and reviewable.

**Total effort:** ~2 hours of implementation + testing.

## Validation

After all tasks complete:

```bash
make lint && make covercheck
go test ./... -race -count=1 -timeout 120s
```

All three must pass clean.
