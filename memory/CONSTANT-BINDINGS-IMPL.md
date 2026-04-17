# Constant Bindings Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Identify constant bindings (imported, statically known) and reject `set!` on imported bindings per R7RS 5.2.

**Architecture:** Add `Imported` and `Constant` flags to `BindingMeta`, set them during library import, check `Imported` in the `set!` compiler. No REPL exemption.

**Tech Stack:** Go, existing `environment` and `machine/compilation` packages, `werr` error infrastructure.

**Design doc:** `plans/CONSTANT-BINDINGS.md`

---

### Task 1: Add `Imported` and `Constant` fields to `BindingMeta`

**Files:**
- Modify: `environment/binding.go`
- Test: `environment/binding_test.go`

**Step 1: Write failing tests**

Add to `environment/binding_test.go`:

```go
func TestBindingImportedFlag(t *testing.T) {
	c := qt.New(t)

	b := NewBinding(values.NewInteger(1), BindingTypeVariable)

	// Default: not imported
	c.Assert(b.IsImported(), qt.IsFalse)

	// Set imported
	b.SetImported(true)
	c.Assert(b.IsImported(), qt.IsTrue)

	// Lazy allocation: meta was nil, now allocated
	c.Assert(b.meta, qt.IsNotNil)
}

func TestBindingConstantFlag(t *testing.T) {
	c := qt.New(t)

	b := NewBinding(values.NewInteger(42), BindingTypeVariable)

	// Default: not constant
	c.Assert(b.IsConstant(), qt.IsFalse)

	// Set constant
	b.SetConstant(true)
	c.Assert(b.IsConstant(), qt.IsTrue)
}

func TestBindingCopyPreservesImportedAndConstant(t *testing.T) {
	c := qt.New(t)

	b := NewBinding(values.NewInteger(1), BindingTypeVariable)
	b.SetImported(true)
	b.SetConstant(true)

	cp := b.Copy().(*Binding)
	c.Assert(cp.IsImported(), qt.IsTrue)
	c.Assert(cp.IsConstant(), qt.IsTrue)

	// Mutations on copy don't affect original
	cp.SetImported(false)
	c.Assert(b.IsImported(), qt.IsTrue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -run 'TestBindingImportedFlag|TestBindingConstantFlag|TestBindingCopyPreservesImportedAndConstant' ./environment/...`
Expected: FAIL — `IsImported`, `SetImported`, `IsConstant`, `SetConstant` undefined.

**Step 3: Implement**

In `environment/binding.go`, add to `BindingMeta`:

```go
type BindingMeta struct {
	Scopes   []*syntax.Scope
	Source   *syntax.SourceContext
	Doc      string
	Imported bool
	Constant bool
}
```

Add accessors on `*Binding`, following the `Doc`/`SetDoc` pattern:

```go
// IsImported returns true if this binding came from a library import.
func (p *Binding) IsImported() bool {
	if p.meta == nil {
		return false
	}
	return p.meta.Imported
}

// SetImported marks this binding as imported from a library.
func (p *Binding) SetImported(v bool) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Imported = v
}

// IsConstant returns true if this binding's value is known at compile time.
func (p *Binding) IsConstant() bool {
	if p.meta == nil {
		return false
	}
	return p.meta.Constant
}

// SetConstant marks this binding's value as known at compile time.
func (p *Binding) SetConstant(v bool) {
	if p.meta == nil {
		p.meta = &BindingMeta{}
	}
	p.meta.Constant = v
}
```

Update `Copy()` to preserve the new fields:

```go
if p.meta != nil {
	b.meta = &BindingMeta{
		Scopes:   p.meta.Scopes,
		Source:   p.meta.Source,
		Doc:      p.meta.Doc,
		Imported: p.meta.Imported,
		Constant: p.meta.Constant,
	}
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -run 'TestBindingImportedFlag|TestBindingConstantFlag|TestBindingCopyPreservesImportedAndConstant' ./environment/...`
Expected: PASS

**Step 5: Run full environment test suite**

Run: `go test ./environment/...`
Expected: PASS — no regressions.

**Step 6: Commit**

```bash
git add environment/binding.go environment/binding_test.go
git commit -m "feat: add Imported and Constant flags to BindingMeta"
```

---

### Task 2: Add `ErrImmutableBinding` sentinel

**Files:**
- Modify: `werr/werr.go`

**Step 1: Add sentinel**

In `werr/werr.go`, add alongside the existing sentinels (alphabetical order within the group):

```go
var ErrImmutableBinding = NewStaticError("immutable binding")
```

**Step 2: Verify it compiles**

Run: `go build ./werr/...`
Expected: Success.

**Step 3: Commit**

```bash
git add werr/werr.go
git commit -m "feat: add ErrImmutableBinding sentinel"
```

---

### Task 3: Mark imported bindings in `CopyLibraryBindingsToEnvAtPhase`

**Files:**
- Modify: `machine/compilation/library_bindings.go`
- Test: new integration test (Task 5 covers this)

There are two import paths that need the flag. Both follow the same pattern:
`MaybeCreateOwnGlobalBinding` → `GetGlobalIndex` → `SetOwnGlobalValue`. After
setting the value, retrieve the binding and mark it.

**Step 1: Modify `CopyLibraryBindingsToEnvAtPhase`**

In `machine/compilation/library_bindings.go`, after line 261 (`SetOwnGlobalValue`),
add the marking logic. The binding is retrieved via `GetOwnGlobalBinding`:

```go
// After: err := phaseEnv.SetOwnGlobalValue(globalIdx, libBinding.Value())
// ... error check ...

// Mark the binding as imported and constant (R7RS 5.2).
targetBinding := phaseEnv.GetOwnGlobalBinding(globalIdx)
if targetBinding != nil {
	targetBinding.SetImported(true)
	if targetBinding.Value() != nil {
		targetBinding.SetConstant(true)
	}
}
```

Also in the `sourcePhase > 0` propagation block (after line 274), add the same
marking for the propagated binding:

```go
// After: _ = propagateEnv.SetOwnGlobalValue(propagateIdx, importedBinding.Value())

propagateBinding := propagateEnv.GetOwnGlobalBinding(propagateIdx)
if propagateBinding != nil {
	propagateBinding.SetImported(true)
	if propagateBinding.Value() != nil {
		propagateBinding.SetConstant(true)
	}
}
```

**Step 2: Modify `copyLibraryBindingsDirect`**

Same pattern. After line 307 (`SetOwnGlobalValue`), and after line 319
(expand-phase `SetOwnGlobalValue`):

```go
// After: err := targetEnv.SetOwnGlobalValue(globalIdx, importedBinding.Value())
// ... error check ...

directBinding := targetEnv.GetOwnGlobalBinding(globalIdx)
if directBinding != nil {
	directBinding.SetImported(true)
	if directBinding.Value() != nil {
		directBinding.SetConstant(true)
	}
}
```

And in the syntax expand-phase block:

```go
// After: _ = expandEnv.SetOwnGlobalValue(expandIdx, importedBinding.Value())

expandBinding := expandEnv.GetOwnGlobalBinding(expandIdx)
if expandBinding != nil {
	expandBinding.SetImported(true)
	if expandBinding.Value() != nil {
		expandBinding.SetConstant(true)
	}
}
```

**Step 3: Check `GetOwnGlobalBinding` is available on `EnvironmentFrame`**

`GetOwnGlobalBinding` is on `GlobalEnvironmentFrame`. `EnvironmentFrame` delegates
to `p.global`. Verify this method exists on `EnvironmentFrame`; if not, call
`phaseEnv.Global().GetOwnGlobalBinding(globalIdx)` or add a one-line delegator.

**Step 4: Verify it compiles**

Run: `go build ./machine/compilation/...`
Expected: Success.

**Step 5: Commit**

```bash
git add machine/compilation/library_bindings.go
git commit -m "feat: mark imported bindings with Imported and Constant flags"
```

---

### Task 4: Reject `set!` on imported bindings in `CompileValidatedSetBang`

**Files:**
- Modify: `machine/compilation/compile_validated.go`

**Step 1: Add the check**

In `CompileValidatedSetBang`, after the binding resolution (line 469-472) and
before the local/global dispatch (line 474), insert:

```go
// R7RS 5.2: reject set! on imported bindings.
if binding.IsImported() {
	return werr.WrapForeignErrorf(
		werr.ErrImmutableBinding,
		"set!: cannot mutate imported binding %q",
		sym.Key,
	)
}
```

**Step 2: Verify it compiles**

Run: `go build ./machine/compilation/...`
Expected: Success.

**Step 3: Commit**

```bash
git add machine/compilation/compile_validated.go
git commit -m "feat: reject set! on imported bindings (R7RS 5.2)"
```

---

### Task 5: Integration tests

**Files:**
- Test: `machine/compilation/immutable_import_test.go` (new file)

**Step 1: Write tests**

```go
package compilation_test

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/werr"
)

func TestSetBangOnImportedBindingRejected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	// set! on an imported binding must fail with ErrImmutableBinding.
	_, err = eng.EvalMultiple(ctx, `
		(import (scheme base))
		(set! cons 42)
	`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue)
}

func TestSetBangOnLocalDefineAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	// set! on a locally defined variable must succeed.
	result, err := eng.EvalMultiple(ctx, `
		(import (scheme base))
		(define x 1)
		(set! x 2)
		x
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, wile.NewInteger(2))
}

func TestSetBangOnShadowedImportAllowed(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	// Shadowing an import with define creates a fresh binding;
	// set! on the shadow must succeed.
	result, err := eng.EvalMultiple(ctx, `
		(import (scheme base))
		(define cons 42)
		(set! cons 99)
		cons
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, wile.NewInteger(99))
}

func TestImportedBindingHasConstantFlag(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	// After importing, the binding for "cons" should be constant.
	_, err = eng.EvalMultiple(ctx, `(import (scheme base))`)
	c.Assert(err, qt.IsNil)

	binding := eng.Environment().GetBinding(wile.NewSymbol("cons"), nil)
	c.Assert(binding, qt.IsNotNil)
	c.Assert(binding.IsImported(), qt.IsTrue)
	c.Assert(binding.IsConstant(), qt.IsTrue)
}
```

Note: The exact assertion API for inspecting bindings depends on what `Engine`
exposes. If `eng.Environment()` is not public or `GetBinding` returns an
unexported type, adjust to use whatever test-accessible path exists. Check
`engine.go` for the public API. An alternative is to test the flag via the
Scheme-level error behavior only (the first three tests) and skip the direct
binding inspection test.

**Step 2: Run the tests**

Run: `go test -run 'TestSetBangOnImported|TestSetBangOnLocal|TestSetBangOnShadowed|TestImportedBinding' ./machine/compilation/...`
Expected: PASS

**Step 3: Commit**

```bash
git add machine/compilation/immutable_import_test.go
git commit -m "test: integration tests for imported binding immutability"
```

---

### Task 6: Lint, coverage, and final verification

**Files:** None (verification only).

**Step 1: Run linter**

Run: `make lint`
Expected: 0 issues.

**Step 2: Run coverage check**

Run: `make covercheck`
Expected: All packages meet 80% threshold.

**Step 3: Run full test suite**

Run: `make test`
Expected: PASS — no regressions.

**Step 4: Update CLAUDE.md or docs if needed**

If the `environment/CLAUDE.local.md` binding documentation mentions the
`BindingMeta` struct fields, add `Imported` and `Constant` to the listing.

**Step 5: Commit any doc updates**

```bash
git add -A
git commit -m "docs: update binding metadata documentation for Imported/Constant"
```
