# Continuation Marks Phase 1: Data Structure + `with-continuation-mark`

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add per-frame key-value marks to the continuation chain and compile the `with-continuation-mark` special form.

**Architecture:** A `marks` map on `vmState` propagates to both `MachineContext` and `MachineContinuation`. Three complex operations handle mark set (tail), save+set (non-tail), and restore. The form goes through the Tier 1 validated pipeline: validator, expander, compiler.

**Tech Stack:** Go, Wile bytecode VM, existing validated-form infrastructure.

---

## Overview

The work breaks into 5 tasks:

1. **Data structure** — `marks` field, accessors, save/restore propagation
2. **Operations** — 3 complex ops for the VM
3. **Validator + expander** — syntax validation and macro expansion
4. **Compiler** — bytecode emission for tail and non-tail positions
5. **Integration tests** — end-to-end Scheme programs

## Sentinel value

Operations use a package-level sentinel to distinguish "no previous mark" from a mark whose value happens to be nil/void:

```go
// machine/operation_cont_mark.go
var noMarkSentinel = &values.Symbol{} // unexported; pointer identity only
```

## Mark propagation rules

The `marks` field participates in save/restore like most vmState fields:

| Operation                          | marks behavior                           |
|------------------------------------|------------------------------------------|
| `NewMachineContinuationFromMC`     | Copy map pointer from mc                 |
| `SaveContinuation` (after create)  | Nil mc.marks (callee starts clean)       |
| `Restore`                          | mc.marks = cont.marks                    |
| `RestoreAndRelease`                | mc.marks = cont.marks                    |
| `PopContinuation`                  | mc.marks = cont.marks                    |
| `Copy()`                           | `maps.Clone(p.marks)` (shallow copy)     |
| `releaseContinuation`              | Already zeros struct → nils map          |

---

### Task 1: Data Structure

**Files:**
- Modify: `machine/vm_state.go` — add `marks` field
- Modify: `machine/machine_context.go` — add `SetMark`, `GetMark`, `DeleteMark` accessors
- Modify: `machine/machine_continuation.go` — update `Copy()`, `NewMachineContinuationFromMachineContext()`
- Modify: `machine/machine_context_continuation.go` — update `SaveContinuation`, `Restore`, `RestoreAndRelease`, `PopContinuation`
- Test: `machine/machine_context_test.go`

**Step 1: Write failing tests**

Add to `machine/machine_context_test.go`:

```go
func TestContMark_SetGetDelete(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()

	// Initially no marks
	val := mc.GetMark(values.NewSymbol("k"))
	c.Assert(val, qt.IsNil)

	// Set and get
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(42))
	val = mc.GetMark(values.NewSymbol("k"))
	c.Assert(val, qt.Equals, values.NewFixnum(42))

	// Delete
	mc.DeleteMark(values.NewSymbol("k"))
	val = mc.GetMark(values.NewSymbol("k"))
	c.Assert(val, qt.IsNil)
}

func TestContMark_SaveContinuation_NilsMarks(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(1))

	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	// After save, mc.marks should be nil (callee starts clean)
	c.Assert(mc.GetMark(values.NewSymbol("k")), qt.IsNil)

	// Saved continuation should have the mark
	c.Assert(mc.CurrentContinuationUnsafe().marks != nil, qt.IsTrue)
}

func TestContMark_PopContinuation_RestoresMarks(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(1))

	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	// Callee sets different mark
	mc.SetMark(values.NewSymbol("other"), values.NewFixnum(99))

	// Pop restores saved marks
	_, err = mc.PopContinuation()
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetMark(values.NewSymbol("k")), qt.Equals, values.NewFixnum(1))
	c.Assert(mc.GetMark(values.NewSymbol("other")), qt.IsNil)
}

func TestContMark_Copy_Independent(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(1))
	err := mc.SaveContinuation(1)
	c.Assert(err, qt.IsNil)

	original := mc.CurrentContinuationUnsafe()
	copied := original.Copy()

	// Mutating copy doesn't affect original
	copied.marks[values.NewSymbol("k")] = values.NewFixnum(999)
	c.Assert(original.marks[values.NewSymbol("k")], qt.Equals, values.NewFixnum(1))
}
```

Note: `newTestMachineContext` and `CurrentContinuationUnsafe` are existing test helpers. If `CurrentContinuationUnsafe` doesn't exist, use `mc.cont` directly (tests are in `package machine`, so fields are accessible).

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestContMark_' ./machine/...`
Expected: compile errors (marks field doesn't exist, methods not defined)

**Step 3: Add marks field to vmState**

In `machine/vm_state.go`, add field after `envPooled`:

```go
// marks holds per-frame continuation marks (Racket-style key-value annotations).
// nil when no marks are set on this frame (zero-cost common case).
// Lazily allocated by SetMark on first use.
//
// Propagation: SaveContinuation copies to continuation then nils mc.marks
// (callee starts clean). Restore/PopContinuation restores from continuation.
// Copy() uses maps.Clone (shallow copy for call/cc re-invocation safety).
marks map[values.Value]values.Value
```

Update the save/restore table in the vmState doc comment — add a row for `marks`:

```
│ marks        │ ✓              │ ✓           │ ✓                   │ ✓                │
```

**Step 4: Add accessors to MachineContext**

In `machine/machine_context.go`, add methods:

```go
// SetMark sets a continuation mark on the current frame.
// Lazily allocates the marks map on first use.
func (p *MachineContext) SetMark(key, val values.Value) {
	if p.marks == nil {
		p.marks = make(map[values.Value]values.Value)
	}
	p.marks[key] = val
}

// GetMark returns the continuation mark for key on the current frame,
// or nil if no mark is set.
func (p *MachineContext) GetMark(key values.Value) values.Value {
	if p.marks == nil {
		return nil
	}
	return p.marks[key]
}

// DeleteMark removes the continuation mark for key from the current frame.
func (p *MachineContext) DeleteMark(key values.Value) {
	delete(p.marks, key)
}
```

**Step 5: Update NewMachineContinuationFromMachineContext**

In `machine/machine_continuation.go`, function `NewMachineContinuationFromMachineContext`, add after `q.envPooled = mc.envPooled`:

```go
q.marks = mc.marks
```

**Step 6: Update SaveContinuation**

In `machine/machine_context_continuation.go`, function `SaveContinuation`, add after `p.cont = cont`:

```go
p.marks = nil // callee starts with no marks
```

**Step 7: Update Restore**

In `machine/machine_context_continuation.go`, function `Restore`, add after `p.envPooled = false`:

```go
p.marks = cont.marks
```

**Step 8: Update RestoreAndRelease**

In `machine/machine_context_continuation.go`, function `RestoreAndRelease`:
- In the shared branch (after `p.envPooled = false`): add `p.marks = cont.marks`
- In the non-shared branch (after `p.envPooled = cont.envPooled`): add `p.marks = cont.marks`

**Step 9: Update PopContinuation**

In `machine/machine_context_continuation.go`, function `PopContinuation`, add after `p.envPooled = q.envPooled`:

```go
p.marks = q.marks
```

**Step 10: Update Copy()**

In `machine/machine_continuation.go`, function `Copy()`, add after the `q.envPooled` comment block (before `q.parent = p.parent`):

```go
if len(p.marks) > 0 {
	q.marks = maps.Clone(p.marks)
}
```

Add `"maps"` to the import block.

**Step 11: Run tests**

Run: `go test -v -run 'TestContMark_' ./machine/...`
Expected: PASS

**Step 12: Run full test suite**

Run: `make test`
Expected: PASS (no regressions — nil marks is zero-cost)

**Step 13: Commit**

```
feat(machine): add continuation marks field to vmState

Add per-frame marks map to vmState with lazy allocation. Marks
propagate through SaveContinuation/Restore/PopContinuation and
are shallow-copied by Copy() for call/cc safety.
```

---

### Task 2: VM Operations

**Files:**
- Create: `machine/operation_cont_mark.go` — 3 operations + sentinel
- Test: `machine/operation_cont_mark_test.go`

**Step 1: Write failing tests**

Create `machine/operation_cont_mark_test.go`:

```go
package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestOperationSetContMark_Apply(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()

	// Stack: [key], value register: val
	mc.evals.Push(values.NewSymbol("k"))
	mc.SetValue(values.NewFixnum(42))

	op := NewOperationSetContMark()
	result, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)

	// Mark set on frame
	c.Assert(mc.GetMark(values.NewSymbol("k")), qt.Equals, values.NewFixnum(42))
	// Key popped from stack
	c.Assert(mc.evals.Len(), qt.Equals, 0)
	// PC advanced
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationSaveContMark_Apply(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()

	// Pre-existing mark
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(1))

	// Stack: [key], value register: new val
	mc.evals.Push(values.NewSymbol("k"))
	mc.SetValue(values.NewFixnum(2))

	op := NewOperationSaveContMark()
	result, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)

	// Mark updated
	c.Assert(mc.GetMark(values.NewSymbol("k")), qt.Equals, values.NewFixnum(2))
	// Stack: [key, old_value] — 2 entries saved
	c.Assert(mc.evals.Len(), qt.Equals, 2)
	// PC advanced
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationSaveContMark_NoExisting(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()

	// No pre-existing mark
	mc.evals.Push(values.NewSymbol("k"))
	mc.SetValue(values.NewFixnum(2))

	op := NewOperationSaveContMark()
	_, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)

	// Stack: [key, sentinel]
	c.Assert(mc.evals.Len(), qt.Equals, 2)
}

func TestOperationRestoreContMark_RestoresOld(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()

	// Simulate post-body state: stack has [key, old_value]
	mc.evals.Push(values.NewSymbol("k"))
	mc.evals.Push(values.NewFixnum(1))
	// Current mark was changed by body
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(99))

	op := NewOperationRestoreContMark()
	result, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, mc)

	// Mark restored to old value
	c.Assert(mc.GetMark(values.NewSymbol("k")), qt.Equals, values.NewFixnum(1))
	// Stack cleaned
	c.Assert(mc.evals.Len(), qt.Equals, 0)
	// PC advanced
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationRestoreContMark_DeletesWhenSentinel(t *testing.T) {
	c := qt.New(t)
	mc := newTestMachineContext()

	// Stack has [key, sentinel] — no previous mark existed
	mc.evals.Push(values.NewSymbol("k"))
	mc.evals.Push(noMarkSentinel)
	mc.SetMark(values.NewSymbol("k"), values.NewFixnum(99))

	op := NewOperationRestoreContMark()
	_, err := op.Apply(mc)
	c.Assert(err, qt.IsNil)

	// Mark removed
	c.Assert(mc.GetMark(values.NewSymbol("k")), qt.IsNil)
}

func TestOperationContMark_EqualTo(t *testing.T) {
	c := qt.New(t)
	c.Assert(NewOperationSetContMark().EqualTo(NewOperationSetContMark()), qt.IsTrue)
	c.Assert(NewOperationSaveContMark().EqualTo(NewOperationSaveContMark()), qt.IsTrue)
	c.Assert(NewOperationRestoreContMark().EqualTo(NewOperationRestoreContMark()), qt.IsTrue)
	c.Assert(NewOperationSetContMark().EqualTo(NewOperationSaveContMark()), qt.IsFalse)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'TestOperationSetContMark\|TestOperationSaveContMark\|TestOperationRestoreContMark\|TestOperationContMark' ./machine/...`
Expected: compile errors

**Step 3: Implement operations**

Create `machine/operation_cont_mark.go`:

```go
package machine

import (
	"github.com/aalpar/wile/values"
)

// noMarkSentinel is a package-level sentinel used by OperationSaveContMark
// and OperationRestoreContMark to distinguish "no previous mark" from a mark
// whose value is nil. Compared by pointer identity only.
var noMarkSentinel = values.NewSymbol("\x00no-mark-sentinel")

// OperationSetContMark sets a continuation mark on the current frame.
// Used in tail position where no restore is needed.
//
// Pre:  eval stack = [..., key], value register = val
// Post: eval stack = [...], marks[key] = val, pc++
type OperationSetContMark struct {
	OperationBase
}

func NewOperationSetContMark() *OperationSetContMark {
	return &OperationSetContMark{
		OperationBase: NewOperationBase("machine-operation-set-cont-mark"),
	}
}

func (*OperationSetContMark) Apply(mc *MachineContext) (*MachineContext, error) {
	key := mc.evals.Pop()
	mc.SetMark(key, mc.Value())
	mc.pc++
	return mc, nil
}

func (p *OperationSetContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSetContMark)
	return sameType(p, v, ok)
}

// OperationSaveContMark saves the previous mark value and sets a new one.
// Used in non-tail position, paired with OperationRestoreContMark.
//
// Pre:  eval stack = [..., key], value register = val
// Post: eval stack = [..., key, old_val_or_sentinel], marks[key] = val, pc++
type OperationSaveContMark struct {
	OperationBase
}

func NewOperationSaveContMark() *OperationSaveContMark {
	return &OperationSaveContMark{
		OperationBase: NewOperationBase("machine-operation-save-cont-mark"),
	}
}

func (*OperationSaveContMark) Apply(mc *MachineContext) (*MachineContext, error) {
	key := mc.evals.Pop()
	old := mc.GetMark(key)
	mc.evals.Push(key)
	if old == nil {
		mc.evals.Push(noMarkSentinel)
	} else {
		mc.evals.Push(old)
	}
	mc.SetMark(key, mc.Value())
	mc.pc++
	return mc, nil
}

func (p *OperationSaveContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationSaveContMark)
	return sameType(p, v, ok)
}

// OperationRestoreContMark restores the previous mark value after body evaluation.
// Paired with OperationSaveContMark.
//
// Pre:  eval stack = [..., key, old_val_or_sentinel]
// Post: eval stack = [...], marks[key] restored or deleted, pc++
type OperationRestoreContMark struct {
	OperationBase
}

func NewOperationRestoreContMark() *OperationRestoreContMark {
	return &OperationRestoreContMark{
		OperationBase: NewOperationBase("machine-operation-restore-cont-mark"),
	}
}

func (*OperationRestoreContMark) Apply(mc *MachineContext) (*MachineContext, error) {
	old := mc.evals.Pop()
	key := mc.evals.Pop()
	if old == noMarkSentinel {
		mc.DeleteMark(key)
	} else {
		mc.SetMark(key, old)
	}
	mc.pc++
	return mc, nil
}

func (p *OperationRestoreContMark) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationRestoreContMark)
	return sameType(p, v, ok)
}
```

**Step 4: Run tests**

Run: `go test -v -run 'TestOperationSetContMark\|TestOperationSaveContMark\|TestOperationRestoreContMark\|TestOperationContMark' ./machine/...`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add continuation mark VM operations

Three complex operations for with-continuation-mark compilation:
- OperationSetContMark: tail position (set, no save)
- OperationSaveContMark: non-tail (save old + set)
- OperationRestoreContMark: non-tail cleanup (restore old)
```

---

### Task 3: Validator + Expander

**Files:**
- Create: `internal/validate/validate_cont_mark.go` — validator
- Modify: `internal/validate/validated_forms.go` — add `ValidatedWithContinuationMark` type
- Modify: `internal/validate/register.go` — register validator
- Modify: `machine/primitive_expanders_registry.go` — register expander
- Modify: `machine/expander_primitive_forms.go` — add expander function
- Test: `internal/validate/validate_test.go`, `machine/expander_primitive_forms_test.go` or similar

**Step 1: Write failing validator test**

Add to `internal/validate/validate_test.go`:

```go
func TestValidateWithContinuationMark(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name  string
		input values.Value
		ok    bool
	}{
		{
			name: "valid",
			input: values.List(
				values.NewSymbol("with-continuation-mark"),
				values.NewSymbol("key"),
				values.NewFixnum(1),
				values.NewSymbol("body"),
			),
			ok: true,
		},
		{
			name:  "too few args",
			input: values.List(values.NewSymbol("with-continuation-mark"), values.NewSymbol("key")),
			ok:    false,
		},
		{
			name: "too many args",
			input: values.List(
				values.NewSymbol("with-continuation-mark"),
				values.NewSymbol("key"),
				values.NewFixnum(1),
				values.NewSymbol("body"),
				values.NewSymbol("extra"),
			),
			ok: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(tt.input))
			if tt.ok {
				c.Assert(result.Ok(), qt.IsTrue)
				wcm, ok := result.Expr.(*ValidatedWithContinuationMark)
				c.Assert(ok, qt.IsTrue)
				c.Assert(wcm.FormName(), qt.Equals, "with-continuation-mark")
				c.Assert(wcm.Key, qt.IsNotNil)
				c.Assert(wcm.Val, qt.IsNotNil)
				c.Assert(wcm.Body, qt.IsNotNil)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}
```

**Step 2: Run test to verify failure**

Run: `go test -v -run 'TestValidateWithContinuationMark' ./internal/validate/...`
Expected: compile error (type doesn't exist)

**Step 3: Add ValidatedWithContinuationMark type**

In `internal/validate/validated_forms.go`, add after `ValidatedDynamicWind`:

```go
// ValidatedWithContinuationMark represents (with-continuation-mark key val body)
//
// Sets a continuation mark on the current frame during body evaluation.
// In tail position, the mark replaces any existing mark with the same key
// on the current frame. In non-tail position, the mark is removed after
// body completes.
type ValidatedWithContinuationMark struct {
	validatedBase
	Key  ValidatedExpr
	Val  ValidatedExpr
	Body ValidatedExpr
}
```

**Step 4: Create validator**

Create `internal/validate/validate_cont_mark.go`:

```go
package validate

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// validateWithContinuationMark validates (with-continuation-mark key val body)
func validateWithContinuationMark(ctx context.Context, env *environment.EnvironmentFrame, pair *syntax.SyntaxPair, result *ValidationResult) ValidatedExpr {
	source, elements, ok := formPrologue(pair, "with-continuation-mark", 3, 3, result)
	if !ok {
		return nil
	}

	key := validateExpr(ctx, env, elements[1], result)
	val := validateExpr(ctx, env, elements[2], result)
	body := validateExpr(ctx, env, elements[3], result)

	if key == nil || val == nil || body == nil {
		return nil
	}

	return &ValidatedWithContinuationMark{
		validatedBase: validatedBase{formName: "with-continuation-mark", source: source},
		Key:           key,
		Val:           val,
		Body:          body,
	}
}
```

**Step 5: Register validator**

In `internal/validate/register.go`, add after the `"dynamic-wind"` line:

```go
registerValidator("with-continuation-mark", validateWithContinuationMark)
```

**Step 6: Run validator test**

Run: `go test -v -run 'TestValidateWithContinuationMark' ./internal/validate/...`
Expected: PASS

**Step 7: Register expander**

In `machine/primitive_expanders_registry.go`, add to the "Forms that expand their subexpressions" section:

```go
{"with-continuation-mark", (*ExpanderTimeContinuation).expandWithContinuationMarkForm},
```

**Step 8: Implement expander**

In `machine/expander_primitive_forms.go`, add:

```go
// expandWithContinuationMarkForm expands all three sub-expressions of
// (with-continuation-mark key val body).
func (p *ExpanderTimeContinuation) expandWithContinuationMarkForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := expr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(pair) {
		return syntax.NewSyntaxCons(sym, expr, sym.SourceContext()), nil
	}

	// Expand key
	expandedKey, err := p.ExpandExpression(pair.SyntaxCar())
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "with-continuation-mark: failed to expand key")
	}

	// Get val
	valPair, ok := pair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(valPair) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-continuation-mark: missing value")
	}
	expandedVal, err := p.ExpandExpression(valPair.SyntaxCar())
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "with-continuation-mark: failed to expand value")
	}

	// Get body
	bodyPair, ok := valPair.SyntaxCdr().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bodyPair) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "with-continuation-mark: missing body")
	}
	expandedBody, err := p.ExpandExpression(bodyPair.SyntaxCar())
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "with-continuation-mark: failed to expand body")
	}

	args := syntax.SyntaxList(sym.SourceContext(), expandedKey, expandedVal, expandedBody)
	return syntax.NewSyntaxCons(sym, args, sym.SourceContext()), nil
}
```

**Step 9: Run lint and test**

Run: `make lint && go test -v -run 'TestValidateWithContinuationMark' ./internal/validate/... && go test -v ./machine/... -count=1 2>&1 | tail -5`
Expected: lint clean, tests pass

**Step 10: Commit**

```
feat(validate,machine): add with-continuation-mark validator and expander

Validates (with-continuation-mark key val body) as exactly 3 args.
Expander recursively expands all three sub-expressions.
```

---

### Task 4: Compiler

**Files:**
- Modify: `machine/compile_validated.go` — add `CompileValidatedWithContinuationMark`
- Modify: `machine/register.go` — register typed compiler
- Test: `machine/compile_validated_test.go`

**Step 1: Write failing compiler tests**

Add to `machine/compile_validated_test.go`:

```go
func TestCompileWithContinuationMark_TailPosition(t *testing.T) {
	c := qt.New(t)
	// (with-continuation-mark 'k 1 'result)
	// In tail position: SetContMark, no restore
	result, err := evalString("(with-continuation-mark 'k 1 'result)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "result")
}

func TestCompileWithContinuationMark_NonTailPosition(t *testing.T) {
	c := qt.New(t)
	// (list (with-continuation-mark 'k 1 'inner) 'outer)
	// Non-tail: SaveContMark + RestoreContMark
	result, err := evalString("(list (with-continuation-mark 'k 1 'inner) 'outer)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(inner outer)")
}

func TestCompileWithContinuationMark_BodyIsCall(t *testing.T) {
	c := qt.New(t)
	// Body is a function call — mark should be visible during call
	result, err := evalString(`
		(with-continuation-mark 'k 1
			(+ 2 3))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "5")
}

func TestCompileWithContinuationMark_Nested(t *testing.T) {
	c := qt.New(t)
	// Nested non-tail: both marks saved/restored
	result, err := evalString(`
		(list
			(with-continuation-mark 'a 1
				(with-continuation-mark 'b 2
					(+ 10 20))))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "(30)")
}
```

Note: these tests use `evalString` which should already exist as a test helper. If not, use the `Engine` directly — check existing test patterns.

**Step 2: Run tests to verify failure**

Run: `go test -v -run 'TestCompileWithContinuationMark' ./machine/...`
Expected: error — form not recognized or not compiled

**Step 3: Implement compiler**

In `machine/compile_validated.go`, add:

```go
// CompileValidatedWithContinuationMark compiles (with-continuation-mark key val body).
//
// Tail position:
//
//	<compile key> PUSH
//	<compile val>
//	SetContMark               ; pops key, sets marks[key] = val
//	<compile body in tail>
//
// Non-tail position:
//
//	<compile key> PUSH
//	<compile val>
//	SaveContMark              ; pops key, saves (key, old) on stack, sets mark
//	<compile body in non-tail>
//	RestoreContMark           ; pops (old, key), restores mark
func (p *CompileTimeContinuation) CompileValidatedWithContinuationMark(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedWithContinuationMark,
) error {
	exprCtx := ctctx.NotInTail()

	// Compile key expression
	err := p.compileValidated(exprCtx, v.Key)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationPush())

	// Compile val expression
	err = p.compileValidated(exprCtx, v.Val)
	if err != nil {
		return err
	}

	if ctctx.InTail() {
		// Tail position: set mark, compile body in tail, no restore
		p.AppendOperations(NewOperationSetContMark())
		return p.compileValidated(ctctx, v.Body)
	}

	// Non-tail position: save+set, body, restore
	p.AppendOperations(NewOperationSaveContMark())
	err = p.compileValidated(exprCtx, v.Body)
	if err != nil {
		return err
	}
	p.AppendOperations(NewOperationRestoreContMark())
	return nil
}
```

**Step 4: Register compiler**

In `machine/register.go`, add to the `init()` function alongside other `registerTypedCompiler` calls:

```go
registerTypedCompiler("with-continuation-mark", func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, v *validate.ValidatedWithContinuationMark) error {
	return ctc.CompileValidatedWithContinuationMark(ctctx, v)
})
```

**Step 5: Run compiler tests**

Run: `go test -v -run 'TestCompileWithContinuationMark' ./machine/...`
Expected: PASS

**Step 6: Run full suite**

Run: `make lint && make test`
Expected: all pass

**Step 7: Commit**

```
feat(machine): compile with-continuation-mark special form

Tail position emits SetContMark (no restore needed).
Non-tail position emits SaveContMark/RestoreContMark pair
with old mark value preserved on eval stack across body.
```

---

### Task 5: Integration Tests

**Files:**
- Create: `integration/cont_mark_test.go` (or add to existing integration test file)

These tests verify end-to-end behavior through the full pipeline (parse → expand → validate → compile → run).

**Step 1: Write integration tests**

```go
func TestIntegration_WithContinuationMark_BasicValue(t *testing.T) {
	c := qt.New(t)
	// Mark doesn't affect return value
	result := evalOrFail(t, "(with-continuation-mark 'k 1 42)")
	c.Assert(result, qt.Equals, values.NewFixnum(42))
}

func TestIntegration_WithContinuationMark_TailCallPreservation(t *testing.T) {
	c := qt.New(t)
	// Tail-recursive loop with mark — should not stack overflow
	result := evalOrFail(t, `
		(define (loop n)
			(with-continuation-mark 'iter n
				(if (= n 0)
					'done
					(loop (- n 1)))))
		(loop 100000)
	`)
	c.Assert(result, qt.Equals, values.NewSymbol("done"))
}

func TestIntegration_WithContinuationMark_NonTailRestore(t *testing.T) {
	c := qt.New(t)
	// After with-continuation-mark in non-tail, subsequent code runs normally
	result := evalOrFail(t, `
		(let ((x (with-continuation-mark 'k 1 42)))
			(+ x 1))
	`)
	c.Assert(result, qt.Equals, values.NewFixnum(43))
}

func TestIntegration_WithContinuationMark_WithLambda(t *testing.T) {
	c := qt.New(t)
	// Mark survives across lambda body
	result := evalOrFail(t, `
		(with-continuation-mark 'k 'outer
			((lambda () 'inner-result)))
	`)
	c.Assert(result, qt.Equals, values.NewSymbol("inner-result"))
}

func TestIntegration_WithContinuationMark_NestedNonTail(t *testing.T) {
	c := qt.New(t)
	// Nested non-tail marks both save/restore correctly
	result := evalOrFail(t, `
		(+ (with-continuation-mark 'a 1
				(with-continuation-mark 'b 2
					10))
			(with-continuation-mark 'c 3
				20))
	`)
	c.Assert(result, qt.Equals, values.NewFixnum(30))
}
```

Note: use whatever test helper pattern exists in `integration/` — check for `evalOrFail` or similar. If the integration directory uses a different pattern, adapt accordingly.

**Step 2: Run integration tests**

Run: `go test -v -run 'TestIntegration_WithContinuationMark' ./integration/...`
Expected: PASS

**Step 3: Run full suite + lint**

Run: `make lint && make covercheck`
Expected: both pass

**Step 4: Commit**

```
test(integration): add with-continuation-mark integration tests

Covers basic value passthrough, tail-call preservation (100k
iterations), non-tail restore, lambda body, and nested marks.
```

---

## Post-Phase 1 Verification

After all tasks complete:

1. `make lint && make covercheck` — both pass
2. Marks don't appear in any hot path when unused (nil map = zero cost)
3. The `noCopyApply` optimization in `NativeTemplate` is not affected (marks don't escape via closures)
4. `with-continuation-mark` is not yet *useful* — Phase 2 adds `current-continuation-marks` for reading marks back

## What Phase 2 Adds

Phase 2 (`ContinuationMarkSet` + collection primitives) will make marks readable:
- `current-continuation-marks` walks the chain, collects marks
- `continuation-mark-set->list` / `continuation-mark-set-first` extract values
- Prompt-delimited collection via `promptTag`

Phase 1 is complete when marks are stored, propagated, and the `with-continuation-mark` form compiles and runs — even though no Scheme code can yet *read* the marks.
