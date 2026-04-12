# Funnel prim_eval.go Through NewSubContext — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate the "forgotten field" bug class in `PrimEval` and `PrimLoad` by routing all production context creation through `NewSubContext`, which propagates fields automatically.

**Architecture:** Add `NewSubContextWithTemplate(tpl, env)` to `MachineContext` that delegates to `NewSubContext()` then overrides `template` and `env`. Replace the 6-line manual construction in both `PrimEval` and `PrimLoad` with a single call. Add pool release (`ReleaseSubContext`) since `NewSubContext` uses the pool (current code uses `NewMachineContext` which allocates on heap).

**Tech Stack:** Go, quicktest

---

### Context

`PrimEval` (`internal/extensions/eval/prim_eval.go:92-97`) and `PrimLoad` (`:175-180`) construct `MachineContext` directly via `NewMachineContext`, then manually propagate 4 fields. They miss 4 others that `NewSubContext` (`machine/machine_context_subcontext.go:43-60`) propagates:

| Missing field | Consequence |
|---|---|
| `windingStack` | `dynamic-wind` thunks skipped in eval'd code |
| `parentMC` | `call/cc` escape tracking broken inside `eval` |
| `escapeCont` | Nested escape continuations lost |
| `barrierValid` | `with-continuation-barrier` not enforced inside `eval` |

The intermediate `NewMachineContinuation` allocation is also eliminated.

---

### Task 1: Add `NewSubContextWithTemplate` method

**Files:**
- Modify: `machine/machine_context_subcontext.go` (after `NewSubContextWithWinding`, ~line 71)

**Step 1: Write the failing test**

In `machine/machine_context_test.go`, add after the existing `TestNewSubContext_InheritsMaxStackSize` test (~line 1270):

```go
func TestNewSubContextWithTemplate(t *testing.T) {
	c := qt.New(t)
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	parent := NewMachineContext(context.Background(), NewMachineContinuation(nil, nil, env))

	// Set up parent state that should propagate
	parent.SetMaxCallDepth(100)
	parent.SetMaxStackSize(200)
	handler := NewParameter(values.NewSymbol("test-handler"), nil)
	parent.PushExceptionHandler(handler)
	parent.windingStack = WindingStack{{}}

	// Target template and env for sub-context
	tpl := NewNativeTemplate(0, 0, false)
	targetEnv := environment.NewEnvironmentFrameWithParent(nil, topEnv)

	sub := parent.NewSubContextWithTemplate(tpl, targetEnv)
	defer ReleaseSubContext(sub)

	// Template and env come from arguments, not parent
	c.Assert(sub.template, qt.Equals, tpl)
	c.Assert(sub.env, qt.Equals, targetEnv)
	c.Assert(sub.pc, qt.Equals, 0)

	// All NewSubContext fields propagate from parent
	c.Assert(sub.parentMC, qt.Equals, parent)
	c.Assert(sub.maxCallDepth, qt.Equals, uint64(100))
	c.Assert(sub.maxStackSize, qt.Equals, uint64(200))
	c.Assert(sub.ExceptionHandler(), qt.Not(qt.IsNil))
	c.Assert(len(sub.windingStack), qt.Equals, 1)

	// Fresh state
	c.Assert(sub.evals.Len(), qt.Equals, 0)
	c.Assert(sub.cont, qt.IsNil)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNewSubContextWithTemplate ./machine/...`
Expected: FAIL — `NewSubContextWithTemplate` not defined

**Step 3: Write the implementation**

In `machine/machine_context_subcontext.go`, add after `NewSubContextWithWinding` (after line 71):

```go
// NewSubContextWithTemplate creates a sub-context configured to execute the
// given template in the given environment. This is the correct way for
// primitives like eval and load to create execution contexts — it propagates
// all parent fields automatically via NewSubContext, preventing the "forgotten
// field" bug class that NewMachineContext + manual setters is vulnerable to.
//
// The template and env override NewSubContext's defaults (nil template,
// parent's TopLevel env). pc starts at 0 (pool zero-value).
func (p *MachineContext) NewSubContextWithTemplate(
	tpl *NativeTemplate,
	env *environment.EnvironmentFrame,
) *MachineContext {
	mc := p.NewSubContext()
	mc.template = tpl
	mc.env = env
	return mc
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestNewSubContextWithTemplate ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

---

### Task 2: Migrate `PrimEval` to `NewSubContextWithTemplate`

**Files:**
- Modify: `internal/extensions/eval/prim_eval.go:91-98`

**Step 1: Write the failing integration test**

In `internal/extensions/eval/prim_eval_test.go`, add to `TestEvalDynamicContextInheritance` test cases:

```go
{"inherits dynamic-wind thunks",
    `(let ((log '()))
       (dynamic-wind
         (lambda () (set! log (cons 'before log)))
         (lambda ()
           (eval '(+ 1 2) (interaction-environment))
           (set! log (cons 'body log)))
         (lambda () (set! log (cons 'after log))))
       (reverse log))`,
    values.ListFromSlice(
        values.NewSymbol("before"),
        values.NewSymbol("body"),
        values.NewSymbol("after"),
    )},
```

This test passes with the current code (dynamic-wind wraps `eval`, not the other way around). The real validation is the unit test in Task 1. But let's also add a test that exercises `call/cc` inside eval:

```go
{"call/cc inside eval captures continuation",
    `(call-with-current-continuation
       (lambda (k)
         (eval '(begin (k 99) 0) (interaction-environment))))`,
    values.NewInteger(99)},
```

**Step 2: Run test to see current state**

Run: `go test -v -run TestEvalDynamicContextInheritance ./internal/extensions/eval/...`

**Step 3: Replace the manual construction in PrimEval**

In `prim_eval.go`, replace lines 91-98:

```go
	// Run the compiled code in a sub-context
	cont := machine.NewMachineContinuation(nil, tpl, env)
	sub := machine.NewMachineContext(mc.Context(), cont)
	sub.SetExceptionHandler(mc.ExceptionHandler())
	sub.SetMaxCallDepth(mc.MaxCallDepth())
	sub.SetMaxStackSize(mc.MaxStackSize())
	sub.SetThread(mc.Thread())
	err = sub.Run()
```

With:

```go
	// Run the compiled code in a sub-context.
	// NewSubContextWithTemplate propagates all parent fields automatically
	// (exception handler, winding stack, barrier, escape continuation, etc.).
	sub := mc.NewSubContextWithTemplate(tpl, env)
	err = sub.Run()
	machine.ReleaseSubContext(sub)
```

Wait — release must happen after reading the value. Correct replacement for lines 91-104:

```go
	// Run the compiled code in a sub-context.
	// NewSubContextWithTemplate propagates all parent fields automatically
	// (exception handler, winding stack, barrier, escape continuation, etc.).
	sub := mc.NewSubContextWithTemplate(tpl, env)
	err = sub.Run()
	if err != nil {
		machine.ReleaseSubContext(sub)
		return err
	}

	mc.SetValues(sub.GetValues()...)
	machine.ReleaseSubContext(sub)
	return nil
```

**Step 4: Run tests**

Run: `go test -v -run TestEval ./internal/extensions/eval/...`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

---

### Task 3: Migrate `PrimLoad` to `NewSubContextWithTemplate`

**Files:**
- Modify: `internal/extensions/eval/prim_eval.go:174-186`

**Step 1: Replace the manual construction in PrimLoad**

Replace lines 174-186:

```go
		// Run the compiled code
		cont := machine.NewMachineContinuation(nil, tpl, env)
		sub := machine.NewMachineContext(mc.Context(), cont)
		sub.SetExceptionHandler(mc.ExceptionHandler())
		sub.SetMaxCallDepth(mc.MaxCallDepth())
		sub.SetMaxStackSize(mc.MaxStackSize())
		sub.SetThread(mc.Thread())
		err = sub.Run()
		if err != nil {
			return werr.WrapForeignErrorf(err, "load: runtime error in %s", filename.Value)
		}

		lastValue = sub.GetValue()
```

With:

```go
		// Run the compiled code.
		// NewSubContextWithTemplate propagates all parent fields automatically.
		sub := mc.NewSubContextWithTemplate(tpl, env)
		err = sub.Run()
		if err != nil {
			machine.ReleaseSubContext(sub)
			return werr.WrapForeignErrorf(err, "load: runtime error in %s", filename.Value)
		}

		lastValue = sub.GetValue()
		machine.ReleaseSubContext(sub)
```

Note: `defer` is wrong here — the loop creates a sub-context per expression. Explicit release on both paths.

**Step 2: Run load tests**

Run: `go test -v -run TestLoad ./internal/extensions/eval/...`
Expected: PASS

**Step 3: Run full test suite**

Run: `go test -v ./internal/extensions/eval/...`
Expected: PASS

---

### Task 4: Remove unused import and run full validation

**Step 1: Check if `NewMachineContinuation` import is still needed**

`prim_eval.go` still uses `machine.NewNativeTemplate`, `machine.NewVMMacroEvaluator`, `machine.ReleaseSubContext`, etc. Check whether `machine.NewMachineContinuation` is still referenced — if not, the import stays (other `machine` symbols are used), but dead code is removed.

Grep `prim_eval.go` for `NewMachineContinuation`. If no remaining uses, the removal is automatic (goimports handles it).

**Step 2: Run goimports**

Run: `goimports -w internal/extensions/eval/prim_eval.go`

**Step 3: Run full validation**

Run: `make lint && make covercheck`
Expected: Both PASS

**Step 4: Run full test suite**

Run: `go test ./...`
Expected: PASS
