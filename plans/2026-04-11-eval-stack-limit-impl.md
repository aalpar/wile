# Eval Stack Size Limit — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `WithMaxStackSize(n)` engine option so sandboxed embedders can cap the eval stack, preventing OOM from huge argument lists or deeply nested non-tail expressions.

**Architecture:** New `maxStackSize uint64` field on `MachineContext`, checked at the 5 eval-stack-growing opcodes in `Run()`. Follows the `maxCallDepth` pattern exactly: field + accessor + engine option + propagation to sub-contexts. Opt-in only (no default).

**Tech Stack:** Go, existing `werr` sentinel infrastructure, existing engine option pattern.

**Design doc:** `plans/2026-04-11-eval-stack-limit-design.md`

---

### Task 1: Add `ErrStackOverflow` sentinel

**Files:**
- Modify: `werr/werr.go` (near `ErrStackUnderflow`, line ~140)

**Step 1: Add the sentinel**

In `werr/werr.go`, add `ErrStackOverflow` next to the existing stack sentinel:

```go
ErrStackUnderflow = NewStaticError("stack underflow")
ErrStackOverflow  = NewStaticError("stack overflow")
```

**Step 2: Verify**

Run: `go build ./werr/...`
Expected: clean build.

---

### Task 2: Add `WithMaxStackSize` engine option

**Files:**
- Modify: `options.go` — add `stackSizeSet bool` and `maxStackSize uint64` to `engineConfig`, add `WithMaxStackSize` function
- Modify: `engine.go` — add `maxStackSize uint64` field to `Engine` struct, wire config into `Engine` at construction, set on `MachineContext` at the two `SetMaxCallDepth` sites

**Step 1: Add fields to `engineConfig` (options.go)**

In `engineConfig` struct, after `callDepthSet`:

```go
maxStackSize  uint64
stackSizeSet  bool // true if WithMaxStackSize was explicitly called
```

**Step 2: Add `WithMaxStackSize` function (options.go)**

After `WithMaxCallDepth`:

```go
// WithMaxStackSize sets the maximum eval stack size for the VM.
// When the eval stack exceeds this size, ErrStackOverflow is returned.
// A value of 0 means unlimited (no size check). When not called, the
// stack size is unlimited (matching pre-existing behavior).
func WithMaxStackSize(n uint64) EngineOption {
	return func(cfg *engineConfig) {
		cfg.maxStackSize = n
		cfg.stackSizeSet = true
	}
}
```

**Step 3: Add field to `Engine` struct (engine.go)**

After `maxCallDepth uint64`:

```go
maxStackSize uint64
```

**Step 4: Wire into Engine construction (engine.go)**

In `NewEngine`, after `maxCallDepth: cfg.maxCallDepth,`:

```go
maxStackSize: cfg.maxStackSize,
```

**Step 5: Set on MachineContext at both call sites (engine.go)**

At the two sites that call `mc.SetMaxCallDepth(p.maxCallDepth)` (lines ~503 and ~711),
add immediately after:

```go
mc.SetMaxStackSize(p.maxStackSize)
```

**Step 6: Verify**

Run: `go build ./...`
Expected: build fails — `SetMaxStackSize` not yet defined on `MachineContext`. That's Task 3.

---

### Task 3: Add `maxStackSize` field, accessors, and sub-context propagation to MachineContext

**Files:**
- Modify: `machine/machine_context.go` — add field and accessors
- Modify: `machine/machine_context_subcontext.go` — propagate in `NewSubContext`, `CaptureSubContextParams`, `SubContextParams`, `NewThreadSubContext`

**Step 1: Add field to MachineContext (machine_context.go)**

After `maxCallDepth uint64`:

```go
maxStackSize uint64 // 0 = unlimited (default), otherwise max eval stack entries
```

**Step 2: Add accessors (machine_context.go)**

After `SetMaxCallDepth`:

```go
// MaxStackSize returns the maximum eval stack size limit. 0 means unlimited.
func (p *MachineContext) MaxStackSize() uint64 {
	return p.maxStackSize
}

// SetMaxStackSize sets the maximum eval stack size limit. 0 means unlimited.
func (p *MachineContext) SetMaxStackSize(n uint64) {
	p.maxStackSize = n
}
```

**Step 3: Propagate in `NewSubContext` (machine_context_subcontext.go)**

After `mc.maxCallDepth = p.maxCallDepth`:

```go
mc.maxStackSize = p.maxStackSize
```

**Step 4: Add to `SubContextParams` struct (machine_context_subcontext.go)**

After `MaxCallDepth uint64`:

```go
MaxStackSize uint64
```

**Step 5: Capture in `CaptureSubContextParams` (machine_context_subcontext.go)**

After `MaxCallDepth: p.maxCallDepth,`:

```go
MaxStackSize: p.maxStackSize,
```

**Step 6: Apply in `NewThreadSubContext` (machine_context_subcontext.go)**

After `maxCallDepth: params.MaxCallDepth,`:

```go
maxStackSize: params.MaxStackSize,
```

**Step 7: Verify**

Run: `go build ./...`
Expected: clean build.

---

### Task 4: Add enforcement in `Run()` loop

**Files:**
- Modify: `machine/machine_context.go` — add stack size check at 5 opcodes

**Step 1: Add a helper method**

Below the `SetMaxStackSize` accessor:

```go
// checkStackSize returns ErrStackOverflow if the eval stack has exceeded
// the configured maximum. Called after opcodes that push to the eval stack.
func (p *MachineContext) checkStackSize() error {
	if p.maxStackSize > 0 && uint64(p.evals.Len()) > p.maxStackSize {
		return werr.WrapForeignErrorf(werr.ErrStackOverflow,
			"eval stack size %d exceeds limit %d", p.evals.Len(), p.maxStackSize)
	}
	return nil
}
```

**Step 2: Add check to `OpPush`**

After the existing push logic and before `mc.pc++`:

```go
case OpPush:
	if mc.multiValues != nil {
		mc.evals.PushAll(mc.multiValues)
	} else if mc.singleValue != nil {
		mc.evals.Push(mc.singleValue)
	}
	if err := mc.checkStackSize(); err != nil {
		return err
	}
	mc.pc++
```

**Step 3: Add check to `OpPushLiteral`**

```go
case OpPushLiteral:
	mc.evals.Push(mc.template.literals[instr.Arg])
	if err := mc.checkStackSize(); err != nil {
		return err
	}
	mc.pc++
```

**Step 4: Add check to `OpPushGlobal`**

```go
case OpPushGlobal:
	bd, err := mc.resolveGlobalBinding(instr)
	if err != nil {
		return err
	}
	mc.evals.Push(bd.Value())
	if err := mc.checkStackSize(); err != nil {
		return err
	}
	mc.pc++
```

**Step 5: Add check to `OpPushLocal`**

```go
case OpPushLocal:
	bd, err := mc.resolveLocalBinding(instr)
	if err != nil {
		return err
	}
	mc.evals.Push(bd.Value())
	if err := mc.checkStackSize(); err != nil {
		return err
	}
	mc.pc++
```

**Step 6: Add check to `OpPushCachedBinding`**

```go
case OpPushCachedBinding:
	mc.evals.Push(mc.template.cachedBindings[instr.Arg].Value())
	if err := mc.checkStackSize(); err != nil {
		return err
	}
	mc.pc++
```

**Step 7: Verify**

Run: `make lint && make test`
Expected: all pass (no default limit, so enforcement is dormant).

---

### Task 5: Write tests

**Files:**
- Modify: `wile_test.go` — integration tests (parallel to `TestWithMaxCallDepth`)
- Modify: `machine/machine_context_test.go` — sub-context propagation test

**Step 1: Write integration test in `wile_test.go`**

After `TestWithMaxCallDepth`:

```go
func TestWithMaxStackSize(t *testing.T) {
	tests := []struct {
		name        string
		code        string
		stackSize   uint64
		wantErr     bool
		errSentinel error
	}{
		{
			name:        "large argument list exceeds limit",
			code:        "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)",
			stackSize:   10,
			wantErr:     true,
			errSentinel: werr.ErrStackOverflow,
		},
		{
			name:      "small argument list within limit",
			code:      "(list 1 2 3)",
			stackSize: 10,
			wantErr:   false,
		},
		{
			name:      "zero means unlimited",
			code:      "(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)",
			stackSize: 0,
			wantErr:   false,
		},
		{
			name:        "nested non-tail expressions exceed limit",
			code:        "(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10)))))))))",
			stackSize:   5,
			wantErr:     true,
			errSentinel: werr.ErrStackOverflow,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng, engErr := NewEngine(context.Background(), WithMaxStackSize(tt.stackSize))
			if engErr != nil {
				t.Fatalf("NewEngine: %v", engErr)
			}
			_, err := eng.Eval(context.Background(), eng.MustParse(context.Background(), tt.code))
			if tt.wantErr {
				if err == nil {
					t.Fatal("expected error, got nil")
				}
				if !errors.Is(err, tt.errSentinel) {
					t.Fatalf("expected %v, got: %v", tt.errSentinel, err)
				}
			} else if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
		})
	}
}
```

**Step 2: Write sub-context propagation test in `machine/machine_context_test.go`**

After the existing `TestSubContextInherits*` tests:

```go
func TestSubContextInheritsMaxStackSize(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false, NewOperationLoadVoid())
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)
	mc.SetMaxStackSize(500)

	sub := mc.NewSubContext()
	if sub.MaxStackSize() != 500 {
		t.Fatalf("sub-context maxStackSize = %d, want 500", sub.MaxStackSize())
	}
}
```

**Step 3: Write thread sub-context propagation test**

```go
func TestThreadSubContextInheritsMaxStackSize(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false, NewOperationLoadVoid())
	topEnv := environment.NewNamespace().Runtime()
	env := environment.NewEnvironmentFrameWithParent(nil, topEnv)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)
	mc.SetMaxStackSize(750)

	params := mc.CaptureSubContextParams()
	thread := values.NewThread(nil)
	sub := NewThreadSubContext(params, thread)
	if sub.MaxStackSize() != 750 {
		t.Fatalf("thread sub-context maxStackSize = %d, want 750", sub.MaxStackSize())
	}
}
```

**Step 4: Run all tests**

Run: `make lint && make test`
Expected: all pass.

---

### Task 6: Update TODO.md

**Files:**
- Modify: `TODO.md` — mark Task 1.4 as done

**Step 1: Mark Task 1.4 complete**

Change:
```markdown
- [ ] **Task 1.4: Eval stack size limit** [Medium, S]:
```
to:
```markdown
- [x] **Task 1.4: Eval stack size limit** [Medium, S, Done]:
```

---

### Task 7: Update TECH-DEBT plan

**Files:**
- Modify: `plans/TECH-DEBT-2026-04.md` — mark Task 1.4 done, update Phase 1 status

**Step 1: Mark Task 1.4 done**

Replace the Task 1.4 section with a done summary (matching the style of 1.1-1.3).

**Step 2: Update Phase 1 header**

Change "Mostly Complete" to "Complete" in the Phase 1 header.
