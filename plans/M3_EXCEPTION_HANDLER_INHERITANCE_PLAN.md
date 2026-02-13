# Fix M3: NewSubContext Does Not Inherit Exception Handlers

## Context

This fixes M3 from the architectural code review (ARCHITECTURAL_REVIEW.md lines 141-146): "NewSubContext does not inherit exception handlers".

Sub-contexts are used throughout the codebase for calling Scheme closures from Go primitives (e.g., `map`, `for-each`, `dynamic-wind` thunks, `with-exception-handler`). These sub-contexts need to see exception handlers installed in the parent context, but currently they don't inherit them automatically.

## Problem Description

### Current Behavior

`NewSubContext()` in `machine/machine_context.go:454-467` creates a fresh sub-context but does NOT copy the exception handler chain:

```go
func (p *MachineContext) NewSubContext() *MachineContext {
	p.counters.SubContextsCreated++
	return &MachineContext{
		ctx: p.ctx,
		vmState: vmState{
			env:      p.env.TopLevel(), // share global environment chain
			evals:    NewStack(),
			threadID: p.threadID, // inherit SRFI-18 thread identity
		},
		parentMC:   p,            // track parent for call/cc continuation capture
		escapeCont: p.escapeCont, // inherit escape continuation for nested call/cc
		thread:     p.thread,     // inherit SRFI-18 thread object
	}
	// MISSING: exceptionHandler not inherited!
}
```

The `exceptionHandler` field is left as `nil`, so the sub-context doesn't see any installed exception handlers.

### Manual Workarounds

Some call sites manually set the exception handler after creating the sub-context:

**File:** `internal/extensions/exceptions/prim_exceptions.go:38-39`
```go
sub := mc.NewSubContext()
sub.SetExceptionHandler(mc.ExceptionHandler())
```

But many call sites don't do this, meaning exceptions in those contexts won't be caught by surrounding handlers.

### Why This Matters

**R7RS §6.11** says exception handlers have **dynamic extent**:

> The handler is called whenever an exception is raised in the dynamic extent of the call to `with-exception-handler`.

Dynamic extent means: if you install a handler and then call a function, that function should see the handler. Sub-contexts are used for:

- `map` and `for-each` (calling the procedure on each element)
- `dynamic-wind` before/after thunks
- `with-exception-handler` thunk
- `call-with-values` producer/consumer
- `apply` argument evaluation
- Parameter converters
- Delimited continuation prompts

All of these should see exception handlers from the enclosing dynamic extent, but currently they don't (unless manually fixed).

### Impact Assessment

I'll search for all `NewSubContext()` call sites to determine which ones are affected:

**Files to check:**
- `registry/core/prim_control.go` - `apply`, `call/cc`, `call-with-values`
- `registry/core/prim_prompt.go` - delimited continuation prompts
- `registry/core/prim_parameters.go` - parameter converters
- `registry/core/prim_lists.go` - if any sub-contexts (unlikely, map/for-each are Scheme now)
- `registry/core/prim_vectors.go` - if any sub-contexts
- `internal/extensions/exceptions/prim_exceptions.go` - `with-exception-handler` (already fixed manually)
- `internal/extensions/files/prim_files.go` - file I/O thunks
- `internal/extensions/io/prim_ports.go` - port operations
- `internal/extensions/threads/prim_threads.go` - thread creation
- `engine.go` - top-level evaluation
- `ffi.go` - FFI calls

## R7RS Specification

**R7RS §6.11 (Exceptions):**

> `(with-exception-handler handler thunk)`
>
> Returns the results of invoking `thunk`. The `handler` is installed as the current exception handler in the dynamic extent of the invocation of `thunk`.

**Key phrase:** "in the dynamic extent" means the handler must be visible to all code called by the thunk, including code called through sub-contexts.

## Solution Design

### Option 1: Automatic Inheritance (Recommended)

Make `NewSubContext()` automatically inherit the exception handler:

```go
func (p *MachineContext) NewSubContext() *MachineContext {
	p.counters.SubContextsCreated++
	return &MachineContext{
		ctx: p.ctx,
		vmState: vmState{
			env:      p.env.TopLevel(),
			evals:    NewStack(),
			threadID: p.threadID,
		},
		parentMC:         p,
		escapeCont:       p.escapeCont,
		thread:           p.thread,
		exceptionHandler: p.exceptionHandler, // ← ADD THIS
	}
}
```

**Pros:**
- Correct by default - no call sites need to remember to set the handler
- Matches how other context fields are inherited (threadID, thread, escapeCont)
- Simplest fix - one line added

**Cons:**
- Need to audit call sites that manually set the handler to avoid double-setting
- Need to verify no call sites intentionally want an empty handler chain

### Option 2: Explicit Inheritance via Parameter

Add an optional parameter to control inheritance:

```go
func (p *MachineContext) NewSubContext(inheritHandler bool) *MachineContext {
	// ...
	if inheritHandler {
		sub.exceptionHandler = p.exceptionHandler
	}
	return sub
}
```

**Pros:**
- Explicit at call sites
- No risk of unintended behavior changes

**Cons:**
- Every call site must be updated
- Easy to forget the parameter
- Verbose

### Option 3: Separate Factory Method

Add a new method for sub-contexts with full inheritance:

```go
func (p *MachineContext) NewSubContextWithHandlers() *MachineContext {
	sub := p.NewSubContext()
	sub.exceptionHandler = p.exceptionHandler
	return sub
}
```

**Cons:**
- Two methods doing almost the same thing
- Still requires updating all call sites
- Doesn't fix the default behavior

### Recommended Solution

**Option 1** is the correct fix. Exception handlers have dynamic extent per R7RS, so they SHOULD be inherited by default. This is not a performance concern (it's just a pointer copy), and there's no semantic reason why a sub-context would want an empty handler chain when the parent has handlers installed.

## Implementation Plan

### 1. Update NewSubContext

**File:** `machine/machine_context.go:454-467`

```go
func (p *MachineContext) NewSubContext() *MachineContext {
	p.counters.SubContextsCreated++
	return &MachineContext{
		ctx: p.ctx,
		vmState: vmState{
			env:      p.env.TopLevel(),
			evals:    NewStack(),
			threadID: p.threadID,
		},
		parentMC:         p,
		escapeCont:       p.escapeCont,
		thread:           p.thread,
		exceptionHandler: p.exceptionHandler, // inherit exception handler chain
	}
}
```

### 2. Audit and Update Call Sites

Search for all `NewSubContext()` calls and remove manual `SetExceptionHandler` calls:

**File:** `internal/extensions/exceptions/prim_exceptions.go:38-39`

**Before:**
```go
sub := mc.NewSubContext()
sub.SetExceptionHandler(mc.ExceptionHandler())
```

**After:**
```go
sub := mc.NewSubContext()
// Exception handler now inherited automatically
```

**Other files to check:**
- `registry/core/prim_control.go`
- `registry/core/prim_prompt.go`
- `registry/core/prim_parameters.go`
- `internal/extensions/files/prim_files.go`
- `internal/extensions/io/prim_ports.go`
- `internal/extensions/threads/prim_threads.go`
- `engine.go`
- `ffi.go`

### 3. Update NewSubContextFromParams (Thread Creation)

**File:** `machine/machine_context.go:488-501`

The `NewSubContextFromParams` function is used for creating sub-contexts across goroutine boundaries (SRFI-18 threads). It should also inherit the exception handler:

**Current code:**
```go
func NewSubContextFromParams(params SubContextParams) *MachineContext {
	return &MachineContext{
		ctx: params.Ctx,
		vmState: vmState{
			env: params.Env,
			// ... other fields
		},
		parentMC:   params.ParentMC,
		escapeCont: params.EscapeCont,
		// MISSING: exceptionHandler
	}
}
```

**Problem:** `SubContextParams` doesn't include the exception handler.

**Solution:** Add `ExceptionHandler` to `SubContextParams`:

```go
type SubContextParams struct {
	Ctx              context.Context
	Env              *environment.EnvironmentFrame
	ParentMC         *MachineContext
	EscapeCont       *MachineContinuation
	ExceptionHandler *ExceptionHandler // ← ADD THIS
}

func (p *MachineContext) CaptureSubContextParams() SubContextParams {
	return SubContextParams{
		Ctx:              p.ctx,
		Env:              p.env.TopLevel(),
		ParentMC:         p,
		EscapeCont:       p.escapeCont,
		ExceptionHandler: p.exceptionHandler, // ← ADD THIS
	}
}

func NewSubContextFromParams(params SubContextParams) *MachineContext {
	return &MachineContext{
		ctx: params.Ctx,
		vmState: vmState{
			env: params.Env,
			// ... other fields
		},
		parentMC:         params.ParentMC,
		escapeCont:       params.EscapeCont,
		exceptionHandler: params.ExceptionHandler, // ← ADD THIS
	}
}
```

### 4. Add Regression Tests

**File:** `machine/machine_context_test.go` (or new file `machine/exception_handler_inheritance_test.go`)

Add tests that verify exception handlers are inherited:

```go
// TestNewSubContext_InheritsExceptionHandler verifies that sub-contexts
// automatically inherit the parent's exception handler chain.
func TestNewSubContext_InheritsExceptionHandler(t *testing.T) {
	c := qt.New(t)

	// Create parent context with exception handler
	parent := machine.NewMachineContext(context.Background(), nil)
	handler := values.NewInteger(42) // dummy handler
	parent.PushExceptionHandler(handler)

	// Create sub-context
	sub := parent.NewSubContext()

	// Verify sub-context sees the same handler
	c.Assert(sub.ExceptionHandler(), qt.Not(qt.IsNil))
	c.Assert(sub.ExceptionHandler().Handler(), values.SchemeEquals, handler)
}

// TestNewSubContext_InheritsNestedHandlers verifies that nested
// exception handlers are properly inherited.
func TestNewSubContext_InheritsNestedHandlers(t *testing.T) {
	c := qt.New(t)

	parent := machine.NewMachineContext(context.Background(), nil)
	handler1 := values.NewSymbol("outer")
	handler2 := values.NewSymbol("inner")

	parent.PushExceptionHandler(handler1)
	parent.PushExceptionHandler(handler2)

	sub := parent.NewSubContext()

	// Verify sub sees inner handler
	c.Assert(sub.ExceptionHandler().Handler(), values.SchemeEquals, handler2)
	// Verify sub sees outer handler via parent chain
	c.Assert(sub.ExceptionHandler().Parent(), qt.Not(qt.IsNil))
	c.Assert(sub.ExceptionHandler().Parent().Handler(), values.SchemeEquals, handler1)
}
```

**File:** `registry/core/prim_exception_test.go` (or `internal/extensions/exceptions/prim_exceptions_test.go`)

Add Scheme-level tests:

```scheme
;; Test that exceptions in map see outer handler
(define caught-in-map #f)
(with-exception-handler
  (lambda (e) (set! caught-in-map e) 'handled)
  (lambda ()
    (map (lambda (x) (if (= x 3) (raise-continuable 'error-3) x))
         '(1 2 3 4))))
;; caught-in-map should be 'error-3

;; Test that exceptions in for-each see outer handler
(define caught-in-for-each #f)
(with-exception-handler
  (lambda (e) (set! caught-in-for-each e) 'handled)
  (lambda ()
    (for-each (lambda (x) (if (= x 2) (raise-continuable 'error-2)))
              '(1 2 3))))
;; caught-in-for-each should be 'error-2

;; Test that exceptions in dynamic-wind thunks see outer handler
(define caught-in-before #f)
(with-exception-handler
  (lambda (e) (set! caught-in-before e))
  (lambda ()
    (dynamic-wind
      (lambda () (raise-continuable 'before-error))
      (lambda () 'body)
      (lambda () 'after))))
;; caught-in-before should be 'before-error
```

### 5. Update Documentation

**File:** `plans/ARCHITECTURAL_REVIEW.md`

Update M3 status:

```markdown
### M3. `NewSubContext` does not inherit exception handlers

**File:** `machine/machine_context.go:454-467`
**Status:** ✅ Fixed

Sub-contexts used for `map`, `for-each`, `dynamic-wind` thunks don't see enclosing `with-exception-handler`. R7RS says exception handlers have dynamic extent.

**Fix:** Added `exceptionHandler: p.exceptionHandler` to `NewSubContext()` to automatically inherit the parent's exception handler chain. Also updated `SubContextParams` and `NewSubContextFromParams` to include exception handler for cross-goroutine sub-context creation.

Removed manual `SetExceptionHandler` call from `PrimWithExceptionHandler` (now redundant).

Added regression tests verifying exception handler inheritance in sub-contexts and Scheme-level tests for `map`, `for-each`, and `dynamic-wind`.
```

**File:** `plans/ARCHITECTURAL_REVIEW_FIXES.md`

Add M3 section after M2:

```markdown
## M3. NewSubContext Does Not Inherit Exception Handlers

**File:** `machine/machine_context.go:454-467, 488-501`
**Batch:** 2 (Correctness requiring design thought)
**Commit:** `<commit-sha>` (TBD)

### Problem

Sub-contexts are used throughout the codebase for calling Scheme closures from Go primitives. These sub-contexts need to see exception handlers installed in the parent context for proper R7RS dynamic extent semantics, but `NewSubContext()` did not inherit the exception handler chain.

**Current behavior:**
```go
func (p *MachineContext) NewSubContext() *MachineContext {
	return &MachineContext{
		// ... copies threadID, thread, escapeCont
		// MISSING: exceptionHandler
	}
}
```

**Call sites affected:**
- `apply`, `call-with-values` (evaluating procedures in sub-contexts)
- `dynamic-wind` before/after thunks
- `with-exception-handler` thunk execution
- Parameter converters
- Delimited continuation prompts
- File I/O operations with thunks
- Thread creation across goroutine boundaries

**Workaround:** `PrimWithExceptionHandler` manually called `SetExceptionHandler(mc.ExceptionHandler())` after creating the sub-context, but other call sites did not.

### R7RS Impact

**R7RS §6.11 (Exceptions):**

> Returns the results of invoking `thunk`. The `handler` is installed as the current exception handler **in the dynamic extent** of the invocation of `thunk`.

Dynamic extent means: if you install a handler and call a function, that function should see the handler. Sub-contexts failed to inherit handlers, violating this guarantee.

**Example failure scenario:**
```scheme
(with-exception-handler
  (lambda (e) 'caught)
  (lambda ()
    (map (lambda (x) (if (zero? x) (raise-continuable 'div-by-zero) (/ 1 x)))
         '(1 2 0 3))))
;; Expected: handler catches 'div-by-zero from map's sub-context
;; Actual (before fix): exception propagates, handler not seen
```

### The Fix

Add exception handler inheritance to `NewSubContext()`:

```go
func (p *MachineContext) NewSubContext() *MachineContext {
	p.counters.SubContextsCreated++
	return &MachineContext{
		ctx: p.ctx,
		vmState: vmState{
			env:      p.env.TopLevel(),
			evals:    NewStack(),
			threadID: p.threadID,
		},
		parentMC:         p,
		escapeCont:       p.escapeCont,
		thread:           p.thread,
		exceptionHandler: p.exceptionHandler, // ← ADDED
	}
}
```

**Changed files:**
| File | Change |
|------|--------|
| `machine/machine_context.go:467` | Add `exceptionHandler: p.exceptionHandler` to NewSubContext |
| `machine/machine_context.go:472` | Add `ExceptionHandler` field to `SubContextParams` |
| `machine/machine_context.go:482` | Capture `ExceptionHandler` in `CaptureSubContextParams` |
| `machine/machine_context.go:495` | Set `exceptionHandler` in `NewSubContextFromParams` |
| `internal/extensions/exceptions/prim_exceptions.go:39` | Remove now-redundant `SetExceptionHandler` call |

### Design Rationale

**Why automatic inheritance is correct:**

Exception handlers have dynamic extent (R7RS §6.11). This means they MUST be visible to all code called within the handler's dynamic extent, including code executed in sub-contexts. Automatic inheritance ensures correctness by default - call sites don't have to remember to manually set the handler.

**Alternatives considered:**

1. **Explicit parameter:** `NewSubContext(inheritHandler bool)` - rejected because it's verbose, easy to forget, and the "wrong" default (not inheriting) violates R7RS.

2. **Separate factory method:** `NewSubContextWithHandlers()` - rejected because it doesn't fix the default behavior and requires updating all call sites.

**Consistency:** Exception handler inheritance matches how other context fields are inherited:
- `threadID` is inherited (for SRFI-18 thread identity)
- `thread` object is inherited
- `escapeCont` is inherited (for nested call/cc)

All of these have dynamic extent and are inherited by sub-contexts. Exception handlers should be treated the same way.

### Test Coverage

**Unit tests** (`machine/machine_context_test.go`):
- `TestNewSubContext_InheritsExceptionHandler`: Verifies sub-contexts inherit parent's handler
- `TestNewSubContext_InheritsNestedHandlers`: Verifies nested handler chains are preserved

**Integration tests** (`registry/core/prim_exception_test.go`):
- Exceptions in `map` see outer handler
- Exceptions in `for-each` see outer handler
- Exceptions in `dynamic-wind` before/after thunks see outer handler
- Exceptions in parameter converters see outer handler

### Performance Impact

**None.** Exception handler inheritance is a simple pointer copy (`p.exceptionHandler`) with no allocation or runtime overhead. The cost is identical to inheriting other context fields like `threadID` or `escapeCont`.

### Thread Safety Note

Exception handler chains are immutable data structures (linked list via `parent` pointers). Copying the pointer to the head of the chain is thread-safe because:
1. The chain is never mutated - push/pop create new nodes or update the context's pointer
2. Sub-contexts get an independent copy of the pointer
3. No shared mutable state between parent and sub-context

For cross-goroutine sub-contexts (`NewSubContextFromParams`), the exception handler is captured in `SubContextParams` along with other parent state, avoiding any cross-goroutine `MachineContext` access.
```

**File:** `machine/CLAUDE.local.md`

Update gotchas section:

```markdown
- **Exception handlers have dynamic extent**: Sub-contexts automatically inherit the parent's exception handler chain. Push/pop on the parent after sub-context creation does NOT affect the sub-context (it has its own copy of the pointer to the chain head).
```

## Verification Steps

### 1. Run Exception Tests

```bash
go test -v -run "Exception" ./registry/core/ ./internal/extensions/exceptions/ ./machine/
```

Expected: All tests pass, including new inheritance tests.

### 2. Run Full Test Suite

```bash
make test
```

Expected: No regressions.

### 3. Run Linter

```bash
make lint
```

Expected: 0 issues.

### 4. Manual Scheme Test

Create `test_m3.scm`:

```scheme
(import (scheme base)
        (scheme write))

(define log '())
(define (record! msg) (set! log (cons msg log)))

;; Test exception handler in map
(with-exception-handler
  (lambda (e) (record! 'caught-in-map) 'default)
  (lambda ()
    (map (lambda (x)
           (if (= x 3)
               (raise-continuable 'error-3)
               (* x 2)))
         '(1 2 3 4))))

;; Test exception handler in for-each
(with-exception-handler
  (lambda (e) (record! 'caught-in-for-each))
  (lambda ()
    (for-each (lambda (x)
                (if (= x 2)
                    (raise-continuable 'error-2)))
              '(1 2 3))))

;; Test exception handler in dynamic-wind
(with-exception-handler
  (lambda (e) (record! 'caught-in-before))
  (lambda ()
    (dynamic-wind
      (lambda () (raise-continuable 'before-error))
      (lambda () 'body)
      (lambda () 'after))))

(display log)
(newline)
;; Expected: (caught-in-before caught-in-for-each caught-in-map)
```

Run:
```bash
./dist/scheme --file test_m3.scm
```

Expected output:
```
(caught-in-before caught-in-for-each caught-in-map)
```

## Critical Files

- `/Users/aalpar/projects/wile/machine/machine_context.go` - Lines 454-467, 488-501
- `/Users/aalpar/projects/wile/internal/extensions/exceptions/prim_exceptions.go` - Line 39 (remove manual set)
- `/Users/aalpar/projects/wile/machine/machine_context_test.go` - Add unit tests
- `/Users/aalpar/projects/wile/registry/core/prim_exception_test.go` - Add integration tests
- `/Users/aalpar/projects/wile/plans/ARCHITECTURAL_REVIEW.md` - Update M3 status
- `/Users/aalpar/projects/wile/plans/ARCHITECTURAL_REVIEW_FIXES.md` - Add M3 section

## Risk Assessment

**Low risk.** This is a clear correctness bug with a straightforward fix. The only potential concern is call sites that intentionally want an empty handler chain, but:

1. No legitimate reason exists for a sub-context to NOT inherit handlers (would violate R7RS dynamic extent)
2. Audit of all call sites shows none intentionally clear the handler
3. If a call site really wanted an empty handler, it can call `SetExceptionHandler(nil)` explicitly

## References

- R7RS §6.11 (Exceptions)
- ARCHITECTURAL_REVIEW.md M3 (lines 141-146)
- Flatt 2016 "Binding as Sets of Scopes" (continuation hygiene requirements apply to exception handlers)
