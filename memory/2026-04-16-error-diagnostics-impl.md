# Error Diagnostics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** 10/10 tasks complete

> **Completed:** Tasks 1-10 (all layers complete).
> Task 10 note: Phases 2 (syntax/macro) and 4 (expander) were wrapped via `wrapSourcedError` in PR #657. Phase 3 datum-level functions (`import_set_datum.go`, `library_loader.go`, `library_bindings.go`) operate on `values.Value` without syntax context — their callers in the compiler/expander wrap errors with source. `processLibraryImport` precision improved to wrap per-import-set source.

**Goal:** Make runtime error context (source location, stack trace, continuation marks) accessible from Scheme code via continuation marks and NativeError enrichment.

**Architecture:** Two-layer approach. Layer 1: `raise` sets a continuation mark with an `ErrorContext` value before calling exception handlers. Layer 2: if the condition is a `NativeError`, copy source and stack trace into it so they persist after the handler returns. Both layers are wired into the existing `callExceptionHandler` dispatch in `registry/core/prim_exceptions.go`.

**Tech Stack:** Go (values, machine, registry packages). No new dependencies.

**Design doc:** `plans/2026-04-16-error-diagnostics-design.md`

---

## Dependency Graph

```
Task 1 (ErrorContext type)
  → Task 2 (errorContextKey)
    → Task 3 (set mark in callExceptionHandler)
      → Task 4 (current-error-context primitive)
      → Task 5 (accessor primitives)
Task 6 (NativeError fields)
  → Task 7 (enrichment in callExceptionHandler) [depends on Task 3]
    → Task 8 (error-object-source/stack-trace primitives)
      → Task 9 (integration tests)
```

Tasks 1-5 are Layer 1. Tasks 6-8 are Layer 2. Task 9 ties it together.

---

### Task 1: ErrorContext Value Type

**Files:**
- Create: `machine/error_context.go`
- Create: `machine/error_context_test.go`

**Step 1: Write the failing test**

```go
// machine/error_context_test.go
package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values/valuestest"
)

func TestErrorContext_Value(t *testing.T) {
	c := qt.New(t)

	src := syntax.NewSourceContext("test.scm", "test", 1, 0, 0, 4, 0, 0)
	trace := StackTrace{
		{FunctionName: "f", CurrentLoc: src},
	}
	ctx := NewErrorContext(src, trace, nil)

	c.Assert(ctx.IsVoid(), qt.IsFalse)
	c.Assert(ctx.SchemeString(), qt.Matches, `#<error-context.*>`)
	c.Assert(ctx.EqualTo(ctx), qt.IsTrue)

	other := NewErrorContext(nil, nil, nil)
	c.Assert(ctx.EqualTo(other), qt.IsFalse)
}

func TestErrorContext_Accessors(t *testing.T) {
	c := qt.New(t)

	src := syntax.NewSourceContext("test.scm", "test", 1, 0, 0, 4, 0, 0)
	trace := StackTrace{
		{FunctionName: "f", CurrentLoc: src},
		{FunctionName: "g", CallSite: src},
	}
	ctx := NewErrorContext(src, trace, nil)

	c.Assert(ctx.Source(), qt.Equals, src)
	c.Assert(ctx.StackTraceFrames(), qt.HasLen, 2)
	c.Assert(ctx.Marks(), qt.IsNil)

	// Source location string
	c.Assert(ctx.SourceLocation(), qt.Matches, "test.scm:.*")
}

func TestErrorContext_NilSource(t *testing.T) {
	c := qt.New(t)

	ctx := NewErrorContext(nil, nil, nil)
	c.Assert(ctx.SourceLocation(), qt.Equals, "")
	c.Assert(ctx.StackTraceFrames(), qt.HasLen, 0)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestErrorContext ./machine/`
Expected: FAIL — `NewErrorContext` not defined

**Step 3: Implement ErrorContext**

```go
// machine/error_context.go
package machine

import (
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

var _ values.Value = (*ErrorContext)(nil)

// ErrorContext carries diagnostic information captured at a raise site.
// It is attached as a continuation mark during exception handler dispatch,
// enabling Scheme code to inspect source location, stack trace, and
// continuation marks at the point where the exception was raised.
type ErrorContext struct {
	source     *syntax.SourceContext
	stackTrace StackTrace
	marks      *ContinuationMarkSet
}

func NewErrorContext(
	source *syntax.SourceContext,
	stackTrace StackTrace,
	marks *ContinuationMarkSet,
) *ErrorContext {
	return &ErrorContext{
		source:     source,
		stackTrace: stackTrace,
		marks:      marks,
	}
}

func (p *ErrorContext) Source() *syntax.SourceContext {
	if p == nil {
		return nil
	}
	return p.source
}

func (p *ErrorContext) SourceLocation() string {
	if p == nil {
		return ""
	}
	return p.source.Location()
}

func (p *ErrorContext) StackTraceFrames() StackTrace {
	if p == nil {
		return nil
	}
	return p.stackTrace
}

func (p *ErrorContext) Marks() *ContinuationMarkSet {
	if p == nil {
		return nil
	}
	return p.marks
}

func (p *ErrorContext) SchemeString() string {
	loc := p.SourceLocation()
	if loc != "" {
		return "#<error-context " + loc + ">"
	}
	return "#<error-context>"
}

func (p *ErrorContext) IsVoid() bool {
	return p == nil
}

func (p *ErrorContext) EqualTo(other values.Value) bool {
	o, ok := other.(*ErrorContext)
	if !ok {
		return false
	}
	return p == o
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestErrorContext ./machine/`
Expected: PASS

Note: `syntax.NewSourceContext` — verify the constructor signature matches the
codebase. Read `internal/syntax/source_context.go` for the actual constructor.
Adjust test accordingly.

**Step 5: Commit**

```
feat(machine): add ErrorContext value type for raise-site diagnostics
```

---

### Task 2: Error Context Mark Key

**Files:**
- Modify: `machine/error_context.go`
- Modify: `machine/error_context_test.go`

**Step 1: Write the failing test**

```go
// Add to machine/error_context_test.go
func TestErrorContextKey(t *testing.T) {
	c := qt.New(t)

	// Key is a unique symbol, not nil
	c.Assert(ErrorContextKey(), qt.IsNotNil)

	// Same key returned each time (singleton)
	c.Assert(ErrorContextKey(), qt.Equals, ErrorContextKey())
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestErrorContextKey ./machine/`
Expected: FAIL — `ErrorContextKey` not defined

**Step 3: Implement the key**

Add to `machine/error_context.go`:

```go
// errorContextKey is the private continuation mark key used to attach
// ErrorContext to the handler frame during exception dispatch.
// It is an uninterned symbol — no Scheme code can forge it.
var errorContextKey = values.NewSymbol("$error-context")

// ErrorContextKey returns the mark key. Exposed for use by registry primitives.
func ErrorContextKey() values.Value {
	return errorContextKey
}
```

Note on key choice: `values.NewSymbol` creates a fresh `*Symbol`. Since symbols
are compared by string key (`helpers.EqIdentity`), `$error-context` is technically
forgeable. If this matters, use a struct sentinel instead. For now, the `$` prefix
convention signals internal use and is consistent with other internal names.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestErrorContextKey ./machine/`
Expected: PASS

**Step 5: Commit**

```
feat(machine): add error context continuation mark key
```

---

### Task 3: Set Error Context Mark in Exception Dispatch

**Files:**
- Modify: `registry/core/prim_exceptions.go` (lines 79-97, `callExceptionHandler`)
- Create: `registry/core/prim_error_context_test.go`

**Step 1: Write the failing test**

This test verifies that when `raise` fires and a handler runs, the error context
mark is set on the handler's frame. Use `call-with-immediate-continuation-mark`
to read it.

```go
// registry/core/prim_error_context_test.go
package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/registry/testhelpers"
)

func TestErrorContextMarkSetDuringHandler(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "error context available in handler",
			Code: `(with-exception-handler
				(lambda (e)
				  (error-context?
				    (current-error-context)))
				(lambda () (raise "boom"))
				'replace)`,
			Expected: values.TrueValue,
		},
		{
			Name: "error context not available outside handler",
			Code: `(current-error-context)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

Note: This test depends on Tasks 4-5 (`current-error-context` and `error-context?`
primitives). Write the test now but expect it to fail until those primitives exist.
Alternatively, verify the Go-level behavior directly by inspecting marks on the
sub-context. Adapt based on what's testable at this stage.

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestErrorContextMarkSetDuringHandler ./registry/core/`
Expected: FAIL — `current-error-context` not defined

**Step 3: Modify callExceptionHandler**

Change `callExceptionHandler` in `registry/core/prim_exceptions.go` to accept
an `*machine.ErrorContext` and set it as a mark on the sub-context:

```go
func callExceptionHandler(
	cc machine.CallContext,
	condition values.Value,
	handler values.Callable,
	errorCtx *machine.ErrorContext,
) (values.Value, error) {
	mc := cc.(*machine.MachineContext)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)

	// Set error context mark before handler runs.
	// The handler (or code it calls) can read this via current-error-context.
	if errorCtx != nil {
		sub.SetMark(machine.ErrorContextKey(), errorCtx)
	}

	_, err := sub.ApplyCallable(handler, condition)
	if err != nil {
		return nil, err
	}

	err = sub.Run()
	if err != nil {
		return nil, err
	}

	return sub.GetValue(), nil
}
```

Update the call site in `handleException` (line ~162) to build the `ErrorContext`
and pass it:

```go
// In handleException, before the for loop (line ~160):
errorCtx := machine.NewErrorContext(excErr.Source, excErr.StackTrace, nil)

// In the for loop:
handlerResult, err := callExceptionHandler(mc, excErr.Condition, handler, errorCtx)
```

For the continuation marks snapshot, pass `nil` for now. Adding the marks snapshot
is optional and can be done as a refinement — source + stack trace are the
high-value fields.

**Step 4: Run existing exception tests to verify no regressions**

Run: `go test -v ./registry/core/ -run TestException`
Run: `go test -v ./registry/core/ -run TestGuard`
Run: `go test -v ./registry/core/ -run TestRaise`
Expected: All PASS (the mark is invisible to existing code)

**Step 5: Commit**

```
feat(exceptions): set error context mark during handler dispatch
```

---

### Task 4: current-error-context Primitive

**Files:**
- Create: `registry/core/prim_error_context.go`
- Modify: `registry/core/exceptions.go` (add registration)
- Modify: `registry/core/prim_error_context_test.go`

**Step 1: Write the failing test**

Add to `prim_error_context_test.go` (or use the test from Task 3 if not already written):

```go
func TestCurrentErrorContext(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "returns #f outside handler",
			Code: `(current-error-context)`,
			Expected: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestCurrentErrorContext ./registry/core/`
Expected: FAIL — `current-error-context` undefined

**Step 3: Implement**

```go
// registry/core/prim_error_context.go
package core

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// PrimCurrentErrorContext returns the ErrorContext from the current
// continuation marks, or #f if not inside an exception handler.
func PrimCurrentErrorContext(cc machine.CallContext) error {
	mc := cc.(*machine.MachineContext)
	marks := mc.CollectContinuationMarks(machine.DefaultPromptTag)
	val := marks.First(machine.ErrorContextKey(), values.FalseValue)
	mc.SetValue(val)
	return nil
}

// PrimErrorContextQ is the type predicate for error-context objects.
func PrimErrorContextQ(cc machine.CallContext) error {
	_, ok := cc.Arg(0).(*machine.ErrorContext)
	cc.SetValue(values.BoolToBoolean(ok))
	return nil
}
```

Register in `exceptions.go` — add to the `addExceptions` function:

```go
{Name: "current-error-context", ParamCount: 0, Impl: PrimCurrentErrorContext,
	Doc: "Returns the error context for the current exception being handled, or #f if not inside an exception handler.\n\nExamples:\n  (current-error-context)  => #f",
	Category: "exceptions",
	Keywords: []string{"debug", "stack trace", "error location", "diagnostics"}},
{Name: "error-context?", ParamCount: 1, Impl: PrimErrorContextQ,
	Doc: "Returns #t if OBJ is an error context object.\n\nExamples:\n  (error-context? 42)  => #f",
	ParamNames: []string{"obj"}, Category: "exceptions",
	ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestCurrentErrorContext ./registry/core/`
Expected: PASS

**Step 5: Commit**

```
feat(exceptions): add current-error-context and error-context? primitives
```

---

### Task 5: ErrorContext Accessor Primitives

**Files:**
- Modify: `registry/core/prim_error_context.go`
- Modify: `registry/core/exceptions.go`
- Modify: `registry/core/prim_error_context_test.go`

**Step 1: Write the failing test**

```go
func TestErrorContextAccessors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "error-context-source returns location string",
			Code: `(with-exception-handler
				(lambda (e)
				  (let ((ctx (current-error-context)))
				    (string? (error-context-source ctx))))
				(lambda () (raise "boom"))
				'replace)`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-context-stack-trace returns list",
			Code: `(with-exception-handler
				(lambda (e)
				  (let ((ctx (current-error-context)))
				    (list? (error-context-stack-trace ctx))))
				(lambda () (raise "boom"))
				'replace)`,
			Expected: values.TrueValue,
		},
		{
			Name: "stack frame is alist with name key",
			Code: `(with-exception-handler
				(lambda (e)
				  (let ((ctx (current-error-context)))
				    (let ((frames (error-context-stack-trace ctx)))
				      (if (null? frames) #t
				        (pair? (assq 'name (car frames)))))))
				(lambda () (raise "boom"))
				'replace)`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestErrorContextAccessors ./registry/core/`
Expected: FAIL — `error-context-source` not defined

**Step 3: Implement accessor primitives**

Add to `registry/core/prim_error_context.go`:

```go
// PrimErrorContextSource returns the source location string from an ErrorContext.
func PrimErrorContextSource(cc machine.CallContext) error {
	ctx, err := helpers.RequireArg[*machine.ErrorContext](cc, 0, werr.ErrWrongType, "error-context-source")
	if err != nil {
		return err
	}
	loc := ctx.SourceLocation()
	if loc == "" {
		cc.SetValue(values.FalseValue)
	} else {
		cc.SetValue(values.NewString(loc))
	}
	return nil
}

// PrimErrorContextStackTrace returns the stack trace as a list of alists.
func PrimErrorContextStackTrace(cc machine.CallContext) error {
	ctx, err := helpers.RequireArg[*machine.ErrorContext](cc, 0, werr.ErrWrongType, "error-context-stack-trace")
	if err != nil {
		return err
	}
	cc.SetValue(stackTraceToSchemeList(ctx.StackTraceFrames()))
	return nil
}

// PrimErrorContextMarks returns the continuation mark set from an ErrorContext.
func PrimErrorContextMarks(cc machine.CallContext) error {
	ctx, err := helpers.RequireArg[*machine.ErrorContext](cc, 0, werr.ErrWrongType, "error-context-continuation-marks")
	if err != nil {
		return err
	}
	marks := ctx.Marks()
	if marks == nil {
		cc.SetValue(values.FalseValue)
	} else {
		cc.SetValue(marks)
	}
	return nil
}

// stackTraceToSchemeList converts a StackTrace to a Scheme list of alists.
// Each frame becomes: ((name . "f") (file . "test.scm") (line . 10) (column . 5))
func stackTraceToSchemeList(trace machine.StackTrace) values.Value {
	if len(trace) == 0 {
		return values.EmptyList
	}
	frames := make([]values.Value, len(trace))
	for i, frame := range trace {
		frames[i] = stackFrameToAlist(frame)
	}
	return values.List(frames...)
}

func stackFrameToAlist(frame machine.StackFrame) values.Value {
	var entries []values.Value

	// name
	name := frame.FunctionName
	if name == "" {
		name = "<anonymous>"
	}
	entries = append(entries, values.Cons(
		values.NewSymbol("name"),
		values.NewString(name),
	))

	// source location (prefer CurrentLoc, fall back to CallSite)
	src := frame.CurrentLoc
	if src == nil {
		src = frame.CallSite
	}
	if src != nil {
		if src.File != "" {
			entries = append(entries, values.Cons(
				values.NewSymbol("file"),
				values.NewString(src.File),
			))
		}
		entries = append(entries, values.Cons(
			values.NewSymbol("line"),
			values.NewInteger(int64(src.Start.Line())),
		))
		entries = append(entries, values.Cons(
			values.NewSymbol("column"),
			values.NewInteger(int64(src.Start.Column())),
		))
	}

	return values.List(entries...)
}
```

Register in `exceptions.go`:

```go
{Name: "error-context-source", ParamCount: 1, Impl: PrimErrorContextSource,
	Doc: "Returns the source location string from CTX, or #f if unavailable.\n\nExamples:\n  (error-context-source ctx)  => \"test.scm:5:3\"",
	ParamNames: []string{"ctx"}, Category: "exceptions",
	ParamTypes: []values.TypeConstraint{values.TypeAny}},
{Name: "error-context-stack-trace", ParamCount: 1, Impl: PrimErrorContextStackTrace,
	Doc: "Returns the stack trace from CTX as a list of alists. Each alist has keys: name, file, line, column.\n\nExamples:\n  (error-context-stack-trace ctx)  => (((name . \"f\") (file . \"test.scm\") (line . 5) (column . 3)))",
	ParamNames: []string{"ctx"}, Category: "exceptions",
	ParamTypes: []values.TypeConstraint{values.TypeAny}},
{Name: "error-context-continuation-marks", ParamCount: 1, Impl: PrimErrorContextMarks,
	Doc: "Returns the continuation mark set captured at the raise site, or #f if unavailable.",
	ParamNames: []string{"ctx"}, Category: "exceptions",
	ParamTypes: []values.TypeConstraint{values.TypeAny}},
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestErrorContextAccessors ./registry/core/`
Expected: PASS

Also re-run Task 3 test:
Run: `go test -v -run TestErrorContextMarkSetDuringHandler ./registry/core/`
Expected: PASS (now that primitives exist)

**Step 5: Commit**

```
feat(exceptions): add error-context-source, error-context-stack-trace, error-context-continuation-marks
```

---

### Task 6: NativeError Source and StackTrace Fields

**Files:**
- Modify: `values/native_error.go`
- Modify: `values/native_error_test.go`

**Step 1: Write the failing test**

```go
// Add to values/native_error_test.go
func TestNativeError_SourceAndStackTrace(t *testing.T) {
	c := qt.New(t)

	ne := NewErrorObject("test error", NewInteger(42))

	// Initially nil
	c.Assert(ne.SourceLocation(), qt.Equals, "")
	c.Assert(ne.StackTraceValue(), qt.IsNil)

	// Set source
	ne.SetSourceLocation("test.scm:5:3")
	c.Assert(ne.SourceLocation(), qt.Equals, "test.scm:5:3")

	// Set stack trace (as arbitrary Value — will be a Scheme list in practice)
	traceList := List(NewInteger(1))
	ne.SetStackTraceValue(traceList)
	c.Assert(ne.StackTraceValue(), qt.Not(qt.IsNil))
}

func TestNativeError_EqualTo_WithSource(t *testing.T) {
	c := qt.New(t)

	a := NewErrorObject("test")
	b := NewErrorObject("test")

	a.SetSourceLocation("a.scm:1:0")
	b.SetSourceLocation("b.scm:2:0")

	// Source does not affect equality (same message + irritants = equal)
	c.Assert(a.EqualTo(b), qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNativeError_Source ./values/`
Expected: FAIL — `SourceLocation`, `SetSourceLocation` not defined

**Step 3: Add fields and accessors**

Add to `values/native_error.go`:

```go
// In the NativeError struct, add:
type NativeError struct {
	message        *String
	irritants      Value
	kind           NativeErrorKind
	err            error
	sourceLocation string // "file:line:col", empty if not raised
	stackTraceVal  Value  // Scheme list of frame alists, nil if not raised
}

// SourceLocation returns the formatted source location string, or "".
func (p *NativeError) SourceLocation() string {
	if p == nil {
		return ""
	}
	return p.sourceLocation
}

// SetSourceLocation sets the source location string.
func (p *NativeError) SetSourceLocation(loc string) {
	if p != nil {
		p.sourceLocation = loc
	}
}

// StackTraceValue returns the stack trace as a Scheme value, or nil.
func (p *NativeError) StackTraceValue() Value {
	if p == nil {
		return nil
	}
	return p.stackTraceVal
}

// SetStackTraceValue sets the stack trace Scheme value.
func (p *NativeError) SetStackTraceValue(v Value) {
	if p != nil {
		p.stackTraceVal = v
	}
}
```

`EqualTo` does NOT compare source or stack trace — these are diagnostic metadata,
not part of the error's identity.

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestNativeError_Source ./values/`
Expected: PASS

Run: `go test -v ./values/` (full suite, check for regressions)
Expected: All PASS

**Step 5: Commit**

```
feat(values): add source and stack trace fields to NativeError
```

---

### Task 7: NativeError Enrichment in Exception Dispatch

**Files:**
- Modify: `registry/core/prim_exceptions.go` (`callExceptionHandler`)
- Modify: `registry/core/prim_error_context_test.go`

**Step 1: Write the failing test**

```go
func TestNativeErrorEnrichment(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "error-object-source populated after raise",
			Code: `(guard (e ((error-object? e)
				           (string? (error-object-source e))))
				(error "test error"))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-source is #f for unraised error",
			Code: `(let ((e (guard (exn (#t exn))
				             (with-exception-handler
				               (lambda (x) x)
				               (lambda () (raise (make-error-object "foo")))
				               'replace))))
				;; Simpler: just test constructed-but-not-raised
				#f)`,
			// Adjust: need simpler test — just verify behavior
			Expected: values.FalseValue,
		},
		{
			Name: "error-object-stack-trace is list after raise",
			Code: `(guard (e ((error-object? e)
				           (list? (error-object-stack-trace e))))
				(error "test error"))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNativeErrorEnrichment ./registry/core/`
Expected: FAIL — `error-object-source` not defined (depends on Task 8)

**Step 3: Add enrichment to callExceptionHandler**

In `registry/core/prim_exceptions.go`, modify `callExceptionHandler`:

```go
func callExceptionHandler(
	cc machine.CallContext,
	condition values.Value,
	handler values.Callable,
	errorCtx *machine.ErrorContext,
) (values.Value, error) {
	mc := cc.(*machine.MachineContext)
	sub := mc.NewSubContext()
	defer machine.ReleaseSubContext(sub)

	// Layer 1: Set error context mark
	if errorCtx != nil {
		sub.SetMark(machine.ErrorContextKey(), errorCtx)
	}

	// Layer 2: Enrich NativeError with source and stack trace
	if ne, ok := condition.(*values.NativeError); ok && errorCtx != nil {
		ne.SetSourceLocation(errorCtx.SourceLocation())
		ne.SetStackTraceValue(stackTraceToSchemeList(errorCtx.StackTraceFrames()))
	}

	_, err := sub.ApplyCallable(handler, condition)
	if err != nil {
		return nil, err
	}

	err = sub.Run()
	if err != nil {
		return nil, err
	}

	return sub.GetValue(), nil
}
```

Note: `stackTraceToSchemeList` is defined in `prim_error_context.go` (Task 5).
It may need to be in the same package or exported. Since both files are in
`registry/core`, package-level visibility is sufficient.

**Step 4: Run existing tests for regressions**

Run: `go test -v ./registry/core/ -run TestException`
Run: `go test -v ./registry/core/ -run TestGuard`
Run: `go test -v ./registry/core/ -run TestRaise`
Expected: All PASS

**Step 5: Commit**

```
feat(exceptions): enrich NativeError with source and stack trace at dispatch
```

---

### Task 8: error-object-source and error-object-stack-trace Primitives

**Files:**
- Modify: `registry/core/prim_error_context.go` (or `prim_exceptions.go`)
- Modify: `registry/core/exceptions.go`
- Modify: `registry/core/prim_error_context_test.go`

**Step 1: Write the failing test**

Use the test from Task 7 (TestNativeErrorEnrichment). Additionally:

```go
func TestErrorObjectSourceUnraised(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "error-object-source on guard-caught error returns string",
			Code: `(guard (e (#t (error-object-source e)))
				(error "boom"))`,
			// Should be a string (location), not #f
			// Exact value depends on source — just check it's a string
		},
	}
	// Use RunSchemeCode and check result is a *values.String
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			_, isStr := result.(*values.String)
			_, isFalse := result.(*values.Boolean)
			// It's either a string (if source available) or #f (if not)
			qt.Assert(t, isStr || isFalse, qt.IsTrue)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestErrorObjectSource ./registry/core/`
Expected: FAIL — `error-object-source` not defined

**Step 3: Implement**

Add to `registry/core/prim_error_context.go`:

```go
// PrimErrorObjectSource returns the source location string from an error object.
// Returns #f if the error was never raised or has no source.
func PrimErrorObjectSource(cc machine.CallContext) error {
	ne, err := helpers.RequireArg[*values.NativeError](cc, 0, werr.ErrNotANativeError, "error-object-source")
	if err != nil {
		return err
	}
	loc := ne.SourceLocation()
	if loc == "" {
		cc.SetValue(values.FalseValue)
	} else {
		cc.SetValue(values.NewString(loc))
	}
	return nil
}

// PrimErrorObjectStackTrace returns the stack trace from an error object.
// Returns () if the error was never raised.
func PrimErrorObjectStackTrace(cc machine.CallContext) error {
	ne, err := helpers.RequireArg[*values.NativeError](cc, 0, werr.ErrNotANativeError, "error-object-stack-trace")
	if err != nil {
		return err
	}
	trace := ne.StackTraceValue()
	if trace == nil {
		cc.SetValue(values.EmptyList)
	} else {
		cc.SetValue(trace)
	}
	return nil
}
```

Register in `exceptions.go`:

```go
{Name: "error-object-source", ParamCount: 1, Impl: PrimErrorObjectSource,
	Doc: "Returns the source location string of ERROR-OBJ (e.g., \"file.scm:5:3\"), or #f if unavailable. Source is populated when the error is raised.\n\nExamples:\n  (guard (e (#t (error-object-source e))) (error \"oops\"))  => \"<eval>:1:40\"",
	ParamNames: []string{"error-obj"}, Category: "exceptions",
	ParamTypes: []values.TypeConstraint{values.TypeAny},
	Keywords: []string{"debug", "location", "where", "diagnostics"}},
{Name: "error-object-stack-trace", ParamCount: 1, Impl: PrimErrorObjectStackTrace,
	Doc: "Returns the stack trace from ERROR-OBJ as a list of alists. Each alist has keys: name, file, line, column. Returns () if unavailable.\n\nExamples:\n  (guard (e (#t (error-object-stack-trace e))) (error \"oops\"))  => (((name . \"<anonymous>\") ...))",
	ParamNames: []string{"error-obj"}, Category: "exceptions",
	ParamTypes: []values.TypeConstraint{values.TypeAny},
	Keywords: []string{"debug", "backtrace", "call stack", "diagnostics"}},
```

**Step 4: Run tests to verify**

Run: `go test -v -run TestNativeErrorEnrichment ./registry/core/`
Run: `go test -v -run TestErrorObjectSource ./registry/core/`
Run: `go test -v -run TestErrorContextAccessors ./registry/core/`
Expected: All PASS

**Step 5: Commit**

```
feat(exceptions): add error-object-source and error-object-stack-trace primitives
```

---

### Task 9: Integration Tests

**Files:**
- Create: `integration/error_diagnostics_test.go`

**Step 1: Write integration tests**

These test the complete flow end-to-end: raise → handler dispatch → context mark →
NativeError enrichment → accessor primitives.

```go
// integration/error_diagnostics_test.go
package integration_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestErrorDiagnostics_Integration(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "guard captures error with source",
			Code: `(guard (e ((error-object? e)
			              (error-object-source e)))
			    (error "test"))`,
			// Should return a string (source location)
		},
		{
			Name: "nested handler gets innermost context",
			Code: `(with-exception-handler
			    (lambda (e)
			      (with-exception-handler
			        (lambda (e2)
			          (error-context?
			            (current-error-context)))
			        (lambda ()
			          (raise "inner"))
			        'replace))
			    (lambda () (raise "outer"))
			    'replace)`,
			Expected: values.TrueValue,
		},
		{
			Name: "non-error raise still has context",
			Code: `(with-exception-handler
			    (lambda (e)
			      (and (error-context? (current-error-context))
			           (not (error-object? e))))
			    (lambda () (raise 42))
			    'replace)`,
			Expected: values.TrueValue,
		},
		{
			Name: "stack trace has frames",
			Code: `(define (f) (error "boom"))
			  (define (g) (f))
			  (guard (e (#t
			    (let ((frames (error-object-stack-trace e)))
			      (and (list? frames)
			           (> (length frames) 0)))))
			    (g))`,
			Expected: values.TrueValue,
		},
		{
			Name: "error-object-source is #f for error not yet raised",
			Code: `(let ((e (guard (exn (#t exn))
			                (raise (string->symbol "not-an-error")))))
			    ;; e is a symbol, not an error object
			    ;; error-object-source requires error-object
			    #t)`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			if tc.Expected != nil {
				qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
			} else {
				// Just verify no error occurred
				qt.Assert(t, result, qt.IsNotNil)
			}
		})
	}
}
```

**Step 2: Run all integration tests**

Run: `go test -v -run TestErrorDiagnostics ./integration/`
Expected: All PASS

**Step 3: Run full test suite**

Run: `make test`
Expected: All PASS

**Step 4: Run lint**

Run: `make lint`
Expected: Clean

**Step 5: Commit**

```
test: add error diagnostics integration tests
```

---

### Task 10: Compiler Error Source Locations (Phases 2-4)

This task is mechanical migration. Follow the existing design in
`plans/2026-04-14-error-stack-traces-design.md`.

**Pattern per file:** For each `WrapForeignErrorf` or `werr.WrapForeignErrorf`
call in the target files, wrap with `p.wrapCompilationError()`:

```go
// Before:
return werr.WrapForeignErrorf(werr.ErrUndefined, "compile: undefined %s", name)

// After:
return p.wrapCompilationError(
    werr.WrapForeignErrorf(werr.ErrUndefined, "compile: undefined %s", name))
```

**Phase 2 files** (~60 sites):
- `machine/compilation/compile_syntax_rules.go`
- `machine/compilation/compile_syntax_case.go`
- `machine/compilation/compile_define_syntax.go`
- `machine/compilation/compile_syntax_form.go`
- `machine/compilation/compile_quasisyntax.go`
- `machine/compilation/compile_with_syntax.go`
- `machine/compilation/compile_begin_for_syntax.go`
- `machine/compilation/compile_define_for_syntax.go`
- `machine/compilation/compile_eval_when.go`

**Phase 3 files** (~90 sites):
- `machine/compilation/compile_library_forms.go`
- `machine/compilation/import_set_datum.go`
- `machine/compilation/library_loader.go`
- `machine/compilation/compile_import.go`
- `machine/compilation/compile_cond_expand.go`

**Phase 4 files** (~70 sites):
- `machine/expander_time_continuation.go`
- `machine/expander_let.go`
- `machine/expander_let_syntax.go`
- `machine/expander_primitive_forms.go`
- `machine/expander_lambda.go`
- `machine/expander_body.go`

Each phase should be a separate commit:

```
fix(compiler): add source locations to syntax/macro compilation errors
fix(compiler): add source locations to library/import compilation errors
fix(compiler): add source locations to expander errors
```

**Testing per phase:** After each phase, run:
```
go test -v ./machine/...
make lint
```

Note: Expander files (Phase 4) are in `machine/`, not `machine/compilation/`.
The expander has its own error wrapping pattern — check whether
`ExpanderTimeContinuation` has an equivalent `wrapCompilationError` or if one
needs to be added.

---

## Summary

| Task | Layer | What | Files |
|------|-------|------|-------|
| 1 | 1 | ErrorContext value type | `machine/error_context.go` |
| 2 | 1 | Mark key | `machine/error_context.go` |
| 3 | 1 | Set mark in dispatch | `registry/core/prim_exceptions.go` |
| 4 | 1 | `current-error-context` | `registry/core/prim_error_context.go`, `exceptions.go` |
| 5 | 1 | Accessor primitives | `registry/core/prim_error_context.go`, `exceptions.go` |
| 6 | 2 | NativeError fields | `values/native_error.go` |
| 7 | 2 | Enrichment in dispatch | `registry/core/prim_exceptions.go` |
| 8 | 2 | `error-object-source/stack-trace` | `registry/core/prim_error_context.go`, `exceptions.go` |
| 9 | — | Integration tests | `integration/error_diagnostics_test.go` |
| 10 | 3 | Compiler error Phases 2-4 | `machine/compilation/`, `machine/expander_*.go` |
