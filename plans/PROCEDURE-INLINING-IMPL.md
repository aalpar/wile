# Procedure Inlining Implementation Plan

> Implement this plan task-by-task, running relevant tests after each task.

**Goal:** Inline let-bound lambda calls as synthetic `let` forms, eliminating closure allocation and call dispatch when `!Mutable && !Escapes && init is lambda`.

**Architecture:** The compiler detects inlinable calls via an inline candidate registry on `CompileTimeContinuation`, keyed by `BindingID`. `CompileValidatedLet` registers candidates; `compileValidatedCall` consumes them by constructing a synthetic `ValidatedLet` and delegating to `CompileValidatedLet`. A recursion guard prevents infinite self-inlining.

**Tech Stack:** Go, `machine/compilation`, `internal/validate`, `environment`, `wile/` (engine options)

**Design:** `plans/PROCEDURE-INLINING.md`

---

### Task 1: Add inline candidate infrastructure to CompileTimeContinuation

**Files:**
- Modify: `machine/compilation/compile_time_continuation.go` (~lines 30-66)

**Step 1: Add the types and fields**

Add the inline candidate type and two fields to `CompileTimeContinuation`:

```go
// inlineCandidate holds a let-bound lambda eligible for call-site inlining.
type inlineCandidate struct {
	lambda  *validate.ValidatedLambda
	binding *validate.ValidatedLetBinding
}
```

Add to `CompileTimeContinuation` struct (after the `evaluator` field):

```go
	// inlineCandidates maps BindingID → lambda for let-bound closures eligible
	// for call-site inlining. Populated by CompileValidatedLet, consumed by
	// compileValidatedCall. Keyed by BindingID for stable cross-scope identity.
	inlineCandidates map[environment.BindingID]inlineCandidate
	// currentlyInlining tracks bindings being inlined to prevent infinite
	// recursion for self-referential letrec bindings.
	currentlyInlining map[environment.BindingID]struct{}
	// inlineThreshold is the maximum body length (in top-level expressions)
	// for inlining eligibility. 0 disables inlining.
	inlineThreshold int
```

**Step 2: Initialize in constructor**

In `NewCompileTimeContinuation`, after setting `evaluator`, add:

```go
		inlineThreshold: DefaultInlineThreshold,
```

**Step 3: Add the constant and setter**

```go
// DefaultInlineThreshold is the default maximum body length for procedure
// inlining. A lambda body with more expressions than this is not inlined.
const DefaultInlineThreshold = 5

// SetInlineThreshold sets the maximum body length for procedure inlining.
// 0 disables inlining entirely.
func (p *CompileTimeContinuation) SetInlineThreshold(n int) {
	p.inlineThreshold = n
}
```

**Step 4: Run existing tests**

Run: `go test ./machine/compilation/...`
Expected: PASS (new fields are zero-value safe; maps initialized lazily)

**Step 5: Commit**

```
feat(compile): add inline candidate infrastructure to CompileTimeContinuation

Adds inlineCandidates map (BindingID → lambda), currentlyInlining
recursion guard, and configurable inlineThreshold (default 5). Maps
are initialized lazily to avoid allocation for code without let forms.
```

---

### Task 2: Add WithInlineThreshold engine option

**Files:**
- Modify: `options.go` (~line 95, after `WithMaxCallDepth`)
- Modify: `engine.go` (~lines 52-60, 649-652)

**Step 1: Add field to engineConfig**

In `options.go`, add to `engineConfig` struct (after `callDepthSet`):

```go
	inlineThreshold    int
	inlineThresholdSet bool
```

**Step 2: Add the option function**

In `options.go`, after `WithMaxCallDepth`:

```go
// WithInlineThreshold sets the maximum body length (in expressions) for
// procedure inlining. Let-bound lambdas with bodies larger than this are
// not inlined at call sites. Default is compilation.DefaultInlineThreshold (5).
// 0 disables inlining entirely.
func WithInlineThreshold(n int) EngineOption {
	return func(cfg *engineConfig) {
		cfg.inlineThreshold = n
		cfg.inlineThresholdSet = true
	}
}
```

**Step 3: Add field to Engine struct**

In `engine.go`, add to `Engine` struct (after `maxCallDepth`):

```go
	inlineThreshold int
```

**Step 4: Set during Engine construction**

In `NewEngine` (find where `maxCallDepth` is set from config), add the
same pattern for `inlineThreshold`:

```go
	if cfg.inlineThresholdSet {
		eng.inlineThreshold = cfg.inlineThreshold
	} else {
		eng.inlineThreshold = compilation.DefaultInlineThreshold
	}
```

**Step 5: Thread to expandAndCompile**

Add `inlineThreshold int` parameter to `expandAndCompile`:

```go
func expandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver compilation.FileResolver, inlineThreshold int) (*machine.NativeTemplate, error) {
```

After `compiler.SetFileResolver(resolver)` (or alongside it), add:

```go
	compiler.SetInlineThreshold(inlineThreshold)
```

Update all three call sites:
- `Engine.Compile()` (~line 337): pass `p.inlineThreshold`
- `Engine.compileExpr()` (~line 663): pass `p.inlineThreshold`
- `runBootstrapMacroStx()` (~line 755): pass `compilation.DefaultInlineThreshold`

**Step 6: Run tests**

Run: `go test ./... 2>&1 | head -50`
Expected: PASS (default threshold matches previous behavior of no inlining)

**Step 7: Commit**

```
feat: add WithInlineThreshold engine option

Threads inline threshold from Engine → expandAndCompile →
CompileTimeContinuation. Default 5; 0 disables. Bootstrap uses
the default constant directly.
```

---

### Task 3: Register inline candidates in CompileValidatedLet

**Files:**
- Modify: `machine/compilation/compile_let.go` (~lines 38-70)

**Step 1: Write the registration helper**

Add to `compile_let.go`:

```go
// registerInlineCandidates scans let bindings for lambdas eligible for
// call-site inlining and registers them in the compiler's candidate map.
// Called after createLetCompileEnv, before compiling the body.
//
// A binding qualifies when: !Mutable && !Escapes, init is *ValidatedLambda
// (not case-lambda), not variadic, and body length <= threshold.
func (p *CompileTimeContinuation) registerInlineCandidates(
	childEnv *environment.EnvironmentFrame,
	bindings []validate.ValidatedLetBinding,
) []environment.BindingID {
	if p.inlineThreshold == 0 {
		return nil
	}

	var registered []environment.BindingID
	for i := range bindings {
		b := &bindings[i]
		if b.Mutable || b.Escapes {
			continue
		}
		lam, ok := b.Init.(*validate.ValidatedLambda)
		if !ok {
			continue
		}
		params := lam.Params()
		if params.Rest != nil {
			continue
		}
		if len(lam.Body()) > p.inlineThreshold {
			continue
		}

		bid, resolved := childEnv.ResolveBindingID(b.Name.Sym, b.Name.Scopes())
		if !resolved {
			continue
		}

		if p.inlineCandidates == nil {
			p.inlineCandidates = make(map[environment.BindingID]inlineCandidate)
		}
		p.inlineCandidates[bid] = inlineCandidate{lambda: lam, binding: b}
		registered = append(registered, bid)
	}
	return registered
}

// unregisterInlineCandidates removes previously registered candidates
// when their enclosing let scope exits.
func (p *CompileTimeContinuation) unregisterInlineCandidates(bids []environment.BindingID) {
	for _, bid := range bids {
		delete(p.inlineCandidates, bid)
	}
}
```

**Step 2: Wire into CompileValidatedLet**

In `CompileValidatedLet`, after `childEnv := p.createLetCompileEnv(v)` and
before `savedEnv := p.env` (~line 58-60), add:

```go
	registeredBIDs := p.registerInlineCandidates(childEnv, v.Bindings)
	defer p.unregisterInlineCandidates(registeredBIDs)
```

**Step 3: Run tests**

Run: `go test ./machine/compilation/...`
Expected: PASS (registration is side-effect-free; no inlining consumed yet)

**Step 4: Commit**

```
feat(compile): register inline candidates in CompileValidatedLet

Scans let bindings after environment creation. Qualifying bindings
(!Mutable, !Escapes, non-variadic lambda, body <= threshold) are
registered by BindingID. Candidates are unregistered on scope exit.
```

---

### Task 4: Detect and inline calls in compileValidatedCall

**Files:**
- Modify: `machine/compilation/compile_validated.go` (~lines 574-609)

**Step 1: Write the inlining method**

Add to `compile_validated.go` (or a new `compile_inline.go`):

```go
// tryInlineCall checks whether a ValidatedCall can be inlined as a synthetic
// let form. Returns true if inlining was performed, false if the call should
// be compiled normally.
func (p *CompileTimeContinuation) tryInlineCall(
	ctctx CompileTimeCallContext,
	v *validate.ValidatedCall,
) (bool, error) {
	if p.inlineCandidates == nil {
		return false, nil
	}

	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return false, nil
	}

	bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
	if !resolved {
		return false, nil
	}

	candidate, found := p.inlineCandidates[bid]
	if !found {
		return false, nil
	}

	// Recursion guard: don't inline if we're already inlining this binding.
	if p.currentlyInlining != nil {
		if _, inlining := p.currentlyInlining[bid]; inlining {
			return false, nil
		}
	}

	// Arity check: call args must match lambda params exactly.
	params := candidate.lambda.Params()
	args := v.Body()
	if len(args) != len(params.Required) {
		return false, nil
	}

	// Construct synthetic ValidatedLet.
	syntheticBindings := make([]validate.ValidatedLetBinding, len(params.Required))
	for i, param := range params.Required {
		syntheticBindings[i] = validate.ValidatedLetBinding{
			Name: param,
			Init: args[i],
		}
	}

	syntheticLet := validate.NewValidatedLet(
		"let",
		v.Source(),
		validate.LetKindLet,
		syntheticBindings,
		candidate.lambda.Body(),
	)

	// Set recursion guard.
	if p.currentlyInlining == nil {
		p.currentlyInlining = make(map[environment.BindingID]struct{})
	}
	p.currentlyInlining[bid] = struct{}{}
	defer delete(p.currentlyInlining, bid)

	return true, p.CompileValidatedLet(ctctx, syntheticLet)
}
```

**Step 2: Wire into compileValidatedCall**

At the top of `compileValidatedCall` (before the SaveContinuation logic),
add:

```go
	inlined, err := p.tryInlineCall(ctctx, v)
	if err != nil {
		return err
	}
	if inlined {
		return nil
	}
```

**Step 3: Run tests**

Run: `go test ./machine/compilation/...`
Expected: PASS (inlining now active for qualifying calls)

**Step 4: Run full test suite**

Run: `make test`
Expected: PASS

**Step 5: Commit**

```
feat(compile): inline let-bound lambda calls as synthetic let forms

When compileValidatedCall encounters a call to a registered inline
candidate, it constructs a synthetic ValidatedLet mapping lambda
params to call args, then delegates to CompileValidatedLet. Eliminates
closure allocation, SaveContinuation, and Apply dispatch. Recursion
guard prevents infinite self-inlining for letrec bindings.
```

---

### Task 5: Add ValidatedLet constructor (if needed)

**Files:**
- Modify: `internal/validate/validated_forms.go` (~line 320)

Task 4 uses `validate.NewValidatedLet(...)`. Check whether this constructor
exists. If not, add it:

**Step 1: Add constructor**

```go
// NewValidatedLet constructs a ValidatedLet. Used by the compiler for
// synthetic let forms (e.g., procedure inlining).
func NewValidatedLet(
	formName string,
	source *syntax.SourceContext,
	kind LetKind,
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
) *ValidatedLet {
	return &ValidatedLet{
		validatedBase: validatedBase{formName: formName, source: source},
		Kind:          kind,
		Bindings:      bindings,
		body:          body,
	}
}
```

**Step 2: Run tests**

Run: `go test ./internal/validate/...`
Expected: PASS

**Step 3: Commit**

```
feat(validate): add NewValidatedLet constructor

Enables construction of synthetic ValidatedLet forms outside the
validation package (e.g., procedure inlining in the compiler).
```

---

### Task 6: Write failing integration tests

**Files:**
- Create: `machine/compilation/compile_inline_test.go`

**Step 1: Write table-driven integration tests**

```go
package compilation_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestInlineLetBoundLambda(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Basic inlining
		{Name: "single arg", Code: `(let ((f (lambda (x) (+ x 1)))) (f 42))`, Expected: values.NewInteger(43)},
		{Name: "two args", Code: `(let ((f (lambda (x y) (+ x y)))) (f 3 4))`, Expected: values.NewInteger(7)},
		{Name: "thunk", Code: `(let ((f (lambda () 42))) (f))`, Expected: values.NewInteger(42)},
		{Name: "multiple calls", Code: `(let ((f (lambda (x) (+ x 1)))) (+ (f 1) (f 2)))`, Expected: values.NewInteger(5)},

		// Two bindings, both inlinable
		{Name: "two inlinable", Code: `
			(let ((add (lambda (a b) (+ a b)))
			      (mul (lambda (a b) (* a b))))
			  (add (mul 2 3) (mul 4 5)))`, Expected: values.NewInteger(26)},

		// Free variables resolve correctly
		{Name: "free variable", Code: `
			(let ((x 10))
			  (let ((f (lambda (y) (+ x y))))
			    (f 32)))`, Expected: values.NewInteger(42)},

		// Tail position preserved
		{Name: "tail position", Code: `(let ((f (lambda (x) x))) (f 99))`, Expected: values.NewInteger(99)},

		// Multi-expression body
		{Name: "multi-expr body", Code: `
			(let ((f (lambda (x)
			           (define y (+ x 1))
			           (+ y 2))))
			  (f 10))`, Expected: values.NewInteger(13)},

		// Arg side effects evaluated left-to-right
		{Name: "arg eval order", Code: `
			(let ((result '()))
			  (let ((f (lambda (a b) (cons a (cons b result)))))
			    (f 1 2)))`,
			Expected: testhelpers.ParseSchemeValue(t, "(1 2)")},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestInlineSkipsNonEligible(t *testing.T) {
	// These should compile and run correctly WITHOUT inlining.
	tcs := []testhelpers.SchemeCodeTestCase{
		// Mutable binding — not inlined
		{Name: "mutable", Code: `
			(let ((f (lambda (x) x)))
			  (set! f (lambda (x) (+ x 1)))
			  (f 42))`, Expected: values.NewInteger(43)},

		// Escaped binding — not inlined
		{Name: "escaped as arg", Code: `
			(let ((f (lambda (x) (+ x 1))))
			  (map f '(1 2 3)))`,
			Expected: testhelpers.ParseSchemeValue(t, "(2 3 4)")},

		// Variadic — not inlined
		{Name: "variadic", Code: `
			(let ((f (lambda (x . rest) (cons x rest))))
			  (f 1 2 3))`,
			Expected: testhelpers.ParseSchemeValue(t, "(1 2 3)")},

		// Recursive (letrec) — first call not inlined (recursion guard)
		{Name: "letrec recursive", Code: `
			(letrec ((f (lambda (n)
			              (if (= n 0) 1
			                  (* n (f (- n 1)))))))
			  (f 5))`, Expected: values.NewInteger(120)},

		// Named let loop
		{Name: "named let", Code: `
			(let loop ((n 5) (acc 1))
			  (if (= n 0) acc
			      (loop (- n 1) (* acc n))))`, Expected: values.NewInteger(120)},
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

**Step 2: Run tests**

Run: `go test -v -run 'TestInline' ./machine/compilation/...`
Expected: PASS (these are end-to-end — they verify correct results, which
should work whether or not inlining fires)

**Step 3: Commit**

```
test(compile): add integration tests for procedure inlining

Table-driven tests covering: basic inlining (single/multi arg, thunk,
multiple calls), non-eligible cases (mutable, escaped, variadic,
recursive), free variable resolution, tail position, multi-expression
bodies, and argument evaluation order.
```

---

### Task 7: Write predicate unit tests

**Files:**
- Create: `machine/compilation/compile_inline_predicate_test.go`

These tests verify that `registerInlineCandidates` correctly identifies
which bindings are eligible. Use the test helpers from
`internal/validate/validate_capture_test.go` as a pattern for constructing
validated forms directly.

Test cases:
- Binding with `Mutable=true` → not registered
- Binding with `Escapes=true` → not registered
- Binding with non-lambda init → not registered
- Binding with variadic lambda → not registered
- Binding with body > threshold → not registered
- Binding with `!Mutable && !Escapes && lambda && body <= 5` → registered
- `inlineThreshold=0` → nothing registered
- Multiple bindings, mixed eligibility → only qualifying ones registered

**Step 1: Write the tests**

The exact test code depends on available test helpers in `machine/compilation/`.
Use `testhelpers.RunSchemeCode` for integration-level predicate tests, or
construct `ValidatedLetBinding` structs directly for unit-level tests.

**Step 2: Run tests**

Run: `go test -v -run 'TestInlinePredicate' ./machine/compilation/...`
Expected: PASS

**Step 3: Commit**

```
test(compile): add unit tests for inline candidate predicate
```

---

### Task 8: Final verification

**Step 1: Run lint**

Run: `make lint`
Expected: PASS

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: PASS

**Step 3: Run Gabriel benchmarks**

Run: `make bench-gabriel`
Expected: No regression. Benchmarks using let-bound helpers (tak, fib) may
show improvement.

**Step 4: Run extended benchmarks**

Run: `make bench-extended`
Expected: No regression.

**Step 5: Commit any fixes**

If lint or covercheck reveals issues, fix and commit.

---

## Summary of Changes

| File | Change |
|------|--------|
| `machine/compilation/compile_time_continuation.go` | Add `inlineCandidate` type, `inlineCandidates`/`currentlyInlining` maps, `inlineThreshold` field, `DefaultInlineThreshold` constant, `SetInlineThreshold` setter |
| `machine/compilation/compile_let.go` | Add `registerInlineCandidates`/`unregisterInlineCandidates`; wire into `CompileValidatedLet` |
| `machine/compilation/compile_validated.go` | Add `tryInlineCall`; wire into `compileValidatedCall` |
| `internal/validate/validated_forms.go` | Add `NewValidatedLet` constructor |
| `options.go` | Add `inlineThreshold`/`inlineThresholdSet` to `engineConfig`; add `WithInlineThreshold` option |
| `engine.go` | Add `inlineThreshold` to `Engine`; thread through `expandAndCompile` |
| `machine/compilation/compile_inline_test.go` | Integration tests |
| `machine/compilation/compile_inline_predicate_test.go` | Predicate unit tests |
