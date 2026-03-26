# Capture Analysis Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `Captured bool` to `ValidatedLetBinding` that tracks whether a binding is referenced from inside an escaping closure.

**Architecture:** Post-validation walk over the `ValidatedExpr` body (and inits for letrec/let*/letrec*). A `captureWalker` struct tracks closure depth and a BindingID-to-index map. Called at the same sites as `markMutableBindings`.

**Tech Stack:** Go, `internal/validate`, `environment.BindingID`, `environment.EnvironmentFrame.ResolveBindingID`

---

## Design Refinement: Walking Init Expressions

The design doc (`plans/CAPTURE-ANALYSIS.md`) specifies walking the body only.
This is insufficient for letrec, letrec*, and let* where init expressions see
the bindings. Example:

```scheme
(letrec ((f (lambda () x)) (x 1)) (f))
```

The body `(f)` doesn't reference `x` directly, but `f`'s init `(lambda () x)`
captures it. Without walking inits, `x` would be incorrectly marked as
non-captured.

**Rule:** Walk init expressions for all forms except plain `let` (where inits
are validated in the outer scope and can't reference the let bindings).

The `walkInits` parameter controls this:

| Kind | `walkInits` | Why |
|------|-------------|-----|
| `let` | `false` | Inits in outer scope |
| `let*` | `true` | Inits see preceding bindings (flat path has no dups, safe) |
| `letrec` | `true` | Inits see all bindings |
| `letrec*` | `true` | Inits see all bindings |

**Named let body depth:** The design doc (`plans/CAPTURE-ANALYSIS.md`) originally
specified walking the named let body at `closureDepth + 1`. This is wrong. Named
let `(let loop ((x init)) body)` compiles as `(letrec ((loop (lambda (x) body)))
(loop init))`. The body is inside the lambda *init*, not at the top level — so
when `walkInits=true` processes the init, the lambda increments depth naturally.
The body passed to `markCapturedBindings` at Site 4 is the *outer* body `(loop
init)`, which executes at depth 0. No special-casing needed.

**Known limitation:** The walker uses the outer let's `childEnv` for all symbol
resolution. When a nested `let` inside the body shadows an outer binding name,
the walker may produce a false positive (marking the outer binding as captured
when only the inner one is). This is conservative (safe) but not precise.

---

### Task 1: Add `Captured` field to `ValidatedLetBinding`

**Files:**
- Modify: `internal/validate/validated_forms.go:296-302`
- Test: `internal/validate/validated_forms_test.go`

**Step 1: Write the failing test**

In `internal/validate/validated_forms_test.go`, update `TestValidatedLet_Getters` to also assert `Captured`:

```go
// After line 390:
c.Assert(vl.Bindings[0].Captured, qt.IsFalse)
```

**Step 2: Run test to verify it passes (field addition is zero-value compatible)**

Run: `go test -v -run TestValidatedLet_Getters ./internal/validate/`
Expected: PASS (new bool field defaults to false, assertion is IsFalse)

**Step 3: Add the field**

In `internal/validate/validated_forms.go`, update the struct:

```go
// ValidatedLetBinding represents a single (name init-expr) binding pair.
// Mutable is true if the binding is targeted by set! in the body.
// Captured is true if the binding is referenced from inside an escaping closure.
type ValidatedLetBinding struct {
	Name     *syntax.SyntaxSymbol
	Init     ValidatedExpr
	Mutable  bool
	Captured bool
}
```

**Step 4: Run test to verify it still passes**

Run: `go test -v -run TestValidatedLet_Getters ./internal/validate/`
Expected: PASS

**Step 5: Commit**

```
feat(validate): add Captured field to ValidatedLetBinding
```

---

### Task 2: Write the capture walker

**Files:**
- Create: `internal/validate/validate_capture.go`
- Create: `internal/validate/validate_capture_test.go`

**Step 1: Write the failing tests**

Create `internal/validate/validate_capture_test.go` (package `validate`,
internal test — needs access to unexported types and `markCapturedBindings`):

```go
package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// makeTestEnvAndBindings creates an EnvironmentFrame with local bindings for
// the given names and returns the bindings slice with corresponding entries.
func makeTestEnvAndBindings(names ...string) (
	*environment.EnvironmentFrame,
	[]ValidatedLetBinding,
) {
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, nil)
	var bindings []ValidatedLetBinding
	for _, name := range names {
		sym := values.NewSymbol(name)
		ssym := syntax.NewSyntaxSymbol(name, nil)
		env.MaybeCreateLocalBindingWithScopes(
			sym,
			environment.BindingTypeVariable,
			nil,
			nil,
		)
		bindings = append(bindings, ValidatedLetBinding{
			Name: ssym,
			Init: &ValidatedLiteral{
				validatedBase: validatedBase{formName: "@literal"},
			},
		})
	}
	return env, bindings
}

// symRef creates a ValidatedSymbol referencing the given name.
func symRef(name string) *ValidatedSymbol {
	return &ValidatedSymbol{
		validatedBase: validatedBase{formName: "@symbol"},
		Symbol:        syntax.NewSyntaxSymbol(name, nil),
	}
}

// lit creates a ValidatedLiteral (no sub-expressions).
func lit() *ValidatedLiteral {
	return &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal"},
	}
}

// lam creates a ValidatedLambda with given body expressions and no params.
func lam(body ...ValidatedExpr) *ValidatedLambda {
	return &ValidatedLambda{
		validatedBase: validatedBase{formName: "lambda"},
		validatedProcBase: validatedProcBase{
			params: &ValidatedParams{},
			body:   body,
		},
	}
}

// call creates a ValidatedCall.
func call(proc ValidatedExpr, args ...ValidatedExpr) *ValidatedCall {
	return &ValidatedCall{
		validatedBase: validatedBase{formName: "@call"},
		proc:          proc,
		args:          args,
	}
}

// caseLam creates a ValidatedCaseLambda with a single clause.
func caseLam(body ...ValidatedExpr) *ValidatedCaseLambda {
	return &ValidatedCaseLambda{
		validatedBase: validatedBase{formName: "case-lambda"},
		clauses: []*ValidatedCaseLambdaClause{{
			validatedBase: validatedBase{formName: "@clause"},
			validatedProcBase: validatedProcBase{
				params: &ValidatedParams{},
				body:   body,
			},
		}},
	}
}

// defineFn creates a ValidatedDefine in function form (body at depth+1).
func defineFn(name string, body ...ValidatedExpr) *ValidatedDefine {
	return &ValidatedDefine{
		validatedBase: validatedBase{formName: "define"},
		validatedProcBase: validatedProcBase{
			params: &ValidatedParams{},
			body:   body,
		},
		name:       syntax.NewSyntaxSymbol(name, nil),
		IsFunction: true,
	}
}

// defineVal creates a ValidatedDefine in value form (expr at current depth).
func defineVal(name string, expr ValidatedExpr) *ValidatedDefine {
	return &ValidatedDefine{
		validatedBase: validatedBase{formName: "define"},
		name:          syntax.NewSyntaxSymbol(name, nil),
		subExp:        expr,
		IsFunction:    false,
	}
}

// applyExpr creates a ValidatedApply.
func applyExpr(proc ValidatedExpr, prefixArgs []ValidatedExpr, finalList ValidatedExpr) *ValidatedApply {
	return &ValidatedApply{
		validatedBase: validatedBase{formName: "apply"},
		Proc:          proc,
		PrefixArgs:    prefixArgs,
		FinalList:     finalList,
	}
}

// nestedLet creates a ValidatedLet for nesting inside another let's body.
func nestedLet(bindings []ValidatedLetBinding, body ...ValidatedExpr) *ValidatedLet {
	return &ValidatedLet{
		validatedBase: validatedBase{formName: "let"},
		Kind:          LetKindLet,
		Bindings:      bindings,
		body:          body,
	}
}

func TestMarkCapturedBindings_DirectReference(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) x) — direct reference, no lambda
	body := []ValidatedExpr{symRef("x")}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_EscapingLambda(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (lambda () x)) — escaping lambda captures x
	body := []ValidatedExpr{lam(symRef("x"))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_ImmediatelyApplied(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) ((lambda () x))) — immediately applied, not captured
	body := []ValidatedExpr{call(lam(symRef("x")))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_NestedEscape(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) ((lambda () (lambda () x)))) —
	// outer lambda immediately applied, inner escapes → captured
	body := []ValidatedExpr{call(lam(lam(symRef("x"))))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_PartialCapture(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x", "y")
	// (let ((x 1) (y 2)) (lambda () x)) — x captured, y not
	body := []ValidatedExpr{lam(symRef("x"))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
	c.Assert(bindings[1].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_CallArgNotCaptured(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (+ x 1)) — x as call arg, not inside lambda
	body := []ValidatedExpr{call(symRef("+"), symRef("x"), lit())}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_WalkInits(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f", "x")
	// (letrec ((f (lambda () x)) (x 1)) (f))
	// f's init captures x → x is captured
	bindings[0].Init = lam(symRef("x"))
	body := []ValidatedExpr{call(symRef("f"))}
	markCapturedBindings(env, bindings, body, true)
	c.Assert(bindings[1].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_WalkInitsFalseSkipsInits(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// Plain let: init has lambda but walkInits=false → not walked
	bindings[0].Init = lam(symRef("x"))
	body := []ValidatedExpr{symRef("x")}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_IfBranches(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (if #t (lambda () x) x))
	body := []ValidatedExpr{
		&ValidatedIf{
			validatedBase: validatedBase{formName: "if"},
			Test:          lit(),
			Conseq:        lam(symRef("x")),
			Alt:           symRef("x"),
		},
	}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_NilEnv(t *testing.T) {
	c := qt.New(t)
	// Graceful no-op when env is nil
	bindings := []ValidatedLetBinding{{
		Name: syntax.NewSyntaxSymbol("x", nil),
		Init: lit(),
	}}
	body := []ValidatedExpr{lam(symRef("x"))}
	markCapturedBindings(nil, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_EmptyBindings(t *testing.T) {
	env, _ := makeTestEnvAndBindings()
	// No bindings — nothing to capture
	body := []ValidatedExpr{lam(symRef("x"))}
	markCapturedBindings(env, nil, body, false)
	// No panic, no crash
}

func TestMarkCapturedBindings_ImmediatelyAppliedCaseLambda(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) ((case-lambda (() x)))) — immediately applied, not captured
	body := []ValidatedExpr{call(caseLam(symRef("x")))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_EscapingCaseLambda(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (case-lambda (() x))) — escaping case-lambda captures x
	body := []ValidatedExpr{caseLam(symRef("x"))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_DefineFunctionCaptures(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (define (f) x) ...) — function define body is a closure
	body := []ValidatedExpr{defineFn("f", symRef("x")), symRef("x")}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_DefineValueNoClosure(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (define y x)) — value define, no closure boundary
	body := []ValidatedExpr{defineVal("y", symRef("x"))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_ApplyWithLambdaArg(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (apply f (lambda () x) '()))
	// lambda in apply prefix args is escaping → captures x
	body := []ValidatedExpr{
		applyExpr(symRef("f"), []ValidatedExpr{lam(symRef("x"))}, lit()),
	}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_CrossLetBoundaryCapture(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (let ((f (lambda () x))) (f)))
	// Inner let's init has lambda capturing outer x → outer x is captured.
	innerBindings := []ValidatedLetBinding{{
		Name: syntax.NewSyntaxSymbol("f", nil),
		Init: lam(symRef("x")),
	}}
	body := []ValidatedExpr{nestedLet(innerBindings, call(symRef("f")))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestMarkCapturedBindings ./internal/validate/`
Expected: FAIL — `markCapturedBindings` undefined

**Step 3: Write the implementation**

Create `internal/validate/validate_capture.go`:

```go
package validate

import "github.com/aalpar/wile/environment"

// markCapturedBindings walks the validated body (and optionally init
// expressions) to determine which let bindings are referenced from inside
// escaping closures. A lambda is non-escaping only when it appears as the
// operator of a ValidatedCall (immediately-applied lambda).
//
// walkInits should be true for let*, letrec, and letrec* (where inits see the
// bindings) and false for plain let (where inits are in the outer scope).
func markCapturedBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
	walkInits bool,
) {
	if childEnv == nil || len(bindings) == 0 {
		return
	}

	// Build BindingID → index map for the let bindings.
	idToIdx := make(map[environment.BindingID]int, len(bindings))
	for i, b := range bindings {
		bid, ok := childEnv.ResolveBindingID(b.Name.Sym, b.Name.Scopes())
		if ok {
			idToIdx[bid] = i
		}
	}
	if len(idToIdx) == 0 {
		return
	}

	w := captureWalker{
		env:      childEnv,
		bindings: bindings,
		idToIdx:  idToIdx,
	}

	if walkInits {
		for _, b := range bindings {
			w.walkExpr(b.Init, 0)
		}
	}
	for _, expr := range body {
		w.walkExpr(expr, 0)
	}
}

// captureWalker walks a ValidatedExpr tree tracking closure depth.
type captureWalker struct {
	env      *environment.EnvironmentFrame
	bindings []ValidatedLetBinding
	idToIdx  map[environment.BindingID]int
}

func (p *captureWalker) walkExpr(expr ValidatedExpr, depth int) {
	if expr == nil {
		return
	}
	switch e := expr.(type) {
	case *ValidatedSymbol:
		if depth > 0 {
			bid, ok := p.env.ResolveBindingID(e.Symbol.Sym, e.Symbol.Scopes())
			if ok {
				if idx, found := p.idToIdx[bid]; found {
					p.bindings[idx].Captured = true
				}
			}
		}

	case *ValidatedLambda:
		p.walkBody(e.Body(), depth+1)

	case *ValidatedCaseLambda:
		for _, clause := range e.Clauses() {
			p.walkBody(clause.Body(), depth+1)
		}

	case *ValidatedCall:
		switch proc := e.Proc().(type) {
		case *ValidatedLambda:
			// Immediately applied — walk body at current depth
			p.walkBody(proc.Body(), depth)
		case *ValidatedCaseLambda:
			// Immediately applied — walk each clause at current depth
			for _, clause := range proc.Clauses() {
				p.walkBody(clause.Body(), depth)
			}
		default:
			p.walkExpr(e.Proc(), depth)
		}
		// Walk args at current depth in all cases
		for _, arg := range e.Body() {
			p.walkExpr(arg, depth)
		}

	case *ValidatedIf:
		p.walkExpr(e.Test, depth)
		p.walkExpr(e.Conseq, depth)
		p.walkExpr(e.Alt, depth)

	case *ValidatedBegin:
		p.walkBody(e.Body(), depth)

	case *ValidatedSetBang:
		p.walkExpr(e.SubExp(), depth)

	case *ValidatedLet:
		// Nested let: walk inits and body at current depth to find
		// references to the OUTER let's bindings through lambdas in
		// the inner scope. The inner let handles its own bindings via
		// its own markCapturedBindings call at validation time.
		for _, b := range e.Bindings {
			p.walkExpr(b.Init, depth)
		}
		p.walkBody(e.Body(), depth)

	case *ValidatedDynamicWind:
		p.walkExpr(e.Before, depth)
		p.walkExpr(e.Thunk, depth)
		p.walkExpr(e.After, depth)

	case *ValidatedWithContinuationMark:
		p.walkExpr(e.Key, depth)
		p.walkExpr(e.Val, depth)
		p.walkExpr(e.Body, depth)

	case *ValidatedApply:
		p.walkExpr(e.Proc, depth)
		for _, arg := range e.PrefixArgs {
			p.walkExpr(arg, depth)
		}
		p.walkExpr(e.FinalList, depth)

	case *ValidatedDefine:
		if e.IsFunction {
			// (define (f x) body) — the body is inside a closure
			p.walkBody(e.Body(), depth+1)
		} else {
			p.walkExpr(e.SubExp(), depth)
		}

	case *ValidatedQuote, *ValidatedLiteral, *ValidatedQuasiquote:
		// No sub-expressions to walk

	default:
		// Unknown validated form — conservative skip.
		// ValidatedLiteral passthrough forms (syntax-case, import, etc.)
		// don't contain sub-expressions that reference let bindings.
	}
}

func (p *captureWalker) walkBody(body []ValidatedExpr, depth int) {
	for _, expr := range body {
		p.walkExpr(expr, depth)
	}
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestMarkCapturedBindings ./internal/validate/`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
feat(validate): add capture walker for let bindings

Walks the ValidatedExpr body to detect which let bindings are
referenced from inside escaping closures. A lambda in call position
((lambda ...) args) is treated as non-escaping (B1 analysis).
```

---

### Task 3: Wire into call sites

**Files:**
- Modify: `internal/validate/validate_let.go` (4 sites)

**Step 1: Add calls to `markCapturedBindings` at each site**

Site 1 — `validateLetBindingsAndBody` (~line 113):

```go
	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, false)
```

Site 2 — `validateLetStarFlat` (~line 240):

```go
	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, true)
```

Site 3 — `validateLetStarNested` (~line 303):

```go
		markMutableBindings(vb.childEnv, []ValidatedLetBinding{vb.binding}, result)
		markCapturedBindings(vb.childEnv, []ValidatedLetBinding{vb.binding}, innerBody, true)
```

Note: for the nested path, `innerBody` is the body of the current node (the next
inner node or the final body). Each binding has its own env with only that
binding, so resolution is correct.

Site 4 — `validateLetrecBindingsAndBody` (~line 421):

```go
	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, true)
```

**Step 2: Run all existing let tests**

Run: `go test -v -run 'TestLet|TestLetStar|TestLetrec|TestNamedLet' ./internal/validate/`
Expected: PASS (Captured defaults to false, no behavior change)

**Step 3: Run full test suite**

Run: `go test ./internal/validate/...`
Expected: PASS

**Step 4: Commit**

```
feat(validate): wire capture analysis into let validation

Calls markCapturedBindings at all 4 let validation sites, alongside
the existing markMutableBindings. walkInits=true for let*/letrec/letrec*
where init expressions can reference the bindings.
```

---

### Task 4: Full-pipeline integration tests

**Files:**
- Modify: `internal/validate/validate_capture_test.go`

These tests verify the `Captured` flag through the full expand + validate
pipeline using `testhelpers.RunSchemeCode` indirectly. Since we can't inspect
the flag through the public execution API (the compiler doesn't use it yet),
these tests parse + expand + validate and inspect the `ValidatedLet` directly.

However, creating a full expand+validate test helper is out of scope for this
task. The unit tests from Task 2 cover the walker logic thoroughly. Full-pipeline
tests can be added when the compiler starts using the `Captured` flag.

**Step 1: Add a test for the named let case**

Named let produces a `ValidatedLet` with `LetKindLetrec` and `Tag != nil`. The
tag binding's init is a lambda (the loop body), which references the tag for
recursive calls. Since `walkInits=true` (letrec), the tag is captured.

Add to `validate_capture_test.go`:

```go
func TestMarkCapturedBindings_NamedLetPattern(t *testing.T) {
	c := qt.New(t)
	// Named let produces: (letrec ((loop (lambda (x) ... (loop ...)))) (loop init))
	// The tag "loop" is referenced inside its own lambda init → captured.
	env, bindings := makeTestEnvAndBindings("loop")
	bindings[0].Init = lam(
		call(symRef("loop"), lit()), // recursive call inside lambda
	)
	body := []ValidatedExpr{call(symRef("loop"), lit())}
	markCapturedBindings(env, bindings, body, true) // letrec: walkInits=true
	c.Assert(bindings[0].Captured, qt.IsTrue)
}
```

**Step 2: Add a test for let* init capture**

```go
func TestMarkCapturedBindings_LetStarInitCapture(t *testing.T) {
	c := qt.New(t)
	// (let* ((x 1) (y (lambda () x))) y)
	// y's init captures x. With walkInits=true, x is captured.
	env, bindings := makeTestEnvAndBindings("x", "y")
	bindings[1].Init = lam(symRef("x"))
	body := []ValidatedExpr{symRef("y")}
	markCapturedBindings(env, bindings, body, true)
	c.Assert(bindings[0].Captured, qt.IsTrue)
	c.Assert(bindings[1].Captured, qt.IsFalse)
}
```

**Step 3: Run all capture tests**

Run: `go test -v -run TestMarkCapturedBindings ./internal/validate/`
Expected: PASS

**Step 4: Run full test suite + lint**

Run: `make lint && go test ./...`
Expected: PASS

**Step 5: Commit**

```
test(validate): add integration-pattern tests for capture analysis

Named let (tag always captured via recursive reference in init),
let* init capture (preceding binding captured through lambda in
later init).
```

---

### Task 5: Final verification

**Step 1: Run Gabriel benchmarks to verify no regression**

Run: `make bench-gabriel`
Expected: No significant regression (capture analysis adds a cheap AST walk)

**Step 2: Run full test suite**

Run: `make test`
Expected: PASS

**Step 3: Run lint and covercheck**

Run: `make lint && make covercheck`
Expected: PASS

---

## Summary of changes

| File | Change |
|------|--------|
| `internal/validate/validated_forms.go` | Add `Captured bool` to `ValidatedLetBinding` |
| `internal/validate/validate_capture.go` | New file: `markCapturedBindings` + `captureWalker` |
| `internal/validate/validate_capture_test.go` | New file: unit tests for capture analysis |
| `internal/validate/validate_let.go` | Add `markCapturedBindings` calls at 4 sites |
| `internal/validate/validated_forms_test.go` | Assert `Captured` in getter test |
