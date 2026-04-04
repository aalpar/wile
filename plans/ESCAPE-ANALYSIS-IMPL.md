# Escape Analysis Implementation Plan (B2)

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `Escapes bool` to `ValidatedLetBinding` that tracks whether a binding is referenced in a non-call position (argument, return value, init expression — but NOT `set!`, which is tracked by `Mutable`).

**Architecture:** Post-validation walk using a shared `WalkSubExprs` function that consolidates the validated expression type switch into one location. An `escapeWalker` struct holds the binding map and plugs into `WalkSubExprs` via callback. No depth tracking needed — call position is reported by `WalkSubExprs` via the `callPosition bool` parameter. Called at the same 5 sites as `markCapturedBindings`.

**Tech Stack:** Go, `internal/validate`, `environment.BindingID`, `environment.EnvironmentFrame.ResolveBindingID`

**Design:** `plans/ESCAPE-ANALYSIS.md`

**Dependency:** Task 2 introduces `WalkSubExprs` — a standalone function that walks all sub-expressions of a `ValidatedExpr`, reporting `callPosition bool` for each child. This is the #1 (simple) variant described in the design doc. The full `ChildRole` enum variant (#2, adding `RoleClosureBody` for B1 refactoring) is a separate TODO item.

---

## Design Note: set! Does Not Mark Escapes

`Mutable`, `Captured`, and `Escapes` form an implicational base — three
orthogonal bits where each carries information not derivable from the others.
`set!` is mutation, tracked by `Mutable`. The `ValidatedSetBang` case walks the
value expression (to detect non-call references to other bindings in the RHS)
but does NOT mark the target as escaped.

The compiler's inlining predicate: `!Mutable && !Escapes && init is lambda`.

---

### Task 1: Add `Escapes` field to `ValidatedLetBinding`

**Files:**
- Modify: `internal/validate/validated_forms.go` (~line 307)
- Modify: `internal/validate/validated_forms_test.go` (~line 391)

**Step 1: Update the struct**

In `internal/validate/validated_forms.go`, add `Escapes` to the struct and
update the comment:

```go
// ValidatedLetBinding represents a single (name init-expr) binding pair.
// Mutable is true if the binding is targeted by set! in the body.
// Captured is true if the binding is referenced from inside an escaping closure.
// Escapes is true if the binding is referenced in a non-call position.
type ValidatedLetBinding struct {
	Name     *syntax.SyntaxSymbol
	Init     ValidatedExpr
	Mutable  bool
	Captured bool
	Escapes  bool
}
```

**Step 2: Add assertion in getter test**

In `internal/validate/validated_forms_test.go`, after the `Captured` assertion
(~line 391):

```go
c.Assert(vl.Bindings[0].Escapes, qt.IsFalse)
```

**Step 3: Run test to verify**

Run: `go test -v -run TestValidatedLet_Getters ./internal/validate/`
Expected: PASS (new bool field defaults to false)

**Step 4: Commit**

```
feat(validate): add Escapes field to ValidatedLetBinding

Tracks whether a let binding is referenced in a non-call position
(argument, return value, init expression). Orthogonal to Mutable
(set! mutation) and Captured (closure capture). B2 analysis
infrastructure — compiler does not use this field yet.
```

---

### Task 2a: Write WalkSubExprs

**Files:**
- Create: `internal/validate/walk_sub_exprs.go`
- Create: `internal/validate/walk_sub_exprs_test.go`

**Goal:** Consolidate the validated expression type switch into a single
standalone function. Analysis passes (B2 escape, and eventually B1 capture)
plug in callbacks instead of duplicating structural recursion.

This is the #1 (simple) variant: `callPosition bool`. The full `ChildRole`
enum (#2) is a separate TODO item for when B1 is refactored.

**Step 1: Write the implementation**

Create `internal/validate/walk_sub_exprs.go`:

```go
package validate

// WalkSubExprs calls fn for every direct sub-expression of expr.
//
// callPosition is true only for the operator of ValidatedCall and
// ValidatedApply. All other sub-expressions (arguments, bodies, inits,
// branch arms, etc.) pass callPosition=false.
//
// ValidatedSetBang: walks only the value expression (callPosition=false).
// The set! target is mutation (tracked by Mutable), not a reference.
//
// ValidatedSymbol has no children — fn is not called. The caller handles
// symbols directly before calling WalkSubExprs.
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, callPosition bool)) {
	if expr == nil {
		return
	}
	switch e := expr.(type) {
	case *ValidatedCall:
		fn(e.Proc(), true)
		for _, arg := range e.Body() {
			fn(arg, false)
		}

	case *ValidatedApply:
		fn(e.Proc, true)
		for _, arg := range e.PrefixArgs {
			fn(arg, false)
		}
		fn(e.FinalList, false)

	case *ValidatedLambda:
		for _, b := range e.Body() {
			fn(b, false)
		}

	case *ValidatedCaseLambda:
		for _, clause := range e.Clauses() {
			for _, b := range clause.Body() {
				fn(b, false)
			}
		}

	case *ValidatedIf:
		fn(e.Test, false)
		fn(e.Conseq, false)
		fn(e.Alt, false)

	case *ValidatedBegin:
		for _, b := range e.Body() {
			fn(b, false)
		}

	case *ValidatedSetBang:
		fn(e.SubExp(), false)

	case *ValidatedLet:
		for _, b := range e.Bindings {
			fn(b.Init, false)
		}
		for _, b := range e.Body() {
			fn(b, false)
		}

	case *ValidatedDynamicWind:
		fn(e.Before, false)
		fn(e.Thunk, false)
		fn(e.After, false)

	case *ValidatedWithContinuationMark:
		fn(e.Key, false)
		fn(e.Val, false)
		fn(e.Body, false)

	case *ValidatedDefine:
		if e.IsFunction {
			for _, b := range e.Body() {
				fn(b, false)
			}
		} else {
			fn(e.SubExp(), false)
		}

	case *ValidatedQuote, *ValidatedLiteral, *ValidatedQuasiquote, *ValidatedSymbol:
		// No sub-expressions
	}
}
```

**Step 2: Write tests**

Create `internal/validate/walk_sub_exprs_test.go`. Tests verify that
`WalkSubExprs` correctly enumerates children and reports `callPosition`
for each validated form type:

```go
package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/syntax"
)

// collectChildren calls WalkSubExprs and collects (expr, callPosition) pairs.
type childEntry struct {
	expr         ValidatedExpr
	callPosition bool
}

func collectChildren(expr ValidatedExpr) []childEntry {
	var result []childEntry
	WalkSubExprs(expr, func(child ValidatedExpr, callPos bool) {
		result = append(result, childEntry{child, callPos})
	})
	return result
}

func TestWalkSubExprs_Call(t *testing.T) {
	c := qt.New(t)
	proc := symRef("f")
	arg1 := symRef("x")
	arg2 := lit()
	expr := call(proc, arg1, arg2)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	c.Assert(children[0].expr, qt.Equals, proc)
	c.Assert(children[0].callPosition, qt.IsTrue)
	c.Assert(children[1].expr, qt.Equals, arg1)
	c.Assert(children[1].callPosition, qt.IsFalse)
	c.Assert(children[2].expr, qt.Equals, arg2)
	c.Assert(children[2].callPosition, qt.IsFalse)
}

func TestWalkSubExprs_Apply(t *testing.T) {
	c := qt.New(t)
	proc := symRef("f")
	prefix := symRef("x")
	final := lit()
	expr := applyExpr(proc, []ValidatedExpr{prefix}, final)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	c.Assert(children[0].callPosition, qt.IsTrue)
	c.Assert(children[1].callPosition, qt.IsFalse)
	c.Assert(children[2].callPosition, qt.IsFalse)
}

func TestWalkSubExprs_Lambda(t *testing.T) {
	c := qt.New(t)
	b1 := lit()
	b2 := symRef("x")
	expr := lam(b1, b2)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 2)
	c.Assert(children[0].callPosition, qt.IsFalse)
	c.Assert(children[1].callPosition, qt.IsFalse)
}

func TestWalkSubExprs_If(t *testing.T) {
	c := qt.New(t)
	test := lit()
	conseq := symRef("x")
	alt := symRef("y")
	expr := &ValidatedIf{
		validatedBase: validatedBase{formName: "if"},
		Test: test, Conseq: conseq, Alt: alt,
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	for _, ch := range children {
		c.Assert(ch.callPosition, qt.IsFalse)
	}
}

func TestWalkSubExprs_SetBang(t *testing.T) {
	c := qt.New(t)
	val := symRef("x")
	expr := setBang("f", val)
	children := collectChildren(expr)
	// Only the value expression, not the target
	c.Assert(len(children), qt.Equals, 1)
	c.Assert(children[0].expr, qt.Equals, val)
	c.Assert(children[0].callPosition, qt.IsFalse)
}

func TestWalkSubExprs_Symbol(t *testing.T) {
	// Symbols have no children
	children := collectChildren(symRef("x"))
	c := qt.New(t)
	c.Assert(len(children), qt.Equals, 0)
}

func TestWalkSubExprs_Literal(t *testing.T) {
	children := collectChildren(lit())
	c := qt.New(t)
	c.Assert(len(children), qt.Equals, 0)
}

func TestWalkSubExprs_Let(t *testing.T) {
	c := qt.New(t)
	init1 := lit()
	init2 := symRef("x")
	body1 := symRef("y")
	bindings := []ValidatedLetBinding{
		{Name: syntax.NewSyntaxSymbol("a", nil), Init: init1},
		{Name: syntax.NewSyntaxSymbol("b", nil), Init: init2},
	}
	expr := nestedLet(bindings, body1)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3) // 2 inits + 1 body
	for _, ch := range children {
		c.Assert(ch.callPosition, qt.IsFalse)
	}
}

func TestWalkSubExprs_DynamicWind(t *testing.T) {
	c := qt.New(t)
	expr := &ValidatedDynamicWind{
		validatedBase: validatedBase{formName: "dynamic-wind"},
		Before: symRef("a"), Thunk: symRef("b"), After: symRef("c"),
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	for _, ch := range children {
		c.Assert(ch.callPosition, qt.IsFalse)
	}
}

func TestWalkSubExprs_Nil(t *testing.T) {
	// No panic on nil
	WalkSubExprs(nil, func(child ValidatedExpr, callPos bool) {
		t.Fatal("should not be called")
	})
}
```

**Step 3: Run tests**

Run: `go test -v -run TestWalkSubExprs ./internal/validate/`
Expected: PASS

**Step 4: Run lint**

Run: `make lint`
Expected: PASS

**Step 5: Commit**

```
feat(validate): add WalkSubExprs for validated expression traversal

Standalone function that enumerates the direct sub-expressions of any
ValidatedExpr, reporting callPosition=true for the operator of
ValidatedCall and ValidatedApply. Consolidates the per-form type switch
into one location so analysis passes plug in callbacks instead of
duplicating structural recursion.
```

---

### Task 2b: Write the escape walker using WalkSubExprs

**Files:**
- Create: `internal/validate/validate_escape.go`
- Create: `internal/validate/validate_escape_test.go`

**Step 1: Write the failing tests**

Create `internal/validate/validate_escape_test.go` (package `validate`,
internal test — reuses helpers from `validate_capture_test.go`):

```go
package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/syntax"
)

func TestMarkEscapedBindings_CallPosition(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 1))) (f)) — f in call position
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{call(symRef("f"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_Returned(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 1))) f) — f returned (non-call)
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{symRef("f")}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_PassedAsArg(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 1))) (g f)) — f as argument
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{call(symRef("g"), symRef("f"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_CallAndNonCall(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 1))) (f) f) — one call, one non-call
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{call(symRef("f")), symRef("f")}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_ApplyCallPosition(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 1))) (apply f '())) — apply proc is call position
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{applyExpr(symRef("f"), nil, lit())}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_TwoBindings(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f", "g")
	// (let ((f ...) (g ...)) (f) (g)) — both call-only
	bindings[0].Init = lam(lit())
	bindings[1].Init = lam(lit())
	body := []ValidatedExpr{call(symRef("f")), call(symRef("g"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
	c.Assert(bindings[1].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_TwoBindingsPartialEscape(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f", "g")
	// (let ((f ...) (g ...)) (f) g) — f call-only, g returned
	bindings[0].Init = lam(lit())
	bindings[1].Init = lam(lit())
	body := []ValidatedExpr{call(symRef("f")), symRef("g")}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
	c.Assert(bindings[1].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_SetBangDoesNotMarkEscapes(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 1))) (set! f (lambda () 2)))
	// set! is mutation (Mutable), not escape
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{setBang("f", lam(lit()))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_SetBangValueExprWalked(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f", "g")
	// (let ((f ...) (g ...)) (set! f g))
	// f: not escaped (set! target). g: escaped (non-call in RHS).
	bindings[0].Init = lam(lit())
	bindings[1].Init = lam(lit())
	body := []ValidatedExpr{setBang("f", symRef("g"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
	c.Assert(bindings[1].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_CallInsideClosure(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 42))) (lambda () (f)))
	// f is called inside escaping closure — still call position
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{lam(call(symRef("f")))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_NonCallInsideClosure(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f (lambda () 42))) (lambda () f))
	// f in non-call position inside closure
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{lam(symRef("f"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_NonLambdaBinding(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) x) — non-lambda, non-call reference
	body := []ValidatedExpr{symRef("x")}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_IfBothBranchesCall(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (if #t (f) (f))) — call in both branches
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{
		&ValidatedIf{
			validatedBase: validatedBase{formName: "if"},
			Test:          lit(),
			Conseq:        call(symRef("f")),
			Alt:           call(symRef("f")),
		},
	}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_IfOneBranchNonCall(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (if #t f (f))) — non-call in consequent
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{
		&ValidatedIf{
			validatedBase: validatedBase{formName: "if"},
			Test:          lit(),
			Conseq:        symRef("f"),
			Alt:           call(symRef("f")),
		},
	}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_NilEnv(t *testing.T) {
	c := qt.New(t)
	bindings := []ValidatedLetBinding{{
		Name: syntax.NewSyntaxSymbol("f", nil),
		Init: lam(lit()),
	}}
	body := []ValidatedExpr{symRef("f")}
	markEscapedBindings(nil, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_EmptyBindings(t *testing.T) {
	env, _ := makeTestEnvAndBindings()
	body := []ValidatedExpr{symRef("f")}
	markEscapedBindings(env, nil, body, false)
	// No panic, no crash
}

func TestMarkEscapedBindings_WalkInits(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f", "g")
	// (let* ((f (lambda () 1)) (g f)) (g))
	// f used as init for g (non-call) with walkInits=true → escapes
	bindings[0].Init = lam(lit())
	bindings[1].Init = symRef("f")
	body := []ValidatedExpr{call(symRef("g"))}
	markEscapedBindings(env, bindings, body, true)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
	c.Assert(bindings[1].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_WalkInitsFalseSkipsInits(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// Plain let: init references f but walkInits=false → not walked
	bindings[0].Init = symRef("f")
	body := []ValidatedExpr{call(symRef("f"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_NamedLetPattern(t *testing.T) {
	c := qt.New(t)
	// Named let: (let loop ((x 1)) (if (= x 0) x (loop (- x 1))))
	// loop: always in call position → !Escapes
	// x: used as argument → Escapes
	env, bindings := makeTestEnvAndBindings("loop", "x")
	bindings[0].Init = lam(
		&ValidatedIf{
			validatedBase: validatedBase{formName: "if"},
			Test:          call(symRef("="), symRef("x"), lit()),
			Conseq:        symRef("x"),
			Alt:           call(symRef("loop"), call(symRef("-"), symRef("x"), lit())),
		},
	)
	body := []ValidatedExpr{call(symRef("loop"), lit())}
	markEscapedBindings(env, bindings, body, true)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
	c.Assert(bindings[1].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_LetrecSelfRecursiveCall(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (letrec ((f (lambda () (f)))) (f))
	// f only in call position (self-recursive + body call)
	bindings[0].Init = lam(call(symRef("f")))
	body := []ValidatedExpr{call(symRef("f"))}
	markEscapedBindings(env, bindings, body, true)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_DefineFunctionBody(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (define (g) (f)))
	// f in call position inside define function body
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{defineFn("g", call(symRef("f")))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_DefineValueNonCall(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (define g f))
	// f in non-call position (value define)
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{defineVal("g", symRef("f"))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_ApplyNonCallArgs(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f", "g")
	// (let ((f ...) (g ...)) (apply f g '()))
	// f: call position (apply proc). g: non-call (prefix arg).
	bindings[0].Init = lam(lit())
	bindings[1].Init = lam(lit())
	body := []ValidatedExpr{applyExpr(symRef("f"), []ValidatedExpr{symRef("g")}, lit())}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
	c.Assert(bindings[1].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_NestedLetInitEscapes(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (let ((g f)) (g)))
	// f used as init for inner let binding (non-call)
	bindings[0].Init = lam(lit())
	innerBindings := []ValidatedLetBinding{{
		Name: syntax.NewSyntaxSymbol("g", nil),
		Init: symRef("f"),
	}}
	body := []ValidatedExpr{nestedLet(innerBindings, call(symRef("g")))}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_BeginSequence(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (begin (f) (f)))
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{
		&ValidatedBegin{
			validatedBase: validatedBase{formName: "begin"},
			body:          []ValidatedExpr{call(symRef("f")), call(symRef("f"))},
		},
	}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}

func TestMarkEscapedBindings_DynamicWindCallPosition(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (dynamic-wind f f f))
	// f in all three positions — none are call position
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{
		&ValidatedDynamicWind{
			validatedBase: validatedBase{formName: "dynamic-wind"},
			Before:        symRef("f"),
			Thunk:         symRef("f"),
			After:         symRef("f"),
		},
	}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsTrue)
}

func TestMarkEscapedBindings_WithContinuationMark(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("f")
	// (let ((f ...)) (with-continuation-mark 'k 'v (f)))
	// f in call position inside body
	bindings[0].Init = lam(lit())
	body := []ValidatedExpr{
		&ValidatedWithContinuationMark{
			validatedBase: validatedBase{formName: "with-continuation-mark"},
			Key:           lit(),
			Val:           lit(),
			Body:          call(symRef("f")),
		},
	}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestMarkEscapedBindings ./internal/validate/`
Expected: FAIL — `markEscapedBindings` undefined

**Step 3: Write the implementation**

Create `internal/validate/validate_escape.go`:

```go
package validate

import "github.com/aalpar/wile/environment"

// markEscapedBindings walks the validated body (and optionally init
// expressions) to determine which let bindings are referenced in non-call
// positions (argument, return value, init expression). A reference is in
// call position only when it is the Proc of a ValidatedCall or ValidatedApply.
//
// set! targets are NOT marked as escaped — mutation is tracked by Mutable.
// The three fields (Mutable, Captured, Escapes) form an implicational base:
// each carries information not derivable from the others.
//
// walkInits should be true for let*, letrec, and letrec* (where inits see the
// bindings) and false for plain let (where inits are in the outer scope).
//
// Best-effort: if binding resolution fails (scope mismatch), the binding
// stays non-escaped. Must not gate correctness-critical optimizations
// without re-validation.
func markEscapedBindings(
	childEnv *environment.EnvironmentFrame,
	bindings []ValidatedLetBinding,
	body []ValidatedExpr,
	walkInits bool,
) {
	if childEnv == nil || len(bindings) == 0 {
		return
	}

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

	w := escapeWalker{
		env:      childEnv,
		bindings: bindings,
		idToIdx:  idToIdx,
	}

	if walkInits {
		for _, b := range bindings {
			w.walkExpr(b.Init)
		}
	}
	for _, expr := range body {
		w.walkExpr(expr)
	}
}

// escapeWalker walks a ValidatedExpr tree detecting non-call-position
// references to tracked let bindings. Uses WalkSubExprs for structural
// recursion — only the symbol check and call-position logic are here.
type escapeWalker struct {
	env      *environment.EnvironmentFrame
	bindings []ValidatedLetBinding
	idToIdx  map[environment.BindingID]int
}

func (p *escapeWalker) walkExpr(expr ValidatedExpr) {
	if expr == nil {
		return
	}
	// Symbols are leaf nodes — check for non-call-position reference.
	if sym, ok := expr.(*ValidatedSymbol); ok {
		bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
		if resolved {
			if idx, found := p.idToIdx[bid]; found {
				p.bindings[idx].Escapes = true
			}
		}
		return
	}
	// Structural recursion via WalkSubExprs.
	WalkSubExprs(expr, func(child ValidatedExpr, callPosition bool) {
		if callPosition {
			if sym, ok := child.(*ValidatedSymbol); ok {
				bid, resolved := p.env.ResolveBindingID(sym.Symbol.Sym, sym.Symbol.Scopes())
				if resolved {
					if _, tracked := p.idToIdx[bid]; tracked {
						// Call position — do NOT mark Escapes.
						return
					}
				}
			}
		}
		p.walkExpr(child)
	})
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestMarkEscapedBindings ./internal/validate/`
Expected: PASS

**Step 5: Run lint**

Run: `make lint`
Expected: PASS

**Step 6: Commit**

```
feat(validate): add escape walker for let bindings

Uses WalkSubExprs for structural recursion. Only the symbol check
and call-position skip logic are in the escape walker itself. A
symbol in ValidatedCall.Proc or ValidatedApply.Proc is call position;
all other references mark
Escapes=true. set! targets are not marked (tracked by Mutable).
B2 analysis infrastructure.
```

---

### Task 3: Wire into call sites

**Files:**
- Modify: `internal/validate/validate_let.go` (5 sites)

**Step 1: Add calls to `markEscapedBindings` at each site**

Site 1 — `validateLetBindingsAndBody` (~line 114):

```go
	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, false)
	markEscapedBindings(childEnv, bindings, body, false)
```

Site 2 — `validateLetStarFlat` (~line 242):

```go
	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, true)
	markEscapedBindings(childEnv, bindings, body, true)
```

Site 3 — `validateLetStarNested` (~line 306-307):

```go
		markMutableBindings(vb.childEnv, bindings, result)
		markCapturedBindings(vb.childEnv, bindings, innerBody, true)
		markEscapedBindings(vb.childEnv, bindings, innerBody, true)
```

Site 4 — `validateLetrecBindingsAndBody` (~line 425-426):

```go
	markMutableBindings(childEnv, bindings, result)
	markCapturedBindings(childEnv, bindings, body, true)
	markEscapedBindings(childEnv, bindings, body, true)
```

Site 5 — named let tag (~line 502-503):

```go
	markMutableBindings(tagEnv, tagBindings, result)
	markCapturedBindings(tagEnv, tagBindings, tagBody, true)
	markEscapedBindings(tagEnv, tagBindings, tagBody, true)
```

**Step 2: Run all existing let tests**

Run: `go test -v -run 'TestLet|TestLetStar|TestLetrec|TestNamedLet' ./internal/validate/`
Expected: PASS (Escapes defaults to false, no behavior change)

**Step 3: Run full test suite**

Run: `go test ./internal/validate/...`
Expected: PASS

**Step 4: Commit**

```
feat(validate): wire escape analysis into let validation

Calls markEscapedBindings at all 5 let validation sites, alongside
markMutableBindings and markCapturedBindings. walkInits=true for
let*/letrec/letrec* where init expressions see the bindings.
```

---

### Task 4: Final verification

**Step 1: Run Gabriel benchmarks to verify no regression**

Run: `make bench-gabriel`
Expected: No significant regression (escape analysis adds a cheap AST walk)

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
| `internal/validate/validated_forms.go` | Add `Escapes bool` to `ValidatedLetBinding`, update comment |
| `internal/validate/validated_forms_test.go` | Assert `Escapes` in getter test |
| `internal/validate/walk_sub_exprs.go` | New file: `WalkSubExprs` — shared structural traversal |
| `internal/validate/walk_sub_exprs_test.go` | New file: unit tests for `WalkSubExprs` |
| `internal/validate/validate_escape.go` | New file: `markEscapedBindings` + `escapeWalker` (uses `WalkSubExprs`) |
| `internal/validate/validate_escape_test.go` | New file: unit tests for escape analysis |
| `internal/validate/validate_let.go` | Add `markEscapedBindings` calls at 5 sites |
