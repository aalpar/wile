// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// makeTestEnvAndBindings creates an EnvironmentFrame with local bindings for
// the given names and returns the bindings slice with corresponding entries.
func makeTestEnvAndBindings(names ...string) (
	*environment.EnvironmentFrame,
	[]ValidatedLetBinding,
) {
	parent := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, parent)
	var bindings []ValidatedLetBinding
	for _, name := range names {
		ssym := syntax.NewSyntaxSymbol(name, nil)
		env.MaybeCreateLocalBindingWithScopes(
			ssym.Sym,
			environment.BindingTypeVariable,
			nil,
			nil,
		)
		bindings = append(bindings, ValidatedLetBinding{
			Name: ssym,
			Init: lit(),
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
func applyExpr(
	proc ValidatedExpr,
	prefixArgs []ValidatedExpr,
	finalList ValidatedExpr,
) *ValidatedApply {
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

// setBang creates a ValidatedSetBang.
func setBang(name string, expr ValidatedExpr) *ValidatedSetBang {
	return &ValidatedSetBang{
		validatedBase: validatedBase{formName: "set!"},
		Name:          syntax.NewSyntaxSymbol(name, nil),
		subExp:        expr,
	}
}

func TestMarkCapturedBindings_SetBangInsideLambda(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (lambda () (set! x 2))) — set! target captured
	body := []ValidatedExpr{lam(setBang("x", lit()))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
}

func TestMarkCapturedBindings_SetBangOutsideLambda(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x")
	// (let ((x 1)) (set! x 2)) — set! at depth 0, not captured
	body := []ValidatedExpr{setBang("x", lit())}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsFalse)
}

func TestMarkCapturedBindings_SetBangValueExprCaptures(t *testing.T) {
	c := qt.New(t)
	env, bindings := makeTestEnvAndBindings("x", "y")
	// (let ((x 1) (y 2)) (lambda () (set! x y))) — both captured
	body := []ValidatedExpr{lam(setBang("x", symRef("y")))}
	markCapturedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Captured, qt.IsTrue)
	c.Assert(bindings[1].Captured, qt.IsTrue)
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
