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

	"github.com/aalpar/wile/pkg/syntax"
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
			formName: "if",
			Test:     lit(),
			Conseq:   call(symRef("f")),
			Alt:      call(symRef("f")),
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
			formName: "if",
			Test:     lit(),
			Conseq:   symRef("f"),
			Alt:      call(symRef("f")),
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
			formName: "if",
			Test:     call(symRef("="), symRef("x"), lit()),
			Conseq:   symRef("x"),
			Alt:      call(symRef("loop"), call(symRef("-"), symRef("x"), lit())),
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
			formName: "begin",
			body:     []ValidatedExpr{call(symRef("f")), call(symRef("f"))},
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
			formName: "dynamic-wind",
			Before:   symRef("f"),
			Thunk:    symRef("f"),
			After:    symRef("f"),
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
			formName: "with-continuation-mark",
			Key:      lit(),
			Val:      lit(),
			Body:     call(symRef("f")),
		},
	}
	markEscapedBindings(env, bindings, body, false)
	c.Assert(bindings[0].Escapes, qt.IsFalse)
}
