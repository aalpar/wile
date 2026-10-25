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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// binderTestEnv is a frame in which the core forms the walk dispatches on denote
// themselves, as they do in a real namespace, with an empty scope set.
func binderTestEnv() *environment.EnvironmentFrame {
	env := environment.NewEnvironmentFrameWithParent(
		environment.NewLocalEnvironment(0), environment.NewNamespace().Runtime())
	for _, name := range []string{"quote", "quasiquote", "unquote", "lambda", "case-lambda", "let", "let*", "letrec", "letrec*", "begin"} {
		sym := values.NewSymbol(name)
		env.MaybeCreateLocalBinding(sym, environment.BindingTypeSyntax, syntax.Scopes{}, nil)
		env.GetBinding(sym, syntax.ScopesOf(syntax.Scopes{})).SetValue(environment.NewFormKeyword(name))
	}
	return env
}

// stxList builds a syntax list from already-built elements, so a row can carry a
// symbol whose scope set differs from its neighbours'.
func stxList(elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	var q syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i := len(elems) - 1; i >= 0; i-- {
		q = syntax.NewSyntaxCons(elems[i], q, syntax.NewZeroValueSourceContext())
	}
	return q
}

// TestRawWalkResolvesHeadsInBinderFrames is the precision ratchet on
// forEachRawBinderForm. Every row asks one question of the walkWrites scan: is
// x, which sits under a quote head, reported? It must be when the quote is a
// binder of the enclosing form, and must NOT be when the frame would have to
// leak past the binder's own region, because a reported name loses its Stable
// stamp (markOpaqueSubtree). A walk that demoted every quote passes every value
// row in pkg/wile; the false rows here are what fail it.
func TestRawWalkResolvesHeadsInBinderFrames(t *testing.T) {
	L, S := values.List, values.NewSymbol
	quoteX := L(S("quote"), S("x"))
	scoped := syntax.NewScope()

	tcs := []struct {
		name  string
		form  syntax.SyntaxValue
		quasi int
		want  bool
	}{
		{"lambda parameter shadows the body's quote",
			makeSyntax(L(L(S("lambda"), L(S("quote")), quoteX), S("list"))), quasiDepthCode, true},
		{"lambda rest parameter",
			makeSyntax(L(L(S("lambda"), S("quote"), quoteX))), quasiDepthCode, true},
		{"lambda dotted rest parameter",
			makeSyntax(L(S("lambda"), values.NewCons(S("a"), S("quote")), quoteX)), quasiDepthCode, true},
		{"case-lambda clause parameter",
			makeSyntax(L(S("case-lambda"), L(L(S("quote")), quoteX))), quasiDepthCode, true},
		{"let name covers the body",
			makeSyntax(L(S("let"), L(L(S("quote"), S("list"))), quoteX)), quasiDepthCode, true},
		{"let* init sees the names before it",
			makeSyntax(L(S("let*"), L(L(S("quote"), S("list")), L(S("y"), quoteX)), S("y"))), quasiDepthCode, true},
		{"letrec init sees every name",
			makeSyntax(L(S("letrec"), L(L(S("y"), quoteX), L(S("quote"), S("list"))), S("y"))), quasiDepthCode, true},
		{"letrec* init sees every name",
			makeSyntax(L(S("letrec*"), L(L(S("y"), quoteX), L(S("quote"), S("list"))), S("y"))), quasiDepthCode, true},
		{"named let name covers the body",
			makeSyntax(L(S("let"), S("lp"), L(L(S("quote"), S("list"))), quoteX)), quasiDepthCode, true},
		{"named let tag covers the body",
			makeSyntax(L(S("let"), S("quote"), L(L(S("y"), values.NewInteger(1))), quoteX)), quasiDepthCode, true},
		{"internal define covers the body",
			makeSyntax(L(S("lambda"), L(), L(S("define"), S("quote"), S("list")), quoteX)), quasiDepthCode, true},
		{"internal procedure define covers the body",
			makeSyntax(L(S("lambda"), L(), L(S("define"), L(S("quote"), S("v")), S("v")), quoteX)), quasiDepthCode, true},
		{"internal define spliced through a begin",
			makeSyntax(L(S("let"), L(), L(S("begin"), L(S("define"), S("quote"), S("list"))), quoteX)), quasiDepthCode, true},
		{"binder form inside an unquote",
			makeSyntax(L(S("a"), L(S("unquote"), L(L(S("lambda"), L(S("quote")), quoteX), S("list"))))), quasiDepthTemplate, true},

		{"control: a bare quote is a barrier",
			makeSyntax(quoteX), quasiDepthCode, false},
		{"a lambda's frame does not reach its sibling",
			makeSyntax(L(S("begin"), L(L(S("lambda"), L(S("quote")), S("quote")), values.NewInteger(1)), quoteX)), quasiDepthCode, false},
		{"an internal define does not reach past its body",
			makeSyntax(L(S("begin"), L(L(S("lambda"), L(), L(S("define"), S("quote"), S("list")), values.NewInteger(1))), quoteX)), quasiDepthCode, false},
		{"a let's names do not reach its inits",
			makeSyntax(L(S("let"), L(L(S("quote"), S("list")), L(S("y"), quoteX)), S("y"))), quasiDepthCode, false},
		{"a let* name does not reach an earlier init",
			makeSyntax(L(S("let*"), L(L(S("y"), quoteX), L(S("quote"), S("list"))), S("y"))), quasiDepthCode, false},
		{"a let* name does not reach its own init",
			makeSyntax(L(S("let*"), L(L(S("quote"), quoteX)), S("quote"))), quasiDepthCode, false},
		{"a named let's tag does not reach its inits",
			makeSyntax(L(S("let"), S("quote"), L(L(S("y"), quoteX)), S("y"))), quasiDepthCode, false},
		{"a binder form inside template data opens no frame",
			makeSyntax(L(S("a"), L(L(S("lambda"), L(S("quote")), L(S("unquote"), quoteX)), S("list")))), quasiDepthTemplate, false},
		// Identity is by scopes: a binder carrying a scope the reference lacks does
		// not cover it, so the reference still reaches the quote form.
		{"a binder the reference's scopes do not cover does not shadow",
			stxList(stxList(syntax.NewSyntaxSymbol("lambda", nil),
				stxList(withScope(syntax.NewSyntaxSymbol("quote", nil), scoped)),
				makeSyntax(quoteX)),
				syntax.NewSyntaxSymbol("list", nil)), quasiDepthCode, false},
		// A malformed form falls back to the ordinary walk rather than raising.
		{"malformed parameter list falls back",
			makeSyntax(L(L(S("lambda"), L(values.NewInteger(1)), quoteX), S("list"))), quasiDepthCode, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := false
			forEachRawSymbol(binderTestEnv(), tc.form, tc.quasi, quasiquoteKW, walkWrites, func(sym *syntax.SyntaxSymbol) {
				if sym.Key() == "x" {
					got = true
				}
			})
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestRawWalkBinderFormDispatchesOnDenotation: a local variable spelled lambda
// is an ordinary operator, so its "parameter list" binds nothing and the quote
// beneath it stays the barrier.
func TestRawWalkBinderFormDispatchesOnDenotation(t *testing.T) {
	L, S := values.List, values.NewSymbol
	env := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), binderTestEnv())
	bindLocalSymbol(env, syntax.NewSyntaxSymbol("lambda", nil))
	form := makeSyntax(L(S("lambda"), L(S("quote")), L(S("quote"), S("x"))))
	got := false
	forEachRawSymbol(env, form, quasiDepthCode, quasiquoteKW, walkWrites, func(sym *syntax.SyntaxSymbol) {
		if sym.Key() == "x" {
			got = true
		}
	})
	qt.Assert(t, got, qt.IsFalse)
}

// TestRawWalkDottedQuasiquoteTail pins dottedTailMarker's nesting arm: `(a . `x)
// reads as (a quasiquote x), and the compiler (dottedTailCell) takes x one level
// deeper. One unquote under it stays data; two reach code.
func TestRawWalkDottedQuasiquoteTail(t *testing.T) {
	L, S := values.List, values.NewSymbol
	tail := func(inner values.Value) syntax.SyntaxValue {
		return makeSyntax(values.NewCons(S("a"), L(S("quasiquote"), L(S("b"), inner))))
	}
	tcs := []struct {
		name string
		form syntax.SyntaxValue
		want bool
	}{
		{"one unquote is data", tail(L(S("unquote"), S("x"))), false},
		{"two unquotes reach code", tail(L(S("unquote"), L(S("unquote"), S("x")))), true},
		{"a quote at depth 1 is no barrier", tail(L(S("unquote"), L(S("quote"), L(L(S("unquote"), S("x")))))), true},
		{"control: dotted unquote reaches code", makeSyntax(values.NewCons(S("a"), L(S("unquote"), S("x")))), true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := false
			forEachRawSymbol(binderTestEnv(), tc.form, quasiDepthTemplate, quasiquoteKW, walkWrites, func(sym *syntax.SyntaxSymbol) {
				if sym.Key() == "x" {
					got = true
				}
			})
			qt.Assert(t, got, qt.Equals, tc.want)
		})
	}
}

// TestRawWalkAmbiguousHeadDemotesAtCodeDepth: an incomparable tie on a head at
// code depth reads as a call, the reading that reports more. A tie among the
// frames forEachRawBinderForm mints is not one the compiler sees, so reading it
// as the quote barrier could hide a live set!.
func TestRawWalkAmbiguousHeadDemotesAtCodeDepth(t *testing.T) {
	a, b, c := syntax.NewScope(), syntax.NewScope(), syntax.NewScope()
	env := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), binderTestEnv())
	quote := syntax.NewSyntaxSymbol("quote", nil)
	bindLocalSymbol(env, withScope(withScope(quote, a), b))
	bindLocalSymbol(env, withScope(withScope(quote, a), c))
	form := stxList(withScope(withScope(withScope(quote, a), b), c), syntax.NewSyntaxSymbol("x", nil))

	got := false
	forEachRawSymbol(env, form, quasiDepthCode, quasiquoteKW, walkWrites, func(sym *syntax.SyntaxSymbol) {
		if sym.Key() == "x" {
			got = true
		}
	})
	qt.Assert(t, got, qt.IsTrue)
}
