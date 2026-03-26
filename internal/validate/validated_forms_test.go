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
	"context"
	"testing"

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func makeSyntaxFromValue(v values.Value) syntax.SyntaxValue {
	return schemeutil.DatumToSyntaxValue(context.Background(), syntax.NewZeroValueSourceContext(), v)
}

// ValidatedIf getters

func TestValidatedIf_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("if"),
		values.TrueValue,
		values.NewInteger(1),
		values.NewInteger(2),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vi, ok := result.Expr.(*ValidatedIf)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vi.FormName(), qt.Equals, "if")
	c.Assert(vi.Source(), qt.IsNotNil)

	vi.SetFormName("if-renamed")
	c.Assert(vi.FormName(), qt.Equals, "if-renamed")
}

// ValidatedDefine getters (variable form)

func TestValidatedDefine_Variable_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("define"),
		values.NewSymbol("x"),
		values.NewInteger(42),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vd, ok := result.Expr.(*ValidatedDefine)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vd.FormName(), qt.Equals, "define")
	c.Assert(vd.Source(), qt.IsNotNil)
	c.Assert(vd.Name(), qt.IsNotNil)
	c.Assert(vd.SubExp(), qt.IsNotNil)
	c.Assert(vd.IsFunction, qt.IsFalse)
	c.Assert(vd.Params(), qt.IsNil)
	c.Assert(vd.Body(), qt.IsNil)

	vd.SetFormName("define-renamed")
	c.Assert(vd.FormName(), qt.Equals, "define-renamed")
}

// ValidatedDefine getters (function form)

func TestValidatedDefine_Function_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("define"),
		values.List(values.NewSymbol("f"), values.NewSymbol("x")),
		values.NewSymbol("x"),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vd, ok := result.Expr.(*ValidatedDefine)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vd.IsFunction, qt.IsTrue)
	c.Assert(vd.Params(), qt.IsNotNil)
	c.Assert(len(vd.Params().Required), qt.Equals, 1)
	c.Assert(len(vd.Body()), qt.Equals, 1)
	c.Assert(vd.SubExp(), qt.IsNil)
}

// ValidatedLambda getters

func TestValidatedLambda_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("a"), values.NewSymbol("b")),
		values.NewSymbol("a"),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vl, ok := result.Expr.(*ValidatedLambda)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vl.FormName(), qt.Equals, "lambda")
	c.Assert(vl.Source(), qt.IsNotNil)
	c.Assert(vl.Params(), qt.IsNotNil)
	c.Assert(len(vl.Params().Required), qt.Equals, 2)
	c.Assert(vl.Params().Rest, qt.IsNil)
	c.Assert(len(vl.Body()), qt.Equals, 1)

	vl.SetFormName("lambda-renamed")
	c.Assert(vl.FormName(), qt.Equals, "lambda-renamed")
}

// ValidatedLambda with rest parameter

func TestValidatedLambda_RestParam(t *testing.T) {
	c := qt.New(t)
	// (lambda (a . rest) a)
	input := values.List(
		values.NewSymbol("lambda"),
		values.NewCons(values.NewSymbol("a"), values.NewSymbol("rest")),
		values.NewSymbol("a"),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vl, ok := result.Expr.(*ValidatedLambda)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(vl.Params().Required), qt.Equals, 1)
	c.Assert(vl.Params().Rest, qt.IsNotNil)
}

// ValidatedSetBang getters

func TestValidatedSetBang_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("set!"),
		values.NewSymbol("x"),
		values.NewInteger(99),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vs, ok := result.Expr.(*ValidatedSetBang)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vs.FormName(), qt.Equals, "set!")
	c.Assert(vs.Source(), qt.IsNotNil)
	c.Assert(vs.SubExp(), qt.IsNotNil)

	vs.SetFormName("set!-renamed")
	c.Assert(vs.FormName(), qt.Equals, "set!-renamed")
}

// ValidatedQuote getters

func TestValidatedQuote_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("quote"),
		values.NewSymbol("hello"),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vq, ok := result.Expr.(*ValidatedQuote)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vq.FormName(), qt.Equals, "quote")
	c.Assert(vq.Source(), qt.IsNotNil)
	c.Assert(vq.Datum, qt.IsNotNil)

	vq.SetFormName("quote-renamed")
	c.Assert(vq.FormName(), qt.Equals, "quote-renamed")
}

// ValidatedBegin getters

func TestValidatedBegin_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("begin"),
		values.NewInteger(1),
		values.NewInteger(2),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vb, ok := result.Expr.(*ValidatedBegin)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vb.FormName(), qt.Equals, "begin")
	c.Assert(vb.Source(), qt.IsNotNil)
	c.Assert(len(vb.Body()), qt.Equals, 2)

	vb.SetFormName("begin-renamed")
	c.Assert(vb.FormName(), qt.Equals, "begin-renamed")
}

// ValidatedCall getters

func TestValidatedCall_Getters(t *testing.T) {
	c := qt.New(t)
	// (foo 1 2) — a procedure call
	input := values.List(
		values.NewSymbol("foo"),
		values.NewInteger(1),
		values.NewInteger(2),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vc, ok := result.Expr.(*ValidatedCall)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vc.FormName(), qt.Equals, "@call")
	c.Assert(vc.Source(), qt.IsNotNil)
	c.Assert(vc.Proc(), qt.IsNotNil)
	c.Assert(len(vc.Body()), qt.Equals, 2) // args

	vc.SetFormName("call-renamed")
	c.Assert(vc.FormName(), qt.Equals, "call-renamed")
}

// ValidatedSymbol getters

func TestValidatedSymbol_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.NewSymbol("x")
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vs, ok := result.Expr.(*ValidatedSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vs.FormName(), qt.Equals, "@symbol")
	c.Assert(vs.Source(), qt.IsNotNil)
	c.Assert(vs.Symbol, qt.IsNotNil)

	vs.SetFormName("sym-renamed")
	c.Assert(vs.FormName(), qt.Equals, "sym-renamed")
}

// ValidatedLiteral getters

func TestValidatedLiteral_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.NewInteger(42)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vl, ok := result.Expr.(*ValidatedLiteral)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vl.FormName(), qt.Equals, "@literal")
	c.Assert(vl.Source(), qt.IsNotNil)
	c.Assert(vl.Value, qt.IsNotNil)

	vl.SetFormName("lit-renamed")
	c.Assert(vl.FormName(), qt.Equals, "lit-renamed")
}

// ValidatedQuasiquote getters

func TestValidatedQuasiquote_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("quasiquote"),
		values.List(values.NewSymbol("a"), values.NewInteger(1)),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vq, ok := result.Expr.(*ValidatedQuasiquote)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vq.FormName(), qt.Equals, "quasiquote")
	c.Assert(vq.Source(), qt.IsNotNil)
	c.Assert(vq.Template, qt.IsNotNil)

	vq.SetFormName("qq-renamed")
	c.Assert(vq.FormName(), qt.Equals, "qq-renamed")
}

// ValidatedCaseLambda getters

func TestValidatedCaseLambda_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("case-lambda"),
		values.List(
			values.List(values.NewSymbol("x")),
			values.NewSymbol("x"),
		),
		values.List(
			values.List(values.NewSymbol("x"), values.NewSymbol("y")),
			values.NewSymbol("x"),
		),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vcl, ok := result.Expr.(*ValidatedCaseLambda)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vcl.FormName(), qt.Equals, "case-lambda")
	c.Assert(vcl.Source(), qt.IsNotNil)
	c.Assert(len(vcl.Clauses()), qt.Equals, 2)

	vcl.SetFormName("cl-renamed")
	c.Assert(vcl.FormName(), qt.Equals, "cl-renamed")

	// Test clause getters
	clause := vcl.Clauses()[0]
	c.Assert(clause.FormName(), qt.Equals, "@clause")
	c.Assert(clause.Params(), qt.IsNotNil)
	c.Assert(len(clause.Params().Required), qt.Equals, 1)
	c.Assert(len(clause.Body()), qt.Equals, 1)

	clause.SetFormName("clause-renamed")
	c.Assert(clause.FormName(), qt.Equals, "clause-renamed")
}

// ValidatedDynamicWind getters

func TestValidatedDynamicWind_Getters(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("dynamic-wind"),
		values.NewSymbol("before"),
		values.NewSymbol("thunk"),
		values.NewSymbol("after"),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vdw, ok := result.Expr.(*ValidatedDynamicWind)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vdw.FormName(), qt.Equals, "dynamic-wind")
	c.Assert(vdw.Source(), qt.IsNotNil)
	c.Assert(vdw.Before, qt.IsNotNil)
	c.Assert(vdw.Thunk, qt.IsNotNil)
	c.Assert(vdw.After, qt.IsNotNil)

	vdw.SetFormName("dw-renamed")
	c.Assert(vdw.FormName(), qt.Equals, "dw-renamed")
}

// Binding form types

func TestValidatedLet_Getters(t *testing.T) {
	c := qt.New(t)
	sym := syntax.NewSyntaxSymbol("x", nil)
	init := &ValidatedLiteral{
		validatedBase: validatedBase{formName: "@literal"},
		Value:         syntax.NewSyntaxObject(values.NewInteger(1), nil),
	}
	body := &ValidatedSymbol{
		validatedBase: validatedBase{formName: "@symbol"},
		Symbol:        sym,
	}

	tcs := []struct {
		name string
		kind LetKind
		form string
	}{
		{"let", LetKindLet, "let"},
		{"let*", LetKindLetStar, "let*"},
		{"letrec", LetKindLetrec, "letrec"},
		{"letrec*", LetKindLetrecStar, "letrec*"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			vl := &ValidatedLet{
				validatedBase: validatedBase{formName: tc.form},
				Kind:          tc.kind,
				Bindings:      []ValidatedLetBinding{{Name: sym, Init: init}},
				body:          []ValidatedExpr{body},
			}
			c.Assert(vl.FormName(), qt.Equals, tc.form)
			c.Assert(vl.Kind, qt.Equals, tc.kind)
			c.Assert(len(vl.Bindings), qt.Equals, 1)
			c.Assert(vl.Bindings[0].Name, qt.Equals, sym)
			c.Assert(vl.Bindings[0].Mutable, qt.IsFalse)
			c.Assert(vl.Bindings[0].Captured, qt.IsFalse)
			c.Assert(len(vl.Body()), qt.Equals, 1)
		})
	}
}

func TestLetKind_Predicates(t *testing.T) {
	c := qt.New(t)
	c.Assert(LetKindLet.InitsInScope(), qt.IsFalse)
	c.Assert(LetKindLet.Sequential(), qt.IsFalse)

	c.Assert(LetKindLetStar.InitsInScope(), qt.IsFalse)
	c.Assert(LetKindLetStar.Sequential(), qt.IsTrue)

	c.Assert(LetKindLetrec.InitsInScope(), qt.IsTrue)
	c.Assert(LetKindLetrec.Sequential(), qt.IsFalse)

	c.Assert(LetKindLetrecStar.InitsInScope(), qt.IsTrue)
	c.Assert(LetKindLetrecStar.Sequential(), qt.IsTrue)
}

// Passthrough form (ValidatedLiteral for let-syntax)

func TestValidatedLiteral_Passthrough(t *testing.T) {
	c := qt.New(t)
	input := values.List(
		values.NewSymbol("let-syntax"),
		values.EmptyList,
		values.NewInteger(1),
	)
	result := ValidateExpression(context.TODO(), nil, makeSyntaxFromValue(input))
	c.Assert(result.Ok(), qt.IsTrue)

	vl, ok := result.Expr.(*ValidatedLiteral)
	c.Assert(ok, qt.IsTrue)
	c.Assert(vl.FormName(), qt.Equals, "let-syntax")
}
