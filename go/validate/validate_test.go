// Copyright 2025 Aaron Alpar
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

	"wile/syntax"
	"wile/utils"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func makeSyntax(v values.Value) syntax.SyntaxValue {
	return utils.DatumToSyntaxValue(syntax.NewZeroValueSourceContext(), v)
}

// TestValidateIf tests the if form validator
func TestValidateIf(t *testing.T) {
	tests := []struct {
		name     string
		input    values.Value
		wantOk   bool
		wantType interface{}
	}{
		{
			name:     "valid 3-arg if",
			input:    values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1), values.NewInteger(2)),
			wantOk:   true,
			wantType: &ValidatedIf{formName: "if"},
		},
		{
			name:     "valid 2-arg if",
			input:    values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1)),
			wantOk:   true,
			wantType: &ValidatedIf{formName: "if"},
		},
		{
			name:   "too few args",
			input:  values.List(values.NewSymbol("if"), values.TrueValue),
			wantOk: false,
		},
		{
			name:   "too many args",
			input:  values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				c.Assert(result.Expr, qt.IsNotNil)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
				c.Assert(len(result.Errors), qt.Not(qt.Equals), 0)
			}
		})
	}
}

// TestValidateDefine tests the define form validator
func TestValidateDefine(t *testing.T) {
	tests := []struct {
		name       string
		input      values.Value
		wantOk     bool
		isFunction bool
	}{
		{
			name:       "simple variable",
			input:      values.List(values.NewSymbol("define"), values.NewSymbol("x"), values.NewInteger(42)),
			wantOk:     true,
			isFunction: false,
		},
		{
			name:       "function form",
			input:      values.List(values.NewSymbol("define"), values.List(values.NewSymbol("f"), values.NewSymbol("x")), values.NewSymbol("x")),
			wantOk:     true,
			isFunction: true,
		},
		{
			name:       "function with multiple params",
			input:      values.List(values.NewSymbol("define"), values.List(values.NewSymbol("add"), values.NewSymbol("a"), values.NewSymbol("b")), values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b"))),
			wantOk:     true,
			isFunction: true,
		},
		{
			name:   "missing value",
			input:  values.List(values.NewSymbol("define"), values.NewSymbol("x")),
			wantOk: false,
		},
		{
			name:   "too many values for variable",
			input:  values.List(values.NewSymbol("define"), values.NewSymbol("x"), values.NewInteger(1), values.NewInteger(2)),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				def, ok := result.Expr.(*ValidatedDefine)
				c.Assert(ok, qt.IsTrue)
				c.Assert(def.IsFunction, qt.Equals, tt.isFunction)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateLambda tests the lambda form validator
func TestValidateLambda(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name:   "single param",
			input:  values.List(values.NewSymbol("lambda"), values.NewSymbol("x"), values.NewSymbol("x")),
			wantOk: true,
		},
		{
			name:   "no params",
			input:  values.List(values.NewSymbol("lambda"), values.EmptyList, values.NewInteger(42)),
			wantOk: true,
		},
		{
			name:   "multiple params",
			input:  values.List(values.NewSymbol("lambda"), values.List(values.NewSymbol("a"), values.NewSymbol("b")), values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b"))),
			wantOk: true,
		},
		{
			name:   "missing body",
			input:  values.List(values.NewSymbol("lambda"), values.List(values.NewSymbol("x"))),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLambda)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateSetBang tests the set! form validator
func TestValidateSetBang(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name:   "valid set!",
			input:  values.List(values.NewSymbol("set!"), values.NewSymbol("x"), values.NewInteger(42)),
			wantOk: true,
		},
		{
			name:   "missing value",
			input:  values.List(values.NewSymbol("set!"), values.NewSymbol("x")),
			wantOk: false,
		},
		{
			name:   "non-symbol target",
			input:  values.List(values.NewSymbol("set!"), values.NewInteger(1), values.NewInteger(42)),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedSetBang)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateQuote tests the quote form validator
func TestValidateQuote(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name:   "quote symbol",
			input:  values.List(values.NewSymbol("quote"), values.NewSymbol("x")),
			wantOk: true,
		},
		{
			name:   "quote list",
			input:  values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2))),
			wantOk: true,
		},
		{
			name:   "missing datum",
			input:  values.List(values.NewSymbol("quote")),
			wantOk: false,
		},
		{
			name:   "too many args",
			input:  values.List(values.NewSymbol("quote"), values.NewSymbol("x"), values.NewSymbol("y")),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedQuote)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateBegin tests the begin form validator
func TestValidateBegin(t *testing.T) {
	tests := []struct {
		name      string
		input     values.Value
		wantOk    bool
		exprCount int
	}{
		{
			name:      "single expr",
			input:     values.List(values.NewSymbol("begin"), values.NewInteger(42)),
			wantOk:    true,
			exprCount: 1,
		},
		{
			name:      "multiple exprs",
			input:     values.List(values.NewSymbol("begin"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			wantOk:    true,
			exprCount: 3,
		},
		{
			name:      "empty begin",
			input:     values.List(values.NewSymbol("begin")),
			wantOk:    true,
			exprCount: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				b, ok := result.Expr.(*ValidatedBegin)
				c.Assert(ok, qt.IsTrue)
				c.Assert(len(b.Exprs), qt.Equals, tt.exprCount)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateCall tests function call validation
func TestValidateCall(t *testing.T) {
	tests := []struct {
		name     string
		input    values.Value
		wantOk   bool
		argCount int
	}{
		{
			name:     "simple call",
			input:    values.List(values.NewSymbol("f"), values.NewInteger(1)),
			wantOk:   true,
			argCount: 1,
		},
		{
			name:     "no args",
			input:    values.List(values.NewSymbol("f")),
			wantOk:   true,
			argCount: 0,
		},
		{
			name:     "multiple args",
			input:    values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			wantOk:   true,
			argCount: 3,
		},
		{
			name:     "nested call",
			input:    values.List(values.NewSymbol("+"), values.List(values.NewSymbol("*"), values.NewInteger(2), values.NewInteger(3)), values.NewInteger(4)),
			wantOk:   true,
			argCount: 2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				call, ok := result.Expr.(*ValidatedCall)
				c.Assert(ok, qt.IsTrue)
				c.Assert(len(call.Args), qt.Equals, tt.argCount)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateSymbol tests symbol validation
func TestValidateSymbol(t *testing.T) {
	c := qt.New(t)
	result := ValidateExpression(nil, makeSyntax(values.NewSymbol("foo")))
	c.Assert(result.Ok(), qt.IsTrue)
	sym, ok := result.Expr.(*ValidatedSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(sym.Symbol, qt.IsNotNil)
}

// TestValidateLiteral tests literal validation
func TestValidateLiteral(t *testing.T) {
	tests := []struct {
		name  string
		input values.Value
	}{
		{"integer", values.NewInteger(42)},
		{"float", values.NewFloat(3.14)},
		{"string", values.NewString("hello")},
		{"boolean true", values.TrueValue},
		{"boolean false", values.FalseValue},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			c.Assert(result.Ok(), qt.IsTrue)
			_, ok := result.Expr.(*ValidatedLiteral)
			c.Assert(ok, qt.IsTrue)
		})
	}
}

// TestValidateNestedExpressions tests validation of nested expressions
func TestValidateNestedExpressions(t *testing.T) {
	c := qt.New(t)

	// (if (> x 0) (+ x 1) (- x 1))
	input := values.List(
		values.NewSymbol("if"),
		values.List(values.NewSymbol(">"), values.NewSymbol("x"), values.NewInteger(0)),
		values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewInteger(1)),
		values.List(values.NewSymbol("-"), values.NewSymbol("x"), values.NewInteger(1)),
	)

	result := ValidateExpression(nil, makeSyntax(input))
	c.Assert(result.Ok(), qt.IsTrue)

	ifExpr, ok := result.Expr.(*ValidatedIf)
	c.Assert(ok, qt.IsTrue)

	// Test is a call
	_, ok = ifExpr.Test.(*ValidatedCall)
	c.Assert(ok, qt.IsTrue)

	// Conseq is a call
	_, ok = ifExpr.Conseq.(*ValidatedCall)
	c.Assert(ok, qt.IsTrue)

	// Alt is a call
	_, ok = ifExpr.Alt.(*ValidatedCall)
	c.Assert(ok, qt.IsTrue)
}

// TestValidateCaseLambda tests the case-lambda form validator
func TestValidateCaseLambda(t *testing.T) {
	tests := []struct {
		name        string
		input       values.Value
		wantOk      bool
		clauseCount int
	}{
		{
			name: "single clause",
			input: values.List(
				values.NewSymbol("case-lambda"),
				values.List(values.List(values.NewSymbol("x")), values.NewSymbol("x")),
			),
			wantOk:      true,
			clauseCount: 1,
		},
		{
			name: "multiple clauses",
			input: values.List(
				values.NewSymbol("case-lambda"),
				values.List(values.EmptyList, values.NewInteger(0)),
				values.List(values.List(values.NewSymbol("x")), values.NewSymbol("x")),
				values.List(values.List(values.NewSymbol("x"), values.NewSymbol("y")), values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewSymbol("y"))),
			),
			wantOk:      true,
			clauseCount: 3,
		},
		{
			name: "clause with rest params",
			input: values.List(
				values.NewSymbol("case-lambda"),
				values.List(values.NewSymbol("args"), values.NewSymbol("args")),
			),
			wantOk:      true,
			clauseCount: 1,
		},
		{
			name:   "no clauses",
			input:  values.List(values.NewSymbol("case-lambda")),
			wantOk: false,
		},
		{
			name: "clause missing body",
			input: values.List(
				values.NewSymbol("case-lambda"),
				values.List(values.List(values.NewSymbol("x"))),
			),
			wantOk: false,
		},
		{
			name: "clause not a list",
			input: values.List(
				values.NewSymbol("case-lambda"),
				values.NewInteger(42),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				caseLambda, ok := result.Expr.(*ValidatedCaseLambda)
				c.Assert(ok, qt.IsTrue)
				c.Assert(len(caseLambda.Clauses), qt.Equals, tt.clauseCount)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateQuasiquote tests the quasiquote form validator
func TestValidateQuasiquote(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name:   "simple quasiquote",
			input:  values.List(values.NewSymbol("quasiquote"), values.NewSymbol("x")),
			wantOk: true,
		},
		{
			name: "quasiquote list",
			input: values.List(
				values.NewSymbol("quasiquote"),
				values.List(values.NewInteger(1), values.NewInteger(2)),
			),
			wantOk: true,
		},
		{
			name:   "missing template",
			input:  values.List(values.NewSymbol("quasiquote")),
			wantOk: false,
		},
		{
			name: "too many args",
			input: values.List(
				values.NewSymbol("quasiquote"),
				values.NewSymbol("x"),
				values.NewSymbol("y"),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedQuasiquote)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateDefineSyntax tests the define-syntax form validator
func TestValidateDefineSyntax(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid define-syntax",
			input: values.List(
				values.NewSymbol("define-syntax"),
				values.NewSymbol("my-macro"),
				values.List(values.NewSymbol("syntax-rules"), values.EmptyList),
			),
			wantOk: true,
		},
		{
			name:   "missing transformer",
			input:  values.List(values.NewSymbol("define-syntax"), values.NewSymbol("my-macro")),
			wantOk: false,
		},
		{
			name: "too many args",
			input: values.List(
				values.NewSymbol("define-syntax"),
				values.NewSymbol("my-macro"),
				values.NewSymbol("transformer"),
				values.NewSymbol("extra"),
			),
			wantOk: false,
		},
		{
			name: "keyword not a symbol",
			input: values.List(
				values.NewSymbol("define-syntax"),
				values.NewInteger(42),
				values.NewSymbol("transformer"),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateSyntaxRules tests the syntax-rules form validator
func TestValidateSyntaxRules(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid syntax-rules with empty literals",
			input: values.List(
				values.NewSymbol("syntax-rules"),
				values.EmptyList,
				values.List(
					values.List(values.NewSymbol("_"), values.NewSymbol("x")),
					values.NewSymbol("x"),
				),
			),
			wantOk: true,
		},
		{
			name: "valid syntax-rules with literals",
			input: values.List(
				values.NewSymbol("syntax-rules"),
				values.List(values.NewSymbol("else"), values.NewSymbol("=>")),
				values.List(
					values.List(values.NewSymbol("_"), values.NewSymbol("x")),
					values.NewSymbol("x"),
				),
			),
			wantOk: true,
		},
		{
			name:   "missing literals list",
			input:  values.List(values.NewSymbol("syntax-rules")),
			wantOk: false,
		},
		{
			name: "non-symbol literal",
			input: values.List(
				values.NewSymbol("syntax-rules"),
				values.List(values.NewInteger(42)),
			),
			wantOk: false,
		},
		{
			name: "clause with wrong element count",
			input: values.List(
				values.NewSymbol("syntax-rules"),
				values.EmptyList,
				values.List(values.List(values.NewSymbol("_"))),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateImport tests the import form validator
func TestValidateImport(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid import with library name",
			input: values.List(
				values.NewSymbol("import"),
				values.List(values.NewSymbol("scheme"), values.NewSymbol("base")),
			),
			wantOk: true,
		},
		{
			name: "import with symbol",
			input: values.List(
				values.NewSymbol("import"),
				values.NewSymbol("library"),
			),
			wantOk: true,
		},
		{
			name:   "import with no sets",
			input:  values.List(values.NewSymbol("import")),
			wantOk: false,
		},
		{
			name: "import with non-list non-symbol",
			input: values.List(
				values.NewSymbol("import"),
				values.NewInteger(42),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateExport tests the export form validator
func TestValidateExport(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid export with symbols",
			input: values.List(
				values.NewSymbol("export"),
				values.NewSymbol("foo"),
				values.NewSymbol("bar"),
			),
			wantOk: true,
		},
		{
			name: "valid export with rename",
			input: values.List(
				values.NewSymbol("export"),
				values.List(values.NewSymbol("rename"), values.NewSymbol("internal"), values.NewSymbol("external")),
			),
			wantOk: true,
		},
		{
			name:   "empty export",
			input:  values.List(values.NewSymbol("export")),
			wantOk: true,
		},
		{
			name: "export with invalid spec",
			input: values.List(
				values.NewSymbol("export"),
				values.NewInteger(42),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateDefineLibrary tests the define-library form validator
func TestValidateDefineLibrary(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid define-library",
			input: values.List(
				values.NewSymbol("define-library"),
				values.List(values.NewSymbol("my"), values.NewSymbol("library")),
				values.List(values.NewSymbol("export"), values.NewSymbol("foo")),
			),
			wantOk: true,
		},
		{
			name: "library name with version",
			input: values.List(
				values.NewSymbol("define-library"),
				values.List(values.NewSymbol("srfi"), values.NewInteger(1)),
			),
			wantOk: true,
		},
		{
			name:   "missing library name",
			input:  values.List(values.NewSymbol("define-library")),
			wantOk: false,
		},
		{
			name: "library name not a list",
			input: values.List(
				values.NewSymbol("define-library"),
				values.NewSymbol("not-a-list"),
			),
			wantOk: false,
		},
		{
			name: "library name with invalid component",
			input: values.List(
				values.NewSymbol("define-library"),
				values.List(values.NewSymbol("valid"), values.NewString("invalid")),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateInclude tests the include form validator
func TestValidateInclude(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid include",
			input: values.List(
				values.NewSymbol("include"),
				values.NewString("file.scm"),
			),
			wantOk: true,
		},
		{
			name: "multiple files",
			input: values.List(
				values.NewSymbol("include"),
				values.NewString("file1.scm"),
				values.NewString("file2.scm"),
			),
			wantOk: true,
		},
		{
			name:   "missing filename",
			input:  values.List(values.NewSymbol("include")),
			wantOk: false,
		},
		{
			name: "non-string filename",
			input: values.List(
				values.NewSymbol("include"),
				values.NewInteger(42),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateCondExpand tests the cond-expand form validator
func TestValidateCondExpand(t *testing.T) {
	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "valid cond-expand",
			input: values.List(
				values.NewSymbol("cond-expand"),
				values.List(values.NewSymbol("r7rs"), values.List(values.NewSymbol("import"), values.NewSymbol("scheme"))),
			),
			wantOk: true,
		},
		{
			name:   "empty cond-expand",
			input:  values.List(values.NewSymbol("cond-expand")),
			wantOk: true,
		},
		{
			name: "clause not a list",
			input: values.List(
				values.NewSymbol("cond-expand"),
				values.NewInteger(42),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
				_, ok := result.Expr.(*ValidatedLiteral)
				c.Assert(ok, qt.IsTrue)
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidationErrorFormatting tests error message formatting
func TestValidationErrorFormatting(t *testing.T) {
	c := qt.New(t)

	// Single error
	result := ValidateExpression(nil, makeSyntax(values.List(values.NewSymbol("if"))))
	c.Assert(result.Ok(), qt.IsFalse)
	c.Assert(len(result.Errors), qt.Equals, 1)
	errStr := result.Error()
	c.Assert(errStr, qt.Not(qt.Equals), "")

	// Multiple errors - case-lambda with multiple bad clauses
	badCaseLambda := values.List(
		values.NewSymbol("case-lambda"),
		values.NewInteger(1),
		values.NewInteger(2),
	)
	result2 := ValidateExpression(nil, makeSyntax(badCaseLambda))
	c.Assert(result2.Ok(), qt.IsFalse)
	c.Assert(len(result2.Errors) > 1, qt.IsTrue)
	errStr2 := result2.Error()
	c.Assert(errStr2, qt.Not(qt.Equals), "")
	// Should contain "validation errors" plural
	c.Assert(errStr2, qt.Contains, "validation errors")
}

// TestValidatedFormSources tests that all validated forms have Source() methods
func TestValidatedFormSources(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name  string
		input values.Value
	}{
		{"if", values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1))},
		{"define", values.List(values.NewSymbol("define"), values.NewSymbol("x"), values.NewInteger(1))},
		{"lambda", values.List(values.NewSymbol("lambda"), values.EmptyList, values.NewInteger(1))},
		{"set!", values.List(values.NewSymbol("set!"), values.NewSymbol("x"), values.NewInteger(1))},
		{"quote", values.List(values.NewSymbol("quote"), values.NewSymbol("x"))},
		{"begin", values.List(values.NewSymbol("begin"), values.NewInteger(1))},
		{"call", values.List(values.NewSymbol("f"), values.NewInteger(1))},
		{"symbol", values.NewSymbol("x")},
		{"literal", values.NewInteger(42)},
		{"quasiquote", values.List(values.NewSymbol("quasiquote"), values.NewSymbol("x"))},
		{
			"case-lambda",
			values.List(
				values.NewSymbol("case-lambda"),
				values.List(values.EmptyList, values.NewInteger(0)),
			),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ValidateExpression(nil, makeSyntax(tt.input))
			c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
			c.Assert(result.Expr, qt.IsNotNil)
			// Source() should not panic
			_ = result.Expr.Source()
		})
	}
}

// TestGetSourceContext tests getSourceContext helper
func TestGetSourceContext(t *testing.T) {
	c := qt.New(t)

	// Test with different syntax types
	pairSyntax := makeSyntax(values.List(values.NewSymbol("test")))
	_ = getSourceContext(pairSyntax)

	symSyntax := makeSyntax(values.NewSymbol("x"))
	_ = getSourceContext(symSyntax)

	objSyntax := makeSyntax(values.NewInteger(42))
	_ = getSourceContext(objSyntax)

	// Test with a non-standard syntax value (nil case)
	ctx := getSourceContext(nil)
	c.Assert(ctx, qt.IsNil)
}

// TestValidateSyntaxObject tests the validateSyntaxObject function
func TestValidateSyntaxObject(t *testing.T) {
	c := qt.New(t)

	// Test with a self-evaluating value wrapped in SyntaxObject
	syntaxObj := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	result := &ValidationResult{}
	validated := validateSyntaxObject(syntaxObj, result)
	c.Assert(result.Ok(), qt.IsTrue)
	c.Assert(validated, qt.IsNotNil)
	_, ok := validated.(*ValidatedLiteral)
	c.Assert(ok, qt.IsTrue)

	// Test with a string
	syntaxObj2 := syntax.NewSyntaxObject(values.NewString("hello"), nil)
	result2 := &ValidationResult{}
	validated2 := validateSyntaxObject(syntaxObj2, result2)
	c.Assert(result2.Ok(), qt.IsTrue)
	c.Assert(validated2, qt.IsNotNil)
}

// TestValidateExprEdgeCases tests edge cases in validateExpr
func TestValidateExprEdgeCases(t *testing.T) {
	c := qt.New(t)

	// Test with different literal types
	tests := []struct {
		name  string
		input values.Value
	}{
		{"boolean", values.TrueValue},
		{"string", values.NewString("test")},
		{"float", values.NewFloat(3.14)},
		{"character", values.NewCharacter('a')},
		{"vector", values.NewVector(values.NewInteger(1), values.NewInteger(2))},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ValidateExpression(nil, makeSyntax(tt.input))
			c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
			_, ok := result.Expr.(*ValidatedLiteral)
			c.Assert(ok, qt.IsTrue)
		})
	}
}

// TestValidateMetaForm tests that meta forms pass through as literals
func TestValidateMetaForm(t *testing.T) {
	c := qt.New(t)

	metaForm := values.List(
		values.NewSymbol("meta"),
		values.NewSymbol("some-metadata"),
	)
	result := ValidateExpression(nil, makeSyntax(metaForm))
	c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
	_, ok := result.Expr.(*ValidatedLiteral)
	c.Assert(ok, qt.IsTrue)
}

// TestValidationErrorWithSource tests error formatting with source location
func TestValidationErrorWithSource(t *testing.T) {
	c := qt.New(t)

	// Create an error with source context
	source := syntax.NewSourceContext("(if)", "test.scm",
		syntax.NewSourceIndexes(0, 5, 1),
		syntax.NewSourceIndexes(4, 10, 1))
	err := ValidationError{
		Source:  source,
		Message: "invalid syntax",
		Form:    "if",
	}
	errStr := err.Error()
	c.Assert(errStr, qt.Contains, "test.scm")
	c.Assert(errStr, qt.Contains, "invalid syntax")
	c.Assert(errStr, qt.Contains, "if")

	// Test error without source
	err2 := ValidationError{
		Source:  nil,
		Message: "bad form",
		Form:    "lambda",
	}
	errStr2 := err2.Error()
	c.Assert(errStr2, qt.Contains, "bad form")
	c.Assert(errStr2, qt.Contains, "lambda")
}

// TestValidateParamsEdgeCases tests edge cases in parameter validation
func TestValidateParamsEdgeCases(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name   string
		input  values.Value
		wantOk bool
	}{
		{
			name: "duplicate parameters",
			input: values.List(
				values.NewSymbol("lambda"),
				values.List(values.NewSymbol("x"), values.NewSymbol("x")),
				values.NewInteger(1),
			),
			wantOk: false,
		},
		{
			name: "improper list with rest param",
			input: values.List(
				values.NewSymbol("lambda"),
				values.NewCons(values.NewSymbol("x"), values.NewSymbol("rest")),
				values.NewInteger(1),
			),
			wantOk: true,
		},
		{
			name: "duplicate in rest param",
			input: values.List(
				values.NewSymbol("lambda"),
				values.NewCons(
					values.NewSymbol("x"),
					values.NewSymbol("x"),
				),
				values.NewInteger(1),
			),
			wantOk: false,
		},
		{
			name: "non-symbol parameter",
			input: values.List(
				values.NewSymbol("lambda"),
				values.List(values.NewInteger(42)),
				values.NewInteger(1),
			),
			wantOk: false,
		},
		{
			name: "non-symbol rest parameter",
			input: values.List(
				values.NewSymbol("lambda"),
				values.NewCons(values.NewSymbol("x"), values.NewInteger(42)),
				values.NewInteger(1),
			),
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := ValidateExpression(nil, makeSyntax(tt.input))
			if tt.wantOk {
				c.Assert(result.Ok(), qt.IsTrue, qt.Commentf("errors: %v", result.Errors))
			} else {
				c.Assert(result.Ok(), qt.IsFalse)
			}
		})
	}
}

// TestValidateSyntaxRulesEdgeCases tests additional syntax-rules edge cases
func TestValidateSyntaxRulesEdgeCases(t *testing.T) {
	c := qt.New(t)

	// Test with literals not being a list
	result := ValidateExpression(nil, makeSyntax(
		values.List(
			values.NewSymbol("syntax-rules"),
			values.NewInteger(42),
		),
	))
	c.Assert(result.Ok(), qt.IsFalse)

	// Test with improper literals list
	result2 := ValidateExpression(nil, makeSyntax(
		values.List(
			values.NewSymbol("syntax-rules"),
			values.NewCons(values.NewSymbol("else"), values.NewInteger(42)),
		),
	))
	c.Assert(result2.Ok(), qt.IsFalse)

	// Test with clause not being a pair
	result3 := ValidateExpression(nil, makeSyntax(
		values.List(
			values.NewSymbol("syntax-rules"),
			values.EmptyList,
			values.NewInteger(42),
		),
	))
	c.Assert(result3.Ok(), qt.IsFalse)

	// Test with improper clause list
	result4 := ValidateExpression(nil, makeSyntax(
		values.List(
			values.NewSymbol("syntax-rules"),
			values.EmptyList,
			values.NewCons(values.NewSymbol("pattern"), values.NewInteger(42)),
		),
	))
	c.Assert(result4.Ok(), qt.IsFalse)
}
