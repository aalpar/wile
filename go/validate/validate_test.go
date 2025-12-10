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
			wantType: &ValidatedIf{},
		},
		{
			name:     "valid 2-arg if",
			input:    values.List(values.NewSymbol("if"), values.TrueValue, values.NewInteger(1)),
			wantOk:   true,
			wantType: &ValidatedIf{},
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
			result := ValidateExpression(makeSyntax(tt.input))
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
			result := ValidateExpression(makeSyntax(tt.input))
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
			result := ValidateExpression(makeSyntax(tt.input))
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
			result := ValidateExpression(makeSyntax(tt.input))
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
			result := ValidateExpression(makeSyntax(tt.input))
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
			result := ValidateExpression(makeSyntax(tt.input))
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
			result := ValidateExpression(makeSyntax(tt.input))
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
	result := ValidateExpression(makeSyntax(values.NewSymbol("foo")))
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
			result := ValidateExpression(makeSyntax(tt.input))
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

	result := ValidateExpression(makeSyntax(input))
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
