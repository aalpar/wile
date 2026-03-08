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

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newExpanderEnv creates a test environment with expander support.
func newExpanderEnv() (*environment.EnvironmentFrame, *ExpanderTimeContinuation) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	expander := NewExpanderTimeContinuation(context.Background(), env)
	return env, expander
}

// --- Primitive expander: expandUnchanged ---

// TestExpandUnchanged_ReturnsFormUnchanged verifies that expandUnchanged
// returns the form as (sym . expr) without any transformation.
func TestExpandUnchanged_ReturnsFormUnchanged(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	// Test with various form names that use expandUnchanged
	testCases := []string{"quote", "quasiquote", "unsyntax", "unsyntax-splicing", "with-syntax"}
	for _, formName := range testCases {
		sym := syntax.NewSyntaxSymbol(formName, sctx)
		body := syntax.SyntaxList(sctx, syntax.NewSyntaxObject(values.NewInteger(1), sctx))

		result, err := expander.expandUnchanged(sym, body)
		c.Assert(err, qt.IsNil, qt.Commentf("form: %s", formName))
		c.Assert(result, qt.IsNotNil, qt.Commentf("form: %s", formName))
		// Result is (formName . body) — returned unchanged
		pair, ok := result.(*syntax.SyntaxPair)
		c.Assert(ok, qt.IsTrue, qt.Commentf("form: %s", formName))
		c.Assert(pair.SyntaxCar().(*syntax.SyntaxSymbol).Sym.Key, qt.Equals, formName)
	}
}

// --- ExpandPrimitiveForm ---

// TestExpandPrimitiveForm_KnownForm verifies that ExpandPrimitiveForm dispatches
// to the registered primitive expander for known forms.
func TestExpandPrimitiveForm_KnownForm(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	// "quote" has a registered primitive expander that returns unchanged
	sym := syntax.NewSyntaxSymbolForSymbol(values.NewSymbol("quote"), sctx)
	body := syntax.SyntaxList(sctx, syntax.NewSyntaxObject(values.NewInteger(42), sctx))

	result, err := expander.ExpandPrimitiveForm("quote", sym, body)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
}

// TestExpandPrimitiveForm_UnknownForm verifies that unknown primitives return unchanged.
func TestExpandPrimitiveForm_UnknownForm(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("not-a-real-primitive", sctx)
	body := syntax.SyntaxList(sctx)

	result, err := expander.ExpandPrimitiveForm("not-a-real-primitive", sym, body)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
}

// --- ExpandOnce ---

// TestExpandOnce_NonPair verifies that ExpandOnce returns non-pairs unchanged.
func TestExpandOnce_NonPair(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	// Symbol: not a macro call
	sym := syntax.NewSyntaxSymbol("x", sctx)
	result, expanded, err := expander.ExpandOnce(sym)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, sym)

	// Self-evaluating: not a macro call
	lit := syntax.NewSyntaxObject(values.NewInteger(42), sctx)
	result, expanded, err = expander.ExpandOnce(lit)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, lit)
}

// TestExpandOnce_EmptyList verifies that ExpandOnce returns empty list unchanged.
func TestExpandOnce_EmptyList(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	empty := syntax.SyntaxList(sctx)
	result, expanded, err := expander.ExpandOnce(empty)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, empty)
}

// TestExpandOnce_NonMacroCall verifies that a non-macro procedure call
// returns unchanged with expanded=false.
func TestExpandOnce_NonMacroCall(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	// (bindSymbolWithScopes 1 2) where bindSymbolWithScopes is not a macro
	form := syntax.SyntaxList(sctx,
		syntax.NewSyntaxSymbol("bindSymbolWithScopes", sctx),
		syntax.NewSyntaxObject(values.NewInteger(1), sctx),
		syntax.NewSyntaxObject(values.NewInteger(2), sctx),
	)
	result, expanded, err := expander.ExpandOnce(form)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, form)
}

// --- ExpanderContext bridge ---

// TestExpanderContext_Expand verifies the Expand bridge method.
func TestExpanderContext_Expand(t *testing.T) {
	c := qt.New(t)
	env, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	expanderCtx := NewExpanderContext(env, expander)

	// Self-evaluating value should pass through
	lit := syntax.NewSyntaxObject(values.NewInteger(42), sctx)
	result, err := expanderCtx.Expand(lit)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, lit)
}

// TestExpanderContext_ExpandOnce verifies the ExpandOnce bridge method.
func TestExpanderContext_ExpandOnce(t *testing.T) {
	c := qt.New(t)
	env, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	expanderCtx := NewExpanderContext(env, expander)

	// Non-macro call should return unchanged
	lit := syntax.NewSyntaxObject(values.NewInteger(42), sctx)
	result, expanded, err := expanderCtx.ExpandOnce(lit)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, lit)
}

// --- expandSyntaxError ---

// TestExpandSyntaxError verifies that syntax-error raises an error at expansion time.
func TestExpandSyntaxError(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	// (syntax-error "bad thing happened")
	sym := syntax.NewSyntaxSymbol("syntax-error", sctx)
	body := syntax.SyntaxList(sctx,
		syntax.NewSyntaxObject(values.NewString("bad thing happened"), sctx),
	)

	result, err := expander.expandSyntaxError(sym, body)
	c.Assert(result, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `syntax-error: bad thing happened: .*`)
}

// TestExpandSyntaxError_WithIrritants verifies syntax-error with irritant arguments.
func TestExpandSyntaxError_WithIrritants(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	// (syntax-error "bad" x y)
	sym := syntax.NewSyntaxSymbol("syntax-error", sctx)
	body := syntax.SyntaxList(sctx,
		syntax.NewSyntaxObject(values.NewString("bad"), sctx),
		syntax.NewSyntaxSymbol("x", sctx),
		syntax.NewSyntaxSymbol("y", sctx),
	)

	result, err := expander.expandSyntaxError(sym, body)
	c.Assert(result, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `syntax-error: bad: .*`)
}

// TestExpandSyntaxError_MissingMessage verifies syntax-error with no arguments.
func TestExpandSyntaxError_MissingMessage(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("syntax-error", sctx)
	body := syntax.SyntaxList(sctx) // empty

	_, err := expander.expandSyntaxError(sym, body)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `syntax-error: missing message argument: .*`)
}

// --- formatIrritants ---

func TestFormatIrritants(t *testing.T) {
	c := qt.New(t)

	c.Assert(formatIrritants([]string{"a"}), qt.Equals, "a")
	c.Assert(formatIrritants([]string{"a", "b"}), qt.Equals, "a, b")
	c.Assert(formatIrritants([]string{"x", "y", "z"}), qt.Equals, "x, y, z")
}
