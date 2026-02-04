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
	"testing"

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/syntax"
	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// newExpanderEnv creates a test environment with expander support.
func newExpanderEnv() (*environment.EnvironmentFrame, *ExpanderTimeContinuation) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	expander := NewExpanderTimeContinuation(env)
	return env, expander
}

// --- Primitive expander stubs ---

// TestExpandQuasisyntax_ReturnsUnchanged verifies that the quasisyntax
// primitive expander returns the form unchanged (it's handled at compile time).
func TestExpandQuasisyntax_ReturnsUnchanged(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("quasisyntax", sctx)
	body := syntax.SyntaxList(sctx, syntax.NewSyntaxObject(values.NewInteger(1), sctx))

	result, err := expander.expandQuasisyntax(ectx, sym, body)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	// Result is (quasisyntax . body) — returned unchanged
	pair, ok := result.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pair.SyntaxCar().(*syntax.SyntaxSymbol).Sym.Key, qt.Equals, "quasisyntax")
}

// TestExpandUnsyntax_ReturnsUnchanged verifies that unsyntax returns unchanged.
func TestExpandUnsyntax_ReturnsUnchanged(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("unsyntax", sctx)
	body := syntax.SyntaxList(sctx, syntax.NewSyntaxObject(values.NewInteger(1), sctx))

	result, err := expander.expandUnsyntax(ectx, sym, body)
	c.Assert(err, qt.IsNil)
	pair, ok := result.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pair.SyntaxCar().(*syntax.SyntaxSymbol).Sym.Key, qt.Equals, "unsyntax")
}

// TestExpandUnsyntaxSplicing_ReturnsUnchanged verifies unsyntax-splicing returns unchanged.
func TestExpandUnsyntaxSplicing_ReturnsUnchanged(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("unsyntax-splicing", sctx)
	body := syntax.SyntaxList(sctx, syntax.NewSyntaxObject(values.NewInteger(1), sctx))

	result, err := expander.expandUnsyntaxSplicing(ectx, sym, body)
	c.Assert(err, qt.IsNil)
	pair, ok := result.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pair.SyntaxCar().(*syntax.SyntaxSymbol).Sym.Key, qt.Equals, "unsyntax-splicing")
}

// TestExpandWithSyntax_ReturnsUnchanged verifies with-syntax returns unchanged.
func TestExpandWithSyntax_ReturnsUnchanged(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("with-syntax", sctx)
	body := syntax.SyntaxList(sctx)

	result, err := expander.expandWithSyntax(ectx, sym, body)
	c.Assert(err, qt.IsNil)
	pair, ok := result.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	c.Assert(pair.SyntaxCar().(*syntax.SyntaxSymbol).Sym.Key, qt.Equals, "with-syntax")
}

// --- ExpandPrimitiveForm ---

// TestExpandPrimitiveForm_KnownForm verifies that ExpandPrimitiveForm dispatches
// to the registered primitive expander for known forms.
func TestExpandPrimitiveForm_KnownForm(t *testing.T) {
	c := qt.New(t)
	env, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// "quote" has a registered primitive expander that returns unchanged
	sym := syntax.NewSyntaxSymbolForSymbol(env.InternSymbol(values.NewSymbol("quote")), sctx)
	body := syntax.SyntaxList(sctx, syntax.NewSyntaxObject(values.NewInteger(42), sctx))

	result, err := expander.ExpandPrimitiveForm(ectx, "quote", sym, body)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
}

// TestExpandPrimitiveForm_UnknownForm verifies that unknown primitives return unchanged.
func TestExpandPrimitiveForm_UnknownForm(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("not-a-real-primitive", sctx)
	body := syntax.SyntaxList(sctx)

	result, err := expander.ExpandPrimitiveForm(ectx, "not-a-real-primitive", sym, body)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
}

// --- ExpandOnce ---

// TestExpandOnce_NonPair verifies that ExpandOnce returns non-pairs unchanged.
func TestExpandOnce_NonPair(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// Symbol: not a macro call
	sym := syntax.NewSyntaxSymbol("x", sctx)
	result, expanded, err := expander.ExpandOnce(ectx, sym)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, sym)

	// Self-evaluating: not a macro call
	lit := syntax.NewSyntaxObject(values.NewInteger(42), sctx)
	result, expanded, err = expander.ExpandOnce(ectx, lit)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, lit)
}

// TestExpandOnce_EmptyList verifies that ExpandOnce returns empty list unchanged.
func TestExpandOnce_EmptyList(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	empty := syntax.SyntaxList(sctx)
	result, expanded, err := expander.ExpandOnce(ectx, empty)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, empty)
}

// TestExpandOnce_NonMacroCall verifies that a non-macro procedure call
// returns unchanged with expanded=false.
func TestExpandOnce_NonMacroCall(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// (foo 1 2) where foo is not a macro
	form := syntax.SyntaxList(sctx,
		syntax.NewSyntaxSymbol("foo", sctx),
		syntax.NewSyntaxObject(values.NewInteger(1), sctx),
		syntax.NewSyntaxObject(values.NewInteger(2), sctx),
	)
	result, expanded, err := expander.ExpandOnce(ectx, form)
	c.Assert(err, qt.IsNil)
	c.Assert(expanded, qt.IsFalse)
	c.Assert(result, qt.Equals, form)
}

// --- ExpanderContext bridge ---

// TestExpanderContext_Expand verifies the Expand bridge method.
func TestExpanderContext_Expand(t *testing.T) {
	c := qt.New(t)
	env, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	expanderCtx := NewExpanderContext(env, expander, ectx)

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
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	expanderCtx := NewExpanderContext(env, expander, ectx)

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
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// (syntax-error "bad thing happened")
	sym := syntax.NewSyntaxSymbol("syntax-error", sctx)
	body := syntax.SyntaxList(sctx,
		syntax.NewSyntaxObject(values.NewString("bad thing happened"), sctx),
	)

	result, err := expander.expandSyntaxError(ectx, sym, body)
	c.Assert(result, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `syntax-error: bad thing happened`)
}

// TestExpandSyntaxError_WithIrritants verifies syntax-error with irritant arguments.
func TestExpandSyntaxError_WithIrritants(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// (syntax-error "bad" x y)
	sym := syntax.NewSyntaxSymbol("syntax-error", sctx)
	body := syntax.SyntaxList(sctx,
		syntax.NewSyntaxObject(values.NewString("bad"), sctx),
		syntax.NewSyntaxSymbol("x", sctx),
		syntax.NewSyntaxSymbol("y", sctx),
	)

	result, err := expander.expandSyntaxError(ectx, sym, body)
	c.Assert(result, qt.IsNil)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `syntax-error: bad: .*`)
}

// TestExpandSyntaxError_MissingMessage verifies syntax-error with no arguments.
func TestExpandSyntaxError_MissingMessage(t *testing.T) {
	c := qt.New(t)
	_, expander := newExpanderEnv()
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	sym := syntax.NewSyntaxSymbol("syntax-error", sctx)
	body := syntax.SyntaxList(sctx) // empty

	_, err := expander.expandSyntaxError(ectx, sym, body)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err, qt.ErrorMatches, `syntax-error: missing message argument`)
}

// --- formatIrritants ---

func TestFormatIrritants(t *testing.T) {
	c := qt.New(t)

	c.Assert(formatIrritants([]string{"a"}), qt.Equals, "a")
	c.Assert(formatIrritants([]string{"a", "b"}), qt.Equals, "a, b")
	c.Assert(formatIrritants([]string{"x", "y", "z"}), qt.Equals, "x, y, z")
}
