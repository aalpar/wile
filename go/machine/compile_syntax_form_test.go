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

package machine

import (
	"testing"

	"wile/environment"
	"wile/syntax"

	qt "github.com/frankban/quicktest"
)

func TestCompileSyntax_SingleArg(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (syntax foo) -> (foo)
	template := syntax.NewSyntaxSymbol("foo", nil)
	expr := syntax.NewSyntaxCons(template, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
	c.Assert(len(tpl.operations) > 0, qt.IsTrue)
}

func TestCompileSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax")
}

func TestCompileSyntax_Error_TooManyArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (syntax foo bar) -> (foo bar)
	template := syntax.NewSyntaxSymbol("foo", nil)
	extra := syntax.NewSyntaxSymbol("bar", nil)
	expr := syntax.NewSyntaxCons(template,
		syntax.NewSyntaxCons(extra, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax")
}

func TestTemplateContainsEllipsis_NoEllipsis(t *testing.T) {
	c := qt.New(t)

	// Simple symbol
	stx := syntax.NewSyntaxSymbol("foo", nil)
	c.Assert(templateContainsEllipsis(stx), qt.IsFalse)
}

func TestTemplateContainsEllipsis_WithEllipsis(t *testing.T) {
	c := qt.New(t)

	// Sym named "..."
	stx := syntax.NewSyntaxSymbol("...", nil)
	c.Assert(templateContainsEllipsis(stx), qt.IsTrue)
}

func TestTemplateContainsEllipsis_InList(t *testing.T) {
	c := qt.New(t)

	// (foo ...)
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("foo", nil)
	list := syntax.NewSyntaxCons(foo,
		syntax.NewSyntaxCons(ellipsis, syntax.NewSyntaxEmptyList(nil), nil), nil)

	c.Assert(templateContainsEllipsis(list), qt.IsTrue)
}

func TestTemplateContainsEllipsis_EmptyList(t *testing.T) {
	c := qt.New(t)

	stx := syntax.NewSyntaxEmptyList(nil)
	c.Assert(templateContainsEllipsis(stx), qt.IsFalse)
}

func TestTemplateContainsEllipsis_EscapeForm(t *testing.T) {
	c := qt.New(t)

	// (... foo) - escape form, should return false because the ellipsis
	// is the escape marker, not an actual ellipsis to expand
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("foo", nil)
	escapeForm := syntax.NewSyntaxCons(ellipsis,
		syntax.NewSyntaxCons(foo, syntax.NewSyntaxEmptyList(nil), nil), nil)

	c.Assert(templateContainsEllipsis(escapeForm), qt.IsFalse)
}

func TestTemplateContainsEllipsis_EscapeFormWithEllipsisInside(t *testing.T) {
	c := qt.New(t)

	// (... ...) - escape form containing ellipsis, should return false
	// because the inner ellipsis is escaped (has no special meaning)
	ellipsis1 := syntax.NewSyntaxSymbol("...", nil)
	ellipsis2 := syntax.NewSyntaxSymbol("...", nil)
	escapeForm := syntax.NewSyntaxCons(ellipsis1,
		syntax.NewSyntaxCons(ellipsis2, syntax.NewSyntaxEmptyList(nil), nil), nil)

	c.Assert(templateContainsEllipsis(escapeForm), qt.IsFalse)
}

func TestTemplateContainsEllipsis_EscapeFormFollowedByEllipsis(t *testing.T) {
	c := qt.New(t)

	// ((... foo) x ...) - escape form followed by actual ellipsis
	// Should return true because of the trailing ellipsis
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("foo", nil)
	x := syntax.NewSyntaxSymbol("x", nil)
	trailingEllipsis := syntax.NewSyntaxSymbol("...", nil)

	escapeForm := syntax.NewSyntaxCons(ellipsis,
		syntax.NewSyntaxCons(foo, syntax.NewSyntaxEmptyList(nil), nil), nil)

	outerList := syntax.NewSyntaxCons(escapeForm,
		syntax.NewSyntaxCons(x,
			syntax.NewSyntaxCons(trailingEllipsis, syntax.NewSyntaxEmptyList(nil), nil), nil), nil)

	c.Assert(templateContainsEllipsis(outerList), qt.IsTrue)
}

func TestTemplateContainsEllipsis_BareEllipsisNotEscapeForm(t *testing.T) {
	c := qt.New(t)

	// (...) - just ellipsis with no template, NOT an escape form
	// Should return true because this is an unescaped ellipsis
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	bareEllipsis := syntax.NewSyntaxCons(ellipsis, syntax.NewSyntaxEmptyList(nil), nil)

	c.Assert(templateContainsEllipsis(bareEllipsis), qt.IsTrue)
}

// TestCompileSyntax_EscapeFormCompilesDirectly verifies that escape forms
// are compiled to direct bytecode operations rather than falling back to
// runtime expansion via OperationSyntaxTemplateExpand.
func TestCompileSyntax_EscapeFormCompilesDirectly(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (syntax (... foo)) - escape form should compile directly
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("foo", nil)
	escapeForm := syntax.NewSyntaxCons(ellipsis,
		syntax.NewSyntaxCons(foo, syntax.NewSyntaxEmptyList(nil), nil), nil)
	expr := syntax.NewSyntaxCons(escapeForm, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)

	// Verify NO OperationSyntaxTemplateExpand was generated
	// (escape forms should compile to direct literal loads, not runtime expansion)
	for _, op := range tpl.operations {
		_, isTemplateExpand := op.(*OperationSyntaxTemplateExpand)
		c.Assert(isTemplateExpand, qt.IsFalse,
			qt.Commentf("escape form should not generate OperationSyntaxTemplateExpand"))
	}
}

// TestCompileSyntax_NonEscapeEllipsisUsesRuntimeExpansion verifies that templates
// with actual (non-escaped) ellipsis fall back to runtime expansion.
func TestCompileSyntax_NonEscapeEllipsisUsesRuntimeExpansion(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (syntax (foo ...)) - actual ellipsis, needs runtime expansion
	foo := syntax.NewSyntaxSymbol("foo", nil)
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	template := syntax.NewSyntaxCons(foo,
		syntax.NewSyntaxCons(ellipsis, syntax.NewSyntaxEmptyList(nil), nil), nil)
	expr := syntax.NewSyntaxCons(template, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)

	// Verify OperationSyntaxTemplateExpand WAS generated
	hasTemplateExpand := false
	for _, op := range tpl.operations {
		if _, ok := op.(*OperationSyntaxTemplateExpand); ok {
			hasTemplateExpand = true
			break
		}
	}
	c.Assert(hasTemplateExpand, qt.IsTrue,
		qt.Commentf("non-escape ellipsis should generate OperationSyntaxTemplateExpand"))
}
