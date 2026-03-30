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
	"errors"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

func TestCompileSyntax_SingleArg(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env, NewVMMacroEvaluator())

	// (syntax bindSymbolWithScopes) -> (bindSymbolWithScopes)
	template := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	expr := syntax.NewSyntaxCons(template, syntax.SyntaxEmptyList, nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil)
	c.Assert(tpl.CodeLen() > 0, qt.IsTrue)
}

func TestCompileSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env, NewVMMacroEvaluator())

	// Empty args
	expr := syntax.SyntaxEmptyList

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax")
	c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue)
}

func TestCompileSyntax_Error_TooManyArgs(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env, NewVMMacroEvaluator())

	// (syntax bindSymbolWithScopes bar) -> (bindSymbolWithScopes bar)
	template := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	extra := syntax.NewSyntaxSymbol("bar", nil)
	expr := syntax.NewSyntaxCons(template,
		syntax.NewSyntaxCons(extra, syntax.SyntaxEmptyList, nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax")
	c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue)
}

func TestTemplateContainsEllipsis_NoEllipsis(t *testing.T) {
	c := qt.New(t)

	// Simple symbol
	stx := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
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

	// (bindSymbolWithScopes ...)
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	list := syntax.NewSyntaxCons(foo,
		syntax.NewSyntaxCons(ellipsis, syntax.SyntaxEmptyList, nil), nil)

	c.Assert(templateContainsEllipsis(list), qt.IsTrue)
}

func TestTemplateContainsEllipsis_EmptyList(t *testing.T) {
	c := qt.New(t)

	stx := syntax.SyntaxEmptyList
	c.Assert(templateContainsEllipsis(stx), qt.IsFalse)
}

func TestTemplateContainsEllipsis_EscapeForm(t *testing.T) {
	c := qt.New(t)

	// (... bindSymbolWithScopes) - escape form, should return false because the ellipsis
	// is the escape marker, not an actual ellipsis to expand
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	escapeForm := syntax.NewSyntaxCons(ellipsis,
		syntax.NewSyntaxCons(foo, syntax.SyntaxEmptyList, nil), nil)

	c.Assert(templateContainsEllipsis(escapeForm), qt.IsFalse)
}

func TestTemplateContainsEllipsis_EscapeFormWithEllipsisInside(t *testing.T) {
	c := qt.New(t)

	// (... ...) - escape form containing ellipsis, should return false
	// because the inner ellipsis is escaped (has no special meaning)
	ellipsis1 := syntax.NewSyntaxSymbol("...", nil)
	ellipsis2 := syntax.NewSyntaxSymbol("...", nil)
	escapeForm := syntax.NewSyntaxCons(ellipsis1,
		syntax.NewSyntaxCons(ellipsis2, syntax.SyntaxEmptyList, nil), nil)

	c.Assert(templateContainsEllipsis(escapeForm), qt.IsFalse)
}

func TestTemplateContainsEllipsis_EscapeFormFollowedByEllipsis(t *testing.T) {
	c := qt.New(t)

	// ((... bindSymbolWithScopes) x ...) - escape form followed by actual ellipsis
	// Should return true because of the trailing ellipsis
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	x := syntax.NewSyntaxSymbol("x", nil)
	trailingEllipsis := syntax.NewSyntaxSymbol("...", nil)

	escapeForm := syntax.NewSyntaxCons(ellipsis,
		syntax.NewSyntaxCons(foo, syntax.SyntaxEmptyList, nil), nil)

	outerList := syntax.NewSyntaxCons(escapeForm,
		syntax.NewSyntaxCons(x,
			syntax.NewSyntaxCons(trailingEllipsis, syntax.SyntaxEmptyList, nil), nil), nil)

	c.Assert(templateContainsEllipsis(outerList), qt.IsTrue)
}

func TestTemplateContainsEllipsis_BareEllipsisNotEscapeForm(t *testing.T) {
	c := qt.New(t)

	// (...) - just ellipsis with no template, NOT an escape form
	// Should return true because this is an unescaped ellipsis
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	bareEllipsis := syntax.NewSyntaxCons(ellipsis, syntax.SyntaxEmptyList, nil)

	c.Assert(templateContainsEllipsis(bareEllipsis), qt.IsTrue)
}

// TestCompileSyntax_EscapeFormCompilesDirectly verifies that escape forms
// are compiled to direct bytecode operations rather than falling back to
// runtime expansion via OperationSyntaxTemplateExpand.
func TestCompileSyntax_EscapeFormCompilesDirectly(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env, NewVMMacroEvaluator())

	// (syntax (... bindSymbolWithScopes)) - escape form should compile directly
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	escapeForm := syntax.NewSyntaxCons(ellipsis,
		syntax.NewSyntaxCons(foo, syntax.SyntaxEmptyList, nil), nil)
	expr := syntax.NewSyntaxCons(escapeForm, syntax.SyntaxEmptyList, nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil)

	// Verify NO OperationSyntaxTemplateExpand was generated
	// (escape forms should compile to direct literal loads, not runtime expansion)
	for _, op := range tpl.Operations() {
		_, isTemplateExpand := op.(*OperationSyntaxTemplateExpand)
		c.Assert(isTemplateExpand, qt.IsFalse,
			qt.Commentf("escape form should not generate OperationSyntaxTemplateExpand"))
	}
}

// TestCompileSyntax_NonEscapeEllipsisUsesRuntimeExpansion verifies that templates
// with actual (non-escaped) ellipsis fall back to runtime expansion.
func TestCompileSyntax_NonEscapeEllipsisUsesRuntimeExpansion(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env, NewVMMacroEvaluator())

	// (syntax (bindSymbolWithScopes ...)) - actual ellipsis, needs runtime expansion
	foo := syntax.NewSyntaxSymbol("bindSymbolWithScopes", nil)
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	template := syntax.NewSyntaxCons(foo,
		syntax.NewSyntaxCons(ellipsis, syntax.SyntaxEmptyList, nil), nil)
	expr := syntax.NewSyntaxCons(template, syntax.SyntaxEmptyList, nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil)

	// Verify OperationSyntaxTemplateExpand WAS generated
	hasTemplateExpand := false
	for _, op := range tpl.Operations() {
		_, ok := op.(*OperationSyntaxTemplateExpand)
		if ok {
			hasTemplateExpand = true
			break
		}
	}
	c.Assert(hasTemplateExpand, qt.IsTrue,
		qt.Commentf("non-escape ellipsis should generate OperationSyntaxTemplateExpand"))
}
