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

	// Symbol named "..."
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
