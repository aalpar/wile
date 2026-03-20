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

func TestCompileSyntaxCase_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.SyntaxEmptyList

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
	c.Assert(errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue)
}

func TestCompileSyntaxCase_Error_NoLiterals(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (input) - missing literals and clauses
	input := syntax.NewSyntaxSymbol("x", nil)
	expr := syntax.NewSyntaxCons(input, syntax.SyntaxEmptyList, nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}

func TestCompileSyntaxCase_Error_NoClauses(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (input ()) - missing clauses
	input := syntax.NewSyntaxSymbol("x", nil)
	literals := syntax.SyntaxEmptyList
	expr := syntax.NewSyntaxCons(input,
		syntax.NewSyntaxCons(literals, syntax.SyntaxEmptyList, nil), nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}
