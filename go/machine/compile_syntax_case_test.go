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

func TestCompileSyntaxCase_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}

func TestCompileSyntaxCase_Error_NoLiterals(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (input) - missing literals and clauses
	input := syntax.NewSyntaxSymbol("x", nil)
	expr := syntax.NewSyntaxCons(input, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}

func TestCompileSyntaxCase_Error_NoClauses(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (input ()) - missing clauses
	input := syntax.NewSyntaxSymbol("x", nil)
	literals := syntax.NewSyntaxEmptyList(nil)
	expr := syntax.NewSyntaxCons(input,
		syntax.NewSyntaxCons(literals, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}
