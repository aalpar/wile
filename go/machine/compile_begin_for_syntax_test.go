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

func TestCompileBeginForSyntax_Error_NilEnv(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	ccnt := &CompileTimeContinuation{
		template: tpl,
		env:      nil,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, nil), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "begin-for-syntax")
}

func TestCompileBeginForSyntax_Error_NilTemplate(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	ccnt := &CompileTimeContinuation{
		template: nil,
		env:      env,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "begin-for-syntax")
}

func TestCompileBeginForSyntax_Error_NotPair(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Not a pair
	expr := syntax.NewSyntaxSymbol("bad", nil)

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "begin-for-syntax")
}

func TestCompileBeginForSyntax_Empty(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Use an empty SyntaxPair (which returns true for IsSyntaxEmptyList)
	exprPair := &syntax.SyntaxPair{}

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, env), exprPair)
	c.Assert(err, qt.IsNil)
}
