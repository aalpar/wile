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
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestCompileWithSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "with-syntax")
}

func TestCompileWithSyntax_Error_NoBody(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (()) - empty bindings list, no body
	bindings := syntax.NewSyntaxEmptyList(nil)
	expr := syntax.NewSyntaxCons(bindings, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "with-syntax")
}

func TestCompileWithSyntax_EmptyBindings(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (() body) - empty bindings, simple body
	bindings := syntax.NewSyntaxEmptyList(nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(bindings,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
}
