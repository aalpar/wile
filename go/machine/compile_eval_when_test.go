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

func TestCompileEvalWhen_Error_NilEnv(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	ccnt := &CompileTimeContinuation{
		template: tpl,
		env:      nil,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, nil), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_Error_NilTemplate(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	ccnt := &CompileTimeContinuation{
		template: nil,
		env:      env,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_EmptyBody(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// ((run)) - phases with empty body, should emit void
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("run", nil),
		syntax.NewSyntaxEmptyList(nil), nil)
	expr := syntax.NewSyntaxCons(phases, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil) // Empty body is valid, emits void
}

func TestCompileEvalWhen_Error_UnknownPhase(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// ((unknown) body)
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("unknown", nil),
		syntax.NewSyntaxEmptyList(nil), nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unknown phase")
}

func TestCompileEvalWhen_RunPhase(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// ((run) 42)
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("run", nil),
		syntax.NewSyntaxEmptyList(nil), nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
	c.Assert(len(tpl.operations) > 0, qt.IsTrue)
}

func TestCompileEvalWhen_EmptyPhases(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (() 42) - no phases, should emit void
	phases := syntax.NewSyntaxEmptyList(nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
}
