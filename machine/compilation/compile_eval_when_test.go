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

package compilation

import (
	"context"
	"testing"

	"github.com/aalpar/wile/machine"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestCompileEvalWhen_Error_NilEnv(t *testing.T) {
	c := qt.New(t)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := &CompileTimeContinuation{
		template: tpl,
		env:      nil,
	}

	expr := syntax.SyntaxEmptyList

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_Error_NilTemplate(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	ccnt := &CompileTimeContinuation{
		template: nil,
		env:      env,
	}

	expr := syntax.SyntaxEmptyList

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// Empty args
	expr := syntax.SyntaxEmptyList

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_EmptyBody(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// ((run)) - phases with empty body, should emit void
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("run", nil),
		syntax.SyntaxEmptyList, nil)
	expr := syntax.NewSyntaxCons(phases, syntax.SyntaxEmptyList, nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil) // Empty body is valid, emits void
}

func TestCompileEvalWhen_Error_UnknownPhase(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// ((unknown) body)
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("unknown", nil),
		syntax.SyntaxEmptyList, nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.SyntaxEmptyList, nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unknown phase")
}

func TestCompileEvalWhen_RunPhase(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// ((run) 42)
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("run", nil),
		syntax.SyntaxEmptyList, nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.SyntaxEmptyList, nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil)
	c.Assert(tpl.CodeLen() > 0, qt.IsTrue)
}

func TestCompileEvalWhen_EmptyPhases(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// (() 42) - no phases, should emit void
	phases := syntax.SyntaxEmptyList
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.SyntaxEmptyList, nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil)
}
