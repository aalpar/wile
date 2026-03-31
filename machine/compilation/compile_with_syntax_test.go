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

func TestCompileWithSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// Empty args
	expr := syntax.SyntaxEmptyList

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "with-syntax")
}

func TestCompileWithSyntax_Error_NoBody(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// (()) - empty bindings list, no body
	bindings := syntax.SyntaxEmptyList
	expr := syntax.NewSyntaxCons(bindings, syntax.SyntaxEmptyList, nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "with-syntax")
}

func TestCompileWithSyntax_EmptyBindings(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	// (() body) - empty bindings, simple body
	bindings := syntax.SyntaxEmptyList
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(bindings,
		syntax.NewSyntaxCons(body, syntax.SyntaxEmptyList, nil), nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(context.Background(), false), expr)
	c.Assert(err, qt.IsNil)
}
