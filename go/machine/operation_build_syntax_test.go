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
	"context"
	"testing"

	"wile/environment"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestNewOperationBuildSyntaxList(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)

	c.Assert(op, qt.IsNotNil)
	c.Assert(op.Count, qt.Equals, 3)
}

func TestOperationBuildSyntaxList_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)
	c.Assert(op.String(), qt.Equals, "BuildSyntaxList")
}

func TestOperationBuildSyntaxList_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:build-syntax-list>")
}

func TestOperationBuildSyntaxList_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationBuildSyntaxList_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationBuildSyntaxList(3)
	op2 := NewOperationBuildSyntaxList(3)
	op3 := NewOperationBuildSyntaxList(5)

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(op3), qt.IsFalse)
	c.Assert(op1.EqualTo(values.NewInteger(3)), qt.IsFalse)
}

func TestOperationBuildSyntaxList_Apply_Empty(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationBuildSyntaxList(0)
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	// Should produce empty list
	val := mc.GetValue()
	c.Assert(val, qt.IsNotNil)
}

func TestOperationBuildSyntaxList_Apply_WithSyntaxValues(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Push syntax values to stack
	stx1 := syntax.NewSyntaxSymbol("a", nil)
	stx2 := syntax.NewSyntaxSymbol("b", nil)
	mc.evals.Push(stx1)
	mc.evals.Push(stx2)

	op := NewOperationBuildSyntaxList(2)
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	// Result should be a syntax list
	val := mc.GetValue()
	c.Assert(val, qt.IsNotNil)
}

func TestOperationBuildSyntaxList_Apply_WithValues(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Push regular values to stack
	mc.evals.Push(values.NewInteger(1))
	mc.evals.Push(values.NewInteger(2))

	op := NewOperationBuildSyntaxList(2)
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
}
