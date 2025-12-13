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
	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestNewOperationDrop(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationDrop_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-drop>")
}

func TestOperationDrop_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op.IsVoid(), qt.IsFalse)

	var nilOp *OperationDrop
	c.Assert(nilOp.IsVoid(), qt.IsTrue)
}

func TestOperationDrop_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationDrop()
	op2 := NewOperationDrop()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationDrop_EqualTo_NilCases(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	var nilOp *OperationDrop

	c.Assert(op.EqualTo(nilOp), qt.IsFalse)
	c.Assert(nilOp.EqualTo(op), qt.IsFalse)
	c.Assert(nilOp.EqualTo(nilOp), qt.IsTrue)
}

func TestOperationDrop_Apply(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Push value to stack
	mc.evals.Push(values.NewInteger(42))
	c.Assert(mc.evals.Length(), qt.Equals, 1)

	op := NewOperationDrop()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(mc.evals.Length(), qt.Equals, 0)
	c.Assert(mc.pc, qt.Equals, 1)
}
