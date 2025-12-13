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

func TestNewOperationPopEnv(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationPopEnv_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op.String(), qt.Equals, "PopEnv")
}

func TestOperationPopEnv_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:pop-env>")
}

func TestOperationPopEnv_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationPopEnv_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationPopEnv()
	op2 := NewOperationPopEnv()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationPopEnv_Apply_Success(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	parentEnv := environment.NewEnvironmentFrame(nil, genv)
	childEnv := environment.NewEnvironmentFrameWithParent(nil, parentEnv)

	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, childEnv))

	op := NewOperationPopEnv()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(mc.env, qt.Equals, parentEnv)
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationPopEnv_Apply_Error_NoParent(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	// env has no parent (Parent() returns nil)

	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationPopEnv()
	_, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "PopEnv")
}
