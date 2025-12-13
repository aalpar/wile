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
	"wile/environment"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestMachine_Operations(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(0, 0, false, NewOperationPush())
	tpl.MaybeAppendLiteral(values.NewSymbol("foo"))
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err := mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
}

func TestMachineContinuation_Parent(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	parent := NewMachineContinuation(nil, nil, env)
	child := NewMachineContinuation(parent, nil, env)

	qt.Assert(t, child.Parent(), qt.Equals, parent)
	qt.Assert(t, parent.Parent(), qt.IsNil)
}

func TestMachineContinuation_EnvironmentFrame(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.EnvironmentFrame(), qt.Equals, env)
}

func TestMachineContinuation_Template(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(3, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	qt.Assert(t, cont.Template(), qt.Equals, tpl)
}

func TestMachineContinuation_SetPC(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.PC(), qt.Equals, 0)

	cont.SetPC(42)
	qt.Assert(t, cont.PC(), qt.Equals, 42)
}

func TestMachineContinuation_PushValues(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, len(cont.value), qt.Equals, 0)

	cont.PushValues(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, len(cont.value), qt.Equals, 2)
	qt.Assert(t, cont.value[0], values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, cont.value[1], values.SchemeEquals, values.NewInteger(2))

	cont.PushValues(values.NewInteger(3))
	qt.Assert(t, len(cont.value), qt.Equals, 3)
}

func TestMachineContinuation_Copy(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(2, 0, false)

	parent := NewMachineContinuation(nil, nil, env)
	cont := NewMachineContinuation(parent, tpl, env)
	cont.SetPC(10)
	cont.evals.Push(values.NewInteger(42))
	cont.value = NewMultipleValues(values.NewInteger(100))

	copy := cont.Copy()

	// Verify copy is a different object
	qt.Assert(t, copy != cont, qt.IsTrue)
	// Verify fields match
	qt.Assert(t, copy.parent, qt.Equals, cont.parent)
	qt.Assert(t, copy.env, qt.Equals, cont.env)
	qt.Assert(t, copy.template, qt.Equals, cont.template)
	qt.Assert(t, copy.pc, qt.Equals, cont.pc)
	// Verify value slice is copied
	qt.Assert(t, copy.value[0], values.SchemeEquals, cont.value[0])
	qt.Assert(t, &copy.value[0] != &cont.value[0], qt.IsTrue)
	// Verify evals stack is copied
	qt.Assert(t, copy.evals != cont.evals, qt.IsTrue)
}

func TestMachineContinuation_SchemeString(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.SchemeString(), qt.Equals, "<machine-continuation %0>")

	cont.SetPC(42)
	qt.Assert(t, cont.SchemeString(), qt.Equals, "<machine-continuation %42>")
}

func TestMachineContinuation_IsVoid(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.IsVoid(), qt.IsFalse)

	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.IsVoid(), qt.IsTrue)
}

func TestMachineContinuation_EqualTo(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(2, 0, false)

	cont1 := NewMachineContinuation(nil, tpl, env)
	cont2 := cont1 // Same object
	cont3 := NewMachineContinuation(nil, tpl, env)

	// Same object
	qt.Assert(t, cont1.EqualTo(cont2), qt.IsTrue)

	// Different objects with same fields - different evals stacks
	qt.Assert(t, cont1.EqualTo(cont3), qt.IsFalse)

	// Different type
	qt.Assert(t, cont1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Nil comparison
	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.EqualTo(nilCont), qt.IsTrue)

	// Different pc
	cont4 := NewMachineContinuation(nil, tpl, env)
	cont4.pc = 10
	qt.Assert(t, cont1.EqualTo(cont4), qt.IsFalse)

	// Different parent
	parent := NewMachineContinuation(nil, nil, env)
	cont5 := &MachineContinuation{
		parent:   parent,
		env:      cont1.env,
		template: cont1.template,
		evals:    cont1.evals,
		pc:       cont1.pc,
	}
	qt.Assert(t, cont1.EqualTo(cont5), qt.IsFalse)

	// Different template
	tpl2 := NewNativeTemplate(3, 0, false)
	cont6 := &MachineContinuation{
		parent:   nil,
		env:      cont1.env,
		template: tpl2,
		evals:    cont1.evals,
		pc:       cont1.pc,
	}
	qt.Assert(t, cont1.EqualTo(cont6), qt.IsFalse)
}

// Tests moved from coverage_additional_test.go
// TestMachineContinuationMethodsAdditional tests MachineContinuation methods
func TestMachineContinuationMethodsAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations, NewOperationLoadVoid())
	tpl.operations = append(tpl.operations, NewOperationRestoreContinuation())

	cont := NewMachineContinuation(nil, tpl, env)

	qt.Assert(t, cont.IsVoid(), qt.IsFalse)
	qt.Assert(t, cont.SchemeString(), qt.Contains, "continuation")
	qt.Assert(t, cont.Template(), qt.Equals, tpl)
	qt.Assert(t, cont.Parent(), qt.IsNil)

	// Test EqualTo - same object should be equal to itself
	qt.Assert(t, cont.EqualTo(cont), qt.IsTrue)

	var nilCont *MachineContinuation
	qt.Assert(t, cont.EqualTo(nilCont), qt.IsFalse)
}

// TestMachineContinuationFromMachineContext tests creating continuation from context
func TestMachineContinuationFromMachineContext(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.operations = append(tpl.operations,
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(42))),
		NewOperationRestoreContinuation(),
	)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(cont)
	mc.pc = 0

	// Create continuation from machine context
	newCont := NewMachineContinuationFromMachineContext(mc, 1)
	qt.Assert(t, newCont, qt.IsNotNil)
}

// TestMachineContinuationMethods tests MachineContinuation methods
func TestMachineContinuationMethods(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)

	qt.Assert(t, cont.SchemeString(), qt.Contains, "machine-continuation")
	qt.Assert(t, cont.IsVoid(), qt.IsFalse)

	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.IsVoid(), qt.IsTrue)
}

// TestMachineContinuationEqualToDifferentTemplates tests continuation equality with different templates
func TestMachineContinuationEqualToDifferentTemplates(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationRestoreContinuation())
	cont := NewMachineContinuation(nil, tpl, env)

	// Test with different templates
	tpl2 := NewNativeTemplate(1, 1, true)
	tpl2.AppendOperations(NewOperationLoadLiteralInteger(1), NewOperationRestoreContinuation())
	cont2 := NewMachineContinuation(nil, tpl2, env)
	qt.Assert(t, cont.EqualTo(cont2), qt.IsFalse) // Different templates
}
