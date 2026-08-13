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

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestMachine_Operations(t *testing.T) {
	topEnv := environment.NewNamespace().Runtime()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrameWithParent(lenv, topEnv)
	// The load is load-bearing: OpPush now requires exactly one live value in
	// the register, so pushing off an uninitialized register raises.
	tpl := NewNativeTemplate(0, 0, false,
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush())
	tpl.MaybeAppendLiteral(values.NewSymbol("bindSymbolWithScopes"))
	mc := NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
	err := mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

func TestMachineContinuation_Parent(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	parent := NewMachineContinuation(nil, nil, env)
	child := NewMachineContinuation(parent, nil, env)

	qt.Assert(t, child.Parent(), qt.Equals, parent)
	qt.Assert(t, parent.Parent(), qt.IsNil)
}

func TestMachineContinuation_EnvironmentFrame(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.EnvironmentFrame(), qt.Equals, env)
}

func TestMachineContinuation_Template(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(3, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	qt.Assert(t, cont.Template(), qt.Equals, tpl)
}

func TestMachineContinuation_SetPC(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.PC(), qt.Equals, 0)

	cont.SetPC(42)
	qt.Assert(t, cont.PC(), qt.Equals, 42)
}

func TestMachineContinuation_PushValues(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, len(cont.multiValues), qt.Equals, 0)

	cont.PushValues(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, len(cont.multiValues), qt.Equals, 2)
	qt.Assert(t, cont.multiValues[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, cont.multiValues[1], valuestest.SchemeEquals, values.NewInteger(2))

	cont.PushValues(values.NewInteger(3))
	qt.Assert(t, len(cont.multiValues), qt.Equals, 3)
}

func TestMachineContinuation_PushValues_PromoteSingleToMulti(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	cont := NewMachineContinuation(nil, nil, env)

	// Start with a single value set on the continuation.
	cont.singleValue = values.NewInteger(1)

	// Pushing additional values should promote the single value into multiValues.
	cont.PushValues(values.NewInteger(2), values.NewInteger(3))

	qt.Assert(t, len(cont.multiValues), qt.Equals, 3)
	qt.Assert(t, cont.multiValues[0], valuestest.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, cont.multiValues[1], valuestest.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, cont.multiValues[2], valuestest.SchemeEquals, values.NewInteger(3))
	qt.Assert(t, cont.singleValue, qt.IsNil)
}

func TestMachineContinuation_Copy(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(2, 0, false)

	parent := NewMachineContinuation(nil, nil, env)
	cont := NewMachineContinuation(parent, tpl, env)
	cont.SetPC(10)
	cont.evals.Push(values.NewInteger(42))
	cont.singleValue = values.NewInteger(100)

	cpy := cont.Copy()

	// Verify copy is a different object
	qt.Assert(t, cpy != cont, qt.IsTrue)
	// Verify fields match
	qt.Assert(t, cpy.parent, qt.Equals, cont.parent)
	qt.Assert(t, cpy.env, qt.Equals, cont.env)
	qt.Assert(t, cpy.template, qt.Equals, cont.template)
	qt.Assert(t, cpy.pc, qt.Equals, cont.pc)
	// Verify singleValue is copied
	qt.Assert(t, cpy.singleValue, valuestest.SchemeEquals, cont.singleValue)
	// Verify evals stack is copied
	qt.Assert(t, cpy.evals != cont.evals, qt.IsTrue)
}

// TestMachineContinuation_Copy_MultiValuesIndependent locks in the
// slice-independence invariant that cloneValueRegisterFrom exists for:
// after Copy(), PushValues on either continuation must not affect the
// other. This is the property that makes call/cc re-invocation of
// multi-value continuations safe.
func TestMachineContinuation_Copy_MultiValuesIndependent(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	cont := NewMachineContinuation(nil, nil, env)
	cont.SetValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))

	cpy := cont.Copy()

	// Both sides see the initial three values.
	qt.Assert(t, len(cpy.multiValues), qt.Equals, 3)
	qt.Assert(t, len(cont.multiValues), qt.Equals, 3)

	// Mutating cpy must not affect cont — slices.Clone in
	// cloneValueRegisterFrom should have allocated a fresh backing array.
	cpy.PushValues(values.NewInteger(4))
	qt.Assert(t, len(cpy.multiValues), qt.Equals, 4)
	qt.Assert(t, len(cont.multiValues), qt.Equals, 3)

	// And symmetrically: mutating cont must not affect cpy.
	cont.PushValues(values.NewInteger(99))
	qt.Assert(t, len(cont.multiValues), qt.Equals, 4)
	qt.Assert(t, cpy.multiValues[3], valuestest.SchemeEquals, values.NewInteger(4))
}

func TestMachineContinuation_SchemeString(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.SchemeString(), qt.Equals, "<machine-continuation %0>")

	cont.SetPC(42)
	qt.Assert(t, cont.SchemeString(), qt.Equals, "<machine-continuation %42>")
}

func TestMachineContinuation_IsVoid(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	cont := NewMachineContinuation(nil, nil, env)
	qt.Assert(t, cont.IsVoid(), qt.IsFalse)

	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.IsVoid(), qt.IsTrue)
}

// TestMachineContinuation_EqualTo verifies identity comparison: a
// MachineContinuation equals only itself. Two distinct nodes are never equal,
// even with identical fields, because EqualTo is pointer identity.
func TestMachineContinuation_EqualTo(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(2, 0, false)

	cont1 := NewMachineContinuation(nil, tpl, env)

	// Same object.
	qt.Assert(t, cont1.EqualTo(cont1), qt.IsTrue)

	// A distinct node with identical fields is not equal (identity, not value).
	cont2 := NewMachineContinuation(nil, tpl, env)
	qt.Assert(t, cont1.EqualTo(cont2), qt.IsFalse)

	// Different Value type.
	qt.Assert(t, cont1.EqualTo(values.NewInteger(42)), qt.IsFalse)

	// Nil receiver: equal to nil, unequal to non-nil.
	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.EqualTo(nilCont), qt.IsTrue)
	qt.Assert(t, cont1.EqualTo(nilCont), qt.IsFalse)
	qt.Assert(t, nilCont.EqualTo(cont1), qt.IsFalse)
}

// Tests moved from coverage_additional_test.go
// TestMachineContinuationMethodsAdditional tests MachineContinuation methods
func TestMachineContinuationMethodsAdditional(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(
		NewOperationLoadVoid(),
		NewOperationRestoreContinuation())

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
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(tpl.MaybeAppendLiteral(values.NewInteger(42))),
		NewOperationRestoreContinuation(),
	)

	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)
	mc.pc = 0

	// Create continuation from machine context
	newCont := NewMachineContinuationFromMachineContext(mc, 1)
	qt.Assert(t, newCont, qt.IsNotNil)
}

// TestMachineContinuationMethods tests MachineContinuation methods
func TestMachineContinuationMethods(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)

	qt.Assert(t, cont.SchemeString(), qt.Contains, "machine-continuation")
	qt.Assert(t, cont.IsVoid(), qt.IsFalse)

	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.IsVoid(), qt.IsTrue)
}

// --- DeepCopy tests ---

func TestMachineContinuation_DeepCopy_Nil(t *testing.T) {
	var nilCont *MachineContinuation
	result := nilCont.DeepCopy()
	qt.Assert(t, result, qt.IsNil)
}

func TestMachineContinuation_DeepCopy_SingleFrame(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	cont.SetPC(7)
	cont.evals.Push(values.NewInteger(42))

	cpy := cont.DeepCopy()

	// Different object
	qt.Assert(t, cpy != cont, qt.IsTrue)
	// Same fields
	qt.Assert(t, cpy.pc, qt.Equals, 7)
	qt.Assert(t, cpy.template, qt.Equals, tpl)
	qt.Assert(t, cpy.env, qt.Equals, env)
	// No parent
	qt.Assert(t, cpy.parent, qt.IsNil)
	// Evals independently copied
	qt.Assert(t, cpy.evals != cont.evals, qt.IsTrue)
}

func TestMachineContinuation_DeepCopy_MultiFrameChain(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	bottom := NewMachineContinuation(nil, tpl, env)
	bottom.SetPC(1)
	middle := NewMachineContinuation(bottom, tpl, env)
	middle.SetPC(2)
	top := NewMachineContinuation(middle, tpl, env)
	top.SetPC(3)

	cpy := top.DeepCopy()

	// All frames are different objects
	qt.Assert(t, cpy != top, qt.IsTrue)
	qt.Assert(t, cpy.parent != middle, qt.IsTrue)
	qt.Assert(t, cpy.parent.parent != bottom, qt.IsTrue)

	// But preserve the chain structure
	qt.Assert(t, cpy.pc, qt.Equals, 3)
	qt.Assert(t, cpy.parent.pc, qt.Equals, 2)
	qt.Assert(t, cpy.parent.parent.pc, qt.Equals, 1)
	qt.Assert(t, cpy.parent.parent.parent, qt.IsNil)

	// Mutating the copy doesn't affect the original
	cpy.parent.SetPC(99)
	qt.Assert(t, middle.pc, qt.Equals, 2)
}

func TestMachineContinuation_DeepCopy_PreservesPromptTag(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tag := NewPromptTag("test")

	parent := NewMachineContinuation(nil, tpl, env)
	cont := NewMachineContinuationWithPrompt(parent, tpl, env, tag, nil)

	cpy := cont.DeepCopy()

	qt.Assert(t, cpy.PromptTag(), qt.Equals, tag)
	qt.Assert(t, cpy.parent != parent, qt.IsTrue)
}

// --- GraftContinuation tests ---

func TestGraftContinuation_NilSegment(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	target := NewMachineContinuation(nil, tpl, env)

	// Should not panic
	GraftContinuation(nil, target)
}

func TestGraftContinuation_SingleFrame(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	segment := NewMachineContinuation(nil, tpl, env)
	segment.SetPC(10)
	target := NewMachineContinuation(nil, tpl, env)
	target.SetPC(20)

	GraftContinuation(segment, target)

	qt.Assert(t, segment.parent, qt.Equals, target)
	qt.Assert(t, segment.parent.pc, qt.Equals, 20)
}

func TestGraftContinuation_MultiFrameSegment(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	bottom := NewMachineContinuation(nil, tpl, env)
	bottom.SetPC(1)
	top := NewMachineContinuation(bottom, tpl, env)
	top.SetPC(2)

	target := NewMachineContinuation(nil, tpl, env)
	target.SetPC(99)

	GraftContinuation(top, target)

	// Top's parent is still bottom
	qt.Assert(t, top.parent, qt.Equals, bottom)
	// Bottom's parent is now target
	qt.Assert(t, bottom.parent, qt.Equals, target)
	qt.Assert(t, bottom.parent.pc, qt.Equals, 99)
}

func TestGraftContinuation_NilTarget(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	segment := NewMachineContinuation(nil, tpl, env)

	GraftContinuation(segment, nil)

	qt.Assert(t, segment.parent, qt.IsNil)
}

// --- CallDepth tests ---

func TestMachineContinuation_CallDepth(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	var nilCont *MachineContinuation
	qt.Assert(t, nilCont.CallDepth(), qt.Equals, 0)

	c0 := NewMachineContinuation(nil, tpl, env)
	qt.Assert(t, c0.CallDepth(), qt.Equals, 0)

	c1 := NewMachineContinuation(c0, tpl, env)
	qt.Assert(t, c1.CallDepth(), qt.Equals, 1)

	c2 := NewMachineContinuation(c1, tpl, env)
	qt.Assert(t, c2.CallDepth(), qt.Equals, 2)
}

// TestNewMachineContinuationFromMachineContext_CallDepth verifies that
// NewMachineContinuationFromMachineContext computes callDepth correctly
// regardless of whether the caller pre-incremented mc.callDepth.
// This covers the PrimCallCC sub-context path, where mc.callDepth == 0
// would previously underflow.
func TestNewMachineContinuationFromMachineContext_CallDepth(t *testing.T) {
	c := qt.New(t)
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(0, 0, false)

	tcs := []struct {
		name      string
		setup     func() *MachineContext
		wantDepth int
	}{
		{
			name: "fresh context with nil cont",
			setup: func() *MachineContext {
				cont := NewMachineContinuation(nil, tpl, env)
				return NewMachineContext(context.Background(), cont)
			},
			wantDepth: 0,
		},
		{
			name: "context after one SaveContinuation",
			setup: func() *MachineContext {
				cont := NewMachineContinuation(nil, tpl, env)
				mc := NewMachineContext(context.Background(), cont)
				_ = mc.SaveContinuation(1)
				return mc
			},
			wantDepth: 1,
		},
		{
			name: "sub-context with callDepth 0",
			setup: func() *MachineContext {
				cont := NewMachineContinuation(nil, tpl, env)
				mc := NewMachineContext(context.Background(), cont)
				return mc.NewSubContext()
			},
			wantDepth: 0,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := tc.setup()
			newCont := NewMachineContinuationFromMachineContext(mc, 1)
			c.Assert(newCont.CallDepth(), qt.Equals, tc.wantDepth)
		})
	}
}
