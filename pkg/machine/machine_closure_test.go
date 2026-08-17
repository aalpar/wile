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
	"testing"
	"unsafe"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestMachineClosureIsTwoWords is a ratchet, not a curiosity. Closure size is
// what the shape-vs-parent arc has been trading against since the pair split:
// materializing the environment cost an 80-byte frame per evaluated lambda, the
// pair that replaced it cost a third word (16 -> 24B, measured as +1.3% on
// call-bound code), and moving the shape onto NativeTemplate is what bought that
// word back. A fourth field would undo it silently — every functional test would
// still pass.
//
// Adding a field here is a decision to re-open that trade. Measure it, do not
// just update the constant.
func TestMachineClosureIsTwoWords(t *testing.T) {
	qt.Assert(t, unsafe.Sizeof(MachineClosure{}), qt.Equals, 2*unsafe.Sizeof(uintptr(0)))
}

func TestMachineClosure_IsVoid(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, cls.IsVoid(), qt.IsFalse)

	var nilCls *MachineClosure
	qt.Assert(t, nilCls.IsVoid(), qt.IsTrue)
}

func TestMachineClosure_SchemeString(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, cls.SchemeString(), qt.Equals, "#<machine-closure>")
}

func TestMachineClosure_EqualTo(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	cls1 := NewClosureWithTemplate(tpl, env)
	cls2 := NewClosureWithTemplate(tpl, env)
	cls3 := cls1

	// Same object
	qt.Assert(t, cls1.EqualTo(cls3), qt.IsTrue)
	// Distinct objects are distinct procedures even when every field agrees.
	// This assertion used to read IsTrue, which is exactly why the suite stayed
	// green while (equal? a b) diverged from (eqv? a b) at the Scheme level.
	// R7RS §6.1 requires them to agree; EqualTo is therefore object identity.
	qt.Assert(t, cls1.EqualTo(cls2), qt.IsFalse)

	// Different template
	tpl2 := NewNativeTemplate(1, 0, false)
	cls4 := NewClosureWithTemplate(tpl2, env)
	qt.Assert(t, cls1.EqualTo(cls4), qt.IsFalse)

	// Different environment
	env2 := environment.NewNamespace().Runtime()
	cls5 := NewClosureWithTemplate(tpl, env2)
	qt.Assert(t, cls1.EqualTo(cls5), qt.IsFalse)

	// The capturing representation, which no test covered before: two closures
	// from one lambda form in one activation share parent AND template — and so,
	// necessarily, the shape the template carries.
	shape := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), env)
	tpl.SetShape(shape)
	pair1 := NewClosureCapturing(tpl, env)
	pair2 := NewClosureCapturing(tpl, env)
	qt.Assert(t, pair1.EqualTo(pair2), qt.IsFalse)
	qt.Assert(t, pair1.EqualTo(pair1), qt.IsTrue)

	// Nil comparison
	var nilCls *MachineClosure
	qt.Assert(t, nilCls.EqualTo(nilCls), qt.IsTrue)
	qt.Assert(t, cls1.EqualTo(nilCls), qt.IsFalse)

	// Different type
	qt.Assert(t, cls1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

func TestMachineClosure_AcceptsArity(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	tcs := []struct {
		name     string
		params   int
		variadic bool
		n        int
		want     bool
	}{
		{"fixed-2 accepts 2", 2, false, 2, true},
		{"fixed-2 rejects 1", 2, false, 1, false},
		{"fixed-2 rejects 3", 2, false, 3, false},
		{"fixed-0 accepts 0", 0, false, 0, true},
		{"fixed-0 rejects 1", 0, false, 1, false},
		{"variadic-1 accepts 0", 1, true, 0, true},
		{"variadic-1 accepts 1", 1, true, 1, true},
		{"variadic-1 accepts 5", 1, true, 5, true},
		{"variadic-3 rejects 1", 3, true, 1, false},
		{"variadic-3 accepts 2", 3, true, 2, true},
		{"variadic-3 accepts 3", 3, true, 3, true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tpl := NewNativeTemplate(tc.params, 0, tc.variadic)
			cls := NewClosureWithTemplate(tpl, env)
			qt.Assert(t, cls.AcceptsArity(tc.n), qt.Equals, tc.want)
		})
	}
}

// Tests moved from coverage_additional_test.go
// TestMachineClosureMethodsAdditional tests MachineClosure methods
func TestMachineClosureMethodsAdditional(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(1, 0, false)
	closure := NewClosureWithTemplate(tpl, env)

	qt.Assert(t, closure.IsVoid(), qt.IsFalse)
	qt.Assert(t, closure.SchemeString(), qt.Contains, "closure")
	qt.Assert(t, closure.Template(), qt.Equals, tpl)

	// EqualTo is object identity, so a second closure over the same env and
	// template is NOT equal. See TestMachineClosure_EqualTo for why: equal? must
	// agree with eqv? on procedures (R7RS §6.1).
	closure2 := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, closure.EqualTo(closure2), qt.IsFalse)
	qt.Assert(t, closure.EqualTo(closure), qt.IsTrue)

	var nilClosure *MachineClosure
	qt.Assert(t, closure.EqualTo(nilClosure), qt.IsFalse)
}

// TestMachineClosureMethods tests MachineClosure methods
func TestMachineClosureMethods(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(2, 1, true)
	closure := NewClosureWithTemplate(tpl, env)

	qt.Assert(t, closure.SchemeString(), qt.Contains, "#<machine-closure")
	qt.Assert(t, closure.IsVoid(), qt.IsFalse)

	var nilClosure *MachineClosure
	qt.Assert(t, nilClosure.IsVoid(), qt.IsTrue)
}

// TestMachineClosureEqualToNil tests closure equality with nil
func TestMachineClosureEqualToNil(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := NewNativeTemplate(2, 2, true)
	cls := NewClosureWithTemplate(tpl, env)

	// Test with nil closure
	var nilCls *MachineClosure
	qt.Assert(t, nilCls.IsVoid(), qt.IsTrue)
	qt.Assert(t, cls.EqualTo(nilCls), qt.IsFalse)
}

// TestForeignClosureSchemeString tests ForeignClosure methods
func TestForeignClosureSchemeString(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	fn := func(_ CallContext) error {
		return nil
	}
	cls := NewForeignClosure(env, 2, true, fn)

	// Check that closure was created (can't test ParameterCount/IsVariadic on MachineClosure)
	qt.Assert(t, cls, qt.IsNotNil)
	qt.Assert(t, cls.SchemeString(), qt.Contains, "closure")
}
