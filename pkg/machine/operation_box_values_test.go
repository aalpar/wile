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
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// Neither op had a unit test before, which is how the single-value fast path
// could delete the carrier — the type that proved the register came from the
// partner op — without anything noticing. The round trip is the contract:
// dynamic-wind must return the thunk's values, zero and several included
// (R7RS §6.10).

func boxUnboxContext() *MachineContext {
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationBoxValues(), NewOperationUnboxValues())
	env := environment.NewNamespace().Runtime()
	return NewMachineContext(context.Background(), NewMachineContinuation(nil, tpl, env))
}

func TestBoxUnboxRoundTrip(t *testing.T) {
	tcs := []struct {
		name string
		vals []values.Value
	}{
		{name: "zero values", vals: nil},
		{name: "one value", vals: []values.Value{values.NewInteger(7)}},
		{name: "two values", vals: []values.Value{values.NewInteger(1), values.NewInteger(2)}},
		{name: "three values", vals: []values.Value{values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)}},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := boxUnboxContext()
			mc.SetValues(tc.vals...)

			_, err := (&OperationBoxValues{}).Apply(mc)
			qt.Assert(t, err, qt.IsNil)
			// Whatever the count, the register is now exactly one eval-stack
			// slot's worth: OpPush pushes every value in the register, so a
			// multi-value register here would misalign dynamic-wind's PeekK
			// offsets.
			qt.Assert(t, len(mc.GetValues()) <= 1, qt.IsTrue)

			_, err = (&OperationUnboxValues{}).Apply(mc)
			qt.Assert(t, err, qt.IsNil)

			got := mc.GetValues()
			qt.Assert(t, len(got), qt.Equals, len(tc.vals))
			for i := range tc.vals {
				qt.Assert(t, got[i], qt.Equals, tc.vals[i])
			}
		})
	}
}

func TestBoxValuesLeavesSingleValueAlone(t *testing.T) {
	mc := boxUnboxContext()
	v := values.NewInteger(42)
	mc.SetValues(v)

	_, err := (&OperationBoxValues{}).Apply(mc)
	qt.Assert(t, err, qt.IsNil)
	// Not merely equal: the identical value, unboxed. This is the allocation
	// the fast path exists to avoid.
	qt.Assert(t, mc.GetValue(), qt.Equals, values.Value(v))
	_, isCarrier := mc.GetValue().(*BoxedValues)
	qt.Assert(t, isCarrier, qt.IsFalse)
}

func TestUnboxValuesDoesNotAliasTheCarrier(t *testing.T) {
	mc := boxUnboxContext()
	mc.SetValues(values.NewInteger(1), values.NewInteger(2))

	_, err := (&OperationBoxValues{}).Apply(mc)
	qt.Assert(t, err, qt.IsNil)
	carrier, ok := mc.GetValue().(*BoxedValues)
	qt.Assert(t, ok, qt.IsTrue)

	_, err = (&OperationUnboxValues{}).Apply(mc)
	qt.Assert(t, err, qt.IsNil)

	// The register must not share the carrier's backing array: the same carrier
	// can be unboxed again on continuation re-entry, and SetValues stores the
	// slice by reference for N>1.
	got := mc.GetValues()
	got[0] = values.NewInteger(99)
	qt.Assert(t, carrier.vals[0].EqualTo(values.NewInteger(1)), qt.IsTrue)
}

// TestUnboxValuesRejectsAMisalignedRegister pins the check that replaced the
// deleted type assertion. Two values in the register at unbox time means the
// eval stack is not what CompileValidatedDynamicWind emitted.
func TestUnboxValuesRejectsAMisalignedRegister(t *testing.T) {
	mc := boxUnboxContext()
	mc.SetValues(values.NewInteger(1), values.NewInteger(2))

	_, err := (&OperationUnboxValues{}).Apply(mc)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrInternal), qt.IsTrue)
}
