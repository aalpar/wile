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
	"slices"

	"github.com/aalpar/wile/pkg/values"
)

var _ values.Value = (*boxedValues)(nil)

// boxedValues is an internal carrier that collapses a zero- or many-valued
// value register into a SINGLE values.Value so it can be saved on the eval
// stack as one slot. It never escapes to Scheme: it lives only on
// the eval stack between an OperationBoxValues and its paired OperationUnboxValues
// (currently dynamic-wind, bracketing the after-thunk call). Identified by type,
// like noMarkSentinelType.
//
// It is used through a POINTER (*boxedValues), and that is load-bearing, not
// incidental. The struct holds a slice, so the bare struct type is not
// Go-comparable; boxed into a values.Value it would fault any `==` or map-key
// hash of the interface — values.EqIdentity (eq?) is exactly such an `==`. A
// pointer is comparable, which is what keeps this carrier inside the Value
// contract while it sits in the value register. See values.Value's doc comment,
// and TestSliceCarriersAreNotValues.
type boxedValues struct {
	vals []values.Value
}

func (*boxedValues) SchemeString() string {
	return "#<boxed-values>"
}

func (*boxedValues) IsVoid() bool {
	return false
}

func (p *boxedValues) EqualTo(o values.Value) bool {
	v, ok := o.(*boxedValues)
	return SameType(p, v, ok)
}

// OperationBoxValues reduces the value register to something a following OpPush
// saves as exactly one eval-stack slot, so callers that must preserve a
// multiple-value result across an intervening call (dynamic-wind's after-thunk)
// keep a fixed one-slot footprint regardless of value count. Paired with
// OperationUnboxValues.
//
// Zero or several values become a *boxedValues carrier. Exactly one value is
// left ALONE: it already occupies one slot, so boxing it bought nothing and
// cost three allocations per dynamic-wind (the carrier, its slice, and the
// Clone on the way back out). That is the overwhelmingly common case, and
// skipping it is worth ~5.7% on a dynamic-wind-bound workload.
//
// Pre:  value register = v1 … vN
// Post: N == 1: unchanged. Otherwise a carrier holding v1 … vN. pc++
//
//	(the carrier prints as #<boxed-values>; it never escapes to Scheme)
type OperationBoxValues struct {
	OperationBase
}

func NewOperationBoxValues() *OperationBoxValues {
	return &OperationBoxValues{
		OperationBase: NewOperationBase("machine-operation-box-values"),
	}
}

func (*OperationBoxValues) Apply(mc *MachineContext) (*MachineContext, error) {
	src := mc.GetValues()
	// One value is already exactly one eval-stack slot, so the carrier buys
	// nothing: leave the register alone and let OperationUnboxValues pass it
	// through. GetValues's single-value result does not escape this frame, so
	// the check itself allocates nothing.
	if len(src) == 1 {
		mc.pc++
		return mc, nil
	}
	// Copy: GetValues returns the live multiValues slice; the box outlives the
	// register (the bracketed call rebinds the register), so own the values.
	boxed := &boxedValues{}
	if len(src) > 0 {
		boxed.vals = make([]values.Value, len(src))
		copy(boxed.vals, src)
	}
	mc.SetValue(boxed)
	mc.pc++
	return mc, nil
}

func (p *OperationBoxValues) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationBoxValues)
	return SameType(p, v, ok)
}

// OperationUnboxValues expands a *boxedValues carrier in the value register
// back into the value register's 0/N values (the inverse of OperationBoxValues).
// A preceding OpPeekK loads the carrier into the value register; this op replaces
// it with the boxed values. A register holding anything else is the single-value
// fast path and passes through untouched.
//
// Pre:  value register = a carrier holding v1 … vN, or the single value itself
// Post: value register = v1 … vN, pc++
type OperationUnboxValues struct {
	OperationBase
}

func NewOperationUnboxValues() *OperationUnboxValues {
	return &OperationUnboxValues{
		OperationBase: NewOperationBase("machine-operation-unbox-values"),
	}
}

func (*OperationUnboxValues) Apply(mc *MachineContext) (*MachineContext, error) {
	boxed, ok := mc.GetValue().(*boxedValues)
	if !ok {
		// The single-value fast path: OperationBoxValues declined to box, so the
		// register already holds the thunk's one value. A fallthrough rather than
		// an error because CompileValidatedDynamicWind is the pair's ONLY emitter
		// and emits both ops in one function, so this register can hold only a
		// carrier made by this op's partner or the value that partner left alone.
		// Nothing else in the tree produces this slot.
		mc.pc++
		return mc, nil
	}
	// Clone on the way out: SetValues stores the slice by reference for N>1
	// (vm_state.go), so without this the value register would alias the box's
	// private backing array — and the same box may be unboxed again on
	// continuation re-entry. Symmetric with the defensive copy in
	// OperationBoxValues; cheap (dynamic-wind exit path only).
	mc.SetValues(slices.Clone(boxed.vals)...)
	mc.pc++
	return mc, nil
}

func (p *OperationUnboxValues) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnboxValues)
	return SameType(p, v, ok)
}
