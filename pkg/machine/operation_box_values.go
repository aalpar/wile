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
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// boxedValuesType is an internal carrier that collapses the whole value register
// (zero, one, or several values) into a SINGLE values.Value so it can be saved
// on the eval stack as one slot. It never escapes to Scheme: it lives only on
// the eval stack between an OperationBoxValues and its paired OperationUnboxValues
// (currently dynamic-wind, bracketing the after-thunk call). Identified by type,
// like noMarkSentinelType.
type boxedValuesType struct {
	vals []values.Value
}

func (boxedValuesType) SchemeString() string {
	return "#<boxed-values>"
}

func (boxedValuesType) IsVoid() bool {
	return false
}

func (p boxedValuesType) EqualTo(o values.Value) bool {
	_, ok := o.(boxedValuesType)
	return ok
}

// OperationBoxValues collapses the value register (0, 1, or N values) into a
// single boxedValuesType carrier left in the value register. A following OpPush
// then saves it as exactly one eval-stack slot, so callers that must preserve a
// multiple-value result across an intervening call (dynamic-wind's after-thunk)
// keep a fixed one-slot footprint regardless of value count. Paired with
// OperationUnboxValues.
//
// Pre:  value register = v1 … vN
// Post: value register = #<boxed-values v1 … vN>, pc++
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
	// Copy: GetValues returns the live multiValues slice; the box outlives the
	// register (the bracketed call rebinds the register), so own the values.
	boxed := boxedValuesType{}
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

// OperationUnboxValues expands a boxedValuesType carrier in the value register
// back into the value register's 0/1/N values (the inverse of OperationBoxValues).
// A preceding OpPeekK loads the carrier into the value register; this op replaces
// it with the boxed values.
//
// Pre:  value register = #<boxed-values v1 … vN>
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
	boxed, ok := mc.GetValue().(boxedValuesType)
	if !ok {
		// Compiler invariant: OperationUnboxValues only ever runs on a value
		// register holding a box produced by OperationBoxValues.
		err := mc.WrapError(werr.ErrInternal,
			"unbox-values: value register does not hold a boxed-values carrier")
		return mc, err
	}
	mc.SetValues(boxed.vals...)
	mc.pc++
	return mc, nil
}

func (p *OperationUnboxValues) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationUnboxValues)
	return SameType(p, v, ok)
}
