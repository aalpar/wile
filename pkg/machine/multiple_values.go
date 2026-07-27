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
	"strings"

	"github.com/aalpar/wile/pkg/values"
)

// MultipleValues represents multiple return values from a function. It is
// deliberately NOT a values.Value.
//
// Multiple values are carried in the VM's dedicated multi-value register
// (vmState.multiValues), under a singleValue-XOR-multiValues invariant — never
// in a Value slot. The Value conformance this type used to carry was therefore
// unreachable as a Scheme datum, and it was actively harmful: []values.Value is
// not Go-comparable, so a MultipleValues boxed into a values.Value would fault
// any `==` or map-key hash of that interface. values.EqIdentity (eq?) is exactly
// such an `==`.
//
// It still carries SchemeString and IsVoid for diagnostics, which leaves it ONE method
// — EqualTo(values.Value) — from silently re-entering the Value set as a non-comparable
// slice. TestSliceCarriersAreNotValues exists to stop that.
//
// The type remains a convenient []values.Value alias for internal carriers
// (NativeTemplate.literals, the value register). It just is not a Scheme value.
type MultipleValues []values.Value

// NewMultipleValues creates a new MultipleValues from the given values.
func NewMultipleValues(values ...values.Value) MultipleValues {
	return values
}

// Len returns the number of values in the MultipleValues.
func (p MultipleValues) Len() int {
	return len(p)
}

// Copy creates a copy of the MultipleValues.
func (p MultipleValues) Copy() MultipleValues {
	return slices.Clone(p)
}

// IsVoid reports whether the register content this carries is the absence of a
// result: no values at all, or exactly one void value.
//
// Not part of any interface — see the type's doc comment. It is a predicate the
// VM asks of a register snapshot, kept because the callers want it, not because
// values.Value demands it.
func (p MultipleValues) IsVoid() bool {
	if len(p) == 0 {
		return true
	}
	if len(p) == 1 && values.IsVoid(p[0]) {
		return true
	}
	return false
}

// SchemeString renders the values space-separated, as `(values …)` would print
// them. Diagnostic support (disassembly, test failure messages); not a Scheme
// external representation, since this type is not a Scheme datum.
func (p MultipleValues) SchemeString() string {
	q := strings.Builder{}
	if len(p) == 0 {
		return values.SpecialVoid
	}
	if len(p) == 1 && values.IsVoid(p[0]) {
		return values.SpecialVoid
	}
	q.WriteString(p[0].SchemeString())
	for _, v := range p[1:] {
		q.WriteString(" ")
		q.WriteString(v.SchemeString())
	}
	return q.String()
}

// EqualTo reports whether two value lists are element-wise equal.
//
// It takes a concrete MultipleValues, not a values.Value: this type is not a
// Scheme datum and does not implement the Value interface.
func (p MultipleValues) EqualTo(v MultipleValues) bool {
	if len(p) != len(v) {
		return false
	}
	for i := range p {
		if !p[i].EqualTo(v[i]) {
			return false
		}
	}
	return true
}
