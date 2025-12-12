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


package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimBytevectorCopyBang implements the bytevector-copy! primitive.
// Copies bytes between bytevectors.
func PrimBytevectorCopyBang(_ context.Context, mc *machine.MachineContext) error {
	to := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	at := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	from := mc.EnvironmentFrame().GetLocalBindingByIndex(2).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(3).Value()

	toBv, ok := to.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-copy!: expected a bytevector but got %T", to)
	}
	atIdx, ok := at.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy!: at must be an integer but got %T", at)
	}
	fromBv, ok := from.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-copy!: from must be a bytevector but got %T", from)
	}

	start := int64(0)
	end := int64(len(*fromBv))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			startVal, ok := pr.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy!: start must be an integer but got %T", pr.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := pr.Cdr()
			if !values.IsEmptyList(cdr) {
				pr2, ok := cdr.(*values.Pair)
				if ok && !values.IsEmptyList(pr2) {
					endVal, ok := pr2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy!: end must be an integer but got %T", pr2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(*fromBv)) {
		return values.NewForeignError("bytevector-copy!: start index out of bounds")
	}
	if end < start || end > int64(len(*fromBv)) {
		return values.NewForeignError("bytevector-copy!: end index out of bounds")
	}
	if atIdx.Value < 0 {
		return values.NewForeignError("bytevector-copy!: at index out of bounds")
	}
	if atIdx.Value+(end-start) > int64(len(*toBv)) {
		return values.NewForeignError("bytevector-copy!: not enough space in destination")
	}

	// Use copy with correct slice bounds - handles overlapping regions correctly
	copy((*toBv)[atIdx.Value:], (*fromBv)[start:end])
	mc.SetValues()
	return nil
}
