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

package helpers

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// Lengthable is the constraint for SequenceLength/Ref/Set: any pointer type
// that exposes a Length() int method. Satisfied by *values.Vector and
// *values.ByteVector.
type Lengthable interface {
	Length() int
}

// SequenceLength extracts a sequence of type T from arg 0, and sets
// the machine context value to its length as an exact integer.
// T must be a pointer type satisfying Lengthable.
func SequenceLength[T Lengthable](
	mc machine.CallContext,
	sentinel error,
	name string,
) error {
	seq, err := RequireArg[T](mc, 0, sentinel, name)
	if err != nil {
		return err
	}
	mc.SetValue(values.NewInteger(int64(seq.Length())))
	return nil
}

// SequenceRef extracts a sequence of type T from arg 0, validates an
// exact integer index from arg 1, and sets the machine context value
// to getElement(seq, idx). The getElement closure handles type-specific
// element retrieval (e.g., Vector.Get vs ByteVector byte-to-integer conversion).
func SequenceRef[T Lengthable](
	mc machine.CallContext,
	sentinel error,
	name string,
	getElement func(T, int) values.Value,
) error {
	seq, err := RequireArg[T](mc, 0, sentinel, name)
	if err != nil {
		return err
	}
	idx, err := RequireIndex(mc, 1, seq.Length(), name)
	if err != nil {
		return err
	}
	mc.SetValue(getElement(seq, idx))
	return nil
}

// SequenceSet extracts a sequence of type T from arg 0, validates an
// exact integer index from arg 1, and delegates to setElement(seq, idx, mc)
// for the type-specific mutation. Sets Void on success.
func SequenceSet[T Lengthable](
	mc machine.CallContext,
	sentinel error,
	name string,
	setElement func(T, int, machine.CallContext) error,
) error {
	seq, err := RequireArg[T](mc, 0, sentinel, name)
	if err != nil {
		return err
	}
	idx, err := RequireIndex(mc, 1, seq.Length(), name)
	if err != nil {
		return err
	}
	err = setElement(seq, idx, mc)
	if err != nil {
		return err
	}
	mc.SetValue(values.Void)
	return nil
}
