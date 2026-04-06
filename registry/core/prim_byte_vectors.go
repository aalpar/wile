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

package core

import (
	"context"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimMakeBytevector implements the (make-bytevector) primitive.
// Creates a bytevector of the given size, optionally filled with a specified byte value.
func PrimMakeBytevector(mc machine.CallContext) error {
	size, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "make-bytevector")
	if err != nil {
		return err
	}
	if size.Value < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "make-bytevector: size must be non-negative")
	}
	fillInt, err := helpers.OptionalArg[*values.Integer](mc.Arg(1), values.NewInteger(0), werr.ErrNotAnInteger, "make-bytevector")
	if err != nil {
		return err
	}
	err = values.ValidateByteValue(fillInt, "make-bytevector", "fill")
	if err != nil {
		return err
	}
	fill := uint8(fillInt.Value)
	bv := make(values.ByteVector, size.Value)
	for i := range bv {
		bv[i] = &values.Byte{Value: fill}
	}
	mc.SetValue(&bv)
	return nil
}

// PrimBytevector implements the bytevector primitive.
// Creates bytevector from byte arguments.
func PrimBytevector(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "bytevector: expected a list but got %T", o)
	}
	var bytes []*values.Byte
	v, err := tuple.ForEach(mc.Context(), func(_ context.Context, _ int, hasNext bool, v values.Value) error {
		intVal, ok := v.(*values.Integer)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "bytevector: expected an integer but got %T", v)
		}
		err := values.ValidateByteValue(intVal, "bytevector", "value")
		if err != nil {
			return err
		}
		bytes = append(bytes, values.NewByte(uint8(intVal.Value)))
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "bytevector: not a proper list")
	}
	bv := values.ByteVector(bytes)
	mc.SetValue(&bv)
	return nil
}

// PrimBytevectorLength implements the bytevector-length primitive.
// Returns length of bytevector.
func PrimBytevectorLength(mc machine.CallContext) error {
	return helpers.SequenceLength[*values.ByteVector](mc, werr.ErrNotAByteVector, "bytevector-length")
}

// PrimBytevectorU8Ref implements the bytevector-u8-ref primitive.
// Returns byte at index as an exact integer (R7RS §6.9).
func PrimBytevectorU8Ref(mc machine.CallContext) error {
	return helpers.SequenceRef(mc, werr.ErrNotAByteVector, "bytevector-u8-ref",
		func(bv *values.ByteVector, idx int) values.Value {
			return values.NewInteger(int64((*bv)[idx].Value))
		},
	)
}

// PrimBytevectorU8Set implements the bytevector-u8-set! primitive.
// Sets byte at index.
func PrimBytevectorU8Set(mc machine.CallContext) error {
	return helpers.SequenceSet(mc, werr.ErrNotAByteVector, "bytevector-u8-set!",
		func(bv *values.ByteVector, idx int, mc machine.CallContext) error {
			byteVal, err := helpers.RequireType[*values.Integer](mc.Arg(2), werr.ErrNotAnInteger, "bytevector-u8-set!")
			if err != nil {
				return err
			}
			err = values.ValidateByteValue(byteVal, "bytevector-u8-set!", "value")
			if err != nil {
				return err
			}
			(*bv)[idx] = values.NewByte(uint8(byteVal.Value))
			return nil
		},
	)
}

// PrimBytevectorCopy implements the bytevector-copy primitive.
// Returns a copy of a bytevector.
func PrimBytevectorCopy(mc machine.CallContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "bytevector-copy")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseSubrange(rest, len(*bv), "bytevector-copy")
	if err != nil {
		return err
	}

	result := make(values.ByteVector, end-start)
	copy(result, (*bv)[start:end])
	mc.SetValue(&result)
	return nil
}

// PrimBytevectorCopyBang implements the bytevector-copy! primitive.
// Copies bytes between bytevectors.
func PrimBytevectorCopyBang(mc machine.CallContext) error {
	toBv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "bytevector-copy!")
	if err != nil {
		return err
	}
	atIdx, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotAnInteger, "bytevector-copy!")
	if err != nil {
		return err
	}
	fromBv, err := helpers.RequireArg[*values.ByteVector](mc, 2, werr.ErrNotAByteVector, "bytevector-copy!")
	if err != nil {
		return err
	}
	rest := mc.Arg(3)

	start, end, err := helpers.ParseSubrange(rest, len(*fromBv), "bytevector-copy!")
	if err != nil {
		return err
	}
	if atIdx.Value < 0 || atIdx.Value+int64(end-start) > int64(len(*toBv)) {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "bytevector-copy!: invalid destination index")
	}

	// Use copy with correct slice bounds - handles overlapping regions correctly
	copy((*toBv)[atIdx.Value:], (*fromBv)[start:end])
	mc.SetValue(values.Void)
	return nil
}

// PrimBytevectorAppend implements the bytevector-append primitive.
// Concatenates bytevectors.
func PrimBytevectorAppend(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "bytevector-append: expected a list but got %T", o)
	}
	result := values.NewByteVector()
	v, err := tuple.ForEach(mc.Context(), func(_ context.Context, _ int, _ bool, v values.Value) error {
		bv, ok := v.(*values.ByteVector)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAByteVector, "bytevector-append: expected a bytevector but got %T", v)
		}
		*result = append(*result, *bv...)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "bytevector-append: not a proper list")
	}
	mc.SetValue(result)
	return nil
}

// PrimUtf8ToString implements the utf8->string primitive.
// Converts a UTF-8 encoded bytevector to a string with optional start and end indices.
//
// R7RS §6.9: (utf8->string bytevector [start [end]])
// Decodes the bytes of a bytevector between start and end (byte positions) and
// returns the corresponding string.
func PrimUtf8ToString(mc machine.CallContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "utf8->string")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseSubrange(rest, len(*bv), "utf8->string")
	if err != nil {
		return err
	}

	// Convert bytes to string
	bytes := make([]byte, end-start)
	for i := start; i < end; i++ {
		bytes[i-start] = (*bv)[i].Value
	}
	mc.SetValue(values.NewString(string(bytes)))
	return nil
}

// PrimStringToUtf8 implements the string->utf8 primitive.
// Converts a string to a UTF-8 encoded bytevector with optional start and end indices.
//
// R7RS §6.9: (string->utf8 string [start [end]])
// Returns a newly allocated bytevector containing the UTF-8 encoding of the
// characters in string between start and end (character positions, not byte positions).
func PrimStringToUtf8(mc machine.CallContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string->utf8")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	// Convert to runes for CHARACTER-based indexing (R7RS §6.9)
	runes := []rune(str.Value)

	// Parse indices as CHARACTER positions
	start, end, err := helpers.ParseSubrange(rest, len(runes), "string->utf8")
	if err != nil {
		return err
	}

	// Extract character range, convert to UTF-8 bytes
	substring := string(runes[start:end])
	bytes := []byte(substring)

	bv := make(values.ByteVector, len(bytes))
	for i, b := range bytes {
		bv[i] = values.NewByte(b)
	}
	mc.SetValue(&bv)
	return nil
}
