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
)

// PrimMakeBytevector implements the (make-bytevector) primitive.
// Creates a bytevector of the given size, optionally filled with a specified byte value.
func PrimMakeBytevector(_ context.Context, mc *machine.MachineContext) error {
	size, err := helpers.RequireArg[*values.Integer](mc, 0, values.ErrNotAnInteger, "make-bytevector")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)
	if size.Value < 0 {
		return values.NewForeignError("make-bytevector: size must be non-negative")
	}
	var fill uint8
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if ok && !tuple.IsEmptyList() {
			fillVal := tuple.Car()
			fillInt, ok := fillVal.(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "make-bytevector: fill must be an integer but got %T", fillVal)
			}
			if fillInt.Value < 0 || fillInt.Value > 255 {
				return values.NewForeignError("make-bytevector: fill must be a byte (0-255)")
			}
			fill = uint8(fillInt.Value)
		}
	}
	bv := make(values.ByteVector, size.Value)
	for i := range bv {
		bv[i] = &values.Byte{Value: fill}
	}
	mc.SetValue(&bv)
	return nil
}

// PrimBytevector implements the bytevector primitive.
// Creates bytevector from byte arguments.
func PrimBytevector(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector: expected a list but got %T", o)
	}
	var bytes []*values.Byte
	v, err := tuple.ForEach(ctx, func(_ context.Context, _ int, hasNext bool, v values.Value) error {
		intVal, ok := v.(*values.Integer)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector: expected an integer but got %T", v)
		}
		if intVal.Value < 0 || intVal.Value > 255 {
			return values.NewForeignError("bytevector: value must be a byte (0-255)")
		}
		bytes = append(bytes, values.NewByte(uint8(intVal.Value)))
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector: not a proper list")
	}
	bv := values.ByteVector(bytes)
	mc.SetValue(&bv)
	return nil
}

// PrimBytevectorLength implements the bytevector-length primitive.
// Returns length of bytevector.
func PrimBytevectorLength(_ context.Context, mc *machine.MachineContext) error {
	return helpers.SequenceLength[*values.ByteVector](mc, values.ErrNotAByteVector, "bytevector-length")
}

// PrimBytevectorU8Ref implements the bytevector-u8-ref primitive.
// Returns byte at index as an exact integer (R7RS §6.9).
func PrimBytevectorU8Ref(_ context.Context, mc *machine.MachineContext) error {
	return helpers.SequenceRef(mc, values.ErrNotAByteVector, "bytevector-u8-ref",
		func(bv *values.ByteVector, idx int) values.Value {
			return values.NewInteger(int64((*bv)[idx].Value))
		},
	)
}

// PrimBytevectorU8Set implements the bytevector-u8-set! primitive.
// Sets byte at index.
func PrimBytevectorU8Set(_ context.Context, mc *machine.MachineContext) error {
	return helpers.SequenceSet(mc, values.ErrNotAByteVector, "bytevector-u8-set!",
		func(bv *values.ByteVector, idx int, mc *machine.MachineContext) error {
			byteVal, err := helpers.RequireType[*values.Integer](mc.Arg(2), values.ErrNotAnInteger, "bytevector-u8-set!")
			if err != nil {
				return err
			}
			if byteVal.Value < 0 || byteVal.Value > 255 {
				return values.NewForeignError("bytevector-u8-set!: value must be a byte (0-255)")
			}
			(*bv)[idx] = values.NewByte(uint8(byteVal.Value))
			return nil
		},
	)
}

// PrimBytevectorCopy implements the bytevector-copy primitive.
// Returns a copy of a bytevector.
func PrimBytevectorCopy(_ context.Context, mc *machine.MachineContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, values.ErrNotAByteVector, "bytevector-copy")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseOptionalStartEnd(rest, int64(len(*bv)), "bytevector-copy")
	if err != nil {
		return err
	}
	err = helpers.ValidateStartEnd(start, end, int64(len(*bv)), "bytevector-copy")
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
func PrimBytevectorCopyBang(_ context.Context, mc *machine.MachineContext) error {
	toBv, err := helpers.RequireArg[*values.ByteVector](mc, 0, values.ErrNotAByteVector, "bytevector-copy!")
	if err != nil {
		return err
	}
	atIdx, err := helpers.RequireArg[*values.Integer](mc, 1, values.ErrNotAnInteger, "bytevector-copy!")
	if err != nil {
		return err
	}
	fromBv, err := helpers.RequireArg[*values.ByteVector](mc, 2, values.ErrNotAByteVector, "bytevector-copy!")
	if err != nil {
		return err
	}
	rest := mc.Arg(3)

	start, end, err := helpers.ParseOptionalStartEnd(rest, int64(len(*fromBv)), "bytevector-copy!")
	if err != nil {
		return err
	}
	err = helpers.ValidateStartEnd(start, end, int64(len(*fromBv)), "bytevector-copy!")
	if err != nil {
		return err
	}
	if atIdx.Value < 0 || atIdx.Value+(end-start) > int64(len(*toBv)) {
		return values.WrapForeignErrorf(values.ErrIndexOutOfRange, "bytevector-copy!: invalid destination index")
	}

	// Use copy with correct slice bounds - handles overlapping regions correctly
	copy((*toBv)[atIdx.Value:], (*fromBv)[start:end])
	mc.SetValue(values.Void)
	return nil
}

// PrimBytevectorAppend implements the bytevector-append primitive.
// Concatenates bytevectors.
func PrimBytevectorAppend(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		bv := values.ByteVector{}
		mc.SetValue(&bv)
		return nil
	}
	tuple, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector-append: expected a list but got %T", o)
	}
	result := values.NewByteVector()
	v, err := tuple.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
		bv, ok := v.(*values.ByteVector)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-append: expected a bytevector but got %T", v)
		}
		*result = append(*result, *bv...)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "bytevector-append: not a proper list")
	}
	mc.SetValue(result)
	return nil
}

// PrimUtf8ToString implements the utf8->string primitive.
// Converts a UTF-8 encoded bytevector to a string with optional start and end indices.
func PrimUtf8ToString(_ context.Context, mc *machine.MachineContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, values.ErrNotAByteVector, "utf8->string")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseOptionalStartEnd(rest, int64(len(*bv)), "utf8->string")
	if err != nil {
		return err
	}
	err = helpers.ValidateStartEnd(start, end, int64(len(*bv)), "utf8->string")
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
func PrimStringToUtf8(_ context.Context, mc *machine.MachineContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string->utf8")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	s := str.Value
	start, end, err := helpers.ParseOptionalStartEnd(rest, int64(len(s)), "string->utf8")
	if err != nil {
		return err
	}
	err = helpers.ValidateStartEnd(start, end, int64(len(s)), "string->utf8")
	if err != nil {
		return err
	}

	// Convert string to bytevector
	bytes := []byte(s[start:end])
	bv := make(values.ByteVector, len(bytes))
	for i, b := range bytes {
		bv[i] = values.NewByte(b)
	}
	mc.SetValue(&bv)
	return nil
}
