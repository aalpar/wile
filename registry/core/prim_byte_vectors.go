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
	"github.com/aalpar/wile/values"
)

// PrimMakeBytevector implements the (make-bytevector) primitive.
// Creates a bytevector of the given size, optionally filled with a specified byte value.
func PrimMakeBytevector(_ context.Context, mc *machine.MachineContext) error {
	k := mc.Arg(0)
	rest := mc.Arg(1)
	size, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "make-bytevector: expected an integer but got %T", k)
	}
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
	o := mc.Arg(0)
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-length: expected a bytevector but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(len(*bv))))
	return nil
}

// PrimBytevectorU8Ref implements the bytevector-u8-ref primitive.
// Returns byte at index.
func PrimBytevectorU8Ref(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-u8-ref: expected a bytevector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-u8-ref: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*bv)) {
		return values.NewForeignError("bytevector-u8-ref: index out of bounds")
	}
	mc.SetValue(values.NewInteger(int64((*bv)[idx.Value].Value)))
	return nil
}

// PrimBytevectorU8Set implements the bytevector-u8-set! primitive.
// Sets byte at index.
func PrimBytevectorU8Set(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	obj := mc.Arg(2)
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-u8-set!: expected a bytevector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-u8-set!: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*bv)) {
		return values.NewForeignError("bytevector-u8-set!: index out of bounds")
	}
	byteVal, ok := obj.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-u8-set!: expected an integer but got %T", obj)
	}
	if byteVal.Value < 0 || byteVal.Value > 255 {
		return values.NewForeignError("bytevector-u8-set!: value must be a byte (0-255)")
	}
	(*bv)[idx.Value] = values.NewByte(uint8(byteVal.Value))
	mc.SetValues()
	return nil
}

// PrimBytevectorCopy implements the bytevector-copy primitive.
// Returns a copy of a bytevector.
func PrimBytevectorCopy(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	rest := mc.Arg(1)
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "bytevector-copy: expected a bytevector but got %T", o)
	}

	start := int64(0)
	end := int64(len(*bv))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if ok && !tuple.IsEmptyList() {
			startVal, ok := tuple.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy: start must be an integer but got %T", tuple.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := tuple.Cdr()
			if !values.IsEmptyList(cdr) {
				tuple2, ok := cdr.(values.Tuple)
				if ok && !tuple2.IsEmptyList() {
					endVal, ok := tuple2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy: end must be an integer but got %T", tuple2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(*bv)) {
		return values.NewForeignError("bytevector-copy: start index out of bounds")
	}
	if end < start || end > int64(len(*bv)) {
		return values.NewForeignError("bytevector-copy: end index out of bounds")
	}

	result := make(values.ByteVector, end-start)
	copy(result, (*bv)[start:end])
	mc.SetValue(&result)
	return nil
}

// PrimBytevectorCopyBang implements the bytevector-copy! primitive.
// Copies bytes between bytevectors.
func PrimBytevectorCopyBang(_ context.Context, mc *machine.MachineContext) error {
	to := mc.Arg(0)
	at := mc.Arg(1)
	from := mc.Arg(2)
	rest := mc.Arg(3)

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
		tuple, ok := rest.(values.Tuple)
		if ok && !tuple.IsEmptyList() {
			startVal, ok := tuple.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy!: start must be an integer but got %T", tuple.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := tuple.Cdr()
			if !values.IsEmptyList(cdr) {
				tuple2, ok := cdr.(values.Tuple)
				if ok && !tuple2.IsEmptyList() {
					endVal, ok := tuple2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "bytevector-copy!: end must be an integer but got %T", tuple2.Car())
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
	o := mc.Arg(0)
	rest := mc.Arg(1)
	bv, ok := o.(*values.ByteVector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAByteVector, "utf8->string: expected a bytevector but got %T", o)
	}

	start := int64(0)
	end := int64(len(*bv))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if ok && !tuple.IsEmptyList() {
			startVal, ok := tuple.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "utf8->string: start must be an integer but got %T", tuple.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := tuple.Cdr()
			if !values.IsEmptyList(cdr) {
				tuple2, ok := cdr.(values.Tuple)
				if ok && !tuple2.IsEmptyList() {
					endVal, ok := tuple2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "utf8->string: end must be an integer but got %T", tuple2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(*bv)) {
		return values.NewForeignError("utf8->string: start index out of bounds")
	}
	if end < start || end > int64(len(*bv)) {
		return values.NewForeignError("utf8->string: end index out of bounds")
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
	o := mc.Arg(0)
	rest := mc.Arg(1)
	str, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->utf8: expected a string but got %T", o)
	}

	s := str.Value
	start := int64(0)
	end := int64(len(s))

	// Parse optional start and end arguments
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if ok && !tuple.IsEmptyList() {
			startVal, ok := tuple.Car().(*values.Integer)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAnInteger, "string->utf8: start must be an integer but got %T", tuple.Car())
			}
			start = startVal.Value

			// Check for end argument
			cdr := tuple.Cdr()
			if !values.IsEmptyList(cdr) {
				tuple2, ok := cdr.(values.Tuple)
				if ok && !tuple2.IsEmptyList() {
					endVal, ok := tuple2.Car().(*values.Integer)
					if !ok {
						return values.WrapForeignErrorf(values.ErrNotAnInteger, "string->utf8: end must be an integer but got %T", tuple2.Car())
					}
					end = endVal.Value
				}
			}
		}
	}

	if start < 0 || start > int64(len(s)) {
		return values.NewForeignError("string->utf8: start index out of bounds")
	}
	if end < start || end > int64(len(s)) {
		return values.NewForeignError("string->utf8: end index out of bounds")
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
