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
	"errors"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimMakeVector implements the (make-vector) primitive.
// Creates a vector of the given size, optionally filled with a specified value.
func PrimMakeVector(_ context.Context, mc *machine.MachineContext) error {
	size, err := helpers.RequireArg[*values.Integer](mc, 0, values.ErrNotANumber, "make-vector")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)
	if size.Value < 0 {
		return values.NewForeignError("make-vector: size must be non-negative")
	}
	var fill values.Value = values.FalseValue
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if ok && !tuple.IsEmptyList() {
			fill = tuple.Car()
		}
	}
	elems := make(values.Vector, size.Value)
	for i := range elems {
		elems[i] = fill
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}

// PrimVector implements the vector primitive.
func PrimVector(_ context.Context, mc *machine.MachineContext) error {
	return helpers.ListToVector(mc, "vector")
}

// PrimVectorLength implements the vector-length primitive.
// Returns the number of elements in a vector as an integer.
func PrimVectorLength(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector-length")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewInteger(int64(len(*v))))
	return nil
}

// PrimVectorRef implements the vector-ref primitive.
// Returns the element of a vector at the given index.
// R7RS §6.8: The index must be an exact non-negative integer.
func PrimVectorRef(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector-ref")
	if err != nil {
		return err
	}
	k := mc.Arg(1)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "vector-ref: expected an exact integer but got %T", k)
	}
	if idx < 0 || idx >= int64(len(*v)) {
		return values.NewForeignError("vector-ref: index out of bounds")
	}
	mc.SetValue((*v)[idx])
	return nil
}

// PrimVectorSet implements the vector-set! primitive.
// Sets the element of a vector at the given index to a new value.
// R7RS §6.8: The index must be an exact non-negative integer.
func PrimVectorSet(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector-set!")
	if err != nil {
		return err
	}
	k := mc.Arg(1)
	obj := mc.Arg(2)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "vector-set!: expected an exact integer but got %T", k)
	}
	if idx < 0 || idx >= int64(len(*v)) {
		return values.NewForeignError("vector-set!: index out of bounds")
	}
	(*v)[idx] = obj
	mc.SetValue(values.Void)
	return nil
}

// PrimVectorToList implements the vector->list primitive.
// R7RS §6.8: (vector->list vector [start [end]])
// Converts a vector (or subvector) to a list with the same elements in the same order.
func PrimVectorToList(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector->list")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	length := len(*v)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "vector->list")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("vector->list: invalid indices")
	}

	var result values.Value = values.EmptyList
	for i := end - 1; i >= start; i-- {
		result = values.NewCons((*v)[i], result)
	}
	mc.SetValue(result)
	return nil
}

// PrimListToVector implements the list->vector primitive.
func PrimListToVector(_ context.Context, mc *machine.MachineContext) error {
	return helpers.ListToVector(mc, "list->vector")
}

// PrimVectorCopy implements the vector-copy primitive.
// R7RS §6.8: (vector-copy vector [start [end]])
// Returns a newly allocated copy of the elements of vector between start and end.
func PrimVectorCopy(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector-copy")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	length := len(*v)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "vector-copy")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("vector-copy: invalid indices")
	}

	// Create a new vector with the copied elements
	newLen := end - start
	elems := make(values.Vector, newLen)
	copy(elems, (*v)[start:end])
	mc.SetValue(values.NewVector(elems...))
	return nil
}

// PrimVectorCopyTo implements the vector-copy! primitive.
// R7RS §6.8: (vector-copy! to at from [start [end]])
// Copies elements from vector from to vector to, starting at index at in to.
func PrimVectorCopyTo(_ context.Context, mc *machine.MachineContext) error {
	to, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector-copy!")
	if err != nil {
		return err
	}
	at, err := helpers.RequireArg[*values.Integer](mc, 1, values.ErrNotANumber, "vector-copy!")
	if err != nil {
		return err
	}
	rest := mc.Arg(2)
	atIdx := int(at.Value)

	// Parse from vector and optional start/end from rest list
	if values.IsEmptyList(rest) {
		return values.NewForeignError("vector-copy!: missing from argument")
	}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "vector-copy!: improper argument list")
	}

	from, err := helpers.RequireType[*values.Vector](tuple.Car(), values.ErrNotAVector, "vector-copy!")
	if err != nil {
		return err
	}

	fromLen := len(*from)

	start64, end64, err := helpers.ParseOptionalStartEnd(tuple.Cdr(), int64(fromLen), "vector-copy!")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > fromLen || start > end {
		return values.NewForeignError("vector-copy!: invalid source indices")
	}
	if atIdx < 0 || atIdx+(end-start) > len(*to) {
		return values.NewForeignError("vector-copy!: invalid destination index")
	}

	// Copy elements
	copy((*to)[atIdx:], (*from)[start:end])
	mc.SetValue(values.Void)
	return nil
}

// PrimVectorFill implements the vector-fill! primitive.
// R7RS §6.8: (vector-fill! vector fill [start [end]])
// Sets the elements of vector between start and end to fill.
func PrimVectorFill(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector-fill!")
	if err != nil {
		return err
	}
	fillArg := mc.Arg(1)
	rest := mc.Arg(2)

	length := len(*v)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "vector-fill!")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("vector-fill!: invalid indices")
	}

	// Fill the elements
	for i := start; i < end; i++ {
		(*v)[i] = fillArg
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimVectorAppend implements the vector-append primitive.
// R7RS §6.8: (vector-append vector ...)
// Returns a newly allocated vector whose elements are the concatenation of the elements of the given vectors.
func PrimVectorAppend(_ context.Context, mc *machine.MachineContext) error {
	rest := mc.Arg(0)

	// Collect all vectors
	var vectors []*values.Vector
	totalLen := 0

	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "vector-append: improper argument list")
		}
		v, ok := tuple.Car().(*values.Vector)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAVector, "vector-append: expected a vector but got %T", tuple.Car())
		}
		vectors = append(vectors, v)
		totalLen += len(*v)
		current = tuple.Cdr()
	}

	// Create the result vector
	elems := make(values.Vector, totalLen)
	idx := 0
	for _, v := range vectors {
		copy(elems[idx:], *v)
		idx += len(*v)
	}

	mc.SetValue(values.NewVector(elems...))
	return nil
}

// PrimVectorMap implements the vector-map primitive.
// R7RS §6.8: (vector-map proc vector1 vector2 ...)
// Returns a new vector containing the results of applying proc element-wise to the vectors.
func PrimVectorMap(_ context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	rest := mc.Arg(1)

	mcls, err := helpers.RequireType[*machine.MachineClosure](proc, values.ErrNotAProcedure, "vector-map")
	if err != nil {
		return err
	}

	if values.IsEmptyList(rest) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "vector-map: expected at least one vector")
	}

	// Collect all vectors into a slice
	var vectors []*values.Vector
	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "vector-map: improper argument list")
		}
		v, err := helpers.RequireType[*values.Vector](tuple.Car(), values.ErrNotAVector, "vector-map")
		if err != nil {
			return err
		}
		vectors = append(vectors, v)
		current = tuple.Cdr()
	}

	// Find the minimum length
	minLen := len(*vectors[0])
	for _, v := range vectors[1:] {
		if len(*v) < minLen {
			minLen = len(*v)
		}
	}

	// Apply the procedure to each set of elements
	results := make(values.Vector, minLen)
	sub := mc.NewSubContext()
	for i := 0; i < minLen; i++ {
		args := make(values.Vector, len(vectors))
		for j, v := range vectors {
			args[j] = (*v)[i]
		}

		_, err := sub.Apply(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			var escapeErr *machine.ErrContinuationEscape
			if errors.As(err, &escapeErr) {
				return err
			}
			if !errors.Is(err, machine.ErrMachineHalt) {
				return err
			}
		}
		results[i] = sub.GetValue()
	}

	mc.SetValue(values.NewVector(results...))
	return nil
}

// PrimVectorForEach implements the vector-for-each primitive.
// R7RS §6.8: (vector-for-each proc vector1 vector2 ...)
// Applies proc element-wise to the vectors for side effects.
func PrimVectorForEach(_ context.Context, mc *machine.MachineContext) error {
	proc := mc.Arg(0)
	rest := mc.Arg(1)

	mcls, err := helpers.RequireType[*machine.MachineClosure](proc, values.ErrNotAProcedure, "vector-for-each")
	if err != nil {
		return err
	}

	if values.IsEmptyList(rest) {
		return values.WrapForeignErrorf(values.ErrWrongNumberOfArguments, "vector-for-each: expected at least one vector")
	}

	// Collect all vectors into a slice
	var vectors []*values.Vector
	current := rest
	for !values.IsEmptyList(current) {
		tuple, ok := current.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "vector-for-each: improper argument list")
		}
		v, err := helpers.RequireType[*values.Vector](tuple.Car(), values.ErrNotAVector, "vector-for-each")
		if err != nil {
			return err
		}
		vectors = append(vectors, v)
		current = tuple.Cdr()
	}

	// Find the minimum length
	minLen := len(*vectors[0])
	for _, v := range vectors[1:] {
		if len(*v) < minLen {
			minLen = len(*v)
		}
	}

	// Apply the procedure to each set of elements
	sub := mc.NewSubContext()
	for i := 0; i < minLen; i++ {
		args := make(values.Vector, len(vectors))
		for j, v := range vectors {
			args[j] = (*v)[i]
		}

		_, err := sub.Apply(mcls, args...)
		if err != nil {
			return err
		}
		err = sub.Run()
		if err != nil {
			var escapeErr *machine.ErrContinuationEscape
			if errors.As(err, &escapeErr) {
				return err
			}
			if !errors.Is(err, machine.ErrMachineHalt) {
				return err
			}
		}
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimVectorToString implements the vector->string primitive.
// R7RS §6.8: (vector->string vector [start [end]])
// Returns a string constructed from the characters in vector between start and end.
func PrimVectorToString(_ context.Context, mc *machine.MachineContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, values.ErrNotAVector, "vector->string")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	length := len(*v)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "vector->string")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("vector->string: invalid indices")
	}

	// Convert characters to string
	runes := make([]rune, end-start)
	for i := start; i < end; i++ {
		ch, err := helpers.RequireType[*values.Character]((*v)[i], values.ErrNotACharacter, "vector->string")
		if err != nil {
			return err
		}
		runes[i-start] = ch.Value
	}

	mc.SetValue(values.NewMutableString(string(runes)))
	return nil
}

// PrimStringToVector implements the string->vector primitive.
// R7RS §6.8: (string->vector string [start [end]])
// Returns a vector containing the characters of string between start and end.
func PrimStringToVector(_ context.Context, mc *machine.MachineContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, values.ErrNotAString, "string->vector")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	runes := str.Runes()
	length := len(runes)

	start64, end64, err := helpers.ParseOptionalStartEnd(rest, int64(length), "string->vector")
	if err != nil {
		return err
	}
	start, end := int(start64), int(end64)

	// Validate indices
	if start < 0 || end > length || start > end {
		return values.NewForeignError("string->vector: invalid indices")
	}

	// Create vector of characters
	elems := make(values.Vector, end-start)
	for i := start; i < end; i++ {
		elems[i-start] = values.NewCharacter(runes[i])
	}

	mc.SetValue(values.NewVector(elems...))
	return nil
}
