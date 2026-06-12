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
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimMakeVector implements the (make-vector) primitive.
// Creates a vector of the given size, optionally filled with a specified value.
func PrimMakeVector(mc machine.CallContext) error {
	size, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "make-vector")
	if err != nil {
		return err
	}
	err = helpers.ValidateMakeLength(size.Value, "make-vector")
	if err != nil {
		return err
	}
	var fill values.Value = values.FalseValue
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(1), "make-vector")
	if err != nil {
		return err
	}
	if ok {
		fill = v
	}
	elems := make(values.Vector, size.Value)
	for i := range elems {
		elems[i] = fill
	}
	mc.SetValue(values.NewVector(elems...))
	return nil
}

// PrimVector implements the vector primitive.
func PrimVector(mc machine.CallContext) error {
	return helpers.ListToVector(mc, "vector")
}

// PrimVectorLength implements the vector-length primitive.
// Returns the number of elements in a vector as an integer.
func PrimVectorLength(mc machine.CallContext) error {
	return helpers.SequenceLength[*values.Vector](mc, werr.ErrNotAVector, "vector-length")
}

// PrimVectorRef implements the vector-ref primitive.
// Returns the element of a vector at the given index.
// R7RS §6.8: The index must be an exact non-negative integer.
func PrimVectorRef(mc machine.CallContext) error {
	return helpers.SequenceRef(mc, werr.ErrNotAVector, "vector-ref",
		(*values.Vector).Get,
	)
}

// PrimVectorSet implements the vector-set! primitive.
// Sets the element of a vector at the given index to a new value.
// R7RS §6.8: The index must be an exact non-negative integer.
func PrimVectorSet(mc machine.CallContext) error {
	return helpers.SequenceSet(mc, werr.ErrNotAVector, "vector-set!",
		func(v *values.Vector, idx int, mc machine.CallContext) error {
			set := mc.ImmutableLiterals()
			if set != nil && set.Contains(v) {
				return werr.WrapForeignErrorf(werr.ErrImmutableVector, "vector-set!: cannot mutate immutable literal vector")
			}
			return v.Set(idx, mc.Arg(2))
		},
	)
}

// PrimVectorToList implements the vector->list primitive.
// R7RS §6.8: (vector->list vector [start [end]])
// Converts a vector (or subvector) to a list with the same elements in the same order.
func PrimVectorToList(mc machine.CallContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, werr.ErrNotAVector, "vector->list")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseSubrange(rest, len(*v), "vector->list")
	if err != nil {
		return err
	}

	mc.SetValue(values.List((*v)[start:end]...))
	return nil
}

// PrimListToVector implements the list->vector primitive.
func PrimListToVector(mc machine.CallContext) error {
	return helpers.ListToVector(mc, "list->vector")
}

// PrimVectorCopy implements the vector-copy primitive.
// R7RS §6.8: (vector-copy vector [start [end]])
// Returns a newly allocated copy of the elements of vector between start and end.
func PrimVectorCopy(mc machine.CallContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, werr.ErrNotAVector, "vector-copy")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseSubrange(rest, len(*v), "vector-copy")
	if err != nil {
		return err
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
func PrimVectorCopyTo(mc machine.CallContext) error {
	to, err := helpers.RequireArg[*values.Vector](mc, 0, werr.ErrNotAVector, "vector-copy!")
	if err != nil {
		return err
	}
	at, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotAnInteger, "vector-copy!")
	if err != nil {
		return err
	}
	rest := mc.Arg(2)
	atIdx := int(at.Value)

	// Parse from vector and optional start/end from rest list
	if values.IsEmptyList(rest) {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments, "vector-copy!: missing from argument")
	}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "vector-copy!: improper argument list")
	}

	from, err := helpers.RequireType[*values.Vector](tuple.Car(), werr.ErrNotAVector, "vector-copy!")
	if err != nil {
		return err
	}

	start, end, err := helpers.ParseSubrange(tuple.Cdr(), len(*from), "vector-copy!")
	if err != nil {
		return err
	}
	if atIdx < 0 || atIdx+(end-start) > len(*to) {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "vector-copy!: invalid destination index")
	}

	// Copy elements
	copy((*to)[atIdx:], (*from)[start:end])
	mc.SetValue(values.Void)
	return nil
}

// PrimVectorFill implements the vector-fill! primitive.
// R7RS §6.8: (vector-fill! vector fill [start [end]])
// Sets the elements of vector between start and end to fill.
func PrimVectorFill(mc machine.CallContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, werr.ErrNotAVector, "vector-fill!")
	if err != nil {
		return err
	}
	set := mc.ImmutableLiterals()
	if set != nil && set.Contains(v) {
		return werr.WrapForeignErrorf(werr.ErrImmutableVector, "vector-fill!: cannot mutate immutable literal vector")
	}
	fillArg := mc.Arg(1)
	rest := mc.Arg(2)

	start, end, err := helpers.ParseSubrange(rest, len(*v), "vector-fill!")
	if err != nil {
		return err
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
func PrimVectorAppend(mc machine.CallContext) error {
	vectors, _, err := helpers.CollectVectors(mc.Arg(0), "vector-append")
	if err != nil {
		return err
	}

	// Create the result vector
	totalLen := 0
	for _, v := range vectors {
		totalLen += len(*v)
	}
	elems := make(values.Vector, totalLen)
	idx := 0
	for _, v := range vectors {
		copy(elems[idx:], *v)
		idx += len(*v)
	}

	mc.SetValue(values.NewVector(elems...))
	return nil
}

// PrimVectorToString implements the vector->string primitive.
// R7RS §6.8: (vector->string vector [start [end]])
// Returns a string constructed from the characters in vector between start and end.
func PrimVectorToString(mc machine.CallContext) error {
	v, err := helpers.RequireArg[*values.Vector](mc, 0, werr.ErrNotAVector, "vector->string")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	start, end, err := helpers.ParseSubrange(rest, len(*v), "vector->string")
	if err != nil {
		return err
	}

	// Convert characters to string
	runes := make([]rune, end-start)
	for i := start; i < end; i++ {
		ch, err := helpers.RequireType[*values.Character]((*v)[i], werr.ErrNotACharacter, "vector->string")
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
func PrimStringToVector(mc machine.CallContext) error {
	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "string->vector")
	if err != nil {
		return err
	}
	rest := mc.Arg(1)

	runes := str.Runes()

	start, end, err := helpers.ParseSubrange(rest, len(runes), "string->vector")
	if err != nil {
		return err
	}

	// Create vector of characters
	elems := make(values.Vector, end-start)
	for i := start; i < end; i++ {
		elems[i-start] = values.NewCharacter(runes[i])
	}

	mc.SetValue(values.NewVector(elems...))
	return nil
}
