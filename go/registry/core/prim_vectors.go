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

	"wile/machine"
	"wile/registry/helpers"
	"wile/values"
)

// PrimMakeVector implements the (make-vector) primitive.
// Creates a vector of the given size, optionally filled with a specified value.
func PrimMakeVector(_ context.Context, mc *machine.MachineContext) error {
	k := mc.Arg(0)
	rest := mc.Arg(1)
	size, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-vector: expected an integer but got %T", k)
	}
	if size.Value < 0 {
		return values.NewForeignError("make-vector: size must be non-negative")
	}
	var fill values.Value = values.FalseValue
	if !values.IsEmptyList(rest) {
		pr, ok := rest.(*values.Pair)
		if ok && !values.IsEmptyList(pr) {
			fill = pr.Car()
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
	o := mc.Arg(0)
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector-length: expected a vector but got %T", o)
	}
	mc.SetValue(values.NewInteger(int64(len(*v))))
	return nil
}

// PrimVectorRef implements the vector-ref primitive.
// Returns the element of a vector at the given index.
func PrimVectorRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector-ref: expected a vector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "vector-ref: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*v)) {
		return values.NewForeignError("vector-ref: index out of bounds")
	}
	mc.SetValue((*v)[idx.Value])
	return nil
}

// PrimVectorSet implements the vector-set! primitive.
// Sets the element of a vector at the given index to a new value.
func PrimVectorSet(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	obj := mc.Arg(2)
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector-set!: expected a vector but got %T", o)
	}
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "vector-set!: expected an integer but got %T", k)
	}
	if idx.Value < 0 || idx.Value >= int64(len(*v)) {
		return values.NewForeignError("vector-set!: index out of bounds")
	}
	(*v)[idx.Value] = obj
	mc.SetValues()
	return nil
}

// PrimVectorToList implements the vector->list primitive.
// Converts a vector to a list with the same elements in the same order.
func PrimVectorToList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	v, ok := o.(*values.Vector)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAVector, "vector->list: expected a vector but got %T", o)
	}
	var result values.Value = values.EmptyList
	for i := len(*v) - 1; i >= 0; i-- {
		result = values.NewCons((*v)[i], result)
	}
	mc.SetValue(result)
	return nil
}

// PrimListToVector implements the list->vector primitive.
func PrimListToVector(_ context.Context, mc *machine.MachineContext) error {
	return helpers.ListToVector(mc, "list->vector")
}
