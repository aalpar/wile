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

// numericFoldVariadic is a helper for variadic arithmetic operations (+ and *).
// It takes a rest parameter at index 0 and folds with the binary operation.
// Returns identity for empty list, first arg for single element.
func numericFoldVariadic(
	mc *machine.MachineContext,
	name string,
	identity values.Number,
	binOp func(acc, val values.Number) values.Number,
) error {
	o := mc.Arg(0)
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, o)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(identity)
		return nil
	}
	o = pr.Car()
	nbr, ok := o.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
	}
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(nbr)
		return nil
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
		}
		nbr = binOp(nbr, v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "%s: error processing arguments", name)
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %s", name, v.SchemeString())
	}
	mc.SetValue(nbr)
	return nil
}

// numericFoldWithFirst is a helper for arithmetic operations with required first arg (- and /).
// First arg at index 0, rest at index 1. Applies unaryOp for single arg case.
func numericFoldWithFirst(
	mc *machine.MachineContext,
	name string,
	unaryOp func(val values.Number) values.Number,
	binOp func(acc, val values.Number) values.Number,
) error {
	o0 := mc.Arg(0)
	nbr0, ok := o0.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o0)
	}
	o1 := mc.Arg(1)
	pr, ok := o1.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", name, o1)
	}
	if values.IsEmptyList(pr) {
		mc.SetValue(unaryOp(nbr0))
		return nil
	}
	o2 := pr.Car()
	nbr2, ok := o2.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o2)
	}
	acc := binOp(nbr0, nbr2)
	pr, ok = pr.Cdr().(*values.Pair)
	if !ok {
		mc.SetValue(acc)
		return nil
	}
	v, err := pr.ForEach(context.TODO(), func(_ context.Context, _ int, _ bool, o values.Value) error {
		v, ok := o.(values.Number)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected a number but got %T", name, o)
		}
		acc = binOp(acc, v)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "%s: error processing arguments", name)
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "%s: expected a list but got %s", name, v.SchemeString())
	}
	mc.SetValue(acc)
	return nil
}
