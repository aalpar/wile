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
	"slices"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimList implements the (list) primitive.
// Creates a list from the given arguments.
//
// The rest-arg list may be backed by a reusable buffer (restArgBuf),
// so we must copy the spine to produce a persistent list.
func PrimList(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.EmptyList)
		return nil
	}
	var elems []values.Value
	_, err := values.ForEach(mc.Context(), o, func(_ context.Context, _ int, _ bool, v values.Value) error {
		elems = append(elems, v)
		return nil
	})
	if err != nil {
		return err
	}
	mc.SetValue(values.List(elems...))
	return nil
}

// PrimMakeList implements the Scheme make-list primitive.
func PrimMakeList(mc machine.CallContext) error {
	k, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "make-list")
	if err != nil {
		return err
	}
	count := int(k.Value)
	if count < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "make-list: k must be non-negative")
	}
	fill := values.Value(values.FalseValue)
	v, ok, err := helpers.ParseOptionalArg(mc.Arg(1), "make-list")
	if err != nil {
		return err
	}
	if ok {
		fill = v
	}

	elems := make([]values.Value, count)
	for i := range elems {
		elems[i] = fill
	}
	mc.SetValue(values.List(elems...))
	return nil
}

// PrimAppend implements (append list ...) per R7RS §6.4.
// Returns a list consisting of the elements of the first list followed by
// the elements of the other lists. The last argument may be any object.
// Benchmarked: kept in Go — Scheme impl is 4-9x slower on short lists
// (benchmark gate: 20% threshold; actual regression was ~363% for Append).
func PrimAppend(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.EmptyList)
		return nil
	}
	args, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "append: expected a list but got %T", o)
	}

	var lists values.Vector
	err := helpers.ForEachList(mc.Context(), args, "append", func(_ context.Context, _ int, _ bool, elem values.Value) error {
		lists = append(lists, elem)
		return nil
	})
	if err != nil {
		return err
	}
	var result values.Value = values.EmptyList
	for i := range slices.Backward(lists) {
		lst := lists[i]
		if i == len(lists)-1 {
			result = lst
			continue
		}
		if values.IsEmptyList(lst) {
			continue
		}
		pr, ok := lst.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "append: expected list but got %T", lst)
		}
		var elems values.Vector
		err = helpers.ForEachList(mc.Context(), pr, "append", func(_ context.Context, _ int, _ bool, elem values.Value) error {
			elems = append(elems, elem)
			return nil
		})
		if err != nil {
			return err
		}
		for j := range slices.Backward(elems) {
			result = values.NewCons(elems[j], result)
		}
	}
	mc.SetValue(result)
	return nil
}

// PrimReverse implements the (reverse) primitive.
// Benchmarked: kept in Go — Scheme impl is 7x slower on short lists.
func PrimReverse(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.EmptyList)
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "reverse: expected a list but got %T", o)
	}
	var result values.Value = values.EmptyList
	err := helpers.ForEachList(mc.Context(), pr, "reverse", func(_ context.Context, _ int, _ bool, v values.Value) error {
		result = values.NewCons(v, result)
		return nil
	})
	if err != nil {
		return err
	}
	mc.SetValue(result)
	return nil
}

// PrimLength implements the (length) primitive.
// Benchmarked: kept in Go — Scheme impl is 9x slower on short lists.
func PrimLength(mc machine.CallContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "length: expected a list but got %T", o)
	}
	count := int64(0)
	err := helpers.ForEachList(mc.Context(), pr, "length", func(_ context.Context, _ int, _ bool, _ values.Value) error {
		count++
		return nil
	})
	if err != nil {
		return err
	}
	mc.SetValue(values.NewInteger(count))
	return nil
}

// PrimListRef implements the (list-ref) primitive.
// Returns the element at the given index in a list.
// R7RS §6.4: The index must be an exact non-negative integer.
func PrimListRef(mc machine.CallContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "list-ref: expected an exact integer index but got %T", k)
	}
	if idx < 0 {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-ref: index must be non-negative")
	}
	if values.IsEmptyList(o) {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-ref: index out of bounds for empty list")
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "list-ref: expected a list but got %T", o)
	}
	for range idx {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-ref: index out of bounds")
		}
		pr, ok = next.(values.Tuple)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAList, "list-ref: expected a list but got %T", next)
		}
	}
	mc.SetValue(pr.Car())
	return nil
}

// PrimListSet implements the Scheme list-set! primitive.
// R7RS §6.4: The index must be an exact non-negative integer.
func PrimListSet(mc machine.CallContext) error {
	p, err := helpers.RequireArg[*values.Pair](mc, 0, werr.ErrNotAList, "list-set!")
	if err != nil {
		return err
	}
	idxVal := mc.Arg(1)
	val := mc.Arg(2)

	idx, ok := values.ExactInteger(idxVal)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "list-set!: expected an exact integer index but got %T", idxVal)
	}
	k := int(idx)
	if k < 0 {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-set!: index must be non-negative")
	}

	current := p
	for range k {
		cdr := current.Cdr()
		next, ok := cdr.(*values.Pair)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-set!: index out of range")
		}
		current = next
	}

	current.SetCar(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimListTail implements the (list-tail) primitive.
// Benchmarked: kept in Go — Scheme impl is 6x slower on short lists.
func PrimListTail(mc machine.CallContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "list-tail: expected an exact integer index but got %T", k)
	}
	if idx < 0 {
		return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-tail: index must be non-negative")
	}
	if idx == 0 {
		mc.SetValue(o)
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "list-tail: expected a list but got %T", o)
	}
	for i := range idx {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			if i == idx-1 {
				mc.SetValue(values.EmptyList)
				return nil
			}
			return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange, "list-tail: index out of bounds")
		}
		pr, ok = next.(values.Tuple)
		if !ok {
			if i == idx-1 {
				mc.SetValue(next)
				return nil
			}
			return werr.WrapForeignErrorf(werr.ErrNotAList, "list-tail: expected a list but got %T", next)
		}
	}
	mc.SetValue(pr)
	return nil
}

// PrimListCopy implements the list-copy primitive.
// Benchmarked: kept in Go — Scheme impl is 7x slower on short lists.
func PrimListCopy(mc machine.CallContext) error {
	obj := mc.Arg(0)
	if values.IsEmptyList(obj) {
		mc.SetValue(values.EmptyList)
		return nil
	}
	pr, ok := obj.(values.Tuple)
	if !ok {
		mc.SetValue(obj)
		return nil
	}
	var head, tail *values.Pair
	current := values.Value(pr)
	for {
		p, ok := current.(values.Tuple)
		if !ok {
			if tail != nil {
				tail.SetCdr(current)
			}
			break
		}
		newPair := values.NewCons(p.Car(), values.EmptyList)
		if head == nil {
			head = newPair
		} else {
			tail.SetCdr(newPair)
		}
		tail = newPair
		cdr := p.Cdr()
		if values.IsEmptyList(cdr) {
			break
		}
		current = cdr
	}
	if head == nil {
		mc.SetValue(values.EmptyList)
	} else {
		mc.SetValue(head)
	}
	return nil
}

// PrimMemq implements the memq primitive.
// Finds an element in a list using eq? for comparison.
func PrimMemq(mc machine.CallContext) error {
	return helpers.MemberLookup(mc, "memq", helpers.EqIdentity)
}

// PrimMemv implements the memv primitive.
// Finds an element in a list using eqv? for comparison.
func PrimMemv(mc machine.CallContext) error {
	return helpers.MemberLookup(mc, "memv", helpers.Eqv)
}

// PrimAssq implements the assq primitive.
func PrimAssq(mc machine.CallContext) error {
	return helpers.AssocLookup(mc, "assq", helpers.EqIdentity)
}

// PrimAssv implements the assv primitive.
func PrimAssv(mc machine.CallContext) error {
	return helpers.AssocLookup(mc, "assv", helpers.Eqv)
}
