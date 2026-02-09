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

	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
)

// PrimList implements the (list) primitive.
// Creates a list from the given arguments.
func PrimList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	// The variadic args come as a list - just return them
	mc.SetValue(o)
	return nil
}

// PrimMakeList implements the Scheme make-list primitive.
func PrimMakeList(_ context.Context, mc *machine.MachineContext) error {
	kVal := mc.Arg(0)
	restVal := mc.Arg(1)

	k, ok := kVal.(*values.Integer)
	if !ok {
		return values.NewForeignError("make-list: expected an integer for k")
	}
	count := int(k.Value)
	if count < 0 {
		return values.NewForeignError("make-list: k must be non-negative")
	}

	// Default fill value is unspecified; we use #f
	fill := values.Value(values.FalseValue)

	// Check for optional fill argument
	if !values.IsEmptyList(restVal) {
		if rest, ok := restVal.(values.Tuple); ok { //nolint:gocritic
			fill = rest.Car()
		}
	}

	// Build list from tail to head
	result := values.Value(values.EmptyList)
	for i := 0; i < count; i++ {
		result = values.NewCons(fill, result)
	}

	mc.SetValue(result)
	return nil
}

// PrimAppend implements (append list ...) per R7RS.
// Returns a list consisting of the elements of the first list followed by
// the elements of the other lists. The last argument may be any object.
//
// Algorithm overview:
//  1. Collect all argument lists into a vector for random access
//  2. Build result from right to left, starting with the last argument as the tail
//  3. For each preceding list, collect its elements into a vector, then prepend
//     them to the result in reverse order to preserve original ordering
//
// Why use a vector for intermediate storage (lines 1276-1290)?
// Lists are singly-linked and can only be efficiently traversed forward.
// To prepend list elements while preserving order, we need to process them
// in reverse. We collect elements into a vector (O(1) append), then iterate
// backward through the vector to prepend each element to the result.
// This achieves O(n) time complexity where n is total elements across all lists.
//
// Example: (append '(a b) '(c d) '(e))
// - lists vector: ['(a b), '(c d), '(e)]
// - Start with result = '(e) (last element)
// - Process '(c d): collect [c, d], prepend d then c → result = '(c d e)
// - Process '(a b): collect [a, b], prepend b then a → result = '(a b c d e)
func PrimAppend(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	args, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "append: expected a list but got %T", o)
	}

	// Collect all argument lists into a vector for random access (right-to-left processing)
	var lists values.Vector
	v, err := args.ForEach(ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		lists = append(lists, elem)
		return nil
	})
	if err != nil {
		return values.WrapForeignErrorf(err, "append: error processing arguments: %s", args.SchemeString())
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "append: expected proper list of arguments")
	}
	if len(lists) == 0 {
		mc.SetValue(values.EmptyList)
		return nil
	}
	// Build result from right to left
	var result values.Value = values.EmptyList
	for i := len(lists) - 1; i >= 0; i-- {
		lst := lists[i]
		if i == len(lists)-1 {
			// Last element can be any value (for improper lists)
			result = lst
			continue
		}
		// Prepend elements of this list to result
		if values.IsEmptyList(lst) {
			continue
		}
		pr, ok := lst.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "append: expected list but got %T", lst)
		}
		// Collect list elements into a vector for reverse-order access.
		// We use a vector because lists only support forward traversal, but we need
		// to prepend elements in reverse order to preserve the original sequence.
		// E.g., for list (a b c), we collect [a, b, c], then prepend c, b, a
		// to result, yielding (a b c . result).
		var elems values.Vector
		v, err = pr.ForEach(ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
			elems = append(elems, elem)
			return nil
		})
		if err != nil {
			return values.WrapForeignErrorf(err, "append: error processing list: %s", pr.SchemeString())
		}
		if !values.IsEmptyList(v) {
			return values.WrapForeignErrorf(values.ErrNotAList, "append: expected proper list but got improper list")
		}
		// Prepend elements in reverse order: iterate backward through vector,
		// consing each element onto result. This reconstructs the original order.
		for j := len(elems) - 1; j >= 0; j-- {
			result = values.NewCons(elems[j], result)
		}
	}
	mc.SetValue(result)
	return nil
}

// PrimReverse implements the (reverse) primitive.
// Returns reversed copy of list.
func PrimReverse(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.EmptyList)
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "reverse: expected a list but got %T", o)
	}
	var result values.Value = values.EmptyList
	v, err := pr.ForEach(ctx, func(_ context.Context, _ int, _ bool, v values.Value) error {
		result = values.NewCons(v, result)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "reverse: expected a proper list")
	}
	mc.SetValue(result)
	return nil
}

// PrimLength implements the (length) primitive.
// Returns the length of a proper list.
func PrimLength(ctx context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	pr, ok := o.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "length: expected a list but got %T", o)
	}
	count := int64(0)
	v, err := pr.ForEach(ctx, func(_ context.Context, _ int, _ bool, _ values.Value) error {
		count++
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "length: expected a proper list")
	}
	mc.SetValue(values.NewInteger(count))
	return nil
}

// PrimListRef implements the (list-ref) primitive.
// Returns the element at the given index in a list.
// R7RS §6.4: The index must be an exact non-negative integer.
func PrimListRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-ref: expected an exact integer index but got %T", k)
	}
	if idx < 0 {
		return values.NewForeignError("list-ref: index must be non-negative")
	}
	if values.IsEmptyList(o) {
		return values.NewForeignError("list-ref: index out of bounds for empty list")
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "list-ref: expected a pair but got %T", o)
	}
	for i := int64(0); i < idx; i++ {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			return values.NewForeignError("list-ref: index out of bounds")
		}
		pr, ok = next.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "list-ref: expected a pair but got %T", next)
		}
	}
	mc.SetValue(pr.Car())
	return nil
}

// PrimListSet implements the Scheme list-set! primitive.
// R7RS §6.4: The index must be an exact non-negative integer.
func PrimListSet(_ context.Context, mc *machine.MachineContext) error {
	listVal := mc.Arg(0)
	idxVal := mc.Arg(1)
	val := mc.Arg(2)

	p, ok := listVal.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "list-set!: expected a list but got %T", listVal)
	}

	idx, ok := values.ExactInteger(idxVal)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-set!: expected an exact integer index but got %T", idxVal)
	}
	k := int(idx)
	if k < 0 {
		return values.NewForeignError("list-set!: index must be non-negative")
	}

	current := p
	for i := 0; i < k; i++ {
		cdr := current.Cdr()
		next, ok := cdr.(*values.Pair)
		if !ok {
			return values.NewForeignError("list-set!: index out of range")
		}
		current = next
	}

	current.SetCar(val)
	mc.SetValue(values.Void)
	return nil
}

// PrimListTail implements the (list-tail) primitive.
// Returns the sublist starting at the given index.
// R7RS §6.4: The index must be an exact non-negative integer.
func PrimListTail(_ context.Context, mc *machine.MachineContext) error {
	o := mc.Arg(0)
	k := mc.Arg(1)
	idx, ok := values.ExactInteger(k)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-tail: expected an exact integer index but got %T", k)
	}
	if idx < 0 {
		return values.NewForeignError("list-tail: index must be non-negative")
	}
	if idx == 0 {
		mc.SetValue(o)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "list-tail: expected a pair but got %T", o)
	}
	for i := int64(0); i < idx; i++ {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			if i == idx-1 {
				mc.SetValue(values.EmptyList)
				return nil
			}
			return values.NewForeignError("list-tail: index out of bounds")
		}
		pr, ok = next.(*values.Pair)
		if !ok {
			if i == idx-1 {
				mc.SetValue(next)
				return nil
			}
			return values.WrapForeignErrorf(values.ErrNotAPair, "list-tail: expected a pair but got %T", next)
		}
	}
	mc.SetValue(pr)
	return nil
}

// PrimMemq implements the memq primitive.
// Finds an element in a list using eq? for comparison.
func PrimMemq(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	lst := mc.Arg(1)
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "memq: expected a list but got %T", lst)
		}
		if pr.Car() == obj {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// PrimMemv implements the memv primitive.
// Finds an element in a list using eqv? for comparison.
func PrimMemv(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	lst := mc.Arg(1)
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "memv: expected a list but got %T", lst)
		}
		if helpers.Eqv(pr.Car(), obj) {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// PrimMember implements the member primitive.
// R7RS §6.4: (member obj list [compare])
// Finds an element in a list using equal? for comparison, or a custom compare procedure.
func PrimMember(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	lst := mc.Arg(1)
	rest := mc.Arg(2)

	// Check for optional compare procedure
	var compareCls *machine.MachineClosure
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "member: improper argument list")
		}
		cmp, ok := tuple.Car().(*machine.MachineClosure)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAProcedure, "member: expected a procedure for compare but got %T", tuple.Car())
		}
		compareCls = cmp
	}

	// If no compare procedure, use equal?
	if compareCls == nil {
		for !values.IsEmptyList(lst) {
			pr, ok := lst.(values.Tuple)
			if !ok {
				return values.WrapForeignErrorf(values.ErrNotAList, "member: expected a list but got %T", lst)
			}
			if values.EqualTo(pr.Car(), obj) {
				mc.SetValue(pr)
				return nil
			}
			lst = pr.Cdr()
		}
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Use custom compare procedure
	sub := mc.NewSubContext()
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "member: expected a list but got %T", lst)
		}

		// Call compare procedure with (obj, element)
		_, err := sub.Apply(compareCls, obj, pr.Car())
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

		// If compare returns a true value (not #f), we found a match
		result := sub.GetValue()
		if schemeutil.ValueToBool(result) {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// PrimAssq implements the assq primitive.
func PrimAssq(_ context.Context, mc *machine.MachineContext) error {
	return helpers.AssocLookup(mc, "assq", func(a, b values.Value) bool { return a == b })
}

// PrimAssv implements the assv primitive.
func PrimAssv(_ context.Context, mc *machine.MachineContext) error {
	return helpers.AssocLookup(mc, "assv", helpers.Eqv)
}

// PrimAssoc implements the assoc primitive.
// R7RS §6.4: (assoc obj alist [compare])
// Finds an entry in an alist using equal? for comparison, or a custom compare procedure.
func PrimAssoc(ctx context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	alist := mc.Arg(1)
	rest := mc.Arg(2)

	// Check for optional compare procedure
	var compareCls *machine.MachineClosure
	if !values.IsEmptyList(rest) {
		tuple, ok := rest.(values.Tuple)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "assoc: improper argument list")
		}
		cmp, ok := tuple.Car().(*machine.MachineClosure)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAProcedure, "assoc: expected a procedure for compare but got %T", tuple.Car())
		}
		compareCls = cmp
	}

	// If no compare procedure, use equal?
	if compareCls == nil {
		return helpers.AssocLookup(mc, "assoc", values.EqualTo)
	}

	// Handle empty list
	if values.IsEmptyList(alist) {
		mc.SetValue(values.FalseValue)
		return nil
	}

	pr, ok := alist.(values.Tuple)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "assoc: expected a list but got %T", alist)
	}

	// Use custom compare procedure
	sub := mc.NewSubContext()
	v, err := pr.ForEach(ctx, func(_ context.Context, _ int, _ bool, elem values.Value) error {
		entry, ok := elem.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "assoc: expected a pair in alist but got %T", elem)
		}

		// Call compare procedure with (obj, car of entry)
		_, err := sub.Apply(compareCls, obj, entry.Car())
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

		// If compare returns a true value (not #f), we found a match
		result := sub.GetValue()
		if schemeutil.ValueToBool(result) {
			mc.SetValue(entry)
			return values.ErrStopIteration
		}
		return nil
	})
	if errors.Is(err, values.ErrStopIteration) {
		return nil
	}
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "assoc: expected a proper list")
	}
	mc.SetValue(values.FalseValue)
	return nil
}

// PrimListCopy implements the list-copy primitive.
// R7RS §6.4: (list-copy obj)
// Returns a newly allocated copy of obj if it is a list.
// Only the pairs are copied; the car elements are shared.
func PrimListCopy(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.Arg(0)

	// If not a pair, return as-is
	if values.IsEmptyList(obj) {
		mc.SetValue(values.EmptyList)
		return nil
	}

	pr, ok := obj.(*values.Pair)
	if !ok {
		// Not a list, return as-is per R7RS
		mc.SetValue(obj)
		return nil
	}

	// Copy the spine of the list
	var head, tail *values.Pair
	current := values.Value(pr)
	for {
		p, ok := current.(*values.Pair)
		if !ok {
			// Improper list ending - append the final cdr
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
