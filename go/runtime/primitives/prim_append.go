package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

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
func PrimAppend(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	args, ok := o.(*values.Pair)
	if !ok {
		if values.IsEmptyList(o) {
			mc.SetValue(values.EmptyList)
			return nil
		}
		return values.WrapForeignErrorf(values.ErrNotAPair, "append: expected a list but got %T", o)
	}

	// Collect all argument lists into a vector for random access (right-to-left processing)
	var lists values.Vector
	v, err := args.ForEach(func(i int, hasNext bool, elem values.Value) error {
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
		pr, ok := lst.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "append: expected list but got %T", lst)
		}
		// Collect list elements into a vector for reverse-order access.
		// We use a vector because lists only support forward traversal, but we need
		// to prepend elements in reverse order to preserve the original sequence.
		// E.g., for list (a b c), we collect [a, b, c], then prepend c, b, a
		// to result, yielding (a b c . result).
		var elems values.Vector
		v, err = pr.ForEach(func(i int, hasNext bool, elem values.Value) error {
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
