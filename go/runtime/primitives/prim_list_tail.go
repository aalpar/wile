package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimListTail implements the (list-tail) primitive.
// Returns the sublist starting at the given index.
func PrimListTail(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-tail: expected an integer index but got %T", k)
	}
	if idx.Value < 0 {
		return values.NewForeignError("list-tail: index must be non-negative")
	}
	if idx.Value == 0 {
		mc.SetValue(o)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "list-tail: expected a pair but got %T", o)
	}
	for i := int64(0); i < idx.Value; i++ {
		next := pr.Cdr()
		if values.IsEmptyList(next) {
			if i == idx.Value-1 {
				mc.SetValue(values.EmptyList)
				return nil
			}
			return values.NewForeignError("list-tail: index out of bounds")
		}
		pr, ok = next.(*values.Pair)
		if !ok {
			if i == idx.Value-1 {
				mc.SetValue(next)
				return nil
			}
			return values.WrapForeignErrorf(values.ErrNotAPair, "list-tail: expected a pair but got %T", next)
		}
	}
	mc.SetValue(pr)
	return nil
}
