package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimListRef implements the (list-ref) primitive.
// Returns the element at the given index in a list.
func PrimListRef(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	k := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	idx, ok := k.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "list-ref: expected an integer index but got %T", k)
	}
	if idx.Value < 0 {
		return values.NewForeignError("list-ref: index must be non-negative")
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "list-ref: expected a pair but got %T", o)
	}
	for i := int64(0); i < idx.Value; i++ {
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
