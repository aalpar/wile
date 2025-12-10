package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimLength implements the (length) primitive.
// Returns the length of a proper list.
func PrimLength(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewInteger(0))
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "length: expected a list but got %T", o)
	}
	count := int64(0)
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
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
