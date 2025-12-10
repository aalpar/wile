package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimReverse implements the (reverse) primitive.
// Returns reversed copy of list.
func PrimReverse(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.EmptyList)
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "reverse: expected a list but got %T", o)
	}
	var result values.Value = values.EmptyList
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
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
