package primitives

import (
	"context"
	"errors"

	"skeme/machine"
	"skeme/values"
)

// PrimAssv implements the assv primitive.
// Finds pair in alist by key using eqv?
func PrimAssv(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	alist := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := alist.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "assv: expected a list but got %T", alist)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, elem values.Value) error {
		entry, ok := elem.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "assv: expected a pair in alist but got %T", elem)
		}
		if Eqv(entry.Car(), obj) {
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
		return values.WrapForeignErrorf(values.ErrNotAList, "assv: expected a proper list")
	}
	mc.SetValue(values.FalseValue)
	return nil
}
