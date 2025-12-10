package primitives

import (
	"context"
	"errors"

	"skeme/machine"
	"skeme/values"
)

// PrimAssoc implements the assoc primitive.
// Finds pair in alist by key using equal?
func PrimAssoc(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	alist := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := alist.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "assoc: expected an alist but got %T", alist)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, elem values.Value) error {
		entry, ok := elem.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "assoc: expected a pair in alist but got %T", elem)
		}
		if values.EqualTo(entry.Car(), obj) {
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
