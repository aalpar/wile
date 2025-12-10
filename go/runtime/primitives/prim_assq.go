package primitives

import (
	"context"
	"errors"

	"wile/machine"
	"wile/values"
)

// PrimAssq implements the assq primitive.
// Finds pair in alist by key using eq?
func PrimAssq(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	alist := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := alist.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "assq: expected a list but got %T", alist)
	}
	v, err := pr.ForEach(func(i int, hasNext bool, elem values.Value) error {
		entry, ok := elem.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAPair, "assq: expected a pair in alist but got %T", pr.Car())
		}
		if entry.Car() == obj {
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
		return values.WrapForeignErrorf(values.ErrNotAList, "assq: expected a proper list")
	}
	mc.SetValue(values.FalseValue)
	return nil
}
