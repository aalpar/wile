package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimMemq implements the memq primitive.
// Finds an element in a list using eq? for comparison.
func PrimMemq(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	lst := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(*values.Pair)
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
