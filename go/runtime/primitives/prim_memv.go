package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimMemv implements the memv primitive.
// Finds an element in a list using eqv? for comparison.
func PrimMemv(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	lst := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "memv: expected a list but got %T", lst)
		}
		if Eqv(pr.Car(), obj) {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}
