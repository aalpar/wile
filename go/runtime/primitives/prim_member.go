package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimMember implements the member primitive.
// Finds an element in a list using equal? for comparison.
func PrimMember(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	lst := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	for !values.IsEmptyList(lst) {
		pr, ok := lst.(*values.Pair)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotAList, "member: expected a list but got %T", lst)
		}
		if values.EqualTo(pr.Car(), obj) {
			mc.SetValue(pr)
			return nil
		}
		lst = pr.Cdr()
	}
	mc.SetValue(values.FalseValue)
	return nil
}
