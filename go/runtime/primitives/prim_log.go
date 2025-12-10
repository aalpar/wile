package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/values"
)

// PrimLog implements the (log) primitive.
// Returns the natural logarithm of a number, optionally with a specified base.
func PrimLog(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	rest := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	x, err := ToFloat64(o)
	if err != nil {
		return values.WrapForeignErrorf(err, "log: %v", err)
	}
	if rest == values.EmptyList {
		mc.SetValue(values.NewFloat(math.Log(x)))
	} else {
		baseArg, ok := rest.(*values.Pair)
		if !ok {
			return values.NewForeignError("log: expected a list for rest arguments")
		}
		base, err := ToFloat64(baseArg.Car())
		if err != nil {
			return values.WrapForeignErrorf(err, "log: %v", err)
		}
		mc.SetValue(values.NewFloat(math.Log(x) / math.Log(base)))
	}
	return nil
}
