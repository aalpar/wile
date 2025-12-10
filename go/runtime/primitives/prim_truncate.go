package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimTruncate implements the truncate primitive.
// Truncates a number toward zero, removing any fractional part.
func PrimTruncate(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch v := o.(type) {
	case *values.Integer:
		mc.SetValue(v)
	case *values.Float:
		mc.SetValue(values.NewFloat(math.Trunc(v.Value)))
	case *values.Rational:
		f := v.Float64()
		mc.SetValue(values.NewFloat(math.Trunc(f)))
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "truncate: expected a real number but got %T", o)
	}
	return nil
}
