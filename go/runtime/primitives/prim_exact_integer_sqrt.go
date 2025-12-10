package primitives

import (
	"context"
	"math"

	"wile/machine"
	"wile/values"
)

// PrimExactIntegerSqrt implements the (exact-integer-sqrt) primitive.
// Returns the integer square root and remainder as two values.
func PrimExactIntegerSqrt(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	v, ok := o.(*values.Integer)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "exact-integer-sqrt: expected an exact integer but got %T", o)
	}
	if v.Value < 0 {
		return values.NewForeignError("exact-integer-sqrt: expected a non-negative integer")
	}
	s := int64(math.Sqrt(float64(v.Value)))
	// Correct for potential floating point errors
	for s*s > v.Value {
		s--
	}
	for (s+1)*(s+1) <= v.Value {
		s++
	}
	r := v.Value - s*s
	mc.SetValues(values.NewInteger(s), values.NewInteger(r))
	return nil
}
