package primitives

import (
	"context"
	"math"

	"skeme/machine"
	"skeme/values"
)

// PrimMakePolar implements the (make-polar) primitive.
// Creates a complex number from magnitude and angle (in radians).
func PrimMakePolar(_ context.Context, mc *machine.MachineContext) error {
	r := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	theta := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	var mag, angle float64
	switch v := r.(type) {
	case *values.Integer:
		mag = float64(v.Value)
	case *values.Float:
		mag = v.Value
	case *values.Rational:
		mag = v.Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-polar: expected a real number but got %T", r)
	}
	switch v := theta.(type) {
	case *values.Integer:
		angle = float64(v.Value)
	case *values.Float:
		angle = v.Value
	case *values.Rational:
		angle = v.Float64()
	default:
		return values.WrapForeignErrorf(values.ErrNotANumber, "make-polar: expected a real number but got %T", theta)
	}
	realPart := mag * math.Cos(angle)
	imagPart := mag * math.Sin(angle)
	mc.SetValue(values.NewComplexFromParts(realPart, imagPart))
	return nil
}
