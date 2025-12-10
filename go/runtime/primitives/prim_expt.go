package primitives

import (
	"context"
	"math"
	"math/cmplx"

	"wile/machine"
	"wile/values"
)

// PrimExpt implements the (expt) primitive.
// Returns base raised to the exponent power.
func PrimExpt(_ context.Context, mc *machine.MachineContext) error {
	base := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	exp := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	baseNum, ok := base.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "expt: expected a number but got %T", base)
	}
	expNum, ok := exp.(values.Number)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotANumber, "expt: expected a number but got %T", exp)
	}
	// Special case: integer exponents on integers
	if baseInt, ok := baseNum.(*values.Integer); ok {
		if expInt, ok := expNum.(*values.Integer); ok {
			if expInt.Value >= 0 {
				result := int64(1)
				b := baseInt.Value
				e := expInt.Value
				for e > 0 {
					if e%2 == 1 {
						result *= b
					}
					b *= b
					e /= 2
				}
				mc.SetValue(values.NewInteger(result))
				return nil
			}
			// Negative exponent: return float
			mc.SetValue(values.NewFloat(math.Pow(float64(baseInt.Value), float64(expInt.Value))))
			return nil
		}
	}
	// General case: use float math
	switch b := baseNum.(type) {
	case *values.Complex:
		switch e := expNum.(type) {
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, e.Value)))
		case *values.Float:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(e.Value, 0))))
		case *values.Integer:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(float64(e.Value), 0))))
		case *values.Rational:
			mc.SetValue(values.NewComplex(cmplx.Pow(b.Value, complex(e.Float64(), 0))))
		}
	default:
		var bf float64
		switch v := baseNum.(type) {
		case *values.Integer:
			bf = float64(v.Value)
		case *values.Float:
			bf = v.Value
		case *values.Rational:
			bf = v.Float64()
		}
		var ef float64
		switch v := expNum.(type) {
		case *values.Integer:
			ef = float64(v.Value)
		case *values.Float:
			ef = v.Value
		case *values.Rational:
			ef = v.Float64()
		case *values.Complex:
			mc.SetValue(values.NewComplex(cmplx.Pow(complex(bf, 0), v.Value)))
			return nil
		}
		mc.SetValue(values.NewFloat(math.Pow(bf, ef)))
	}
	return nil
}
