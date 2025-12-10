package primitives

import "skeme/values"

func ToFloat64(v values.Value) (float64, error) {
	switch n := v.(type) {
	case *values.Integer:
		return float64(n.Value), nil
	case *values.Float:
		return n.Value, nil
	case *values.Rational:
		f, _ := n.Rat().Float64()
		return f, nil
	default:
		return 0, values.WrapForeignErrorf(values.ErrNotANumber, "expected a real number but got %T", v)
	}
}
