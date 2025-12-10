package primitives

import "skeme/values"

// Eqv is a helper implementing eqv? semantics for memv and assv.
func Eqv(a, b values.Value) bool {
	if a == b {
		return true
	}
	switch va := a.(type) {
	case *values.Integer:
		if vb, ok := b.(*values.Integer); ok {
			return va.Value == vb.Value
		}
	case *values.Float:
		if vb, ok := b.(*values.Float); ok {
			return va.Value == vb.Value
		}
	case *values.Rational:
		if vb, ok := b.(*values.Rational); ok {
			return va.Rat().Cmp(vb.Rat()) == 0
		}
	case *values.Complex:
		if vb, ok := b.(*values.Complex); ok {
			return va.Value == vb.Value
		}
	case *values.Character:
		if vb, ok := b.(*values.Character); ok {
			return va.Value == vb.Value
		}
	}
	return false
}
