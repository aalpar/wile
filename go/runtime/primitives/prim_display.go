package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimDisplay implements the (display) primitive.
// Writes a human-readable representation of an object to an output port.
func PrimDisplay(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	outpp := GetCurrentOutputPort()
	if !values.IsEmptyList(pr) {
		outpp, ok = pr.Car().(*values.CharacterOutputPort)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacterOutputPort, "expected a character output port but got %T", pr.Car())
		}
	}
	_, err := outpp.Value.Write([]byte(StringValue(obj)))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing to output port %s", outpp.SchemeString())
	}
	mc.SetValues()
	return nil
}
