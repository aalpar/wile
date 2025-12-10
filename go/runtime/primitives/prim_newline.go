package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimNewline implements the newline primitive.
// Writes a newline character to the output port.
func PrimNewline(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
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
	_, err := outpp.Value.Write([]byte("\n"))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing newline to output port %s", outpp.SchemeString())
	}
	mc.SetValues()
	return nil
}
