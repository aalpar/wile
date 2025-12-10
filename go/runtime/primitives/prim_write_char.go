package primitives

import (
	"context"
	"unicode/utf8"

	"wile/environment"
	"wile/machine"
	"wile/values"
)

// PrimWriteChar implements the write-char primitive.
// Writes a character to the current output port or to the specified output port.
func PrimWriteChar(_ context.Context, mc *machine.MachineContext) error {
	obj := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	ch, ok := obj.(*values.Character)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotACharacter, "expected a character but got %T", obj)
	}
	o := mc.EnvironmentFrame().GetLocalBinding(environment.NewLocalIndex(1, 0)).Value()
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
	buf := make([]byte, 0, utf8.UTFMax)
	_, err := outpp.Value.Write(utf8.AppendRune(buf, ch.Value))
	if err != nil {
		return values.WrapForeignErrorf(err, "error writing character to output port %s", outpp.SchemeString())
	}
	mc.SetValues()
	return nil
}
