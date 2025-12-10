package primitives

import (
	"context"
	"io"
	"weak"

	"wile/machine"
	"wile/tokenizer"
	"wile/values"
)

// PrimReadToken implements the (read-token) primitive.
// Reads a single token from port.
func PrimReadToken(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAPair, "expected a pair but got %T", o)
	}
	if !pr.IsList() {
		return values.WrapForeignErrorf(values.ErrNotAList, "expected a list but got %s", pr.SchemeString())
	}
	inpp := GetCurrentInputPort()
	if !values.IsEmptyList(pr) {
		inpp, ok = pr.Car().(*values.CharacterInputPort)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacterInputPort, "expected a character input port but got %T", pr.Car())
		}
	}
	tknz, ok := Tokenizers[inpp]
	if !ok || tknz.Value() == nil {
		tknz = weak.Make(tokenizer.NewTokenizer(inpp.Value, false))
		Tokenizers[inpp] = tknz
	}
	q, err := tknz.Value().Next()
	if err == io.EOF {
		return values.WrapForeignErrorf(values.ErrEndOfFile, "end of file on %s", inpp.SchemeString())
	}
	if err != nil {
		return values.WrapForeignErrorf(err, "error reading token on %s", inpp.SchemeString())
	}
	mc.SetValue(q.(values.Value))
	return nil
}
