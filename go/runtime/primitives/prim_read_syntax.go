package primitives

import (
	"context"
	"weak"

	"skeme/machine"
	"skeme/parser"
	"skeme/values"
)

// PrimReadSyntax implements the (read-syntax) primitive.
// Reads datum with source information.
func PrimReadSyntax(_ context.Context, mc *machine.MachineContext) error {
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
	prss, ok := Parsers[inpp]
	if !ok || prss.Value() == nil {
		prss = weak.Make(parser.NewParser(mc.EnvironmentFrame(), inpp.Value))
		Parsers[inpp] = prss
	}
	q, err := prss.Value().ReadSyntax()
	if err != nil {
		return values.WrapForeignErrorf(err, "error reading syntax from input port %s", inpp.SchemeString())
	}
	mc.SetValue(q)
	return nil
}
