package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimStringToList implements the string->list primitive.
// Converts a string to a list of characters.
func PrimStringToList(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	s, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "string->list: expected a string but got %T", o)
	}
	runes := []rune(s.Value)
	var result values.Value = values.EmptyList
	for i := len(runes) - 1; i >= 0; i-- {
		result = values.NewCons(values.NewCharacter(runes[i]), result)
	}
	mc.SetValue(result)
	return nil
}
