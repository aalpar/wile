package primitives

import (
	"context"

	"skeme/machine"
	"skeme/values"
)

// PrimListToString implements the (list->string) primitive.
// Converts a list of characters to a string.
func PrimListToString(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	if values.IsEmptyList(o) {
		mc.SetValue(values.NewString(""))
		return nil
	}
	pr, ok := o.(*values.Pair)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->string: expected a list but got %T", o)
	}
	var runes []rune
	v, err := pr.ForEach(func(i int, hasNext bool, v values.Value) error {
		ch, ok := v.(*values.Character)
		if !ok {
			return values.WrapForeignErrorf(values.ErrNotACharacter, "list->string: expected a character but got %T", v)
		}
		runes = append(runes, ch.Value)
		return nil
	})
	if err != nil {
		return err
	}
	if !values.IsEmptyList(v) {
		return values.WrapForeignErrorf(values.ErrNotAList, "list->string: expected a proper list")
	}
	mc.SetValue(values.NewString(string(runes)))
	return nil
}
