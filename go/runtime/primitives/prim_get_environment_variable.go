package primitives

import (
	"context"
	"os"

	"skeme/machine"
	"skeme/values"
)

// PrimGetEnvironmentVariable implements the (get-environment-variable) primitive.
// Gets environment variable value.
func PrimGetEnvironmentVariable(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	name, ok := o.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "get-environment-variable: expected a string but got %T", o)
	}
	val, exists := os.LookupEnv(name.Value)
	if exists {
		mc.SetValue(values.NewString(val))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}
