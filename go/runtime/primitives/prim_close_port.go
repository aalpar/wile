package primitives

import (
	"context"
	"io"

	"wile/machine"
	"wile/values"
)

// PrimClosePort implements the (close-port) primitive.
// Closes an input or output port.
func PrimClosePort(_ context.Context, mc *machine.MachineContext) error {
	o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	switch p := o.(type) {
	case *values.CharacterInputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			if err := closer.Close(); err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	case *values.CharacterOutputPort:
		if closer, ok := p.Value.(io.Closer); ok {
			if err := closer.Close(); err != nil {
				return values.WrapForeignErrorf(err, "close-port: %v", err)
			}
		}
	default:
		return values.WrapForeignErrorf(values.ErrNotAnInputPort, "close-port: expected a port but got %T", o)
	}
	mc.SetValues()
	return nil
}
