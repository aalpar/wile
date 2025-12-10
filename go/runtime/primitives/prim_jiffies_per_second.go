package primitives

import (
	"context"

	"wile/machine"
	"wile/values"
)

// PrimJiffiesPerSecond implements the (jiffies-per-second) primitive.
// Returns the number of jiffies per second (1 billion nanoseconds).
func PrimJiffiesPerSecond(_ context.Context, mc *machine.MachineContext) error {
	mc.SetValue(values.NewInteger(1000000000)) // 1 billion nanoseconds per second
	return nil
}
