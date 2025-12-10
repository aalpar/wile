package primitives

import (
	"context"
	"time"

	"skeme/machine"
	"skeme/values"
)

// PrimCurrentJiffy implements the (current-jiffy) primitive.
// Returns current time in jiffies since program start.
func PrimCurrentJiffy(_ context.Context, mc *machine.MachineContext) error {
	elapsed := time.Since(ProgramStartTime)
	jiffies := elapsed.Nanoseconds()
	mc.SetValue(values.NewInteger(jiffies))
	return nil
}
