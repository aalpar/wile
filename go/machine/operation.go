package machine

import (
	"context"
	"skeme/values"
)

type Operation interface {
	values.Value
	Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}
