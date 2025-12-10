package machine

import (
	"context"
	"wile/values"
)

type Operation interface {
	values.Value
	Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}
