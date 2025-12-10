package machine

import (
	"context"
	"fmt"
	"skeme/environment"
	"skeme/values"
)

type OperationStoreLocalByLocalIndexImmediate struct {
	LocalIndex *environment.LocalIndex
}

func NewOperationStoreLocalByLocalIndexImmediate(li *environment.LocalIndex) *OperationStoreLocalByLocalIndexImmediate {
	return &OperationStoreLocalByLocalIndexImmediate{LocalIndex: li}
}

func (p *OperationStoreLocalByLocalIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-local-by-local-index-immediate %s>", p.LocalIndex)
}

func (p *OperationStoreLocalByLocalIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationStoreLocalByLocalIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreLocalByLocalIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LocalIndex.EqualTo(v.LocalIndex)
}

func (p *OperationStoreLocalByLocalIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	err := mc.env.SetLocalValue(p.LocalIndex, mc.evals.Pop())
	if err != nil {
		return mc, err
	}
	mc.pc++
	return mc, nil
}
