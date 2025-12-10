package machine

import (
	"context"
	"fmt"
	"skeme/values"
)

type OperationLoadLiteralByLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

func NewOperationLoadLiteralByLiteralIndexImmediate(li LiteralIndex) *OperationLoadLiteralByLiteralIndexImmediate {
	return &OperationLoadLiteralByLiteralIndexImmediate{LiteralIndex: li}
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	mc.value = []values.Value{o}
	mc.pc++
	return mc, nil
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-literal-by-literal-index-immediate %d>", p.LiteralIndex)
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadLiteralByLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadLiteralByLiteralIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LiteralIndex == v.LiteralIndex
}
