package machine

import (
	"context"
	"fmt"
	"skeme/environment"
	"skeme/values"
)

type OperationLoadGlobalByGlobalIndexLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

func NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(li LiteralIndex) *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationLoadGlobalByGlobalIndexLiteralIndexImmediate{LiteralIndex: li}
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-global-by-global-index-literal-index-immediate %d>", p.LiteralIndex)
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadGlobalByGlobalIndexLiteralIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LiteralIndex == v.LiteralIndex
}

func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	if o == nil {
		return nil, values.WrapForeignErrorf(values.ErrNotAGlobalIndex, "literal index, %v, does not exist", p.LiteralIndex)
	}
	gi, ok := o.(*environment.GlobalIndex)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAGlobalIndex, "literal, %v, is not a global index", gi)
	}
	bd := mc.env.GlobalEnvironment().GetGlobalBinding(gi)
	if bd == nil {
		return nil, values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such global binding for %s", gi.SchemeString())
	}
	mc.value = NewMultipleValues(bd.Value())
	mc.pc++
	return mc, nil
}
