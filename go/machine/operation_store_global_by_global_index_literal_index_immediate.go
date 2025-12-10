package machine

import (
	"context"
	"fmt"
	"wile/environment"
	"wile/values"
)

type OperationStoreGlobalByGlobalIndexLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

func NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(liti LiteralIndex) *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationStoreGlobalByGlobalIndexLiteralIndexImmediate{LiteralIndex: liti}
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-store-global-by-global-index-literal-immediate %d>", p.LiteralIndex)
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationStoreGlobalByGlobalIndexLiteralIndexImmediate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return v == p
	}
	return p.LiteralIndex == v.LiteralIndex
}

func (p *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	if o == nil {
		return nil, values.WrapForeignErrorf(values.ErrNotAGlobalIndex, "literal index, %v, does not exist", p.LiteralIndex)
	}
	gi, ok := o.(*environment.GlobalIndex)
	if !ok {
		return nil, values.WrapForeignErrorf(values.ErrNotAGlobalIndex, "literal, %v, is not a global index", gi)
	}
	val := mc.evals.Pop()
	err := mc.env.GlobalEnvironment().SetGlobalValue(gi, val)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "no such global binding for %s", gi.SchemeString())
	}
	mc.pc++
	return mc, nil
}
