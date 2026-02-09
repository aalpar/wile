// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package machine

import (
	"context"
	"fmt"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

// OperationLoadGlobalByGlobalIndexLiteralIndexImmediate loads a global variable using an index from the literals pool.
type OperationLoadGlobalByGlobalIndexLiteralIndexImmediate struct {
	LiteralIndex LiteralIndex
}

// NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate creates a new global load operation.
func NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(li LiteralIndex) *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate {
	return &OperationLoadGlobalByGlobalIndexLiteralIndexImmediate{LiteralIndex: li}
}

// SchemeString returns the Scheme representation of the operation.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) SchemeString() string {
	return fmt.Sprintf("#<machine-operation-load-global-by-global-index-literal-index-immediate %d>", p.LiteralIndex)
}

// IsVoid returns true if the operation is nil.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if both operations have the same literal index.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationLoadGlobalByGlobalIndexLiteralIndexImmediate)
	return fieldMatches(p, v, ok, func(op *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) LiteralIndex { return op.LiteralIndex })
}

// Apply executes the operation, loading the global variable's value.
func (p *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	o := mc.template.literals[p.LiteralIndex]
	if o == nil {
		return nil, mc.Error(fmt.Sprintf("literal index %v does not exist", p.LiteralIndex))
	}
	gi, ok := o.(*environment.GlobalIndex)
	if !ok {
		return nil, mc.Error(fmt.Sprintf("literal %v is not a global index", o))
	}
	// If the GlobalIndex carries a definition-site environment (cross-library
	// macro hygiene), look up directly in that frame. Otherwise fall back to
	// GetGlobalBinding which traverses the parent chain — necessary for
	// cross-phase lookups (e.g., expand-time primitives accessed from
	// syntax-case fenders running in a child environment).
	var bd *environment.Binding
	if gi.Env != nil {
		bd = gi.Env.GetOwnGlobalBinding(gi)
	} else {
		bd = mc.env.GetGlobalBinding(gi)
	}
	if bd == nil {
		return nil, mc.Error(fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
	}
	mc.value = NewMultipleValues(bd.Value())
	mc.pc++
	return mc, nil
}
