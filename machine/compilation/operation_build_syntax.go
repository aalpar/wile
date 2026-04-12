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

package compilation

import (
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// OperationBuildSyntaxList builds a syntax list from elements on the eval stack.
// n elements are popped from the stack (in reverse order) and consed into a list.
type OperationBuildSyntaxList struct {
	machine.OperationBase
	Count int
}

// NewOperationBuildSyntaxList creates a new OperationBuildSyntaxList.
func NewOperationBuildSyntaxList(count int) *OperationBuildSyntaxList {
	return &OperationBuildSyntaxList{
		OperationBase: machine.NewOperationBaseWithGoName("operation:build-syntax-list", "BuildSyntaxList"),
		Count:         count,
	}
}

// Apply implements the Operation interface.
func (p *OperationBuildSyntaxList) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Batch pop all elements from stack and build a list
	var result syntax.SyntaxValue = syntax.SyntaxEmptyList

	if p.Count == 0 {
		mc.SetValue(result)
		mc.IncrPC()
		return mc, nil
	}

	// PopN returns elements in stack order (bottom to top)
	// We iterate backwards to build the list in reverse
	elements := mc.Evals().PopN(p.Count)
	for i := len(elements) - 1; i >= 0; i-- {
		elem := elements[i]
		// Wrap non-syntax values
		var stx syntax.SyntaxValue
		s, ok := elem.(syntax.SyntaxValue)
		if ok {
			stx = s
		} else {
			v := elem
			stx = syntax.NewSyntaxObject(v, nil)
		}
		result = syntax.NewSyntaxCons(stx, result, nil)
	}

	mc.SetValue(result)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationBuildSyntaxList) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationBuildSyntaxList)
	return machine.FieldMatches(p, v, ok,
		func(op *OperationBuildSyntaxList) int {
			return op.Count
		})
}
