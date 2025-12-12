// Copyright 2025 Aaron Alpar
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

	"wile/syntax"
	"wile/values"
)

// OperationBuildSyntaxList builds a syntax list from elements on the eval stack.
// n elements are popped from the stack (in reverse order) and consed into a list.
type OperationBuildSyntaxList struct {
	Count int
}

func NewOperationBuildSyntaxList(count int) *OperationBuildSyntaxList {
	return &OperationBuildSyntaxList{Count: count}
}

func (p *OperationBuildSyntaxList) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	// Pop elements from stack in reverse order and build a list
	var result syntax.SyntaxValue = syntax.NewSyntaxEmptyList(nil)

	for i := 0; i < p.Count; i++ {
		elem := mctx.evals.Pop()
		// Wrap non-syntax values
		var stx syntax.SyntaxValue
		if s, ok := elem.(syntax.SyntaxValue); ok {
			stx = s
		} else if v, ok := elem.(values.Value); ok {
			stx = syntax.NewSyntaxObject(v, nil)
		} else {
			return nil, values.NewForeignErrorf("build-syntax-list: unexpected element type %T", elem)
		}
		result = syntax.NewSyntaxCons(stx, result, nil)
	}

	mctx.SetValue(result)
	mctx.pc++
	return mctx, nil
}

func (p *OperationBuildSyntaxList) String() string {
	return "BuildSyntaxList"
}

func (p *OperationBuildSyntaxList) SchemeString() string {
	return "#<operation:build-syntax-list>"
}

func (p *OperationBuildSyntaxList) EqualTo(other values.Value) bool {
	o, ok := other.(*OperationBuildSyntaxList)
	return ok && p.Count == o.Count
}

func (p *OperationBuildSyntaxList) IsVoid() bool {
	return false
}
