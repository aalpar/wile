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
	"slices"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// OperationBuildSyntaxList builds a syntax list from elements on the eval stack.
// Count elements are popped (in reverse order) and consed into a list.
//
// Dotted makes the LAST of those elements the list's tail rather than its final
// element, which is what `(a b . c)` needs. Without it every template was built
// onto SyntaxEmptyList, so an improper template came back proper —
// (syntax->datum (syntax (a b . c))) answered (a b c) where Chez answers
// (a b . c). The tail rides on the stack like any other element rather than
// getting its own operand, so the push protocol is unchanged and Count still
// says exactly how many values this pops.
type OperationBuildSyntaxList struct {
	machine.OperationBase
	Count  int
	Dotted bool
}

// NewOperationBuildSyntaxList creates a new OperationBuildSyntaxList.
func NewOperationBuildSyntaxList(count int) *OperationBuildSyntaxList {
	return &OperationBuildSyntaxList{
		OperationBase: machine.NewOperationBaseWithGoName("operation:build-syntax-list", "BuildSyntaxList"),
		Count:         count,
	}
}

// NewOperationBuildDottedSyntaxList creates an OperationBuildSyntaxList whose
// last popped element is the improper tail. count includes that tail, so a
// template with k elements before the dot has count == k+1.
func NewOperationBuildDottedSyntaxList(count int) *OperationBuildSyntaxList {
	return &OperationBuildSyntaxList{
		OperationBase: machine.NewOperationBaseWithGoName("operation:build-syntax-list", "BuildSyntaxList"),
		Count:         count,
		Dotted:        true,
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
	if p.Dotted {
		// Seed the fold with the tail instead of the empty list, and drop it
		// from the elements still to be consed on.
		result = asSyntaxValue(elements[len(elements)-1])
		elements = elements[:len(elements)-1]
	}
	for i := range slices.Backward(elements) {
		result = syntax.NewSyntaxCons(asSyntaxValue(elements[i]), result, nil)
	}

	mc.SetValue(result)
	mc.IncrPC()
	return mc, nil
}

// asSyntaxValue wraps a non-syntax stack value so it can sit in a syntax list.
func asSyntaxValue(v values.Value) syntax.SyntaxValue {
	s, ok := v.(syntax.SyntaxValue)
	if ok {
		return s
	}
	return syntax.NewSyntaxObject(v, nil)
}

// buildSyntaxListShape is the comparable projection of the two fields that
// decide what this operation builds. FieldMatches takes one extractor, and a
// Count-only comparison would call a dotted build equal to a proper one.
type buildSyntaxListShape struct {
	count  int
	dotted bool
}

func (p *OperationBuildSyntaxList) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationBuildSyntaxList)
	return machine.FieldMatches(p, v, ok,
		func(op *OperationBuildSyntaxList) buildSyntaxListShape {
			return buildSyntaxListShape{count: op.Count, dotted: op.Dotted}
		})
}
