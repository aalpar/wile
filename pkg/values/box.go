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

package values

import "fmt"

var _ Value = (*Box)(nil)

// Box represents a mutable Scheme box (container).
type Box struct {
	Value Value
}

// NewBox creates a new box containing the given value.
func NewBox(v Value) *Box {
	q := &Box{
		Value: v,
	}
	return q
}

// Unbox returns the boxed value.
func (p *Box) Unbox() Value {
	return p.Value
}

// IsVoid returns true if the box is nil.
func (p *Box) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the boxes contain equal values.
func (p *Box) EqualTo(v Value) bool {
	return Equal(p, v)
}

// EqualComponents pushes the two boxes' contents for Equal to compare. A box is
// mutable, so a cycle can run through one; Equal's visited set closes it.
func (p *Box) EqualComponents(v Value, push func(a, b Value)) bool {
	other, ok := v.(*Box)
	if !ok {
		return false
	}
	if p == nil || other == nil {
		return p == other
	}
	push(p.Value, other.Value)
	return true
}

// SchemeString returns the Scheme representation of the box.
func (p *Box) SchemeString() string {
	if p == nil {
		return "#<box:void>"
	}
	return p.schemeStringWithVisited(NewMapSet[Value](0), 1)
}

// schemeStringWithVisited renders the box under the same path-scoped cycle
// detection Pair, Vector and Hashtable use. set-box! can point a box at a
// structure that reaches the box again, so the naive "#&" + contents rendering
// this replaced overflowed the host Go stack.
//
// depth is this box's nesting level (root = 1); its content sits one level
// deeper, where schemeStringChild enforces the host-safety bound.
func (p *Box) schemeStringWithVisited(visited MapSet[Value], depth int) string {
	seen := visited.ContainsOne(p)
	if seen {
		return "..."
	}
	visited.Set(p)
	defer func() {
		visited.Unset(p)
	}()
	return fmt.Sprintf("#&%s", schemeStringChild(p.Value, visited, depth+1))
}
