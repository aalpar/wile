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
	"slices"
	"strings"

	"github.com/aalpar/wile/values"
)

type Stack values.Vector

// NewStack creates a new stack with the given initial values.
func NewStack(vs ...values.Value) *Stack {
	return (*Stack)(&vs)
}

// Push adds a value to the top of the stack.
func (p *Stack) Push(v values.Value) {
	*p = append(*p, v)
}

// Pull removes and returns the bottom value from the stack.
func (p *Stack) Pull() values.Value {
	if len(*p) == 0 {
		panic(values.ErrStackUnderflow)
	}
	q := (*p)[0]
	*p = (*p)[1:]
	return q
}

// Pop removes and returns the top value from the stack.
func (p *Stack) Pop() values.Value {
	l := len(*p)
	if l == 0 {
		panic(values.ErrStackUnderflow)
	}
	v := (*p)[l-1]
	*p = (*p)[:l-1]
	return v
}

// AsList converts the stack to a Scheme list (values.Tuple).
// The list is in stack order (first pushed = first element).
func (p Stack) AsList() values.Tuple {
	if p.IsVoid() {
		return (*values.Pair)(nil)
	}
	if len(p) == 0 {
		return values.EmptyList
	}
	// Build list from end to start to avoid mutating EmptyList
	result := values.EmptyList
	for i := len(p) - 1; i >= 0; i-- {
		result = values.NewCons(p[i], result)
	}
	return result
}

// PushAll pushes all values from the slice onto the stack.
func (p *Stack) PushAll(vs []values.Value) {
	*p = append(*p, vs...)
}

// PopAll removes and returns all values from the stack.
// The caller gets exclusive ownership of the returned slice's backing array.
func (p *Stack) PopAll() []values.Value {
	old := []values.Value(*p)
	*p = nil
	return old
}

// PeekK returns the kth value from the top of the stack without removing it.
// `K` is zero-based, so PeekK(0) returns the top value. `K` is used for methods that need a numeric index.
func (p Stack) PeekK(i int) values.Value {
	l := len(p)
	if i < 0 || i >= l {
		panic(values.WrapForeignErrorf(values.ErrStackUnderflow, "PeekK: index %d out of range for stack of length %d", i, l))
	}
	v := (p)[l-(i+1)]
	return v
}

// Copy creates a shallow copy of the stack.
func (p Stack) Copy() *Stack {
	newStack := slices.Clone(p)
	return &newStack
}

// Clear removes all elements from the stack.
func (p *Stack) Clear() {
	if p == nil {
		return
	}
	*p = (*p)[:0]
}

// Len returns the number of elements in the stack.
func (p Stack) Len() int {
	if p == nil {
		return 0
	}
	return len(p)
}

// SchemeString returns a Scheme-like string representation of the stack.
func (p Stack) SchemeString() string {
	str := strings.Builder{}
	str.WriteString("#<stack (")
	for i, v := range p {
		if i > 0 {
			str.WriteString(" ")
		}
		str.WriteString(v.SchemeString())
	}
	str.WriteString(")>")
	return str.String()
}

// IsVoid returns true if the stack is nil.
func (p Stack) IsVoid() bool {
	return p == nil
}

// String returns a string representation of the stack.
func (p Stack) String() string {
	str := strings.Builder{}
	str.WriteString("[")
	for i, v := range p {
		if i > 0 {
			str.WriteString(" ")
		}
		str.WriteString(v.SchemeString())
	}
	str.WriteString("]")
	return str.String()
}
