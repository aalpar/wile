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
	"github.com/aalpar/wile/werr"
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

// Pull removes and returns the bottom value from the stack. In the SECD
// model (Landin 1964), the procedure is evaluated first but needed last.
// Pull retrieves it from the bottom after all arguments have been pushed
// on top. See BIBLIOGRAPHY.md "Stack-Based Virtual Machines".
func (p *Stack) Pull() values.Value {
	if len(*p) == 0 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "Stack.Pull: stack is empty"))
	}
	q := (*p)[0]
	copy((*p), (*p)[1:])
	*p = (*p)[:len(*p)-1]
	return q
}

// PullDrain removes and returns the bottom value (position 0) as the first
// return, and all remaining values as the second return. The stack is cleared.
// This is O(1) — no element shifting, just slice header arithmetic.
//
// The returned args slice shares the stack's backing array (same contract
// as Drain). Valid only until the next stack mutation.
func (p *Stack) PullDrain() (values.Value, []values.Value) {
	n := len(*p)
	if n == 0 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "Stack.PullDrain: stack is empty"))
	}
	first := (*p)[0]
	var rest []values.Value
	if n > 1 {
		rest = (*p)[1:n:n]
	}
	*p = (*p)[:0]
	return first, rest
}

// Pop2 removes and returns the top two values from the stack. q0 is the
// top-most value (popped first), q1 is the value beneath it. It checks the
// length once and is equivalent to two successive Pop() calls.
func (p *Stack) Pop2() (q0, q1 values.Value) {
	l := len(*p)
	if l < 2 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "Stack.Pop2: fewer than two values"))
	}
	q0 = (*p)[l-1]
	q1 = (*p)[l-2]
	*p = (*p)[:l-2]
	return q0, q1
}

// Pop removes and returns the top value from the stack.
func (p *Stack) Pop() values.Value {
	l := len(*p)
	if l == 0 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "Stack.Pop: stack is empty"))
	}
	v := (*p)[l-1]
	*p = (*p)[:l-1]
	return v
}

// PopN removes and returns the top n values from the stack.
// Values are returned in stack order (top-most element last).
// This is more efficient than calling Pop() n times.
func (p *Stack) PopN(n int) []values.Value {
	l := len(*p)
	if n < 0 {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "PopN: negative count %d", n))
	}
	if n > l {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "PopN: requested %d elements from stack of length %d", n, l))
	}
	var q []values.Value
	switch n {
	case 0:
		return nil
	case 1:
		q = []values.Value{
			(*p)[len(*p)-1],
		}
	case 2:
		q = []values.Value{
			(*p)[len(*p)-2],
			(*p)[len(*p)-1],
		}
	default:
		// Allocate result and copy in one operation
		q = make([]values.Value, n)
		copy(q, (*p)[l-n:])
	}
	*p = (*p)[:l-n]
	return q
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
	return values.List([]values.Value(p)...)
}

// PushAll pushes all values from the slice onto the stack.
func (p *Stack) PushAll(vs []values.Value) {
	*p = append(*p, vs...)
}

// Drain returns a view of all stack elements and resets the stack to empty.
// The returned slice shares the stack's backing array — it is valid only
// until the next mutation (Push, PushAll, or any append). Callers must
// finish reading before any stack mutation.
//
// Unlike PopAll (which copies into a new slice), Drain is zero-allocation.
// Use Drain when the caller consumes values immediately (e.g., binding
// arguments in Apply); use PopAll when the caller needs to own the slice.
func (p *Stack) Drain() []values.Value {
	n := len(*p)
	if n == 0 {
		return nil
	}
	view := (*p)[:n:n]
	*p = (*p)[:0]
	return view
}

// DrainN returns the top n values (in stack order: first-pushed first) and
// removes them, leaving any values beneath them on the stack. Like Drain it
// returns a non-allocating view, not a copy — the caller must consume it before
// pushing. A request larger than the stack panics with a wrapped sentinel rather
// than letting a downstream index run out of range. Used by OpSelfTailCall, which
// must consume exactly the call's argument count and never silently discard
// anything beneath it.
func (p *Stack) DrainN(n int) []values.Value {
	l := len(*p)
	if n > l {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow,
			"DrainN: requested %d elements from stack of length %d", n, l))
	}
	if n == 0 {
		return nil
	}
	view := (*p)[l-n : l : l]
	*p = (*p)[:l-n]
	return view
}

// PopAll removes and returns all values from the stack.
// The caller gets exclusive ownership of the returned slice.
// The stack retains its backing array for reuse (avoids re-allocation on
// subsequent pushes). References in the retained portion are cleared so the
// GC can collect the values.
func (p *Stack) PopAll() []values.Value {
	n := len(*p)
	if n == 0 {
		return nil
	}
	result := make([]values.Value, n)
	copy(result, *p)
	clear((*p)[:n])
	*p = (*p)[:0]
	return result
}

// PeekK returns the kth value from the top of the stack without removing it.
// `K` is zero-based, so PeekK(0) returns the top value. `K` is used for methods that need a numeric index.
func (p Stack) PeekK(i int) values.Value {
	l := len(p)
	if i < 0 || i >= l {
		panic(werr.WrapForeignErrorf(werr.ErrStackUnderflow, "PeekK: index %d out of range for stack of length %d", i, l))
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
