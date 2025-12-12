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
	"wile/values"
	"slices"
	"strings"
)

type Stack values.Vector

func NewStack(vs ...values.Value) *Stack {
	return (*Stack)(&vs)
}

func (p *Stack) Push(v values.Value) {
	*p = append(*p, v)
}

func (p *Stack) Pull() values.Value {
	q := (*p)[0]
	*p = (*p)[1:]
	return q
}

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
	var result values.Tuple = values.EmptyList
	for i := len(p) - 1; i >= 0; i-- {
		result = &values.Pair{p[i], result}
	}
	return result
}

func (p *Stack) PushAll(vs []values.Value) {
	*p = append(*p, vs...)
}

func (s *Stack) PopAll() []values.Value {
	q := s.Copy()
	s.Clear()
	return *q
}

func (s Stack) PeekK(i int) values.Value {
	l := len(s)
	v := (s)[l-(i+1)]
	return v
}

func (s Stack) Copy() *Stack {
	newStack := slices.Clone(s)
	return &newStack
}

func (s *Stack) Clear() {
	if s == nil {
		return
	}
	*s = (*s)[:0]
}

func (s Stack) Length() int {
	if s == nil {
		return 0
	}
	return len(s)
}

func (s Stack) SchemeString() string {
	str := strings.Builder{}
	str.WriteString("#<stack (")
	if s != nil {
		for i, v := range s {
			if i > 0 {
				str.WriteString(" ")
			}
			str.WriteString(v.SchemeString())
		}
	}
	str.WriteString(")>")
	return str.String()
}

func (s Stack) IsVoid() bool {
	return s == nil
}

func (s Stack) String() string {
	str := strings.Builder{}
	str.WriteString("[")
	if s != nil {
		for i, v := range s {
			if i > 0 {
				str.WriteString(" ")
			}
			str.WriteString(v.SchemeString())
		}
	}
	str.WriteString("]")
	return str.String()
}
