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

// FIXME: turn into a helper. Share with vector.go
// AsList converts the stack to a Scheme list (values.Tuple).
func (p Stack) AsList() values.Tuple {
	if p.IsVoid() {
		return (*values.Pair)(nil)
	}
	if len(p) == 0 {
		return values.EmptyList
	}
	q := &values.Pair{p[0], values.EmptyList}
	curr := q
	for _, v := range p[1:] {
		curr = curr[1].(*values.Pair)
		curr[0] = v
		curr[1] = &values.Pair{}
	}
	curr[1] = values.EmptyList
	return q
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
