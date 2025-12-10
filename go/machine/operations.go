package machine

import (
	"wile/values"
)

type Operations []Operation

func NewOperations(ops ...Operation) Operations {
	return ops
}

func (p Operations) Length() int {
	return len(p)
}
func (p Operations) SchemeString() string {
	return "#<machine-operations>"
}

func (p Operations) IsVoid() bool {
	return p == nil
}

func (p Operations) EqualTo(o values.Value) bool {
	v, ok := o.(Operations)
	if !ok {
		return false
	}
	if len(p) != len(v) {
		return false
	}
	for i := 0; i < len(p); i++ {
		if !p[i].EqualTo(v[i]) {
			return false
		}
	}
	return true
}

func (p Operations) Copy() Operations {
	q := NewOperations(p...)
	return q
}
