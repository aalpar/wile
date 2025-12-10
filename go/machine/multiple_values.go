package machine

import (
	"skeme/values"
	"slices"
	"strings"
)

type MultipleValues []values.Value

func NewMultipleValues(values ...values.Value) MultipleValues {
	return values
}

func (p MultipleValues) Length() int {
	return len(p)
}

func (p MultipleValues) Copy() MultipleValues {
	return slices.Clone(p)
}

func (p MultipleValues) IsVoid() bool {
	if p == nil || len(p) == 0 {
		return true
	}
	if len(p) == 1 && values.IsVoid(p[0]) {
		return true
	}
	return false
}

func (p MultipleValues) SchemeString() string {
	q := strings.Builder{}
	if len(p) == 0 {
		return values.SpecialVoid
	}
	if len(p) == 1 && values.IsVoid(p[0]) {
		return values.SpecialVoid
	}
	q.WriteString(p[0].SchemeString())
	for i, v := range p[1:] {
		if i != 0 {
			q.WriteString(" ")
		}
		q.WriteString(v.SchemeString())
	}
	return q.String()
}

func (p MultipleValues) EqualTo(o values.Value) bool {
	v, ok := o.(MultipleValues)
	if !ok {
		return false
	}
	if len(p) != len(v) {
		return false
	}
	for i := range p {
		if !p[i].EqualTo(v[i]) {
			return false
		}
	}
	return true
}
