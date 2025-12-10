package syntax

import (
	"reflect"
	"skeme/values"
)

func SyntaxList(sc *SourceContext, os ...SyntaxValue) *SyntaxPair {
	l := len(os)
	switch l {
	case 0:
		return NewSyntaxEmptyList(sc)
	case 1:
		return &SyntaxPair{
			Values:        [2]SyntaxValue{os[0], NewSyntaxEmptyList(sc)},
			sourceContext: sc,
		}
	}
	q := &SyntaxPair{
		Values:        [2]SyntaxValue{os[0], &SyntaxPair{}},
		sourceContext: sc,
	}
	curr := q
	for _, v := range os[1:] {
		curr = curr.Cdr().(*SyntaxPair)
		curr.SetCar(v)
		curr.SetCdr(&SyntaxPair{})
	}
	curr.SetCdr(NewSyntaxEmptyList(sc))
	return q
}

func SyntaxForEach(o SyntaxValue, fn func(i int, hasNext bool, v SyntaxValue) error) (SyntaxValue, error) {
	pr, ok := o.(SyntaxTuple)
	if ok {
		return pr.SyntaxForEach(fn)
	}
	return o, nil
}

func EqualTo(a, b SyntaxValue) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
	if av.Comparable() && bv.Comparable() && a == b {
		return true
	}
	if av.IsNil() && bv.IsNil() {
		return true
	}
	return a.EqualTo(b)
}

func IsSyntaxList(v SyntaxValue) bool {
	if v == nil {
		return false
	}
	switch pr := v.(type) {
	case *SyntaxPair:
		return pr.IsList()
	}
	return false
}

func IsSyntaxVoid(v values.Value) bool {
	return v == nil || v.IsVoid()
}

func IsSyntaxEmptyList(v values.Value) bool {
	if v == nil {
		return false
	}
	pr, ok := v.(*SyntaxPair)
	if ok {
		return pr.IsEmptyList()
	}
	return false
}
