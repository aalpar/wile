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

package syntax

import (
	"reflect"
)

// SyntaxList constructs a syntax list from the given elements.
// The sc parameter provides a fallback source context for the list container.
// Each intermediate pair uses the source context of its car element when available,
// preserving per-element source location information for better error reporting.
func SyntaxList(sc *SourceContext, os ...SyntaxValue) *SyntaxPair {
	l := len(os)
	switch l {
	case 0:
		return NewSyntaxEmptyList(sc)
	case 1:
		// Use element's source context if available, otherwise fall back to sc
		elemSc := sc
		if os[0] != nil {
			if esc := os[0].SourceContext(); esc != nil {
				elemSc = esc
			}
		}
		return &SyntaxPair{
			Values:        [2]SyntaxValue{os[0], NewSyntaxEmptyList(sc)},
			sourceContext: elemSc,
		}
	}
	// Use first element's source context for the head pair
	headSc := sc
	if os[0] != nil {
		if esc := os[0].SourceContext(); esc != nil {
			headSc = esc
		}
	}
	q := &SyntaxPair{
		Values:        [2]SyntaxValue{os[0], &SyntaxPair{}},
		sourceContext: headSc,
	}
	curr := q
	for _, v := range os[1:] {
		curr = curr.Cdr().(*SyntaxPair)
		curr.SetCar(v)
		curr.SetCdr(&SyntaxPair{})
		// Use element's source context for this pair
		if v != nil {
			if esc := v.SourceContext(); esc != nil {
				curr.sourceContext = esc
			}
		}
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

func IsSyntaxVoid(v SyntaxValue) bool {
	return v == nil || v.IsVoid()
}

func IsSyntaxEmptyList(v SyntaxValue) bool {
	if v == nil {
		return false
	}
	pr, ok := v.(*SyntaxPair)
	if ok {
		return pr.IsEmptyList()
	}
	return false
}
