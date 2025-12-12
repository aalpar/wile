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
	"testing"

	qt "github.com/frankban/quicktest"
)

func Test_SyntaxForEach(t *testing.T) {
	// Create a syntax list to iterate over
	list := SyntaxList(nil,
		NewSyntaxSymbol("first", nil),
		NewSyntaxSymbol("second", nil),
		NewSyntaxSymbol("third", nil))

	// Track iteration results
	var items []SyntaxValue
	var indices []int
	var hasNextValues []bool

	// Call SyntaxForEach
	_, err := SyntaxForEach(list, func(i int, hasNext bool, v SyntaxValue) error {
		indices = append(indices, i)
		hasNextValues = append(hasNextValues, hasNext)
		items = append(items, v)
		return nil
	})

	// Assertions
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, len(items), qt.Equals, 3)
	qt.Assert(t, indices, qt.DeepEquals, []int{0, 1, 2})
	qt.Assert(t, hasNextValues[0], qt.IsTrue)
	qt.Assert(t, hasNextValues[2], qt.IsFalse)

	// Test with non-list value
	nonList := NewSyntaxSymbol("symbol", nil)
	result, err := SyntaxForEach(nonList, func(i int, hasNext bool, v SyntaxValue) error {
		t.Fatalf("Should not be called for non-list")
		return nil
	})

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.Equals, nonList)
}

func Test_EqualTo(t *testing.T) {
	tcs := []struct {
		name   string
		a      SyntaxValue
		b      SyntaxValue
		expect bool
	}{
		{
			name:   "identical symbols",
			a:      NewSyntaxSymbol("test", nil),
			b:      NewSyntaxSymbol("test", nil),
			expect: false,
		},
		{
			name:   "different symbols",
			a:      NewSyntaxSymbol("test1", nil),
			b:      NewSyntaxSymbol("test2", nil),
			expect: false,
		},
		{
			name:   "nil values",
			a:      nil,
			b:      nil,
			expect: true,
		},
		{
			name:   "empty lists",
			a:      NewSyntaxEmptyList(nil),
			b:      NewSyntaxEmptyList(nil),
			expect: false,
		},
		{
			name:   "equal lists",
			a:      SyntaxList(nil, NewSyntaxSymbol("a", nil)),
			b:      SyntaxList(nil, NewSyntaxSymbol("a", nil)),
			expect: false,
		},
		{
			name:   "different lists",
			a:      SyntaxList(nil, NewSyntaxSymbol("a", nil)),
			b:      SyntaxList(nil, NewSyntaxSymbol("b", nil)),
			expect: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := EqualTo(tc.a, tc.b)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func Test_IsSyntaxList(t *testing.T) {
	tcs := []struct {
		name   string
		value  SyntaxValue
		expect bool
	}{
		{
			name:   "nil",
			value:  nil,
			expect: false,
		},
		{
			name:   "empty list",
			value:  NewSyntaxEmptyList(nil),
			expect: true,
		},
		{
			name:   "list with one element",
			value:  SyntaxList(nil, NewSyntaxSymbol("test", nil)),
			expect: true,
		},
		{
			name:   "list with multiple elements",
			value:  SyntaxList(nil, NewSyntaxSymbol("a", nil), NewSyntaxSymbol("b", nil)),
			expect: true,
		},
		{
			name:   "non-list syntax object",
			value:  NewSyntaxSymbol("symbol", nil),
			expect: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := IsSyntaxList(tc.value)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func Test_IsSyntaxVoid(t *testing.T) {
	tcs := []struct {
		name   string
		value  SyntaxValue
		expect bool
	}{
		{
			name:   "nil",
			value:  nil,
			expect: true,
		},
		{
			name:   "SyntaxVoid",
			value:  SyntaxVoid,
			expect: true,
		},
		{
			name:   "empty list",
			value:  NewSyntaxEmptyList(nil),
			expect: false,
		},
		{
			name:   "syntax object",
			value:  NewSyntaxSymbol("test", nil),
			expect: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := IsSyntaxVoid(tc.value)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func Test_IsSyntaxEmptyList(t *testing.T) {
	tcs := []struct {
		name   string
		value  SyntaxValue
		expect bool
	}{
		{
			name:   "nil",
			value:  nil,
			expect: false,
		},
		{
			name:   "empty list",
			value:  NewSyntaxEmptyList(nil),
			expect: true,
		},
		{
			name:   "non-empty list",
			value:  SyntaxList(nil, NewSyntaxSymbol("test", nil)),
			expect: false,
		},
		{
			name:   "syntax object",
			value:  NewSyntaxSymbol("test", nil),
			expect: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := IsSyntaxEmptyList(tc.value)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func Test_SyntaxList(t *testing.T) {
	tcs := []struct {
		name   string
		in     *SyntaxPair
		out    *SyntaxPair
		expect bool
	}{
		{
			name:   "0",
			in:     SyntaxList(nil),
			out:    NewSyntaxEmptyList(nil),
			expect: true,
		},
		{
			name:   "1",
			in:     SyntaxList(nil, nil),
			out:    NewSyntaxCons(nil, NewSyntaxEmptyList(nil), nil),
			expect: true,
		},
		{
			name:   "2",
			in:     SyntaxList(nil, NewSyntaxSymbol("first", nil)),
			out:    NewSyntaxCons(NewSyntaxSymbol("first", nil), NewSyntaxEmptyList(nil), nil),
			expect: true,
		},
		{
			name: "3",
			in: SyntaxList(nil,
				NewSyntaxSymbol("first", nil),
				NewSyntaxSymbol("second", nil)),
			out: NewSyntaxCons(
				NewSyntaxSymbol("first", nil),
				NewSyntaxCons(
					NewSyntaxSymbol("second", nil),
					NewSyntaxEmptyList(nil),
					nil,
				),
				nil,
			),
			expect: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.UnwrapAll().EqualTo(tc.out.UnwrapAll()), qt.Equals, tc.expect)
		})
	}
}
