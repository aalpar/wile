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

package syntax

import (
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestSyntaxEmptyList_EqualTo(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		other values.Value
		want  bool
	}{
		{
			name:  "equal to SyntaxEmptyList singleton",
			other: SyntaxEmptyList,
			want:  true,
		},
		{
			name:  "not equal to values.EmptyList",
			other: values.EmptyList,
			want:  false,
		},
		{
			name:  "not equal to integer",
			other: values.NewInteger(0),
			want:  false,
		},
		{
			name:  "not equal to nil",
			other: nil,
			want:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := SyntaxEmptyList.EqualTo(tc.other)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

func TestSyntaxEmptyList_SchemeString(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.SchemeString(), qt.Equals, "#'()")
}

func TestSyntaxEmptyList_IsEmptyList(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.IsEmptyList(), qt.Equals, true)
}

func TestSyntaxEmptyList_IsList(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.IsList(), qt.Equals, true)
}

func TestSyntaxEmptyList_Length(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.Length(), qt.Equals, 0)
}

func TestSyntaxEmptyList_Unwrap(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.Unwrap(), qt.Equals, values.EmptyList)
}

func TestSyntaxEmptyList_UnwrapAll(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.UnwrapAll(), qt.Equals, values.EmptyList)
}

func TestSyntaxEmptyList_AddScope(t *testing.T) {
	c := qt.New(t)

	scope := NewScope()
	empty := SyntaxEmptyList.(*syntaxEmptyListType)
	result := empty.AddScope(scope)

	// AddScope returns the singleton unchanged — empty lists have no symbols
	c.Assert(result, qt.Equals, SyntaxEmptyList)
}

func TestSyntaxEmptyList_SourceContext(t *testing.T) {
	c := qt.New(t)
	c.Assert(SyntaxEmptyList.SourceContext(), qt.IsNil)
}

func TestSyntaxEmptyList_SingletonIdentity(t *testing.T) {
	c := qt.New(t)

	// AddScope returns the exact same pointer
	scope := NewScope()
	empty := SyntaxEmptyList.(*syntaxEmptyListType)
	after := empty.AddScope(scope)
	c.Assert(after, qt.Equals, SyntaxEmptyList)
}

func TestIsSyntaxEmptyList_RecognizesSingleton(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		value SyntaxValue
		want  bool
	}{
		{
			name:  "SyntaxEmptyList singleton",
			value: SyntaxEmptyList,
			want:  true,
		},
		{
			name:  "non-empty SyntaxPair",
			value: NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), SyntaxEmptyList, nil),
			want:  false,
		},
		{
			name:  "nil",
			value: nil,
			want:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := IsSyntaxEmptyList(tc.value)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}
