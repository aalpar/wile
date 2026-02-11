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
			name:  "equal to another syntaxEmptyListType",
			other: syntaxEmptyListType{},
			want:  true,
		},
		{
			name:  "equal to SyntaxEmptyList constant",
			other: SyntaxEmptyList,
			want:  true,
		},
		{
			name:  "not equal to values.EmptyList",
			other: values.EmptyList,
			want:  false,
		},
		{
			name:  "not equal to *SyntaxPair empty list",
			other: NewSyntaxEmptyList(nil),
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
			empty := syntaxEmptyListType{}
			got := empty.EqualTo(tc.other)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

func TestSyntaxEmptyList_SchemeString(t *testing.T) {
	c := qt.New(t)
	empty := syntaxEmptyListType{}
	c.Assert(empty.SchemeString(), qt.Equals, "()")
}

func TestSyntaxEmptyList_IsEmptyList(t *testing.T) {
	c := qt.New(t)
	empty := syntaxEmptyListType{}
	c.Assert(empty.IsEmptyList(), qt.Equals, true)
}

func TestSyntaxEmptyList_IsList(t *testing.T) {
	c := qt.New(t)
	empty := syntaxEmptyListType{}
	c.Assert(empty.IsList(), qt.Equals, true)
}

func TestSyntaxEmptyList_Length(t *testing.T) {
	c := qt.New(t)
	empty := syntaxEmptyListType{}
	c.Assert(empty.Length(), qt.Equals, 0)
}

func TestSyntaxEmptyList_Unwrap(t *testing.T) {
	c := qt.New(t)
	empty := syntaxEmptyListType{}
	unwrapped := empty.Unwrap()
	c.Assert(unwrapped, qt.Equals, values.EmptyList)
}

func TestSyntaxEmptyList_UnwrapAll(t *testing.T) {
	c := qt.New(t)
	empty := syntaxEmptyListType{}
	unwrapped := empty.UnwrapAll()
	c.Assert(unwrapped, qt.Equals, values.EmptyList)
}

func TestSyntaxEmptyList_AddScope(t *testing.T) {
	c := qt.New(t)

	scope := NewScope()
	empty := syntaxEmptyListType{sourceContext: nil}

	result := empty.AddScope(scope)

	// Should return a new syntaxEmptyListType with scope in source context
	resultEmpty, ok := result.(syntaxEmptyListType)
	c.Assert(ok, qt.IsTrue)
	c.Assert(resultEmpty.sourceContext, qt.IsNotNil)
	c.Assert(len(resultEmpty.sourceContext.Scopes), qt.Equals, 1)
	c.Assert(resultEmpty.sourceContext.Scopes[0], qt.Equals, scope)
}

func TestSyntaxEmptyList_AddScope_WithExistingContext(t *testing.T) {
	c := qt.New(t)

	scope1 := NewScope()
	scope2 := NewScope()

	sctx := NewSourceContext("", "", SourceIndexes{}, SourceIndexes{}).WithScope(scope1)
	empty := syntaxEmptyListType{sourceContext: sctx}

	result := empty.AddScope(scope2)

	resultEmpty, ok := result.(syntaxEmptyListType)
	c.Assert(ok, qt.IsTrue)
	c.Assert(len(resultEmpty.sourceContext.Scopes), qt.Equals, 2)
	c.Assert(resultEmpty.sourceContext.Scopes[0], qt.Equals, scope2) // prepended
	c.Assert(resultEmpty.sourceContext.Scopes[1], qt.Equals, scope1)
}

func TestNewSyntaxEmptyListWithContext(t *testing.T) {
	c := qt.New(t)

	sctx := NewSourceContext("test", "test.scm", SourceIndexes{}, SourceIndexes{})
	result := NewSyntaxEmptyListWithContext(sctx)

	empty, ok := result.(syntaxEmptyListType)
	c.Assert(ok, qt.IsTrue)
	c.Assert(empty.sourceContext, qt.Equals, sctx)
}

func TestIsSyntaxEmptyList_RecognizesSyntaxEmptyListType(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		value SyntaxValue
		want  bool
	}{
		{
			name:  "syntaxEmptyListType",
			value: syntaxEmptyListType{},
			want:  true,
		},
		{
			name:  "SyntaxEmptyList constant",
			value: SyntaxEmptyList,
			want:  true,
		},
		{
			name:  "*SyntaxPair empty list",
			value: NewSyntaxEmptyList(nil),
			want:  true,
		},
		{
			name:  "non-empty *SyntaxPair",
			value: NewSyntaxCons(NewSyntaxObject(values.NewInteger(1), nil), NewSyntaxEmptyList(nil), nil),
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
