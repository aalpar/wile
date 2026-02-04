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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteValueToString_SimpleValues(t *testing.T) {
	tcs := []struct {
		name string
		in   Value
		out  string
	}{
		{"integer", NewInteger(42), "42"},
		{"negative integer", NewInteger(-7), "-7"},
		{"float", NewFloat(3.14), "3.14"},
		{"string", NewString("hello"), "\"hello\""},
		{"symbol", NewSymbol("foo"), "foo"},
		{"true", TrueValue, "#t"},
		{"false", FalseValue, "#f"},
		{"void", Void, "#!void"},
		{"eof", EOFObject, "#!eof"},
		{"nil", nil, "#<void>"},
		{"character", NewCharacter('a'), "#\\a"},
		{"character space", NewCharacter(' '), "#\\ "},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, WriteValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_Lists(t *testing.T) {
	tcs := []struct {
		name string
		in   Value
		out  string
	}{
		{"empty list", EmptyList, "()"},
		{"single element", List(NewInteger(1)), "(1)"},
		{"proper list", List(NewInteger(1), NewInteger(2), NewInteger(3)), "(1 2 3)"},
		{"improper pair", NewCons(NewInteger(1), NewInteger(2)), "(1 . 2)"},
		{"nested list", List(List(NewInteger(1), NewInteger(2)), NewInteger(3)), "((1 2) 3)"},
		{"list with string", List(NewString("a"), NewString("b")), "(\"a\" \"b\")"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, WriteValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_Vectors(t *testing.T) {
	tcs := []struct {
		name string
		in   Value
		out  string
	}{
		{"empty vector", NewVector(), "#()"},
		{"single element", NewVector(NewInteger(1)), "#(1)"},
		{"multiple elements", NewVector(NewInteger(1), NewSymbol("a"), NewString("b")), "#(1 a \"b\")"},
		{"nil vector", (*Vector)(nil), "#()"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, WriteValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_CircularPair(t *testing.T) {
	// Create a circular list: (1 . #0#) where #0# is the pair itself
	p := NewCons(NewInteger(1), EmptyList)
	p[1] = p // make circular

	result := WriteValueToString(p)
	qt.Assert(t, result, qt.Equals, "#0=(1 . #0#)")
}

func TestWriteValueToString_CircularVector(t *testing.T) {
	// Create a vector that contains itself
	v := NewVector(NewInteger(1), nil)
	(*v)[1] = v // make circular

	result := WriteValueToString(v)
	qt.Assert(t, result, qt.Equals, "#0=#(1 #0#)")
}

func TestWriteSharedValueToString_SharedStructure(t *testing.T) {
	// Create shared structure: (#0=(1 2) #0#)
	shared := List(NewInteger(1), NewInteger(2))
	outer := List(shared, shared)

	result := WriteSharedValueToString(outer)
	qt.Assert(t, result, qt.Equals, "(#0=(1 2) #0#)")
}

func TestWriteSharedValueToString_NoSharing(t *testing.T) {
	// No shared structure => no labels
	l := List(NewInteger(1), NewInteger(2))
	result := WriteSharedValueToString(l)
	qt.Assert(t, result, qt.Equals, "(1 2)")
}

func TestWriteValueToString_SharedButNotCircular(t *testing.T) {
	// WriteModeWrite should NOT label shared-but-not-circular structures
	shared := List(NewInteger(1), NewInteger(2))
	outer := List(shared, shared)

	result := WriteValueToString(outer)
	qt.Assert(t, result, qt.Equals, "((1 2) (1 2))")
}

func TestDisplayValueToString_Strings(t *testing.T) {
	tcs := []struct {
		name string
		in   Value
		out  string
	}{
		{"string unquoted", NewString("hello"), "hello"},
		{"string with spaces", NewString("hello world"), "hello world"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, DisplayValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestDisplayValueToString_Characters(t *testing.T) {
	tcs := []struct {
		name string
		in   Value
		out  string
	}{
		{"char a", NewCharacter('a'), "a"},
		{"char space", NewCharacter(' '), " "},
		{"char newline", NewCharacter('\n'), "\n"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, DisplayValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestDisplayValueToString_NonStringValues(t *testing.T) {
	// Non-string/character values are the same as write
	qt.Assert(t, DisplayValueToString(NewInteger(42)), qt.Equals, "42")
	qt.Assert(t, DisplayValueToString(NewSymbol("foo")), qt.Equals, "foo")
	qt.Assert(t, DisplayValueToString(TrueValue), qt.Equals, "#t")
}

func TestDisplayValueToString_List(t *testing.T) {
	l := List(NewString("hello"), NewCharacter('!'))
	result := DisplayValueToString(l)
	qt.Assert(t, result, qt.Equals, "(hello !)")
}

func TestWriteValueToString_NilPair(t *testing.T) {
	result := WriteValueToString((*Pair)(nil))
	qt.Assert(t, result, qt.Equals, "#<void>")
}

func TestWriteSharedValueToString_SharedVector(t *testing.T) {
	// Shared vector structure
	shared := NewVector(NewInteger(1))
	outer := NewVector(shared, shared)

	result := WriteSharedValueToString(outer)
	qt.Assert(t, result, qt.Equals, "#(#0=#(1) #0#)")
}
