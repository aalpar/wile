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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestWriteValueToString_SimpleValues(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"integer", values.NewInteger(42), "42"},
		{"negative integer", values.NewInteger(-7), "-7"},
		{"float", values.NewFloat(3.14), "3.14"},
		{"string", values.NewString("hello"), "\"hello\""},
		{"symbol", values.NewSymbol("foo"), "foo"},
		{"true", values.TrueValue, "#t"},
		{"false", values.FalseValue, "#f"},
		{"void", values.Void, "#!void"},
		{"eof", values.EOFObject, "#!eof"},
		{"nil", nil, "#<void>"},
		{"character", values.NewCharacter('a'), "#\\a"},
		{"character space", values.NewCharacter(' '), "#\\ "},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.WriteValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_Lists(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"empty list", values.EmptyList, "()"},
		{"single element", values.List(values.NewInteger(1)), "(1)"},
		{"proper list", values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)), "(1 2 3)"},
		{"improper pair", values.NewCons(values.NewInteger(1), values.NewInteger(2)), "(1 . 2)"},
		{"nested list", values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)), "((1 2) 3)"},
		{"list with string", values.List(values.NewString("a"), values.NewString("b")), "(\"a\" \"b\")"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.WriteValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_Vectors(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"empty vector", values.NewVector(), "#()"},
		{"single element", values.NewVector(values.NewInteger(1)), "#(1)"},
		{"multiple elements", values.NewVector(values.NewInteger(1), values.NewSymbol("a"), values.NewString("b")), "#(1 a \"b\")"},
		{"nil vector", (*values.Vector)(nil), "#()"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.WriteValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestWriteValueToString_CircularPair(t *testing.T) {
	// Create a circular list: (1 . #0#) where #0# is the pair itself
	p := values.NewCons(values.NewInteger(1), values.EmptyList)
	p[1] = p // make circular

	result := values.WriteValueToString(p)
	qt.Assert(t, result, qt.Equals, "#0=(1 . #0#)")
}

func TestWriteValueToString_CircularVector(t *testing.T) {
	// Create a vector that contains itself
	v := values.NewVector(values.NewInteger(1), nil)
	(*v)[1] = v // make circular

	result := values.WriteValueToString(v)
	qt.Assert(t, result, qt.Equals, "#0=#(1 #0#)")
}

func TestWriteSharedValueToString_SharedStructure(t *testing.T) {
	// Create shared structure: (#0=(1 2) #0#)
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	outer := values.List(shared, shared)

	result := values.WriteSharedValueToString(outer)
	qt.Assert(t, result, qt.Equals, "(#0=(1 2) #0#)")
}

func TestWriteSharedValueToString_NoSharing(t *testing.T) {
	// No shared structure => no labels
	l := values.List(values.NewInteger(1), values.NewInteger(2))
	result := values.WriteSharedValueToString(l)
	qt.Assert(t, result, qt.Equals, "(1 2)")
}

func TestWriteValueToString_SharedButNotCircular(t *testing.T) {
	// WriteModeWrite should NOT label shared-but-not-circular structures
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	outer := values.List(shared, shared)

	result := values.WriteValueToString(outer)
	qt.Assert(t, result, qt.Equals, "((1 2) (1 2))")
}

func TestDisplayValueToString_Strings(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"string unquoted", values.NewString("hello"), "hello"},
		{"string with spaces", values.NewString("hello world"), "hello world"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.DisplayValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestDisplayValueToString_Characters(t *testing.T) {
	tcs := []struct {
		name string
		in   values.Value
		out  string
	}{
		{"char a", values.NewCharacter('a'), "a"},
		{"char space", values.NewCharacter(' '), " "},
		{"char newline", values.NewCharacter('\n'), "\n"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.DisplayValueToString(tc.in), qt.Equals, tc.out)
		})
	}
}

func TestDisplayValueToString_NonStringValues(t *testing.T) {
	// Non-string/character values are the same as write
	qt.Assert(t, values.DisplayValueToString(values.NewInteger(42)), qt.Equals, "42")
	qt.Assert(t, values.DisplayValueToString(values.NewSymbol("foo")), qt.Equals, "foo")
	qt.Assert(t, values.DisplayValueToString(values.TrueValue), qt.Equals, "#t")
}

func TestDisplayValueToString_List(t *testing.T) {
	l := values.List(values.NewString("hello"), values.NewCharacter('!'))
	result := values.DisplayValueToString(l)
	qt.Assert(t, result, qt.Equals, "(hello !)")
}

func TestWriteValueToString_NilPair(t *testing.T) {
	result := values.WriteValueToString((*values.Pair)(nil))
	qt.Assert(t, result, qt.Equals, "#<void>")
}

func TestWriteSharedValueToString_SharedVector(t *testing.T) {
	// Shared vector structure
	shared := values.NewVector(values.NewInteger(1))
	outer := values.NewVector(shared, shared)

	result := values.WriteSharedValueToString(outer)
	qt.Assert(t, result, qt.Equals, "#(#0=#(1) #0#)")
}
