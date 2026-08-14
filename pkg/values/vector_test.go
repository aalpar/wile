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

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

func TestVectorCreation(t *testing.T) {
	tcs := []struct {
		name   string
		values []values.Value
		length int
	}{
		{
			name:   "empty vector",
			values: []values.Value{},
			length: 0,
		},
		{
			name:   "single element",
			values: []values.Value{values.NewInteger(1)},
			length: 1,
		},
		{
			name:   "two elements",
			values: []values.Value{values.NewInteger(1), values.NewInteger(2)},
			length: 2,
		},
		{
			name:   "three elements",
			values: []values.Value{values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)},
			length: 3,
		},
		{
			name:   "mixed types",
			values: []values.Value{values.NewInteger(1), values.NewString("hello"), values.TrueValue},
			length: 3,
		},
		{
			name:   "nil element",
			values: []values.Value{nil},
			length: 1,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v := values.NewVector(tc.values...)
			qt.Assert(t, v, qt.Not(qt.IsNil))
			qt.Assert(t, v.Length(), qt.Equals, tc.length)
		})
	}
}

func TestVectorIsVoid(t *testing.T) {
	tcs := []struct {
		name string
		in   *values.Vector
		out  bool
	}{
		{
			name: "nil vector is void",
			in:   nil,
			out:  true,
		},
		{
			name: "empty vector is not void",
			in:   values.NewVector(),
			out:  false,
		},
		{
			name: "single element vector is not void",
			in:   values.NewVector(values.NewInteger(1)),
			out:  false,
		},
		{
			name: "multiple element vector is not void",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.IsVoid(), qt.Equals, tc.out)
		})
	}
}

func TestVectorEqualTo(t *testing.T) {
	tcs := []struct {
		name string
		a    *values.Vector
		b    values.Value
		out  bool
	}{
		{
			name: "equal vectors same content",
			a:    values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			b:    values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			out:  true,
		},
		{
			name: "different content same length",
			a:    values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			b:    values.NewVector(values.NewInteger(2), values.NewInteger(1)),
			out:  false,
		},
		{
			name: "different lengths",
			a:    values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			b:    values.NewVector(values.NewInteger(1)),
			out:  false,
		},
		{
			name: "empty vectors equal",
			a:    values.NewVector(),
			b:    values.NewVector(),
			out:  true,
		},
		{
			name: "comparison with non-vector",
			a:    values.NewVector(values.NewInteger(1)),
			b:    values.NewInteger(1),
			out:  false,
		},
		{
			name: "comparison with nil vector",
			a:    values.NewVector(values.NewInteger(1)),
			b:    (*values.Vector)(nil),
			out:  false,
		},
		{
			name: "nil vectors equal",
			a:    nil,
			b:    (*values.Vector)(nil),
			out:  true,
		},
		{
			name: "single element equal",
			a:    values.NewVector(values.NewInteger(42)),
			b:    values.NewVector(values.NewInteger(42)),
			out:  true,
		},
		{
			name: "nested vectors equal",
			a:    values.NewVector(values.NewVector(values.NewInteger(1), values.NewInteger(2))),
			b:    values.NewVector(values.NewVector(values.NewInteger(1), values.NewInteger(2))),
			out:  true,
		},
		{
			name: "nested vectors different",
			a:    values.NewVector(values.NewVector(values.NewInteger(1), values.NewInteger(2))),
			b:    values.NewVector(values.NewVector(values.NewInteger(1), values.NewInteger(3))),
			out:  false,
		},
		{
			name: "mixed types equal",
			a:    values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
			b:    values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
			out:  true,
		},
		{
			name: "mixed types different",
			a:    values.NewVector(values.NewInteger(1), values.NewString("hello")),
			b:    values.NewVector(values.NewInteger(1), values.NewString("world")),
			out:  false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.a.EqualTo(tc.b), qt.Equals, tc.out)
		})
	}
}

func TestVectorSchemeString(t *testing.T) {
	tcs := []struct {
		name string
		in   *values.Vector
		out  string
	}{
		{
			name: "empty vector",
			in:   values.NewVector(),
			out:  "#()",
		},
		{
			name: "single element",
			in:   values.NewVector(values.NewInteger(42)),
			out:  "#(42)",
		},
		{
			name: "two elements",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			out:  "#(1 2)",
		},
		{
			name: "three elements",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  "#(1 2 3)",
		},
		{
			name: "mixed types",
			in:   values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
			out:  "#(1 \"hello\" #t)",
		},
		{
			name: "nested vector",
			in:   values.NewVector(values.NewVector(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)),
			out:  "#(#(1 2) 3)",
		},
		{
			name: "nested list",
			in:   values.NewVector(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)),
			out:  "#((1 2) 3)",
		},
		{
			name: "symbols",
			in:   values.NewVector(values.NewSymbol("a"), values.NewSymbol("b")),
			out:  "#(a b)",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

// TestVectorSchemeStringCyclic verifies a self-referential vector renders a
// bounded, correctly-placed cycle marker rather than overflowing the Go stack
// (C3). Assert the exact rendering, not just that "..." appears, so a wrong
// truncation (dropped element, marker in the wrong slot) is caught.
func TestVectorSchemeStringCyclic(t *testing.T) {
	v := values.NewVector(values.NewInteger(1), values.NewInteger(2), nil)
	v.Elems()[2] = v // self-reference

	qt.Assert(t, v.SchemeString(), qt.Equals, "#(1 2 ...)")
}

// TestVectorSchemeStringCrossCycle verifies a vector→pair→vector cycle is
// bounded from BOTH entry points. The shared visited set must be threaded
// across Pair and Vector rendering, otherwise each level allocates a fresh set
// and recurses forever.
func TestVectorSchemeStringCrossCycle(t *testing.T) {
	v := values.NewVector(values.NewInteger(1), nil)
	p := values.NewCons(values.NewSymbol("a"), v)
	v.Elems()[1] = p // vector -> pair -> vector

	// Entry from the vector root.
	qt.Assert(t, v.SchemeString(), qt.Equals, "#(1 (a . ...))")
	// Entry from the pair root (symmetric direction): pair -> vector -> pair.
	qt.Assert(t, p.SchemeString(), qt.Equals, "(a . #(1 ...))")
}

// TestVectorSchemeStringSharedAcyclic pins path-scoped marking: a non-cyclic
// shared vector renders in full at every occurrence. (Pre-Phase-3 this pinned
// the all-visited tradeoff "(#(1) ...)"; Phase 3 switched to path-scoped
// marking, so sharing is no longer mistaken for a cycle.)
func TestVectorSchemeStringSharedAcyclic(t *testing.T) {
	shared := values.NewVector(values.NewInteger(1))
	dag := values.List(shared, shared) // (#(1) #(1)) — acyclic, shared

	qt.Assert(t, dag.SchemeString(), qt.Equals, "(#(1) #(1))")
}

// TestVectorSchemeStringSharedVectorRow pins that a vector holding the SAME
// sub-vector twice renders both occurrences in full (acyclic sharing inside a
// vector, not a list).
func TestVectorSchemeStringSharedVectorRow(t *testing.T) {
	sub := values.NewVector(values.NewInteger(1), values.NewInteger(2))
	row := values.NewVector(sub, sub) // #(#(1 2) #(1 2)) — acyclic, shared

	qt.Assert(t, row.SchemeString(), qt.Equals, "#(#(1 2) #(1 2))")
}

func TestVectorAsList(t *testing.T) {
	tcs := []struct {
		name  string
		in    *values.Vector
		out   values.Tuple
		isNil bool
	}{
		{
			name:  "nil vector returns nil",
			in:    nil,
			isNil: true,
		},
		{
			name: "empty vector returns empty list",
			in:   values.NewVector(),
			out:  values.EmptyList,
		},
		{
			name: "single element vector",
			in:   values.NewVector(values.NewInteger(42)),
			out:  values.List(values.NewInteger(42)),
		},
		{
			name: "two element vector",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2)),
			out:  values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "three element vector",
			in:   values.NewVector(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "mixed types",
			in:   values.NewVector(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
			out:  values.List(values.NewInteger(1), values.NewString("hello"), values.TrueValue),
		},
		{
			name: "nested list as element",
			in:   values.NewVector(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)),
			out:  values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewInteger(3)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.in.AsList()
			if tc.isNil {
				qt.Assert(t, got, qt.IsNil)
			} else {
				qt.Assert(t, got, valuestest.SchemeEquals, tc.out)
			}
		})
	}
}
