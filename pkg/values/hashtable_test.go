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

// helper to build a hashtable from key-value pairs for tests.
func makeHT(kvs ...any) *values.Hashtable {
	ht := values.NewEmptyHashtable()
	for i := 0; i < len(kvs); i += 2 {
		_ = ht.Set(kvs[i].(values.Value), kvs[i+1].(values.Value))
	}
	return ht
}

func TestHashtable_EqualTo(t *testing.T) {
	tcs := []struct {
		name string
		in0  values.Value
		in1  values.Value
		out  bool
	}{
		{
			name: "different keys",
			in0:  makeHT(values.NewSymbol("key1"), values.NewInteger(1)),
			in1:  makeHT(values.NewSymbol("key2"), values.NewInteger(1)),
			out:  false,
		},
		{
			name: "equal contents",
			in0:  makeHT(values.NewSymbol("key1"), values.NewInteger(1)),
			in1:  makeHT(values.NewSymbol("key1"), values.NewInteger(1)),
			out:  true,
		},
		{
			name: "different values",
			in0:  makeHT(values.NewSymbol("key1"), values.NewInteger(1)),
			in1:  makeHT(values.NewSymbol("key1"), values.NewInteger(2)),
			out:  false,
		},
		{
			name: "different sizes",
			in0:  makeHT(values.NewSymbol("a"), values.NewInteger(1), values.NewSymbol("b"), values.NewInteger(2)),
			in1:  makeHT(values.NewSymbol("a"), values.NewInteger(1)),
			out:  false,
		},
		{
			name: "not a hashtable",
			in0:  values.NewEmptyHashtable(),
			in1:  values.NewInteger(1),
			out:  false,
		},
		{
			name: "empty hashtables equal",
			in0:  values.NewEmptyHashtable(),
			in1:  values.NewEmptyHashtable(),
			out:  true,
		},
		{
			name: "integer keys",
			in0:  makeHT(values.NewInteger(1), values.NewString("one")),
			in1:  makeHT(values.NewInteger(1), values.NewString("one")),
			out:  true,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}

func TestHashtable_SchemeString(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()
	c.Assert(ht.SchemeString(), qt.Equals, "#hash()")

	ht2 := makeHT(values.NewSymbol("a"), values.NewInteger(1))
	c.Assert(ht2.SchemeString(), qt.Equals, "#hash((a . 1))")
}

func TestHashtable_NewEmptyHashtable(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()
	c.Assert(ht.Size(), qt.Equals, 0)
	c.Assert(ht.IsVoid(), qt.IsFalse)
}

func TestHashtable_GetSetDelete(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()

	// Set and Get — uses structural equality, not pointer identity
	err := ht.Set(values.NewSymbol("key"), values.NewInteger(42))
	c.Assert(err, qt.IsNil)

	val, found, err := ht.Get(values.NewSymbol("key"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(42))

	// HasKey
	found, err = ht.HasKey(values.NewSymbol("key"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)

	found, err = ht.HasKey(values.NewSymbol("missing"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsFalse)

	// Get missing
	_, found, err = ht.Get(values.NewSymbol("missing"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsFalse)

	// Delete
	err = ht.Delete(values.NewSymbol("key"))
	c.Assert(err, qt.IsNil)
	c.Assert(ht.Size(), qt.Equals, 0)
}

func TestHashtable_KeysValues(t *testing.T) {
	c := qt.New(t)

	ht := values.NewEmptyHashtable()
	c.Assert(ht.Keys(), qt.Equals, values.EmptyList)
	c.Assert(ht.Values(), qt.Equals, values.EmptyList)

	err := ht.Set(values.NewSymbol("a"), values.NewInteger(1))
	c.Assert(err, qt.IsNil)

	keys := ht.Keys()
	c.Assert(keys.Length(), qt.Equals, 1)
	c.Assert(keys.Car(), valuestest.SchemeEquals, values.NewSymbol("a"))

	vals := ht.Values()
	c.Assert(vals.Length(), qt.Equals, 1)
	c.Assert(vals.Car(), valuestest.SchemeEquals, values.NewInteger(1))
}

func TestHashtable_CopyClear(t *testing.T) {
	c := qt.New(t)

	ht := values.NewEmptyHashtable()
	err := ht.Set(values.NewSymbol("a"), values.NewInteger(1))
	c.Assert(err, qt.IsNil)

	// Copy is independent
	cp := ht.Copy()
	c.Assert(cp.Size(), qt.Equals, 1)

	err = ht.Set(values.NewSymbol("b"), values.NewInteger(2))
	c.Assert(err, qt.IsNil)
	c.Assert(ht.Size(), qt.Equals, 2)
	c.Assert(cp.Size(), qt.Equals, 1)

	// Clear
	ht.Clear()
	c.Assert(ht.Size(), qt.Equals, 0)
	c.Assert(cp.Size(), qt.Equals, 1)
}

func TestHashtable_NonHashableKey(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()
	key := values.NewCons(values.NewInteger(1), values.NewInteger(2))

	err := ht.Set(key, values.NewInteger(1))
	c.Assert(err, qt.IsNotNil)

	_, _, err = ht.Get(key)
	c.Assert(err, qt.IsNotNil)

	_, err = ht.HasKey(key)
	c.Assert(err, qt.IsNotNil)

	err = ht.Delete(key)
	c.Assert(err, qt.IsNotNil)
}

func TestHashtable_VariousKeyTypes(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()

	// Symbol key
	err := ht.Set(values.NewSymbol("sym"), values.NewInteger(1))
	c.Assert(err, qt.IsNil)

	// Integer key
	err = ht.Set(values.NewInteger(42), values.NewInteger(2))
	c.Assert(err, qt.IsNil)

	// String key
	err = ht.Set(values.NewString("str"), values.NewInteger(3))
	c.Assert(err, qt.IsNil)

	// Boolean key
	err = ht.Set(values.TrueValue, values.NewInteger(4))
	c.Assert(err, qt.IsNil)

	// Character key
	err = ht.Set(values.NewCharacter('x'), values.NewInteger(5))
	c.Assert(err, qt.IsNil)

	c.Assert(ht.Size(), qt.Equals, 5)

	// Look up with new pointers — structural equality
	val, found, err := ht.Get(values.NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(2))

	val, found, err = ht.Get(values.NewSymbol("sym"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(1))
}

func TestHashtable_OverwriteKey(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()

	err := ht.Set(values.NewSymbol("k"), values.NewInteger(1))
	c.Assert(err, qt.IsNil)
	err = ht.Set(values.NewSymbol("k"), values.NewInteger(2))
	c.Assert(err, qt.IsNil)

	c.Assert(ht.Size(), qt.Equals, 1)
	val, found, err := ht.Get(values.NewSymbol("k"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(2))
}

// TestHashtable_SchemeString_CompoundCycleBounded confirms a cycle that passes
// THROUGH a hashtable is bounded (renders "...", does not overflow the Go
// stack). Without path-scoped marking threaded through the hashtable renderer,
// pair -> hashtable -> pair-value -> hashtable -> ... recurses forever.
func TestHashtable_SchemeString_CompoundCycleBounded(t *testing.T) {
	ht := values.NewEmptyHashtable()
	p := values.NewCons(values.NewSymbol("a"), values.EmptyList)
	err := ht.Set(values.NewSymbol("k"), p)
	qt.Assert(t, err, qt.IsNil)
	p.SetCdr(ht) // pair -> hashtable -> pair (value) -> hashtable -> ...

	// Reaching this line at all proves no stack overflow. The cycle through the
	// hashtable must collapse to "..." rather than recurse forever.
	got := p.SchemeString()
	qt.Assert(t, got, qt.Contains, "...")
}

// TestHashtable_SchemeString_SharedAcyclic confirms acyclic sharing through a
// hashtable value renders in full (path-scoped, not all-visited).
func TestHashtable_SchemeString_SharedAcyclic(t *testing.T) {
	shared := values.List(values.NewInteger(1), values.NewInteger(2))
	ht := values.NewEmptyHashtable()
	err := ht.Set(values.NewSymbol("k"), shared)
	qt.Assert(t, err, qt.IsNil)
	// The list (shared . ht-with-shared) shares `shared` across two sibling
	// paths but is acyclic; both occurrences must render in full.
	dag := values.NewCons(shared, values.NewCons(ht, values.EmptyList))
	qt.Assert(t, dag.SchemeString(), qt.Equals, "((1 2) #hash((k . (1 2))))")
}
