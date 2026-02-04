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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// helper to build a hashtable from key-value pairs for tests.
func makeHT(kvs ...any) *Hashtable {
	ht := NewEmptyHashtable()
	for i := 0; i < len(kvs); i += 2 {
		_ = ht.Set(kvs[i].(Value), kvs[i+1].(Value))
	}
	return ht
}

func TestHashtable_EqualTo(t *testing.T) {
	tcs := []struct {
		name string
		in0  Value
		in1  Value
		out  bool
	}{
		{
			name: "different keys",
			in0:  makeHT(NewSymbol("key1"), NewInteger(1)),
			in1:  makeHT(NewSymbol("key2"), NewInteger(1)),
			out:  false,
		},
		{
			name: "equal contents",
			in0:  makeHT(NewSymbol("key1"), NewInteger(1)),
			in1:  makeHT(NewSymbol("key1"), NewInteger(1)),
			out:  true,
		},
		{
			name: "different values",
			in0:  makeHT(NewSymbol("key1"), NewInteger(1)),
			in1:  makeHT(NewSymbol("key1"), NewInteger(2)),
			out:  false,
		},
		{
			name: "different sizes",
			in0:  makeHT(NewSymbol("a"), NewInteger(1), NewSymbol("b"), NewInteger(2)),
			in1:  makeHT(NewSymbol("a"), NewInteger(1)),
			out:  false,
		},
		{
			name: "not a hashtable",
			in0:  NewEmptyHashtable(),
			in1:  NewInteger(1),
			out:  false,
		},
		{
			name: "empty hashtables equal",
			in0:  NewEmptyHashtable(),
			in1:  NewEmptyHashtable(),
			out:  true,
		},
		{
			name: "integer keys",
			in0:  makeHT(NewInteger(1), NewString("one")),
			in1:  makeHT(NewInteger(1), NewString("one")),
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
	ht := NewEmptyHashtable()
	c.Assert(ht.SchemeString(), qt.Equals, "#hash()")

	ht2 := makeHT(NewSymbol("a"), NewInteger(1))
	c.Assert(ht2.SchemeString(), qt.Equals, "#hash((a . 1))")
}

func TestHashtable_NewEmptyHashtable(t *testing.T) {
	c := qt.New(t)
	ht := NewEmptyHashtable()
	c.Assert(ht.Size(), qt.Equals, 0)
	c.Assert(ht.IsVoid(), qt.IsFalse)
}

func TestHashtable_GetSetDelete(t *testing.T) {
	c := qt.New(t)
	ht := NewEmptyHashtable()

	// Set and Get — uses structural equality, not pointer identity
	err := ht.Set(NewSymbol("key"), NewInteger(42))
	c.Assert(err, qt.IsNil)

	val, found, err := ht.Get(NewSymbol("key"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, SchemeEquals, NewInteger(42))

	// HasKey
	found, err = ht.HasKey(NewSymbol("key"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)

	found, err = ht.HasKey(NewSymbol("missing"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsFalse)

	// Get missing
	_, found, err = ht.Get(NewSymbol("missing"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsFalse)

	// Delete
	err = ht.Delete(NewSymbol("key"))
	c.Assert(err, qt.IsNil)
	c.Assert(ht.Size(), qt.Equals, 0)
}

func TestHashtable_KeysValues(t *testing.T) {
	c := qt.New(t)

	ht := NewEmptyHashtable()
	c.Assert(ht.Keys(), qt.Equals, EmptyList)
	c.Assert(ht.Values(), qt.Equals, EmptyList)

	err := ht.Set(NewSymbol("a"), NewInteger(1))
	c.Assert(err, qt.IsNil)

	keys := ht.Keys()
	c.Assert(keys.Length(), qt.Equals, 1)
	c.Assert(keys.Car(), SchemeEquals, NewSymbol("a"))

	vals := ht.Values()
	c.Assert(vals.Length(), qt.Equals, 1)
	c.Assert(vals.Car(), SchemeEquals, NewInteger(1))
}

func TestHashtable_CopyClear(t *testing.T) {
	c := qt.New(t)

	ht := NewEmptyHashtable()
	err := ht.Set(NewSymbol("a"), NewInteger(1))
	c.Assert(err, qt.IsNil)

	// Copy is independent
	cp := ht.Copy()
	c.Assert(cp.Size(), qt.Equals, 1)

	err = ht.Set(NewSymbol("b"), NewInteger(2))
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
	ht := NewEmptyHashtable()
	key := NewCons(NewInteger(1), NewInteger(2))

	err := ht.Set(key, NewInteger(1))
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
	ht := NewEmptyHashtable()

	// Symbol key
	err := ht.Set(NewSymbol("sym"), NewInteger(1))
	c.Assert(err, qt.IsNil)

	// Integer key
	err = ht.Set(NewInteger(42), NewInteger(2))
	c.Assert(err, qt.IsNil)

	// String key
	err = ht.Set(NewString("str"), NewInteger(3))
	c.Assert(err, qt.IsNil)

	// Boolean key
	err = ht.Set(TrueValue, NewInteger(4))
	c.Assert(err, qt.IsNil)

	// Character key
	err = ht.Set(NewCharacter('x'), NewInteger(5))
	c.Assert(err, qt.IsNil)

	c.Assert(ht.Size(), qt.Equals, 5)

	// Look up with new pointers — structural equality
	val, found, err := ht.Get(NewInteger(42))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, SchemeEquals, NewInteger(2))

	val, found, err = ht.Get(NewSymbol("sym"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, SchemeEquals, NewInteger(1))
}

func TestHashtable_OverwriteKey(t *testing.T) {
	c := qt.New(t)
	ht := NewEmptyHashtable()

	err := ht.Set(NewSymbol("k"), NewInteger(1))
	c.Assert(err, qt.IsNil)
	err = ht.Set(NewSymbol("k"), NewInteger(2))
	c.Assert(err, qt.IsNil)

	c.Assert(ht.Size(), qt.Equals, 1)
	val, found, err := ht.Get(NewSymbol("k"))
	c.Assert(err, qt.IsNil)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, SchemeEquals, NewInteger(2))
}
