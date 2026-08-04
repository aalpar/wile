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
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// helper to build a hashtable from key-value pairs for tests.
func makeHT(kvs ...any) *values.Hashtable {
	ht := values.NewEmptyHashtable()
	for i := 0; i < len(kvs); i += 2 {
		ht.Set(kvs[i].(values.Value), kvs[i+1].(values.Value))
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
	ht.Set(values.NewSymbol("key"), values.NewInteger(42))

	val, found := ht.Get(values.NewSymbol("key"))
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(42))

	// HasKey
	found = ht.HasKey(values.NewSymbol("key"))
	c.Assert(found, qt.IsTrue)

	found = ht.HasKey(values.NewSymbol("missing"))
	c.Assert(found, qt.IsFalse)

	// Get missing
	_, found = ht.Get(values.NewSymbol("missing"))
	c.Assert(found, qt.IsFalse)

	// Delete
	ht.Delete(values.NewSymbol("key"))
	c.Assert(ht.Size(), qt.Equals, 0)
}

func TestHashtable_KeysValues(t *testing.T) {
	c := qt.New(t)

	ht := values.NewEmptyHashtable()
	c.Assert(ht.Keys(), qt.Equals, values.EmptyList)
	c.Assert(ht.Values(), qt.Equals, values.EmptyList)

	ht.Set(values.NewSymbol("a"), values.NewInteger(1))

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
	ht.Set(values.NewSymbol("a"), values.NewInteger(1))

	// Copy is independent
	cp := ht.Copy()
	c.Assert(cp.Size(), qt.Equals, 1)

	ht.Set(values.NewSymbol("b"), values.NewInteger(2))
	c.Assert(ht.Size(), qt.Equals, 2)
	c.Assert(cp.Size(), qt.Equals, 1)

	// Clear
	ht.Clear()
	c.Assert(ht.Size(), qt.Equals, 0)
	c.Assert(cp.Size(), qt.Equals, 1)
}

// TestHashtable_ContainerKeyIsAdmitted is the inverse of the test it replaces.
//
// A container key used to be REJECTED — Set type-asserted Hashable and returned
// an error for anything else, which is what TestHashtable_NonHashableKey
// asserted. HashtableKind moved the hash to the table, so admission is gone and
// every kind takes every key; the four error returns went with it. The behaviour
// under test flipped, so the test flipped rather than being deleted.
func TestHashtable_ContainerKeyIsAdmitted(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()
	key := values.NewCons(values.NewInteger(1), values.NewInteger(2))

	ht.Set(key, values.NewInteger(1))
	c.Assert(ht.Size(), qt.Equals, 1)

	// A DISTINCT but equal? pair finds it: the table hashes with EqualHash and
	// compares with Equal, so structural identity is what makes it one key.
	lookup := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	val, found := ht.Get(lookup)
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(1))
	c.Assert(ht.HasKey(lookup), qt.IsTrue)

	ht.Delete(lookup)
	c.Assert(ht.Size(), qt.Equals, 0)
	c.Assert(ht.HasKey(key), qt.IsFalse)
}

func TestHashtable_VariousKeyTypes(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()

	// Symbol key
	ht.Set(values.NewSymbol("sym"), values.NewInteger(1))

	// Integer key
	ht.Set(values.NewInteger(42), values.NewInteger(2))

	// String key
	ht.Set(values.NewString("str"), values.NewInteger(3))

	// Boolean key
	ht.Set(values.TrueValue, values.NewInteger(4))

	// Character key
	ht.Set(values.NewCharacter('x'), values.NewInteger(5))

	c.Assert(ht.Size(), qt.Equals, 5)

	// Look up with new pointers — structural equality
	val, found := ht.Get(values.NewInteger(42))
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(2))

	val, found = ht.Get(values.NewSymbol("sym"))
	c.Assert(found, qt.IsTrue)
	c.Assert(val, valuestest.SchemeEquals, values.NewInteger(1))
}

func TestHashtable_OverwriteKey(t *testing.T) {
	c := qt.New(t)
	ht := values.NewEmptyHashtable()

	ht.Set(values.NewSymbol("k"), values.NewInteger(1))
	ht.Set(values.NewSymbol("k"), values.NewInteger(2))

	c.Assert(ht.Size(), qt.Equals, 1)
	val, found := ht.Get(values.NewSymbol("k"))
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
	ht.Set(values.NewSymbol("k"), p)
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
	ht.Set(values.NewSymbol("k"), shared)
	// The list (shared . ht-with-shared) shares `shared` across two sibling
	// paths but is acyclic; both occurrences must render in full.
	dag := values.NewCons(shared, values.NewCons(ht, values.EmptyList))
	qt.Assert(t, dag.SchemeString(), qt.Equals, "((1 2) #hash((k . (1 2))))")
}

// TestHashtableKinds pins the R6RS inversion: the hash belongs to the TABLE, so
// which objects count as one key is the table's choice, not the key's.
func TestHashtableKinds(t *testing.T) {
	tcs := []struct {
		name     string
		kind     values.HashtableKind
		k1, k2   values.Value
		wantSame bool
	}{
		{"equal: distinct equal lists are one key", values.HashtableEqual,
			values.NewCons(values.NewInteger(1), values.EmptyList),
			values.NewCons(values.NewInteger(1), values.EmptyList), true},
		{"equal: distinct equal strings are one key", values.HashtableEqual,
			values.NewString("a"), values.NewMutableString("a"), true},
		{"equal: distinct vectors are one key", values.HashtableEqual,
			values.NewVector(values.NewInteger(1)), values.NewVector(values.NewInteger(1)), true},
		{"eqv: distinct equal lists are two keys", values.HashtableEqv,
			values.NewCons(values.NewInteger(1), values.EmptyList),
			values.NewCons(values.NewInteger(1), values.EmptyList), false},
		{"eqv: exact across representations is one key", values.HashtableEqv,
			values.NewInteger(5), values.NewBigIntegerFromInt64(5), true},
		{"eqv: distinct equal strings are two keys", values.HashtableEqv,
			values.NewString("a"), values.NewMutableString("a"), false},
		{"eq: same-named symbols are one key", values.HashtableEq,
			values.NewSymbol("s"), values.NewSymbol("s"), true},
		{"eq: distinct equal strings are two keys", values.HashtableEq,
			values.NewString("a"), values.NewMutableString("a"), false},
		{"eq: distinct equal lists are two keys", values.HashtableEq,
			values.NewCons(values.NewInteger(1), values.EmptyList),
			values.NewCons(values.NewInteger(1), values.EmptyList), false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			ht := values.NewHashtable(tc.kind)
			ht.Set(tc.k1, values.NewInteger(1))
			ht.Set(tc.k2, values.NewInteger(2))
			want := 1
			if !tc.wantSame {
				want = 2
			}
			qt.Assert(t, ht.Size(), qt.Equals, want)
			v, found := ht.Get(tc.k1)
			qt.Assert(t, found, qt.IsTrue)
			if tc.wantSame {
				qt.Assert(t, v, valuestest.SchemeEquals, values.NewInteger(2))
			}
		})
	}
}

// TestHashtableZeroValueIsEqualKind pins the zero-value choice. A bare
// &Hashtable{} — which NewEmptyHashtable returns and Copy inherits — must keep
// today's equal? semantics, so HashtableEqual is deliberately the zero value.
// Reordering the iota silently reinterprets every existing table.
func TestHashtableZeroValueIsEqualKind(t *testing.T) {
	qt.Assert(t, values.NewEmptyHashtable().Kind(), qt.Equals, values.HashtableEqual)
	qt.Assert(t, values.HashtableEqual, qt.Equals, values.HashtableKind(0))
}

// TestHashtableCyclicKeyTerminates is what lifting key admission buys and what it
// risks. A cyclic pair key is safe because (*Pair).EqualTo delegates to the
// iterative values.Equal — it is NOT Go recursion, contrary to what
// hashtable.go's comment claimed before this change.
func TestHashtableCyclicKeyTerminates(t *testing.T) {
	cyc := values.NewCons(values.NewInteger(1), values.EmptyList)
	cyc.SetCdr(cyc)
	done := make(chan struct{})
	go func() {
		ht := values.NewHashtable(values.HashtableEqual)
		ht.Set(cyc, values.NewInteger(1))
		_, _ = ht.Get(cyc)
		close(done)
	}()
	select {
	case <-done:
	case <-time.After(5 * time.Second):
		t.Fatal("cyclic pair key did not terminate")
	}
}
