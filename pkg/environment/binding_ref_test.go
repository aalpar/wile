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

package environment

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestBindingRefMapKeyIdentity pins the == matrix that every downstream
// map[BindingRef]T leans on. Two distinct frame pointers stand in for two
// distinct local scopes; the refs need only differ by identity, not hold real
// bindings.
func TestBindingRefMapKeyIdentity(t *testing.T) {
	fA := &LocalEnvironmentFrame{}
	fB := &LocalEnvironmentFrame{}

	locA0 := LocalRef(BindingID{Frame: fA, Slot: 0})
	locA0bis := LocalRef(BindingID{Frame: fA, Slot: 0}) // independently built, same identity
	locA1 := LocalRef(BindingID{Frame: fA, Slot: 1})
	locB0 := LocalRef(BindingID{Frame: fB, Slot: 0})

	globX := GlobalRef("x")
	globXbis := GlobalRef("x")
	globY := GlobalRef("y")

	locZero := LocalRef(BindingID{}) // kind==Local, zero payload
	globEmpty := GlobalRef("")       // kind==Global, empty payload
	invalid := BindingRef{}          // zero value, kind==Invalid

	cases := []struct {
		name string
		a, b BindingRef
		want bool // expected a == b
	}{
		{"local same identity", locA0, locA0bis, true},
		{"local diff slot", locA0, locA1, false},
		{"local diff frame", locA0, locB0, false},
		{"global same key", globX, globXbis, true},
		{"global diff key", globX, globY, false},
		{"cross-kind local vs global", locA0, globX, false},
		// The discriminant, not the payload, must separate the arms:
		{"cross-kind degenerate (local zero vs global empty)", locZero, globEmpty, false},
		{"invalid vs local-zero", invalid, locZero, false},
		{"invalid vs global-empty", invalid, globEmpty, false},
		{"invalid vs invalid", invalid, BindingRef{}, true},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, tc.a == tc.b, qt.Equals, tc.want)
		})
	}
}

// TestBindingRefAsMapKey proves the identity matrix through actual map
// semantics: equal refs collapse to one entry, unequal refs stay distinct, and
// a lookup with an independently-built equal ref hits.
func TestBindingRefAsMapKey(t *testing.T) {
	fA := &LocalEnvironmentFrame{}
	fB := &LocalEnvironmentFrame{}

	m := map[BindingRef]int{}
	m[LocalRef(BindingID{Frame: fA, Slot: 0})] = 1
	m[GlobalRef("x")] = 2

	// Equal ref, independently constructed → hit.
	v, ok := m[LocalRef(BindingID{Frame: fA, Slot: 0})]
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, qt.Equals, 1)

	v, ok = m[GlobalRef("x")]
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, v, qt.Equals, 2)

	// Unequal ref (different frame) → miss.
	_, ok = m[LocalRef(BindingID{Frame: fB, Slot: 0})]
	qt.Assert(t, ok, qt.IsFalse)

	// Re-inserting an equal ref collapses to the same entry.
	m[LocalRef(BindingID{Frame: fA, Slot: 0})] = 9
	qt.Assert(t, len(m), qt.Equals, 2) // still {localA0, globX}

	// A local and a global with matching zero payloads are distinct keys.
	m[LocalRef(BindingID{})] = 10
	m[GlobalRef("")] = 11
	qt.Assert(t, len(m), qt.Equals, 4)
}

// TestBindingRefPredicates sanity-checks the discriminant accessors.
func TestBindingRefPredicates(t *testing.T) {
	loc := LocalRef(BindingID{Frame: &LocalEnvironmentFrame{}, Slot: 0})
	glob := GlobalRef("x")
	invalid := BindingRef{}

	qt.Assert(t, loc.IsLocal(), qt.IsTrue)
	qt.Assert(t, loc.IsGlobal(), qt.IsFalse)
	qt.Assert(t, loc.IsValid(), qt.IsTrue)

	qt.Assert(t, glob.IsGlobal(), qt.IsTrue)
	qt.Assert(t, glob.IsLocal(), qt.IsFalse)
	qt.Assert(t, glob.IsValid(), qt.IsTrue)

	qt.Assert(t, invalid.IsValid(), qt.IsFalse)
	qt.Assert(t, invalid.IsLocal(), qt.IsFalse)
	qt.Assert(t, invalid.IsGlobal(), qt.IsFalse)
}
