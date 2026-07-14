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
	"reflect"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestValue_ImplementorsAreGoComparable enforces the Value contract's
// comparability requirement (see the Value doc comment in values.go).
//
// Why a test and not a compile-time check: Go comparability is a static property
// of a type with NO method set. It cannot be asserted (`v.(comparable)` does not
// compile), and the `comparable` *constraint* happily accepts an interface like
// Value and then panics at runtime — so generics are not a safety net either. The
// only query is reflect.TypeOf(v).Comparable(), which means the invariant can be
// enforced at test time or not at all.
//
// What breaks without it: EqIdentity (utils.go) is a bare `a == b` on the eq?
// hot path. A slice-, map-, or func-backed Value faults there with
// "comparing uncomparable type", taking down the host — not returning #f.
//
// The receiver, not the underlying type, is what decides. Vector is []Value and
// ByteVector wraps a slice, yet both are safe: their methods take POINTER
// receivers, so the boxed dynamic type is *Vector — a pointer, hence comparable.
// A value receiver on a slice-backed type is what puts the naked slice in the
// interface. That is the mistake this test catches.
func TestValue_ImplementorsAreGoComparable(t *testing.T) {
	for _, exemplar := range allValueExemplars {
		rt := reflect.TypeOf(exemplar)
		qt.Assert(t, rt.Comparable(), qt.IsTrue,
			qt.Commentf("Value implementor %s is not Go-comparable — eq? (values.EqIdentity) "+
				"would panic on it. Give the type pointer receivers so the boxed dynamic type "+
				"is a pointer, or stop implementing Value.", rt))
	}
}

// TestDeepEqualer_ImplementorsAreGoComparable enforces the stronger requirement
// DeepEqualer states in prose: an implementor MUST be pointer-shaped.
//
// Equal keys its visited set on equalPairKey{a, b}, which HASHES both elements.
// Hashing is a strictly stronger demand than comparing: `==` on two interfaces
// holding *different* non-comparable types answers false without faulting, but a
// map key hashes unconditionally. So a DeepEqualer that slipped through would
// panic on the cycle-detection path even against an unrelated operand.
func TestDeepEqualer_ImplementorsAreGoComparable(t *testing.T) {
	seen := 0
	for _, exemplar := range allValueExemplars {
		_, ok := exemplar.(DeepEqualer)
		if !ok {
			continue
		}
		seen++
		rt := reflect.TypeOf(exemplar)
		qt.Assert(t, rt.Comparable(), qt.IsTrue,
			qt.Commentf("DeepEqualer %s is not Go-comparable — Equal would panic hashing it "+
				"as a visited-set key", rt))
	}
	// Guard against the roster silently losing every container: a green run over
	// an empty set proves nothing. Pair, Vector, Record, Box, Hashtable,
	// CompileTimeValue are the implementors as of this writing.
	qt.Assert(t, seen >= 4, qt.IsTrue,
		qt.Commentf("expected the exemplar roster to carry several DeepEqualers, found %d — "+
			"has allValueExemplars lost its container types?", seen))
}
