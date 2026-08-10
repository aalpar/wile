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

	"github.com/aalpar/wile/pkg/values"
)

func TestImmutableLiterals_MarkContains(t *testing.T) {
	set := &ImmutableLiterals{}
	p := values.NewCons(values.NewInteger(1), values.EmptyList)
	q := values.NewCons(values.NewInteger(1), values.EmptyList)

	if set.Contains(p) {
		t.Fatalf("unmarked pair must not be immutable")
	}
	set.Mark(p)
	if !set.Contains(p) {
		t.Fatalf("marked pair must be immutable")
	}
	// Membership is by pointer identity, not equal? — a distinct equal pair is mutable.
	if set.Contains(q) {
		t.Fatalf("distinct equal pair must not be marked (identity, not equal?)")
	}
}

// TestImmutableLiterals_IsImmutable covers the predicate that spans both
// immutability mechanisms — the intrinsic values.Immutable arm and the
// engine-scoped side set — with one row per type and BOTH arms of each.
//
// *Box and *Record are deliberately absent rather than missing. Both are
// non-flaggable by decision: nothing marks either and nothing can (records have
// no reader syntax, so a record can never appear in a quoted literal; boxes have
// no side-set fallback because the side set only ever receives pairs, vectors
// and bytevectors). The consequence is that a quoted literal box is mutable —
// (set-box! (car '(#&1)) 2) succeeds — which is extension semantics, not an
// R7RS deviation, since boxes are not R7RS. Adding a row for either would assert
// a property this design does not intend to hold.
func TestImmutableLiterals_IsImmutable(t *testing.T) {
	set := &ImmutableLiterals{}

	markedPair := values.NewCons(values.NewInteger(1), values.EmptyList)
	set.Mark(markedPair)
	markedVector := values.NewVector(values.NewInteger(1))
	set.Mark(markedVector)
	markedByteVector := values.NewByteVectorFromBytes(1)
	set.Mark(markedByteVector)

	// Marked in set, but queried through a nil receiver below. Marking it is what
	// makes that row load-bearing: the answer must be false because the receiver
	// is nil, not because the value was never marked.
	nilReceiverPair := values.NewCons(values.NewInteger(2), values.EmptyList)
	set.Mark(nilReceiverPair)

	tests := []struct {
		name string
		set  *ImmutableLiterals
		val  values.Value
		want bool
	}{
		{"String immutable", set, values.NewString("abc"), true},
		{"String mutable", set, values.NewMutableString("abc"), false},
		{"Hashtable immutable", set, values.NewEmptyHashtable().Copy(false), true},
		{"Hashtable mutable", set, values.NewEmptyHashtable(), false},
		{"Pair marked", set, markedPair, true},
		{"Pair unmarked", set, values.NewCons(values.NewInteger(1), values.EmptyList), false},
		{"Vector marked", set, markedVector, true},
		{"Vector unmarked", set, values.NewVector(values.NewInteger(1)), false},
		{"ByteVector marked", set, markedByteVector, true},
		{"ByteVector unmarked", set, values.NewByteVectorFromBytes(1), false},
		{"Symbol never flaggable", set, values.NewSymbol("s"), false},
		{"Integer never flaggable", set, values.NewInteger(7), false},
		// The intrinsic arm answers without a receiver; the side-set arm cannot.
		{"nil receiver, flagged value", nil, values.NewString("abc"), true},
		{"nil receiver, side-set value", nil, nilReceiverPair, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.set.IsImmutable(tt.val)
			if got != tt.want {
				t.Fatalf("IsImmutable(%s) = %v, want %v", tt.val.SchemeString(), got, tt.want)
			}
		})
	}
}
