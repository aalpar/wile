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

	"github.com/aalpar/wile/pkg/values"
)

// TestImmutable_ImplementorsAnswerBothWays is the surviving half of what used to
// be pkg/environment's ImmutableLiterals table, relocated when the side set was
// deleted: the property is now entirely a values one.
//
// One row per implementing type, BOTH arms of each, because the underlying
// fields have opposite polarity (String.immutable vs Hashtable.mutable) and the
// interface exists to normalize exactly that.
//
// *Pair is deliberately absent and that absence is the decision, not an
// oversight: a flag word is ~25% growth on the 32-byte cons cell, so pair
// literals are mutable. *Box and *Record are absent for a different reason —
// nothing marks either and nothing can (a record has no reader syntax, so it
// cannot appear in a quoted literal; a box can, and a quoted literal box is
// therefore mutable, which is extension semantics rather than an R7RS deviation
// since boxes are not R7RS).
func TestImmutable_ImplementorsAnswerBothWays(t *testing.T) {
	markedVector := values.NewVector(values.NewInteger(1))
	values.MarkImmutable(markedVector)
	markedByteVector := values.NewByteVectorFromBytes(1)
	values.MarkImmutable(markedByteVector)

	tests := []struct {
		name string
		val  values.Value
		want bool
	}{
		{"String immutable", values.NewString("abc"), true},
		{"String mutable", values.NewMutableString("abc"), false},
		{"Hashtable immutable", values.NewEmptyHashtable().Copy(false), true},
		{"Hashtable mutable", values.NewEmptyHashtable(), false},
		{"Vector marked", markedVector, true},
		{"Vector unmarked", values.NewVector(values.NewInteger(1)), false},
		{"ByteVector marked", markedByteVector, true},
		{"ByteVector unmarked", values.NewByteVectorFromBytes(1), false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			q, ok := tt.val.(values.Immutable)
			if !ok {
				t.Fatalf("%T does not implement values.Immutable", tt.val)
			}
			got := q.IsImmutable()
			if got != tt.want {
				t.Fatalf("IsImmutable(%s) = %v, want %v", tt.val.SchemeString(), got, tt.want)
			}
		})
	}
}

// TestMarkImmutable_IsTotalAndOnlyFlagsThePostHocTypes pins both halves of
// MarkImmutable's contract.
//
// It is TOTAL — a non-flaggable value answers false rather than erroring —
// because "this type cannot carry the constraint" is the compiler's normal case:
// a literal is a tree and most of its nodes are scalars.
//
// It reaches only the types whose flag is set AFTER construction. *String and
// *Hashtable are constructor-determined and must NOT be reachable: a post-hoc
// setter on a hashtable would destroy the write-once property its lock-free
// contract rests on, and *String's polarity is decided by which of NewString /
// NewMutableString ran.
func TestMarkImmutable_IsTotalAndOnlyFlagsThePostHocTypes(t *testing.T) {
	tests := []struct {
		name    string
		val     values.Value
		flagged bool
	}{
		{"Vector", values.NewVector(values.NewInteger(1)), true},
		{"ByteVector", values.NewByteVectorFromBytes(1), true},
		{"Pair is not flaggable", values.NewCons(values.NewInteger(1), values.EmptyList), false},
		{"String is constructor-determined", values.NewMutableString("abc"), false},
		{"Hashtable is constructor-determined", values.NewEmptyHashtable(), false},
		{"Symbol", values.NewSymbol("s"), false},
		{"Integer", values.NewInteger(7), false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := values.MarkImmutable(tt.val)
			if got != tt.flagged {
				t.Fatalf("MarkImmutable(%s) = %v, want %v", tt.val.SchemeString(), got, tt.flagged)
			}
			// A type that reports it took the flag must read back as immutable,
			// and one that reports it did not must be unchanged. Without this the
			// bool could drift from what actually happened.
			q, ok := tt.val.(values.Immutable)
			if !ok {
				return
			}
			if q.IsImmutable() != tt.flagged {
				t.Fatalf("%s: MarkImmutable returned %v but IsImmutable reads %v",
					tt.name, tt.flagged, q.IsImmutable())
			}
		})
	}
}

// TestMarkImmutable_MutatorsRefuseAfterFlagging pins the self-enforcement half:
// a flagged aggregate's own Set refuses, so a caller reaching the setter from a
// path nobody gated still gets the refusal. This is why the flag lives on the
// value rather than only at the primitives.
func TestMarkImmutable_MutatorsRefuseAfterFlagging(t *testing.T) {
	v := values.NewVector(values.NewInteger(1))
	err := v.Set(0, values.NewInteger(2))
	if err != nil {
		t.Fatalf("an unflagged vector must accept Set, got %v", err)
	}
	values.MarkImmutable(v)
	err = v.Set(0, values.NewInteger(3))
	if err == nil {
		t.Fatalf("a flagged vector's Set must refuse")
	}

	b := values.NewByteVectorFromBytes(1)
	err = b.Set(0, values.NewByte(2))
	if err != nil {
		t.Fatalf("an unflagged bytevector must accept Set, got %v", err)
	}
	values.MarkImmutable(b)
	err = b.Set(0, values.NewByte(3))
	if err == nil {
		t.Fatalf("a flagged bytevector's Set must refuse")
	}
}
