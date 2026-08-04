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
	"math"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestEqualHash_Contract pins the one-directional contract: Equal(a, b) implies
// EqualHash(a) == EqualHash(b). The converse is NOT asserted anywhere.
func TestEqualHash_Contract(t *testing.T) {
	list := func(ns ...int64) values.Value {
		q := values.Value(values.EmptyList)
		for i := len(ns) - 1; i >= 0; i-- {
			q = values.NewCons(values.NewInteger(ns[i]), q)
		}
		return q
	}
	vec := values.NewVector(values.NewInteger(1), values.NewString("a"))
	vec2 := values.NewVector(values.NewInteger(1), values.NewString("a"))
	pointType := values.NewRecordType(values.NewSymbol("point"),
		[]*values.Symbol{values.NewSymbol("x"), values.NewSymbol("y")})
	rec := func(rt *values.RecordType, fs ...values.Value) values.Value {
		q, err := values.NewRecord(rt, fs)
		qt.Assert(t, err, qt.IsNil)
		return q
	}
	tcs := []struct {
		name string
		a, b values.Value
	}{
		{"identical fixnums", values.NewInteger(7), values.NewInteger(7)},
		{"exact across representations", values.NewInteger(5), values.NewBigIntegerFromInt64(5)},
		{"equal strings", values.NewString("abc"), values.NewMutableString("abc")},
		{"same-named symbols", values.NewSymbol("s"), values.NewSymbol("s")},
		{"equal flat lists", list(1, 2, 3), list(1, 2, 3)},
		{"equal nested lists", values.NewCons(list(1, 2), list(3)), values.NewCons(list(1, 2), list(3))},
		{"equal vectors", vec, vec2},
		{"equal boxes", values.NewBox(list(1, 2)), values.NewBox(list(1, 2))},
		// Records share a RecordType POINTER when equal? — see Record.EqualComponents.
		// The hash must therefore agree on distinct record OBJECTS of one type, which
		// is the case equal-hash's Scheme callers hit first.
		{"equal records of one type",
			rec(pointType, values.NewInteger(1), values.NewInteger(2)),
			rec(pointType, values.NewInteger(1), values.NewInteger(2))},
		{"equal records with container fields",
			rec(pointType, list(1, 2), values.NewString("a")),
			rec(pointType, list(1, 2), values.NewMutableString("a"))},
		{"empty lists", values.EmptyList, values.EmptyList},
		{"NaN against NaN", values.NewFloat(math.NaN()), values.NewFloat(math.Inf(1) - math.Inf(1))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.Equal(tc.a, tc.b), qt.IsTrue,
				qt.Commentf("test case is malformed: inputs are not equal?"))
			qt.Assert(t, values.EqualHash(tc.a), qt.Equals, values.EqualHash(tc.b))
		})
	}
}

// TestEqualHash_BisimilarCyclesAgree is the pin on the reason EqualHash hashes an
// UNFOLDING and keeps no visited set.
//
// A = (1 2 . A) is a 2-cycle; B = (1 2 1 2 . B) is a 4-cycle. They are equal? —
// equal.go's coinductive visited set closes the walk when it re-encounters the
// (A, B) pair. Any hash that folds per DISTINCT NODE (including a visited map
// keyed on first-visit ordinal, which an earlier design specified) counts 2 for A
// and 4 for B and returns different hashes, violating the contract. Their
// unfoldings are identical, so a budgeted unfolding walk agrees.
func TestEqualHash_BisimilarCyclesAgree(t *testing.T) {
	cyc := func(ns ...int64) values.Value {
		head := values.NewCons(values.NewInteger(ns[0]), values.EmptyList)
		tail := head
		for _, n := range ns[1:] {
			next := values.NewCons(values.NewInteger(n), values.EmptyList)
			tail.SetCdr(next)
			tail = next
		}
		tail.SetCdr(head)
		return head
	}
	a := cyc(1, 2)
	b := cyc(1, 2, 1, 2)
	qt.Assert(t, values.Equal(a, b), qt.IsTrue,
		qt.Commentf("premise: the two cycles must be equal? for the pin to mean anything"))
	qt.Assert(t, values.EqualHash(a), qt.Equals, values.EqualHash(b))
}

// TestEqualHash_TerminatesOnCycles asserts R6RS's written requirement. A hang here
// is a failure, so the walk runs under a deadline rather than inline.
func TestEqualHash_TerminatesOnCycles(t *testing.T) {
	self := values.NewCons(values.NewInteger(1), values.EmptyList)
	self.SetCdr(self)
	v := values.NewVector(values.NewInteger(0))
	(*v)[0] = v
	// A record field is MUTABLE (Record.SetField), so a record can contain itself.
	// Record.EqualComponents' own comment names this as the cycle that used to
	// overflow the host stack; the budget is what terminates it here.
	selfRec, err := values.NewRecord(
		values.NewRecordType(values.NewSymbol("node"), []*values.Symbol{values.NewSymbol("self")}),
		[]values.Value{values.EmptyList})
	qt.Assert(t, err, qt.IsNil)
	selfRec.SetField(0, selfRec)
	tcs := []struct {
		name string
		v    values.Value
	}{
		{"self-referential pair", self},
		{"self-referential vector", v},
		{"box containing its own cycle", values.NewBox(self)},
		{"self-referential record", selfRec},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			done := make(chan uint64, 1)
			go func() {
				done <- values.EqualHash(tc.v)
			}()
			select {
			case <-done:
			case <-time.After(5 * time.Second):
				t.Fatal("EqualHash did not terminate on cyclic input")
			}
		})
	}
}

// TestEqualHash_DistinguishesShape is a quality check, not a contract: the hash
// would be correct if it returned a constant, and useless. These must differ.
func TestEqualHash_DistinguishesShape(t *testing.T) {
	one := values.NewInteger(1)
	two := values.NewInteger(2)
	fields := []*values.Symbol{values.NewSymbol("x"), values.NewSymbol("y")}
	pointType := values.NewRecordType(values.NewSymbol("point"), fields)
	otherType := values.NewRecordType(values.NewSymbol("interval"), fields)
	rec := func(rt *values.RecordType, fs ...values.Value) values.Value {
		q, err := values.NewRecord(rt, fs)
		qt.Assert(t, err, qt.IsNil)
		return q
	}
	tcs := []struct {
		name string
		a, b values.Value
	}{
		{"order matters", values.NewCons(one, two), values.NewCons(two, one)},
		{"list vs vector", values.NewCons(one, values.EmptyList), values.NewVector(one)},
		{"symbol vs string", values.NewSymbol("a"), values.NewString("a")},
		{"nesting depth", values.NewCons(values.NewCons(one, two), values.EmptyList),
			values.NewCons(one, values.NewCons(two, values.EmptyList))},
		// These two are the reason the *Record arm exists. Without it both sides of
		// each pair hash to the same value — reflect.TypeOf is "*values.Record" for
		// every record of every type — and equal-hash is a constant on the one kind
		// of object a Scheme user is most likely to define.
		{"record field values", rec(pointType, one, two), rec(pointType, two, one)},
		{"distinct record types, same fields",
			rec(pointType, one, two), rec(otherType, one, two)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.EqualHash(tc.a), qt.Not(qt.Equals), values.EqualHash(tc.b))
		})
	}
}
