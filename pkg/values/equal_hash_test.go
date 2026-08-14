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
	"slices"

	"math"
	"strconv"
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
		for _, n := range slices.Backward(ns) {
			q = values.NewCons(values.NewInteger(n), q)
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
	v.Elems()[0] = v
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
		// *ByteVector's equal? is CONTENT-based, but it implements neither
		// Hashable nor EqualComponents, so without its own arm it fell to the
		// opaque type-name bucket and EVERY bytevector hashed alike.
		{"bytevector contents", values.NewByteVectorFromBytes(1, 2), values.NewByteVectorFromBytes(3, 4)},
		{"bytevector length", values.NewByteVectorFromBytes(1, 2), values.NewByteVectorFromBytes(1, 2, 3)},
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

// TestEqualHash_WideValueIsBudgetBounded pins that the node budget bounds WORK,
// not merely nodes mixed.
//
// Every container arm used to push all of its children before the pop loop
// re-checked the budget, so a wide value was fully materialized on the stack and
// then discarded: a 2^20-element vector measured 22.8ms and 89MB per call, which
// an equal-kind table pays on EVERY lookup with such a key — newly reachable,
// because container keys only became legal with this work.
//
// Allocation is the assertion rather than time: it is the quantity that was
// proportional to the input, and it is stable across machines.
func TestEqualHash_WideValueIsBudgetBounded(t *testing.T) {
	wide := func(n int) values.Value {
		vs := make([]values.Value, n)
		for i := range vs {
			vs[i] = values.NewInteger(int64(i))
		}
		return values.NewVector(vs...)
	}
	// Built OUTSIDE the measured closure: AllocsPerRun counts everything the
	// function does, and constructing the vector would swamp the walk.
	narrow := wide(1 << 10)
	broad := wide(1 << 20)
	small := testing.AllocsPerRun(20, func() {
		_ = values.EqualHash(narrow)
	})
	large := testing.AllocsPerRun(20, func() {
		_ = values.EqualHash(broad)
	})
	// A 1024x wider input must not cost meaningfully more. Compared with slack
	// rather than for equality: the walk allocates a little as the stack grows to
	// the budget, and that is what is being bounded.
	qt.Assert(t, large <= small*2, qt.IsTrue,
		qt.Commentf("EqualHash allocations scale with input width: 2^10 => %v, 2^20 => %v", small, large))
}

// TestEqualHash_WideValuesAgreePastTheBudget guards the truncation against being
// made asymmetric later. Two equal? values must still hash alike when they are
// far wider than the budget, and when they differ only beyond it they may
// legitimately collide — that is the one-directional contract, not a defect.
func TestEqualHash_WideValuesAgreePastTheBudget(t *testing.T) {
	wide := func(n int, tail int64) values.Value {
		vs := make([]values.Value, n)
		for i := range vs {
			vs[i] = values.NewInteger(int64(i))
		}
		vs[n-1] = values.NewInteger(tail)
		return values.NewVector(vs...)
	}
	a := wide(1<<20, 1)
	b := wide(1<<20, 1)
	qt.Assert(t, values.Equal(a, b), qt.IsTrue)
	qt.Assert(t, values.EqualHash(a), qt.Equals, values.EqualHash(b))

	// Records take the same truncation path.
	fields := make([]*values.Symbol, 1000)
	vals := make([]values.Value, 1000)
	for i := range fields {
		fields[i] = values.NewSymbol("f" + strconv.Itoa(i))
		vals[i] = values.NewInteger(int64(i))
	}
	rt := values.NewRecordType(values.NewSymbol("wide"), fields)
	r1, err := values.NewRecord(rt, vals)
	qt.Assert(t, err, qt.IsNil)
	r2, err := values.NewRecord(rt, append([]values.Value(nil), vals...))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, values.Equal(r1, r2), qt.IsTrue)
	qt.Assert(t, values.EqualHash(r1), qt.Equals, values.EqualHash(r2))
}
