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
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestAddBigIntInPlace_Correctness(t *testing.T) {
	cases := []struct {
		name string
		p, v string // decimal strings; big enough to escape int64 in some cases
		want string
	}{
		{"zero-plus-zero", "0", "0", "0"},
		{"small-positive", "3", "4", "7"},
		{"small-mixed-sign", "10", "-3", "7"},
		{"negative-plus-negative", "-5", "-7", "-12"},
		{"large-positive", "18446744073709551616", "1", "18446744073709551617"},
		{"large-cancellation", "12345678901234567890", "-12345678901234567890", "0"},
		{"large-both", "99999999999999999999999999999999", "1", "100000000000000000000000000000000"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			p := newBigIntegerFromDecimal(c, tc.p)
			v := newBigIntegerFromDecimal(c, tc.v)
			dest := &BigInteger{value: new(big.Int)}
			ret := addBigIntInPlace(dest, p, v)
			c.Assert(ret, qt.Equals, dest, qt.Commentf("returns dest for chaining"))
			c.Assert(dest.value.String(), qt.Equals, tc.want)
			// Allocating reference: same answer.
			ref := new(big.Int).Add(p.value, v.value)
			c.Assert(dest.value.Cmp(ref), qt.Equals, 0)
		})
	}
}

func TestAddBigIntInPlace_AliasDestEqP(t *testing.T) {
	c := qt.New(t)
	// dest aliases p: addBigIntInPlace(dest, dest, v).
	dest := newBigIntegerFromDecimal(c, "100")
	v := newBigIntegerFromDecimal(c, "23")
	want := new(big.Int).Add(dest.value, v.value) // snapshot before mutation
	addBigIntInPlace(dest, dest, v)
	c.Assert(dest.value.Cmp(want), qt.Equals, 0)
}

func TestAddBigIntInPlace_AliasDestEqV(t *testing.T) {
	c := qt.New(t)
	// dest aliases v: addBigIntInPlace(dest, p, dest).
	p := newBigIntegerFromDecimal(c, "7")
	dest := newBigIntegerFromDecimal(c, "35")
	want := new(big.Int).Add(p.value, dest.value)
	addBigIntInPlace(dest, p, dest)
	c.Assert(dest.value.Cmp(want), qt.Equals, 0)
}

func TestAddBigIntInPlace_AliasAll(t *testing.T) {
	c := qt.New(t)
	// dest aliases both p and v: addBigIntInPlace(dest, dest, dest) → 2 * dest.
	dest := newBigIntegerFromDecimal(c, "12345678901234567890")
	want := new(big.Int).Add(dest.value, dest.value)
	addBigIntInPlace(dest, dest, dest)
	c.Assert(dest.value.Cmp(want), qt.Equals, 0)
}

func TestAddBigIntInPlace_StorageReuse(t *testing.T) {
	c := qt.New(t)
	// Pre-size dest's backing to a capacity that comfortably holds the result.
	// Repeated additions within capacity should not allocate a new []Word.
	dest := &BigInteger{value: new(big.Int)}
	dest.value.SetBits(make([]big.Word, 0, 16))
	one := newBigIntegerFromDecimal(c, "1")

	// Establish the cap of the backing after one warmup op.
	addBigIntInPlace(dest, dest, one)
	initialCap := cap(dest.value.Bits())
	c.Assert(initialCap >= 1, qt.IsTrue, qt.Commentf("backing should have capacity after first op"))

	// 1023 more additions: result is 1024, well within the pre-allocated capacity.
	for range 1023 {
		addBigIntInPlace(dest, dest, one)
	}
	c.Assert(dest.value.Cmp(big.NewInt(1024)), qt.Equals, 0)
	// Capacity should not have grown — value fits in one word, was pre-sized to 16.
	c.Assert(cap(dest.value.Bits()) <= initialCap, qt.IsTrue,
		qt.Commentf("backing should not have reallocated; was %d, now %d", initialCap, cap(dest.value.Bits())))
}

// newBigIntegerFromDecimal is a test helper that constructs a *BigInteger from
// a decimal string and fails the test if parsing fails.
func newBigIntegerFromDecimal(c *qt.C, s string) *BigInteger {
	c.Helper()
	v, ok := new(big.Int).SetString(s, 10)
	c.Assert(ok, qt.IsTrue, qt.Commentf("parsing %q", s))
	return &BigInteger{value: v}
}
