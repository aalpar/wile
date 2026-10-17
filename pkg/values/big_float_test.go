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
	"math/rand"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestDecimalLiteralPrecision(t *testing.T) {
	tcs := []struct {
		in   string
		want uint
	}{
		{"1.5", DefaultBigFloatPrecision},
		{"1e400", DefaultBigFloatPrecision},
		{"0." + strings.Repeat("0", 200) + "1", DefaultBigFloatPrecision},
		{strings.Repeat("7", 100), 334},
		{"-1." + strings.Repeat("7", 99) + "e5", 334},
	}
	for _, tc := range tcs {
		t.Run(tc.in[:min(len(tc.in), 20)], func(t *testing.T) {
			c := qt.New(t)
			c.Assert(decimalLiteralPrecision(tc.in), qt.Equals, tc.want)
		})
	}
}

// TestDecimalLiteralPrecisionIntegersAreExact checks the formula against the
// oracle rather than a constant: an n-digit integer must parse exactly.
func TestDecimalLiteralPrecisionIntegersAreExact(t *testing.T) {
	c := qt.New(t)
	for n := 1; n <= 2000; n++ {
		s := strings.Repeat("9", n)
		want, _ := new(big.Int).SetString(s, 10)
		got, acc := NewBigFloatFromString(s).BigFloatValue().Int(nil)
		c.Assert(acc, qt.Equals, big.Exact, qt.Commentf("n=%d", n))
		c.Assert(got.Cmp(want), qt.Equals, 0, qt.Commentf("n=%d", n))
	}
}

// TestLongDecimalFractionRoundTrips pins the Matula margin: fractions with a
// leading 8 or 9 fail a +1 bound for some digit strings.
func TestLongDecimalFractionRoundTrips(t *testing.T) {
	rng := rand.New(rand.NewSource(192))
	for n := 78; n <= 130; n++ {
		for i := 0; i < 60; i++ {
			var b strings.Builder
			b.WriteByte(byte('8' + rng.Intn(2)))
			b.WriteByte('.')
			for j := 1; j < n-1; j++ {
				b.WriteByte(byte('0' + rng.Intn(10)))
			}
			b.WriteByte(byte('1' + rng.Intn(9)))
			in := b.String()
			got := NewBigFloatFromString(in).value.Text('g', -1)
			if got != in {
				t.Fatalf("n=%d: %s read back as %s", n, in, got)
			}
		}
	}
}
