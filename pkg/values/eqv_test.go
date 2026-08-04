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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestEqv pins values.Eqv against the three rules it composes: identity for
// non-numbers, EqvNumber for numbers, code point for characters.
func TestEqv(t *testing.T) {
	sym := values.NewSymbol("x")
	str := values.NewString("a")
	tcs := []struct {
		name string
		a, b values.Value
		want bool
	}{
		{"same symbol object", sym, sym, true},
		{"de-interned same-named symbols", values.NewSymbol("y"), values.NewSymbol("y"), true},
		{"distinct equal strings are not eqv", values.NewString("a"), values.NewString("a"), false},
		{"same string object", str, str, true},
		{"exact across representations", values.NewInteger(5), values.NewBigIntegerFromInt64(5), true},
		{"exact vs inexact", values.NewInteger(5), values.NewFloat(5.0), false},
		// math.Copysign, not a -0.0 literal. Go's untyped constants have no signed
		// zero, so NewFloat(-0.0) is NewFloat(+0.0) and the case would assert
		// nothing.
		{"signed zero distinguished", values.NewFloat(0.0), values.NewFloat(math.Copysign(0, -1)), false},
		{"same character", values.NewCharacter('a'), values.NewCharacter('a'), true},
		{"different character", values.NewCharacter('a'), values.NewCharacter('b'), false},
		{"distinct pairs", values.NewCons(values.NewInteger(1), values.EmptyList),
			values.NewCons(values.NewInteger(1), values.EmptyList), false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, values.Eqv(tc.a, tc.b), qt.Equals, tc.want)
		})
	}
}
