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

// TestBigAccuracyToSymbol verifies the closed-domain mapping from
// big.Accuracy to the singleton Scheme symbols, plus the panic path for
// values outside Go's documented enum.
func TestBigAccuracyToSymbol(t *testing.T) {
	cases := []struct {
		name string
		in   big.Accuracy
		want *Symbol
	}{
		{"below", big.Below, SymbolAccuracyBelow},
		{"exact", big.Exact, SymbolAccuracyExact},
		{"above", big.Above, SymbolAccuracyAbove},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(BigAccuracyToSymbol(tc.in), qt.Equals, tc.want)
		})
	}

	t.Run("unknown-accuracy-panics", func(t *testing.T) {
		c := qt.New(t)
		// big.Accuracy is documented as Below(-1)/Exact(0)/Above(1); any
		// other value is out-of-domain and must trigger the ErrInternal panic.
		c.Assert(func() { BigAccuracyToSymbol(big.Accuracy(42)) },
			qt.PanicMatches, ".*BigAccuracyToSymbol: unknown big.Accuracy value.*")
	})
}
