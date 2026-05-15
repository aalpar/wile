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

	"github.com/aalpar/wile/werr"
)

// Accuracy singleton symbols — paraphrase big.Accuracy at the Scheme level.
// Returned by primitives like inexact-accuracy and inexact-with-accuracy.
//
// 'below — result < true value (rounded down)
// 'exact — result == true value (lossless)
// 'above — result > true value (rounded up)
var (
	SymbolAccuracyBelow = NewSymbol("below")
	SymbolAccuracyExact = NewSymbol("exact")
	SymbolAccuracyAbove = NewSymbol("above")
)

// BigAccuracyToSymbol maps a Go big.Accuracy to the corresponding Scheme
// singleton symbol. Used by primitives in extensions/math/ that surface
// accuracy to Scheme.
func BigAccuracyToSymbol(acc big.Accuracy) *Symbol {
	switch acc {
	case big.Below:
		return SymbolAccuracyBelow
	case big.Exact:
		return SymbolAccuracyExact
	case big.Above:
		return SymbolAccuracyAbove
	default:
		panic(werr.WrapForeignErrorf(werr.ErrInternal, "BigAccuracyToSymbol: unknown big.Accuracy value %d", int(acc)))
	}
}
