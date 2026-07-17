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
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestBigIntegerBigIntAliasesStorage pins the documented contract of
// BigInteger.BigInt: it hands out the live storage, it does not copy.
//
// This is a characterization test, not an endorsement. The accessor stays
// no-copy because it is on the numeric hot path and nearly every caller only
// reads. If a defensive copy is ever added, this test is where that decision
// surfaces — flipping it is the point at which the cost lands on every reader,
// so make that trade deliberately rather than by editing the assertion.
func TestBigIntegerBigIntAliasesStorage(t *testing.T) {
	c := qt.New(t)

	bi := values.NewBigInteger(big.NewInt(42))
	c.Assert(bi.BigInt().String(), qt.Equals, "42")

	// Two calls hand back the same object, not independent snapshots.
	first := bi.BigInt()
	second := bi.BigInt()
	c.Assert(first == second, qt.IsTrue,
		qt.Commentf("BigInt() returned distinct pointers; the no-copy contract changed"))

	// Mutating through the accessor rewrites the "immutable" exact integer.
	bi.BigInt().SetInt64(99)
	c.Assert(bi.BigInt().String(), qt.Equals, "99")
	c.Assert(bi.Int64(), qt.Equals, int64(99))

	// The documented defense: the mutating caller takes its own copy.
	safe := values.NewBigInteger(big.NewInt(7))
	mine := new(big.Int).Set(safe.BigInt())
	mine.SetInt64(1234)
	c.Assert(safe.Int64(), qt.Equals, int64(7))
}

// TestBigIntegerConstructorsCopyInput is the other half of the contract: the
// constructors do copy, so a BigInteger never aliases a caller's big.Int.
// Without this, the accessor's aliasing would extend backwards to the source
// and the "take your own copy" advice above would not be sufficient.
func TestBigIntegerConstructorsCopyInput(t *testing.T) {
	c := qt.New(t)

	src := big.NewInt(5)
	bi := values.NewBigInteger(src)
	src.SetInt64(500)
	c.Assert(bi.Int64(), qt.Equals, int64(5),
		qt.Commentf("NewBigInteger aliased its input"))
}
