package values

import (
	"math"
	"math/big"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestIntegerDivideTableMinInt64ByNegOne pins the integerDivide[KindInteger]
// entry, which (*Integer).Divide's same-kind fast path keeps from ever being
// reached through Divide: reverting the MinInt64 / -1 fix in the table alone
// would otherwise pass every test.
func TestIntegerDivideTableMinInt64ByNegOne(t *testing.T) {
	c := qt.New(t)
	result, err := integerDivide[KindInteger](NewInteger(math.MinInt64), NewInteger(-1))
	c.Assert(err, qt.IsNil)
	bi, ok := result.(*BigInteger)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T (%s)", result, result.SchemeString()))
	c.Assert(bi.value.Cmp(new(big.Int).Neg(big.NewInt(math.MinInt64))), qt.Equals, 0)
}
