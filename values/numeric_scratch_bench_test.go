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
)

// In-place benchmarks for the Phase-1 numeric scratch helpers. Establish a
// regression gate against the corresponding allocating variants in
// `integer_bench_test.go` (BenchmarkBigIntegerAdd / BenchmarkBigIntegerMultiply).
//
// The allocating path goes through (*BigInteger).Add → newBigIntFromOp →
// new(big.Int) + &BigInteger{...}, totalling 3 heap allocations per op.
// The in-place path reuses dest's storage; expected 0 allocs after warmup.

func BenchmarkBigIntAddInPlace(b *testing.B) {
	p := &BigInteger{value: big.NewInt(1000000)}
	v := &BigInteger{value: big.NewInt(2000000)}
	dest := &BigInteger{value: new(big.Int)}
	addBigIntInPlace(dest, p, v) // warm capacity
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		addBigIntInPlace(dest, p, v)
	}
}

func BenchmarkBigIntMulInPlace(b *testing.B) {
	p := &BigInteger{value: big.NewInt(1000000)}
	v := &BigInteger{value: big.NewInt(2000000)}
	dest := &BigInteger{value: new(big.Int)}
	mulBigIntInPlace(dest, p, v) // warm capacity
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mulBigIntInPlace(dest, p, v)
	}
}

// Self-aliasing variants — the Pattern-3A inner loop from the
// bignum-allocation-reduction plan uses `addBigIntInPlace(d[v], d[v], d[u])`
// with self-loops (e.u == e.v) reducing to `addBigIntInPlace(d, d, d)`.
// This bench exercises the all-aliased path so its cost is part of the
// regression gate.

func BenchmarkBigIntAddInPlace_SelfAlias(b *testing.B) {
	x := &BigInteger{value: big.NewInt(1)}
	for range 10 {
		addBigIntInPlace(x, x, x) // grow to a stable working size
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// reset and re-double to keep the value within a fixed size band
		x.value.SetInt64(1)
		addBigIntInPlace(x, x, x)
	}
}
