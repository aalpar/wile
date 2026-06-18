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
	b.ReportAllocs()
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
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		mulBigIntInPlace(dest, p, v)
	}
}

// Self-aliasing smoke bench — confirms the all-aliased path
// (`addBigIntInPlace(x, x, x)`) does not regress relative to a
// distinct-operand call. The reset-and-double pattern keeps the
// working value at minimum size (`1+1=2`), so this bench measures the
// fixed-overhead of self-aliasing dispatch in math/big, not the cost
// at production-scale bignum width. For the latter, the production
// Pattern-3A workload is exercised end-to-end by
// `examples/benchmarks/bench-bigint-counting-unit-weight.scm`.

func BenchmarkBigIntAddInPlace_SelfAlias(b *testing.B) {
	x := &BigInteger{value: big.NewInt(1)}
	for range 10 {
		addBigIntInPlace(x, x, x) // grow to a stable working size
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// reset and re-double to keep the value within a fixed size band
		x.value.SetInt64(1)
		addBigIntInPlace(x, x, x)
	}
}

// TestBigIntInPlace_ZeroAllocs pins the published zero-allocation
// claim from values/CLAUDE.md's bench table. Any future change to
// math/big or to the helpers that introduces an allocation on the
// steady-state in-place path fails this test.
func TestBigIntInPlace_ZeroAllocs(t *testing.T) {
	p := &BigInteger{value: big.NewInt(1000000)}
	v := &BigInteger{value: big.NewInt(2000000)}
	dest := &BigInteger{value: new(big.Int)}

	// Warm dest's backing.
	addBigIntInPlace(dest, p, v)
	mulBigIntInPlace(dest, p, v)

	addAllocs := testing.AllocsPerRun(100, func() {
		addBigIntInPlace(dest, p, v)
	})
	if addAllocs != 0 {
		t.Errorf("addBigIntInPlace: expected 0 allocs/op, got %v", addAllocs)
	}

	mulAllocs := testing.AllocsPerRun(100, func() {
		mulBigIntInPlace(dest, p, v)
	})
	if mulAllocs != 0 {
		t.Errorf("mulBigIntInPlace: expected 0 allocs/op, got %v", mulAllocs)
	}
}
