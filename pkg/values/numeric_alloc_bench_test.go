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
	"testing"
)

// Baseline probes for the unboxed-scalar work
// (plans/2026-06-24-unboxed-scalar-arithmetic-design.md). Each probe drives the
// same Number methods the VM's inline arithmetic ops call, so the numbers here
// are the per-operation cost the unboxed lane is trying to remove.
//
// The controls are the point. BenchmarkProbeIntAddInCache is the same operation
// with the allocation removed — NewInteger returns a cached pointer for results
// in [-32768, 32767] — and BenchmarkProbeFloatLt is an arithmetic-shaped op
// whose result is a Boolean and so never allocates at all. The delta between a
// float add and the in-cache integer add is the allocation plus its GC
// accounting, and that delta is what the Cell lane recovers.
//
// Run: go test ./pkg/values/ -run '^$' -bench 'BenchmarkProbe' -benchmem

// probeSink defeats dead-store elimination without adding an allocation of its
// own: the results are interface values that already exist.
var (
	probeSink     Number
	probeBoolSink bool
)

func BenchmarkProbeFloatAdd(b *testing.B) {
	x := NewFloat(1.5)
	y := NewFloat(2.25)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		probeSink = x.Add(y)
	}
}

func BenchmarkProbeFloatMul(b *testing.B) {
	x := NewFloat(1.5)
	y := NewFloat(2.25)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		probeSink = x.Multiply(y)
	}
}

// BenchmarkProbeIntAddInCache is the zero-allocation control: both operands and
// the result land inside the small-integer cache.
func BenchmarkProbeIntAddInCache(b *testing.B) {
	x := NewInteger(3)
	y := NewInteger(4)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		probeSink = x.Add(y)
	}
}

// BenchmarkProbeIntAddOutOfCache pushes the result past intCacheMax so
// NewInteger must allocate. It isolates the allocation from every other
// difference between the integer and float paths.
func BenchmarkProbeIntAddOutOfCache(b *testing.B) {
	x := NewInteger(intCacheMax)
	y := NewInteger(intCacheMax)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		probeSink = x.Add(y)
	}
}

// BenchmarkProbeFloatLt is the second control: identical dispatch and operand
// handling to FloatAdd, but a Boolean result, which is a preallocated
// singleton. What remains is the cost of the comparison itself.
func BenchmarkProbeFloatLt(b *testing.B) {
	x := NewFloat(1.5)
	y := NewFloat(2.25)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		probeBoolSink = x.LessThan(y)
	}
}

// TestProbeAllocationCounts pins the allocation behaviour the probes exist to
// measure, so a change that silently removes (or adds) an allocation on these
// paths fails a test rather than quietly shifting a benchmark. testing.AllocsPerRun
// is exact for these loops: no growth, no amortized buffers.
func TestProbeAllocationCounts(t *testing.T) {
	fx, fy := NewFloat(1.5), NewFloat(2.25)
	inCacheX, inCacheY := NewInteger(3), NewInteger(4)
	bigX, bigY := NewInteger(intCacheMax), NewInteger(intCacheMax)

	tests := []struct {
		name   string
		op     func()
		allocs float64
	}{
		{
			name: "float add allocates one *Float",
			op: func() {
				probeSink = fx.Add(fy)
			},
			allocs: 1,
		},
		{
			name: "float multiply allocates one *Float",
			op: func() {
				probeSink = fx.Multiply(fy)
			},
			allocs: 1,
		},
		{
			name: "in-cache integer add allocates nothing",
			op: func() {
				probeSink = inCacheX.Add(inCacheY)
			},
			allocs: 0,
		},
		{
			name: "out-of-cache integer add allocates one *Integer",
			op: func() {
				probeSink = bigX.Add(bigY)
			},
			allocs: 1,
		},
		{
			name: "float comparison allocates nothing",
			op: func() {
				probeBoolSink = fx.LessThan(fy)
			},
			allocs: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := testing.AllocsPerRun(100, tt.op)
			if got != tt.allocs {
				t.Errorf("allocs/op = %v, want %v", got, tt.allocs)
			}
		})
	}
}
