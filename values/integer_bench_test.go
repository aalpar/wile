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

	"github.com/aalpar/wile/values"
)

func BenchmarkIntegerAdd(b *testing.B) {
	x := values.NewInteger(42)
	y := values.NewInteger(17)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = x.Add(y)
	}
}

func BenchmarkIntegerMultiply(b *testing.B) {
	x := values.NewInteger(42)
	y := values.NewInteger(17)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = x.Multiply(y)
	}
}

func BenchmarkIntegerOverflow(b *testing.B) {
	x := values.NewInteger(math.MaxInt64)
	one := values.NewInteger(1)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = x.Add(one)
	}
}

func BenchmarkIntegerCache(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = values.NewInteger(42)
	}
}

func BenchmarkBigIntegerAdd(b *testing.B) {
	x := values.NewBigIntegerFromInt64(1000000)
	y := values.NewBigIntegerFromInt64(2000000)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = x.Add(y)
	}
}

func BenchmarkBigIntegerMultiply(b *testing.B) {
	x := values.NewBigIntegerFromInt64(1000000)
	y := values.NewBigIntegerFromInt64(2000000)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = x.Multiply(y)
	}
}
